(in-package :cl-graphql)

(defgeneric whitespacep (ch))

(define-condition graphql-parse-error (simple-error)
  ((stream :initarg :stream)))

(defun raise-parse-error (stream)
  (let ((line-info (head stream)))
    (if (line-counted-p line-info)
	(error 'graphql-parse-error :stream stream
	       :format-control "Parse error in ~a, Line ~a, Column ~a, before ~a"
	       :format-arguments (list (line-counted-filename line-info)
				       (line-counted-line line-info)
				       (line-counted-col line-info)
				       (coerce (stream->list
						(strip-line-info
						 (take 20 stream)))
					       'string)))
	(error 'graphql-parse-error :stream stream
	       :format-control "Parse error before ~a"
	       :format-arguments (list (coerce (stream->list
						(take 20 stream))
					       'string))))))

(defun raise-missing-bracket-error (beginning expected-bracket)
  (let ((line-info (head beginning)))
    (if (line-counted-p line-info)
	(error 'graphql-parse-error :stream beginning
	       :format-control "Missing closing bracket ~a in ~a, opened at Line ~a, Column ~a; ~a"
	       :format-arguments (list expected-bracket
				       (line-counted-filename line-info)
				       (line-counted-line line-info)
				       (line-counted-col line-info)
				       (coerce (stream->list
						(strip-line-info
						 (take 20 beginning)))
					       'string)))
	(error 'graphql-parse-error :stream beginning
	       :format-control "Missing closing bracket ~a in ~a"
	       :format-arguments (list expected-bracket
				       (coerce (stream->list
						(take 20 beginning))
					       'string))))))

(defun whitespacep (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return #\,) :test #'ch=))

(defun drop-whitespace (lazy-stream)
  (drop-while #'whitespacep lazy-stream))

(defun name-charp (ch)
  (or
   (alphanumericp (raw-ch ch))
   (ch= ch #\_)))

(defun starting-name-charp (ch)
  (or (alpha-char-p (raw-ch ch))
      (ch= ch #\_)))

(defun int-part (stream)
  (cond ((or (and (ch= (head stream) #\-)
		  (numericp (raw-ch (head (tail stream))))
		  (not (ch= (head (tail stream)) #\0)))
	     (numericp (raw-ch (head stream))))
	 (alter-first-value (split-where (compose #'not #'numericp #'raw-ch) (tail stream)) number
	   (cons (head stream) number)))
	(t (values nil stream))))

(defun fractional-part (stream)
  (if (ch= (head stream) #\.)
      (alter-first-value (split-where (compose #'not #'numericp #'raw-ch)
				     (tail stream))
	  numerals
	(stream-cons (head stream) numerals))
      (values nil stream)))

(defun exponential-part (stream)
  (if (member (head stream) '(#\e #\E) :test #'ch=)
      (aif2 (int-part (tail stream))
	    (values (stream-cons (head stream) it)
		    not-it)
	    (values nil stream))))

(defun block-string-common-indent (lines)
    (loop for line in (cdr lines)
       for length = (length line)
       for indent = (loop for ch in line
		       while (whitespacep ch)
		       count t)
	 when (< indent length) minimize indent))

(defun block-string-unindent (lines common-indent)
  (if common-indent
      (cons (car lines)
	    (mapcar (curry #'drop common-indent) (cdr lines)))
      lines))

(defun block-string-value (raw-value)
  "Implements the convoluted logic of the BlockStringValue() function described in the
GraphQL specification."
  (let* ((lines (split-sequence #\Newline
				(stream->list raw-value)))
	 (unindented-lines
	  (nreverse
	   (drop-while (curry #'every #'whitespacep)
		       (nreverse
			(drop-while (curry #'every #'whitespacep)
				    (block-string-unindent lines (block-string-common-indent lines))))))))
    (append (car unindented-lines)
	    (loop for line in (cdr unindented-lines)
	       collect #\Newline
		 append line))))	 

(defun parse-block-string (stream)
  (unless (equal (coerce (stream->list
			  (strip-line-info 
			   (take 3 stream)))
			 'string)
		 "\"\"\"")
    (invoke-restart 'fail))
  (block-string-value
   (let ((original-stream stream))
     (loop for stream = (drop 3 stream) then (tail stream)
	until (equal (coerce (stream->list
			      (strip-line-info
			       (take 3 stream)))
			     'string)
		     "\"\"\"")
	if (stream-null? stream)
	do (raise-missing-bracket-error (or string-characters original-stream) "\"\"\"")
	 
	collect (head stream) into string-characters
	finally (return (values string-characters
				(drop 3 stream)))))))
       

(defun parse-unicode-character (stream)
  (unless (and (ch= (head stream) #\\)
	       (ch= (head (tail stream)) #\u))
    (invoke-restart 'fail))
  (let ((hex-string (handler-case
			(coerce (loop for my-stream = (drop 2 stream) then (tail my-stream)
				   for ch = (raw-ch (head my-stream))
				   repeat 4
				   collect ch)
				'string)
		      (t () (invoke-restart 'fail)))))
    (handler-case (values (code-char (parse-integer hex-string :radix 16))
			  (drop 6 stream))
      (t () (invoke-restart 'fail)))))

(defun parse-escaped-character (stream)
  (unless (ch= (head stream) #\\)
    (invoke-restart 'fail))
  (values
   (list
    (case (raw-ch (head (tail stream)))
      (#\" #\")
      (#\\ #\\)
      (#\/ #\/) ;; FIXME: Is this right?
      (#\b #\Bel)
      (#\f #\Page)
      (#\n #\Newline)
      (#\r #\Return)
      (#\t #\Tab)
      (otherwise (invoke-restart 'fail))))
   (tail (tail stream))))

(defun parse-escaped-string-body (stream)
  (if (ch= (head stream) #\")
      nil
      (loop
	 with unparsed = stream
	 until (ch= (head unparsed) #\")
	 if (stream-null? unparsed)
	 do (raise-missing-bracket-error stream "\"")
	 else append 
	   (choose ((parser ('parse-unicode-character 'parse-escaped-character nil)))
	     (if parser
		 (multiple-value-bind (parsed* unparsed*)
		     (funcall parser unparsed)
		   (setf unparsed unparsed*)
		   parsed*)
		 (prog1
		     (list (head unparsed))
		   (setf unparsed (tail unparsed)))))
	 into parsed
	   finally (return (values parsed (tail unparsed))))))
	    

(defun parse-escaped-string (stream)
  (unless (ch= (head stream) #\")
    (invoke-restart 'fail))
  (parse-escaped-string-body (tail stream)))

(defun parse-string-value (stream)
  (handler-case
      (choose ((parser (#'parse-block-string #'parse-escaped-string)))
	(funcall parser stream))
    (out-of-possibilities ()
      (values nil stream))))
  

(defmacro append-first-value (multi-value-expr list)
  (let ((vals (gensym)))
    `(let ((,vals (multiple-value-list ,multi-value-expr)))
       (apply #'values
	      (cons
	       (append (car ,vals) ,list)
	       (cdr ,vals))))))

(defun token-class (stream)
  (append-first-value
   (acond2 ((whitespacep (head stream))
	    (token-class (drop-whitespace stream)))
	   ((member (head stream) '(#\! #\$ #\( #\) #\: #\= #\@ #\[ #\] #\{ #\| #\}) :test #'ch=)
	    (values (list :punctuator
			  (list (head stream)))
		    (tail stream)))
	   ((every (curry #'ch= #\.)
		   (stream->list (take 3 stream)))
	    (alter-first-value (split-at 3 stream)
		dots
	      (list :punctuator dots)))
	   ((int-part stream)
	    (let ((int-part it)
		  (not-int-part not-it))
	      (acond2 ((fractional-part not-int-part)
		       (let ((frac-part it)
			     (not-frac-part not-it))
			 (aif2 (exponential-part not-frac-part)
			       (values (list :float-value (stream-append int-part frac-part it))
				       not-it)
			       (values (list :float-value (stream-append int-part frac-part))
				       not-frac-part))))
		      ((exponential-part not-int-part)
		       (values (list :float-value (stream-append int-part it))
			       not-it))
		      (t (values (list :int-value int-part)
				 not-int-part)))))
	   ((and (starting-name-charp (head stream))
		 (name-charp (head (tail stream))))
	    (alter-first-value (split-where (compose #'not #'name-charp) stream) name
	      (list :name name)))
	   ((parse-string-value stream)
	    (values (list :string it) not-it))
	   (t (raise-parse-error stream)))
   (list stream)))
  
(defun take-token (lazy-stream)
  (multiple-value-bind (token-class remainder) (token-class lazy-stream)
    (values
     (list (first token-class)
	   (copy-line-count-info (strip-line-info (second token-class)) (head (second token-class)))
	   (third token-class))
     remainder)))

(defun drop-while-comment (lazy-stream)
  (let ((lazy-stream (if (ch= (head lazy-stream) #\#)
			 (drop-while (lambda (ch)
				       (not (ch= ch #\Newline)))
				     lazy-stream)
			 lazy-stream)))
    (if (ch= (head lazy-stream) #\#)
	(drop-while-comment lazy-stream)
	lazy-stream)))

(defun strip-comments (lazy-stream &key quoted escaped)
  (let ((lazy-stream (if quoted
			 lazy-stream
			 (drop-while-comment lazy-stream))))
    (stream-cons (head lazy-stream)
		 (unless (stream-null? (tail lazy-stream))
		   (strip-comments (tail lazy-stream)
				   :quoted (if (and (not escaped)
						    (ch= (head lazy-stream) #\"))
					       (not quoted)
					       quoted)
				   :escaped (ch= (head lazy-stream) #\\))))))

(defun tokens/no-comments (lazy-stream)
  (multiple-value-bind (token remainder)
      (take-token lazy-stream)
    (stream-cons token
		 (if (stream-null? remainder)
		     nil
		     (tokens/no-comments remainder)))))

(defun stripped-token (tok)
  "Returns a token stripped of line-count information"
  (stream-map (lambda (tok)
		(if (line-counted-p tok)
		    (stream->string (line-counted-val tok))
		    tok))
	      tok))

(defun strip-line-info-from-tokens (token-stream)
  (stream-map #'stripped-token token-stream))
 
(defun tokens (lazy-stream)
  "Given a lazy-stream full of line-counted characters, returns a lazy-stream of line-counted tokens."
  (tokens/no-comments (strip-comments lazy-stream)))
