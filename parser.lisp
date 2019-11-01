(defun token-type (tok)
  (first tok))

(defun token-string (tok)
  (coerce
   (stream->list
    (block list-of-chars
      (line-counted-val
       (let ((ti (token-info tok)))
	 (if (line-counted-p ti)
	     ti
	     (return-from list-of-chars ti))))))
   'string))

(defun token-info (tok)
  (second tok))

(defun raise-token-error (stream tok &optional was-expecting)
  (if (line-counted-p (token-info tok))
      (error 'graphql-parse-error :stream stream
	     :format-control "Unexpected token ~s in ~a at Line ~a, Column ~a~a"
	     :format-arguments
	     (list (token-string tok)
		   (line-counted-filename (token-info tok))
		   (line-counted-line (token-info tok))
		   (line-counted-col (token-info tok))
		   (if was-expecting
		       (format nil " (was expecting ~a)" was-expecting)
		       "")))
      (error 'graphql-parse-error :stream stream
	     :format-control "Unexpected token ~s~a"
	     :format-arguments (list (token-string tok)
				     (if was-expecting
					 (format nil " (was expecting ~a)" was-expecting)
					 "")))))

(defmacro with-token-error-on-fail (&body body)
  `(restart-case
       (progn ,@body)
     (fail (&key stream bad-token was-expecting)
       (raise-token-error stream
			  bad-token
			  was-expecting))))


(defclass parse-tree-node ()
  ((tokens :initarg :tokens
	   :reader definition-tokens
	   :writer set-definition-tokens
	   :initform nil)))

(defgeneric parse-node (class-specifier token-stream)
  (:documentation "Parses a node of class CLASS-SPECIFIER out of the TOKEN-STREAM.

The TOKEN-STREAM may not actually have the tokens required to parse the class. In
that case, methods are expected to invoke the FAIL restart.

Otherwise, two values are returned: An object of class CLASS-SPECIFIER, and
the unparsed remainder of the TOKEN-STREAM.

In some cases, the GraphQL spec demands that certain classes are really just lists.
In that case, the CLASS-SPECIFIER may not be a class at all. Methods are permitted
to return lists in those cases."))

(defclass definition (parse-tree-node) ())

(defclass executable-definition (definition) ())

(defun operation-type-p (name)
  (member name '("query" "mutation" "subscription")
	  :test #'equal))


(defclass operation-definition (executable-definition)
  ((selection-set :initarg :selection-set
		  :initform nil
		  :reader selection-set
		  :writer set-selection-set)
   (operation-type :type (or (satisfies operation-type) null)
		   :initarg :operation-type
		   :initform nil
		   :reader operation-type
		   :writer set-operation-type)
   (name :type (or string null)
	 :initarg :name
	 :initform nil
	 :reader name
	 :writer set-name)
   (variable-definitions :type list
			 :initarg :variable-definitions
			 :initform nil
			 :reader variable-definitions
			 :writer set-variable-definitions)
   (directives :type list
	       :initarg :directives
	       :initform nil
	       :reader directives
	       :writer set-directives)))

(defmethod parse-node ((class-specifier (eql 'selection-set)) token-stream)
  (unless (equal (token-string (head token-stream))
		 "{")
    (invoke-restart 'fail
		    :stream token-stream
		    :bad-token (head token-stream)
		    :was-expecting "{"))
  (with-token-error-on-fail
    (loop for unparsed = (tail token-stream) then (tail token-stream)
       for next-token = (head unparsed)
       until (equal (token-string next-token) "}")
       when (stream-null? unparsed)
       do (invoke-restart 'fail
			  :stream token-stream
			  :was-expecting "}")
       collect (multiple-value-bind (next-selection new-unparsed)
		   (parse-node 'selection unparsed)
		 (setf unparsed (cons :drop-me new-unparsed))
		 next-selection))))
  
(defmethod parse-node ((class-specifier (eql 'operation-type)) token-stream)
  (unless (and (eq (token-type (head token-stream))
		   :name)
	       (member (token-string (head token-stream))
		       '("query" "mutation" "subscription")
		       :test #'equal))
    (invoke-restart 'fail
		    :stream token-stream
		    :bad-token (head token-stream)
		    :was-expecting "'query' or 'mutation' or 'subscription'"))
  (values
   (token-string (head token-stream))
   (tail token-stream)))
		   

(defclass selection (parse-tree-node) ())
(defclass field (selection) ())
(defclass fragment-spread (selection) ())
(defclass inline-fragment (selection) ())
		   
(defclass fragment-definition (executable-definition) ())

(defclass type-system-definition (definition) ())
(defclass type-system-extension (definition) ())
