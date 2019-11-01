(in-package :cl-graphql)

(defmacro alter-first-value (two-value-expression var &body body)
  (let ((second-value (gensym)))
    `(multiple-value-bind (,var ,second-value) ,two-value-expression
       (values (progn ,@body) ,second-value))))

(defmacro alter-second-value (two-value-expression var &body body)
  (let ((first-value (gensym)))
    `(multiple-value-bind (,first-value ,var) ,two-value-expression
       (values ,first-value (progn ,@body)))))

(defmacro values->stream-cons (two-value-expression)
  (let ((car (gensym))
	(cdr (gensym)))
    `(multiple-value-bind (,car ,cdr) ,two-value-expression
       (stream-cons ,car ,cdr))))
	

(defun numericp (character)
  (and (alphanumericp character)
       (not (alpha-char-p character))))

(defmacro acond (&rest clauses)
  (if clauses
      (destructuring-bind ((condition &rest body) &rest other-clauses) clauses
	`(aif ,condition
	      (progn ,@body)
	      (acond ,@other-clauses)))))

(defmacro aif2 (cond if-true &optional if-false)
  `(multiple-value-bind (it not-it) ,cond
     (declare (ignorable it not-it))
     (if it ,if-true ,if-false)))

(defmacro acond2 (&rest clauses)
  (if clauses
      (destructuring-bind ((condition &rest body) &rest other-clauses) clauses
	`(aif2 ,condition
	      (progn ,@body)
	      (acond2 ,@other-clauses)))))

(define-condition out-of-possibilities () ())

(defmacro choose (choice-bindings &body body)
  "Based on Paul Graham's CHOOSE from On Lisp. Each of the CHOICE-BINDINGS
has the form (SYMB (v1 v2 ... vn)), where v1 through vn are possible values
that SYMB might take. If (FAIL) is called, then a different set of choices
is made for all the CHOICE-BINDINGS, until a combination is found that doesn't
result in FAIL being called. The values v1 .. vn may be evaluated multiple times."
  (with-gensyms (possible-values cursors increment-cursor
		 initialize-bindings)
    `(let* ((,possible-values (vector ,@(loop for (nil values)
					   in choice-bindings
					   collect `(vector ,@values))))
	    (,cursors (vector ,@(loop for values in choice-bindings
				   collect 0))))
       (let ,(loop for (variable nil) in choice-bindings
		collect `(,variable nil))
	 (labels ((,initialize-bindings ()
		    ,@(loop for (variable nil) in
			 choice-bindings
			 for n from 0
			 collect `(setf ,variable (aref (aref ,possible-values ,n)
							(aref ,cursors ,n)))))
		  (show-cursor-state ()
		    (format t "Cursor state: ~%")
		    (loop for c across ,cursors
		       do (format t "   ~s~%" c)))
		  (,increment-cursor (&optional (n 0))
		    (when (>= n (length ,possible-values))
		      (aif (find-restart 'fail)
			   (invoke-restart it)
			   (error 'out-of-possibilities :format-control "Out of possibilities")))
		    (incf (aref ,cursors n))
		    (when (>= (aref ,cursors n)
			      (length (aref ,possible-values n)))
		      (setf (aref ,cursors n) 0)
		      (,increment-cursor (1+ n)))))

	   (,initialize-bindings)
	   (loop do
		(restart-case
		    (return (progn ,@body))
		  (fail ()
		    (,increment-cursor)
		    (,initialize-bindings)))))))))
		  
			  
	 
  
