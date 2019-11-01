(in-package :lazy-streams)

;; This structure has all the benefits of traditional
;; SRFI-41 streams using a classic implementation of delay
;; and force, but it performs better in SBCL due
;; to less garbage collection.

(defclass stream-cons ()
  ((car-memoized :initarg :car-memoized :initform 'nothing)
   (cdr-memoized :initarg :cdr-memoized :initform 'nothing)
   (car-function :initarg :car-function :initform (lambda () nil) :type function)
   (cdr-function :initarg :cdr-function :initform (lambda () nil) :type function)))


(defgeneric lazy-streamp (obj))
(defmethod lazy-streamp (obj)
  nil)

(defmethod lazy-streamp ((obj stream-cons))
  t)

(defmethod lazy-streamp ((obj null))
  t)

(defmacro def-stream-accessor (part)
  (let ((cons-part-memoized (intern (format nil "~A-MEMOIZED" part)))
	(cons-part-function (intern (format nil "~A-FUNCTION" part)))
	(function-name (intern (format nil "STREAM-~A" part))))
    `(defun ,function-name (stream-cons)
       (cond
	 ((null stream-cons)
	  nil)
	 ((eq (slot-value stream-cons ',cons-part-memoized)
	      'nothing)
	  (setf (slot-value stream-cons ',cons-part-memoized)
		(funcall (slot-value stream-cons ',cons-part-function)))
	  (,function-name stream-cons))
	 (t (slot-value stream-cons ',cons-part-memoized))))))
  

(def-stream-accessor car)
(def-stream-accessor cdr)

(defgeneric stream-null? (str))

(defmethod stream-null? (str) nil)

(defmethod stream-null? ((str null)) t)

(defgeneric stream-append-2 (first second))

(defmethod stream-append-2 ((first stream-cons) second)
  (cond ((stream-null? first)
	 (if (listp second)
	     (list->stream second)
	     second))
	(t
	 (stream-cons
	    (head first)
	    (stream-append-2 (tail first)
			     second)))))

(defmethod stream-append-2 ((first list) second)
  (append first second))

(defun stream-append (&rest lists)
  (reduce #'stream-append-2 lists))

(defmethod print-object ((obj stream-cons) stream)
  (let ((list-form (to-list (take 20 obj))))
    (if (>= (length list-form) 20)
	(format stream "(~{~S ~} ...)" list-form)
	(format stream "~S" list-form))))


(defmacro stream-cons (car cdr)
  `(make-instance 'stream-cons :car-function (lambda () ,car)
		   :cdr-function (lambda () ,cdr)))

(defun stream-map (func stream)
  (cond ((stream-null? stream)
	 nil)
	(t (stream-cons (funcall func (head stream))
			(stream-map func (tail stream))))))

(defun test (n)
  (make-stream-cons :car-function (lambda () n)
		  :cdr-function (lambda () (test (1+ n)))))


(defun list->stream (list)
  (and list
       (stream-cons (car list)
		    (list->stream (cdr list)))))

(defun string->stream (string)
  (list->stream (coerce string 'list)))

(defun stream->list (stream)
  (loop for s = stream then (tail s)
       until (stream-null? s) collect (head s)))

(defun stream->string (stream)
  (coerce (stream->list stream) 'string))

(defgeneric stream-reverse (obj))

(defmethod stream-reverse ((obj sequence))
  (reverse obj))

(defmethod stream-reverse ((obj stream-cons))
  (cond ((stream-null? obj)
	 nil)
	(t (stream-append
	    (stream-reverse (tail obj))
	    (stream-cons (head obj) nil)))))
			  

(defgeneric head (obj))
(defgeneric tail (obj))
(defgeneric take (n obj))
(defgeneric drop (n obj))
(defgeneric to-list (obj))
(defgeneric filter (func obj &key key start end))

(defmethod head ((obj stream-cons))
  (stream-car obj))

(defmethod head ((obj cons))
  (car obj))

(defmethod head ((obj null))
  nil)

(defmethod tail ((obj stream-cons))
  (stream-cdr obj))

(defmethod tail ((obj cons))
  (cdr obj))

(defmethod tail ((obj null))
  nil)

(defmethod take (n (obj stream-cons))
  (stream-cons (head obj)
	     (if (= n 1)
		 nil
		 (take (1- n) (tail obj)))))

(defmethod take (n (obj null))
  nil)

(defmethod take (n (obj cons))
  (loop for item in obj
     repeat 5
     collect item))

(defun take-while-up-to (fn obj limit)
  (let ((counter 0))
    (take-while (lambda (elem)
		  (prog1 (and (funcall fn elem)
			      (< counter limit))
		    (incf counter)))
		obj)))

(defmethod take-while (fn (obj cons))
  (loop for item in obj
     while (funcall fn item)
       collect item))

(defmethod take-while (fn (obj stream-cons))
  (cond
    ((null obj) nil)
    ((funcall fn (head obj))
     (stream-cons (head obj)
		  (take-while fn (tail obj))))
    (t nil)))

(defmethod take-while (fn (obj null))
  nil)

(defmethod to-list ((obj stream-cons))
  (loop for cons = obj then (tail cons)
     until (null cons)
       collect (head cons)))

(defun list->stream (list)
  (and list
       (stream-cons (car list)
		  (list->stream (cdr list)))))

(defmethod drop (n obj)
  (loop for cons = obj then (tail cons)
     repeat n
     finally (return cons)))

(defmethod drop-while (fn obj)
  (loop for cons = obj then (tail cons)
     while (and (not (stream-null? cons))
		(funcall fn (head cons)))
       finally (return cons)))

(defun split-at (n obj)
  (values (take n obj)
	  (drop n obj)))

(defun split-where (fn obj)
  (values (take-while (lambda (item)
			(not (funcall fn item)))
		      obj)
	  (drop-while (lambda (item)
			(not (funcall fn item)))
		      obj)))

(defmethod filter (func (obj stream-cons)
		   &key (key #'identity) (start 0) end)
  " BUG: The following program can crash SBCL:
      (filter (lambda (n)
                (< n 10))
              (drop 8 (numbers 1)))

      The problem is that FILTER never sees the end of the list because NUMBERS
      never ends the list. Instead of going into an infinite loop (which is what
      would happen in Haskell), you end up in LDB. "
  (let* ((next (funcall key (head obj)))
	 (include? (or (not (= start 0))
		       (and end (< end 0))
		       (funcall func next))))	  
    (cond ((and (= start 0)
		(or (not end)
		    (> end 0))
		(not include?))
	   (filter func (tail obj)
		   :key key :start 0 :end (and end
					       (1- end))))
	  (t (stream-cons next
			(filter func (tail obj)
				:key key :start (max 0 (1- start))
				:end (and end (1- end))))))))

(defun from-n (start)
  (stream-cons start
	     (from-n (1+ start))))

(defmethod filter (func (obj null)
		   &key key start end)
  (declare (ignore key start end))
  nil)

(defun drop-test ()
  (time (drop 1000000 (from-n 0))))

(defun zippotest ()
  "This proves that this implementation does not suffer from the SRFI-40 memory leak." 
  (let ((timeszippo
	 (filter (lambda (x)
		   (= 0 (mod x 100000)))
		 (from-n 0))))
    (stream-cdr timeszippo)))
