
(in-package :cl-graphql)

(defstruct line-counted
  val line col filename readtable)

(defun count-lines (lstream &optional (line 1) (col 1) (filename "STDIN"))
  (cond ((null lstream)
	 nil)
	((member (head lstream) '(#\Newline #\Return))
	 (stream-cons (make-line-counted :val (head lstream)
					 :line line
					 :col col
					 :filename filename)
		      (count-lines (tail lstream) (1+ line) 1 filename)))
	(t
	 (stream-cons (make-line-counted :val (head lstream)
					 :line line
					 :col col
					 :filename filename)
		      (count-lines (tail lstream) line (1+ col) filename)))))

(defun lazy-read (lisp-stream)
  "Lazily reads characters from the LISP-STREAM, closing the stream when the end of file is reached."
  (stream-cons
   (read-char lisp-stream)
   (if (eq (peek-char nil lisp-stream nil :eof) :eof)
       (progn
	 (close lisp-stream)
	 nil)
       (lazy-read lisp-stream))))

(defgeneric raw-ch (ch))

(defmethod raw-ch ((ch line-counted))
  (line-counted-val ch))
(defmethod raw-ch ((ch character))
  ch)
(defmethod raw-ch ((ch null))
  nil)

(defun ch= (ch1 ch2)
  (eql (raw-ch ch1)
       (raw-ch ch2)))

(defun normalize-newlines (lazy-stream)
  (cond ((stream-null? lazy-stream)
	 nil)
	((and (ch= (head lazy-stream) #\Return)
	      (not (stream-null? (tail lazy-stream)))
	      (ch= (head (tail lazy-stream)) #\Newline))
	 (tail lazy-stream))
	((ch= (head lazy-stream) #\Return)
	 (stream-cons #\Newline (tail lazy-stream)))
	(t (stream-cons (head lazy-stream)
			(normalize-newlines (tail lazy-stream))))))

(defun count-lines-from-file (filename)
  "Opens the text file FILENAME and returns a lazy stream. The elements of the stream are all the struct
LINE-COUNTED, which wraps each character (the VAL member of the struct) in an object that describes exactly
where the character was read from."
  (let ((stream (open filename :element-type 'character)))
    (count-lines (normalize-newlines (lazy-read stream)) 1 1 filename)))

(defun strip-line-info (counted-stream)
  "Returns a lazy stream containing just the characters from a COUNT-LINES stream."
  (stream-map #'raw-ch counted-stream))

(defun counted-stream->string (counted-stream)
  (coerce (mapcar #'raw-ch (stream->list counted-stream)) 'string))

(defun copy-line-count-info (value line-counted)
  "Wraps the VALUE in a LINE-COUNTED object with the same line, column, file, and readtable as the one provided."
  (if (line-counted-p line-counted)
      (make-line-counted
       :val value
       :line (line-counted-line line-counted)
       :col (line-counted-col line-counted)
       :filename (line-counted-filename line-counted)
       :readtable (line-counted-readtable line-counted))
      value))

(defun hash->alist (h)
  (let ((result nil))
    (maphash (lambda (k v)
	       (push (list k v) result))
	     h)
    result))

(defmacro as-> (initial var &body body)
  (if (null body)
      initial
      `(let ((,var ,initial))
	 (as-> ,(car body) ,var
	       ,@(cdr body)))))

(defmacro aif (expr if-true &optional if-false)
  `(let ((it ,expr))
     (if it ,if-true ,if-false)))
