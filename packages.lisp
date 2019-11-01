;; -*- Package: closer-common-lisp; Syntax: Common-Lisp; Base: 10 -*-

(in-package :closer-common-lisp)

(defpackage :better-handler-case
  (:use :closer-common-lisp)
  (:nicknames :handler-case*)
  (:export :handler-case*))

(defpackage :lazy-streams
  (:use :closer-common-lisp #+genera :clos)
  (:export :stream-cons :stream-car :stream-cdr :head :tail :take :drop :split-at :to-list :filter :list->stream
	   :string->stream :drop-while :take-while :take-while-up-to :split-where :stream->list :stream->string :stream-append :stream-null?
	   :stream-map :lazy-streamp :stream-reverse))

(defpackage :cl-graphql
  (:use :closer-common-lisp :lazy-streams :better-handler-case
	:alexandria :split-sequence))

