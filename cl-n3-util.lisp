(defpackage :cl-n3-util (:use #:common-lisp)
            (:export :make-string-buffer))
(in-package :cl-n3-util)

(defun make-string-buffer ()
  (make-array 5 :fill-pointer 0 :adjustable t :element-type 'character))
