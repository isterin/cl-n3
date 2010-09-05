(defpackage :cl-n3-util (:use #:common-lisp)
            (:export :make-string-buffer
                     :ws-p))
(in-package :cl-n3-util)

(defun make-string-buffer ()
  (make-array 5 :fill-pointer 0 :adjustable t :element-type 'character))

(defun ws-p (char)
  (case char
    ((#\Newline #\Space) t)
    (otherwise nil)))
