(defpackage :cl-n3-lexer (:use #:common-lisp #:cl-n3-util)
            (:export :next-token :token))
(in-package :cl-n3-lexer)

(defun next-token (stream la-chars matcher)
  ())
  
;  (let ((tk la-chars)
;        (char (next-char stream)))
;    (format t "~a and ~a" char (funcall matcher char))
;    (if (funcall matcher char)
;        (progn
;          (consume-next-char stream)
;          (vector-push-extend char (token stream char matcher))))))

(defun lookahead (stream)
  (peek-char nil stream nil))

(defun consume (stream)
  (read-char stream NIL))

(defun buffer-until (stream pred)
  (let ((buffer (make-string-buffer)))
    (loop for char = (lookahead stream)
       until (funcall pred char)
       do  (progn
           (consume stream)
           (format t "~a~%" char)
           (vector-push-extend char buffer)))
    buffer))


(defun word (stream)
  (buffer-until stream (lambda (char) (null char))))
