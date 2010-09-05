;;; Implementation of LL(1) rec descent lexer

(defpackage :cl-n3-lexer (:use #:common-lisp #:cl-n3-util)
            (:export :next-token :token))
(in-package :cl-n3-lexer)

(defclass token ()
  ((name
    :initarg :name)
   (text
    :initarg :text)))

(defmethod print-object ((tk token) stream)
  (with-slots (name text) tk
    (format stream "#<TOKEN {name: ~a, text: ~a}>" name text)))

(defconstant +END-OF-EXPR+ 2)
(defconstant +SEPARATOR+ 3)
(defconstant +DIRECTIVE+ 4)
(defconstant +WORD+ 5)
(defconstant +FQ-IDENTIFIER+ 6)
(defconstant +STRING+ 7)

(defun create-token (name text)
  (make-instance 'token :name name :text text))

(defun create-token-and-consume (name stream)
  (let ((char (consume stream)))
    (create-token name char)))

(defun next-token (stream)
  (tk-ws stream)
  (let ((la-char (lookahead stream)))
    (case la-char
      ('eof nil)
      (#\. (create-token-and-consume +END-OF-EXPR+ stream))
      (#\; (create-token-and-consume +SEPARATOR+ stream))
      (#\@ (create-token +DIRECTIVE+ (tk-word stream)))
      (#\" (create-token +STRING+ (tk-string stream)))
      (#\< (create-token +FQ-IDENTIFIER+ (tk-fq-identifier stream)))
      (otherwise (create-token +WORD+ (tk-word stream))))))

(defun lookahead (stream)
  (peek-char nil stream nil 'eof))

(defun consume (stream)
  (read-char stream NIL))

(defun buffer-until (stream pred &optional (include nil))
  (let ((buffer (make-string-buffer)))
    (loop for char = (lookahead stream)
       until (funcall pred char)
       do  (progn
           (consume stream)
           (vector-push-extend char buffer)))
    (when include
      (let ((char (consume stream)))
      (vector-push-extend char buffer)))
    buffer))

(defun tk-ws (stream)
  (loop for char = (lookahead stream)
     while (ws-p char)
     do (consume stream)))

(defun tk-fq-identifier (stream)
  (consume stream)
  (buffer-until stream (lambda (char) (char= char #\>)) t))

(defun tk-word (stream)
  (buffer-until stream #'ws-p))

(defun tk-string (stream)
  (buffer-until stream (lambda (char) (char= char #\"))))
