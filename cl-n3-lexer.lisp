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
(defconstant +PREFIX-KW+ 3)
(defconstant +WORD+ 4)
(defconstant +FQ-IDENTIFIER+ 5)
(defconstant +STRING+ 6)

(defun create-token (name text)
  (make-instance 'token :name name :text text))

(defun create-token-and-consume (name stream)
  (let ((char (consume stream)))
    (create-token name char)))

(defun next-token (stream)
  (let ((la-char (lookahead stream)))
    (format t "CHAR: ~:c~%" la-char)
    (if (or (char= la-char #\Newline) (char= la-char #\Space))
        (tk-ws stream))
    (case la-char
      (#\. (create-token-and-consume +END-OF-EXPR+ stream))
      (#\@ (create-token +PREFIX-KW+ (tk-word stream)))
      (#\" (create-token +STRING+ (tk-string stream)))
      (#\< (create-token +FQ-IDENTIFIER+ (tk-fq-identifier stream)))
      (otherwise (create-token +WORD+ (word stream))))))

(defun lookahead (stream)
  (peek-char nil stream nil))

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
  (buffer-until stream (lambda (char) (char= char #\>))))

(defun tk-word (stream)
  (buffer-until stream #'ws-p))

(defun tk-string (stream)
  (buffer-until stream (lambda (char) (char= char #\"))))
