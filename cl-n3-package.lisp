(defpackage #:cl-n3
  (:use #:common-lisp)
  (:nicknames #:n3)
  (:export)
  (:documentation "A Lisp-style abstract interface to Kyoto
  Cabinet. The original C function names are not preserved (see
  the :kyoto-cabinet-ffi package for functions that do preserve the
  nomenclature)."))

