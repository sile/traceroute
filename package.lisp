(defpackage traceroute
  (:use :common-lisp :sb-alien)
  (:shadow :common-lisp trace)
  (:export trace
           done))
(in-package :traceroute)

(defvar *muffle-compiler-note* '(sb-ext:muffle-conditions sb-ext:compiler-note))

(defvar *native-endian* 
  (if (eq sb-c:*backend-byte-order* :big-endian) :big :little))
