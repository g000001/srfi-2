;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-2
  (:export :and-let*))

(defpackage :srfi-2-internal
  (:use :srfi-2 :cl :fiveam))

