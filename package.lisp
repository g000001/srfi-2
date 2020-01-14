;;;; package.lisp

(cl:in-package #:cl-user)

(defpackage "https://github.com/g000001/srfi-2"
  (:export #:and-let*))

(defpackage "https://github.com/g000001/srfi-2#internals"
  (:use "https://github.com/g000001/srfi-2" #:cl))

