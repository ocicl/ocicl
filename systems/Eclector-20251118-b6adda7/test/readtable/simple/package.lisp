(cl:defpackage #:eclector.readtable.simple.test
  (:use
   #:common-lisp
   #:fiveam))

(cl:in-package #:eclector.readtable.simple.test)

(def-suite :eclector.readtable.simple
  :in :eclector.readtable)
