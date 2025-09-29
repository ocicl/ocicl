(cl:in-package #:common-lisp-user)

(defpackage #:acclimation
  (:shadow #:condition #:documentation)
  (:use #:common-lisp)
  (:export
   #:condition
   #:report-condition
   #:report-function
   #:interactive-function
   #:with-restarts
   #:documentation
   #:locale
   #:week-start
   #:*locale*
   #:long-day-name
   #:short-day-name
   #:language
   #:english
   #:french
   #:swedish
   #:german
   #:vietnamese
   #:afrikaans
   ))
