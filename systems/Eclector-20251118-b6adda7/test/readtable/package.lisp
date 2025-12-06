(cl:defpackage #:eclector.readtable.test
  (:use
   #:common-lisp
   #:fiveam)

  (:export
   #:run-tests))

(cl:in-package #:eclector.readtable.test)

(def-suite :eclector.readtable
  :in :eclector)

(defun run-tests ()
  (run! :eclector.readtable))
