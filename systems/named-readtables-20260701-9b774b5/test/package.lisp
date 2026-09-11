;;; -*- Mode:Lisp -*-

(cl:defpackage :named-readtables-test
  (:use :common-lisp :named-readtables :try)
  (:import-from :named-readtables
                #:dispatch-macro-char-p
                #:do-readtable
                #:ensure-function
                #:ensure-dispatch-macro-character
                #:function=))
