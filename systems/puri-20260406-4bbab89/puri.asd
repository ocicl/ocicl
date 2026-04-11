;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; Programmer: Kevin Rosenberg


(in-package #:cl-user)
(defpackage #:puri-system (:use #:cl #:asdf))
(in-package #:puri-system)

(defsystem puri
  :name "cl-puri"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "GNU Lesser General Public License"
  :description "Portable Universal Resource Indentifier Library"
  :components
  ((:file "src"))
  :in-order-to ((test-op (test-op "puri/test"))))

(defsystem puri/test
    :depends-on (:ptester :puri)
    :components
    ((:file "tests"))
    :perform (test-op (o s)
                      (or (funcall (intern (symbol-name '#:do-tests)
		                           (find-package '#:puri/test)))
                          (error "test-op failed"))))
