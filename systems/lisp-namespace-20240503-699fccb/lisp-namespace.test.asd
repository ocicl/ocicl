#|
  This file is a part of lisp-namespace project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage lisp-namespace.test-asd
  (:use :cl :asdf))
(in-package :lisp-namespace.test-asd)


(defsystem lisp-namespace.test
  :author "Masataro Asai"
  #+asdf3 :mailto #+asdf3 "guicho2.71828@gmail.com"
  :description "test system for lisp-namespace"
  :license "LLGPL"
  :depends-on (:lisp-namespace :uiop :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))
                :serial t))
  :perform (test-op :after (op c)
		    (eval (read-from-string "(5am:run! :lisp-namespace)"))
		    (asdf:clear-system c)))
