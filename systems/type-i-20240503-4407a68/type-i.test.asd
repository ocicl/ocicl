#|
  This file is a part of type-i project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage type-i.test-asd
  (:use :cl :asdf))
(in-package :type-i.test-asd)


(defsystem type-i.test
  :author "Masataro Asai"
  :description "Test system for type-i"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:type-i
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (load-op :after (op c) ))
