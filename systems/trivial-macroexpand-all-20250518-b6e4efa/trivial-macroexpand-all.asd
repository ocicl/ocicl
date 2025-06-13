;;;; trivial-macroexpand-all.asd

(asdf:defsystem #:trivial-macroexpand-all
  :description "Call each implementation's macroexpand-all equivalent"
  :author "Chris Bagley <chris.bagley@gmail.com>"
  :license "Unlicense"
  :depends-on ((:feature :sbcl (:require "sb-cltl2")))
  :components ((:module "code"
                :serial t
                :components ((:file "packages")
                             (:file "implementation")))))
