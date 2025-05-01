;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

#.(unless (or #+asdf3.1 (version<= "3.1" (asdf-version)))
    (error "You need ASDF >= 3.1 to load this system correctly."))

(defsystem :iolib.asdf
  :description "A few ASDF component classes."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :depends-on (:alexandria)
  :encoding :utf-8
  :pathname "src/base/"
  :components
  ((:file "asdf")))
