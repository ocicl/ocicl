;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :iolib.conf
  :description "Compile-time configuration for IOLib."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib.asdf)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/conf/"
  :components
  ((:file "pkgdcl")
   (:file "requires" :depends-on ("pkgdcl"))))
