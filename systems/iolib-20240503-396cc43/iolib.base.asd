;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :iolib.base
  :description "Base IOlib package, used instead of CL."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib.asdf :iolib.conf)
  :depends-on (:iolib.common-lisp :alexandria :split-sequence)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/base/"
  :components
  ((:file "pkgdcl")
   (:file "return-star" :depends-on ("pkgdcl"))
   (:file "types" :depends-on ("pkgdcl" "return-star"))
   (:file "debug" :depends-on ("pkgdcl" "return-star"))
   (:file "conditions" :depends-on ("pkgdcl" "return-star"))
   (:file "defalias" :depends-on ("pkgdcl" "return-star"))
   (:file "deffoldable" :depends-on ("pkgdcl" "return-star"))
   (:file "defobsolete" :depends-on ("pkgdcl" "return-star"))
   (:file "reader" :depends-on ("pkgdcl" "return-star" "conditions"))
   (:file "sequence" :depends-on ("pkgdcl" "return-star"))
   (:file "matching" :depends-on ("pkgdcl" "return-star"))
   (:file "time" :depends-on ("pkgdcl" "return-star"))
   (:file "dynamic-buffer" :depends-on ("pkgdcl" "return-star" "sequence"))))
