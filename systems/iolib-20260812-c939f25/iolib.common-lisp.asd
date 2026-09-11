;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :iolib.common-lisp
  :description "Slightly modified Common Lisp."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib.asdf :iolib.conf)
  :depends-on (:alexandria)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/new-cl/"
  :components
  ((:file "conduits")
   #+scl (:file "scl-gray-streams")
   (:file "pkgdcl" :depends-on ("conduits" #+scl "scl-gray-streams")
    :perform
    (compile-op :before (o c)
      (symbol-call :iolib.conf '#:load-gray-streams))
    :perform
    (load-op :before (o c)
      (symbol-call :iolib.conf '#:load-gray-streams))
    :perform
    (load-source-op :before (o c)
      (symbol-call :iolib.conf '#:load-gray-streams)))
   (:file "gray-streams"
    :depends-on ("pkgdcl" #+scl "scl-gray-streams"))
   (:file "definitions" :depends-on ("pkgdcl"))
   (:file "types" :depends-on ("pkgdcl"))))

