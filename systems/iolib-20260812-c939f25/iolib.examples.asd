;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :iolib.examples
  :description "Examples for IOLib tutorial at http://pages.cs.wisc.edu/~psilord/blog/data/iolib-tutorial/tutorial.html"
  :author "Peter Keller <psilord@cs.wisc.edu>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib.base)
  :depends-on (:iolib :bordeaux-threads)
  :around-compile "iolib/asdf:compile-wrapper"
  :pathname "examples/"
  :components ((:file "package")
               (:file "ex1-client" :depends-on ("package"))
               (:file "ex2-client" :depends-on ("package"))
               (:file "ex3-client" :depends-on ("package"))
               (:file "ex4-client" :depends-on ("package"))
               (:file "ex5a-client" :depends-on ("package"))
               (:file "ex5b-client" :depends-on ("package"))
               (:file "ex1-server" :depends-on ("package"))
               (:file "ex2-server" :depends-on ("package"))
               (:file "ex3-server" :depends-on ("package"))
               (:file "ex4-server" :depends-on ("package"))
               (:file "ex5-server" :depends-on ("package"))
               (:file "ex6-server" :depends-on ("package"))
               (:file "ex7-buffer" :depends-on ("package"))
               (:file "ex7-server" :depends-on ("package" "ex7-buffer"))
               (:file "ex8-buffer" :depends-on ("package"))
               (:file "ex8-server" :depends-on ("package" "ex8-buffer"))))
