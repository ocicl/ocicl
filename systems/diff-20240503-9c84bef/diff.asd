;;;; diff.asd - the ASDF system definition for diff -*- lisp -*-
(cl:defpackage #:diff-system
  (:use :cl))

(cl:in-package #:diff-system)

(asdf:defsystem :diff
  :version "0.4"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "Nathan Froyd <froydnj@gmail.com>"
  :depends-on (:cl-ppcre :trivial-gray-streams)
  :components ((:file "package")
               (:file "diff" :depends-on ("package"))
               (:file "patch" :depends-on ("diff"))
               (:file "vdelta" :depends-on ("package"))
               (:file "svndiff" :depends-on ("package" "vdelta"))
               (:static-file "README")
               (:static-file "TODO")
               (:static-file "NEWS")
               (:static-file "LICENSE")))