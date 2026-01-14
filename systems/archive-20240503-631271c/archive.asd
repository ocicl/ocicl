;;; -*- mode: lisp -*-
(cl:defpackage :archive-system
  (:use :cl))
(cl:in-package :archive-system)

(asdf:defsystem :archive
  :version "0.9"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "Nathan Froyd <froydnj@gmail.com>"
  :description "A package for reading and writing archive (tar, cpio, etc.) files."
  :license "BSD-style (http://opensource.org/licenses/BSD-3-Clause)"
  :depends-on (#+sbcl sb-posix trivial-gray-streams cl-fad)
  :components ((:file "package")
               (:file "conditions" :depends-on ("package"))
               (:file "generics" :depends-on ("package"))
               (:file "macros" :depends-on ("generics"))
               (:file "formats" :depends-on ("macros"))
               (:file "stream" :depends-on ("package"))
               (:file "archive" :depends-on ("generics" "stream" "macros" "formats"))
               (:file "compat" :depends-on ("package"))
               (:file "tar" :depends-on ("compat" "formats" "conditions" "generics" "archive"))
               (:file "cpio" :depends-on ("compat" "formats" "conditions" "generics" "archive"))
               (:static-file "README")
               (:static-file "TODO")
               (:static-file "NEWS")
               (:static-file "LICENSE")))

(defmethod asdf:perform :around ((o asdf:compile-op)
                                 (c (eql (asdf:find-system 'archive))))
  (let ((use-sb-posix #+(and sbcl (not win32)) t))
    (if use-sb-posix
        (let ((*features* (cons :use-sb-posix *features*)))
          (call-next-method))
        (call-next-method))))
