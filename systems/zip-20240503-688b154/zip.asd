(defpackage :zip-system
  (:use :cl :asdf))
(in-package :zip-system)

(defclass silent-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s silent-source-file))
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (call-next-method)))

(defsystem :zip
  :default-component-class silent-source-file
  :depends-on (:salza2 :trivial-gray-streams :babel :cl-fad)
  :description "Library for ZIP archive file reading and writing"
  :author "David Lichteblau and contributors <zip-devel@common-lisp.net>"
  :licence "Lisp-LGPL (and some parts BSD-style, see LICENSE for details)"
  :components ((:file "package")
	       (:file "gray" :depends-on ("package"))
	       (:file "ifstar" :depends-on ("package"))
	       (:file "inflate" :depends-on ("package" "ifstar"))
	       (:file "zip" :depends-on ("inflate" "gray"))))
