;;;; -*- mode: Lisp -*-

(in-package :asdf)

(defclass named-readtables-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op)
                            (c named-readtables-source-file))
  (let ((sb-ext:*derive-function-types* t))
    (call-next-method)))

;;; See NAMED-READTABLES::@NAMED-READTABLES-MANUAL for the user guide.
(defsystem "named-readtables"
  :description "Library that creates a namespace for readtables akin
  to the namespace of packages."
  :author "Tobias C. Rittweiler <trittweiler@common-lisp.net>"
  :maintainer "Gábor Melis <mega@retes.hu>"
  :mailto "mega@retes.hu"
  :homepage "http://melisgl.github.io/named-readtables"
  :bug-tracker "https://github.com/melisgl/named-readtables/issues"
  :source-control (:git "https://github.com/melisgl/named-readtables.git")
  :version "0.9"
  :licence "BSD, see LICENSE"
  :depends-on ("mgl-pax-bootstrap")
  :default-component-class named-readtables-source-file
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "define-api")
               (:file "cruft")
               (:file "named-readtables")
               (:file "doc"))
  :in-order-to ((test-op (test-op "named-readtables-test"))))
