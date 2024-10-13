;;;; -*- mode: Lisp -*-

(in-package :asdf)

(defclass named-readtables-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op)
                            (c named-readtables-source-file))
  (let ((sb-ext:*derive-function-types* t))
    (call-next-method)))

;;; See NAMED-READTABLES::@NAMED-READTABLES-MANUAL for the user guide
;;; (if NAMED-READTABLES/DOC is loaded).
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
  :default-component-class named-readtables-source-file
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "define-api")
               (:file "cruft")
               (:file "named-readtables"))
  :in-order-to ((test-op (test-op "named-readtables/test"))))

(defsystem "named-readtables/test"
  :description "Test suite for the Named-Readtables library."
  :author "Tobias C. Rittweiler <trittweiler@common-lisp.net>"
  :maintainer "Gábor Melis <mega@retes.hu>"
  :mailto "mega@retes.hu"
  :depends-on ("named-readtables" "try")
  :pathname "test"
  :serial t
  :default-component-class named-readtables-source-file
  :components
  ((:file "package")
   (:file "tests"))
  :perform (test-op (o c) (symbol-call :named-readtables-test '#:test)))

;;; MGL-PAX depends on NAMED-READTABLES so we must put documentation
;;; in a separate system in order to be able to use MGL-PAX.
(defsystem "named-readtables/doc"
  :depends-on ("named-readtables" "mgl-pax")
  :pathname "src"
  :components ((:file "doc")))
