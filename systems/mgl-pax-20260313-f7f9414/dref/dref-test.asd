;;;; -*- mode: Lisp -*-

;;; This is in a separate .asd file help OS-level packaging by making
;;; the dependency graph of .asd files (as opposed to just ASDF
;;; systems) acyclic. See https://github.com/melisgl/try/issues/5.

(asdf:defsystem "dref-test"
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage ""
  :bug-tracker ""
  :source-control ""
  :description "Test system for DREF."
  :long-description ""
  :depends-on ("dref/full" "mgl-pax" "try")
  :defsystem-depends-on ("mgl-pax.asdf")
  :around-compile "mgl-pax.asdf:compile-pax"
  :components ((:module "test/"
                :serial t
                :components ((:file "package")
                             (:file "test-defs")
                             (:file "test-locate")
                             (:file "test"))))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call '#:dref-test '#:test)))

(asdf:defsystem "dref-test/autoload"
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage ""
  :bug-tracker ""
  :source-control ""
  :description "Test system for DREF autoload."
  :long-description "Runnable by test/test.sh only."
  :depends-on ("dref" "try")
  :defsystem-depends-on ("mgl-pax.asdf")
  :around-compile "mgl-pax.asdf:compile-pax"
  :components ((:module "test"
                :serial t
                :components ((:file "test-autoload")))))
