;;;; -*- mode: Lisp -*-

;;; This is in a separate .asd file to help OS-level packaging
;;; (https://github.com/melisgl/try/issues/5) by making the dependency
;;; graph of .asd files (as opposed to that of ASDF systems) acyclic.
(defsystem "named-readtables-test"
  :description "Test suite for the Named-Readtables library."
  :author "Tobias C. Rittweiler <trittweiler@common-lisp.net>"
  :maintainer "Gábor Melis <mega@retes.hu>"
  :mailto "mega@retes.hu"
  :depends-on ("named-readtables" "try")
  :pathname "test"
  :serial t
  :components
  ((:file "package")
   (:file "tests"))
  :perform (test-op (o c) (symbol-call :named-readtables-test '#:test)))
