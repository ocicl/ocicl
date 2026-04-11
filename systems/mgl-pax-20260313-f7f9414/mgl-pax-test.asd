;;;; See comment in mgl-pax-bootstrap.asd for why this is a separate
;;;; .asd file.

(asdf:defsystem "mgl-pax-test"
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage ""
  :bug-tracker ""
  :source-control ""
  :description "Test system for MGL-PAX."
  :long-description ""
  ;; KLUDGE: CMUCL cannot load CL+SSL.
  :depends-on (#-cmucl "mgl-pax/full"
               #+cmucl "mgl-pax/document"
               "dref-test" "try")
  :defsystem-depends-on ("mgl-pax.asdf")
  :around-compile "mgl-pax.asdf:compile-pax"
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "test-defs")
                             (:file "test-util")
                             (:file "test-navigate")
                             (:file "test-document")
                             (:file "test-transcribe")
                             (:file "test"))))
  :perform (asdf:test-op (o s)
             (uiop:symbol-call '#:mgl-pax-test '#:test)))

(asdf:defsystem "mgl-pax-test/extension"
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage ""
  :bug-tracker ""
  :source-control ""
  :description "Test system for MGL-PAX extensions."
  :long-description "Runnable by test/test.sh only."
  :depends-on ("mgl-pax" "try")
  :defsystem-depends-on ("mgl-pax.asdf")
  :around-compile "mgl-pax.asdf:compile-pax"
  :components ((:module "test"
                :serial t
                :components ((:file "test-extension")))))
