;;;; -*- mode: Lisp -*-

;;; See MGL-PAX::@PAX-MANUAL for the user guide.
(asdf:defsystem "mgl-pax"
  :licence "MIT, see COPYING."
  :version (:read-file-form "version.lisp-expr")
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://github.com/melisgl/mgl-pax"
  :bug-tracker "https://github.com/melisgl/mgl-pax/issues"
  :source-control (:git "https://github.com/melisgl/mgl-pax.git")
  :description "Documentation system, browser, generator. See the
  MGL-PAX::@PAX-MANUAL."
  :long-description "The base system. See MGL-PAX::@LINKS-AND-SYSTEMS."
  :depends-on ("mgl-pax-bootstrap" "dref"
               "named-readtables" "pythonic-string-reader")
  :defsystem-depends-on ("mgl-pax.asdf")
  :around-compile "mgl-pax.asdf:compile-pax"
  :components ((:module "src/base/"
                :serial t
                :components ((:file "pax")
                             (:file "extension-api")
                             (:file "document-early")
                             (:file "web-early")
                             (:file "transcribe-early")
                             (:file "locatives-early"))))
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax-test"))))

(asdf:defsystem "mgl-pax/navigate"
  ;; KLUDGE: Prevent inheritance of slot values from the MGL-PAX
  ;; system to keep the generated documentation lean.
  :licence ""
  :author ""
  :mailto ""
  :homepage ""
  :bug-tracker ""
  :source-control ""
  :licence ""
  :description "Support for MGL-PAX::@NAVIGATING-IN-EMACS via Slime's
  `MGL-PAX::@M-.` in [MGL-PAX][mgl-pax::@pax-manual]."
  :depends-on ("alexandria" "mgl-pax" "dref/full"
               (:feature (:not :swank) "swank"))
  :defsystem-depends-on ("mgl-pax.asdf")
  :around-compile "mgl-pax.asdf:compile-pax"
  :components ((:module "src/navigate/"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "hyperspec")
                             (:file "parse")
                             (:file "locatives")
                             (:file "sections")
                             (:file "navigate"))))
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax-test"))))

(asdf:defsystem "mgl-pax/document"
  :licence ""
  :author ""
  :mailto ""
  :homepage ""
  :bug-tracker ""
  :source-control ""
  :description "Support for MGL-PAX::@GENERATING-DOCUMENTATION in
  [MGL-PAX][mgl-pax::@pax-manual]."
  :depends-on ("alexandria" "3bmd" "3bmd-ext-code-blocks" "3bmd-ext-math"
               "colorize" "md5" "mgl-pax/navigate" "mgl-pax/transcribe"
               "trivial-utf-8")
  :defsystem-depends-on ("mgl-pax.asdf")
  :around-compile "mgl-pax.asdf:compile-pax"
  :components ((:module "src/document/"
                :serial t
                :components ((:file "util")
                             (:file "url")
                             (:file "markdown")
                             (:file "pandoc")
                             (:file "stream-spec")
                             (:file "docstring")
                             (:file "document-object")
                             (:file "document")
                             (:file "document-util")
                             (:file "browse"))))
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax-test"))))

(asdf:defsystem "mgl-pax/web"
  :licence ""
  :author ""
  :mailto ""
  :homepage ""
  :bug-tracker ""
  :source-control ""
  :description "Web server for MGL-PAX::@BROWSING-LIVE-DOCUMENTATION
  in [MGL-PAX][mgl-pax::@pax-manual]."
  :depends-on ("hunchentoot" "mgl-pax/document")
  :defsystem-depends-on ("mgl-pax.asdf")
  :around-compile "mgl-pax.asdf:compile-pax"
  :components ((:module "src/web/"
                :serial t
                :components ((:file "web"))))
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax-test"))))

(asdf:defsystem "mgl-pax/transcribe"
  :licence ""
  :author ""
  :mailto ""
  :homepage ""
  :bug-tracker ""
  :source-control ""
  :description "Support for MGL-PAX::@TRANSCRIPTS in
  [MGL-PAX][mgl-pax::@pax-manual]."
  :depends-on ("alexandria" "mgl-pax/navigate")
  :defsystem-depends-on ("mgl-pax.asdf")
  :around-compile "mgl-pax.asdf:compile-pax"
  :components ((:module "src/transcribe/"
                :serial t
                :components ((:file "transcribe"))))
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax-test"))))

(asdf:defsystem "mgl-pax/full"
  :licence ""
  :author ""
  :mailto ""
  :homepage ""
  :bug-tracker ""
  :source-control ""
  :description "The [`mgl-pax`][asdf:system] system with all features
  preloaded."
  :depends-on ("mgl-pax/navigate" "mgl-pax/document"
               ;; KLUDGE: On ABCL, some systems get the error "Class
               ;; not found: com.sun.jna.Pointer" although Maven is
               ;; installed. HUNCHENTOOT depends on BORDEAUX-THREADS,
               ;; which does not support CLISP. On CMUCL, CL+SSL
               ;; doesn't find libcrypto.
               #-(or abcl clisp cmucl) "mgl-pax/web"
               "mgl-pax/transcribe")
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax-test"))))
