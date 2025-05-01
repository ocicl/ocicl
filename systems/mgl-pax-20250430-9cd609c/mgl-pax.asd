;;;; -*- mode: Lisp -*-

;;; See MGL-PAX::@PAX-MANUAL for the user guide.
(asdf:defsystem "mgl-pax"
  :licence "MIT, see COPYING."
  :version "0.4.0"
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://melisgl.github.io/mgl-pax"
  :bug-tracker "https://github.com/melisgl/mgl-pax/issues"
  :source-control (:git "https://github.com/melisgl/mgl-pax.git")
  :description "Documentation system, browser, generator. See the
  MGL-PAX::@PAX-MANUAL."
  :long-description "The set of dependencies of the
  [`mgl-pax`][asdf:system] system is kept light, and its heavier
  dependencies are autoloaded via ASDF when the relevant functionality
  is accessed. See the [`mgl-pax/navigate`][asdf:system],
  [`mgl-pax/document`][asdf:system],
  [`mgl-pax/transcribe`][asdf:system] and
  [`mgl-pax/full`][asdf:system] systems. To keep deployed code small,
  client systems should declare an ASDF dependency on this system,
  never on the others, which are intended for autoloading and
  interactive use."
  :depends-on ("mgl-pax-bootstrap" "dref"
               "named-readtables" "pythonic-string-reader")
  :defsystem-depends-on ("mgl-pax.asdf")
  :around-compile "mgl-pax.asdf:compile-pax"
  :components ((:module "src/base/"
                :serial t
                :components ((:file "autoload")
                             (:file "pax")
                             (:file "extension-api")
                             (:file "document-early")
                             (:file "web-early")
                             (:file "transcribe-early")
                             (:file "locatives-early"))))
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax-test"))))

(asdf:defsystem "mgl-pax/navigate"
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  ;; Prevent inheritance of slot values from the MGL-PAX system.
  :homepage ""
  :bug-tracker ""
  :source-control ""
  :description "Support for MGL-PAX::@NAVIGATING-IN-EMACS via Slime's
  `MGL-PAX::@M-.` in [MGL-PAX][mgl-pax::@pax-manual]."
  :long-description "Do not declare a dependency on this system. It is
  autoloaded by Elisp or by accessing the functionality provided if the
  [`mgl-pax`][asdf:system] system is loaded."
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
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage ""
  :bug-tracker ""
  :source-control ""
  :description "Support for MGL-PAX::@GENERATING-DOCUMENTATION in
  [MGL-PAX][mgl-pax::@pax-manual]."
  :long-description "Do not declare a dependency on this system. It is
  autoloaded by Elisp or by accessing the functionality provided if the
  [`mgl-pax`][asdf:system] system is loaded."
  :depends-on ("alexandria" "3bmd" "3bmd-ext-code-blocks" "colorize" "md5"
               "mgl-pax/navigate" "mgl-pax/transcribe" "trivial-utf-8")
  :defsystem-depends-on ("mgl-pax.asdf")
  :around-compile "mgl-pax.asdf:compile-pax"
  :components ((:module "src/document/"
                :serial t
                :components ((:file "url")
                             (:file "markdown")
                             (:file "stream-spec")
                             (:file "docstring")
                             (:file "util")
                             (:file "document-object")
                             (:file "document")
                             (:file "document-util")
                             (:file "browse"))))
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax-test"))))

(asdf:defsystem "mgl-pax/web"
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage ""
  :bug-tracker ""
  :source-control ""
  :description "Web server for MGL-PAX::@BROWSING-LIVE-DOCUMENTATION
  in [MGL-PAX][mgl-pax::@pax-manual]."
  :long-description "Do not declare a dependency on this system. It is
  autoloaded by Elisp or by accessing the functionality provided if the
  [`mgl-pax`][asdf:system] system is loaded."
  :depends-on ("hunchentoot" "mgl-pax/document")
  :defsystem-depends-on ("mgl-pax.asdf")
  :around-compile "mgl-pax.asdf:compile-pax"
  :components ((:module "src/web/"
                :serial t
                :components ((:file "web"))))
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax-test"))))

(asdf:defsystem "mgl-pax/transcribe"
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage ""
  :bug-tracker ""
  :source-control ""
  :description "Support for MGL-PAX::@TRANSCRIPTS in
  [MGL-PAX][mgl-pax::@pax-manual]."
  :long-description "Do not declare a dependency on this system. It is
  autoloaded by Elisp or by accessing the functionality provided if
  the [`mgl-pax`][asdf:system] system is loaded."
  :depends-on ("alexandria" "mgl-pax/navigate")
  :defsystem-depends-on ("mgl-pax.asdf")
  :around-compile "mgl-pax.asdf:compile-pax"
  :components ((:module "src/transcribe/"
                :serial t
                :components ((:file "transcribe"))))
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax-test"))))

(asdf:defsystem "mgl-pax/full"
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage ""
  :bug-tracker ""
  :source-control ""
  :description "The [`mgl-pax`][asdf:system] system with all features
  preloaded except [`mgl-pax/web`][asdf:system]."
  :long-description "Do not declare a dependency on this system. It
  is for interactive use."
  :depends-on ("mgl-pax/navigate" "mgl-pax/document" "mgl-pax/transcribe")
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-pax-test"))))
