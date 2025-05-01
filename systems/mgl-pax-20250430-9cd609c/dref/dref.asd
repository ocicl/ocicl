;;;; -*- mode: Lisp -*-

;;; See DREF::@DREF-MANUAL for the user guide.
(asdf:defsystem "dref"
  :licence "MIT, see COPYING."
  :version "0.0.1"
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://melisgl.github.io/mgl-pax"
  :bug-tracker "https://github.com/melisgl/mgl-pax/issues"
  :source-control (:git "https://github.com/melisgl/mgl-pax.git")
  :description "Reify definitions, provide portable access to
  docstrings and source locations in an extensible framework."
  :long-description "DEFUN defines a first-class object: a FUNCTION.
  DEFVAR does not. This library provides a way to refer to all
  definitions and smooths over the differences between
  implementations. This system has minimal dependencies. It autoloads
  the `DREF/FULL` ASDF:SYSTEM, which depends Alexandria and Swank."
  :depends-on ("mgl-pax-bootstrap" "named-readtables" "pythonic-string-reader")
  :defsystem-depends-on ("mgl-pax.asdf")
  :around-compile "mgl-pax.asdf:compile-pax"
  :components ((:module "src/base/"
                :serial t
                :components ((:file "package")
                             (:file "autoload")
                             (:file "util")
                             (:file "dref")
                             (:file "extension-api")
                             (:file "early"))))
  :in-order-to ((asdf:test-op (asdf:test-op "dref-test"))))

(asdf:defsystem "dref/full"
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  ;; Prevent inheritance of slot values from the DREF system.
  :homepage ""
  :bug-tracker ""
  :source-control ""
  :description "DREF with everything loaded. There should be no need
  to explicitly load this system (or depend on it) as it is autoloaded
  as necessary by all publicly accessible functionality in DREF."
  :depends-on ("alexandria" "dref" (:feature (:not :swank) "swank"))
  :defsystem-depends-on ("mgl-pax.asdf")
  :around-compile "mgl-pax.asdf:compile-pax"
  :components ((:module "src/full/"
                :serial t
                :components ((:file "package")
                             (:file "loaded")
                             (:file "util")
                             (:file "swank-util")
                             (:file "dtype")
                             (:file "late")
                             (:file "source-location")
                             (:file "locatives")
                             (:file "doc"))))
  :in-order-to ((asdf:test-op (asdf:test-op "dref-test"))))
