;;;; -*- mode: Lisp -*-

(asdf:defsystem "autoload"
  :licence "MIT, see COPYING."
  :version "0.0.1"
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://github.com/melisgl/mgl-pax/tree/master/autoload"
  :bug-tracker "https://github.com/melisgl/mgl-pax/issues"
  :source-control (:git "https://github.com/melisgl/mgl-pax.git")
  :description "Bare-bones autoloading facility."
  :defsystem-depends-on ("mgl-pax.asdf")
  :around-compile "mgl-pax.asdf:compile-pax"
  :serial t
  :components ((:file "package")
               (:file "autoload")))
