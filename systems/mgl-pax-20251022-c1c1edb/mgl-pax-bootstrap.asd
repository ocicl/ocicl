;;; This is in a separate .asd file help OS-level packaging by making
;;; the dependency graph of .asd files (as opposed to just ASDF
;;; systems) acyclic. See https://github.com/melisgl/try/issues/5.
(asdf:defsystem "mgl-pax-bootstrap"
  :licence "MIT, see COPYING."
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :description "Use the [mgl-pax][asdf:system] system. This system is
  not for public consumption but solely for systems on which PAX
  depends and which also use PAX."
  :depends-on ("autoload")
  :defsystem-depends-on ("mgl-pax.asdf")
  :around-compile "mgl-pax.asdf:compile-pax"
  :components ((:module "src/bootstrap/"
                :serial t
                :components ((:file "package")
                             (:file "basics")
                             (:file "pax-world")))))
