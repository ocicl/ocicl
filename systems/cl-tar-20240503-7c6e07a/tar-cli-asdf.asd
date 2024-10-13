(asdf:defsystem #:tar-cli-asdf
  :version (:read-file-form "version.lisp-expr")
  :author "Eric Timmons <eric@timmons.dev>"
  :description "ASDF system def for CL-TAR"
  :license "MIT"
  :depends-on ("asdf-release-ops")
  :pathname "src/asdf/"
  :components ((:file "package")
               (:file "system" :depends-on ("package"))
               (:file "dependencies-license-op" :depends-on ("package" "system"))))
