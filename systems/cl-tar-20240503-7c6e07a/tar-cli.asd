(asdf:defsystem #:tar-cli
  :version (:read-file-form "version.lisp-expr")
  :author "Eric Timmons <eric@timmons.dev>"
  :description "Executable for CL-TAR"
  :license "MIT"
  :defsystem-depends-on ("tar-cli-asdf")
  :class "tar-cli-asdf:tar-cli-system"
  :depends-on ("adopt" "tar" "tar/create" "tar/extract")
  :pathname "src/cli/"
  :entry-point "tar-cli::main"
  :build-operation "asdf-release-ops:dynamic-program-op"
  :build-pathname #-os-windows "../../build/bin/cl-tar" #+os-windows "../../build/bin/cl-tar.exe"
  :components ((:file "package")
               (:file "main" :depends-on ("package")))

  :release-license-file "../../LICENSE"
  :release-readme-file "../../README.md"
  :release-staging-directory "../../build/release-staging/"
  :release-directory "../../releases/"

  :release-structure
  ((:module "bin"
    :components
    ((:program-file "cl-tar")))
   (:module "share"
    :components
    ((:module "cl-tar"
      :append-version t
      :components ((:license-file "LICENSE")
                   (:readme-file "README")
                   (:dependencies-license-file "BUNDLED-LICENSES")))))))
