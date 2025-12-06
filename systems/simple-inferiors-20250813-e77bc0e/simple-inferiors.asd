(asdf:defsystem simple-inferiors
  :version "1.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A very simple library to use inferior processes."
  :homepage "https://shinmera.com/docs/simple-inferiors/"
  :bug-tracker "https://shinmera.com/project/simple-inferiors/issues"
  :source-control (:git "https://shinmera.com/project/simple-inferiors.git")
  :serial T
  :components ((:file "package")
               (:file "process")
               (:file "documentation"))
  :depends-on (:uiop
               :bordeaux-threads
               :documentation-utils))
