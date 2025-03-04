(asdf:defsystem float-features
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A portability library for IEEE float features not covered by the CL standard."
  :homepage "https://github.com/Shinmera/float-features"
  :serial T
  :components ((:file "package")
               (:file "float-features")
               (:file "constants")
               (:file "documentation"))
  :in-order-to ((asdf:test-op (asdf:test-op :float-features-tests)))
  :depends-on (:trivial-features :documentation-utils))
