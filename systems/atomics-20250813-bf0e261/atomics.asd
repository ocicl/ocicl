(asdf:defsystem atomics
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Portability layer for atomic operations like compare-and-swap (CAS)."
  :homepage "https://shinmera.com/docs/atomics/"
  :bug-tracker "https://shinmera.com/project/atomics/issues"
  :source-control (:git "https://shinmera.com/project/atomics.git")
  :serial T
  :components ((:file "atomics")
               (:file "documentation"))
  :depends-on (:documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :atomics-test))))
