(asdf:defsystem atomics-test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Test system for the Atomics library"
  :homepage "https://shinmera.com/docs/atomics/"
  :bug-tracker "https://shinmera.com/project/atomics/issues"
  :source-control (:git "https://shinmera.com/project/atomics.git")
  :serial T
  :components ((:file "tests"))
  :depends-on (:parachute :atomics)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :atomics-test)))
