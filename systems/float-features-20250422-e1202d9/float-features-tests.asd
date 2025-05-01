(asdf:defsystem float-features-tests
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for Float Features"
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :float-features-tests))
  :homepage "https://github.com/Shinmera/float-features"
  :serial T
  :components ((:file "test-float-features"))
  :depends-on (:float-features :parachute))
