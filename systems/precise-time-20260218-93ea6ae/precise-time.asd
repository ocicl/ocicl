(asdf:defsystem precise-time
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Precise time measurements"
  :homepage "https://shinmera.com/docs/precise-time/"
  :bug-tracker "https://shinmera.com/project/precise-time/issues"
  :source-control (:git "https://shinmera.com/project/precise-time.git")
  :serial T
  :components ((:file "package")
               (:file "protocol")
               (:file "posix" :if-feature (:and :unix (:not :darwin)))
               (:file "darwin" :if-feature :darwin)
               (:file "windows" :if-feature :windows)
               (:file "mezzano" :if-feature :mezzano)
               (:file "nx" :if-feature :nx)
               (:file "documentation"))
  :defsystem-depends-on (:trivial-features)
  :depends-on (:documentation-utils
               (:feature (:not :mezzano) :cffi))
  :in-order-to ((asdf:test-op (asdf:test-op :precise-time/test))))

(asdf:defsystem precise-time/test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for the precise-time system."
  :homepage "https://shinmera.com/docs/precise-time/"
  :bug-tracker "https://shinmera.com/project/precise-time/issues"
  :source-control (:git "https://shinmera.com/project/precise-time.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:precise-time :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.precise-time.test)))
