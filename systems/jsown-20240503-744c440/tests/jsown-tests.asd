(asdf:defsystem :jsown-tests
  :name "JSOWN tests"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "Tests for the jsoown library"
  :depends-on (:jsown :fiveam)
  :serial t
  :components ((:file "packages")
               (:file "reader")
               (:file "writer")
               (:file "accessors")))
