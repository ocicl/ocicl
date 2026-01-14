(asdf:defsystem #:cl-semver
  :serial t
  :description "Semantic Version implementation"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:alexandria
               #:esrap
               #:named-readtables)
  :components ((:file "package")
               (:file "semver"))
  :in-order-to ((asdf:test-op (asdf:test-op :cl-semver-test))))
