(asdf:defsystem #:with-user-abort
  :description "provides an easy way to catch ctrl+c. useful for making binaries."
  :author "ava fox"
  :license  "BSD 3-Clause"
  :version "0.1"
  :serial t
  :components ((:file "package")
	       (:file "main")))
