(cl:in-package #:asdf-user)

(defsystem #:ecclesia
  :description "Utilities for parsing Lisp code."
  :author "Robert Strandh <robert.strandh@gmail.com>"
  :license "MIT"
  :depends-on (#:acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-english")
   (:file "argcount")
   (:file "list-structure")
   (:file "form")
   (:file "declarations")
   (:file "lambda-list-parsing")
   (:file "lambda-list-canonicalization")
   (:file "destructuring")
   (:file "canonicalized-destructuring")))
