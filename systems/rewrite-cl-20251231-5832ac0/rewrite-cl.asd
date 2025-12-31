;;;; rewrite-cl.asd - ASDF system definition
;;;; SPDX-License-Identifier: MIT

(asdf:defsystem "rewrite-cl"
  :description "Read, modify, and write Common Lisp source code while preserving whitespace and comments"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("alexandria")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "conditions")
     (:file "util")
     (:module "node"
      :serial t
      :components
      ((:file "protocols")
       (:file "position")
       (:file "whitespace")
       (:file "comment")
       (:file "token")
       (:file "string")
       (:file "character")
       (:file "seq")
       (:file "quote")
       (:file "reader-macro")
       (:file "coerce")
       (:file "forms")))
     (:module "parser"
      :serial t
      :components
      ((:file "reader")
       (:file "whitespace")
       (:file "comment")
       (:file "token")
       (:file "string")
       (:file "character")
       (:file "quote")
       (:file "dispatch")
       (:file "sequence")
       (:file "core")))
     (:module "zip"
      :serial t
      :components
      ((:file "core")
       (:file "move")
       (:file "edit")
       (:file "find")
       (:file "walk")
       (:file "seq")
       (:file "whitespace")))
     (:file "api"))))
  :in-order-to ((test-op (test-op "rewrite-cl/test"))))

(asdf:defsystem "rewrite-cl/test"
  :description "Tests for rewrite-cl"
  :depends-on ("rewrite-cl" "fiveam")
  :serial t
  :components
  ((:module "test"
    :serial t
    :components
    ((:file "package")
     (:file "node-tests")
     (:file "parser-tests")
     (:file "zipper-tests")
     (:file "roundtrip-tests"))))
  :perform (test-op (o s)
             (uiop:symbol-call :rewrite-cl/test :run-tests)))
