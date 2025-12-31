;;;; package.lisp - Test package definition
;;;; SPDX-License-Identifier: MIT

(defpackage #:rewrite-cl/test
  (:documentation "Test suite for rewrite-cl.")
  (:use #:cl #:fiveam #:rewrite-cl)
  (:import-from #:rewrite-cl.node
                #:whitespace-node-p
                #:newline-node-p
                #:token-node-p
                #:comment-node-p
                #:string-node-p
                #:seq-node-p
                #:quote-node-p
                ;; Additional node functions
                #:node-length
                #:node-printable-only-p
                #:node-sexpr-able-p
                #:node-replace-children
                #:make-syntax-quote-node
                #:make-unquote-node
                #:make-unquote-splicing-node)
  (:import-from #:rewrite-cl.parser
                #:parse-string-all)
  (:import-from #:rewrite-cl.zip
                #:zip-length
                #:zip-nth-child
                #:zip-child-sexprs
                #:zip-whitespace-p)
  (:export #:run-tests))

(in-package #:rewrite-cl/test)

(def-suite rewrite-cl-tests
  :description "All tests for rewrite-cl")

(defun run-tests ()
  "Run all rewrite-cl tests."
  (run! 'rewrite-cl-tests))
