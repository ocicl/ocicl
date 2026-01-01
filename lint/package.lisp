;;;; package.lisp
;;;;
;;;; Package definition for ocicl.lint
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025, 2026 Anthony Green

(defpackage #:ocicl.lint
  (:documentation "A Common Lisp linter with comprehensive style and error checking.")
  (:use #:cl)
  (:import-from #:ecclesia
                #:canonicalize-ordinary-lambda-list
                #:canonicalize-macro-lambda-list
                #:separate-function-body
                #:canonicalize-declaration-specifiers
                #:extract-lambda-list-variables
                #:parse-destructuring-bind)
  (:import-from #:alexandria
                #:when-let
                #:if-let
                #:emptyp
                #:switch
                #:nconcf)
  (:import-from #:serapeum
                #:nand
                #:append1
                #:drop)
  (:export #:lint-files
           #:*fix-mode*
           #:*dry-run*))

(in-package #:ocicl.lint)
