;;;; package.lisp
;;;;
;;;; Package definition for ocicl.lint
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025 Anthony Green

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
  (:export #:lint-files))

(in-package #:ocicl.lint)
