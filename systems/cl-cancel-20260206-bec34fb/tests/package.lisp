;;;; tests/package.lisp
;;;;
;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(defpackage #:cl-cancel/tests
  (:use #:cl #:cl-cancel #:fiveam)
  (:export #:run-tests))

(in-package #:cl-cancel/tests)

(def-suite :cl-cancel
  :description "Test suite for cl-cancel")

(defun run-tests ()
  "Run all cl-cancel tests"
  (run! :cl-cancel))
