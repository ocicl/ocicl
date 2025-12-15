;;; package.lisp
;;;
;;; SPDX-License-Identifier: <%= (or (@ license) "MIT") %>
;;;
;;; Copyright (C) 2025 <%= (or (@ author) "Your Name") %>

(defpackage #:<%= @ app-name %>
  (:use #:cl)
  (:documentation "The <%= @ app-name %> package.")
  (:export #:main))
