;;; package.lisp
;;;
;;; SPDX-License-Identifier: <%= or (@ license) "MIT" %>
;;;
;;; Copyright (C) <%= multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time)) (format nil "~A" year) %> <%= (or (@ author) "Your Name") %>

(defpackage #:<%= @ :app-name %>
  (:use #:cl)
  (:export #:main))

(in-package #:<%= @ :app-name %>)
