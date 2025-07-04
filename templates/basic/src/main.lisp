;;; main.lisp
;;;
;;; SPDX-License-Identifier: <%= or (@ license) "MIT" %>
;;;
;;; Copyright (C) <%= multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time)) (format nil "~A" year) %> <%= (or (@ author) "Your Name") %>

(in-package #:<%= @ :app-name %>)

(defun main ()
  "Entry point for the application."
  (format t "Hello, world!~%"))
