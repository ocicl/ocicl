;;; <%= @ app-name %>.asd
;;;
;;; SPDX-License-Identifier: <%= or (@ license) "MIT" %>
;;;
;;; Copyright (C) <%= multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time)) (format nil "~A" year) %> <%= (or (@ author) "Your Name") %>

(asdf:defsystem #:<%= @ app-name %>
  :description "A basic application."
  :author      "<%= (or (@ author) "Your Name") %>"
  :license     "<%= (or (@ license) "MIT") %>"
  :version     "0.1.0"
  :depends-on  ()
  :serial t
  :components ((:file "src/package")
               (:file "src/main")))
