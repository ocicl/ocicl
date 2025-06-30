;;; <%= @ app-name %>.asd
;;;
;;; SPDX-License-Identifier: <%= or (@ license) "MIT" %>
;;;
;;; Copyright (C) <%= multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time)) (format nil "~A" year) %> <%= (or (@ author) "Your Name") %>

(asdf:defsystem #:<%= @ app-name %>
  :description "A command-line tool."
  :author      "<%= (or (@ author) "Your Name") %>"
  :license     "<%= (or (@ license) "MIT") %>"
  :version     "0.1.0"
  :depends-on (:clingon :version-string)
  :serial t
  :components ((:file "src/package")
               (:file "src/main"))
  :build-operation "program-op"
  :build-pathname "<%= @ app-name %>"
  :entry-point "<%= @ app-name %>:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
