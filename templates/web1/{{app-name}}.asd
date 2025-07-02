;;; <%= @ app-name %>.asd
;;;
;;; SPDX-License-Identifier: <%= (or (@ license) "MIT") %>
;;;
;;; Copyright (C) 2025 <%= (or (@ author) "Your Name") %>

(asdf:defsystem #:<%= @ app-name %>
  :description "A web application"
  :author      "<%= (or (@ author) "Your Name") %>"
  :license     "<%= (or (@ license) "MIT") %>"
  :version     "0.1.0"
  :depends-on  (:version-string :clingon :hunchentoot :easy-routes :log4cl
                                :cl-dotenv :slynk)
  :serial      t
  :components  ((:file "src/package")
                (:file "src/server")
                (:file "src/main"))
  :build-operation "program-op"
  :build-pathname "<%= @ app-name %>"
  :entry-point "<%= @ app-name %>:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
