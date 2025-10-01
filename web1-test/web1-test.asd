;;; web1-test.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Your Name

(asdf:defsystem #:web1-test
  :description "A web application"
  :author      "Your Name"
  :license     "MIT"
  :version     "0.1.0"
  :depends-on  (:version-string :clingon :hunchentoot :easy-routes :log4cl
                                :slynk)
  :serial      t
  :components  ((:file "src/package")
                (:file "src/server")
                (:file "src/main"))
  :build-operation "program-op"
  :build-pathname "web1-test"
  :entry-point "web1-test:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
