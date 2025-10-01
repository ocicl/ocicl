;;; cli-test.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Your Name

(asdf:defsystem #:cli-test
  :description "A command-line tool."
  :author      "Your Name"
  :license     "MIT"
  :version     "0.1.0"
  :depends-on (:clingon :version-string)
  :serial t
  :components ((:file "src/package")
               (:file "src/main"))
  :build-operation "program-op"
  :build-pathname "cli-test"
  :entry-point "cli-test:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
