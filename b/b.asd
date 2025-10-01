;;; b.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Your Name

(asdf:defsystem #:b
  :description "A basic application."
  :author      "Your Name"
  :license     "MIT"
  :version     "0.1.0"
  :depends-on  ()
  :serial t
  :components ((:file "src/package")
               (:file "src/main")))
