;;; version-string.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green <green@moxielogic.com>

(asdf:defsystem #:version-string
    :description "Generate version strings."
    :author      "Anthony Green"
    :license     "MIT"
    :version     "1.0.1"
    :depends-on  ()
    :serial t
    :components ((:file "version-string")))

