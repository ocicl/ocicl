;;; cl-selfupdate.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

;;; Load pure-tls compatibility layer before any cl+ssl-dependent systems
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :pure-tls/cl+ssl-compat)
  (asdf:register-immutable-system "cl+ssl"))

;;; Core system - no HTTP client dependency
(asdf:defsystem #:cl-selfupdate
  :description "Self-update functionality for Common Lisp executables via GitHub/GitLab Releases"
  :author      "Anthony Green <green@moxielogic.com>"
  :license     "MIT"
  :version     "0.2.0"
  :depends-on  ("jsown"
                "cl-semver"
                "chipz"
                "archive"
                "zip"
                "flexi-streams"
                "alexandria"
                "cl-ppcre"
                "ironclad"
                "quri")
  :serial t
  :components ((:file "src/package")
               (:file "src/http")
               (:file "src/platform")
               (:file "src/structures")
               (:file "src/provider")
               (:file "src/github")
               (:file "src/gitlab")
               (:file "src/archives")
               (:file "src/validate")
               (:file "src/selfupdate")))

;;; Dexador backend subsystem
(asdf:defsystem #:cl-selfupdate/dexador
  :description "Dexador HTTP backend for cl-selfupdate"
  :author      "Anthony Green <green@moxielogic.com>"
  :license     "MIT"
  :version     "0.2.0"
  :depends-on  ("cl-selfupdate" "dexador")
  :components ((:file "src/http-dexador")))

;;; Drakma backend subsystem
(asdf:defsystem #:cl-selfupdate/drakma
  :description "Drakma HTTP backend for cl-selfupdate"
  :author      "Anthony Green <green@moxielogic.com>"
  :license     "MIT"
  :version     "0.2.0"
  :depends-on  ("cl-selfupdate" "drakma")
  :components ((:file "src/http-drakma")))
