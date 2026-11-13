;;;; cl-cancel.asd
;;;;
;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(asdf:defsystem #:cl-cancel
  :description "Cancellation propagation library for Common Lisp with deadlines and timeouts"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:bordeaux-threads
               #:atomics
               #:precise-time)
  :components ((:file "package")
               (:file "cancellable")
               (:file "deadline")
               (:file "streams"))
  :in-order-to ((test-op (test-op #:cl-cancel/tests))))

(asdf:defsystem #:cl-cancel/tests
  :description "Tests for cl-cancel"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :depends-on (#:cl-cancel
               #:fiveam)
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "cancellable-tests"))))
  :perform (test-op (o c) (symbol-call :fiveam :run! :cl-cancel)))
