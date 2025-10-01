;;; cli-test.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Your Name

(in-package #:cli-test)

(version-string:define-version-parameter +version+ :cli-test)

;;── CLI ────────────────────────────────────────────────────────────────────────

(defun make-app ()
  (let ((n (clingon:make-option :flag :short-name #\n :long-name "dry-run" :key :dry-run
                                :description "Don't really do it.")))
    (clingon:make-command
     :name    "cli-test"
     :version +version+
     :description "A command-line tool"
     :authors (list "Your Name")
     :license "MIT"
     :usage ""
     :options (list n)
     :handler (lambda (cmd)
                ;; Do something with CMD here.
                )
     :examples '(("Do nothing example:"
                  . "cli-test")
                 ("Do even less:"
                  . "cli-test -n")))))

(defun main ()
  "The main entrypoint."
  (handler-case
      (clingon:run (make-app))
    (error (e)
      (format *error-output* "Error: ~A~%" e)
      (uiop:quit 1))))
