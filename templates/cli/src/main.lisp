;;; <%= @ app-name %>.asd
;;;
;;; SPDX-License-Identifier: <%= or (@ license) "MIT" %>
;;;
;;; Copyright (C) <%= multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time)) (format nil "~A" year) %> <%= (or (@ author) "Your Name") %>

(in-package #:<%= @ :app-name %>)

(version-string:define-version-parameter +version+ <%= @ :app-name %>)

;;── CLI ────────────────────────────────────────────────────────────────────────

(defun make-app ()
  (let ((n (clingon:make-option :flag :short-name #\n :long-name "dry-run" :key :dry-run
                                :description "Don't really do it.")))
    (clingon:make-command
     :name    "<%= @ app-name %>"
     :version +version+
     :description "A command-line tool"
     :authors (list "<%= (or (@ author) "Your Name") %>")
     :license "<%= (or (@ license) "MIT") %>"
     :usage ""
     :options (list n)
     :handler (lambda (cmd)
                ;; Do something with CMD here.
                )
     :examples '(("Do nothing example:"
                  . "<%= @ app-name %>")
                 ("Do even less:"
                  . "<%= @ app-name %> -n")))))

(defun main ()
  "The main entrypoint."
  (handler-case
      (clingon:run (make-app))
    (error (e)
      (format *error-output* "Error: ~A~%" e)
      (uiop:quit 1))))
