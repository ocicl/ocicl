;;;; main.lisp
;;;;
;;;; Main entry point for ocicl.lint
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025 Anthony Green

(in-package #:ocicl.lint)

(defconstant +version+
  (if (boundp '+version+)
      (symbol-value '+version+)
      "0.1.0"))

(defun print-usage ()
  "Print usage information and command line options."
  (format t "cl-lint ~A~%" +version+)
  (format t "Usage: cl-lint [OPTIONS] PATH...~%")
  (format t "Options:~%")
  (format t "  --max-line-length N   Set the maximum allowed line length (default 120)~%")
  (format t "  --fix                 Automatically fix issues where possible~%")
  (format t "  --dry-run             Show what would be fixed without modifying files~%")
  (format t "  --verbose             Enable verbose output~%")
  (format t "  -h, --help            Show this help~%"))

(defun parse-args (argv)
  "Parse command line arguments. Returns (values status data)."
  (let ((max-len nil)
        (verbose nil)
        (fix nil)
        (dry-run nil)
        (paths nil))
    (loop while argv do
         (let ((a (pop argv)))
           (cond
             ((or (string= a "-h") (string= a "--help"))
              (return (values :help nil)))
             ((string= a "--verbose")
              (setf verbose t))
             ((string= a "--fix")
              (setf fix t))
             ((string= a "--dry-run")
              (setf dry-run t))
             ((string= a "--max-line-length")
              (unless argv
                (return (values :error "Missing number for --max-line-length")))
              (let ((n (ignore-errors (parse-integer (pop argv)))))
                (unless (and n (> n 0))
                  (return (values :error "Invalid number for --max-line-length")))
                (setf max-len n)))
             (t
              (push a paths)))))
    (setf paths (nreverse paths))
    (if (null paths)
        (values :error "No input paths provided")
        (values :ok (list :max-line-length max-len :verbose verbose
                          :fix fix :dry-run dry-run :paths paths)))))

(defun %print-backtrace-and-exit (condition)
  "Print error message with backtrace and exit."
  (format *error-output* "~&cl-lint: error: ~A~%" condition)
  #+sbcl
  (ignore-errors
    (let ((*debug-io* *error-output*))
      (sb-debug:backtrace 50)))
  #+ccl
  (ignore-errors (ccl:print-call-history *error-output*))
  (uiop:quit 2))

(defparameter *use-color* nil
  "Whether to use color output in linting results.")

(defun lint-files (paths &key max-line-length color fix dry-run)
  "Lint the given files/directories. Returns number of issues found.
   PATHS is a list of file or directory paths to lint.
   If FIX is non-nil, automatically fix issues where possible.
   If DRY-RUN is non-nil, show what would be fixed without modifying."
  (let ((*use-color* color)
        (*fix-mode* fix)
        (*dry-run* dry-run))
    (multiple-value-bind (issues issue-count file-count fixed-count)
        (lint-paths paths :max-line-length max-line-length)
      (mapc #'print-issue issues)
      (cond
        ((and fix (not dry-run) (plusp fixed-count))
         (format t "~%Fixed ~D issue(s), ~D issue(s) remaining.~%"
                 fixed-count (- issue-count fixed-count))
         (format t "Scanned ~D file(s).~%" file-count))
        (dry-run
         (let ((fixable (count-if #'fixable-p issues)))
           (format t "~%Would fix ~D issue(s) (use --fix without --dry-run to apply).~%"
                   fixable)
           (format t "Scanned ~D file(s), found ~D issue(s).~%" file-count issue-count)))
        (t
         (format t "~%Scanned ~D file(s), found ~D issue(s).~%"
                 file-count issue-count)))
      issue-count)))

(defun main ()
  "CLI entry point. Catches errors, prints backtrace, avoids debugger."
  (let ((argv (uiop:command-line-arguments)))
    (labels ((run ()
               (multiple-value-bind (status data) (parse-args argv)
                 (ecase status
                   (:help (print-usage) (uiop:quit 0))
                   (:error (progn (print-usage)
                                  (format *error-output* "Error: ~A~%" data)
                                  (uiop:quit 2)))
                   (:ok
                    (destructuring-bind (&key max-line-length verbose fix dry-run paths) data
                      (setf ocicl.lint::*verbose* verbose)
                      (let ((issue-count (lint-files paths
                                                     :max-line-length max-line-length
                                                     :fix fix
                                                     :dry-run dry-run)))
                        (uiop:quit (if (plusp issue-count) 1 0)))))))))
      #+sbcl
      (let ((sb-ext:*invoke-debugger-hook*
              (lambda (c h)
                (declare (ignore h))
                (%print-backtrace-and-exit c))))
        (handler-bind ((error (lambda (e) (%print-backtrace-and-exit e))))
          (run)))
      #-sbcl
      (handler-bind ((error (lambda (e) (%print-backtrace-and-exit e))))
        (run)))))
