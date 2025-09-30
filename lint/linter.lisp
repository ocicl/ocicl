;;;; linter.lisp
;;;;
;;;; Core linting functionality and issue reporting
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025 Anthony Green

(in-package #:ocicl.lint)

(defstruct issue
  file line column rule message)

(defun %make-issue (file line column rule message)
  "Create an issue structure with given file, position and message."
  (make-issue :file file :line line :column column :rule rule :message message))

(defun lint-file (path &key max-line-length (load-config t))
  "Lint a single file and return list of issues."
  (when load-config
    (load-project-config path))
  (let ((effective-max-line-length (or max-line-length (config-max-line-length))))
    (when *verbose*
      (logf "; lint-file: max-line-length param=~S, config=~S, effective=~S for ~A~%"
            max-line-length (config-max-line-length) effective-max-line-length path))
    (lint-file-internal path :max-line-length effective-max-line-length)))

(defun lint-file-internal (path &key (max-line-length 120))
  "Internal function to lint a file with all available rules."
  (handler-case
      (let* ((lines (read-file-lines path))
             (content (uiop:read-file-string path)))
        (let ((%%parse-context (read-forms-with-full-positions content)))
          (when *verbose*
            (logf "; linter: got ~D parse trees from read-forms-with-full-positions~%"
                  (length (parse-context-parse-trees %%parse-context))))
          (let ((forms-with-pos (read-top-forms-with-pos-ctx %%parse-context)))
            (labels ((safe (name thunk)
                       (handler-case (funcall thunk)
                         (error (e4)
                           ;; Show stack traces only when verbose mode is enabled
                           (when *verbose*
                             (format *error-output* "; INTERNAL ERROR in ~A: ~A~%" name e4)
                             (format *error-output* "; Stack trace:~%")
                             #+sbcl (sb-debug:backtrace 20 *error-output*)
                             #+ccl (ccl:print-call-history *error-output*)
                             (format *error-output* "; End stack trace~%"))
                           ;; Return empty list instead of creating internal error issues
                           nil))))
              (let* ((chunks (list
                             (safe 'trailing-whitespace
                                   (lambda () (rule-trailing-whitespace path lines)))
                             (safe 'no-tabs (lambda () (rule-no-tabs path lines)))
                             (safe 'max-line-length
                                   (lambda ()
                                     (rule-max-line-length path lines :limit max-line-length)))
                             (safe 'multiple-blank-lines
                                   (lambda () (rule-multiple-blank-lines path lines)))
                             (safe 'final-newline (lambda () (rule-final-newline path)))
                             (safe 'in-package (lambda () (rule-in-package-present path lines)))
                             (safe 'spdx-license-identifier
                                   (lambda () (rule-spdx-license-identifier path lines)))
                             (safe 'reader-syntax
                                   (lambda () (rule-reader-syntax path)))
                             (safe 'whitespace-around-parens
                                   (lambda () (rule-whitespace-around-parens path lines)))
                             (safe 'consecutive-closing-parens
                                   (lambda () (rule-consecutive-closing-parens path lines))))))
                (setf chunks (nconc chunks
                                    (list
                                     (safe 'naming-and-packages
                                           (lambda ()
                                             (rule-naming-and-packages path forms-with-pos)))
                                     (safe 'lambda-list-ecclesia
                                           (lambda ()
                                             (rule-lambda-list-ecclesia path forms-with-pos)))
                                     (safe 'unused-parameters
                                           (lambda () (rule-unused-parameters path forms-with-pos)))
                                     (safe 'let-validation
                                           (lambda () (rule-let-validation path forms-with-pos)))
                                     (safe 'single-pass-core
                                           (lambda ()
                                             (run-single-pass-visitors-ctx
                                              path %%parse-context))))))
                (let ((issues (apply #'append chunks)))
                  (sort-issues-by-position (filter-suppressed-issues issues lines))))))))
    (error (e5)
      (list (%make-issue path 0 0 "error" (princ-to-string e5))))))

(defun parse-suppress-comment (line)
  "Parse a line for suppress comments and return list of suppressed rule names.
   Supports syntax: ; lint:suppress rule1 rule2 rule3
   Or: ; lint:suppress (alone) to suppress all rules"
  (let ((comment-pos (position #\; line)))
    (when comment-pos
      (let ((comment-part (subseq line comment-pos)))
        (when (search "lint:suppress" comment-part :test #'char-equal)
          (let* ((suppress-pos (search "lint:suppress" comment-part :test #'char-equal))
                 (after-suppress (subseq comment-part (+ suppress-pos 13))) ; lint:suppress max-line-length
                 (rules-text (string-trim " \t" after-suppress)))
            (if (zerop (length rules-text))
                ;; Empty rules text means suppress all rules
                :all
                ;; Split on whitespace and remove empty strings
                (remove-if (lambda (s) (zerop (length s)))
                           (loop for start = 0 then (1+ pos)
                                 for pos = (position-if (lambda (c) (or (char= c #\Space) (char= c #\Tab))) ; lint:suppress max-line-length
                                                        rules-text :start start)
                                 collect (string-trim " \t" (subseq rules-text start pos))
                                 while pos)))))))))

(defun filter-suppressed-issues (issues lines)
  "Filter out issues that have been suppressed via lint:suppress comments or config."
  (remove-if (lambda (issue)
               (let ((rule-name (issue-rule issue)))
                 ;; Check if rule is globally suppressed
                 (or (rule-suppressed-p rule-name)
                     ;; Check line-specific suppression
                     (let ((line-num (issue-line issue)))
                       (when (and (> line-num 0) (<= line-num (length lines)))
                         (let ((line-content (nth (1- line-num) lines))) ; Convert to 0-based index
                           (let ((suppressed-rules (parse-suppress-comment line-content)))
                             (or (eq suppressed-rules :all)
                                 (member rule-name suppressed-rules :test #'string-equal)))))))))
             issues))

(defun print-issue (iss)
  "Print an issue in standard linter format to stdout."
  (if (and (boundp 'ocicl.lint::*use-color*) ocicl.lint::*use-color*)
      (let ((color-reset (format nil "~c[0m" #\escape))
            (color-bold (format nil "~c[1m" #\escape))
            (color-dim (format nil "~c[2m" #\escape))
            (color-red (format nil "~c[31m" #\escape))      ; Regular red (works on light/dark)
            (color-blue (format nil "~c[34m" #\escape)))    ; Regular blue (works on light/dark)
        (format t "~a~a~a:~a~a~a:~a~a~a: ~a~a~a: ~a~a~a~%"
                color-bold (issue-file iss) color-reset
                color-dim (issue-line iss) color-reset
                color-dim (issue-column iss) color-reset
                color-red (issue-rule iss) color-reset
                color-blue (issue-message iss) color-reset))
      (format t "~A:~A:~A: ~A: ~A~%"
              (issue-file iss)
              (issue-line iss)
              (issue-column iss)
              (issue-rule iss)
              (issue-message iss))))

;; Utilities for ordering issues by source position
(defun %issue-valid-pos-p (iss)
  "Check if an issue has valid line and column position information."
  (let ((l (issue-line iss)) (c (issue-column iss)))
    (and (integerp l) (integerp c) (> l 0) (> c 0))))

(defun %issue-sort-key (iss)
  "Return a numeric key for stable sorting by (line, column). Issues with
unknown positions are ordered after all positioned issues."
  (if (%issue-valid-pos-p iss)
      (+ (* (issue-line iss) 100000) (issue-column iss))
      most-positive-fixnum))

(defun sort-issues-by-position (issues)
  "Return ISSUES stably sorted by line, column; unknown positions at end."
  (stable-sort issues #'< :key #'%issue-sort-key))

(defun lint-paths (paths &key max-line-length)
  "Lint multiple paths and return aggregated results."
  ;; Load config from the first path to establish project-wide settings
  (when paths
    (load-project-config (first paths)))
  (let* ((effective-max-line-length (or max-line-length (config-max-line-length)))
         (files (remove-duplicates (collect-paths paths) :test #'equal))
         (all-issues nil))
    (dolist (f files)
      (dolist (iss (lint-file f :max-line-length effective-max-line-length :load-config nil))
        (push iss all-issues)))
    (setf all-issues (nreverse all-issues))
    (values all-issues (length all-issues) (length files))))
(defparameter *verbose* nil)

(defun logf (fmt &rest args)
  "Log formatted message to error output when verbose mode is enabled."
  (when *verbose*
    (apply #'format *error-output* fmt args)
    (finish-output *error-output*)))
