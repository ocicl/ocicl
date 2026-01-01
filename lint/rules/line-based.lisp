;;;; line-based.lisp
;;;;
;;;; Line-based linting rules for text formatting and structure
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025, 2026 Anthony Green

(in-package #:ocicl.lint)

(defun rule-trailing-whitespace (path lines)
  "Check for trailing whitespace at the end of lines."
  (loop for line in lines
        for ln from 1
        when (and (> (length line) 0)
                  (let ((last (char line (1- (length line)))))
                    (or (char= last #\Space) (char= last #\Tab))))
          collect (%make-issue path ln (length line) "trailing-whitespace"
                               "Line has trailing whitespace")))

(defun rule-no-tabs (path lines)
  "Check for tab characters, recommending spaces instead."
  (loop for line in lines
        for ln from 1
        for pos = (position #\Tab line)
        when pos
          collect (%make-issue path ln (1+ pos) "no-tabs"
                               "Tab character found (use spaces instead)")))

(defun rule-max-line-length (path lines &key (limit 120))
  "Check for lines exceeding the specified character limit."
  (when *verbose*
    (logf "; max-line-length: using limit ~D for ~A~%" limit path))
  (loop for line in lines
        for ln from 1
        when (> (length line) limit)
          collect (%make-issue path ln (1+ limit) "max-line-length"
                               (format nil "Line exceeds ~D characters" limit))))

(defun rule-multiple-blank-lines (path lines &key (max-blank 2))
  "Check for excessive consecutive blank lines."
  (loop with blanks = 0
        for line in lines
        for ln from 1
        do (if (every (lambda (c) (or (char= c #\Space) (char= c #\Tab))) line)
               (incf blanks)
               (setf blanks 0))
        when (> blanks max-blank)
          collect (%make-issue path ln 1 "consecutive-blank-lines"
                               (format nil "More than ~D consecutive blank lines" max-blank))))

(defun rule-final-newline (path)
  "Check that files end with a newline character."
  (let* ((content (uiop:read-file-string path))
         (ok (and (> (length content) 0)
                  (char= (char content (1- (length content))) #\Newline))))
    (unless ok
      (list (%make-issue path 0 0 "final-newline" "File must end with a newline")))))


;; Rule: Require SPDX license identifier
(defun rule-spdx-license-identifier (path lines)
  "Check for SPDX license identifier in file headers."
  (let ((limit (min (length lines) 20)))
    (unless (loop for line in (take limit lines)
                  thereis (search "SPDX-License-Identifier:" line :test #'char-equal))
      (list (%make-issue path 1 1 "spdx-license-identifier"
                         "Missing SPDX-License-Identifier in file header (add comment like: SPDX-License-Identifier: MIT)")))))

(defun rule-reader-syntax (path)
  "Check for reader syntax errors including unbalanced parentheses and other malformed syntax using rewrite-cl."
  (let ((content (uiop:read-file-string path))
        (issues nil)
        (seen-positions (make-hash-table :test 'equal)))
    (handler-case
        (rewrite-cl:parse-string-all content)
      (error (e)
        ;; Report parse errors - rewrite-cl errors include line/col in their message
        (let* ((error-string (format nil "~A" e))
               (position-key (cons 1 1)))
          ;; Only report if we haven't seen this position before
          (unless (gethash position-key seen-positions)
            (setf (gethash position-key seen-positions) t)
            (push (%make-issue path 1 1 "reader-error" error-string)
                  issues)))))
    (nreverse issues)))
