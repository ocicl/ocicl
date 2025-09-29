;;;; line-based.lisp
;;;;
;;;; Line-based linting rules for text formatting and structure
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025 Anthony Green

(in-package #:ocicl.lint)

(defun rule-trailing-whitespace (path lines)
  "Check for trailing whitespace at the end of lines."
  (loop for line in lines
        for ln from 1
        when (and (> (length line) 0)
                  (let ((last (char line (1- (length line)))))
                    (or (char= last #\Space) (char= last #\Tab))))
          collect (%make-issue path ln (length line) "trailing-whitespace"
                               "Trailing whitespace")))

(defun rule-no-tabs (path lines)
  "Check for tab characters, recommending spaces instead."
  (loop for line in lines
        for ln from 1
        for pos = (position #\Tab line)
        when pos
          collect (%make-issue path ln (1+ pos) "no-tabs"
                               "Tab character found; use spaces")))

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
      (list (%make-issue path 0 0 "final-newline" "File does not end with a newline")))))


;; Rule: Require SPDX license identifier
(defun rule-spdx-license-identifier (path lines)
  "Check for SPDX license identifier in file headers."
  (let ((limit (min (length lines) 20)))
    (unless (loop for line in (subseq lines 0 limit)
                  thereis (search "SPDX-License-Identifier:" line :test #'char-equal))
      (list (%make-issue path 1 1 "spdx-license-identifier"
                         "Missing SPDX-License-Identifier comment (e.g., ;; SPDX-License-Identifier: MIT)")))))

(defun line-has-in-package-p (line)
  "Check if LINE contains an (in-package ...) form."
  (and (plusp (length line))
       (not (char= (char line 0) #\;))
       (search "(in-package" line :test #'char-equal)))

(defun line-has-defpackage-p (line)
  "Check if LINE contains a (defpackage ...) form."
  (and (plusp (length line))
       (not (char= (char line 0) #\;))
       (search "(defpackage" line :test #'char-equal)))

(defun rule-in-package-present (path lines)
  "Check that files contain either (in-package ...) or (defpackage ...) forms."
  (let ((type (string-downcase (or (pathname-type path) ""))))
    (if (string= type "asd")
        nil
        (let ((has-in-package (some #'line-has-in-package-p lines))
              (has-defpackage (some #'line-has-defpackage-p lines)))
          (unless (or has-in-package has-defpackage)
            (list (%make-issue path 1 1 "in-package"
                               "Missing (in-package ...) or (defpackage ...) form")))))))

(defun rule-reader-syntax (path)
  "Check for reader syntax errors including unbalanced parentheses and other malformed syntax using Eclector."
  (let ((content (uiop:read-file-string path))
        (line-index (build-line-index (uiop:read-file-string path)))
        (client (make-instance 'lenient-parse-client))
        (issues nil)
        (seen-positions (make-hash-table :test 'equal)))
    (with-input-from-string (stream content)
      (handler-bind ((eclector.base:stream-position-reader-error
                      (lambda (e)
                        ;; Extract position information from Eclector error
                        (let* ((stream-pos (eclector.base:stream-position e))
                               (offset (eclector.base:position-offset e))
                               (effective-pos (+ stream-pos offset))
                               (line-col (if effective-pos
                                             (multiple-value-list (index->line/col effective-pos line-index))
                                             '(1 1)))
                               (line (or (first line-col) 1))
                               (col (or (second line-col) 1))
                               (position-key (cons line col)))
                          ;; Only report if we haven't seen this position before
                          (unless (gethash position-key seen-positions)
                            (setf (gethash position-key seen-positions) t)
                            (push (%make-issue path line col "reader-error"
                                              (format nil "~A" e))
                                  issues))
                          ;; Try to recover and continue
                          (let ((restart (find-restart 'eclector.base:recover)))
                            (when restart
                              (invoke-restart restart)))))))
        (loop
          (let ((result (eclector.parse-result:read client stream nil :eof)))
            (when (eql result :eof)
              (return))))))
    (nreverse issues)))
