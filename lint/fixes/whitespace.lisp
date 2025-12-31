;;;; whitespace.lisp
;;;;
;;;; Fixers for whitespace-related lint rules
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025 Anthony Green

(in-package #:ocicl.lint)

;;; Fix: whitespace-after-open-paren
;;; Removes whitespace node that immediately follows opening paren

(defun fix-whitespace-after-open-paren (content issue)
  "Remove whitespace after opening paren at ISSUE location.
The issue column points to the whitespace (col+1 from list position)."
  (let* ((target-line (issue-line issue))
         (target-col (1- (issue-column issue)))  ; List is at col-1
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (when-let ((first-child (rewrite-cl:zip-down target)))
            (when (eq (rewrite-cl:zip-tag first-child) :whitespace)
              (zip-root-content-string
               (rewrite-cl:zip-remove first-child)))))))))

(register-fixer "whitespace-after-open-paren" #'fix-whitespace-after-open-paren)


;;; Fix: whitespace-before-close-paren
;;; Removes whitespace node that immediately precedes closing paren

(defun fix-whitespace-before-close-paren (content issue)
  "Remove whitespace before closing paren at ISSUE location.
The issue column points to the list position."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          (when-let ((first-child (rewrite-cl:zip-down target)))
            (let* ((last-child (rewrite-cl:zip-rightmost first-child))
                   (tag (rewrite-cl:zip-tag last-child)))
              (when (eq tag :whitespace)
                (zip-root-content-string
                 (rewrite-cl:zip-remove last-child))))))))))

(register-fixer "whitespace-before-close-paren" #'fix-whitespace-before-close-paren)


;;; Fix: trailing-whitespace
;;; String-based fix - simpler and more reliable for line-level changes

(defun fix-trailing-whitespace (content issue)
  "Remove trailing whitespace from line at ISSUE location."
  (let* ((lines (split-lines content))
         (line-idx (1- (issue-line issue))))  ; 0-based index
    (when (and (>= line-idx 0)
               (< line-idx (length lines)))
      (let ((line (nth line-idx lines)))
        (setf (nth line-idx lines)
              (string-right-trim '(#\Space #\Tab) line))
        (join-lines lines)))))

(register-fixer "trailing-whitespace" #'fix-trailing-whitespace)


;;; Fix: closing-parens-same-line
;;; Removes newline between consecutive closing parens
;;; Note: The detection rule already skips cases with comments, so this fixer
;;; will only be called when there are no comments between the parens.

(defun fix-closing-parens-same-line (content issue)
  "Remove newline between consecutive closing parens at ISSUE location."
  (let* ((target-line (issue-line issue))
         (target-col (issue-column issue))
         (z (handler-case (rewrite-cl:of-string content)
              (error () nil))))
    (when z
      (let ((target (find-list-at-position z target-line target-col)))
        (when target
          ;; Find the last meaningful child (which should be a list)
          (when-let ((last-meaningful (rewrite-cl.zip:zip-last-child target)))
            (when (zip-list-p last-meaningful)
              ;; Remove newlines between this child and the end
              (let ((result nil)
                    (current (rewrite-cl:zip-right last-meaningful)))
                (loop while current
                      for tag = (rewrite-cl:zip-tag current)
                      do (cond
                           ((eql tag :newline)
                            ;; Remove newline, keep going with updated zipper
                            (setf current (rewrite-cl:zip-remove current)))
                           ((eql tag :whitespace)
                            ;; Also remove any trailing whitespace after newline
                            (setf current (rewrite-cl:zip-remove current)))
                           (t
                            (setf result current)
                            (setf current (rewrite-cl:zip-right current)))))
                (when result
                  (zip-root-content-string result))))))))))

(register-fixer "closing-parens-same-line" #'fix-closing-parens-same-line)
