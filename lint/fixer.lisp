;;;; fixer.lisp
;;;;
;;;; Core fix infrastructure for ocicl.lint auto-fix feature
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025 Anthony Green

(in-package #:ocicl.lint)

;;; Dynamic variables for fix mode

(defvar *fix-mode* nil
  "When non-nil, apply automatic fixes to issues.")

(defvar *dry-run* nil
  "When non-nil with *fix-mode*, show what would be fixed without modifying files.")

;;; Fixer registry

(defvar *fixers* (make-hash-table :test 'equal)
  "Hash table mapping rule names to fixer functions.
Each fixer function takes (content issue) and returns new content or nil.")

(defun register-fixer (rule-name fixer-fn)
  "Register FIXER-FN as the fixer for RULE-NAME."
  (setf (gethash rule-name *fixers*) fixer-fn))

(defun get-fixer (rule-name)
  "Get the fixer function for RULE-NAME, or nil if none registered."
  (gethash rule-name *fixers*))

(defun fixable-p (issue)
  "Return non-nil if ISSUE has a registered fixer."
  (get-fixer (issue-rule issue)))

;;; Backup support - RCS preferred, .bak fallback

(defun rcs-available-p ()
  "Check if RCS ci/co commands are available."
  (and (program-in-path-p "ci")
       (program-in-path-p "co")))

(defun program-in-path-p (program)
  "Check if PROGRAM is available in PATH."
  (handler-case
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program (list "which" program)
                            :output nil
                            :error-output nil
                            :ignore-error-status t)
        (declare (ignore output error-output))
        (zerop exit-code))
    (error () nil)))

(defun rcs-dir-exists-p (path)
  "Check if an RCS directory already exists for PATH's directory."
  (let ((dir (uiop:pathname-directory-pathname path)))
    (uiop:directory-exists-p (merge-pathnames "RCS/" dir))))

(defun backup-file-rcs (path)
  "Create RCS backup of PATH using ci -l. Only uses existing RCS/ directory."
  (handler-case
      (uiop:run-program
       (list "ci" "-l" "-m'Before ocicl lint --fix'" (namestring path))
       :output nil
       :error-output nil
       :ignore-error-status nil)
    (error (e)
      (when *verbose*
        (logf "; RCS backup failed for ~A: ~A~%" path e))
      nil)))

(defun backup-file-bak (path)
  "Create .bak backup of PATH."
  (let ((bak-path (make-pathname :defaults path
                                 :type (concatenate 'string
                                                    (or (pathname-type path) "lisp")
                                                    ".bak"))))
    (handler-case
        (progn
          (uiop:copy-file path bak-path)
          t)
      (error (e)
        (when *verbose*
          (logf "; Backup failed for ~A: ~A~%" path e))
        nil))))

(defvar *backed-up-files* (make-hash-table :test 'equal)
  "Track which files have been backed up in this session.")

(defun backup-file (path)
  "Create backup of PATH before modification.
Uses RCS if ci/co available AND RCS/ directory exists, else .bak.
Only backs up each file once per session."
  (let ((path-str (namestring path)))
    (unless (gethash path-str *backed-up-files*)
      (when-let ((success (if (and (rcs-available-p) (rcs-dir-exists-p path))
                              (backup-file-rcs path)
                              (backup-file-bak path))))
        (setf (gethash path-str *backed-up-files*) t)
        (when *verbose*
          (logf "; Backed up ~A~%" path))
        success))))

;;; Fix application

(defun apply-fix (path issue)
  "Apply single fix for ISSUE to file at PATH. Re-parses file each time.
Returns t if fix was applied, nil otherwise."
  (when-let ((fixer (get-fixer (issue-rule issue))))
    (let ((content (uiop:read-file-string path)))
      (handler-case
          (let ((new-content (funcall fixer content issue)))
            (when (and new-content (not (string= content new-content)))
              (with-open-file (out path :direction :output
                                       :if-exists :supersede
                                       :external-format :utf-8)
                (write-string new-content out))
              t))
        (error (e)
          (when *verbose*
            (logf "; Fix failed for ~A at ~A:~A: ~A~%"
                  (issue-rule issue) (issue-line issue) (issue-column issue) e))
          nil)))))

(defun apply-fixes-to-file (path issues)
  "Apply all fixes for ISSUES to PATH, one at a time, re-parsing between.
Returns count of fixes applied."
  ;; Backup before any modifications
  (backup-file path)
  (let ((fixed 0))
    ;; Sort by position descending to minimize offset issues
    (dolist (issue (sort (copy-list issues) #'>
                         :key (lambda (i)
                                (+ (* (issue-line i) 100000)
                                   (issue-column i)))))
      (when (apply-fix path issue)
        (incf fixed)))
    fixed))

;;; String utilities for fixers

(defun split-lines (content)
  "Split CONTENT into list of lines, preserving line endings info."
  (let ((lines nil)
        (start 0)
        (len (length content)))
    (loop for i from 0 below len
          for c = (char content i)
          when (char= c #\Newline)
            do (push (subseq content start i) lines)
               (setf start (1+ i)))
    ;; Handle last line without newline
    (when (< start len)
      (push (subseq content start) lines)) ; lint:suppress use-serapeum-drop
    (nreverse lines)))

(defun join-lines (lines)
  "Join LINES with newlines."
  (format nil "~{~A~^~%~}" lines))

;;; Serialization that handles multi-form files correctly

(defun zip-root-content-string (zipper)
  "Return string representation of zipper's root, handling multi-form files correctly.
When rewrite-cl wraps multiple top-level forms in a synthetic list, this returns
just the children's content without the wrapper parens."
  (let* ((root-z (rewrite-cl:zip-root zipper))
         (root (rewrite-cl:zip-node root-z))
         (tag (rewrite-cl:zip-tag root-z)))
    (if (eql tag :list)
        ;; Check if this looks like a synthetic wrapper (list node with no position)
        (if-let ((pos (rewrite-cl.node:node-position root)))
          ;; Real list node with position - serialize normally
          (rewrite-cl.node:node-string root)
          ;; Synthetic wrapper - serialize children only
          (rewrite-cl.node:nodes-string (rewrite-cl.node:node-children root)))
        ;; Single form - serialize normally
        (rewrite-cl.node:node-string root))))

;;; Position finding utilities for AST-based fixers

(defun find-list-at-position (z target-line target-col)
  "Walk zipper Z to find list node at TARGET-LINE/TARGET-COL."
  (let ((result nil))
    (rewrite-cl:zip-walk
     z
     (lambda (node)
       (when (zip-list-p node)
         (when-let ((pos (zip-pos node)))
           (let ((line (first pos))
                 (col (rest pos)))
             (when (and (= line target-line)
                        (= col target-col))
               (setf result node)))))))
    result))

(defun find-symbol-at-position (z target-line target-col)
  "Walk zipper Z to find symbol node at TARGET-LINE/TARGET-COL."
  (let ((result nil))
    (rewrite-cl:zip-walk
     z
     (lambda (node)
       (when (zip-symbol-p node)
         (when-let ((pos (zip-pos node)))
           (let ((line (first pos))
                 (col (rest pos)))
             (when (and (= line target-line)
                        (= col target-col))
               (setf result node)))))))
    result))
