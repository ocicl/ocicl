;;; version-string.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(defpackage #:version-string
  (:use #:cl)
  (:export #:make-version-string #:get-base-version #:define-version-parameter))

(in-package #:version-string)

(defun get-git-hash (&optional (length 7))
  "Get the current git commit hash (short form by default)"
  (handler-case
      (let ((gh (string-trim '(#\Newline #\Return #\Space)
                             (uiop:run-program (list "git" "rev-parse"
                                                     (format nil "--short=~d" length)
                                                     "HEAD")
                                               :output :string
                                               :error-output nil
                                               :ignore-error-status t))))
        (if (zerop (length gh)) nil gh))
    (error () nil)))

(defun get-git-tag ()
  "Get the first git tag that points to the current commit"
  (handler-case
      (let ((output (string-trim '(#\Newline #\Return #\Space)
                                (uiop:run-program '("git" "tag" "--points-at" "HEAD")
                                                 :output :string
                                                 :error-output nil
                                                 :ignore-error-status t))))
        (when (and output (not (string= output "")))
          ;; Just take everything up to the first newline
          (let ((newline-pos (position #\Newline output)))
            (if newline-pos
                (subseq output 0 newline-pos)
                output))))
    (error () nil)))

(defun get-git-dirty-p ()
  "Check if the working directory has uncommitted changes"
  (handler-case
      (let ((output (uiop:run-program '("git" "status" "--porcelain")
                                     :output :string
                                     :error-output nil
                                     :ignore-error-status t)))
        (not (string= (string-trim '(#\Newline #\Return #\Space) output) "")))
    (error () nil)))

(defun get-base-version (system)
  "Extract version from the .asd file"
  (handler-case
      (asdf:component-version (asdf:find-system system))
    (error () "0.0.0")))

(defun make-version-string (system &key include-git-p)
  "Create a version string, optionally including git information"
  (let ((base-version (get-base-version system)))
    (if include-git-p
        (let ((git-tag (get-git-tag))
              (git-hash (get-git-hash))
              (dirty-p (get-git-dirty-p)))
          (cond
            ;; If there's a git tag, use it (with dirty suffix if needed)
            (git-tag
             (format nil "~a~:[~;+dirty~]" git-tag dirty-p))
            ;; Otherwise use base version with git hash
            (git-hash
             (format nil "~a-g~a~:[~;+dirty~]" base-version git-hash dirty-p))
            ;; Fallback to base version
            (t base-version)))
        base-version)))

(defmacro define-version-parameter (symbol system)
  `(progn
     (defparameter ,symbol (version-string:get-base-version ,system))
     (defmethod asdf:perform :before ((op asdf:program-op)
                                      (system (eql (asdf:find-system ,system))))
       (setf ,symbol (version-string:make-version-string ,system :include-git-p t)))))
