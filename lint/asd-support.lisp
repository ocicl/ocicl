;;;; asd-support.lisp
;;;;
;;;; Support for analyzing ASDF system definitions
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025 Anthony Green

(in-package #:ocicl.lint)

(defun %extract-depends (forms)
  "Extract dependencies from ASDF system definition forms."
  (let ((deps nil))
    (labels ((defsystem-p (f)
               (and (consp f)
                    (let ((h (first f)))
                      (or (eq h 'asdf:defsystem)
                          (and (symbolp h) (string-equal (symbol-name h) "DEFSYSTEM")))))))
      (dolist (pair forms)
        (let ((f (first pair)))
          (when (defsystem-p f)
            (let ((v (%plist-value (cddr f) :depends-on)))
              (setf deps (%as-list v)))))))
    (mapcar (lambda (x)
              (etypecase x
                (symbol x)
                (string (intern (string-upcase x) :keyword))))
            deps)))

