;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Debug helpers.
;;;

(in-package :iolib/base)

(defvar *safety-checks*
  #+iolib-debug t #-iolib-debug nil
  "Enables safety checks throught the IOLib codebase.")

(defmacro debug-only (&body body)
  (when *safety-checks*
    `(progn ,@body)))

(defmacro debug-only* (&body body)
  `(when *safety-checks*
     (progn ,@body)))

(defmacro production-only (&body body)
  (unless *safety-checks*
    `(progn ,@body)))

(defmacro production-only* (&body body)
  `(unless *safety-checks*
     ,@body))
