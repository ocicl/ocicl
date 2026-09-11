;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Type definitions and constructors
;;;

(in-package :iolib/common-lisp)

(deftype boolean ()
  'cl:boolean)

(defun boolean (x)
  (if x t nil))
