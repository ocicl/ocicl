(cl:in-package #:eclector.readtable)

;;; We use DEFVAR instead of DEFPARAMETER to ensure that loading
;;; Eclector a second time does not temporarily reset *READTABLE* to
;;; NIL.
;;;
;;; This is important when Eclector is used as an implementation's
;;; reader and that implementation loads, say, a newer version of
;;; Eclector. With DEFPARAMETER, *READTABLE* would be NIL after
;;; loading this file and compilation (reading to be precise) of the
;;; next file would fail.
(defvar *readtable* nil)
