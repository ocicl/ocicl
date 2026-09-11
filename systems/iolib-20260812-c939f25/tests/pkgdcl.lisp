;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; pkgdcl.lisp --- Package definition.
;;;

(in-package :cl-user)

(defpackage :iolib/tests
  (:nicknames :iolib/tests)
  (:use :5am :iolib/base :iolib :iolib/pathnames)
  #+sb-package-locks
  (:lock t)
  (:export #:*echo-address* #:*echo-port*))
