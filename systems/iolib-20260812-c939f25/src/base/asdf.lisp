;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- ASDF component classes
;;;

(defpackage :iolib/asdf
  (:nicknames :iolib.asdf)
  (:use :common-lisp)
  (:export #:compile-wrapper)
  #+sb-package-locks
  (:lock t))

(in-package :iolib/asdf)

(defun compile-wrapper (continuation)
  (let ((*readtable* (copy-readtable))
        (uiop:*uninteresting-compiler-conditions*
          (append '(#+sbcl sb-int:package-at-variance)
                  uiop:*uninteresting-compiler-conditions*)))
    (with-standard-io-syntax
      (let (;; Compilation fails because of CFFI types that
            ;; can't be printed readably, so bind to NIL
            (*print-readably* nil))
        (funcall continuation)))))
