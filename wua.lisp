#|
with-user-abort code copied from ava fox's with-user-abort project.

(asdf:defsystem #:with-user-abort
  :description "provides an easy way to catch ctrl+c. useful for making binaries."
  :author "ava fox"
  :license  "BSD 3-Clause"
  :version "0.1"
  :serial t
  :components ((:file "package")
	       (:file "main")))
|#

(in-package :cl-user)
(defpackage with-user-abort
  (:use :cl)
  (:export :user-abort
	   :with-user-abort))

(in-package :with-user-abort)

(eval-when (compile load eval)

  (defun get-implementation-condition ()
    (quote
     #+sbcl sb-sys:interactive-interrupt
     #+ccl ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     #+lispworks conditions:keyboard-break)))

(define-condition user-abort (#.(get-implementation-condition))
  ()
  (:documentation "condition that inherits from implementation specific interrupt condition"))

(defun user-abort (&optional condition)
  (declare (ignore condition))
  (signal 'user-abort))

#+ccl
(progn
  (defun ccl-break-hook (cond hook)
    "SIGINT handler on CCL."
    (declare (ignore hook))
    (signal cond))

  (setf ccl:*break-hook* #'ccl-break-hook))

(defmacro with-user-abort (&body body)
  "execute BODY, signalling user-abort if the interrupt signal is received"
  `(handler-bind ((#.(get-implementation-condition)

		   #'user-abort))
     ,@body))
