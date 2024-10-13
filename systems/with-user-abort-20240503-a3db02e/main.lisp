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
