(defpackage compile-cl-json
    (:use :common-lisp))

(in-package :compile-cl-json)

(require :asdf)

(asdf:initialize-source-registry '(:source-registry (:directory :here)
                                   :inherit-configuration))

(declaim (optimize (speed 3) (space 3)))

(defun leave-lisp (message return)
  (fresh-line *error-output*)
  (when message
    (format *error-output* message)
    (terpri *error-output*))
  (finish-output *error-output*)
  (finish-output *standard-output*)
  (uiop:quit return))

(defmacro quit-on-error (&body body)
  `(call-quitting-on-error (lambda () ,@body)))

(defun call-quitting-on-error (thunk)
  "Unless the environment variable DEBUG_CL_JSON_TEST
is bound, write a message and exit on an error.  If
*asdf-test-debug* is true, enter the debugger."
  (handler-bind
      ((error (lambda (c)
                (format *error-output* "~&~a~&" c)
                (cond
                  ((ignore-errors (funcall (find-symbol "GETENV" :asdf) "DEBUG_CL_JSON_TEST"))
                   (break))
                  (t
                   (finish-output *standard-output*)
                   (finish-output *trace-output*)
                   (format *error-output* "~&ABORTING:~% ~S~%" c)
                   #+sbcl (sb-debug:backtrace 69)
                   #+clozure (ccl:print-call-history :count 69 :start-frame-number 1)
                   #+clisp (system::print-backtrace)
                   (format *error-output* "~&ABORTING:~% ~S~%" c)
                   (finish-output *error-output*)
                   (leave-lisp "~&Script failed~%" 1))))))
    (funcall thunk)
    (leave-lisp "~&Script succeeded~%" 0)))


(quit-on-error
 (ql:quickload "fiveam")
 (asdf:compile-system "cl-json"))


