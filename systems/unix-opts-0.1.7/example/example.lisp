;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; This is example of Unix-opts, a minimalistic parser of command line
;;; options.
;;;
;;; Copyright © 2015–2018 Mark Karpov
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(asdf:load-system :unix-opts)

;;; First of all, we need to define command line options. We do this with
;;; `define-opts' macro.

(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :verbose
   :description "verbose output"
   :short #\v
   :long "verbose")
  (:name :level
   :description "the program will run on LEVEL level"
   :required t
   :short #\l
   :long "level"
   :arg-parser #'parse-integer
   :meta-var "LEVEL")
  (:name :output
   :description "redirect output to file FILE"
   :short #\o
   :long "output"
   :arg-parser #'identity
   :meta-var "FILE"))

;;; OK, since command line options can be malformed we should use a handy
;;; Common Lisp feature: restarts. Unix-opts gives us all we need to do so.
;;; Here we define a function that will print a warning and ignore
;;; unknown-option. Several restarts (behaviors) are available for every
;;; exception that Unix-opts can throw. See documentation for `get-opts'
;;; function for more information.

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(multiple-value-bind (options free-args)
    (handler-case
        (handler-bind ((opts:unknown-option #'unknown-option))
          (opts:get-opts))
      (opts:missing-arg (condition)
        (format t "fatal: option ~s needs an argument!~%"
                (opts:option condition)))
      (opts:arg-parser-failed (condition)
        (format t "fatal: cannot parse ~s as argument of ~s~%"
                (opts:raw-arg condition)
                (opts:option condition)))
      (opts:missing-required-option (con)
        (format t "fatal: ~a~%" con)
        (opts:exit 1)))
  ;; Here all options are checked independently, it's trivial to code any
  ;; logic to process them.
  (when-option (options :help)
    (opts:describe
     :prefix "example—program to demonstrate unix-opts library"
     :suffix "so that's how it works…"
     :usage-of "example.sh"
     :args     "[FREE-ARGS]"))
  (when-option (options :verbose)
    (format t "OK, running in verbose mode…~%"))
  (when-option (options :level)
    (format t "I see you've supplied level option, you want ~a level!~%" it))
  (when-option (options :output)
    (format t "I see you want to output the stuff to ~s!~%"
            (getf options :output)))
  (format t "free args: ~{~a~^, ~}~%" free-args))
