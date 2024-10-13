;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; This is some tests for Unix-opts library.
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

(in-package #:unix-opts)

(define-opts
  (:name :grab-int
   :description "grab integer INT"
   :short #\i
   :long "grab-int"
   :required t
   :arg-parser #'parse-integer
   :meta-var "INT")
  (:name :grab-str
   :description "grab string STR"
   :short #\s
   :long "grab-str"
   :arg-parser #'identity
   :meta-var "STR")
  (:name :flag-a
   :description "flag with short form only"
   :short #\a)
  (:name :flag-b
   :description "flag with long form only"
   :long "flag-b"))

;;; Here is some variables that we will use and functions to reset them.

(defparameter *unknown-options* nil
  "We collect all unknown options here.")

(defparameter *missing-arg-options* nil
  "Options that need an argument, but don't get one.")

(defparameter *malformed-arguments* nil
  "Here we collect malformed arguments.")

(defparameter *missing-required-options*
  "Here we collect missing required options.")

(defun reset-state ()
  "Reset some special variables that are used to collect data about some
aspects of the tests."
  (setf *unknown-options*          nil
        *missing-arg-options*      nil
        *missing-required-options* nil
        *malformed-arguments*      nil))

(defun finish-collecting ()
  "Call this after parsing."
  (setf *unknown-options*     (nreverse *unknown-options*)
        *missing-required-options* (nreverse *missing-required-options*)
        *missing-arg-options* (nreverse *missing-arg-options*)
        *malformed-arguments* (nreverse *malformed-arguments*)))

;;; The tests themselves.

(defun parse-opts (opts &key unknown-option missing-arg arg-parser-failed missing-required)
  "Parse OPTS, return results and collect some data in special variables.
Keyword arguments allow to set arguments for `invoke-restart' function. It's
recommended to supply them all if you don't want to end in the debugger."
  (reset-state)
  (multiple-value-prog1
      (handler-bind
          ((unknown-option
             (lambda (c)
               (push (option c) *unknown-options*)
               (when unknown-option
                 (apply #'invoke-restart unknown-option))))
           (missing-arg
             (lambda (c)
               (push (option c) *missing-arg-options*)
               (when missing-arg
                 (apply #'invoke-restart missing-arg))))
           (missing-required-option
             (lambda (c)
               (push (mapcar #'name (missing-options c)) *missing-required-options*)
               (when missing-required
                 (apply #'invoke-restart missing-required))))
           (arg-parser-failed
             (lambda (c)
               (push (raw-arg c) *malformed-arguments*)
               (when arg-parser-failed
                 (apply #'invoke-restart arg-parser-failed)))))
        (get-opts opts))
    (finish-collecting)))

(defun run-tests ()
  "Run Unix-opts tests. Signal failure if any test fails and return NIL
otherwise."
  (multiple-value-bind (options free-args)
      (parse-opts '("--grab-int" "10" "--rere" "11" "-s" "-a" "foo.txt")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:grab-int 10 :flag-a t)))
    (assert (equalp free-args '("11" "foo.txt")))
    (assert (equalp *unknown-options* '("--rere")))
    (assert (equalp *missing-arg-options* '("-s")))
    (assert (equalp *malformed-arguments* nil)))
  (multiple-value-bind (options free-args)
      (parse-opts '("-asri=13" "--flag-b" "--flag-b" "foo.txt" "bar.txt")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:flag-a t :grab-int 13 :flag-b t :flag-b t)))
    (assert (equalp free-args '("foo.txt" "bar.txt")))
    (assert (equalp *unknown-options* '("-r")))
    (assert (equalp *missing-arg-options* '("-s")))
    (assert (equalp *malformed-arguments* nil)))
  (multiple-value-bind (options free-args)
      (parse-opts '("--grab-str=fooba" "-i" "what" "-i" "100" "--roro" "-")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:grab-str "fooba" :grab-int 100)))
    (assert (equalp free-args '("-")))
    (assert (equalp *unknown-options* '("--roro")))
    (assert (equalp *missing-arg-options* nil))
    (assert (equalp *malformed-arguments* '("what"))))
  (multiple-value-bind (options free-args)
      (parse-opts '("--foobar" "cat" "-sl") ; very tricky (see restarts)
                  :unknown-option    '(use-value "--grab-int")
                  :missing-arg       '(use-value "my-string")
                  :arg-parser-failed '(reparse-arg "15"))
    (assert (equalp options '(:grab-int 15 :grab-str "my-string"
                              :grab-int "my-string")))
    (assert (equalp free-args nil))
    (assert (equalp *unknown-options* '("--foobar" "-l")))
    (assert (equalp *missing-arg-options* '("-s" "--grab-int")))
    (assert (equalp *malformed-arguments* '("cat"))))
  (multiple-value-bind (options free-args)
      (parse-opts '("--grab-i" "10" "--grab" "14" "--grab-s")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:grab-int 10)))
    (assert (equalp free-args '("14")))
    (assert (equalp *unknown-options* '("--grab")))
    (assert (equalp *missing-arg-options* '("--grab-s")))
    (assert (equalp *malformed-arguments* nil)))
  (multiple-value-bind (options free-args)
      (parse-opts '("--grab-int" "15" "--" "--grab-int" "16")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:grab-int 15)))
    (assert (equalp free-args '("--grab-int" "16")))
    (assert (equalp *unknown-options* nil))
    (assert (equalp *missing-arg-options* nil))
    (assert (equalp *malformed-arguments* nil)))
  (multiple-value-bind (options free-args)
      (parse-opts '("-s" "5")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :missing-required  '(skip-option)
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:grab-str "5")))
    (assert (equalp free-args '()))
    (assert (equalp *missing-required-options* '((:grab-int))))
    (assert (equalp *unknown-options* nil))
    (assert (equalp *missing-arg-options* nil))
    (assert (equalp *malformed-arguments* nil)))
  (multiple-value-bind (options free-args)
      (parse-opts '("-s" "5")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :missing-required  '(use-value (15))
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:grab-str "5" :grab-int 15)))
    (assert (equalp free-args '()))
    (assert (equalp *missing-required-options* '((:grab-int))))
    (assert (equalp *unknown-options* nil))
    (assert (equalp *missing-arg-options* nil))
    (assert (equalp *malformed-arguments* nil))))

(export 'run-tests)
