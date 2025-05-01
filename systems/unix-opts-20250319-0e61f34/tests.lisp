;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; This is some tests for Unix-opts library.
;;;
;;; Copyright © 2015–2018 Mark Karpov
;;; Copyright © 2018–2020 Thomas Schaper
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

(defpackage :unix-opts/tests
  (:shadowing-import-from :unix-opts :describe)
  (:use :cl :unix-opts)
  (:export :run))

(in-package #:unix-opts/tests)

(defun setup ()
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
     :long "flag-b")
    (:name :flag-c
     :description "flag with default value"
     :long "flag-c"
     :arg-parser #'identity
     :default (lambda () (list 1 2)))))

;;; Here is some variables that we will use and functions to reset them.

(defparameter *unknown-options* nil
  "We collect all unknown options here.")

(defparameter *missing-arg-options* nil
  "Options that need an argument, but don't get one.")

(defparameter *malformed-arguments* nil
  "Here we collect malformed arguments.")

(defparameter *missing-required-options*
  "Here we collect missing required options.")

(defparameter *unknown-provided-options*
  "Here we collect the unknown provided options")

(defun reset-state ()
  "Reset some special variables that are used to collect data about some
aspects of the tests."
  (setf *unknown-options*          nil
        *missing-arg-options*      nil
        *unknown-provided-options* nil
        *missing-required-options* nil
        *malformed-arguments*      nil))

(defun finish-collecting ()
  "Call this after parsing."
  (setf *unknown-options*     (nreverse *unknown-options*)
        *missing-required-options* (nreverse *missing-required-options*)
        *unknown-provided-options* (nreverse *unknown-provided-options*)
        *missing-arg-options* (nreverse *missing-arg-options*)
        *malformed-arguments* (nreverse *malformed-arguments*)))

(defmacro with-no-warning (&body body)
  (let ((warning (gensym "WARNING-")))
    `(with-muffled-warning ,warning (progn ,@body)
       (assert (not ,warning)))))

(defmacro with-muffled-warning (warning muffled-form &body body)
  (let ((result (gensym "RESULT-")))
    `(let ((,warning nil)
           (,result nil))
       (handler-bind ((warning (lambda (warn)
                                 (setf ,warning warn)
                                 (let ((muffle (find-restart 'muffle-warning warn)))
                                   (when muffle
                                     (invoke-restart (find-restart 'muffle-warning)))))))
         (setf ,result ,muffled-form))
       ,@body
       (values ,result))))

;;; The tests themselves.

(defun parse-opts (opts &key unknown-option missing-arg arg-parser-failed missing-required
                          unknown-option-provided
                          (all-options unix-opts::*options*))
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
           (unknown-option-provided
             (lambda (c)
               (push (option c) *unknown-provided-options*)
               (when unknown-option-provided
                 (apply #'invoke-restart unknown-option-provided))))
           (missing-arg
             (lambda (c)
               (push (option c) *missing-arg-options*)
               (when missing-arg
                 (apply #'invoke-restart missing-arg))))
           (missing-required-option
             (lambda (c)
               (push (mapcar #'unix-opts::name (missing-options c)) *missing-required-options*)
               (when missing-required
                 (apply #'invoke-restart missing-required))))
           (arg-parser-failed
             (lambda (c)
               (push (raw-arg c) *malformed-arguments*)
               (when arg-parser-failed
                 (apply #'invoke-restart arg-parser-failed)))))
        (get-opts opts all-options))
    (finish-collecting)))

(defvar *all-tests* ())

(defun add-test (tag name value)
  (let ((new-value (list tag name value)))
    (loop :with found = nil
          :for test :in *all-tests*
          :and idx :from 0
          :while (not found)
          :when (and (eql (nth 0 test) tag)
                     (eql (nth 1 test) name))
            :do (progn
                  (format t "Redefining ~(~a~) \"~A\"~%" tag name)
                  (setf found t
                        (nth idx *all-tests*) new-value))
          :finally (unless found
                     (push new-value *all-tests*)))))

(defmacro test (name &body body)
  `(add-test :test ',name (lambda () ,@body)))

(defmacro in-suite (name &key in)
  (declare (ignore in))
  `(add-test :suite ',name nil))

(in-suite argv :in all-tests)
(test argv-list
  :description "(argv) should return a list"
  (assert (typep (argv) 'list)))

(in-suite general-parsing :in all-tests)
(test unknown-args
  :description "Unknown args"
  (multiple-value-bind (options free-args)
      (parse-opts '("--grab-int" "10" "--rere" "11" "-s" "-a" "foo.txt")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:grab-int 10 :flag-a t :flag-c (1 2))))
    (assert (equalp free-args '("11" "foo.txt")))
    (assert (equalp *unknown-options* '("--rere")))
    (assert (equalp *missing-arg-options* '("-s")))
    (assert (equalp *malformed-arguments* nil))))

(test use-undefined-value
  (multiple-value-bind (options free-args)
      (parse-opts '("--a" "b")
                  :unknown-option    '(use-value "--unknown")
                  :missing-required  '(skip-option)
                  :unknown-option-provided '(skip-option)
                  :arg-parser-failed '(reparse-arg "15"))
    (assert (equalp options '(:flag-c (1 2))))
    (assert (equal free-args '("b")))
    (assert (equal *unknown-provided-options* '("--unknown")))))

(test superfluous-args
  (multiple-value-bind (options free-args)
      (parse-opts '("-asri=13" "--flag-b" "--flag-b" "foo.txt" "bar.txt")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:flag-a t :grab-int 13 :flag-b t :flag-b t :flag-c (1 2))))
    (assert (equalp free-args '("foo.txt" "bar.txt")))
    (assert (equalp *unknown-options* '("-r")))
    (assert (equalp *missing-arg-options* '("-s")))
    (assert (equalp *malformed-arguments* nil))))

(test simple-dash
  (multiple-value-bind (options free-args)
      (parse-opts '("--grab-str=fooba" "-i" "what" "-i" "100" "--roro" "-")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:grab-str "fooba" :grab-int 100 :flag-c (1 2))))
    (assert (equalp free-args '("-")))
    (assert (equalp *unknown-options* '("--roro")))
    (assert (equalp *missing-arg-options* nil))
    (assert (equalp *malformed-arguments* '("what")))))

(test tricky-short-arg
  (multiple-value-bind (options free-args)
      (parse-opts '("--foobar" "cat" "-sl") ; very tricky (see restarts)
                  :unknown-option    '(use-value "--grab-int")
                  :missing-arg       '(use-value "my-string")
                  :arg-parser-failed '(reparse-arg "15"))
    (assert (equalp options '(:grab-int 15
                          :grab-str "my-string"
                          :grab-int "my-string"
                          :flag-c (1 2))))
    (assert (equalp free-args nil))
    (assert (equalp *unknown-options* '("--foobar" "-l")))
    (assert (equalp *missing-arg-options* '("-s" "--grab-int")))
    (assert (equalp *malformed-arguments* '("cat")))))

(test double-dash
  :describe "Double dash stops parsing"
  (multiple-value-bind (options free-args)
      (parse-opts '("--grab-int" "15" "--" "--grab-int" "16")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:grab-int 15 :flag-c (1 2))))
    (assert (equalp free-args '("--grab-int" "16")))
    (assert (equalp *unknown-options* nil))
    (assert (equalp *missing-arg-options* nil))
    (assert (equalp *malformed-arguments* nil))))

(test short-parsing
  :describe "Short parsing"
  (multiple-value-bind (options free-args)
      (parse-opts '("-s" "5")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :missing-required  '(skip-option)
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:grab-str "5" :flag-c (1 2))))
    (assert (equalp free-args '()))
    (assert (equalp *missing-required-options* '((:grab-int))))
    (assert (equalp *unknown-options* nil))
    (assert (equalp *missing-arg-options* nil))
    (assert (equalp *malformed-arguments* nil))))

(test value-in-restart
  :describe "Provide value using restart"
  (multiple-value-bind (options free-args)
      (parse-opts '("-s" "5")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :missing-required  '(use-value (15))
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:grab-str "5" :grab-int 15 :flag-c (1 2))))
    (assert (equalp free-args '()))
    (assert (equalp *missing-required-options* '((:grab-int))))
    (assert (equalp *unknown-options* nil))
    (assert (equalp *missing-arg-options* nil))
    (assert (equalp *malformed-arguments* nil))))

(in-suite describe :in all-tests)
(test describe-align
  :describe "Describe should work and align nicely"
  (let ((described (with-output-to-string (s)
                     (describe :stream s))))
    (assert (equal described (format nil "~
Available options:
  -i, --grab-int INT (Required)
                                grab integer INT
  -s, --grab-str STR            grab string STR
  -a                            flag with short form only
  --flag-b                      flag with long form only
  --flag-c ARG                  flag with default value [Default: (1 2)]

"))))

  (let ((described (with-output-to-string (s)
                     (describe :stream s :argument-block-width 30))))
    (assert (equal described (format nil "~
Available options:
  -i, --grab-int INT (Required) grab integer INT
  -s, --grab-str STR            grab string STR
  -a                            flag with short form only
  --flag-b                      flag with long form only
  --flag-c ARG                  flag with default value [Default: (1 2)]

"))))

  (let ((described (with-output-to-string (s)
                     (describe :stream s
                               :available-options-label "Options"
                               :argument-block-width 30))))
    (assert (equal described (format nil "~
Options:
  -i, --grab-int INT (Required) grab integer INT
  -s, --grab-str STR            grab string STR
  -a                            flag with short form only
  --flag-b                      flag with long form only
  --flag-c ARG                  flag with default value [Default: (1 2)]

"))))

  (let ((described (with-output-to-string (s)
                     (describe :stream s
                               :usage-of       "foo"
                               :usage-of-label "program usage"
                               :argument-block-width 30))))
    (assert (equal described (format nil "
program usage: foo [-i|--grab-int INT (Required)] [-s|--grab-str STR] [-a]
                   [--flag-b] [--flag-c ARG]

Available options:
  -i, --grab-int INT (Required) grab integer INT
  -s, --grab-str STR            grab string STR
  -a                            flag with short form only
  --flag-b                      flag with long form only
  --flag-c ARG                  flag with default value [Default: (1 2)]

")))))

(in-suite default-values :in all-tests)
(test provide-value-for-default
  :describe "Provide value for default argument"
  ;; If options with a default value are provided the default value is not used.
  (multiple-value-bind (options _)
      (parse-opts '("--flag-c" "hello")
                  :missing-required '(skip-option))
    (declare (ignore _))
    (assert (equalp options '(:flag-c "hello")))))

(test producer-called-everytime
  :describe "Default value functions should be called every time they are used"
  (multiple-value-bind (options _)
      (parse-opts '()
                  :missing-required '(skip-option))
    (declare (ignore _))
    (let ((first-value (getf options :flag-c)))
      (assert (equalp first-value '(1 2)))
      (multiple-value-bind (options __)
          (parse-opts '()
                      :missing-required '(skip-option))
        (declare (ignore __))
        (let ((second-value (getf options :flag-c)))
          (assert (not (eql first-value second-value)))
          (assert (equalp first-value second-value)))))))

(test string-as-default
  :describe "Can also give a string value as default option."
  (let ((all-options (with-no-warning
                       (list (unix-opts::make-option (list :name :flag-d
                                                           :long "flag-d"
                                                           :arg-parser #'identity
                                                           :default "DEFAULT-FLAG-D"))))))
    (multiple-value-bind (options _)
        (parse-opts '() :missing-required '(skip-option) :all-options all-options)
      (declare (ignore _))
      (assert (equal (getf options :flag-d) "DEFAULT-FLAG-D")))))

(test warn-on-mutable
  :describe "Should warn if a mutable default variable is used."
  (with-muffled-warning warning
      (unix-opts::make-option (list :name :flag-e
                                    :long "flag-e"
                                    :arg-parser #'identity
                                    :default (list 1 2)))
    (assert warning)))


(in-suite prefix-matching :in all-tests)
(test simple-prefix
  :description "Prefixes should be used"
  (multiple-value-bind (options free-args)
      (parse-opts '("--grab-i" "10" "--grab" "14" "--grab-s")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:grab-int 10 :flag-c (1 2))))
    (assert (equalp free-args '("14")))
    (assert (equalp *unknown-options* '("--grab")))
    (assert (equalp *missing-arg-options* '("--grab-s")))
    (assert (equalp *malformed-arguments* nil))))

(test exact-match-prefix
  :description "Prefix matching should work, but exact matches should take precedence"
  (let ((options (make-options `((:name :opt
                                  :long "opt"
                                  :arg-parser ,#'identity)
                                 (:name :opt-longer
                                  :long "opt-longer"
                                  :arg-parser ,#'parse-integer)))))
    (assert (equalp (parse-opts '("--opt" "5") :all-options options) '(:opt "5")))
    (assert (equalp (parse-opts '("--opt-longer" "5") :all-options options) '(:opt-longer 5)))
    (assert (equalp (parse-opts '("--opt-longer" "5"
                                  "--opt" "6")
                                :all-options options) '(:opt-longer 5
                                                        :opt "6")))
    ;; Not really happy about this behavior, but we can't break it anymore.
    (assert (equalp (parse-opts '("--opt-l" "5") :all-options options) '(:opt-longer 5)))
    (let (signaled)
      (handler-bind
          ((unknown-option (lambda (c)
                             (setf signaled c)
                             (invoke-restart 'skip-option))))
        (equalp (parse-opts '("--op" "5") :all-options options) '(:opt-longer 5)))
      (assert signaled))))

(defun run ()
  (let ((success t))
    (handler-case
        (progn
          (loop :for test :in (reverse *all-tests*)
                :do (ecase (nth 0 test)
                      ((:suite)
                       (format t "Running suite ~A~%" (cadr test)))
                      ((:test)
                       (setup)
                       (format t "	Running test ~A: " (nth 1 test))
                       (handler-case
                           (progn
                             (funcall (nth 2 test))
                             (format t "~A~%"  (or #+sbcl #\CHECK_MARK
                                                   "SUCCESS")))
                         (error (err)
                           (setf success nil)
                           (format t "~A: ~A~%~%" (or #+sbcl #\CROSS_MARK
                                                      "X")
                                   err))))))
          success)
      (error () nil))))
