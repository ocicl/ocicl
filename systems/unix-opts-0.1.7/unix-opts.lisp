;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; Unix-opts—a minimalistic parser of command line options.
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

(defpackage   :unix-opts
  (:nicknames :opts)
  (:use       #:common-lisp)
  (:export    #:unknown-option
              #:missing-arg
              #:arg-parser-failed
              #:missing-required-option
              #:use-value
              #:skip-option
              #:reparse-arg
              #:option
              #:missing-options
              #:exit
              #:raw-arg
              #:define-opts
              #:argv
              #:get-opts
              #:describe)
  (:shadow    #:describe))

(in-package #:unix-opts)

(defclass option ()
  ((name
    :initarg  :name
    :accessor name
    :documentation "keyword that will be included in list returned by
`get-opts' function if this option is given by user")
   (description
    :initarg  :description
    :accessor description
    :documentation "description of the option")
   (short
    :initarg  :short
    :accessor short
    :documentation "NIL or single char - short variant of the option")
   (long
    :initarg  :long
    :accessor long
    :documentation "NIL or string - long variant of the option")
   (required
    :initarg :required
    :accessor required
    :initform nil
    :documentation "If not NIL this argument is required.")
   (arg-parser
    :initarg  :arg-parser
    :accessor arg-parser
    :documentation "if not NIL, this option requires an argument, it will be
parsed with this function")
   (meta-var
    :initarg  :meta-var
    :accessor meta-var
    :documentation "if this option requires an argument, this is how it will
be printed in option description"))
  (:documentation "representation of an option"))

(define-condition troublesome-option (simple-error)
  ((option
    :initarg :option
    :reader option))
  (:report (lambda (c s) (format s "troublesome option: ~s" (option c))))
  (:documentation "Generalization over conditions that have to do with some
particular option."))

(define-condition unknown-option (troublesome-option)
  ()
  (:report (lambda (c s) (format s "unknown option: ~s" (option c))))
  (:documentation "This condition is thrown when parser encounters
unknown (not previously defined with `define-opts') option."))

(define-condition missing-arg (troublesome-option)
  ()
  (:report (lambda (c s) (format s "missing arg for option: ~s" (option c))))
  (:documentation "This condition is thrown when some option OPTION wants
an argument, but there is no such argument given."))

(define-condition missing-required-option (troublesome-option)
  ((missing-options
    :initarg :missing-options
    :reader missing-options))
  (:report (lambda (c s)
             (format s "missing required options: ~{\"~a\"~^, ~}"
                     (mapcar (lambda (opt)
                               (with-slots (short long name) opt
                                 (apply #'format nil
                                        (cond
                                          (long (list "--~A" long))
                                          (short (list "-~A" short))
                                          (t (list "~A" name))))))
                             (missing-options c)))))
  (:documentation "This condition is thrown when required options are missing."))

(define-condition arg-parser-failed (troublesome-option)
  ((raw-arg
    :initarg :raw-arg
    :reader raw-arg))
  (:report (lambda (c s)
             (format s
                     "argument parser failed (option: ~s, string to parse: ~s)"
                     (option c)
                     (raw-arg c))))
  (:documentation "This condition is thrown when some option OPTION wants
an argument, it's given but cannot be parsed by argument parser."))

(defparameter *options* nil
  "List of all defined options.")

(defun add-option (&rest args)
  "Register an option according to ARGS."
  (let ((name        (getf args :name))
        (description (getf args :description "?"))
        (short       (getf args :short))
        (long        (getf args :long))
        (arg-parser  (getf args :arg-parser))
        (required    (getf args :required))
        (meta-var    (getf args :meta-var "ARG")))
    (unless (or short long)
      (error "at least one form of the option must be provided"))
    (check-type name        keyword)
    (check-type description string)
    (check-type short       (or null character))
    (check-type long        (or null string))
    (check-type arg-parser  (or null function))
    (check-type meta-var    string)
    (check-type required    boolean)
    (push (make-instance 'option
                         :name        name
                         :description description
                         :short       short
                         :long        long
                         :required    required
                         :arg-parser  arg-parser
                         :meta-var    meta-var)
          *options*)))

(defmacro define-opts (&body descriptions)
  "Define command line options. Arguments of this macro must be plists
containing various parameters. Here we enumerate all allowed parameters:

:NAME—keyword that will be included in list returned by GET-OPTS function if
actual option is supplied by user.

:DESCRIPTION—description of the option (it will be used in DESCRIBE
function). This argument is optional, but it's recommended to supply it.

:SHORT—single character, short variant of the option. You may omit this
argument if you supply :LONG variant of option.

:LONG—string, long variant of option. You may omit this argument if you
supply :SHORT variant of option.

:ARG-PARSER—if actual option must take an argument, supply this argument, it
must be a function that takes a string and parses it.

:META-VAR—if actual option requires an argument, this is how it will be
printed in option description."
  `(progn
     (setf *options* nil)
     ,@(mapcar (lambda (args) (cons 'add-option args))
               descriptions)
     (setf *options* (nreverse *options*))
     (values)))

(defun argv ()
  "Return a list of program's arguments, including command used to execute
the program as first elements of the list. Portable across implementations."
  #+abcl      ext:*command-line-argument-list*
  #+allegro   sys:command-line-arguments
  #+:ccl      ccl:*command-line-argument-list*
  #+clisp     (cons *load-truename* ext:*args*)
  #+clozure   ccl::command-line-arguments
  #+cmu       extensions:*command-line-words*
  #+ecl       (ext:command-args)
  #+gcl       si:*command-args*
  #+lispworks system:*line-arguments-list*
  #+sbcl      sb-ext:*posix-argv*)

(defun split-short-opts (arg)
  "Split short options, for example \"-ab\" will produce \"-a\" and
\"-b\". ARG must be a string, return value is list of strings."
  (if (and (> (length arg) 1)
           (char=  #\- (char arg 0))
           (char/= #\- (char arg 1)))
      (mapcar (lambda (c) (format nil "-~c" c))
              (cdr (coerce arg 'list)))
      (list arg)))

(defun split-on-= (arg)
  "Split string ARG on \"=\", return value is list of strings."
  (if (and (> (length arg) 1)
           (char=  #\- (char arg 0))
           (char/= #\= (char arg 1)))
      (let ((pos (position #\= arg :test #'char=)))
        (if pos
            (list (subseq arg 0 pos)
                  (subseq arg (1+ pos) (length arg)))
            (list arg)))
      (list arg)))

(defun shortp (opt)
  "Predicate that checks if OPT is a short option."
  (and (= (length opt) 2)
       (char=  #\- (char opt 0))
       (char/= #\- (char opt 1))))

(defun longp (opt)
  "Predicate that checks if OPT is a long option."
  (and (> (length opt) 2)
       (char= #\- (char opt 0))
       (char= #\- (char opt 1))))

(defun optionp (str)
  "This predicate checks if string STR is an option."
  (or (shortp str) (longp str)))

(defun argp (str)
  "Check if string STR is an argument (not option)."
  (and (typep str 'string)
       (not (optionp str))))

(defun find-option (opt)
  "Find option OPT and return object that represents it or NIL."
  (multiple-value-bind (opt key)
      (if (shortp opt)
          (values (subseq opt 1) #'short)
          (values (subseq opt 2) #'long))
    (flet ((prefix-p (x)
             (let ((x (string x)))
               (when (>= (length x) (length opt))
                 (string= x opt :end1 (length opt))))))
      (let ((matches (remove-if-not #'prefix-p *options* :key key)))
        (if (cadr matches)
            nil
            (car matches))))))

(defun get-opts (&optional options)
  "Parse command line options. If OPTIONS is given, it should be a list to
parse. If it's not given, the function will use `argv' function to get list
of command line arguments.

Return two values:

* a list that contains keywords associated with command line options with
  `define-opts' macro, and
* a list of free arguments.

If some option requires an argument, you can use `getf' to
test presence of the option and get its argument if the option is present.

The parser may signal various conditions. Let's list them all specifying
which restarts are available for every condition, and what kind of
information the programmer can extract from the conditions.

`unknown-option' is thrown when parser encounters unknown (not previously
defined with `define-opts') option. Use the `option' reader to get name of
the option (string). Available restarts: `use-value' (substitute the option
and try again), `skip-option' (ignore the option).

`missing-arg' is thrown when some option wants an argument, but there is no
such argument given. Use the `option' reader to get name of the
option (string). Available restarts: `use-value' (supplied value will be
used), `skip-option' (ignore the option).

`arg-parser-failed' is thrown when some option wants an argument, it's given
but cannot be parsed by argument parser. Use the `option' reader to get name
of the option (string) and `raw-arg' to get raw string representing the
argument before parsing. Available restarts: `use-value' (supplied value
will be used), `skip-option' (ignore the option), `reparse-arg' (supplied
string will be parsed instead).

`missing-required-option' is thrown when some option was required but was
not given. Use the `missing-options' reader to get the list of options that
are missing. Available restarts: `use-value' (supplied list of values will
be used), `skip-option' (ignore all these options, effectively binding them
to `nil')"
  (do ((tokens (mapcan #'split-short-opts
                       (mapcan #'split-on-=
                               (or options (cdr (argv)))))
               (cdr tokens))
       (required (loop :with table = (make-hash-table)
                       :for option :in *options*
                       :when (required option)
                         :do (setf (gethash (name option) table) option)
                       :finally (return table)))
       poption-name
       poption-raw
       poption-parser
       options
       free-args)
      ((and (null tokens)
            (null poption-name))
       (progn
         (when (/= (hash-table-count required) 0)
           (let ((missing (loop :for val :being :the :hash-values :of required
                                :collect val)))
             (restart-case
                 (error 'missing-required-option
                        :missing-options missing)
               (skip-option ())
               (use-value (values)
                 (loop :for option :in missing
                       :for value :in values
                       :do (push (name option) options)
                       :do (push value options))))))
         (values (nreverse options)
                 (nreverse free-args))))
    (labels ((push-option (name value)
               (push name options)
               (push value options)
               (setf poption-name nil))
             (process-arg (arg)
               (restart-case
                   (handler-case
                       (push-option poption-name
                                    (funcall poption-parser arg))
                     (error (condition)
                       (declare (ignore condition))
                       (error 'arg-parser-failed
                              :option poption-raw
                              :raw-arg arg)))
                 (use-value (value)
                   (push-option poption-name value))
                 (skip-option ()
                   (setf poption-name nil))
                 (reparse-arg (str)
                   (process-arg str))))
             (process-option (opt)
               (let ((option (find-option opt)))
                 (if option
                     (let ((parser (arg-parser option)))
                       (remhash (name option) required)
                       (if parser
                           (setf poption-name   (name option)
                                 poption-raw    opt
                                 poption-parser parser)
                           (push-option (name option) t)))
                     (restart-case
                         (error 'unknown-option
                                :option opt)
                       (use-value (value)
                         (process-option value))
                       (skip-option ()))))))
      (let ((item (car tokens)))
        (cond ((and poption-name (argp item))
               (process-arg item))
              (poption-name
               (restart-case
                   (error 'missing-arg
                          :option poption-raw)
                 (use-value (value)
                   (push-option poption-name value)
                   (when item
                     (process-option item)))
                 (skip-option ()
                   (setf poption-name nil)
                   (when item
                     (process-option item)))))
              ((string= item "--")
               (dolist (tok (cdr tokens))
                 (push tok free-args))
               (setf tokens nil))
              ((optionp item)
               (process-option item))
              (t (push item free-args)))))))

(defun add-text-padding (str &key padding newline)
  "Add padding to text STR. Every line except for the first one, will be
prefixed with PADDING spaces. If NEWLINE is non-NIL, newline character will
be prepended to the text making it start on the next line with padding
applied to every single line."
  (let ((pad (make-string padding :initial-element #\Space)))
    (with-output-to-string (s)
      (when newline
        (format s "~&~a" pad))
      (map nil
           (lambda (x)
             (write-char x s)
             (when (char= x #\Newline)
               (write pad :stream s :escape nil)))
           str))))

(defun print-opts (&optional (stream *standard-output*))
  "Print info about defined options to STREAM. Every option get its own line
with description."
  (dolist (opt *options*)
    (with-slots (short long description required arg-parser meta-var) opt
      (let ((opts-and-meta
             (concatenate
              'string
              (if short (format nil "-~c" short) "")
              (if (and short long) ", " "")
              (if long  (format nil "--~a" long) "")
              (if arg-parser (format nil " ~a" meta-var) "")
              (if required (format nil " (Required)") ""))))
        (format stream "  ~25a~a~%"
                opts-and-meta
                (add-text-padding
                 description
                 :padding 27
                 :newline (>= (length opts-and-meta) 25))))))
  (terpri stream))

(defun print-opts* (margin)
  "Return a string containing info about defined options. All options are
displayed on one line, although this function tries to print it elegantly if
it gets too long. MARGIN specifies margin."
  (let ((fill-col (- 80 margin))
        (i 0)
        (last-newline 0))
    (with-output-to-string (s)
      (dolist (opt *options*)
        (with-slots (short long required arg-parser meta-var) opt
          (let ((str
                 (format nil " [~a]"
                         (concatenate
                          'string
                          (if short (format nil "-~c" short) "")
                          (if (and short long) "|" "")
                          (if long  (format nil "--~a" long) "")
                          (if arg-parser (format nil " ~a" meta-var) "")
                          (if required (format nil " (Required)") "")))))
                (incf i (length str))
                (when (> (- i last-newline) fill-col)
                  (terpri s)
                  (dotimes (x margin)
                    (princ #\space s))
                  (setf last-newline i))
                (princ str s)))))))

(defun describe (&key prefix suffix usage-of args (stream *standard-output*))
  "Return string describing options of the program that were defined with
`define-opts' macro previously. You can supply PREFIX and SUFFIX arguments
that will be printed before and after options respectively. If USAGE-OF is
supplied, it should be a string, name of the program for \"Usage: \"
section. This section is only printed if this name is given. If your program
takes arguments (apart from options), you can specify how to print them in
\"Usage: \" section with ARGS option (should be a string designator). Output
goes to STREAM."
  (flet ((print-part (str)
           (when str
             (princ str stream)
             (terpri stream))))
    (print-part prefix)
    (terpri stream)
    (when usage-of
      (format stream "Usage: ~a~a~@[ ~a~]~%~%"
              usage-of
              (print-opts* (+ 7 (length usage-of)))
              args))
    (when *options*
      (format stream "Available options:~%")
      (print-opts stream))
    (print-part suffix)))

(defun exit (&optional (status 0))
  "Exit the program returning `status'."
  #+sbcl      (sb-ext:exit :code status)
  #+cmu       (unix:unix-exit status)
  #+ccl       (ccl:quit status)
  #+ecl       (ext:quit status)
  #+clisp     (ext:exit status)
  #+abcl      (extensions:exit :status status)
  #+allegro   (excl:exit status :quiet t)
  #+lispworks (lispworks:quit :status status))
