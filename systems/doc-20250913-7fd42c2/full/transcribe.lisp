;;;; TODO
;;;;
;;;; - Maybe implement inline commands that can change prefixes and
;;;;   other parameters.
;;;;
;;;; - special comment syntax for the 'narrative' to produce something
;;;;   like an ipython notebook
;;;;
;;;; - special prompt syntax for forms not to be evaluated when a file
;;;;   is loaded
;;;;
;;;; - capture conditions signalled?

(uiop:define-package #:40ants-doc-full/transcribe
  (:use #:cl)
  (:import-from #:40ants-doc/core
                #:defsection)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader)
  (:import-from #:40ants-doc-full/utils
                #:write-prefixed-lines
                #:read-prefixed-lines
                #:read-line*)
  (:import-from #:40ants-doc/docstring
                #:whitespacep)
  (:import-from #:40ants-doc/reference)
  ;; (:import-from #:40ants-doc-full/page)
  (:import-from #:swank)
  (:import-from #:slynk)
  (:import-from #:alexandria
                #:with-gensyms
                #:once-only)
  (:export
   #:transcribe
   #:*syntaxes*
   #:transcription-consistency-error
   #:transcription-output-consistency-error
   #:transcription-values-consistency-error
   #:transcription-error))
(in-package #:40ants-doc-full/transcribe)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)

(defsection @transcript (:title "Transcripts"
                         :ignore-words ("PAX"
                                        "REPL"
                                        "PREFIX-STRING"
                                        "PREFIXES"
                                        "40ANTS-DOC-RETRANSCRIBE-REGION"))
  "What are transcripts for? When writing a tutorial, one often wants
  to include a REPL session with maybe a few defuns and a couple of
  forms whose output or return values are shown. Also, in a function's
  docstring an example call with concrete arguments and return values
  speaks volumes. A transcript is a text that looks like a repl
  session, but which has a light markup for printed output and return
  values, while no markup (i.e. prompt) for lisp forms. The PAX
  transcripts may include output and return values of all forms, or
  only selected ones. In either case the transcript itself can be
  easily generated from the source code.

  The main worry associated with including examples in the
  documentation is that they tend to get out-of-sync with the code.
  This is solved by being able to parse back and update transcripts.
  In fact, this is exactly what happens during documentation
  generation with PAX. Code sections tagged `cl-transcript` are
  retranscribed and checked for inconsistency (that is, any difference
  in output or return values). If the consistency check fails, an
  error is signalled that includes a reference to the object being
  documented.

  Going beyond documentation, transcript consistency checks can be
  used for writing simple tests in a very readable form. For example:

  ```cl-transcript
  (+ 1 2)
  => 3
  
  (values (princ :hello) (list 1 2))
  .. HELLO
  => :HELLO
  => (1 2)
  ```

  All in all, transcripts are a handy tool especially when combined
  with the Emacs support to regenerate them and with
  PYTHONIC-STRING-READER and its triple-quoted strings that allow one
  to work with nested strings with less noise. The triple-quote syntax
  can be enabled with:

      (in-readtable pythonic-string-syntax)"
  (@transcript-emacs-integration section)
  (@transcript-api section))


(defsection @transcript-api (:title "Transcript API")
  (transcribe function)
  (*syntaxes* variable)
  (transcription-error condition)
  (transcription-consistency-error condition)
  (transcription-output-consistency-error condition)
  (transcription-values-consistency-error condition))


(defparameter *syntaxes*
  '((:default
     (:output "..")
     ;; To give precedence to this no value marker, it is listed
     ;; before :READABLE.
     (:no-value "=> ; No value")
     ;; No :READABLE-CONTINUATION which is fine because READ knows
     ;; where to stop anyway.
     (:readable "=>")
     (:unreadable "==>")
     (:unreadable-continuation "-->"))
    (:commented-1
     (:output ";..")
     (:no-value ";=> ; No value")
     (:readable ";=>")
     (:readable-continuation ";->")
     (:unreadable ";==>")
     (:unreadable-continuation ";-->"))
    (:commented-2
     (:output ";;..")
     (:no-value ";;=> ; No value")
     (:readable ";;=>")
     (:readable-continuation ";;->")
     (:unreadable ";;==>")
     (:unreadable-continuation ";;-->")))
  "The default syntaxes used by TRANSCRIBE for reading and writing
  lines containing output and values of an evaluated form.

  A syntax is a list of of the form `(SYNTAX-ID &REST PREFIXES)` where
  `prefixes` is a list of `(PREFIX-ID PREFIX-STRING)` elements. For
  example the syntax :COMMENTED-1 looks like this:

  ```lisp
  (:commented-1
   (:output \";..\")
   (:no-value \";=>  No value\")
   (:readable \";=>\")
   (:readable-continuation \";->\")
   (:unreadable \";==>\")
   (:unreadable-continuation \";-->\"))
  ```

  All of the above prefixes must be defined for every syntax except
  for :READABLE-CONTINUATION. If that's missing (as in the :DEFAULT
  syntax), then the following value is read with READ and printed with
  PRIN1 (hence no need to mark up the following lines).

  When writing, an extra space is added automatically if the line to
  be prefixed is not empty. Similarly, the first space following the
  prefix discarded when reading.

  See TRANSCRIBE for how the actual syntax to be used is selected.")

(defun transcribe (input output &key update-only
                                     (include-no-output update-only)
                                     (include-no-value update-only)
                                     (echo t)
                                     check-consistency
                                     default-syntax
                                     (input-syntaxes *syntaxes*)
                                     (output-syntaxes *syntaxes*))
  """Read forms from INPUT and write them (if ECHO) to OUTPUT
  followed by any output and return values produced by calling EVAL on
  the form. INPUT can be a stream or a string, while OUTPUT can be a
  stream or NIL in which case transcription goes into a string. The
  return value is the OUTPUT stream or the string that was
  constructed.

  A simple example is this:

  ```cl-transcript
  (transcribe "(princ 42) " nil)
  => "(princ 42)
  .. 42
  => 42
  "
  ```

  However, the above may be a bit confusing since this documentation
  uses TRANSCRIBE markup syntax in this very example, so let's do it
  differently. If we have a file with these contents:

  ```lisp
  (values (princ 42) (list 1 2))
  ```

  it is transcribed to:

  ```lisp
  (values (princ 42) (list 1 2))
  .. 42
  => 42
  => (1 2)
  ```

  Output to all standard streams is captured and printed with
  the :OUTPUT prefix (`".."`). The return values above are printed
  with the :READABLE prefix (`"=>"`). Note how these prefixes are
  always printed on a new line to facilitate parsing.

  **Updating**

  TRANSCRIBE is able to parse its own output. If we transcribe the
  previous output above, we get it back exactly. However, if we remove
  all output markers, leave only a placeholder value marker and
  pass :UPDATE-ONLY T with source:

  ```lisp
  (values (princ 42) (list 1 2))
  =>
  ```

  we get this:

  ```lisp
  (values (princ 42) (list 1 2))
  => 42
  => (1 2)
  ```

  With UPDATE-ONLY, printed output of a form is only transcribed if
  there were output markers in the source. Similarly, with
  UPDATE-ONLY, return values are only transcribed if there were value
  markers in the source.

  **No Output/Values**

  If the form produces no output or returns no values, then whether or
  not output and values are transcribed is controlled by
  INCLUDE-NO-OUTPUT and INCLUDE-NO-VALUE, respectively. By default,
  neither is on so:

  ```lisp
  (values)
  ..
  =>
  ```

  is transcribed to

  ```lisp
  (values)
  ```

  With UPDATE-ONLY true, we probably wouldn't like to lose those
  markers since they were put there for a reason. Hence, with
  UPDATE-ONLY, INCLUDE-NO-OUTPUT and INCLUDE-NO-VALUE default to true.
  So with UPDATE-ONLY the above example is transcribed to:

  ```lisp
  (values)
  ..
  => ; No value
  ```

  where the last line is the :NO-VALUE prefix.

  **Consistency Checks**

  If CHECK-CONSISTENCY is true, then TRANSCRIBE signals a continuable
  TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR whenever a form's output as a
  string is different from what was in INPUT, provided that INPUT
  contained the output. Similary, for values, a continuable
  TRANSCRIPTION-VALUES-CONSISTENCY-ERROR is signalled if a value read
  from the source does not print as the as the value returned by EVAL.
  This allows readable values to be hand-indented without failing
  consistency checks:

  ```lisp
  (list 1 2)
  => (1
        2)
  ```

  **Unreadable Values**

  The above scheme involves READ, so consistency of unreadable values
  cannot be treated the same. In fact, unreadable values must even be
  printed differently for transcribe to be able to read them back:

  ```lisp
  (defclass some-class () ())
  
  (defmethod print-object ((obj some-class) stream)
    (print-unreadable-object (obj stream :type t)
      (format stream \"~%~%end\")))

  (make-instance 'some-class)
  ==> #<SOME-CLASS 
  -->
  --> end>
  ```

  where `"==>"` is the :UNREADABLE prefix and `"-->"` is
  the :UNREADABLE-CONTINUATION prefix. As with outputs, a consistency
  check between an unreadable value from the source and the value from
  EVAL is performed with STRING=. That is, the value from EVAL is
  printed to a string and compared to the source value. Hence, any
  change to unreadable values will break consistency checks. This is
  most troublesome with instances of classes with the default
  PRINT-OBJECT method printing the memory address. There is currently
  no remedy for that, except for customizing PRINT-OBJECT or not
  transcribing that kind of stuff.

  **Syntaxes**

  Finally, a transcript may employ different syntaxes for the output
  and values of different forms. When INPUT is read, the syntax for
  each form is determined by trying to match all prefixes from all
  syntaxes in INPUT-SYNTAXES against a line. If there are no output or
  values for a form in INPUT, then the syntax remains undetermined.

  When OUTPUT is written, the prefixes to be used are looked up in
  DEFAULT-SYNTAX of OUTPUT-SYNTAXES, if DEFAULT-SYNTAX is not NIL. If
  DEFAULT-SYNTAX is NIL, then the syntax used by the same form in the
  INPUT is used or (if that could not be determined) the syntax of the
  previous form. If there was no previous form, then the first syntax
  if OUTPUT-SYNTAXES is used.

  To produce a transcript that's executable Lisp code,
  use :DEFAULT-SYNTAX :COMMENTED-1:

  ```lisp
  (make-instance 'some-class)
  ;==> #<SOME-CLASS
  ;-->
  ;--> end>

  (list 1 2)
  ;=> (1
  ;->    2)
  ```

  To translate the above to uncommented syntax,
  use :DEFAULT-SYNTAX :DEFAULT. If DEFAULT-SYNTAX is NIL (the
  default), the same syntax will be used in the output as in the input
  as much as possible."""
  (write-transcript (read-transcript input :syntaxes input-syntaxes)
                    output
                    :update-only update-only
                    :check-consistency check-consistency
                    :include-no-output include-no-output
                    :include-no-value include-no-value
                    :echo echo
                    :default-syntax default-syntax
                    :syntaxes output-syntaxes))


;;;; Prefix utilities

(defun find-syntax (syntax)
  (or (find syntax *syntaxes* :key #'first)
      (error "Cannot find syntax ~S.~%" syntax)))

(defun find-prefix (id syntax &key (errorp t))
  (flet ((foo (syntax-definition)
           (list (second (find id (rest syntax-definition) :key #'first))
                 (first syntax-definition))))
    (destructuring-bind (prefix syntax) (if syntax
                                            (foo (find-syntax syntax))
                                            (some #'foo *syntaxes*))
      (when (and (not prefix) errorp)
        (error "Cannot find prefix with id ~S~%" id))
      (values prefix syntax ))))

;;; Find a syntax and a prefix that matches LINE. If SYNTAX, then only
;;; consider prefixes in that syntax. Return the id of the matching
;;; prefix, its string and the id of syntax.
(defun match-prefixes (line syntax-id)
  (flet ((foo (syntax-definition)
           (let ((match (find-if (lambda (entry)
                                   (alexandria:starts-with-subseq
                                    (second entry) line))
                                 (rest syntax-definition))))
             (if match
                 (list (first match) (second match) (first syntax-definition))
                 nil))))
    (values-list (if syntax-id
                     (foo (find-syntax syntax-id))
                     (some #'foo *syntaxes*)))))


;;;; READ-TRANSCRIPT constructs a parse tree that's fed into
;;;; WRITE-TRANSCRIPT by TRANSCRIBE. This parse tree is simply called
;;;; _transcript_ and is a list of transcript commands.
;;;;
;;;; A transcript command, or simply _command_, is the parsed
;;;; representation of a single top-level form together with its
;;;; output and values. The following transcript of one command:
;;;;
;;;;     ;;; This is a comment before the form.
;;;;     (values (find-package :keyword) (princ 42))
;;;;     .. 42
;;;;     ==> #<PACKAGE "KEYWORD">
;;;;     => 42
;;;;
;;;; is parsed as:
;;;;
;;;;     ((((VALUES (FIND-PACKAGE :KEYWORD) (PRINC 42))
;;;;        ";;; This is a comment before the form.
;;;;     (values (find-package :keyword) (princ 42))")
;;;;       :DEFAULT
;;;;       (:OUTPUT "42")
;;;;       (:UNREADABLE "#<PACKAGE \"KEYWORD\">")
;;;;       (:READABLE (42 "42"))))
;;;;
;;;; Note how the command contains both the sexp and the original
;;;; string (including preceeding comments). It also has a variable
;;;; number of output (0 or 1) and value captures.

(defun command-form (command)
  (first (first command)))

(defun command-string (command)
  (second (first command)))

(defun command-syntax-id (command)
  (second command))

(defsetf command-syntax-id (command) (syntax-id)
  `(setf (second ,command) ,syntax-id))

(defun command-captures (command)
  (rest (rest command)))

(defun command-output-capture (command)
  (let ((captures
          (remove-if-not #'output-capture-p (command-captures command))))
    (when (< 1 (length captures))
      (transcription-error* "Multiple output captures found."))
    (first captures)))

(defun command-value-captures (command)
  (remove-if-not #'value-capture-p (command-captures command)))

(defun check-command-values (command)
  (when (and (some #'no-value-capture-p (command-captures command))
             (< 1 (count-if #'value-capture-p (command-captures command))))
    (transcription-error* "Found no-value-marker and other values.")))

(defun capture-id (capture)
  (first capture))

(defun capture-value (capture)
  (second capture))

(defun filter-captures (captures &rest ids)
  (remove-if-not (lambda (capture)
                   (member (capture-id capture) ids))
                 captures))

(defun output-capture-p (capture)
  (member (capture-id capture) '(:output :commented-output)))

(defun output-string (output-capture)
  (assert (output-capture-p output-capture))
  (capture-value output-capture))

(defun value-capture-p (capture)
  (or (no-value-capture-p capture)
      (readable-capture-p capture)
      (unreadable-capture-p capture)))

(defun no-value-capture-p (capture)
  (member (capture-id capture) '(:no-value :commented-no-value)))

(defun readable-capture-p (capture)
  (member (capture-id capture) '(:readable :commented-readable)))

(defun readable-object (readable-capture)
  (assert (readable-capture-p readable-capture))
  (first (capture-value readable-capture)))

(defun readable-string (readable-capture)
  (assert (readable-capture-p readable-capture))
  (second (capture-value readable-capture)))

(defun unreadable-capture-p (capture)
  (member (capture-id capture) '(:unreadable :commented-unreadable)))

(defun unreadable-string (unreadable-capture)
  (assert (unreadable-capture-p unreadable-capture))
  (capture-value unreadable-capture))


;;;; READ-TRANSCRIPT implementation

(defmacro with-input-stream ((stream input) &body body)
  `(call-with-input-stream (lambda (,stream)
                             ,@body)
                           ,input))

(defun call-with-input-stream (fn input)
  (cond ((streamp input)
         ;; There is no way to guarantee that FILE-POSITION will work
         ;; on a stream so let's just read the entire INPUT into a
         ;; string.
         (with-input-from-string (stream (uiop:slurp-stream-string input))
           (funcall fn stream)))
        ((stringp input)
         (with-input-from-string (input input)
           (funcall fn input)))
        (t
         ;; CHECK-TYPE in READ-TRANSCRIPT makes this impossible.
         (assert nil))))

(defmacro with-load-environment ((stream) &body body)
  (once-only (stream)
    `(let* ((*readtable* *readtable*)
            (*package* *package*)
            (*load-pathname* (handler-case (pathname ,stream)
                               (error () nil)))
            (*load-truename* (when *load-pathname*
                               (handler-case (truename ,stream)
                                 (file-error () nil))))
            #+sbcl
            (sb-c::*policy* sb-c::*policy*))
       ,@body)))

(defun read-transcript (input &key (syntaxes *syntaxes*))
  (check-type input (or stream string))
  (with-input-stream (stream input)
    (with-load-environment (stream)
      (let ((*syntaxes* syntaxes)
            (transcript ())
            (partial-line-p nil)
            ;; file position of the beginning of LINE
            (file-position (file-position stream)))
        (multiple-value-bind (line missing-newline-p)
            (read-line stream nil nil)
          (handler-case
              (loop while line do
                (multiple-value-bind (prefix-id prefix syntax-id)
                    (and (not partial-line-p)
                         (match-prefixes line (command-syntax-id
                                               (first transcript))))
                  (let ((match-length (length prefix))
                        value
                        n-lines-read
                        file-position-1)
                    (file-position stream (+ file-position match-length))
                    (multiple-value-setq
                        (value n-lines-read
                               line missing-newline-p file-position-1
                               partial-line-p)
                      (parse-transcript-element stream prefix-id syntax-id
                                                line match-length))
                    ;; Forms create a new entry, form output and values are
                    ;; pushed into that entry.
                    (cond (prefix-id
                           (when (endp transcript)
                             (transcription-error* "No open form."))
                           (setf (rest (rest (first transcript)))
                                 (append (command-captures (first transcript))
                                         (list (list prefix-id value))))
                           ;; The first capture determines the syntax
                           ;; and the rest must match.
                           (setf (command-syntax-id (first transcript))
                                 syntax-id)
                           (check-command-values (first transcript)))
                          (t
                           ;; NIL means the syntax is not yet known.
                           (push (list value nil) transcript)))
                    (setq file-position file-position-1))))
            (transcription-error (e)
              (apply #'transcription-error
                     stream file-position
                     (second (first (first transcript)))
                     (transcription-error-message e)
                     (transcription-error-message-args e)))))
        (nreverse transcript)))))


(defun parse-transcript-element (stream prefix-id syntax-id
                                 first-line match-length)
  (cond ((null prefix-id)
         (parse-form stream))
        ((eq prefix-id :output)
         (parse-prefixed stream :output syntax-id))
        ((eq prefix-id :readable)
         ;; It may be that there is no value following the :READEABLE
         ;; prefix, because it was put there as a placeholder for
         ;; UPDATE-ONLY to fill in.
         (cond ((every #'whitespacep (subseq first-line match-length))
                (read-line stream nil nil)
                (values-list `(,(list nil nil)
                               1
                               ,@(multiple-value-list
                                  (read-line* stream nil nil))
                               nil)))
               (t
                (parse-readable stream syntax-id))))
        ((eq prefix-id :unreadable)
         (parse-prefixed stream :unreadable-continuation syntax-id))
        ((eq prefix-id :no-value)
         (when (< match-length (length first-line))
           (transcription-error* "Trailing junk after ~S."
                                 (find-prefix prefix-id syntax-id)))
         (read-line stream nil nil)
         (values-list `(nil
                        1
                        ,@(multiple-value-list
                           (read-line* stream nil nil))
                        nil)))
        ((eq prefix-id :readable-continuation)
         (transcription-error* "Prefix ~S must be preceeded by ~S."
                               (find-prefix prefix-id syntax-id)
                               (find-prefix :readable syntax-id)))
        ((eq prefix-id :unreadable-continuation)
         (transcription-error* "Prefix ~S must be preceeded by ~S."
                               (find-prefix prefix-id syntax-id)
                               (find-prefix :unreadable syntax-id)))
        (t
         (transcription-error* "Unknown prefix id ~S in *PREFIXES*."
                               prefix-id))))

(defun parse-form (stream)
  (let ((form-and-string (read-form-and-string stream 'eof
                                               :preserve-whitespace-p t)))
    (cond ((eq form-and-string 'eof) nil)
          (t
           (let ((at-bol-p (skip-white-space-till-end-of-line stream)))
             (values-list `(,form-and-string
                            ;; FIXME: N-LINES-READ
                            1
                            ,@(multiple-value-list (read-line* stream nil nil))
                            ,(not at-bol-p))))))))

(defun parse-prefixed (stream prefix-id syntax-id)
  (read-prefixed-lines stream (find-prefix prefix-id syntax-id)
                       :first-line-prefix ""))

(defun parse-readable (stream syntax-id)
  (let ((continuation-prefix (find-prefix :readable-continuation syntax-id
                                          :errorp nil)))
    (if continuation-prefix
        (parse-readable-with-continuation stream continuation-prefix)
        (parse-readable* stream))))

(defun parse-readable-with-continuation (stream continuation-prefix)
  (multiple-value-bind (string n-lines-read
                        next-line missing-newline-p file-position)
      (read-prefixed-lines stream continuation-prefix
                           :first-line-prefix "")
    ;; FIXME: eof?
    (let ((form (first (with-input-from-string (stream string)
                         (read-form-and-string stream nil)))))
      (values (list form string) n-lines-read
              next-line missing-newline-p file-position
              nil))))

(defun parse-readable* (stream)
  ;; We are after a readable prefix. Eat a single space if any so that
  ;; "=>1" is parsed the same as "=> 1".
  (when (eql (peek-char nil stream nil nil) #\Space)
    (read-char stream))
  (let ((form-and-string
          (read-form-and-string stream 'eof
                                :preserve-whitespace-p t)))
    (when (eq (first form-and-string) 'eof)
      (transcription-error* "Unexpected EOF while parsing readable value."))
    (unless (skip-white-space-till-end-of-line stream)
      (transcription-error* "Trailing junk after readable value ~S."
                            (second form-and-string)))
    (values-list `(,form-and-string
                   ;; FIXME: N-LINES-READ
                   1
                   ,@(multiple-value-list (read-line* stream nil nil))
                   nil))))

;;; Read a sexp from STREAM or return EOF. The second value is a
;;; string of all of the characters that were read even if EOF
;;; (whitespace, comments).
(defun read-form-and-string (stream eof &key preserve-whitespace-p)
  (let* ((old-file-position (file-position  stream))
         (form (handler-case
                   (funcall (if preserve-whitespace-p
                                #'read-preserving-whitespace
                                #'read)
                            stream nil eof nil)
                 (error (e)
                   (transcription-error* "READ failed with error:~%~A"
                                         (princ-to-string e)))))
         (new-file-position (file-position  stream))
         (n (- new-file-position old-file-position))
         (form-as-string (make-string n)))
    (file-position  stream old-file-position)
    (read-sequence form-as-string stream)
    (assert (= (file-position  stream) new-file-position))
    (list form form-as-string)))

;;; Read all whitespace chars until the first non-whitespace char or
;;; the end of the line. Return :EOF on EOF, T on hitting the end of
;;; the line, and NIL if on running into a non-whitespace char.
(defun skip-white-space-till-end-of-line (stream)
  (loop for char = (peek-char nil stream nil nil)
        do (unless char
             (return :eof))
           (unless (whitespacep char)
             (return nil))
           (read-char stream nil nil)
           (when (char= char #\Newline)
             (return t))))


;;;; WRITE-TRANSCRIPT implementation

(defmacro with-output-stream ((stream output) &body body)
  `(call-with-output-stream (lambda (,stream)
                              ,@body)
                            ,output))

(defun call-with-output-stream (fn output)
  (cond ((streamp output)
         (funcall fn output))
        ((null output)
         (with-output-to-string (stream)
           (funcall fn stream)))
        (t
         (assert nil))))

(defun write-transcript (transcript output &key update-only
                         (include-no-output update-only)
                         (include-no-value update-only)
                         (echo t) check-consistency
                         default-syntax (syntaxes *syntaxes*))
  (check-type output (or stream null))
  (with-output-stream (stream output)
    (let ((*syntaxes* syntaxes)
          (last-syntax-id default-syntax))
      (dolist (command transcript)
        (let ((form (command-form command))
              (form-as-string (command-string command)))
          ;; When SYNTAX is NIL, we default to the syntax of the this
          ;; command or the last command with known syntax.
          (setq last-syntax-id (or default-syntax
                                   (command-syntax-id command)
                                   last-syntax-id))
          (when echo
            (format stream "~A" (command-string command))
            (unless (eq form 'eof)
              (terpri stream)))
          (unless (eq form 'eof)
            (multiple-value-bind (form-output form-values)
                (eval-and-capture form)
              (let ((output-capture (command-output-capture command)))
                (when (and check-consistency output-capture)
                  (check-output-consistency
                   nil form-as-string form-output
                   (output-string output-capture)))
                (transcribe-output stream form-output output-capture
                                   last-syntax-id update-only
                                   include-no-output))
              (let ((value-captures (command-value-captures command)))
                (when check-consistency
                  (check-values-consistency nil form-as-string
                                            form-values value-captures))
                (transcribe-values stream form-values value-captures
                                   last-syntax-id update-only
                                   include-no-value)))))))))

(defun readable-object-p (object)
  (null
   (nth-value 1 (ignore-errors
                 (values (read-from-string (prin1-to-string object)))))))

(defun check-output-consistency (stream form-as-string output captured-output)
  (check-type output string)
  (check-type captured-output (or string null))
  (unless (string= output (or captured-output ""))
    (consistency-error
     'transcription-output-consistency-error
     stream form-as-string
     "Inconsistent output found.~%~%Source: ~:_~S~%~%Output: ~:_~S~%"
     captured-output output)))

(defun check-values-consistency (stream form-as-string values value-captures)
  (when value-captures
    (let ((value-captures (if (and (= 1 (length value-captures))
                                   (no-value-capture-p (first value-captures)))
                              ()
                              value-captures)))
      (cond ((/= (length values) (length value-captures))
             (consistency-error
              'transcription-values-consistency-error
              stream form-as-string
              "Source had ~S return values ~:_while there are actually ~S."
              (length value-captures) (length values)))
            (t
             (loop for value in values
                   for value-capture in value-captures
                   do (check-value-consistency stream form-as-string
                                               value value-capture)))))))

(defmacro with-transcription-syntax (() &body body)
  (with-gensyms (package)
    `(let ((,package *package*))
       (with-standard-io-syntax
         (let ((*package* ,package)
               (*print-readably* nil)
               (*print-pretty* t)
               (*print-right-margin* 72))
           ,@body)))))

(defun check-value-consistency (stream form-as-string value value-capture)
  (assert (not (no-value-capture-p value-capture)))
  (flet ((stringify (object)
           (with-transcription-syntax ()
             (prin1-to-string object))))
    (let ((value-readable-p (readable-object-p value)))
      (cond ((and value-readable-p
                  (not (readable-capture-p value-capture)))
             (consistency-error
              'transcription-values-consistency-error
              stream form-as-string
              "Unreadable value ~:_~S ~:_in source became readable ~:_~S."
              (unreadable-string value-capture) value))
            ((and (not value-readable-p)
                  (not (unreadable-capture-p value-capture)))
             (consistency-error
              'transcription-values-consistency-error
              stream form-as-string
              "Readable value ~:_~S~:_ in source became unreadable ~:_~S."
              (readable-string value-capture) value))
            ;; At this point we know that both are readable or both are
            ;; unreadable.
            (value-readable-p
             (unless (string= (stringify value)
                              (stringify (readable-object value-capture)))
               (consistency-error
                'transcription-values-consistency-error
                stream form-as-string
                "Readable value ~:_~S ~:_in source does not print the ~
                same as ~:_~S." (stringify (readable-object value-capture))
                (stringify value))))
            ((not (string= (stringify value) (unreadable-string value-capture)))
             (consistency-error
              'transcription-values-consistency-error
              stream form-as-string
              "Unreadable value ~:_~S ~:_in source does not print the ~
              same as ~:_~S." (unreadable-string value-capture)
              (stringify value)))))))

(defun eval-and-capture (form)
  (let* ((buffer (make-array 0 :element-type 'character
                             :fill-pointer 0 :adjustable t))
         (values (multiple-value-list
                  (handler-case
                      (with-output-to-string (output buffer)
                        (with-transcription-syntax ()
                          (let ((*standard-output* output)
                                (*error-output* output)
                                (*trace-output* output)
                                (*debug-io* output)
                                (*query-io* output)
                                (*terminal-io* output))
                            (eval form))))
                    (error (e)
                      (transcription-error nil nil form
                                           "Error while evaluating form.~%~A"
                                           (princ-to-string e)))))))
    (values buffer values)))

(defun transcribe-output (stream output capture syntax-id
                          update-only include-no-output)
  (when (if update-only
            (not (null capture))
            (or include-no-output (plusp (length output))))
    (write-prefixed-lines output (find-prefix :output syntax-id) stream)))

(defun transcribe-values (stream values captures syntax-id
                          update-only include-no-value)
  (when (if update-only
            captures
            (or include-no-value values))
    (with-transcription-syntax ()
      (if (endp values)
          (when include-no-value
            (format stream "~A~%" (find-prefix :no-value syntax-id)))
          (loop for value in values
                for i upfrom 0
                for capture = (if (< i (length captures))
                                  (elt captures i)
                                  nil)
                do (if (readable-object-p value)
                       (transcribe-readable-value
                        stream value capture syntax-id)
                       (transcribe-unreadable-value
                        stream value syntax-id)))))))

;;; Assuming that OBJECT prints readably, check that whether CAPTURE
;;; is readable and it prints the same.
(defun readably-consistent-p (object capture)
  (and (readable-capture-p capture)
       (with-standard-io-syntax
         (string= (prin1-to-string object)
                  (prin1-to-string (readable-object capture))))))

(defun transcribe-readable-value (stream value capture syntax-id)
  (let ((prefix (find-prefix :readable syntax-id))
        (continuation-prefix (find-prefix :readable-continuation syntax-id
                                          :errorp nil)))
    (if (or (null capture)
            (not (readably-consistent-p value capture)))
        (if (null continuation-prefix)
            ;; No continuation prefix, just mark the first line.
            (format stream "~A ~S~%" prefix value)
            ;; FIXME: indentation can be wrong with multiline
            (write-prefixed-lines (princ-to-string value) continuation-prefix
                                  stream :first-line-prefix prefix))
        ;; They print the same, so use the parsed string, because it
        ;; might have been hand-indented.
        (if (null continuation-prefix)
            (format stream "~A ~A~%" prefix (readable-string capture))
            (write-prefixed-lines (readable-string capture)
                                  continuation-prefix
                                  stream :first-line-prefix prefix)))))

(defun transcribe-unreadable-value (stream object syntax-id)
  (let ((prefix (find-prefix :unreadable syntax-id))
        (continuation-prefix (find-prefix :unreadable-continuation syntax-id)))
    (write-prefixed-lines (prin1-to-string object) continuation-prefix
                          stream :first-line-prefix prefix)))


;;;; Conditions

(define-condition transcription-error (error)
  (;; This is usually the source stream object on which TRANSCRIBE
   ;; failed, but it can also be a REFERENCE object if transcription
   ;; was invoked by DOCUMENT to check the consistency of a
   ;; transcript.
   (on :initarg :on :reader transcription-error-on)
   ;; The file position at which the error was encountered or NIL if
   ;; unknown.
   (file-position
    :initarg :file-position
    :reader transcription-error-file-position)
   ;; The lisp form that was being processed when the error happened.
   ;; It also includes leading whitespace and comments.
   (form-as-string
    :initarg :form-as-string
    :reader transcription-error-form-as-string)
   ;; A string detailing the circumstances of the error.
   (message
    :initarg :message
    :reader transcription-error-message)
   (message-args
    :initarg :message-args
    :reader transcription-error-message-args))
  (:documentation "Represents syntactic errors in the INPUT argument
  of TRANSCRIBE and also serves as the superclass of
  TRANSCRIPTION-CONSISTENCY-ERROR.")
  (:report (lambda (condition stream)
             (let ((on (transcription-error-on condition)))
               (format stream
                       "~@<Transcription error~@[ in ~:_~A~]~
                       ~@[ ~:_at position ~A~].~
                       ~:_ ~?~%~
                       Form: ~:_~S~:@>"
                       (if (typep on '40ants-doc/reference::reference)
                           ;; Allow M-. to work in the slime debugger.
                           (print-reference-with-package on)
                           ;; Don't print the stream, since it's
                           ;; likely to be constructed by
                           ;; WITH-INPUT-STREAM which is meaningless
                           ;; for the user.
                           nil)
                       (transcription-error-file-position condition)
                       (transcription-error-message condition)
                       (transcription-error-message-args condition)
                       (transcription-error-form-as-string condition))))))

(defun print-reference-with-package (reference)
  (let ((*package* (find-package :keyword)))
    (format nil "~S ~S" (40ants-doc/reference::reference-object reference)
            (40ants-doc/reference::reference-locative reference))))

(defun transcription-error (stream file-position form-as-string
                            message &rest message-args)
  (error 'transcription-error
         :on (or 40ants-doc/reference::*reference-being-documented* stream)
         :file-position (if 40ants-doc/reference::*reference-being-documented* nil file-position)
         :form-as-string form-as-string
         :message message
         :message-args message-args))

(defun transcription-error* (message &rest message-args)
  (apply #'transcription-error nil nil nil message message-args))

(define-condition transcription-consistency-error (transcription-error)
  ()
  (:documentation "A common superclass for
  TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR and
  TRANSCRIPTION-VALUES-CONSISTENCY-ERROR."))

(define-condition transcription-output-consistency-error
    (transcription-consistency-error)
  ()
  (:documentation "Signaled (with CERROR) by TRANSCRIBE when invoked
  with :CHECK-CONSISTENCY and the output of a form is not the same as
  what was parsed."))

(define-condition transcription-values-consistency-error
    (transcription-consistency-error)
  ()
  (:documentation "Signaled (with CERROR) by TRANSCRIBE when invoked
  with :CHECK-CONSISTENCY and the values of a form are inconsistent
  with their parsed representation."))

(defun consistency-error (class stream form-as-string
                          message &rest message-args)
  (cerror "Continue." class
          :on (or 40ants-doc/reference::*reference-being-documented* stream)
          :file-position (cond (40ants-doc/reference::*reference-being-documented*
                                nil)
                               (stream
                                (file-position stream))
                               (t nil))
          :form-as-string form-as-string
          :message message
          :message-args message-args))


(defsection @transcript-emacs-integration
    (:title "Transcribing with Emacs"
     :ignore-words ("PAX"
                    ":HELLO-WORLD"))
  """Typical transcript usage from within Emacs is simple: add a lisp
  form to a docstring or comment at any indentation level. Move the
  cursor right after the end of the form as if you were to evaluate it
  with `C-x C-e`. The cursor is marked by `#\^`:

      This is part of a docstring.

      ```cl-transcript
      (values (princ :hello) (list 1 2))^
      ```

  Note that the use of fenced code blocks with the language tag
  `cl-transcript` is only to tell PAX to perform consistency checks at
  documentation generation time.

  Now invoke the emacs command `mgl-pax-transcribe-last-expression` where the cursor
  is and the fenced code block from the docstring becomes:

      (values (princ :hello) (list 1 2))
      .. HELLO
      => :HELLO
      => (1 2)
      ^

  Then you change the printed message to :HELLO-WORLD and add a comment to the second
  return value:

      (values (princ :hello-world) (list 1 2))
      .. HELLO
      => :HELLO
      => (1
          ;; This value is arbitrary.
          2)

  When generating the documentation you get a
  a warning:

      WARNING:
         Transcription error. Inconsistent output found.
      
      Source:
         "HELLO"

      Output:
         "HELLO-WORLD"

      Form:
         "(values (princ :hello-world) (list 1 2))"

  because the printed output and the
  first return value changed so you regenerate the documentation by
  marking the region of bounded by `|` and the cursor at `^` in
  the example:

      |(values (princ :hello-world) (list 1 2))
      .. HELLO
      => :HELLO
      => (1
          ;; This value is arbitrary.
          2)
      ^

  then invoke the emacs command `40ants-doc-retranscribe-region` to get:

      (values (princ :hello-world) (list 1 2))
      .. HELLO-WORLD
      => :HELLO-WORLD
      => (1
          ;; This value is arbitrary.
          2)
      ^

  Note how the indentation and the comment of `(1 2)` was left alone
  but the output and the first return value got updated.

  Alternatively, `C-u 1 40ants-doc-transcribe-last-expression` will emit commented markup:

      (values (princ :hello) (list 1 2))
      ;.. HELLO
      ;=> :HELLO
      ;=> (1 2)

  This can be useful for producing results outside of the docstrings.

  `C-u 0 40ants-doc-retranscribe-region` will turn commented into
  non-commented markup. In general, the numeric prefix argument is the
  index of the syntax to be used in 40ANTS-DOC-FULL/TRANSCRIBE:*SYNTAXES*. Without a
  prefix argument `40ants-doc-retranscribe-region` will not change the
  markup style.

  Finally, not only do both functions work at any indentation level,
  but in comments too:

      ;;;; (values (princ :hello) (list 1 2))
      ;;;; .. HELLO
      ;;;; => :HELLO
      ;;;; => (1 2)

  Transcription support in emacs can be enabled by adding this to your
  Emacs initialization file (or loading `elisp/transcribe.el`):"""
  (transcribe.el (include
                  #.(asdf:system-relative-pathname :40ants-doc "elisp/transcribe.el")
                  :lang "elisp")))


(defmacro with-buffer-syntax ((&optional package readtable) &body body)
  "Execute BODY with appropriate *package* and *readtable* bindings.

This should be used for code that is conceptionally executed in an
Emacs buffer."
  `(if (boundp 'swank::*buffer-package*)
      (swank::call-with-buffer-syntax ,package (lambda () ,@body))
      (slynk::call-with-buffer-syntax ,package ,readtable (lambda () ,@body))))


(defun transcribe-for-emacs (string default-syntax* update-only echo
                             first-line-special-p)
  (let ((default-syntax (cond ((numberp default-syntax*)
                               (first (elt *syntaxes* default-syntax*)))
                              ((null default-syntax*)
                               nil)
                              (t (error "Unexpected default syntax ~S."
                                        default-syntax*)))))
    (with-buffer-syntax ()
      (multiple-value-bind (string prefix)
          (40ants-doc-full/utils::strip-longest-common-prefix
           string "; " :first-line-special-p first-line-special-p)
        (let ((transcript
                (40ants-doc-full/utils::prefix-lines
                 prefix
                 (transcribe string nil
                             :default-syntax default-syntax
                             :update-only update-only :echo echo)
                 :exclude-first-line-p first-line-special-p)))
          (if echo
              transcript
              (format nil "~%~A" transcript)))))))
