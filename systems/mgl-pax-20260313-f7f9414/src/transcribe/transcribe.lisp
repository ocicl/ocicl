(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

;;;; Utilities

(defun strip-longest-common-prefix (string chars &key first-line-indent
                                    (ignore-blank-lines-p t))
  (let ((prefix (longest-common-prefix
                 string chars
                 :first-line-special-p first-line-indent
                 :ignore-blank-lines-p ignore-blank-lines-p))
        (prefix1 nil))
    (values
     (with-output-to-string (output)
       (with-input-from-string (s string)
         (loop for i upfrom 0
               for line = (read-line s nil nil)
               while line
               do (cond ((and ignore-blank-lines-p
                              (blankp line)
                              ;; Without this, retranscribing with
                              ;; a non-empty prefix can keep
                              ;; increasing the length of the
                              ;; blank line.
                              (not (starts-with-subseq prefix line)))
                         (write-line line output))
                        ((and first-line-indent (zerop i))
                         ;; To tolerate sloppily marked regions where
                         ;; the start is not in the first column, this
                         ;; skips all semicolons and spaces to the
                         ;; right of the cursor in Emacs.
                         (setq prefix1 (matching-prefix line chars))
                         (when first-line-indent
                           ;; We must not remove more than the common
                           ;; PREFIX.
                           (setq prefix1 (subseq prefix1 0
                                                 (max 0
                                                      (min (- (length prefix)
                                                              first-line-indent)
                                                           (length prefix1)))))
                           ;; PREFIX must not extend beyond the column
                           ;; of first form in the transcript.
                           (setq prefix (subseq prefix 0
                                                (min (length prefix)
                                                     (+ first-line-indent
                                                        (length prefix1))))))
                         (write-line (subseq line (length prefix1)) output))
                        (t
                         (write-line (subseq line (length prefix)) output))))))
     prefix1
     prefix)))

;;; Return the longest common prefix of lines of STRING, where the
;;; prefix is made of CHARS.
(defun longest-common-prefix (string chars &key first-line-special-p
                              (ignore-blank-lines-p t))
  (let ((longest-prefix nil))
    (with-input-from-string (s string)
      (loop for i upfrom 0
            for line = (read-line s nil nil)
            while line
            do (unless (or (and first-line-special-p (zerop i))
                           (and ignore-blank-lines-p (blankp line)))
                 (let ((prefix (matching-prefix line chars)))
                   (setq longest-prefix
                         (if longest-prefix
                             (subseq longest-prefix
                                     0 (or (mismatch longest-prefix prefix)
                                           (length longest-prefix)))
                             prefix))))))
    (or longest-prefix "")))

(defun matching-prefix (string chars)
  (let ((pos (position-if-not (lambda (char)
                                (find char chars))
                              string)))
    (if pos
        (subseq string 0 pos)
        string)))

;;; Read as many consecutive lines starting with PREFIX from STREAM as
;;; possible. From each matching line, strip the prefix and join them
;;; into a non-prefixed string conserving the newlines. As the second
;;; value, return the number of lines read.
;;;
;;; As the third value, return the first non-matching line (without
;;; the newline) or NIL at eof. The fourth value is whether the first
;;; non-matching line returned as the third value had a missing
;;; newline. The fifth value is file position of the start of the line
;;; returned as the third value.
;;;
;;; Note that reading (with prefix "..")
;;;
;;;     .. 1
;;;     .. 2
;;;
;;; gives "1~%2". If you want it to end with a newline, then:
;;;
;;;     .. 1
;;;     .. 2
;;;     ..
(defun read-prefixed-lines (stream prefix &key (first-line-prefix prefix)
                                            (eat-one-space-p t))
  (with-output-to-string (output)
    (loop for n-lines-read upfrom 0 do
      (multiple-value-bind (line missing-newline-p file-position)
          (read-line* stream nil nil)
        (let ((prefix (if (zerop n-lines-read) first-line-prefix prefix)))
          (when (or (null line)
                    (not (starts-with-subseq prefix line)))
            (return-from read-prefixed-lines
              (values (get-output-stream-string output)
                      line missing-newline-p file-position)))
          (unless (zerop n-lines-read)
            (terpri output))
          (let ((line (subseq line (length prefix))))
            (format output "~A" (if (and eat-one-space-p
                                         (plusp (length line))
                                         (char= (aref line 0) #\Space))
                                    (subseq line 1)
                                    line))))))))

(defun read-line* (stream &optional (eof-error-p t) eof-value)
  (let ((file-position (file-position stream)))
    (multiple-value-bind (line missing-newline-p)
        (read-line stream eof-error-p eof-value)
      (values line missing-newline-p file-position))))

;;; The inverse of READ-PREFIXED-LINES. If ADD-ONE-SPACE-P, a space
;;; character is printed after the prefix if the line is zero length.
(defun write-prefixed-lines (string prefix stream &key (add-one-space-p t)
                                                    (first-line-prefix prefix))
  (let ((last-newline-missing-p nil))
    (with-input-from-string (s string)
      (loop for n-lines-read upfrom 0 do
        (multiple-value-bind (line missing-newline-p) (read-line s nil nil)
          (unless line
            (return))
          (setq last-newline-missing-p missing-newline-p)
          (if (zerop (length line))
              (write-line prefix stream)
              (format stream "~A~A~A~%"
                      (if (zerop n-lines-read) first-line-prefix prefix)
                      (if add-one-space-p " " "")
                      line)))))
    (unless last-newline-missing-p
      (write-line prefix stream))))


(defsection @transcripts (:title "Transcripts")
  """What are transcripts for? When writing a tutorial, one often
  wants to include a REPL session with maybe a few defuns and a couple
  of forms whose output or return values are shown. Also, in a
  function's docstring an example call with concrete arguments and
  return values speaks volumes. A transcript is a text that looks like
  a REPL session, but which has a light markup for printed output and
  return values, while no markup (i.e. prompt) for Lisp forms. PAX
  transcripts may include output and return values of all forms, or
  only selected ones. In either case, the transcript itself can be
  easily generated from the source code. The main worry associated
  with including examples in the documentation is that they tend to
  get out-of-sync with the code. This is solved by being able to parse
  back and update transcripts."""
  (@transcribing-in-documentation section)
  (@transcribing-with-emacs section)
  (@transcript-dynenv section)
  (@transcript-api section)
  (@transcript-consistency-checking section))

(defsection @transcribing-in-documentation
    (:title "Transcribing in Documentation")
  """Consider this function:

  ```
  (defun foo (x)
    "Return X + 1 and log what happened."
    (format t "Adding 1 to ~S~%" x)
    (1+ x))
  ```

  Let's add an example to the docstring:

  ```
  (defun foo (x)
    "Return X + 1 and log what happened.
    For example,

    ```
    (foo 7)
    .. Adding 1 to 7
    ..
    => 8
    ```"
    (format t "Adding 1 to ~S~%" x)
    (1+ x))
  ```

  In this transcript above, output lines are prefixed with `".. "` and
  return values with `"=> "`. The transcript could have been generated
  with `(TRANSCRIBE` `"(foo 7)" *STANDARD-OUTPUT*)` or interactively
  by invoking `mgl-pax-transcribe-last-expression` in Emacs with the
  point right after `(FOO 7)` (see @TRANSCRIPT-API and
  @TRANSCRIBING-WITH-EMACS).

  If we want PAX to check that the transcript is consistent with the
  code when @GENERATING-DOCUMENTATION, we add the `cl-transcript` tag
  to the code block:

  ```
  (defun foo (x)
    "Return X + 1 and log what happened.
    For example,

    ```cl-transcript
    (foo 7)
    .. Adding 1 to 7
    ..
    => 8
    ```"
    (format t "Adding 1 to ~S~%" x)
    (+ x 2))
  ```

  Since the code was also changed to `(+ X 2)`, documenting this
  function causes the following error:

  ```
  (document #'foo)
  .. debugger invoked on TRANSCRIPTION-VALUES-CONSISTENCY-ERROR:
  ..   Transcription error: Readable value "8" in source is not EQUAL to "9".
  ..     [While documenting (MGL-PAX::FOO COMMON-LISP:FUNCTION)]
  ..
  ..   Form:
  ..   "(foo 7)"
  ```

  When @BROWSING-LIVE-DOCUMENTATION, any errors signalled during
  transcription are downgraded to warnings.

  All in all, transcripts are a handy tool especially when combined
  with the Emacs support to update them and with
  [PYTHONIC-STRING-READER][asdf:system]'s triple-quoted strings, that
  allow one to work with nested strings with less noise. The
  triple-quote syntax can be enabled with:

      (in-readtable pythonic-string-syntax)""")


(defsection @transcribing-with-emacs (:title "Transcribing with Emacs")
  """With [Emacs set up][@emacs-setup], we can have two Elisp
  functions at our disposal for working with transcripts.

  - `mgl-pax-transcribe-last-expression`: Evaluates a single form, and
    insert its output and return values into the current buffer.

  - `mgl-pax-retranscribe-region`: Retranscribes a region as with
    TRANSCRIBE :UPDATE-ONLY T. The region defaults to the innermost
    enclosing `cl-transcript` block if any.

  These functions work in any major and minor mode where the basic
  sexp movements are sufficiently Lisp-like. They also work anywhere
  in the buffer, including docstrings and comments at any indentation
  level. Like DOCUMENT, they understand the :DYNENV argument of
  `cl-transcript` and pass on the other arguments to TRANSCRIBE.

  For example, consider this buffer content, which may be part of a
  docstring. Move the cursor right after the end of the form as if you
  were to evaluate it with `C-x C-e`. The cursor is marked by `^`:

      (values (princ :hello) (list 1 2))^

  Now invoke the Elisp function `mgl-pax-transcribe` where the cursor
  is to insert the transcribed output and return values:

      (values (princ :hello) (list 1 2))
      .. HELLO
      => :HELLO
      => (1 2)
      ^

  Then, change the printed message and add comments to the second
  return value:

      (values (princ :hello-world) (list 1 2))
      .. HELLO
      => :HELLO
      => ;; This is a list.
         (1
          ;; This value is arbitrary.
          2)

  Obviously, the transcript is now out-of-date, and if it is in a
  `cl-transcript` code block in a docstring, then DOCUMENT will signal
  a TRANSCRIPTION-CONSISTENCY-ERROR. So, let's update the transcript
  by marking the region bounded by `|` and the cursor at `^` in this
  example:

      |(values (princ :hello-world) (list 1 2))
      .. HELLO
      => :HELLO
      => ;; This is a list.
         (1
          ;; This value is arbitrary.
          2)
      ^

  Then, invoke the Elisp function `mgl-pax-retranscribe-region` to get

      (values (princ :hello-world) (list 1 2))
      .. HELLO-WORLD
      => :HELLO-WORLD
      => ;; This is a list.
         (1
          ;; This value is arbitrary.
          2)
      ^

  Note how the indentation and comments of `(1 2)` were left alone,
  but the output and the first return value got updated.

  Also note that when the cursor is in `cl-transcript` code block,
  `mgl-pax-retranscribe-region` defaults to the whole block when there
  is no active region.
  
  Alternatively, `C-u 1 mgl-pax-transcribe` will emit commented markup:

      (values (princ :hello) (list 1 2))
      ;.. HELLO
      ;=> :HELLO
      ;=> (1 2)

  `C-u 0 mgl-pax-retranscribe-region` will turn commented into
  non-commented markup. In general, the numeric prefix argument is the
  index of the syntax to be used in *TRANSCRIBE-SYNTAXES*. Without a
  prefix argument, `mgl-pax-retranscribe-region` will not change the
  markup style.

  Finally, not only do both functions work at any indentation level
  but in comments too:

      ;;;; (values (princ :hello) (list 1 2))
      ;;;; .. HELLO
      ;;;; => :HELLO
      ;;;; => (1 2)

  @@TRANSCRIBE-STRIP-PREFIX

  The dynamic environment of the transcription is determined by the
  :DYNENV argument of the enclosing `cl-transcript` code block (see
  @TRANSCRIPT-DYNENV).""")

(defun transcribe-for-emacs (string default-syntax* update-only echo
                             cl-transcript-args-string first-line-indent)
  (with-swank ()
    (swank/backend:converting-errors-to-error-location
      (swank::with-buffer-syntax ()
        (let ((default-syntax (cond ((numberp default-syntax*)
                                     (first (elt *transcribe-syntaxes*
                                                 default-syntax*)))
                                    ((null default-syntax*)
                                     nil)
                                    (t (error "Unexpected default syntax ~S."
                                              default-syntax*)))))
          (multiple-value-bind (dynenv cl-transcript-args)
              (parse-cl-transcribe-args cl-transcript-args-string)
            (list :values (funcall (or dynenv #'funcall)
                                   (lambda ()
                                     (apply #'transcribe-for-emacs-1
                                            string first-line-indent
                                            :default-syntax default-syntax
                                            :update-only update-only
                                            :echo echo
                                            :check-consistency nil
                                            cl-transcript-args))))))))))

(defun parse-cl-transcribe-args (string)
  (let* ((args (read-from-string string nil nil))
         (dynenv (getf args :dynenv 'standard-transcribe-dynenv)))
    (remf args :dynenv)
    (values dynenv args)))

(defun transcribe-for-emacs-1 (string first-line-indent &rest transcribe-args)
  (multiple-value-bind (string prefix1 prefix)
      (note @@transcribe-strip-prefix
        "With `mgl-pax-transcribe-last-expression`, we strip the
        longest run of leading spaces and semicolons common to all
        lines of the expression in the buffer.

        For `mgl-pax-retranscribe-region`, the longest run is
        truncated so that it does not extend beyond the column of the
        first form to be transcribed. Without this rule, the syntax
        used

        ```cl-transcript
        ;;(list 1 2)
        ;;;=> (1
        ;;;->  2)
        ```

        would be ambiguous, as the `;=>` could refer to `=>` in the
        :DEFAULT syntax or to `;=>` in :COMMENTED-1."
        (strip-longest-common-prefix
         string "; " :first-line-indent first-line-indent))
    (multiple-value-bind (output did-something-p)
        (apply #'transcribe string nil transcribe-args)
      (list (format nil "~A~A"
                    (if first-line-indent prefix1 prefix)
                    (prefix-lines prefix output :exclude-first-line-p t
                                  :exclude-blank-p t))
            did-something-p))))


(defsection @transcript-dynenv
    (:title "Controlling the Dynamic Environment")
  """When the TRANSCRIBE function is called directly, the forms in the
  transcript are evaluated in the current dynamic environment with the
  following exceptions.

  - @@TRANSCRIBE-UPDATE-REPL-VARS

  - @@TRANSCRIBE-OUTPUT-VARS

  If TRANSCRIBE is invoked [by Emacs][ @transcribing-with-emacs] or
  [by the DOCUMENT function][ @transcribing-in-documentation], a new
  dynamic environment is established by
  STANDARD-TRANSCRIBE-DYNENV (similar in spirit to
  WITH-STANDARD-IO-SYNTAX), which binds printer and reader variables
  to fixed values to make IO more predictable. This default can be
  overridden with the :DYNENV argument of `cl-transcript`:

      ```cl-transcript (:dynenv my-transcript)
      ...
      ```

  In this case, instead of calling TRANSCRIBE directly,
  `MY-TRANSCRIPT` is called with a thunk (a function of no arguments)
  that wraps a call to TRANSCRIBE. Once `MY-TRANSCRIPT` establishes
  the desired dynamic environment, it calls its argument. The
  following definition of `MY-TRANSCRIPT` simply packages up oft-used
  settings to TRANSCRIBE.

  ```
  (defun my-transcript (fn)
    (standard-transcribe-dynenv
      (let ((*transcribe-check-consistency*
              '((:output my-transcript-output=)
                (:readable equal)
                (:unreadable nil))))
        (funcall fn))))

  (defun my-transcript-output= (string1 string2)
    (string= (my-transcript-normalize-output string1)
             (my-transcript-normalize-output string2)))

  (defun my-transcript-normalize-output (string)
    (squeeze-whitespace (delete-trailing-whitespace (delete-comments string))))
  ```

  The default for :DYNENV is STANDARD-TRANSCRIBE-DYNENV, and it is
  generally a good idea to use it as the above example does, but this
  is not required. Specify :DYNENV NIL explicitly if you want to use
  the current dynamic environment without any changes.

  A more involved solution could establish bindings for global
  variables set in transcripts, unintern symbols created or even
  create a temporary package for evaluation."""
  (standard-transcribe-dynenv function))

(defun/autoloaded standard-transcribe-dynenv (fn)
  "Bind printer and reader variables to standard values and call FN.

  The bindings are the same as with WITH-STANDARD-IO-SYNTAX, but

  - *PACKAGE* and *READTABLE* are unaffected (because both DOCUMENT
    and Emacs set these up);

  - *PRINT-READABLY* is NIL, *PRINT-PRETTY* is T, *PRINT-CIRCLE* is T
    and *PRINT-RIGHT-MARGIN* is 72.

  This function is the default for the :DYNENV argument of
  `cl-transcript`. A function that overrides the default may want to
  call STANDARD-TRANSCRIBE-DYNENV with a lambda that establishes more
  bindings."
  (let ((package *package*)
        (readtable *readtable*))
    (with-standard-io-syntax
      (let ((*package* package)
            (*readtable* readtable)
            (*print-readably* nil)
            #-clisp
            (*print-pretty* t)
            (*print-circle* t)
            (*print-right-margin* 72))
        (funcall fn)))))


(defsection @transcript-api (:title "Transcript API")
  (transcribe function)
  (*transcribe-check-consistency* variable)
  (*transcribe-syntaxes* variable)
  (transcription-error condition)
  (transcription-consistency-error condition)
  (transcription-output-consistency-error condition)
  (transcription-values-consistency-error condition))

(defvar/autoloaded *transcribe-syntaxes*
  '((:default
     (:output "..")
     ;; To give precedence to this no-value marker, it is listed
     ;; before :READABLE.
     (:no-value "=> ; No value")
     ;; Note that :READABLE-CONTINUATION is not needed because READ
     ;; knows where to stop anyway.
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
  `PREFIXES` is a list of `(PREFIX-ID PREFIX-STRING)` elements. For
  example the syntax :COMMENTED-1 looks like this:

  ```
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
  prefix is discarded when reading.

  See TRANSCRIBE for how the actual syntax to be used is selected.")

(defvar/autoloaded *transcribe-check-consistency* nil
  "The default value of TRANSCRIBE's CHECK-CONSISTENCY argument.")

(defun/autoloaded transcribe
    (input output &key update-only (include-no-output update-only)
           (include-no-value update-only) (echo t)
           (check-consistency *transcribe-check-consistency*)
           default-syntax (input-syntaxes *transcribe-syntaxes*)
           (output-syntaxes *transcribe-syntaxes*))
  """Read forms from INPUT and write them (iff ECHO) to OUTPUT
  followed by any output and return values produced by calling EVAL on
  the form.

  INPUT can be a stream or a string, while OUTPUT can be a stream or
  NIL, in which case output goes into a string.
  @@TRANSCRIBE-1ST-RETURN-VALUE @@TRANSCRIBE-2ND-RETURN-VALUE

  Go up to @TRANSCRIBING-WITH-EMACS for nice examples. A more
  mind-bending one is this:

  ```cl-transcript
  (transcribe "(princ 42) " nil)
  => "(princ 42)
  .. 42
  => 42
  "
  => T
  ```

  However, the above may be a bit confusing since this documentation
  uses TRANSCRIBE markup syntax in this very example, so let's do it
  differently. If we have a file with these contents:

  ```
  (values (princ 42) (list 1 2))
  ```

  it is transcribed to:

  ```
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

  ```
  (values (princ 42) (list 1 2))
  =>
  ```

  we get this:

  ```
  (values (princ 42) (list 1 2))
  => 42
  => (1 2)
  ```

  With UPDATE-ONLY, the printed output of a form is transcribed only
  if there were output markers in the source. Similarly, with
  UPDATE-ONLY, return values are transcribed only if there were value
  markers in the source.

  **No Output/Values**

  If the form produces no output or returns no values, then whether or
  not output and values are transcribed is controlled by
  INCLUDE-NO-OUTPUT and INCLUDE-NO-VALUE, respectively. By default,
  neither is on so:

  ```
  (values)
  ..
  =>
  ```

  is transcribed to

  ```
  (values)
  ```

  With UPDATE-ONLY true, we probably wouldn't like to lose those
  markers since they were put there for a reason. Hence, with
  UPDATE-ONLY, INCLUDE-NO-OUTPUT and INCLUDE-NO-VALUE default to true.
  So, with UPDATE-ONLY the above example is transcribed to:

  ```
  (values)
  ..
  => ; No value
  ```

  where the last line is the :NO-VALUE prefix.

  **Consistency Checks**

  If CHECK-CONSISTENCY is true, then TRANSCRIBE signals a continuable
  TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR whenever a form's output as a
  string is different from what was in INPUT, provided that INPUT
  contained the output. Similarly, for values, a continuable
  TRANSCRIPTION-VALUES-CONSISTENCY-ERROR is signalled if a value read
  from the source does not print as the as the value returned by EVAL.
  This allows readable values to be hand-indented without failing
  consistency checks:

  ```
  (list 1 2)
  => ;; This is commented, too.
     (1
        ;; Funny indent.
        2)
  ```

  See @TRANSCRIPT-CONSISTENCY-CHECKING for the full picture.

  **Unreadable Values**

  The above scheme involves READ, so consistency of unreadable values
  cannot be treated the same. In fact, unreadable values must even be
  printed differently for transcribe to be able to read them back:

  ```
  (defclass some-class () ())

  (defmethod print-object ((obj some-class) stream)
    (print-unreadable-object (obj stream :type t)
      (format stream "~%~%end")))

  (make-instance 'some-class)
  ==> #<SOME-CLASS 
  -->
  --> end>
  ```

  where `"==>"` is the :UNREADABLE prefix and `"-->"` is the
  :UNREADABLE-CONTINUATION prefix. As with outputs, a consistency
  check between an unreadable value from the source and the value from
  EVAL is performed with STRING= by default. That is, the value from
  EVAL is printed to a string and compared to the source value. Hence,
  any change to unreadable values will break consistency checks. This
  is most troublesome with instances of classes with the default
  PRINT-OBJECT method printing the memory address. See
  @TRANSCRIPT-FINER-GRAINED-CONSISTENCY-CHECKS.

  **Errors**

  If an ERROR condition is signalled, the error is printed to the
  output and no values are returned.

  ```cl-transcript
  (progn
    (print "hello")
    (error "no greeting"))
  ..
  .. "hello" 
  .. debugger invoked on SIMPLE-ERROR:
  ..   no greeting
  ```

  To keep the textual representation somewhat likely to be portable,
  the printing is done with `(FORMAT T "#<~S ~S>" (TYPE-OF
  ERROR) (PRINC-TO-STRING ERROR))`. SIMPLE-CONDITIONs are formatted to
  strings with SIMPLE-CONDITION-FORMAT-CONTROL and
  SIMPLE-CONDITION-FORMAT-ARGUMENTS.

  **Syntaxes**

  Finally, a transcript may employ different syntaxes for the output
  and values of different forms. When INPUT is read, the syntax for
  each form is determined by trying to match all prefixes from all
  syntaxes in INPUT-SYNTAXES against a line. If there are no output or
  values for a form in INPUT, then the syntax remains undetermined.

  When OUTPUT is written, the prefixes to be used are looked up in
  DEFAULT-SYNTAX of OUTPUT-SYNTAXES if DEFAULT-SYNTAX is not NIL. If
  DEFAULT-SYNTAX is NIL, then the syntax used by the same form in the
  INPUT is used or (if that could not be determined) the syntax of the
  previous form. If there was no previous form, then the first syntax
  if OUTPUT-SYNTAXES is used.

  To produce a transcript that's executable Lisp code,
  use :DEFAULT-SYNTAX :COMMENTED-1:

  ```
  (make-instance 'some-class)
  ;==> #<SOME-CLASS
  ;-->
  ;--> end>

  (list 1 2)
  ;=> (1
  ;->    2)
  ```

  To translate the above to uncommented syntax, use :DEFAULT-SYNTAX
  :DEFAULT. If DEFAULT-SYNTAX is NIL (the default), the same syntax
  will be used in the output as in the input as much as possible."""
  (let ((transcript (read-transcript input :syntaxes input-syntaxes)))
    (values (write-transcript transcript output
                              :update-only update-only
                              :check-consistency check-consistency
                              :include-no-output include-no-output
                              :include-no-value include-no-value
                              :echo echo
                              :default-syntax default-syntax
                              :syntaxes output-syntaxes)
            (note @@transcribe-2nd-return-value
              "As the second value, a generalized boolean indicating
              whether any form was transcribed is returned."
              (not (not (find-if-not
                         (lambda (command)
                           (let ((form-and-string (first command)))
                             (eq (first form-and-string) 'eof)))
                         transcript)))))))


;;;; Prefix utilities

(defun find-syntax (syntax)
  (or (find syntax *transcribe-syntaxes* :key #'first)
      (error "Cannot find syntax ~S.~%" syntax)))

(defun find-prefix (id syntax &key (errorp t))
  (flet ((foo (syntax-definition)
           (list (second (find id (rest syntax-definition) :key #'first))
                 (first syntax-definition))))
    (destructuring-bind (prefix syntax) (if syntax
                                            (foo (find-syntax syntax))
                                            (some #'foo *transcribe-syntaxes*))
      (when (and (not prefix) errorp)
        (error "Cannot find prefix with id ~S~%" id))
      (values prefix syntax ))))

;;; Find a syntax and a prefix that matches LINE. If SYNTAX, then only
;;; consider prefixes in that syntax. Return the id of the matching
;;; prefix, its string and the id of syntax.
(defun match-prefixes (line syntax-id)
  (flet ((foo (syntax-definition)
           (let ((match (find-if (lambda (entry)
                                   (starts-with-subseq
                                    (second entry) line))
                                 (rest syntax-definition))))
             (if match
                 (list (first match) (second match) (first syntax-definition))
                 nil))))
    (values-list (if syntax-id
                     (foo (find-syntax syntax-id))
                     (some #'foo *transcribe-syntaxes*)))))


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
;;;; string (including preceding comments). It also has a variable
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
  (let ((captures (remove-if-not #'output-capture-p
                                 (command-captures command))))
    (when (< 1 (length captures))
      (transcription-error* "Multiple output captures found."))
    (first captures)))

(defun command-value-captures (command)
  (remove-if-not #'value-capture-p (command-captures command)))

(defun check-command-values (command)
  (when (and (some #'no-value-capture-p (command-captures command))
             (< 1 (count-if #'value-capture-p (command-captures command))))
    (transcription-error* "Found both :NO-VALUE marker and other values.")))

(defun capture-id (capture)
  (first capture))

(defun capture-value (capture)
  (second capture))

(defun filter-captures (captures &rest ids)
  (remove-if-not (lambda (capture)
                   (member (capture-id capture) ids))
                 captures))

(defun output-capture-p (capture)
  (eq (capture-id capture) :output))

(defun output-string (output-capture)
  (assert (output-capture-p output-capture))
  (capture-value output-capture))

(defun value-capture-p (capture)
  (or (no-value-capture-p capture)
      (readable-capture-p capture)
      (unreadable-capture-p capture)))

(defun no-value-capture-p (capture)
  (eq (capture-id capture) :no-value))

(defun readable-capture-p (capture)
  (eq (capture-id capture) :readable))

(defun readable-object (readable-capture)
  (assert (readable-capture-p readable-capture))
  (first (capture-value readable-capture)))

(defun readable-string (readable-capture)
  (assert (readable-capture-p readable-capture))
  (second (capture-value readable-capture)))

(defun unreadable-capture-p (capture)
  (eq (capture-id capture) :unreadable))

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
         (with-input-from-string (stream (read-file-into-string input))
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

(defun read-transcript (input &key (syntaxes *transcribe-syntaxes*))
  (check-type input (or stream string))
  (with-input-stream (stream input)
    (with-load-environment (stream)
      (let ((*transcribe-syntaxes* syntaxes)
            (transcript ())
            (partial-line-p nil)
            ;; file position of the beginning of LINE
            (file-position (file-position stream)))
        (multiple-value-bind (line missing-newline-p)
            (read-line stream nil nil)
          (declare (ignorable missing-newline-p))
          (handler-case
              (loop while line do
                (multiple-value-bind (prefix-id prefix syntax-id)
                    (and (not partial-line-p)
                         (match-prefixes line (command-syntax-id
                                               (first transcript))))
                  (let ((match-length (length prefix))
                        value
                        file-position-1)
                    (file-position stream (+ file-position match-length))
                    (multiple-value-setq
                        (value line missing-newline-p file-position-1
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
                     file-position
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
                        ,@(multiple-value-list
                           (read-line* stream nil nil))
                        nil)))
        ((eq prefix-id :readable-continuation)
         (transcription-error* "Prefix ~S must be preceded by ~S."
                               (find-prefix prefix-id syntax-id)
                               (find-prefix :readable syntax-id)))
        ((eq prefix-id :unreadable-continuation)
         (transcription-error* "Prefix ~S must be preceded by ~S."
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
  (multiple-value-bind (string next-line missing-newline-p file-position)
      (read-prefixed-lines stream continuation-prefix
                           :first-line-prefix "")
    (let ((form-and-string (with-input-from-string (stream string)
                             (read-form-and-string stream 'eof))))
      (when (eq (first form-and-string) 'eof)
        (transcription-error* "Unexpected EOF while parsing readable value."))
      (values form-and-string next-line missing-newline-p file-position nil))))

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
                   ,@(multiple-value-list (read-line* stream nil nil))
                   nil))))

;;; Read a sexp from STREAM or return EOF. The second value is a
;;; string of all of the characters that were read even if EOF
;;; (whitespace, comments).
(defun read-form-and-string (stream eof &key preserve-whitespace-p)
  (let* ((old-file-position (file-position stream))
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
  (note @@transcribe-1st-return-value
    "The return value is the OUTPUT stream or the string that was
    constructed.")
  (cond ((streamp output)
         (progn (funcall fn output)
                output))
        ((null output)
         (with-output-to-string (stream)
           (funcall fn stream)))
        (t
         (assert nil))))

(defun write-transcript (transcript output
                         &key update-only (include-no-output update-only)
                         (include-no-value update-only)
                         (echo t) check-consistency
                         default-syntax (syntaxes *transcribe-syntaxes*))
  (check-type output (or stream null))
  (with-output-stream (stream output)
    (let ((*transcribe-syntaxes* syntaxes)
          (last-syntax-id default-syntax)
          (output-checker (consistency-checker check-consistency :output))
          (readable-checker (consistency-checker check-consistency :readable))
          (unreadable-checker (consistency-checker check-consistency
                                                   :unreadable))
          (* nil) (** nil) (*** nil)
          (/ ()) (// ()) (/// ())
          (+ nil) (++ nil) (+++ nil))
      (dolist (command transcript)
        (let ((form (command-form command))
              (form-as-string (command-string command)))
          ;; When DEFAULT-SYNTAX is NIL, we default to the syntax of
          ;; this command or the last command with known syntax.
          (setq last-syntax-id (or default-syntax
                                   (command-syntax-id command)
                                   last-syntax-id))
          (when echo
            (format stream "~A" (command-string command))
            (unless (eq form 'eof)
              (terpri stream)))
          (unless (eq form 'eof)
            (setf - form)
            (multiple-value-bind (form-output form-values errorp)
                (eval-and-capture form)
              (update-repl-vars form-values errorp)
              (let ((output-capture (command-output-capture command)))
                (when (and output-checker output-capture)
                  (check-output-consistency
                   nil form-as-string form-output
                   (output-string output-capture) output-checker))
                (transcribe-output stream form-output
                                   (or output-capture errorp)
                                   last-syntax-id update-only
                                   include-no-output))
              (let ((value-captures (command-value-captures command)))
                (when (or readable-checker unreadable-checker)
                  (check-values-consistency
                   nil form-as-string form-values value-captures
                   readable-checker unreadable-checker
                   form-output errorp))
                (transcribe-values stream form-values value-captures
                                   last-syntax-id update-only
                                   include-no-value)))))))))

(defun update-repl-vars (values errorp)
  (note @@transcribe-update-repl-vars
    """The variables [*][variable], [**][], [\***][], [/][variable],
    [//][], [///][], [-][variable], [+][variable], [++][], [+++][] are
    locally bound and updated as in a new [REPL]["Lisp read-eval-print
    loop" clhs] session."""
    (unless errorp
      (setf /// //
            // /
            / values
            *** **
            ** *
            * (car values))))
  (setf +++ ++
        ++ +
        + -))

(defun readable-object-p (object)
  (null
   (nth-value 1 (ignore-errors
                 (values (read-from-string (prin1-to-string object)))))))

(defun consistency-checker (check-consistency what)
  (let ((checker (if (listp check-consistency)
                     (second (find what check-consistency :key #'first))
                     (not (not check-consistency)))))
    (cond ((eq checker nil) nil)
          ((eq checker t) 'equal)
          (t checker))))

(defun check-output-consistency (stream form-as-string output captured-output
                                 output-checker)
  (check-type output string)
  (check-type captured-output (or string null))
  (unless (funcall output-checker output (or captured-output ""))
    (consistency-error
     'transcription-output-consistency-error
     stream form-as-string
     "Outputs not ~S.~%~%Source: ~:_~S~%~%Output: ~:_~S~%"
     output-checker captured-output output)))

(defun check-values-consistency (stream form-as-string values value-captures
                                 readable-checker unreadable-checker
                                 form-output errorp)
  (when value-captures
    (let ((value-captures (if (and (= 1 (length value-captures))
                                   (no-value-capture-p (first value-captures)))
                              ()
                              value-captures)))
      (cond ((/= (length values) (length value-captures))
             (consistency-error
              'transcription-values-consistency-error
              stream form-as-string
              "Source had ~S return values ~:_while there are actually ~S.~:@_~
              ~@[Note that there was an error during evaluation:~:_~A~]"
              (length value-captures) (length values)
              (when errorp
                form-output)))
            (t
             (loop for value in values
                   for value-capture in value-captures
                   do (check-value-consistency stream form-as-string
                                               value value-capture
                                               readable-checker
                                               unreadable-checker)))))))

(defun check-value-consistency (stream form-as-string value value-capture
                                readable-checker unreadable-checker)
  (assert (not (no-value-capture-p value-capture)))
  (flet ((stringify (object)
           (prin1-to-string object)))
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
             (when readable-checker
               (let ((readable-value-string (stringify (readable-object
                                                        value-capture))))
                 (unless (funcall readable-checker (stringify value)
                                  readable-value-string)
                   (consistency-error
                    'transcription-values-consistency-error
                    stream form-as-string
                    "Readable value ~:_~S ~:_in source is not ~S to ~:_~S."
                    readable-value-string readable-checker
                    (stringify value))))))
            ((and unreadable-checker
                  (not (funcall unreadable-checker (stringify value)
                                (unreadable-string value-capture))))
             (consistency-error
              'transcription-values-consistency-error
              stream form-as-string
              "Unreadable value ~:_~S ~:_in source is not ~S to ~:_~S."
              (unreadable-string value-capture)
              unreadable-checker
              (stringify value)))))))

(defun eval-and-capture (form)
  (let* ((buffer (make-array 0 :element-type 'character
                             :fill-pointer 0 :adjustable t))
         (values (with-output-to-string (output buffer)
                   (note @@transcribe-output-vars
                     "*STANDARD-OUTPUT*, *ERROR-OUTPUT*,
                     *TRACE-OUTPUT*, *DEBUG-IO*, *QUERY-IO* and
                     *TERMINAL-IO* are redirected to capture the
                     output.")
                   (let ((*standard-output* output)
                         (*error-output* output)
                         (*trace-output* output)
                         (*debug-io* output)
                         (*query-io* output)
                         (*terminal-io* output))
                     (flet ((handle-error (c)
                              (print-condition c "debugger invoked on"
                                               ":" "")
                              (return-from eval-and-capture
                                (values buffer () t))))
                       (handler-case
                           (with-debugger-hook #'handle-error
                             (multiple-value-list (eval form)))
                         (error (e)
                           (handle-error e))))))))
    (values buffer values)))

(defun print-condition (c prefix midfix suffix)
  (format t "~&~A ~S~A~%~A~A" prefix (type-of c) midfix
          (prefix-lines "  "
                        (if (typep c 'simple-condition)
                            (apply #'format nil
                                   (simple-condition-format-control c)
                                   (simple-condition-format-arguments c))
                            (princ-to-string c)))
          suffix))

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
                      stream value syntax-id))))))

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
            (write-prefixed-lines (prin1-to-string value) continuation-prefix
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

(define-condition transcription-error (error condition-context-mixin)
  (;; The file position at which the error was encountered or NIL if
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
  (:documentation "Represents syntactic errors in the SOURCE argument
  of TRANSCRIBE and also serves as the superclass of
  TRANSCRIPTION-CONSISTENCY-ERROR.")
  (:report (lambda (condition stream)
             (format stream
                     "~@<Transcription error~@[ ~:_at position ~A~]:~
                      ~:_ ~?~%~
                      Form: ~:_~S~:@>"
                     (transcription-error-file-position condition)
                     (transcription-error-message condition)
                     (transcription-error-message-args condition)
                     (transcription-error-form-as-string condition)))))

(defun print-reference-with-package (reference)
  (let ((*package* (find-package :keyword)))
    (format nil "~S" reference)))

(defun transcription-error (file-position form-as-string
                            message &rest message-args)
  (error 'transcription-error
         :file-position file-position
         :form-as-string form-as-string
         :message message
         :message-args message-args))

(defun transcription-error* (message &rest message-args)
  (apply #'transcription-error nil nil message message-args))

(define-condition transcription-consistency-error (transcription-error)
  ()
  (:documentation "A common superclass for
  TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR and
  TRANSCRIPTION-VALUES-CONSISTENCY-ERROR."))

(define-condition transcription-output-consistency-error
    (transcription-consistency-error)
  ()
  (:documentation "Signalled (with CERROR) by TRANSCRIBE when invoked
  with :CHECK-CONSISTENCY and the output of a form is not the same as
  what was parsed."))

(define-condition transcription-values-consistency-error
    (transcription-consistency-error)
  ()
  (:documentation "Signalled (with CERROR) by TRANSCRIBE when invoked
  with :CHECK-CONSISTENCY and the values of a form are inconsistent
  with their parsed representation."))

(defun consistency-error (class stream form-as-string
                          message &rest message-args)
  (cerror "Continue." class
          :file-position (and stream (file-position stream))
          :form-as-string form-as-string
          :message message
          :message-args message-args))


(defsection @transcript-consistency-checking
    (:title "Transcript Consistency Checking")
  """The main use case for consistency checking is detecting
  out-of-date examples in documentation, although using it for writing
  tests is also a possibility. Here, we focus on the former.

  When a Markdown code block tagged `cl-transcript` is processed
  during @GENERATING-DOCUMENTATION, the code in it is replaced with
  the output of with `(TRANSCRIBE <CODE> NIL :UPDATE-ONLY T
  :CHECK-CONSISTENCY T)`. Suppose we have the following example of the
  function `GREET`, that prints `hello` and returns 7.

      ```cl-transcript
      (greet)
      .. hello
      => 7
      ```

  Now, if we change `GREET` to print or return something else, a
  TRANSCRIPTION-CONSISTENCY-ERROR will be signalled during
  documentation generation. Then we may fix the documentation or
  [CONTINUE][restart] from the error.

  By default, comparisons of previous to current output, readable and
  unreadable return values are performed with STRING=, EQUAL, and
  STRING=, respectively, which is great in the simple case.
  Non-determinism aside, exact matching becomes brittle as soon as the
  notoriously unportable pretty printer is used or when unreadable
  objects are printed with their `#<>` syntax, especially when
  PRINT-UNREADABLE-OBJECT is used with `:IDENTITY T`.
  """
  (@transcript-finer-grained-consistency-checks section)
  (@transcript-utilities-for-consistency-checking section))

(defsection @transcript-finer-grained-consistency-checks
    (:title "Finer-Grained Consistency Checks")
  """To get around this problem, consistency checking of output,
  readable and unreadable values can be customized individually by
  supplying TRANSCRIBE with a CHECK-CONSISTENCY argument
  like `((:OUTPUT <OUTPUT-CHECK>) (:READABLE
  <READABLE-CHECK>) (:UNREADABLE <UNREADABLE-CHECK>))`. In this case,
  `<OUTPUT-CHECK>` may be NIL, T, or a function designator.

  - If it's NIL or there is no :OUTPUT entry in the list, then the
    output is not checked for consistency.
  - If it's T, then the outputs are compared with the default,
    STRING=.
  - If it's a function designator, then it's called with two strings
    and must return whether they are consistent with each other.

  The case of `<READABLE-CHECK>` and `<UNREADABLE-CHECK>` is similar.

  Code blocks tagged `cl-transcript` can take arguments, which they
  pass on to TRANSCRIBE. The following shows how to check only the
  output.

      ```cl-transcript (:check-consistency ((:output t)))
      (error "Oh, no.")
      .. debugger invoked on SIMPLE-ERROR:
      ..   Oh, no.

      (make-condition 'simple-error)
      ==> #<SIMPLE-ERROR {1008A81533}>
      ```

  It is often a good idea to package up these settings in the
  :DYNENV argument of `cl-transcript` (see @TRANSCRIPT-DYNENV).
  """)


(defsection @transcript-utilities-for-consistency-checking
    (:title "Utilities for Consistency Checking")
  (squeeze-whitespace function)
  (delete-trailing-whitespace function)
  (delete-comments function))

(defun/autoloaded squeeze-whitespace (string)
  "Replace consecutive whitespace characters with a single space in
  STRING and trim whitespace from the right. This is useful to undo
  the effects of pretty printing when building comparison functions
  for TRANSCRIBE."
  (with-output-to-string (out)
    (let ((prev-whitespace-p nil))
      (loop for char across string
            do (let ((whitespacep (whitespacep char)))
                 ;; Nothing to do for whitespace chars until followed
                 ;; by a non-whitespace char.
                 (unless whitespacep
                   (when prev-whitespace-p
                     (write-char #\Space out))
                   (write-char char out))
                 (setq prev-whitespace-p whitespacep))))))

(defun/autoloaded delete-trailing-whitespace (string)
  "Delete whitespace characters after the last non-whitespace
  character in each line in STRING."
  (flet ((delete-on-one-line (string)
           (string-right-trim *whitespace-chars* string)))
    (with-output-to-string (out)
      (with-input-from-string (in string)
        (loop for line = (read-line in nil nil)
              while line
              do (write-line (delete-on-one-line line) out))))))

(defun/autoloaded delete-comments (string &key (pattern ";"))
  """For each line in STRING delete the rest of the line after and
  including the first occurrence of PATTERN. On changed lines, delete
  trailing whitespace too. This function does not parse STRING as Lisp
  forms, hence all occurrences of PATTERN (even those seemingly in
  string literals) are recognized as comments.

  Let's define a comparison function:

  ```cl-transcript (:dynenv pax-std-env)
  (defun string=/no-comments (string1 string2)
    (string= (delete-comments string1) (delete-comments string2)))
  ```

  And use it to check consistency of output:

      ```cl-transcript (:check-consistency ((:output string=/no-comments)))
      (format t "hello~%world")
      .. hello     ; This is the first line.
      .. world     ; This is the second line.
      ```

  Just to make sure the above example works, here it is without being
  quoted.

  ```cl-transcript (:check-consistency ((:output string=/no-comments)))
  (format t "hello~%world")
  .. hello     ; This is the first line.
  .. world     ; This is the second line.
  ```"""
  (flet ((delete-on-one-line (string)
           (let ((pos (search pattern string)))
             (if pos
                 (string-right-trim *whitespace-chars* (subseq string 0 pos))
                 string))))
    (with-output-to-string (out)
      (with-input-from-string (in string)
        (loop for line = (read-line in nil nil)
              while line
              do (write-line (delete-on-one-line line) out))))))

(defun/autoloaded ensure-transcribe-loaded ())
