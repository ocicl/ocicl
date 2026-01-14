(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @pax-locatives (:title "PAX Locatives")
  "To the DREF::@BASIC-LOCATIVE-TYPES defined by DRef,
  PAX adds a few of its own."
  (section locative)
  (glossary-term locative)
  (note locative)
  (dislocated locative)
  (argument locative)
  (include locative)
  (docstring locative)
  (go locative)
  (clhs locative))


;;;; SECTION locative

(define-locative-type section (variable)
  "Refers to a [SECTION][class] defined by DEFSECTION.

  SECTION is EXPORTABLE-LOCATIVE-TYPE-P but not exported by
  default (see EXPORTABLE-REFERENCE-P).")

(define-locator section ((section section))
  (make-instance 'section-dref :name (section-name section) :locative 'section))

(define-lookup section (symbol locative-args)
  (unless (and (symbolp symbol)
               (boundp symbol)
               (typep (symbol-value symbol) 'section))
    (locate-error))
  (make-instance 'section-dref :name symbol :locative 'section))

(defmethod resolve* ((dref section-dref))
  (symbol-value (dref-name dref)))

(defmethod docstring* ((dref section-dref))
  nil)


;;;; GLOSSARY-TERM locative

(defun glossary-term-title-or-name (glossary-term)
  (or (glossary-term-title glossary-term)
      (prin1-to-string (glossary-term-name glossary-term))))

(defmethod print-object ((glossary-term glossary-term) stream)
  (print-unreadable-object (glossary-term stream :type t)
    (format stream "~a" (glossary-term-name glossary-term))))

(define-locative-type glossary-term (variable)
  "Refers to a [GLOSSARY-TERM][class] defined by DEFINE-GLOSSARY-TERM.

  GLOSSARY-TERM is EXPORTABLE-LOCATIVE-TYPE-P but not exported by
  default (see EXPORTABLE-REFERENCE-P).")

(define-locator glossary-term ((glossary-term glossary-term))
  (make-instance 'glossary-term-dref :name (glossary-term-name glossary-term)
                                     :locative 'glossary-term))

(define-lookup glossary-term (symbol locative-args)
  (unless (and (symbolp symbol)
               (boundp symbol)
               (typep (symbol-value symbol) 'glossary-term))
    (locate-error))
  (make-instance 'glossary-term-dref :name symbol :locative 'glossary-term))

(defmethod resolve* ((dref glossary-term-dref))
  (symbol-value (dref-name dref)))

(defmethod docstring* ((dref glossary-term-dref))
  (glossary-term-docstring (resolve dref)))


;;;; NOTE locative

(define-locative-type note ()
  "Refers to named notes defined by the NOTE macro.

  If a single link would be made to a NOTE (be it either a
  @SPECIFIC-LINK or an unambiguous @UNSPECIFIC-LINK), then the NOTE's
  DOCSTRING is included as if with the DOCSTRING locative.

  NOTE is EXPORTABLE-LOCATIVE-TYPE-P but not exported by default (see
  EXPORTABLE-REFERENCE-P).")

(define-lookup note (name locative-args)
  (unless (and (symbolp name)
               (symbol-package name)
               (definition-property (xref name 'note) 'note))
    (locate-error))
  (make-instance 'note-dref :name name :locative 'note))


;;;;; GO locative

(define-pseudo-locative-type (go (name locative)) ()
  """Redirect to a definition in the context of the DREF::@REFERENCE
  designated by NAME and LOCATIVE. This PSEUDO locative is intended for
  things that have no explicit global definition.

  As an example, consider this part of a hypothetical documentation of
  CLOS:

      (defsection @clos ()
        (defmethod macro)
        (call-next-method (go (defmethod macro))))

  The GO reference exports the symbol CALL-NEXT-METHOD and also
  produces a terse redirection message in the documentation.

  GO behaves as described below.

  - A GO reference RESOLVEs to what NAME with LOCATIVE resolves to:

      ```cl-transcript (:dynenv pax-std-env)
      (resolve (dref 'xxx '(go (print function))))
      ==> #<FUNCTION PRINT>
      => T
      ```

  - The DOCSTRING of a GO reference is NIL.

  - SOURCE-LOCATION (thus `\\M-.`) returns the source location of the
    embedded reference:

      ```cl-transcript
      (equal (source-location (dref 'xxx '(go (print function))))
             (source-location (dref 'print 'function)))
      => T
      ```"""
  (defclass go-dref ()
    ((target-dref :initarg :target-dref :reader go-target-dref))))

(define-lookup go (name locative-args)
  (destructuring-bind ((go-name go-locative)) locative-args
    (let ((go-dref (dref go-name go-locative)))
      (make-instance 'go-dref :name name
                              :locative `(go (,(dref-name go-dref)
                                              ,(dref-locative go-dref)))
                              :target-dref go-dref))))

(defun go-embedded-dref (go-dref)
  (apply #'dref (first (dref-locative-args go-dref))))

;;; Delegate all known extension methods. For GO definitions to
;;; automatically work with other operations, another approach would
;;; be needed.

(defmethod resolve* ((dref go-dref))
  (resolve* (go-embedded-dref dref)))

(defmethod arglist* ((dref go-dref))
  (arglist* (go-embedded-dref dref)))

(defmethod docstring* ((dref go-dref))
  (docstring* (go-embedded-dref dref)))

(defmethod source-location* ((dref go-dref))
  (source-location (go-embedded-dref dref)))


;;;; DISLOCATED locative

(define-pseudo-locative-type dislocated ()
  "Refers to a symbol in a non-specific context. Useful for
  suppressing @UNSPECIFIC-AUTOLINKing. For example, if there is a
  function called `FOO` then

      `FOO`

  will be linked (if *DOCUMENT-LINK-CODE*) to its definition. However,

      [`FOO`][dislocated]

  will not be. With a dislocated locative, LOCATE always fails with a
  LOCATE-ERROR condition. Also see @ESCAPING-AUTOLINKING.

  DISLOCATED references do not RESOLVE.")

(define-lookup dislocated (symbol locative-args)
  (declare (ignorable symbol))
  (locate-error "~S can never be located." 'dislocated))


;;;; ARGUMENT locative

(define-pseudo-locative-type argument ()
  """An alias for [DISLOCATED][locative], so that one can refer to an
  argument of a macro without accidentally linking to a class that has
  the same name as that argument. In the following example,
  [FORMAT][dislocated] may link to CL:FORMAT (if we generated
  documentation for it):

  ```
  "See FORMAT in DOCUMENT."
  ```

  Since ARGUMENT is a locative, we can prevent that linking by writing:

  ```
  "See the FORMAT argument of DOCUMENT."
  ```

  ARGUMENT references do not RESOLVE.""")

(defvar *local-references* ())

(defun find-local-reference (xref)
  (find xref *local-references* :test #'xref=))

(define-lookup argument (symbol locative-args)
  (if (find-local-reference (xref symbol 'argument))
      (make-instance 'dref :name symbol :locative 'argument)
      (locate-error)))

(defmethod map-definitions-of-name (fn name (locative-type (eql 'argument)))
  (declare (ignorable fn name))
  (when-let (dref (dref name 'argument nil))
    (funcall fn dref))
  nil)

(defmethod map-definitions-of-type (fn (locative-type (eql 'argument)))
  (declare (ignorable fn))
  (dolist (dref *local-references*)
    (when (eq (dref-locative-type dref) 'argument)
      (funcall fn dref)))
  nil)


;;;; DOCSTRING

(define-pseudo-locative-type docstring ()
  "[translate-docstring-links function][docstring]

  There is no way to LOCATE DOCSTRINGs, so nothing to RESOLVE either.")

(define-lookup docstring (symbol locative-args)
  (declare (ignore symbol))
  (locate-error "DOCSTRING can never be located."))


;;;; INCLUDE locative

(define-pseudo-locative-type (include source &key line-prefix header footer
                                      header-nl footer-nl)
    ()
  """This PSEUDO locative refers to a region of a file. SOURCE can be a
  [STRING][type] or a [PATHNAME][type], in which case the whole file
  is being pointed to, or it can explicitly supply START, END
  locatives. INCLUDE is typically used to include non-lisp files in
  the documentation (say Markdown or Elisp as in the next example) or
  regions of Lisp source files. This can reduce clutter and
  duplication.

  ```
  (defsection @example-section ()
    (mgl-pax.el (include #.(asdf:system-relative-pathname
                            :mgl-pax "src/mgl-pax.el")
                         :header-nl "```elisp" :footer-nl "```"))
    (foo-example (include (:start (dref-ext:make-source-location function)
                           :end (dref-ext:source-location-p function))
                          :header-nl "```"
                          :footer-nl "```")))
  ```

  In the above example, when documentation is generated, the entire
  `src/mgl-pax.el` file is included in the Markdown output surrounded
  by the strings given as HEADER-NL and FOOTER-NL. The documentation
  of `FOO-EXAMPLE` will be the region of the file from the
  SOURCE-LOCATION of the START reference (inclusive) to the
  SOURCE-LOCATION of the END reference (exclusive). If only one of
  START and END is specified, then they default to the beginning and
  end of the file, respectively.

  Since START and END are literal references, pressing `\\M-.` on
  `PAX.EL` will open the `src/mgl-pax.el` file and put the cursor on
  its first character. `\\M-.` on `FOO-EXAMPLE` will go to the source
  location of the `FOO` function.

  With the LAMBDA locative, one can specify positions in arbitrary
  files.

  - SOURCE is either an absolute pathname designator or a list
    matching the [destructuring lambda list][clhs] `(&KEY START END)`,
    where START and END must be NIL or `(<NAME> <LOCATIVE>)`
    lists (not evaluated) like a DEFSECTION entry. Their
    SOURCE-LOCATIONs constitute the bounds of the region of the file
    to be included. Note that the file of the source location of START
    and END must be the same. If SOURCE is a pathname designator, then
    it must be absolute so that the locative is context independent.

  - If specified, LINE-PREFIX is a string that's prepended to each
    line included in the documentation. For example, a string of four
    spaces makes Markdown think it's a code block.

  - HEADER and FOOTER, if non-NIL, are printed before the included
    string.

  - HEADER-NL and FOOTER-NL, if non-NIL, are printed between two
    FRESH-LINE calls.

  INCLUDE is not EXPORTABLE-LOCATIVE-TYPE-P, and INCLUDE references do
  not RESOLVE.""")

(define-lookup include (name locative-args)
  (make-instance 'include-dref
                 :name name
                 :locative (cons 'include locative-args)))

(define-condition include-error (error)
  ((format-control :initarg :format-control :reader format-control)
   (format-args :initarg :format-args :reader format-args))
  (:report (lambda (condition stream)
             (format stream "~@<~?~:@>" (format-control condition)
                     (format-args condition)))))

(defun include-error (format-control &rest format-args)
  (error 'include-error :format-control format-control
                        :format-args format-args))

;;; Return the filename plus the START, END source locations of the
;;; region to be included.
(defun include-region (source)
  (cond ((or (stringp source) (pathnamep source))
         (assert (uiop/pathname:absolute-pathname-p source) ()
                 "Pathnames given as the SOURCE argument of the ~
                 INCLUDE locative must be absolute, but ~S is not."
                 source)
         (values source nil nil))
        ((and source (listp source))
         (destructuring-bind (&key start end) source
           (let ((start* (when start
                           (source-location (section-entry-to-xref start))))
                 (end* (when end
                         (source-location (section-entry-to-xref end)))))
             (when start
               (check-source-location start start*))
             (when end
               (check-source-location end end*))
             (let ((start-file (when start* (source-location-file start*)))
                   (end-file (when end* (source-location-file end*))))
               (when (and start* end*
                          (string/= (namestring (truename start-file))
                                    (namestring (truename end-file))))
                 (include-error
                  "~S starts in file ~S and ends in another file ~S."
                  'include start-file end-file))
               (let ((file (or start-file end-file)))
                 (unless file
                   (include-error "No file specified."))
                 (values file start* end*))))))
        (t
         (include-error "Malformed ~S ~S." 'include 'source source))))

(defun check-source-location (ref location)
  (unless (source-location-p location)
    (include-error "~S of ~S is ~S, which is not SOURCE-LOCATION-P."
                   'source-location ref location)))

(defmethod source-location* ((dref include-dref))
  (handler-case
      (multiple-value-bind (file start-location)
          (include-region (first (dref-locative-args dref)))
        (or start-location
            (make-source-location :file file)))
    (include-error (c)
      `(:error ,(princ-to-string c)))))


;;;; CLHS locative

(define-pseudo-locative-type (clhs &optional nested-locative) ()
  """Refers to definitions, glossary entries, sections, issues and
  issue summaries in the Common Lisp HyperSpec. These have no source
  location so @M-. will not work. What works is linking in
  documentation, including @BROWSING-LIVE-DOCUMENTATION. The generated
  links are relative to *DOCUMENT-HYPERSPEC-ROOT* and work even if
  *DOCUMENT-LINK-TO-HYPERSPEC* is NIL. All matching is
  case-insensitive.

  - *definitions*: These are typically unnecessary as DOCUMENT will
    produce the same link for e.g. `\\PPRINT`, `[PPRINT][function]`,
    or `[PPRINT][]` if *DOCUMENT-LINK-TO-HYPERSPEC* is non-NIL and the
    PPRINT function in the running Lisp is not @LINKABLE. When
    @BROWSING-LIVE-DOCUMENTATION, a slight difference is that
    everything is linkable, so using the CLHS link bypasses the page
    with the definition in the running Lisp.

      - *unambiguous definition*: `[pprint][clhs]` ([pprint][clhs])

      - *disambiguation page*: `[function][clhs]` ([function][clhs])

      - *specific*: `[function][(clhs class)]` ([function][(clhs class)])

  - *glossary terms*:

      - `[lambda list][(clhs glossary-term)]`
        ([lambda list][(clhs glossary-term)])

  - *issues*:

      - `[ISSUE:AREF-1D][clhs]` ([ISSUE:AREF-1D][clhs])

      - `[ISSUE:AREF-1D][(clhs section)]` ([ISSUE:AREF-1D][clhs])

  - *issue summaries*: These render
     as ([SUMMARY:CHARACTER-PROPOSAL:2-6-5][clhs]):

      - `[SUMMARY:CHARACTER-PROPOSAL:2-6-5][clhs]`

      - `[SUMMARY:CHARACTER-PROPOSAL:2-6-5][(clhs section)]`

      Since these summary ids are not particularly reader friendly,
      the anchor text a @SPECIFIC-REFLINK-WITH-TEXT may be used:

      - `[see this][SUMMARY:CHARACTER-PROPOSAL:2-6-5 (clhs section)]`
        ([see this][SUMMARY:CHARACTER-PROPOSAL:2-6-5 (clhs section)]).

  - *sections*:

      - *by section number*: `[3.4][clhs]` or `[3.4][(clhs
        section)]` ([3.4][clhs])

      - *by section title*: With the locative `(CLHS SECTION)`,
        substring matching against the title starting at word
        boundaries is performed. With the locative CLHS (where
        `\\SECTION` is not specified explicitly), the name must match
        the title exactly. For example, `[lambda list][(clhs
        section)]` refers to the same definition as `[lambda
        lists][clhs]` ([Lambda Lists][clhs]).

      - *by filename*: `[03_d][clhs]` or `[03_d][(clhs
        section)]` ([03_d][clhs])

      - *by alias*

          - [Format directives][(clhs glossary-term)] are aliases of
            the sections describing them. Thus, `[~c][clhs]` is
            equivalent to `[22.3.1.1][clhs]` and `[Tilde C:
            Character][clhs]`. The full list is
            [*format-directive-alias-links* variable][docstring].

          - Similarly, [reader macro][clhs] characters are aliases of
            the sections describing them. The full list is
            [*reader-macro-alias-links* variable][docstring].

          - Finally, [loop keywords][(clhs glossary-term)] have
            aliases to the sections describing them. For example, the
            strings `loop:for`, `for` and `:for` are aliases of CLHS
            `6.1.2.1`. The `loop:*` aliases are convenient for
            completion at the prompt when
            @BROWSING-LIVE-DOCUMENTATION, while the other aliases are
            for defaulting to buffer contents.

  As the above examples show, the NESTED-LOCATIVE argument of the CLHS
  locative may be omitted. In that case, definitions, glossary terms,
  issues, issue summaries, and sections are considered in that order.
  Sections are considered last because a substring of a section title
  can be matched by chance easily.

  All examples so far used @REFLINKs. @AUTOLINKing also works if the
  @NAME is marked up as code or is [codified][ @codification] (e.g. in
  `COS clhs` (COS clhs).

  As mentioned above, `\\M-.` does not do anything over CLHS
  references. Slightly more usefully, the [live documentation
  browser][@browsing-live-documentation] understands CLHS links so one
  can enter inputs like `3.4 clhs`, `"lambda list" clhs` or
  `error (clhs function)`.

  CLHS references do not RESOLVE."""
  ;; For SUBSTITUTE-CLHS-FOR-MISSING-STANDARD-DEFINITION
  (defclass clhs-dref ()
    ((explicit-p :initform nil :accessor clhs-dref-explicit-p))))

(defvar *clhs-force-exact-match* nil)

(define-lookup clhs (name locative-args)
  (or (let ((name-string (if (stringp name)
                             name
                             (princ-to-string name))))
        (flet ((glossary-term? ()
                 (let ((id (find-hyperspec-glossary-entry-id name-string)))
                   (when id
                     (make-instance 'clhs-dref
                                    :name id
                                    :locative '(clhs glossary-term)))))
               (issue-or-section? (&optional (match-title :word-prefix))
                 (when *clhs-force-exact-match*
                   (setq match-title :exact))
                 (or (let ((id (find-hyperspec-issue-id name-string)))
                       (when id
                         (make-instance 'clhs-dref
                                        :name id
                                        :locative '(clhs section))))
                     (let ((id (find-hyperspec-section-id
                                name-string :match-title match-title)))
                       ;; ex clhs
                       (when id
                         (make-instance 'clhs-dref
                                        :name id
                                        :locative '(clhs section))))))
               (definition? ()
                 (multiple-value-bind (url nested-locative)
                     (find-hyperspec-definition-url name locative-args)
                   (when url
                     (if (eq 'go (locative-type nested-locative))
                         (dref name nested-locative)
                         (make-instance 'clhs-dref
                                        :name name
                                        :locative (if nested-locative
                                                      `(clhs ,nested-locative)
                                                      'clhs)))))))
          (cond ((equal locative-args '(glossary-term))
                 (glossary-term?))
                ((equal locative-args '(section))
                 (issue-or-section?))
                ((endp locative-args)
                 (or (definition?)
                     (glossary-term?)
                     (issue-or-section? :exact)))
                (t
                 (definition?)))))
      (locate-error)))

(defmethod map-definitions-of-name (fn name (locative-type (eql 'clhs)))
  (cond ((symbolp name)
         (loop for locative in (hyperspec-locatives-for-name name)
               ;; LOCATIVE NIL is for *HYPERSPEC-DISAMBIGUATIONS*.
               do (when locative
                    (funcall fn (dref name `(clhs ,locative))))))
        (t
         (when-let (dref (dref name '(clhs glossary-term) nil))
           (funcall fn dref))
         ;; (DREF "EX" 'CLHS) is a LOCATE-ERROR, but here we are
         ;; trying (DREF "EX" '(CLHS SECTION)), which matches a
         ;; :WORD-PREFIX in the title "Language Extensions". This is
         ;; fine when the user explicitly specifies (CLHS SECTION),
         ;; but here SECTION is just one of the things we try, so
         ;; reduce noise by requiring an exact match.
         (let ((*clhs-force-exact-match* t))
           (when-let (dref (dref name '(clhs section) nil))
             (funcall fn dref))))))

(defmethod map-definitions-of-type (fn (locative-type (eql 'clhs)))
  (flet ((foo (name locative)
           (funcall fn (dref name locative))))
    (loop for entry in *hyperspec-sections*
          do (dolist (x entry)
               (cond ((stringp x)
                      (foo x '(clhs section)))
                     ((listp x)
                      (dolist (alias x)
                        (foo alias '(clhs section)))))))
    (dolist (term *hyperspec-glossary-entries*)
      (foo term '(clhs glossary-term)))
    (loop for entry in *hyperspec-issue-summaries*
          do (foo (first entry) '(clhs section))
             (foo (second entry) '(clhs section)))
    (loop for entry in *hyperspec-issues*
          do (foo (first entry) '(clhs section))
             (foo (second entry) '(clhs section)))
    'dref::try-interned-symbols))

(defun clhs-dref (name locative)
  ;; Pick off the impossible cases quickly.
  (when  (member (locative-type locative) *hyperspec-definition-locative-types*)
    (dref name `(clhs ,locative) nil)))
