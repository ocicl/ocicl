(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @navigating-in-emacs (:title "Navigating Sources in Emacs")
  """Integration into @SLIME's @M-. (`slime-edit-definition`) allows
  one to visit the SOURCE-LOCATION of a DREF::@DEFINITION. PAX extends
  standard Slime functionality by

  - adding support for all kinds of definitions (see e.g.
    [ASDF:SYSTEM][locative], [READTABLE][locative] in
    DREF::@BASIC-LOCATIVE-TYPES), not just the ones Slime knows about,

  - providing a portable way to refer to even standard definitions,

  - disambiguating the definition based on buffer content, and

  - adding more powerful completions.

  The definition is either determined from the buffer content at point
  or is prompted for. At the prompt, TAB-completion is available for
  both names and locatives. With a prefix argument (`C-u M-.`), the
  buffer contents are not consulted, and `\\M-.` always prompts.

  The `\\M-.` extensions can be enabled by loading `src/mgl-pax.el`. See
  @EMACS-SETUP. In addition, the Elisp command
  `mgl-pax-edit-parent-section` visits the source location of the
  section containing the definition with point in it.

  A close relative of `\\M-.` is `\\C-.` for @BROWSING-LIVE-DOCUMENTATION."""
  (@m-.-defaulting section)
  (@m-.-prompting section))


(defsection @m-.-defaulting (:title "`\\\\M-.` Defaulting")
  """When `@M-.` is invoked, it first tries to find a @NAME in the
  current buffer at point. If no name is found, then it
  [prompts][@M-.-PROMPTING].

  First, `(slime-sexp-at-point)` is taken as a @WORD, from which the
  @NAME will be [parsed][@parsing]. Then, candidate locatives are
  looked for before and after the @WORD. Thus, if a locative is the
  previous or the next expression, then `\\M-.` will go straight to the
  definition which corresponds to the locative. If that fails, `\\M-.`
  will try to find the definitions in the normal way, which may
  involve popping up an xref buffer and letting the user interactively
  select one of possible definitions.

  `\\M-.` works on parenthesized references, such as those in
  DEFSECTION:

  ```
  (defsection @foo ()
    (cos function))
  ```

  Here, when the cursor is on one of the characters of `\COS` or just
  after `\COS`, pressing `\\M-.` will visit the definition of the
  function `COS`.

  To play nice with @GENERATING-DOCUMENTATION, forms suitable for
  @AUTOLINKing are recognized:

      function cos
      cos function

  ... as well as @REFLINKs:

      [cos][function]
      [see this][cos function]

  ... and @MARKDOWN/INLINE-CODE:

      cos `function`
      `cos` function
      `cos` `function`

  Everything works the same way in comments and docstrings as in code.
  In the next example, pressing `\\M-.` on `RESOLVE*` will visit its
  denoted method:

  ```
  ;;; See RESOLVE* (method (function-dref)) for how this all works.
  ```""")

;;; An acronym for Word-And-Locatives-List. This is what
;;; `mgl-pax-wall-at-point' returns. It may look like this:
;;;
;;;     (("[section][" ("junk-before" "class"))
;;;      ("section" ("class")))
(deftype wall () 'list)

(defun definitions-of-wall (wall &key (definitions #'definitions))
  (delete-duplicates
   (or
    ;; First, try with the given locatives.
    (loop for (word locative-strings) in wall
          append (loop for locative-string in locative-strings
                       append (ensure-list (wal-dref word locative-string))))
    ;; Then, fall back on the no-locative case.
    (loop for entry in wall
          append (find-name definitions (first entry) :trim t :depluralize t)))
   :test #'xref=))

(defun wal-dref (word locative-string)
  (find-name (make-def-lookup-fn locative-string) word :trim t :depluralize t))

;;; This is essentially (DREF NAME LOCATIVE) but also handles
;;; unreadable locatives.
(defun make-def-lookup-fn (locative-string)
  (let ((locative (parse-locative-around locative-string
                                         :junk-allowed t
                                         :on-unreadable :truncate)))
    (flet ((set-unreadable ()
             (when (find-if-in-tree (lambda (obj)
                                      (eq obj 'unreadable))
                                    locative)
               (setq locative 'unreadable))))
      (set-unreadable)
      (lambda (name)
        (when (eq locative 'unreadable)
          (setq locative (parse-locative-around locative-string
                                                :junk-allowed t
                                                :name name)))
        (set-unreadable)
        (when (and locative (not (eq locative 'unreadable)))
          (dref name locative nil))))))

;;; Ensure that some Swank internal facilities (such as
;;; SWANK::FIND-DEFINITIONS-FIND-SYMBOL-OR-PACKAGE,
;;; SWANK::WITH-BUFFER-SYNTAX, SWANK::PARSE-SYMBOL) are operational
;;; even when not running under Slime.
(defmacro with-swank (() &body body)
  `(let* ((swank::*buffer-package* (if (boundp 'swank::*buffer-package*)
                                       swank::*buffer-package*
                                       *package*))
          (swank::*buffer-readtable*
            (if (boundp 'swank::*buffer-readtable*)
                swank::*buffer-readtable*
                (swank::guess-buffer-readtable swank::*buffer-package*))))
     ,@body))

;;; List Swank source locations (suitable for `make-slime-xref') for
;;; the things that the Elisp side considers possible around the point
;;; when M-. is invoked. The return value is a list of (DSPEC
;;; LOCATION) elements (with DSPEC as a string).
;;;
;;; If none of the resulting references can be resolved (including if
;;; no locatives are specified), then list all possible DEFINITIONS.
;;;
;;; Each element in the list WALL consists of a @WORD and a list of
;;; possible DREF::@LOCATIVEs found next to it in the Emacs buffer.
(defun locate-definitions-for-emacs (wall)
  (with-swank ()
    (swank/backend:converting-errors-to-error-location
      (swank::with-buffer-syntax ()
        (locate-definitions-for-emacs-1 wall)))))

(defun locate-definitions-for-emacs-1 (wall)
  (loop for definition in (definitions-of-wall wall)
        for location = (source-location definition :error :error)
        collect `(,(prin1-to-string (dref::definition-to-dspec definition))
                  ,location)))


(defsection @m-.-prompting (:title "`\\\\M-.` Prompting")
  (@m-.-minibuffer-syntax section)
  (@m-.-completion section))

;;; This documents the Elisp function `mgl-pax-edit-definitions'.
;;; There isn't much code exclusively for this on the CL side.
(defsection @m-.-minibuffer-syntax (:title "`\\\\M-.` Minibuffer Syntax")
  """At the minibuffer prompt, the DREF::@DEFINITIONS to edit
  can be specified as follows.

  - NAME: Refers to all DREF:DEFINITIONS of NAME with a [Lisp locative
    type][DREF:LISP-LOCATIVE-TYPES]. See these `NAME -> DEFINITIONS`
    examples:

          print    ->  PRINT FUNCTION
          PRINT    ->  PRINT FUNCTION
          MGL-PAX  ->  "mgl-pax" ASDF:SYSTEM, "MGL-PAX" package
          pax      ->  "PAX" PACKAGE
          "PAX"    ->  "PAX" PACKAGE

      Note that depending on the Lisp implementation there may be more
      definitions. For example, SBCL has an [UNKNOWN][locative]
      :DEFOPTIMIZER definition for `\PRINT`.

  - NAME LOCATIVE: Refers to a single definition (as in `(DREF:DREF
    NAME LOCATIVE)`). Example inputs of this form:

          print function
          dref-ext:docstring* (method (t))

  - LOCATIVE NAME: This has the same form as the previous: two sexps,
    but here the first one is the locative. If ambiguous, this is
    considered in addition to the previous one. Example inputs:

          function print
          (method (t)) dref-ext:docstring*

  In all of the above NAME is a PAX::@RAW-NAME, meaning that `\print`
  will be recognized as `\PRINT` and `\pax` as `"PAX"`.

  The package in which symbols are read is the Elisp
  `slime-current-package`. In Lisp buffers, this is the buffer's
  package, else it's the package of the Slime repl buffer.""")


;;;; Completion of names and locatives for the Elisp function
;;;; `mgl-pax-name-completions-at-point'

(defsection @m-.-completion (:title "`\\\\M-.` Completion")
  "When `\\\\M-.` prompts for the definition to edit, TAB-completion is
  available in the minibuffer for both names and locatives. To reduce
  clutter, string names are completed only if they are typed
  explicitly with an opening quotation mark, and they are
  case-sensitive. Examples:

  - `pri<TAB>` invokes the usual Slime completion.

  - `print <TAB>` (note the space) lists FUNCTION and (PAX:CLHS
    FUNCTION) as locatives.

  - `class dref:<TAB>` lists DREF:XREF and DREF:DREF (all the classes
    in the package DREF).

  - `pax:locative <TAB>` lists all DREF::@LOCATIVE-TYPEs (see the CL
    function DREF:LOCATIVE-TYPES).

  - `package \"MGL<TAB>` lists the names of packages that start with
    `\"MGL\"`.

  - `package <TAB>` lists the names of all packages as strings and
     also CLASS, MGL-PAX:LOCATIVE because PACKAGE denotes a class and
     also a locative.

  For more powerful search, see @APROPOS.")

;;; Called when completing a name and no locative has been typed yet.
;;; E.g.
;;;
;;;    Edit Definitions: "mgl-
;;;
;;; This only completes explicit string prefixes (i.e. starting with a
;;; #\") to avoid cluttering the match list. Completing other prefixes
;;; is left to Slime. LOCATIVE-TYPES is (LOCATIVE-TYPES) for
;;; documenting.
(defun string-name-completions-for-emacs (prefix &key (locative-types
                                                       (lisp-locative-types)))
  (with-swank ()
    (swank::with-buffer-syntax ()
      (mapcar #'prin1-to-string
              (string-name-completions-for-emacs-1
               prefix locative-types)))))

(defun string-name-completions-for-emacs-1 (prefix locative-types)
  (let ((names ()))
    (flet ((maybe-add (name)
             (when (and (stringp name)
                        (starts-with-subseq prefix name))
               (push name names))))
      (dolist (locative-type locative-types)
        (dref::map-names-for-type #'maybe-add locative-type)))
    names))

;;; Called when completing the second sexp at the prompt.
(defun names-or-locatives-for-emacs (sexp-1 prefix
                                     &key (definitions #'definitions))
  (with-swank ()
    (swank::with-buffer-syntax ()
      `(:names ,(mapcar (rcurry #'prin1-to-string/case
                                (guess-print-case prefix))
                        (names-or-locatives-for-emacs-1 sexp-1 prefix
                                                        definitions))))))

(defun guess-print-case (string)
  (if (some #'upper-case-p string)
      :upcase
      :downcase))

(defun names-or-locatives-for-emacs-1 (sexp-1 prefix definitions)
  (let ((locative (parse-locative sexp-1)))
    (if (starts-with #\" prefix)
        (when locative
          (list-string-names-for-locative (subseq prefix 1) locative))
        (append (and locative (list-names-for-locative prefix locative))
                (list-locatives-for-name sexp-1 definitions)))))

(defun list-locatives-for-name (name definitions)
  (flet ((match (name)
           (loop for dref in (funcall definitions name)
                 collect (dref-locative dref))))
    (find-name #'match name)))

(defun list-symbols-for-locative (prefix locative)
  (let ((symbols ())
        (print-fn (rcurry #'prin1-to-string/case (guess-print-case prefix))))
    (flet ((consider (symbol)
             (when (and (or (null prefix)
                            (starts-with-subseq prefix
                                                (funcall print-fn symbol)))
                        (dref symbol locative nil))
               (push symbol symbols))))
      (with-package-iterator (next (remove #.(find-package :keyword)
                                           (list-all-packages))
                                   :external :internal)
        (loop (multiple-value-bind (morep symbol) (next)
                (if morep
                    (consider symbol)
                    (return))))))
    symbols))

(defun list-string-names-for-locative (prefix locative)
  (let ((names ())
        (locative-type (locative-type locative))
        (locative-args (locative-args locative)))
    (flet ((add (name)
             (when (and (stringp name)
                        (or (null prefix)
                            (starts-with-subseq prefix name))
                        (or (null locative-args)
                            (dref name locative nil)))
               (push name names))))
      (dref::map-names-for-type #'add locative-type)
      names)))

(defun list-names-for-locative (prefix locative)
  (let ((names ())
        (locative-type (locative-type locative))
        (locative-args (locative-args locative)))
    (flet ((add (name)
             (when (and (stringp name)
                        (or (null prefix)
                            (starts-with-subseq prefix name))
                        (or (null locative-args)
                            (dref name locative nil)))
               (push name names))))
      (if (eq (dref::map-names-for-type #'add locative-type)
              'dref::try-interned-symbols)
          (append (list-symbols-for-locative prefix locative) names)
          names))))

;;; Return locative types and other symbols (e.g. OR, AND, NOT) from
;;; which @DTYPES may be built. This is not exhaustive because (METHOD
;;; () (NUMBER)) would also need NUMBER.
(defun dtype-symbols-for-emacs (prefix)
  (with-swank ()
    (swank::with-buffer-syntax ()
      (swank/backend:converting-errors-to-error-location
        `(:ok ,(mapcar (rcurry #'prin1-to-string/case
                               (guess-print-case prefix))
                       (list* 'or 'and 'not 'member 'satisfies
                              (locative-types-maybe-with-definitions))))))))

(defun locative-types-maybe-with-definitions ()
  (dref::sort-locative-types
   (loop for locative-type in (locative-types)
         unless (or (not (external-symbol-p locative-type))
                    (not (locative-type-may-have-definitions-p locative-type)))
           collect locative-type)))

(defun locative-type-may-have-definitions-p (locative-type)
  (eq 'dref::try-interned-symbols
      (dref::map-names-for-type
       (lambda (name)
         (declare (ignore name))
         (return-from locative-type-may-have-definitions-p t))
       locative-type)))


;;;; The Common Lisp side of `mgl-pax-find-parent-section'

(defun find-parent-section-for-emacs (buffer filename possibilities)
  (with-swank ()
    (swank/backend:converting-errors-to-error-location
      (swank::with-buffer-syntax ()
        (let ((dref (find-current-definition buffer filename possibilities)))
          (if (null dref)
              '(:error "Cannot determine current definition.")
              (let ((sections (find-parent-sections dref)))
                (if sections
                    (loop for section in sections
                          collect (dspec-and-source-location-for-emacs
                                   (locate section)))
                    `(:error ,(format nil "Cannot find parent section of ~S ~S."
                                      (dref-name dref)
                                      (dref-locative dref)))))))))))

(defun dspec-and-source-location-for-emacs (dref)
  (let ((location (source-location dref)))
    `(,(prin1-to-string (dref::definition-to-dspec dref))
      ,(if (null location)
           '(:error "No source location.")
           location))))

;;; This is also used by CURRENT-DEFINITION-PAX-URL-FOR-EMACS.
(defun find-current-definition (buffer filename possibilities)
  (loop for (name-string snippet pos) in possibilities
          thereis (multiple-value-bind (name foundp)
                      (parse-sexp name-string :errorp nil)
                    (when foundp
                      (or (guess-current-definition name buffer filename
                                                    snippet pos)
                          (when (and (listp name)
                                     (symbolp (first name)))
                            (guess-current-definition
                             (first name) buffer filename snippet pos)))))))

;;; Return the definition of NAME in BUFFER (a string) and FILE (a
;;; string or NIL) whose source location information from
;;; SOURCE-LOCATION matches SNIPPET or is otherwise closest to buffer
;;; positions POS (1-based indexing).
(defun guess-current-definition (name buffer file snippet pos)
  (let ((closest-definition nil)
        ;; Limit the chance of finding an unrelated definition just
        ;; because its @NAME is used as the first arg of some form by
        ;; not accepting position-based matches farther than 2000
        ;; characters from POS.
        (closest-pos 2000)
        ;; To be more resistant to edits since the last definition,
        ;; only match the beginning.
        (snippet (first-lines snippet 2)))
    (flet ((snippets-match (loc-snippet)
             (or
              ;; E.g. ASDF:SYSTEMs on SBCL
              (equal loc-snippet "")
              (let ((mismatch-pos (mismatch loc-snippet snippet)))
                (or (null mismatch-pos)
                    (= (length snippet) mismatch-pos))))))
      (dolist (dref
               ;; Only LISP-LOCATIVE-TYPES have source location.
               (definitions name))
        (let ((location (source-location dref)))
          (if (source-location-p location)
              (let ((loc-file (source-location-file location))
                    (loc-buffer (source-location-buffer location))
                    (loc-pos (source-location-buffer-position location))
                    (loc-snippet (source-location-snippet location)))
                (when (and
                       ;; The files must always match (even if NIL).
                       (equal file loc-file)
                       ;; The buffers must match, but LOC-BUFFER may be
                       ;; NIL (e.g. if the file wasn't compiled via
                       ;; Slime).
                       (or (null loc-buffer) (equal buffer loc-buffer))
                       ;; A match in LOCATION-SNIPPET is most
                       ;; trustworthy, but it's not always available.
                       (if loc-snippet
                           (snippets-match loc-snippet)
                           (<= (abs (- loc-pos pos))
                               (abs (- closest-pos pos)))))
                  ;; Multiple definitions may have the exact source
                  ;; location (e.g. DEFGENERIC with :METHODs in it on
                  ;; SBCL). They may then all match the snippet.
                  (unless (and (typep closest-definition 'generic-function-dref)
                               (typep dref 'method-dref)
                               (eql closest-pos pos))
                    (setq closest-definition dref
                          closest-pos loc-pos))))
              ;; No source location
              (when (and (null closest-definition)
                         (dref-and-snippet-match-p dref snippet))
                (setq closest-definition dref
                      closest-pos most-positive-fixnum)))))
      closest-definition)))

;;; This could use the macroexpanded form instead of the snippet and a
;;; generic function specialized on the locative type, but since it's
;;; a fallback mechanism for the no-source-location case, that may be
;;; an overkill.
(defun dref-and-snippet-match-p (dref snippet)
  (and (search (make-name-pattern (dref-name dref)) snippet
               :test #'char-equal)
       (let ((patterns (case (xref-locative-type dref)
                         ((variable) '("defvar" "defparameter"))
                         ((constant) '("defconstant" "define-constant"))
                         ((macro) '("defmacro"))
                         ((symbol-macro '("define-symbol-macro")))
                         ((compiler-macro '("define-compiler-macro")))
                         ((function) '("defun"))
                         ((generic-function) '("defgeneric"))
                         ;; Note that DEFMETHOD is intentially omitted
                         ;; because without matching the qualifiers
                         ;; and specializers, it's easy to guess
                         ;; wrong.
                         ((method-combination '("define-method-combination")))
                         ;; Can't find :READER, :WRITER, :ACCESSOR in DEFCLASS.
                         ;; Can't find STRUCTURE-ACCESSOR.
                         ((type) '("deftype"))
                         ((class) '("defclass"))
                         ((condition) '("define-condition"))
                         ((declaration) '("define-declaration"))
                         ((restart) '("define-restart"))
                         ((asdf:system) '("defsystem"))
                         ((package) '("defpackage" "define-package"))
                         ((readtable) '("defreadtable"))
                         ((section) '("defsection"))
                         ((glossary-term) '("define-glossary-term"))
                         ((note) '("note"))
                         ((locative) '("define-locative-type")))))
         (loop for pattern in patterns
                 thereis (search pattern snippet :test #'char-equal)))))

(defun make-name-pattern (obj)
  (if (symbolp obj)
      (let ((*package* (or (symbol-package obj)
                           (find-package :keyword))))
        (prin1-to-string obj))
      (princ-to-string obj)))
