(cl:in-package :dref)

(in-readtable pythonic-string-syntax)

(defun/autoloaded definitions (name &key (dtype t))
  """List all definitions of NAME that are of DTYPE as [DREFs][class].

  Just as `(DREF NAME LOCATIVE)` returns the canonical definition, the
  DREF-NAMEs of returned by DEFINITIONS may be different from NAME:

  ```cl-transcript
  (definitions "PAX")
  ==> (#<DREF "MGL-PAX" PACKAGE>)
  ```

  ```cl-transcript
  (definitions 'mgl-pax)
  ==> (#<DREF "mgl-pax" ASDF/SYSTEM:SYSTEM> #<DREF "MGL-PAX" PACKAGE>)
  ```

  Similarly, DREF-LOCATIVE-TYPE may be more made more specific:

  ```cl-transcript
  (definitions 'dref:locate-error :dtype 'type)
  ==> (#<DREF LOCATE-ERROR CONDITION>)
  ```

  Can be extended via MAP-DEFINITIONS-OF-NAME."""
  (let ((drefs (definitions-with-locative-types name (cover-dtype dtype))))
    (filter-covered-drefs (delete-duplicates drefs :test #'xref=) dtype)))

(defun definitions-with-locative-types (name locative-types)
  (let ((drefs ())
        (swank-name-to-locative-types (make-hash-table :test #'equal)))
    (dolist (locative-type locative-types)
      (multiple-value-bind (mapper swank-name)
          (map-definitions-of-name (lambda (dref)
                                     (push dref drefs))
                                   name locative-type)
        (unless swank-name
          (setq swank-name name))
        (when (eq mapper 'swank-definitions)
          (push locative-type
                (gethash swank-name swank-name-to-locative-types)))))
    (append drefs
            (loop for swank-name being the hash-key
                    in swank-name-to-locative-types
                      using (hash-value swank-locative-types)
                  append (swank-definitions swank-name
                                            swank-locative-types)))))

(defun/autoloaded dref-apropos (name &key package external-only case-sensitive
                                     (dtype t))
  """Return a list of [DREF][class]s corresponding to existing
  definitions that match the various arguments. First, `(DREF-APROPOS
  NIL)` lists all definitions in the running Lisp and maybe more (e.g.
  [MGL-PAX:CLHS][locative]). Arguments specify how the list of
  definitions is filtered.

  DREF-APROPOS itself is similar to CL:APROPOS-LIST, but

  - it finds @DEFINITIONs not SYMBOLs,
  - it supports an extensible definition types, and
  - filtering based on them.

  PAX has a live browsing [frontend][PAX::@APROPOS].

  Roughly speaking, when NAME or PACKAGE is a SYMBOL, they must match
  the whole @NAME of the definition:

  ```cl-transcript
  (dref-apropos 'method :package :dref :external-only t)
  ==> (#<DREF METHOD CLASS> #<DREF METHOD LOCATIVE>)
  ```

  On the other hand, when NAME or PACKAGE is a STRING, they are
  matched as substrings to the definition's name PRINC-TO-STRINGed:

  ```cl-transcript
  (dref-apropos "method" :package :dref :external-only t)
  ==> (#<DREF SETF-METHOD LOCATIVE> #<DREF METHOD CLASS>
  -->  #<DREF METHOD LOCATIVE> #<DREF METHOD-COMBINATION CLASS>
  -->  #<DREF METHOD-COMBINATION LOCATIVE>)
  ```

  Definitions that are not of DTYPE (see DTYPEP) are filtered out:

  ```cl-transcript
  (dref-apropos "method" :package :dref :external-only t :dtype 'class)
  ==> (#<DREF METHOD CLASS> #<DREF METHOD-COMBINATION CLASS>)
  ```

  When PACKAGE is :NONE, only non-symbol @NAMES are matched:

  ```
  (dref-apropos "dref" :package :none)
  ==> (#<DREF "DREF" PACKAGE> #<DREF "DREF-EXT" PACKAGE>
  -->  #<DREF "DREF-TEST" PACKAGE> #<DREF "dref" ASDF/SYSTEM:SYSTEM>
  -->  #<DREF "dref/full" ASDF/SYSTEM:SYSTEM>
  -->  #<DREF "dref/test" ASDF/SYSTEM:SYSTEM>
  -->  #<DREF "dref/test-autoload" ASDF/SYSTEM:SYSTEM>)
  ```

  The exact rules of filtering are as follows. Let `C` be the @NAME of
  the candidate definition from the list of all definitions that we
  are matching against the arguments and denote its string
  representation `(PRINC-TO-STRING C)` with `P`. Note that
  PRINC-TO-STRING does not print the package of symbols. We say that
  two strings _match_ if CASE-SENSITIVE is NIL and they are EQUALP, or
  CASE-SENSITIVE is true and they are EQUAL. CASE-SENSITIVE affects
  _substring_ comparisons too.

  - If NAME is a SYMBOL, then its SYMBOL-NAME must _match_ `P`.

  - If NAME is a STRING, then it must be a _substring_ of `P`.

  - If PACKAGE is :ANY, then `C` must be a SYMBOL.

  - If PACKAGE is :NONE, then `C` must _not_ be a SYMBOL.

  - If PACKAGE is not NIL, :ANY or :NONE, then `C` must be a symbol.

  - If PACKAGE is a [PACKAGE][class], it must be EQ to the
    SYMBOL-PACKAGE of `C`.

  - If PACKAGE is a SYMBOL other than NIL, :ANY and :NONE, then its
    SYMBOL-NAME must _match_ the PACKAGE-NAME or one of the
    PACKAGE-NICKNAMES of SYMBOL-PACKAGE of `C`.

  - If PACKAGE is a STRING, then it must be a _substring_ of the
    PACKAGE-NAME of SYMBOL-PACKAGE of `C`.

  - If EXTERNAL-ONLY and `C` is a symbol, then `C` must be external in
    a matching package.

  - DTYPE matches candidate definition `D` if `(DTYPEP D DTYPE)`.

  Can be extended via MAP-REFERENCES-OF-TYPE and
  MAP-DEFINITIONS-OF-NAME."""
  (let ((locative-types (cover-dtype dtype))
        (char-test (if case-sensitive #'char= #'char-equal))
        (string-test (if case-sensitive #'string= #'string-equal))
        (to-try ())
        (drefs ()))
    (labels ((matching-name-p (name-1)
               (and (or (null name)
                        (and (symbolp name)
                             (funcall string-test (symbol-name name)
                                      (princ-to-string name-1)))
                        (and (stringp name)
                             (search name (princ-to-string name-1)
                                     :test char-test)))
                    (cond ((eq package :none)
                           (not (symbolp name-1)))
                          ((eq package :any)
                           (symbolp name-1))
                          (t
                           (not (and (stringp name-1) package))))))
             (matching-package-p (package-1)
               (and (not (eq package-1 #.(find-package '#:keyword)))
                    (or (null package)
                        (and (packagep package)
                             (eq package-1 package))
                        (and (symbolp package)
                             (not (eq package :none))
                             (or (eq package :any)
                                 (find (symbol-name package)
                                       (cons (package-name package-1)
                                             (package-nicknames package-1))
                                       :test string-test)))
                        (and (stringp package)
                             (find-if (lambda (package-name-1)
                                        (search package package-name-1
                                                :test char-test))
                                      (cons (package-name package-1)
                                            (package-nicknames package-1)))))))
             (matching-reference-p (xref)
               (let ((name (xref-name xref))
                     (locative-type (xref-locative-type xref)))
                 (and (matching-name-p name)
                      (member locative-type locative-types))))
             (consider-dref (dref)
               (let ((xref (dref-origin dref)))
                 (when (matching-reference-p xref)
                   (push dref drefs)))))
      ;; Populate MATCHING-NAMES with @NAMEs that combine with some
      ;; locative whose type is in LOCATIVE-TYPES.
      (dolist (locative-type locative-types)
        (let ((mapper (map-definitions-of-type #'consider-dref locative-type)))
          (when (and mapper (symbolp mapper))
            (assert (eq mapper 'try-interned-symbols))
            (push locative-type to-try))))
      ;; For many locative types, we need to consider all symbols as
      ;; @NAMEs. Iterating over many symbols takes time, so iterate
      ;; once for all locative types TO-TRY.
      (when (and (not (eq package :none)) to-try)
        (loop for symbol being the hash-key
                in (matching-symbols #'matching-package-p #'matching-name-p
                                     external-only)
              do (dolist (dref (definitions-with-locative-types symbol to-try))
                   (push dref drefs))))
      (sort-references
       (filter-covered-drefs (remove-duplicate-drefs/nonstable drefs)
                             dtype)))))

(defun matching-symbols (package-pred symbol-pred external-only)
  (let ((h (make-hash-table)))
    (dolist (package-1 (remove (find-package :keyword) (list-all-packages)))
      (when (funcall package-pred package-1)
        (if external-only
            (with-package-iterator (next package-1 :external)
              (loop (multiple-value-bind (morep symbol) (next)
                      (if morep
                          (when (funcall symbol-pred symbol)
                            (setf (gethash symbol h) t))
                          (return)))))
            (with-package-iterator (next package-1 :external :internal)
              (loop (multiple-value-bind (morep symbol) (next)
                      (if morep
                          (when (funcall symbol-pred symbol)
                            (setf (gethash symbol h) t))
                          (return))))))))
    h))

;;; Order REFERENCES in an implementation independent way.
;;; PAX:PAX-APROPOS depends on non-symbol names coming first.
(defun sort-references (references &key key)
  (flet ((sort-key (reference)
           (let* ((reference (if key (funcall key reference) reference))
                  (locative-type (xref-locative-type reference)))
             (with-standard-io-syntax*
               ;; Avoid mentions of BASE-CHAR and such.
               (let ((*print-readably* nil))
                 (if (not (symbolp (xref-name reference)))
                     ;; Non-symbol named references go first ("1.")
                     ;; and are sorted by locative type, name,
                     ;; locative args in that order.
                     (format nil "1. ~A ~S ~S"
                             (locative-type-to-sort-key locative-type)
                             (xref-name reference)
                             (xref-locative-args reference))
                     ;; Symbol-based reference go after ("2.") and are
                     ;; sorted by name first.
                     (format nil "2. ~S ~A ~S"
                             (xref-name reference)
                             (locative-type-to-sort-key locative-type)
                             (xref-locative-args reference))))))))
    (sort-list-with-precomputed-key references #'string< :key #'sort-key)))

(defun locative-type-to-sort-key (locative-type)
  (format nil "~S ~S"
          ;; Sort by SYMBOL-NAME before SYMBOL-PACKAGE of LOCATIVE-TYPE.
          (if (eq locative-type 'unknown)
              ;; The unknown references go last for the same @NAME.
              "~~~~"
              (symbol-name locative-type))
          (package-name (symbol-package locative-type))))

(defun sort-locative-types (locative-types)
  (sort locative-types #'string< :key #'locative-type-to-sort-key))

;;; Like REMOVE-DUPLICATES but does not maintain a stable order and
;;; faster because it first groups DREFs by name. The length of DREFs
;;; can easily be in the tens of thousands.
(defun remove-duplicate-drefs/nonstable (drefs)
  (let ((name-to-drefs (make-hash-table :test #'equal)))
    (dolist (dref drefs)
      (push dref (gethash (dref-name dref) name-to-drefs)))
    (loop for name being the hash-key of name-to-drefs
          append (delete-duplicates (gethash name name-to-drefs)
                                    :test #'xref=))))
