(cl:in-package :dref)

(in-readtable pythonic-string-syntax)

(defmethod arglist-parameters* ((arglist null) (arglist-type null))
  nil)

(defmethod arglist-parameters* (arglist (arglist-type (eql :ordinary)))
  (multiple-value-bind (requireds optionals rest keywords other-keys-p auxs)
      (alexandria:parse-ordinary-lambda-list arglist)
    (declare (ignore other-keys-p))
    (let ((names requireds))
      (dolist (optional optionals)
        (push (first optional) names)
        ;; SUPPLIEDP
        (when (third optional)
          (push (third optional) names)))
      (when rest
        (push rest names))
      (dolist (keyword keywords)
        (push (second (first keyword)) names)
        (when (third keyword)
          (push (third keyword) names)))
      (dolist (aux auxs)
        (push (first aux) names))
      (reverse names))))

(defmethod arglist-parameters* (arglist (arglist-type (eql :macro)))
  (macro-arglist-parameters arglist))

(defmethod arglist-parameters* (arglist (arglist-type (eql :specialized)))
  (let ((seen-special-p nil))
    (loop for arg in arglist
          for i upfrom 0
          do (when (member arg '(&key &optional &rest &aux &allow-other-keys))
               (setq seen-special-p t))
          collect (if (and (not seen-special-p)
                           (listp arg)
                           (= (length arg) 2))
                      (first arg)
                      arg))))

(defmethod arglist-parameters* (arglist (arglist-type (eql :deftype)))
  (arglist-parameters* arglist :macro))

(defmethod arglist-parameters* (arglist (arglist-type (eql :destructuring)))
  (arglist-parameters* arglist :macro))


(defvar *definitions-cache*)
(defvar *cachable-locative-types*)
(defvar *non-cachable-locative-types*)

(defmacro with-definitions-cached ((&key (dtype ''top)) &body body)
  "Allow caching of definition lookups and [DTYPE][@dtypes] computations.
  This can provide a speedup when multiple calls are made to
  DEFINITIONS or DREF-APROPOS within BODY.

  Using this macro makes the promise that, during the [dynamic
  extent][clhs] of BODY, the following are unchanged:

  - the set of @DEFINITIONs of DTYPE,
  - the set of all @LOCATIVE-TYPEs,
  - the set of all @DTYPES.

  Note that definitions _not_ of DTYPE are allowed to be made or
  deleted."
  (once-only (dtype)
    `(with-cover-dtype-cache
       (let* ((*definitions-cache* (make-hash-table :test #'equal))
              (*cachable-locative-types* (support-dtype ,dtype))
              (*non-cachable-locative-types*
                (set-difference (locative-types) *cachable-locative-types*)))
         ,@body))))

(defun/autoloaded definitions (name &key (dtype t) (sort t))
  """List all definitions of NAME that are of DTYPE as [DREFs][class].

  Just as `(DREF NAME LOCATIVE)` returns the canonical definition, the
  DREF-NAMEs of returned by DEFINITIONS may be different from NAME:

  ```cl-transcript
  (definitions "PAX")
  ==> (#<DREF "MGL-PAX" PACKAGE>)
  ```

  ```cl-transcript
  (definitions 'mgl-pax)
  ==> (#<DREF "MGL-PAX" PACKAGE> #<DREF "mgl-pax" ASDF/SYSTEM:SYSTEM>)
  ```

  Similarly, DREF-LOCATIVE-TYPE may be more made more specific:

  ```cl-transcript
  (definitions 'dref:locate-error :dtype 'type)
  ==> (#<DREF LOCATE-ERROR CONDITION>)
  ```

  If SORT, the returned list is ordered according to SORT-REFERENCES.

  Can be extended via MAP-DEFINITIONS-OF-NAME."""
  (labels
      ((definitions* (locative-types)
         (definitions-with-locative-types name locative-types))
       (maybe-cached ()
         (if (boundp '*definitions-cache*)
             (multiple-value-bind (drefs presentp)
                 (gethash name *definitions-cache*)
               ;; The common case is DTYPE T or TOP. Hence, we
               ;; currently cache all cachable definitions even if
               ;; e.g. DTYPE is a single locative type.
               (unless presentp
                 (setq drefs (definitions* *cachable-locative-types*))
                 (setf (gethash name *definitions-cache*) drefs))
               (when *non-cachable-locative-types*
                 (alexandria:appendf drefs (definitions*
                                            *non-cachable-locative-types*)))
               (filter-drefs (delete-duplicates drefs :test #'xref=) dtype))
             (let ((drefs (definitions* (cover-dtype dtype))))
               (filter-covered-drefs (delete-duplicates drefs :test #'xref=)
                                     dtype)))))
    (if sort
        (sort-references (maybe-cached))
        (maybe-cached))))

(defun definitions-with-locative-types (name locative-types)
  (let ((drefs ()))
    (dolist (locative-type locative-types)
      (map-definitions-of-name (lambda (dref)
                                 (push dref drefs))
                               name locative-type))
    drefs))

(defun/autoloaded dref-apropos (name &key package external-only case-sensitive
                                     (dtype t) (sort t))
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

  Roughly speaking, when NAME or PACKAGE is a SYMBOL, it must match
  the whole @NAME of the definition:

  ```cl-transcript (:dynenv dref-std-env)
  (dref-apropos 'method :package :dref :external-only t)
  ==> (#<DREF METHOD CLASS> #<DREF METHOD LOCATIVE>)
  ```

  On the other hand, when NAME or PACKAGE is a STRING, they are
  matched as substrings to the definition's name PRINC-TO-STRINGed:

  ```cl-transcript (:dynenv dref-std-env)
  (dref-apropos "method" :package :dref :external-only t)
  ==> (#<DREF SETF-METHOD LOCATIVE> #<DREF METHOD CLASS>
  -->  #<DREF METHOD LOCATIVE> #<DREF METHOD-COMBINATION CLASS>
  -->  #<DREF METHOD-COMBINATION LOCATIVE>)
  ```

  Definitions that are not of DTYPE (see DTYPEP) are filtered out:

  ```cl-transcript (:dynenv dref-std-env)
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

  If SORT, the returned list is ordered according to SORT-REFERENCES.

  Can be extended via MAP-REFERENCES-OF-TYPE and
  MAP-DEFINITIONS-OF-NAME."""
  (let ((locative-types (cover-dtype dtype))
        (char-test (if case-sensitive #'char= #'char-equal))
        (string-test (if case-sensitive #'string= #'string-equal))
        (to-try ())
        (drefs ())
        (*print-case* :upcase))
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
        (when (map-definitions-of-type #'consider-dref locative-type)
          (push locative-type to-try)))
      ;; For many locative types, we need to consider all symbols as
      ;; @NAMEs. Iterating over many symbols takes time, so iterate
      ;; once for all locative types TO-TRY.
      (when (and (not (eq package :none)) to-try)
        (loop for symbol being the hash-key
                in (matching-symbols #'matching-package-p #'matching-name-p
                                     external-only)
              do (dolist (dref (definitions-with-locative-types symbol to-try))
                   (push dref drefs))))
      (let ((drefs (filter-covered-drefs
                    (remove-duplicate-drefs/nonstable drefs) dtype)))
        (if sort
            (sort-references drefs)
            drefs)))))

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


(defun sort-references (references &key key)
  "Order REFERENCES in an implementation independent way.

  - @SORT-REFERENCES-NON-SYMBOL

  - @SORT-REFERENCES-SYMBOL"
  (flet ((sort-key (reference)
           (let* ((reference (if key (funcall key reference) reference))
                  (locative-type (xref-locative-type reference)))
             (with-standard-io-syntax*
               ;; Avoid mentions of BASE-CHAR and such.
               (let ((*print-readably* nil))
                 (if (not (symbolp (xref-name reference)))
                     (note @sort-references-non-symbol
                       """Non-symbol named references go first and are
                       [sorted by the locative type][
                       sort-locative-types] first, then by name and
                       locative args."""
                       (format nil "1. ~A ~S ~S"
                               (locative-type-to-sort-key locative-type)
                               (xref-name reference)
                               (xref-locative-args reference)))
                     (note @sort-references-symbol
                       """Symbol-based references go after and are
                       sorted by name first, then [by the
                       locative-type][ sort-locative-types], finally
                       by the locative args."""
                       (format nil "2. ~S ~A ~S"
                               (xref-name reference)
                               (locative-type-to-sort-key locative-type)
                               (xref-locative-args reference)))))))))
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
  "Sort LOCATIVE-TYPES in an implementation independent way.
  The order is alphabetical in SYMBOL-NAME, with the PACKAGE-NAME
  acting as a tie breaker. If UNKNOWN is present among LOCATIVE-TYPES,
  it goes last."
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
