;;;; After this file is loaded, the rest of PAX and XREF can be
;;;; written using DEFSECTION.

(in-package :mgl-pax)

;;; Should this remove docstrings of referenced things?
(defvar *discard-documentation-p* nil
  "The default value of DEFSECTION's DISCARD-DOCUMENTATION-P argument.
  One may want to set *DISCARD-DOCUMENTATION-P* to true before
  building a binary application.")

(defmacro defsection (name (&key (package '*package*) (readtable '*readtable*)
                              (export t) title link-title-to
                              (discard-documentation-p
                               *discard-documentation-p*))
                      &body entries)
  "Define a documentation section and maybe export referenced symbols.
  A bit behind the scenes, a global variable with NAME is defined and
  is bound to a [SECTION][class] object. By convention, section names
  start with the character `@`. See @INTRODUCTION for an example.

  **Entries**

  ENTRIES consists of docstrings and references in any order.
  Docstrings are arbitrary strings in Markdown format.

  References are [XREF][class]s given in the form `(NAME LOCATIVE)`.
  For example, `(FOO FUNCTION)` refers to the function `FOO`, `(@BAR
  SECTION)` says that `@BAR` is a subsection of this
  one. `(BAZ (METHOD (T T T)))` refers to the default method of the
  three argument generic function `BAZ`. `(FOO FUNCTION)` is
  equivalent to `(FOO (FUNCTION))`. See the DRef DREF::@INTRODUCTION
  for more.

  The same name may occur in multiple references, typically with
  different locatives, but this is not required.

  The references are not LOCATEd until documentation is generated, so
  they may refer to things yet to be defined.

  **Exporting**

  If EXPORT is true (the default), NAME and the @NAMEs of references
  among ENTRIES which are SYMBOLs are candidates for exporting. A
  candidate symbol is exported if

  - it is [accessible][find-symbol] in PACKAGE, and

  - there is a reference to it in the section being defined which is
    approved by EXPORTABLE-REFERENCE-P.

  See DEFINE-PACKAGE if you use the export feature. The idea with
  confounding documentation and exporting is to force documentation of
  all exported symbols.

  **Misc**

  TITLE is a string containing Markdown or NIL. If non-NIL, it
  determines the text of the heading in the generated output.
  LINK-TITLE-TO is a reference given as an `(NAME LOCATIVE)` pair or
  NIL, to which the heading will link when generating HTML. If not
  specified, the heading will link to its own anchor.

  When DISCARD-DOCUMENTATION-P (defaults to *DISCARD-DOCUMENTATION-P*)
  is true, ENTRIES will not be recorded to save memory."
  (check-section-entries entries name)
  (check-link-title-to link-title-to name)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (when ,export
         (export-some-symbols ',name ',entries
                              ,(if (eq export t)
                                   package
                                   ;; This is a tentative feature,
                                   ;; currently undocumented.
                                   export))))
     (defparameter ,name
       (make-instance 'section
                      :name ',name
                      :package (find-package ,package)
                      :readtable ,readtable
                      :title ,title
                      :link-title-to ',link-title-to
                      :entries ',(and (not discard-documentation-p)
                                      (cons '%to-xref entries))))))

(defclass section ()
  ((name
    :initarg :name :reader section-name
    :documentation "The name of the global variable whose value is
    this SECTION object.")
   (package
    :initarg :package :reader section-package
    :documentation "*PACKAGE* will be bound to this package when
    generating documentation for this section.")
   (readtable
    :initarg :readtable :reader section-readtable
    :documentation "*READTABLE* will be bound to this when generating
    documentation for this section.")
   (title
    :initarg :title :reader section-title
    :documentation "A @TITLE or NIL. Used in generated
    documentation (see @MARKDOWN-OUTPUT) and is returned by DOCTITLE
    for SECTION objects and SECTION DREF::@DEFINITIONS.")
   ;; DEFSECTION's raw link-title-to argument. Translated to an XREF
   ;; by SECTION-LINK-TITLE-TO, which is defined later so that XREF
   ;; can depend on mgl-pax/basics.
   (%link-title-to :initform nil :initarg :link-title-to)
   ;; DEFSECTION's raw ENTRIES argument. See SECTION-ENTRIES.
   (%entries :initarg :entries))
  (:documentation "DEFSECTION stores its NAME, TITLE, [PACKAGE][type],
  [READTABLE][type] and ENTRIES arguments in [SECTION][class]
  objects."))

(defmethod print-object ((section section) stream)
  (print-unreadable-object (section stream :type t)
    (format stream "~S" (section-name section))))

(defun ref-list-p (obj)
  (and (listp obj) (= (length obj) 2)))

(defun check-section-entries (entries section-name)
  (loop for entry in entries
        do (unless (or (stringp entry) (ref-list-p entry))
             (malformed-ref-list-in-section-error "entry" entry section-name))))

(defun check-link-title-to (link-title-to section-name)
  (unless (or (null link-title-to)
              (ref-list-p link-title-to))
    (malformed-ref-list-in-section-error :link-title-to link-title-to
                                         section-name)))

(defun malformed-ref-list-in-section-error (what ref-list section-name)
  (error "~@<Malformed ~A ~S in SECTION ~S. ~
         It should be of form (NAME LOCATIVE).~:@>"
         what (prin1-to-string/fully-qualified ref-list)
         (prin1-to-string/fully-qualified section-name)))

(defun prin1-to-string/fully-qualified (object)
  (let ((*package* (find-package :keyword)))
    (prin1-to-string object)))


;;;; Exporting

(defun export-some-symbols (section-name entries package)
  (let ((package1 (find-package package)))
    (unless package1
      (error "~@<~S for ~S: Cannot export from non-existent package ~S.~:@>"
             'defsection section-name package))
    (when (exportablep section-name package1 section-name 'section)
      (export section-name package1))
    (dolist (entry entries)
      (when (listp entry)
        (destructuring-bind (name locative) entry
          (when (and (symbolp name)
                     (exportablep section-name package1 name locative))
            (export name package1)))))))

(defun exportablep (section-name package symbol locative)
  (let* ((locative (if (listp locative) locative (list locative)))
         (exportablep (exportable-reference-p package symbol (first locative)
                                              (rest locative))))
    (when exportablep
      (if (symbol-accessible-in-package-p symbol package)
          t
          (when (eq exportablep :warn)
            (warn "~@<~S for ~S: Cannot export non-accessible ~
                  symbol ~S from ~S.~:@>"
                  'defsection section-name symbol package))))))

(defun symbol-accessible-in-package-p (symbol package)
  (eq symbol (find-symbol (symbol-name symbol) package)))

(defgeneric exportable-reference-p (package symbol locative-type locative-args)
  (:documentation "Return true if SYMBOL is to be exported from
  PACKAGE when it occurs in a DEFSECTION in a reference with
  LOCATIVE-TYPE and LOCATIVE-ARGS. SYMBOL is [accessible][clhs] in
  PACKAGE.

  The default method calls EXPORTABLE-LOCATIVE-TYPE-P with
  LOCATIVE-TYPE and ignores the other arguments.

  By default, SECTIONs and GLOSSARY-TERMs are not exported although
  they are EXPORTABLE-LOCATIVE-TYPE-P. To export symbols naming
  sections from MGL-PAX, the following method could be added:

  ```
  (defmethod exportable-reference-p ((package (eql (find-package 'mgl-pax)))
                                     symbol (locative-type (eql 'section))
                                     locative-args)
    t)
  ```")
  (:method (package symbol locative-type locative-args)
    (declare (ignore package symbol locative-args))
    (exportable-locative-type-p locative-type)))

(defmethod exportable-reference-p (package symbol
                                   (locative-type (eql 'section))
                                   locative-args)
  (declare (ignore package symbol locative-args))
  nil)

(defmethod exportable-reference-p (package symbol
                                   (locative-type (eql 'glossary-term))
                                   locative-args)
  (declare (ignore package symbol locative-args))
  nil)

(defmethod exportable-reference-p (package symbol
                                   (locative-type (eql 'note))
                                   locative-args)
  (declare (ignore package symbol locative-args))
  nil)

(defgeneric exportable-locative-type-p (locative-type)
  (:documentation "Return true if symbols in references with
  LOCATIVE-TYPE are to be exported by default when they occur in a
  DEFSECTION. The default method returns T, while the methods for
  locative types [SECTION][locative], [GLOSSARY-TERM][locative],
  [PACKAGE][locative], [ASDF:SYSTEM][locative], METHOD and
  [INCLUDE][locative] return NIL.

  This function is called by the default method of
  EXPORTABLE-REFERENCE-P to decide what symbols DEFSECTION shall
  export when its EXPORT argument is true.")
  (:method (locative-type)
    (declare (ignore locative-type))
    t))

;;;; These methods must be defined here else the DEFSECTION forms in
;;;; pax.lisp will export too much.

(defmethod exportable-locative-type-p ((locative-type (eql 'asdf:system)))
  nil)

(defmethod exportable-locative-type-p ((locative-type (eql 'package)))
  nil)

(defmethod exportable-locative-type-p ((locative-type (eql 'method)))
  nil)

(defmethod exportable-locative-type-p ((locative-type (eql 'include)))
  nil)


(defclass glossary-term ()
  ((name
    :initarg :name :reader glossary-term-name
    :documentation "The name of the global variable whose value is
    this GLOSSARY-TERM object.")
   (title
    :initarg :title :reader glossary-term-title
    :documentation "A @TITLE or NIL. Used in generated
    documentation (see @MARKDOWN-OUTPUT) and is returned by DOCTITLE
    for GLOSSARY-TERM objects and GLOSSARY-TERM DREF::@DEFINITIONS..")
   (url
    :initarg :url :reader glossary-term-url
    :documentation "A string or NIL.")
   (docstring :initarg :docstring :reader glossary-term-docstring))
  (:documentation "See DEFINE-GLOSSARY-TERM."))

(defmacro define-glossary-term
    (name (&key title url (discard-documentation-p *discard-documentation-p*))
     &body docstring)
  "Define a global variable with NAME, and set it to a [GLOSSARY-TERM]
  [class] object. TITLE, URL and DOCSTRING are Markdown strings or
  NIL. Glossary terms are DOCUMENTed in the lightweight bullet +
  locative + name/title style. See the glossary entry @NAME for an
  example.

  When a glossary term is linked to in documentation, its TITLE will
  be the link text instead of the name of the symbol (as with
  SECTIONs).

  Glossary entries with a non-NIL URL are like external links: they
  are linked to their URL in the generated documentation. These offer
  a more reliable alternative to using Markdown reference links and
  are usually not included in SECTIONs.

  When DISCARD-DOCUMENTATION-P (defaults to *DISCARD-DOCUMENTATION-P*)
  is true, DOCSTRING will not be recorded to save memory."
  `(defparameter ,name
     (make-instance 'glossary-term
                    :name ',name :title ,title :url ,url
                    :docstring ,(unless discard-documentation-p
                                  (apply #'concatenate 'string docstring)))))


(defmacro define-package (package &rest options)
  "This is like CL:DEFPACKAGE but silences warnings and errors
  signalled when the redefined package is at variance with the current
  state of the package. Typically this situation occurs when symbols
  are exported by calling EXPORT (as is the case with DEFSECTION) as
  opposed to adding :EXPORT forms to the DEFPACKAGE form and the
  package definition is subsequently reevaluated. See the section on
  [package variance](http://www.sbcl.org/manual/#Package-Variance) in
  the SBCL manual.

  The bottom line is that if you rely on DEFSECTION to do the
  exporting, then you'd better use DEFINE-PACKAGE."
  `(eval-when (:compile-toplevel :load-toplevel, :execute)
     (locally
         (declare #+sbcl
                  (sb-ext:muffle-conditions sb-kernel::package-at-variance))
       (handler-bind
           (#+sbcl (sb-kernel::package-at-variance #'muffle-warning))
         (cl:defpackage ,package ,@options)
         ;; https://abcl.org/trac/ticket/16
         #+abcl
         ,@(loop for option in options
                 when (eq (first option) :use)
                   collect `(use-package ',(rest option) ',package)
                 when (eq (first option) :export)
                   collect `(export (mapcar (lambda (sym)
                                              (intern sym ',package))
                                            ',(rest option))
                                    ',package))))))


;;; Arrange for the home package of these LOCATIVEs (exported by DRef)
;;; to be PAX. The home packages are visible in links, and from that
;;; point of view, DRef is an implementation detail.
(export 'locative)
(export 'docstring)
(export 'constant)
(export 'macro)
(export 'symbol-macro)
(export 'accessor)
(export 'reader)
(export 'writer)
(export 'structure-accessor)
(export 'include)
(export 'unknown)
