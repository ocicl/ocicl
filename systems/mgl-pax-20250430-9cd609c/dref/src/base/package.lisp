;;;; Normally, the DEFSECTION forms, which do the actual exporting,
;;;; would live close to the definitions they refer to. However, there
;;;; are some self-inflicted design decisions that get in the way:
;;;;
;;;; - We want to export some symbols from the DREF and some from
;;;;   DREF-EXT package.
;;;;
;;;; - We want the home packages of those symbols to be the package
;;;;   from which they are exported.
;;;;
;;;; - Some of the definitions for both packages are autoloaded via
;;;;   the DREF/FULL ASDF:SYSTEM.
;;;;
;;;; One option to fulfil these requirements would be to :EXPORT from
;;;; the DEFINE-PACKAGE forms below, but 1. that would duplicate
;;;; information in the DEFSECTION forms, 2. the documentation
;;;; wouldn't be available when DREF/FULL is not yet loaded. On the
;;;; other hand, 3. DEFSECTIONs could live near the code, where they
;;;; belong.
;;;;
;;;; So instead, the SECTIONs for DREF/FULL are defined in this file
;;;; for both packages, addressing 1 and 2 at the cost of 3.

(mgl-pax:define-package :dref-ext
  (:documentation "See DREF-EXT::@EXTENDING-DREF.")
  (:use #:common-lisp
        #:mgl-pax #:named-readtables #:pythonic-string-reader))

(mgl-pax:define-package :dref
  (:documentation "See DREF::@DREF-MANUAL.")
  (:use #:common-lisp #:dref-ext
        #:mgl-pax #:named-readtables #:pythonic-string-reader))


;;;; Foreshadowing of what the DREF/FULL system defines in the
;;;; DREF-EXT package.
;;;;
;;;; We want the [home package][clhs] of e.g. DREF-EXT:RESOLVE* to be
;;;; DREF-EXT, so we first export everything from DREF-EXT, which is
;;;; :USEd by DREF.

(in-package :dref-ext)

(defmethod exportable-reference-p ((package (eql (find-package 'dref-ext)))
                                   symbol locative-type locative-args)
  (and (call-next-method)
       (not (eq symbol 'asdf:system))
       :warn))

(defun dref-std-env (fn)
  (let ((*package* (find-package :dref)))
    ;; FIXME: Add all others too.
    (progv '(pax::*document-downcase-uppercase-code*
             pax::*transcribe-check-consistency*)
        '(nil #+sbcl t #-sbcl nil)
      (handler-bind ((warning #'muffle-warning))
        (unwind-protect
             (funcall fn)
          (unintern '*my-var* (find-package :dref)))))))

(in-readtable pythonic-string-syntax)

(defsection @extending-dref (:title "Extending DRef"
                             :package :dref
                             :export :dref-ext)
  (@extension-tutorial section)
  (@locative-type-hierarchy section)
  (@defining-locative-types section)
  (@extending-locate section)
  (@extending-everything-else section)
  (@dref-classes section)
  (@source-locations section))

(defsection @extension-tutorial (:title "Extension Tutorial"
                                 :package :dref
                                 :export :dref-ext)
  "Let's see how to tell DRef about new kinds of definitions through
  the example of the implementation of the CLASS locative. Note that
  this is a verbatim [PAX:INCLUDE][locative] of the sources. Please
  ignore any internal machinery. The first step is to define the
  @LOCATIVE-TYPE:"
  (nil (include (:start (class locative)
                 :end (dref::locate* (method () (class (eql class)))))
                :header-nl "```" :footer-nl "```"))
  "Then, we make it possible to look up CLASS definitions:"
  (nil (include (:start (dref::locate* (method () (class (eql class))))
                 :end (resolve* (method () (class-dref))))
                :header-nl "```" :footer-nl "```"))
  "DEFINE-LOCATOR makes `(LOCATE (FIND-CLASS 'DREF))` work, while
  DEFINE-LOOKUP is for `(DREF 'DREF 'CLASS)`. Naturally, for locative
  types that do not define first-class objects, the first method
  cannot be defined.

  Finally, we define a RESOLVE* method to recover the [CLASS][type]
  object from a CLASS-DREF. We also specialize DOCSTRING* and
  SOURCE-LOCATION*:"
  (nil (include (:start (resolve* (method () (class-dref)))
                 :end (dref::%end-of-class-example variable))
                :header-nl "```" :footer-nl "```"))
  "We took advantage of having just made the class locative type being
  RESOLVEable, by specializing DOCSTRING* on the CLASS class.
  SOURCE-LOCATION* was specialized on CLASS-DREF to demonstrate how
  this can be done for non-RESOLVEable locative types.

  Classes have no arglist, so no ARGLIST* method is needed. In the
  following, we describe the pieces in detail.")

(defsection @locative-type-hierarchy (:title "Locative Type Hierarchy"
                                      :package :dref
                                      :export :dref-ext)
  "[Locative types][@LOCATIVE-TYPE] form their own hierarchy, that
  is only superficially similar to the Lisp CLASS hierarchy.
  [ check-lisp-and-pseudo-are-distinct function ][docstring]"
  (dref-class function)
  (locative-type-direct-supers function)
  (locative-type-direct-subs function))

(defsection @defining-locative-types (:title "Defining Locative Types"
                                      :package :dref
                                      :export :dref-ext)
  (define-locative-type macro)
  (define-pseudo-locative-type macro)
  (define-locative-alias macro)
  (@symbol-locatives section))

(defsection @extending-locate (:title "Extending LOCATE"
                               :package :dref
                               :export :dref-ext)
  "[ dref::%locate function][docstring]"
  (@initial-definition section)
  (@canonicalization section)
  (@defining-lookups-locators-and-casts section))

(defsection @initial-definition (:title "Initial Definition"
                                 :package :dref
                                 :export :dref-ext)
  "[ dref::locate-initial-definition function][docstring]")

(defsection @canonicalization (:title "Canonicalization"
                               :package :dref
                               :export :dref-ext)
  "[ dref::canonicalize-dref function][docstring]"
  (@default-downcast section)
  (@cast-name-change section))

(defsection @default-downcast (:title "Default Downcast"
                               :package :dref
                               :export :dref-ext)
  "[ dref::locate* (method () (dref t))][docstring]")

(defsection @defining-lookups-locators-and-casts
    (:title "Defining Lookups, Locators and Casts"
     :package :dref
     :export :dref-ext)
  "As we have seen, the DREF-EXT::@INITIAL-DEFINITION is provided either by a
  lookup or a locator, then DREF-EXT::@CANONICALIZATION works with
  casts. Here, we look at how to define these.

  _Implementation note:_ All three are currently implemented as
  methods of generic functions with [EQL specializers][7.6.2 clhs] for
  the locative type, which may easily prove to be problematic down the
  road. To make future changes easier, the generic function and the
  methods are hidden behind e.g. the DEFINE-LOOKUP and CALL-LOOKUP
  macros."
  (*check-locate* variable)
  (define-lookup macro)
  (call-lookup macro)
  (define-locator macro)
  (call-locator macro)
  (define-cast macro)
  (call-cast macro)
  (locate-error function)
  (check-locative-args macro))

(defsection @extending-everything-else (:title "Extending Everything Else"
                                        :package :dref
                                        :export :dref-ext)
  (resolve* generic-function)
  (resolve-error function)
  (map-definitions-of-name generic-function)
  (map-definitions-of-type generic-function)
  (arglist* generic-function)
  (docstring* generic-function)
  (source-location* generic-function)
  (@definition-properties section))

(defsection @definition-properties (:title "Definition Properties"
                                    :package :dref
                                    :export :dref-ext)
  "Arbitrary data may be associated with definitions.
  This mechanism is used by `ARGLIST*`, `DOCSTRING*` and
  `SOURCE-LOCATION*` for easy extension.

  The following functions take an XREF argument and not a DREF to
  allow working with [non-canonical][dref-ext::@canonicalization] or
  non-existent definitions."
  (definition-property function)
  (delete-definition-property function)
  (definition-properties function)
  (delete-definition-properties function)
  (move-definition-properties function))

(defsection @symbol-locatives (:title "Symbol Locatives"
                               :package :dref
                               :export :dref-ext)
  "Let's see how the opaque DEFINE-SYMBOL-LOCATIVE-TYPE and the
  obscure DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE macros work together
  to simplify the common task of associating definition with a symbol
  in a certain context."
  (define-symbol-locative-type macro)
  (define-definer-for-symbol-locative-type macro))

(defsection @dref-classes (:title "DREF-CLASSes"
                           :package :dref
                           :export :dref-ext)
  "These are the DREF-CLASSes corresponding to DREF::@BASIC-LOCATIVE-TYPES.
  They are exported to make it possible to go beyond the
  @BASIC-OPERATIONS (e.g. PAX:DOCUMENT-OBJECT*). For
  DREF-EXT::@DEFINING-LOCATIVE-TYPES, they are not necessary, as
  DEFINE-LOCATIVE-TYPE handles inheritance automatically based on its
  LOCATIVE-SUPERTYPES argument."
  "**[for Variables][ @variablelike-locatives]**"
  (variable-dref class)
  (constant-dref class)
  "**[for Macros][ @macrolike-locatives]**"
  (macro-dref class)
  (symbol-macro-dref class)
  (compiler-macro-dref class)
  (setf-dref class)
  (setf-compiler-macro-dref class)
  "**[for Functions][ @functionlike-locatives]**"
  (function-dref class)
  (setf-function-dref class)
  (generic-function-dref class)
  (setf-generic-function-dref class)
  (method-dref class)
  (setf-method-dref class)
  (method-combination-dref class)
  (reader-dref class)
  (writer-dref class)
  (accessor-dref class)
  (structure-accessor-dref class)
  "**[for Types and Declarations][ @typelike-locatives]**"
  (type-dref class)
  (class-dref class)
  (declaration-dref class)
  "**[for the Condition System][ @condition-system-locatives]**"
  (condition-dref class)
  (restart-dref class)
  "**[for Packages and Readtables][ @packagelike-locatives]**"
  (asdf-system-dref class)
  (package-dref class)
  (readtable-dref class)
  "**[for Unknown Definitions][ @unknown-definitions]**"
  (unknown-dref class)
  "**[for DRef Constructs][ @dref-locatives]**"
  (dtype-dref class)
  (locative-dref class)
  (symbol-locative-dref class)
  (lambda-dref class))

(defsection @source-locations (:title "Source Locations"
                               :package :dref
                               :export :dref-ext)
  "These represent the file or buffer position of a [defining
  form][clhs] and are returned by the SOURCE-LOCATION function. For
  the details, see the Elisp function `slime-goto-source-location`."
  (make-source-location function)
  (source-location-p function)
  (source-location-file function)
  (source-location-file-position function)
  (source-location-buffer function)
  (source-location-buffer-position function)
  (source-location-snippet function)
  (source-location-adjusted-file-position function)
  (this-source-location macro))


;;;; Foreshadowing of what the DREF/FULL system defines in the DREF
;;;; package.

(in-package :dref)

(import 'dref-ext::dref-std-env)
(import 'dref-ext::@cast-name-change)

(in-readtable pythonic-string-syntax)

(defsection @basic-locative-types (:title "Basic Locative Types")
  """The following are the @LOCATIVE-TYPEs supported out of the
  box. As all locative types, they are named by symbols. When there is
  a CL type corresponding to the reference's locative type, the
  references can be RESOLVEd to a unique object as is the case in

  ```cl-transcript (:dynenv dref-std-env)
  (resolve (dref 'print 'function))
  ==> #<FUNCTION PRINT>
  => T
  ```

  Even if there is no such CL type, the ARGLIST, the DOCSTRING, and
  the SOURCE-LOCATION of the defining form is usually recorded unless
  otherwise noted.

  The basic locative types and their inheritance structure is loosely
  based on the DOC-TYPE argument of [CL:DOCUMENTATION][clhs]."""
  (@variablelike-locatives section)
  (@macrolike-locatives section)
  (@functionlike-locatives section)
  (@typelike-locatives section)
  (@condition-system-locatives section)
  (@packagelike-locatives section)
  (@unknown-definitions section)
  (@dref-locatives section))

(defsection @variablelike-locatives (:title "Locatives for Variables")
  (variable locative)
  (constant locative))

(defsection @macrolike-locatives (:title "Locatives for Macros")
  (setf locative)
  (macro locative)
  (symbol-macro locative)
  (compiler-macro locative)
  (setf-compiler-macro locative))

(defsection @functionlike-locatives
    (:title "Locatives for Functions and Methods")
  (function locative)
  (setf-function locative)
  (generic-function locative)
  (setf-generic-function locative)
  (method locative)
  (setf-method locative)
  (method-combination locative)
  (reader locative)
  (writer locative)
  (accessor locative)
  (structure-accessor locative))

(defsection @typelike-locatives (:title "Locatives for Types and Declarations")
  (type locative)
  (class locative)
  (declaration locative))

(defsection @condition-system-locatives
    (:title "Locatives for the Condition System")
  (condition locative)
  (restart locative)
  (define-restart macro))

(defsection @packagelike-locatives
    (:title "Locatives for Packages and Readtables")
  (asdf:system locative)
  (package locative)
  (readtable locative))

(defsection @unknown-definitions (:title "Locatives for Unknown Definitions")
  (unknown locative))

(defsection @dref-locatives (:title "Locatives for DRef Constructs")
  (dtype locative)
  (locative locative)
  (lambda locative))
