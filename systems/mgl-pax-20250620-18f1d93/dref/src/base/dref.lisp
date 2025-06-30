(in-package :dref)

(in-readtable pythonic-string-syntax)

(defsection @dref-manual (:title "DRef Manual")
  (@links-and-systems section)
  (@introduction section)
  (@references section)
  (@dtypes section)
  (@listing-definitions section)
  (@basic-operations section)
  (@basic-locative-types section)
  (dref-ext::@extending-dref section))

(defsection @links-and-systems (:title "Links and Systems")
  "Here is the [official
  repository](https://github.com/melisgl/mgl-pax/dref) and the [HTML
  documentation](http://melisgl.github.io/mgl-pax-world/dref-manual.html)
  for the latest version.

  DRef is bundled in the same repository with [PAX][pax::@pax-manual],
  the documentation system."
  ("dref" asdf:system)
  ("dref/full" asdf:system))

(defsection @introduction (:title "Introduction")
  """_What if definitions were first-class objects?_

  Some [defining forms][clhs] do not create first-class
  [objects][(clhs glossary-term)]. For example, DEFUN creates
  [FUNCTION][class] objects, but DEFVAR does not create variable
  objects as no such thing exists. The main purpose of this library is
  to fill this gap with the introduction of [DREF][class] objects:

  ```cl-transcript
  (defvar *my-var* nil
    "This is my var.")
  (dref '*my-var* 'variable)
  ==> #<DREF *MY-VAR* VARIABLE>
  ```

  DREFs just package up a @NAME (`\*MY-VAR*`) and a
  @LOCATIVE ([VARIABLE][locative]) then check that the definition
  actually exists:

  ```cl-transcript
  (dref 'junk 'variable)
  .. debugger invoked on LOCATE-ERROR:
  ..   Could not locate JUNK VARIABLE.
  ```

  The @BASIC-OPERATIONS on definitions in DRef are ARGLIST, DOCSTRING
  and SOURCE-LOCATION.

  ```cl-transcript
  (docstring (dref '*my-var* 'variable))
  => "This is my var."
  ```

  For definitions associated with objects, the definition can be
  LOCATEd from the object:

  ```cl-transcript
  (locate #'print)
  ==> #<DREF PRINT FUNCTION>
  ```

  These objects designate their definitions, so `(DOCSTRING #'PRINT)`
  works. Extending DRef and these operations is possible through
  DREF-EXT::@DEFINING-LOCATIVE-TYPES. It is also possible to define
  new operations. For example, [\\PAX][MGL-PAX::@PAX-MANUAL] makes
  PAX:DOCUMENT extensible through PAX:DOCUMENT-OBJECT*.

  Finally, existing definitions can be queried with DEFINITIONS and
  DREF-APROPOS:

  ```cl-transcript
  (definitions 'dref-ext:arglist*)
  ==> (#<DREF ARGLIST* GENERIC-FUNCTION>
  -->  #<DREF ARGLIST* (METHOD (MGL-PAX::GO-DREF))>
  -->  #<DREF ARGLIST* (METHOD (LAMBDA-DREF))>
  -->  #<DREF ARGLIST* (METHOD (TYPE-DREF))>
  -->  #<DREF ARGLIST* (METHOD (METHOD-DREF))>
  -->  #<DREF ARGLIST* (METHOD (FUNCTION-DREF))>
  -->  #<DREF ARGLIST* (METHOD (COMPILER-MACRO-DREF))>
  -->  #<DREF ARGLIST* (METHOD (MACRO-DREF))>
  -->  #<DREF ARGLIST* (METHOD (SETF-DREF))> #<DREF ARGLIST* (METHOD (T))>
  -->  #<DREF ARGLIST* (METHOD (DREF))>
  -->  #<DREF ARGLIST* (UNKNOWN
  -->                   (DECLAIM ARGLIST*
  -->                            FTYPE))>)
  ```

  ```cl-transcript
  (dref-apropos 'locate-error :package :dref)
  ==> (#<DREF LOCATE-ERROR CONDITION> #<DREF LOCATE-ERROR FUNCTION>)

  (dref-apropos "ate-err" :package :dref :external-only t)
  ==> (#<DREF LOCATE-ERROR CONDITION> #<DREF LOCATE-ERROR FUNCTION>)
  ```""")


(defsection @references (:title "References")
  "After the @INTRODUCTION, here we get into the details. Of special
  interest are:

  - The XREF function to construct an arbitrary @REFERENCE without any
    checking of validity.

  - LOCATE and [DREF][function] to look up the @DEFINITION of an
    object (e.g `#'PRINT`) or a @REFERENCE (e.g. `(XREF 'PRINT
    'FUNCTION)`).

  - RESOLVE to find the first-class (non-[XREF][class]) object the
    definition refers to, if any.

  The @BASIC-OPERATIONS (ARGLIST, DOCSTRING, SOURCE-LOCATION) know how to
  deal with references (discussed in the DREF-EXT::@EXTENDING-DREF)."
  (xref class)
  (xref function)
  (xref= function)
  (dref class)
  (locate function)
  (dref function)
  (resolve function)
  (locate-error condition)
  (resolve-error condition)
  (@dissecting-references section)
  (@references-glossary section))

(defclass xref ()
  ((name :initarg :name :reader xref-name
         :documentation "The @NAME of the reference.")
   (locative :initarg :locative :reader xref-locative
             :documentation "The @LOCATIVE of the reference.

   The locative is normalized by replacing single-element lists with
   their only element:

  ```cl-transcript (:dynenv dref-std-env)
  (xref 'print 'function)
  ==> #<XREF PRINT FUNCTION>
  ```
  ```cl-transcript (:dynenv dref-std-env)
  (xref 'print '(function))
  ==> #<XREF PRINT FUNCTION>
  ```"))
  (:documentation "An XREF (cross-reference) is a @REFERENCE. It may
  represent some kind of @DEFINITION of its @NAME in the context given
  by its @LOCATIVE. The definition may not exist and the locative may
  even be [invalid][@locative]. The subclass [DREF][class] represents
  definitions that exist."))

;;; Canonicalize it a bit for easier comparison. E.g. (FUNCTION) =>
;;; FUNCTION.
(declaim (inline normalize-locative))
(defun normalize-locative (locative)
  (if (and (listp locative)
           (null (cdr locative)))
      (first locative)
      locative))

(defmethod initialize-instance :after ((xref xref) &key &allow-other-keys)
  (setf (slot-value xref 'locative)
        (normalize-locative (slot-value xref 'locative))))

(defun xref (name locative)
  "A shorthand for `(MAKE-INSTANCE 'XREF :NAME NAME :LOCATIVE LOCATIVE)`
  to create [XREF][class] objects. It does no error checking: the
  LOCATIVE-TYPE of LOCATIVE-TYPE need not be defined, and the
  LOCATIVE-ARGS need not be valid. Use LOCATE or the DREF function to
  create [DREF][class] objects."
  (make-instance 'xref :name name :locative locative))

(declaim (inline xref=))
(defun xref= (xref1 xref2)
  "See if XREF1 and XREF2 have the same XREF-NAME and XREF-LOCATIVE
  under EQUAL. Comparing like this makes most sense for
  [DREF][class]s. However, two [XREF][class]s different under XREF=
  may denote the same [DREF][class]s."
  (and (equal (xref-name xref1)
              (xref-name xref2))
       (equal (xref-locative xref1)
              (xref-locative xref2))))

;;; This also checks for EQUALness and not whether NAME is equivalent
;;; to the XREF-NAME of XREF (as in it would resolve to the same thing
;;; with the locative).
(declaim (inline xref-name=))
(defun xref-name= (name xref)
  (equal name (xref-name xref)))

(defclass dref (xref)
  ((name
    :reader dref-name
    :documentation "The same as XREF-NAME, but only works on
    [DREF][class]s. Use it as a statement of intent.")
   (locative
    :reader dref-locative
    :documentation "The same as XREF-LOCATIVE, but only works on
    [DREF][class]s. Use it as a statement of intent.")
   (origin
    :reader dref-origin
    :documentation """The object from which LOCATE constructed this
    [DREF][class]. DREF-ORIGIN may have @PRESENTATION arguments, which
    are not included in LOCATIVE-ARGS as is the case with the INITFORM
    argument of the VARIABLE locative:

    ```cl-transcript (:dynenv dref-std-env)
    (dref '*standard-output* '(variable "see-below"))
    ==> #<DREF *STANDARD-OUTPUT* VARIABLE>
    ```

    ```cl-transcript (:dynenv dref-std-env)
    (dref-origin (dref '*standard-output* '(variable "see-below")))
    ==> #<XREF *STANDARD-OUTPUT* (VARIABLE "see-below")>
    ```

    The INITFORM argument overrides the global binding of
    *STANDARD-OUTPUT* when it's PAX:DOCUMENTed:

    ```cl-transcript (:dynenv dref-std-env)
    (first-line
     (pax:document (dref '*standard-output* '(variable "see-below"))
                   :stream nil))
    => "- [variable] *STANDARD-OUTPUT* \"see-below\""
    ```"""))
  (:documentation "DREFs can be thought of as @DEFINITIONs that
  actually exist, although changes in the system can invalidate
  them (for example, a DREF to a function definition can be
  invalidated by FMAKUNBOUND). DREFs must be created with LOCATE or
  the DREF function.

  Two DREFs created in the same [dynamic environment][clhs] denote the
  same thing if and only if they are XREF=."))

(defmethod print-object ((xref xref) stream)
  (if (or *print-escape* *print-readably*)
      (print-unreadable-object (xref stream :type nil)
        (format stream "~S ~S ~S"
                (if (typep xref 'dref)
                    ;; Hide the actual type of DREFs (e.g. FUNCTION-DREF).
                    ;; That's an implementation detail, and it's
                    ;; determined by the locative, anyway.
                    'dref
                    (type-of xref))
                (xref-name xref)
                (xref-locative xref)))
      ;; PRINC ends up here.
      (format stream "~S ~S" (xref-name xref)
              (xref-locative xref))))


(define-condition locate-error (error)
  ((object :initarg :object :reader locate-error-object)
   (message :initarg :message :reader locate-error-message)
   (message-args :initarg :message-args :reader locate-error-message-args))
  (:documentation "Signalled by LOCATE when the definition cannot be
  found, and ERRORP is true.")
  (:report (lambda (condition stream)
             (let ((object (locate-error-object condition))
                   (message (locate-error-message condition)))
               (when (zerop (length message))
                 (setq message nil))
               (if (typep object 'xref)
                   (format stream "~@<Could not locate ~S ~S.~:_~@[ ~?~]~:@>"
                           (xref-name object)
                           (xref-locative object)
                           message
                           (locate-error-message-args condition))
                   (format stream "~@<Could not locate ~S.~:_~@[ ~?~]~:@>"
                           object
                           message
                           (locate-error-message-args condition)))))))

;;; This gets clobbered with an empty function when DREF/FULL is
;;; loaded.
(autoload ensure-dref-loaded "dref/full" :export nil)

(declaim (ftype function dref-class)
         (ftype function %locate))

(defvar *locate-error-ignored* nil)

(defmacro with-locate-error-ignored (&body body)
  `(handler-case
       (let ((*locate-error-ignored* t))
         ,@body)
     (locate-error ())))

(defun locate (object &optional (errorp t))
  """Return a [DREF][class] representing the @DEFINITION of OBJECT.

  OBJECT must be a supported first-class object, a DREF, or an
  [XREF][class]:

  ```cl-transcript (:dynenv dref-std-env)
  (locate #'print)
  ==> #<DREF PRINT FUNCTION>
  ```
  ```cl-transcript (:dynenv dref-std-env)
  (locate (locate #'print))
  ==> #<DREF PRINT FUNCTION>
  ```
  ```cl-transcript (:dynenv dref-std-env)
  (locate (xref 'print 'function))
  ==> #<DREF PRINT FUNCTION>
  ```

  When OBJECT is a DREF, it is simply returned.

  Else, a LOCATE-ERROR is signalled if OBJECT is an XREF with an
  invalid @LOCATIVE, or if no corresponding definition is found. If
  ERRORP is NIL, then NIL is returned instead.

  ```cl-transcript (:dynenv dref-std-env)
  (locate (xref 'no-such-function 'function))
  .. debugger invoked on LOCATE-ERROR:
  ..   Could not locate NO-SUCH-FUNCTION FUNCTION.
  ..   NO-SUCH-FUNCTION does not name a function.
  ```
  ```cl-transcript (:dynenv dref-std-env)
  (locate (xref 'print '(function xxx)))
  .. debugger invoked on LOCATE-ERROR:
  ..   Could not locate PRINT #'XXX.
  ..   Bad arguments (XXX) for locative FUNCTION with lambda list NIL.
  ```
  ```cl-transcript (:dynenv dref-std-env)
  (locate "xxx")
  .. debugger invoked on LOCATE-ERROR:
  ..   Could not locate "xxx".
  ```

  Use the XREF function to construct an XREF without error checking.

  See DREF-EXT::@EXTENDING-LOCATE."""
  (ensure-dref-loaded)
  (cond ((typep object 'dref)
         object)
        (errorp
         (%locate object))
        (t
         (with-locate-error-ignored
           (%locate object)))))

(defun dref (name locative &optional (errorp t))
  "Shorthand for `(LOCATE (XREF NAME LOCATIVE) ERRORP)`."
  (locate (xref name locative) errorp))

(define-condition resolve-error (error)
  ((dref :initarg :dref :reader resolve-error-dref)
   (message :initarg :message :reader resolve-error-message))
  (:documentation "Signalled by RESOLVE when the object defined cannot
  be returned, and ERRORP is true.")
  (:report (lambda (condition stream)
             (let ((*package* (find-package :cl))
                   (dref (resolve-error-dref condition)))
               (format stream "~@<Could not resolve ~S ~S.~@[ ~A~]~:@>"
                       (dref-name dref) (dref-locative dref)
                       (resolve-error-message condition))))))

(declaim (ftype function resolve*))

(defun resolve (object &optional (errorp t))
  "If OBJECT is an [XREF][class], then return the first-class object
  associated with its definition if any. Return OBJECT if it's not an
  XREF. Thus, the value returned is never an XREF. The second return
  value is whether resolving succeeded.

  ```cl-transcript (:dynenv dref-std-env)
  (resolve (dref 'print 'function))
  ==> #<FUNCTION PRINT>
  => T
  ```
  ```cl-transcript (:dynenv dref-std-env)
  (resolve #'print)
  ==> #<FUNCTION PRINT>
  => T
  ```

  If OBJECT is an XREF, and the definition for it cannot be LOCATEd,
  then LOCATE-ERROR is signalled.

  ```cl-transcript (:dynenv dref-std-env)
  (resolve (xref 'undefined 'variable))
  .. debugger invoked on LOCATE-ERROR:
  ..   Could not locate UNDEFINED VARIABLE.
  ```

  If there is a definition, but there is no first-class object
  corresponding to it, then RESOLVE-ERROR is signalled or NIL is
  returned depending on ERRORP:

  ```cl-transcript (:dynenv dref-std-env)
  (resolve (dref '*print-length* 'variable))
  .. debugger invoked on RESOLVE-ERROR:
  ..   Could not resolve *PRINT-LENGTH* VARIABLE.
  ```
  ```cl-transcript (:dynenv dref-std-env)
  (resolve (dref '*print-length* 'variable) nil)
  => NIL
  => NIL
  ```

  RESOLVE is a partial inverse of LOCATE: if a [DREF][class] is
  RESOLVEable, then LOCATEing the object it resolves to recovers the
  DREF equivalent to the original (XREF= and of the same type but not
  EQ).

  Can be extended via RESOLVE*."
  (ensure-dref-loaded)
  (cond ((not (typep object 'xref))
         (values object t))
        (errorp
         (values (resolve* (locate object)) t))
        (t
         (handler-case
             (values (resolve* (locate object)) t)
           ((or locate-error resolve-error) ()
             (values nil nil))))))


(defsection @dissecting-references (:title "Dissecting References")
  (xref-name (reader xref))
  (xref-locative (reader xref))
  (dref-name (reader dref))
  (dref-locative (reader dref))
  (dref-origin (reader dref))
  (locative-type function)
  (locative-args function)
  "The following convenience functions are compositions of
  {`LOCATIVE-TYPE`, `LOCATIVE-ARGS`} and {`XREF-LOCATIVE`,
  `DREF-LOCATIVE`}."
  (xref-locative-type function)
  (xref-locative-args function)
  (dref-locative-type function)
  (dref-locative-args function))

(defun locative-type (locative)
  "Return @LOCATIVE-TYPE of the @LOCATIVE LOCATIVE. This is the first
  element of LOCATIVE if it's a list. If it's a symbol, it's that
  symbol itself."
  (if (listp locative)
      (first locative)
      locative))

(defun locative-args (locative)
  "Return the REST of @LOCATIVE LOCATIVE if it's a list. If it's a symbol,
  then return NIL. See @LOCATIVE."
  (if (listp locative)
      (rest locative)
      ()))

(declaim (inline xref-locative-type))
(defun xref-locative-type (xref)
  (locative-type (xref-locative xref)))

(declaim (inline xref-locative-args))
(defun xref-locative-args (xref)
  (locative-args (xref-locative xref)))

(declaim (inline dref-locative-type))
(defun dref-locative-type (dref)
  (locative-type (dref-locative dref)))

(declaim (inline dref-locative-args))
(defun dref-locative-args (dref)
  (locative-args (dref-locative dref)))


(defsection @references-glossary (:title "References Glossary")
  (@reference glossary-term)
  (@definition glossary-term)
  (@name glossary-term)
  (@locative glossary-term)
  (@locative-type glossary-term)
  (@presentation glossary-term))

(define-glossary-term @reference (:title "reference")
  "A reference is a @NAME plus a @LOCATIVE, and it identifies a
  possible definition. References are of class XREF. When a reference
  is a [DREF][class], it may also be called a definition.")

(define-glossary-term @definition (:title "definition")
  "A definition is a @REFERENCE that identifies a concrete definition.
  Definitions are of class DREF. A definition RESOLVEs to the
  first-class object associated with the definition if such a thing
  exists, and LOCATE on this object returns the canonical DREF object
  that's unique under XREF=.

  The kind of a definition is given by its @LOCATIVE-TYPE. There is at
  most one definition for any given @NAME and locative type.
  Equivalently, there can be no two definitions of the same DREF-NAME
  and DREF-LOCATIVE-TYPE but different DREF-LOCATIVE-ARGS.")

(define-glossary-term @name (:title "name")
  "Names are symbols, strings and nested lists of the previous, which
  name [functions][function locative], [types][type locative],
  [packages][package locative], etc. Together with @LOCATIVEs, they
  form @REFERENCEs.

  See XREF-NAME and DREF-NAME.")

(define-glossary-term @locative (:title "locative")
  "Locatives specify a _type_ of definition such as
  [FUNCTION][locative] or [VARIABLE][locative]. Together with @NAMEs,
  they form @REFERENCEs.

  In their compound form, locatives may have arguments (see
  LOCATIVE-ARGS) as in `(METHOD (NUMBER))`. In fact, their atomic form
  is shorthand for the common no-argument case: that is, FUNCTION is
  equivalent to `(FUNCTION)`.

  A locative is valid if it names an existing @LOCATIVE-TYPE and its
  LOCATIVE-ARGS match that type's lambda-list (see
  DEFINE-LOCATIVE-TYPE).

  ```cl-transcript
  (arglist (dref 'method 'locative))
  => (&REST QUALIFIERS-AND-SPECIALIZERS)
  => :DESTRUCTURING
  ```

  See XREF-LOCATIVE and DREF-LOCATIVE.")

(define-glossary-term @locative-type (:title "locative type")
  "The locative type is the part of a @LOCATIVE that identifies
  what kind definition is being referred to. This is always a symbol.

  Locative types are defined with DEFINE-LOCATIVE-TYPE or
  DEFINE-PSEUDO-LOCATIVE-TYPE. See @BASIC-LOCATIVE-TYPES for the list
  locative types built into DRef, and MGL-PAX::@PAX-LOCATIVES for
  those in PAX.

  Also, see LOCATIVE-TYPE, XREF-LOCATIVE-TYPE, DREF-LOCATIVE-TYPE,
  DREF-EXT::@DEFINING-LOCATIVE-TYPES.")

(define-glossary-term @presentation (:title "presentation")
  "@REFERENCEs may have arguments (see
  DREF-EXT::@DEFINING-LOCATIVE-TYPES) that do not affect the behaviour
  of LOCATE and the @BASIC-OPERATIONS, but which may be used for
  other, \"presentation\" purposes. For example, the VARIABLE
  locative's INITFORM argument is used for presentation by
  PAX:DOCUMENT. Presentation arguments are available via
  DREF:DREF-ORIGIN but do not feature in DREF-LOCATIVE to ensure the
  uniqueness of the definition under XREF=.")


(defsection @dtypes (:title "DTYPEs")
  """DTYPEs are to Lisp types what @LOCATIVE-TYPEs are to CLASSes.
  A DTYPE is either

  - a @LOCATIVE-TYPE such as [FUNCTION][locative], [TYPE][locative]
    and [CLHS][locative], or

  - a full @LOCATIVE such as `(METHOD (NUMBER))` and `(CLHS SECTION)`,
    or

  - NIL (the empty DTYPE) and T (that encompasses all
    LISP-LOCATIVE-TYPES), or

  - named with DEFINE-DTYPE (such as PSEUDO and TOP), or

  - a combination of the above with [AND][type], [OR][type] and
    [NOT][type], or

  - a MEMBER form with LOCATEable definitions, or

  - a SATISFIES form with the name of a function that takes a single
    @DEFINITION as its argument.

  DTYPEs are used in DEFINITIONS and DREF-APROPOS to filter the set of
  definitions as in

  ```cl-transcript
  (definitions 'print :dtype '(not unknown))
  ==> (#<DREF PRINT (CLHS FUNCTION)> #<DREF PRINT FUNCTION>)
  ```

  ```cl-transcript
  (dref-apropos "type specifier" :dtype 'pseudo)
  ==> (#<DREF "1.4.4.6" #1=(CLHS SECTION)> #<DREF "1.4.4.6.1" #1#>
  -->  #<DREF "1.4.4.6.2" #1#> #<DREF "1.4.4.6.3" #1#>
  -->  #<DREF "1.4.4.6.4" #1#> #<DREF "4.2.3" #1#>
  -->  #<DREF "atomic type specifier" #2=(CLHS GLOSSARY-TERM)>
  -->  #<DREF "compound type specifier" #2#>
  -->  #<DREF "derived type specifier" #2#> #<DREF "type specifier" #2#>)
  ```
  """
  (define-dtype macro)
  (top dtype)
  (pseudo dtype)
  (dtypep function))


(defsection @listing-definitions (:title "Listing Definitions")
  (definitions function)
  (dref-apropos function)
  (@reverse-definition-order glossary-term)
  (locative-types function)
  (lisp-locative-types function)
  (pseudo-locative-types function)
  (locative-aliases function))

(defvar *locative-types* ())
(defvar *lisp-locative-types* ())
(defvar *pseudo-locative-types* ())
(defvar *locative-aliases* ())

(define-glossary-term @reverse-definition-order
    (:title "reverse definition order")
  "Lists of @LOCATIVE-TYPEs and aliases are sometimes in reverse order
  of the time of their definition. This order is not affected by
  redefinition, regardless of whether it's by DEFINE-LOCATIVE-TYPE,
  DEFINE-PSEUDO-LOCATIVE-TYPE, DEFINE-SYMBOL-LOCATIVE-TYPE or
  DEFINE-LOCATIVE-ALIAS.")

;;; This is only for not losing track of @REVERSE-DEFINITION-ORDER of
;;; locative types and aliases.
(defvar *locative-types-and-aliases* ())

;;; Sort LOCATIVE-TYPES in the reverse order of definition.
(defun order-locative-types (locative-types)
  (remove-if-not (lambda (locative-type)
                   (member locative-type locative-types))
                 *locative-types-and-aliases*))

(defun reorder-locative-types ()
  (macrolet ((reorder (var)
               `(setq ,var (order-locative-types ,var))))
    (reorder *locative-types*)
    (reorder *lisp-locative-types*)
    (reorder *pseudo-locative-types*)
    (reorder *locative-aliases*)))

(defmacro removef (var item)
  `(setq ,var (remove ,item ,var)))

(defun declare-locative-type (locative-type)
  (pushnew locative-type *locative-types-and-aliases*)
  (pushnew locative-type *locative-types*)
  (pushnew locative-type *lisp-locative-types*)
  (removef *pseudo-locative-types* locative-type)
  (removef *locative-aliases* locative-type)
  (reorder-locative-types)
  locative-type)

(defun declare-pseudo-locative-type (locative-type)
  (pushnew locative-type *locative-types-and-aliases*)
  (pushnew locative-type *locative-types*)
  (removef *lisp-locative-types* locative-type)
  (pushnew locative-type *pseudo-locative-types*)
  (removef *locative-aliases* locative-type)
  (reorder-locative-types)
  locative-type)

(defun declare-locative-alias (locative-type)
  (pushnew locative-type *locative-types-and-aliases*)
  (removef *locative-types* locative-type)
  (removef *lisp-locative-types* locative-type)
  (removef *pseudo-locative-types* locative-type)
  (pushnew locative-type *locative-aliases*)
  (reorder-locative-types)
  locative-type)

(defun locative-types ()
  "Return a list of non-[alias][locative-aliases] locative types.
  This is the UNION of LISP-LOCATIVE-TYPES and PSEUDO-LOCATIVE-TYPES,
  which is the set of constituents of the DTYPE TOP.

  This list is in @REVERSE-DEFINITION-ORDER."
  *locative-types*)

(defun lisp-locative-types ()
  "Return the locative types that correspond to Lisp definitions,
  which typically have SOURCE-LOCATION. These are defined with
  DEFINE-LOCATIVE-TYPE and DEFINE-SYMBOL-LOCATIVE-TYPE and are the
  constituents of DTYPE T.

  This list is in @REVERSE-DEFINITION-ORDER."
  *lisp-locative-types*)

(defun pseudo-locative-types ()
  "Return the locative types that correspond to non-Lisp definitions.
  These are the ones defined with DEFINE-PSEUDO-LOCATIVE-TYPE and are
  the constituents of DTYPE PSEUDO.

  This list is in @REVERSE-DEFINITION-ORDER."
  *pseudo-locative-types*)

(defun locative-aliases ()
  "Return the list of locatives aliases, defined with DEFINE-LOCATIVE-ALIAS.

  This list is in @REVERSE-DEFINITION-ORDER."
  *locative-aliases*)

(declaim (inline locative-type-p))
(defun locative-type-p (object)
  (not (null (dref-class object))))

(defun check-locative-type (locative-type)
  (unless (locative-type-p locative-type)
    (invalid-locative-type locative-type)))

(defun invalid-locative-type (locative-type)
  (error "~@<~S is not a valid ~S.~:@>" locative-type '@locative-type))

(declaim (inline lisp-locative-type-p))
(defun lisp-locative-type-p (locative-type)
  (find locative-type *lisp-locative-types*))

(defun check-lisp-locative-type (locative-type)
  (unless (lisp-locative-type-p locative-type)
    (invalid-lisp-locative-type locative-type)))

(defun invalid-lisp-locative-type (locative-type)
  (error "~@<~S is not one of ~S.~:@>" locative-type 'lisp-locative-types))


(defsection @basic-operations (:title "Basic Operations")
  "The following functions take a single argument, which may be a
  [DREF][class], or an object denoting its own definition (see
  LOCATE)."
  (arglist function)
  (docstring function)
  (source-location function))

;;; Evaluate BODY. If its NTH-VALUE is NIL, evaluate it again with OBJ
;;; bound to a RESOLVEd object (if OBJ was a definition) or a
;;; definition (if OBJ was not a definition).
(defmacro nth-value-or-with-obj-or-def ((obj nth-value) &body body)
  (let ((%body (gensym "BODY"))
        (%obj (gensym "OBJ")))
    `(flet ((,%body (,obj) ,@body))
       (let ((,%obj ,obj))
         (nth-value-or* ,nth-value
           (,%body ,%obj)
           (if (typep ,%obj 'dref)
               (handler-case
                   (let ((,%obj (resolve ,%obj)))
                     (,%body ,%obj))
                 (resolve-error ()))
               (let ((,%obj (locate ,%obj nil)))
                 (when ,%obj
                   (,%body ,%obj)))))))))

(declaim (ftype function arglist*))

(defun arglist (object)
  "Return the arglist of the definition of OBJECT or NIL if the
  arglist cannot be determined.

  The second return value indicates whether the arglist has been
  found. As the second return value, :ORDINARY indicates an [ordinary
  lambda list][clhs], :MACRO a [macro lambda list][clhs], :DEFTYPE a
  [deftype lambda list][clhs], and :DESTRUCTURING a [destructuring
  lambda list][clhs]. Other non-NIL values are also allowed.

  ```cl-transcript (:dynenv dref-std-env)
  (arglist #'arglist)
  => (OBJECT)
  => :ORDINARY
  ```
  ```cl-transcript (:dynenv dref-std-env)
  (arglist (dref 'define-locative-type 'macro))
  => (LOCATIVE-TYPE-AND-LAMBDA-LIST LOCATIVE-SUPERTYPES &OPTIONAL
      DOCSTRING DREF-DEFCLASS-FORM)
  => :MACRO
  ```
  ```cl-transcript (:dynenv dref-std-env)
  (arglist (dref 'method 'locative))
  => (&REST QUALIFIERS-AND-SPECIALIZERS)
  => :DESTRUCTURING
  ```

  This function supports [MACROs][locative],
  [COMPILER-MACROs][locative], [SETF][locative] functions,
  [FUNCTIONs][locative], [GENERIC-FUNCTIONs][locative],
  [METHODs][locative], [TYPEs][locative], [LOCATIVEs][locative]. Note
  that ARGLIST depends on the quality of SWANK-BACKEND:ARGLIST. With
  the exception of SBCL, which has perfect support, all Lisp
  implementations have minor omissions:

  - DEFTYPE lambda lists on ABCL, AllegroCL, CLISP, \CCL, CMUCL, ECL;
  - default values in MACRO lambda lists on AllegroCL;
  - various edge cases involving traced functions.

  Can be extended via ARGLIST*"
  (ensure-dref-loaded)
  (nth-value-or-with-obj-or-def (object 1)
    (arglist* object)))

(declaim (ftype function docstring*))

(defun docstring (object)
  "Return the docstring from the definition of OBJECT.
  As the second value, return the *PACKAGE* that was in effect when
  the docstring was installed or NIL if it cannot be determined (this
  is used by PAX:DOCUMENT when PAX::@PARSING the docstring). This
  function is similar in purpose to CL:DOCUMENTATION.

  Note that some locative types such as [ASDF:SYSTEMS][locative] and
  [DECLARATIONs][locative] have no docstrings, and some Lisp
  implementations do not record all docstrings. The following are
  known to be missing:

  - [COMPILER-MACRO][locative] docstrings on ABCL, AllegroCL, \CCL, ECL;
  - [METHOD-COMBINATION][locative] docstrings on ABCL, AllegroCL.

  Can be extended via DOCSTRING*."
  (ensure-dref-loaded)
  (nth-value-or-with-obj-or-def (object 0)
    (docstring* object)))

(declaim (ftype function source-location*)
         (ftype function source-location-p))

(defun source-location (object &key error)
  """Return the Swank source location for the [defining form][clhs]
  of OBJECT.

  The returned Swank location object is to be accessed only through
  the DREF-EXT::@SOURCE-LOCATIONS API or to be passed to e.g Slime's
  `slime-goto-source-location`.

  If no source location was found,

  - if ERROR is NIL, then return NIL;

  - if ERROR is :ERROR, then return a list of the form `(:ERROR
    <ERROR-MESSAGE>)` suitable for `slime-goto-source-location`;

  - if ERROR is T, then signal an ERROR condition with the same error
    message as in the previous case.

  Note that the availability of source location information varies
  greatly across Lisp implementations.

  Can be extended via SOURCE-LOCATION*."""
  (ensure-dref-loaded)
  (let* ((swank-error nil)
         (location (nth-value-or-with-obj-or-def (object 0)
                     (let ((location (source-location* object)))
                       (cond ((swank-error-value-p location)
                              (setq swank-error location)
                              nil)
                             (t
                              location)))))
         (validp (source-location-p location)))
    (assert (or validp swank-error (null location)) ()
            "~@<(~S ~S) return value ~S is invalid.~:@>"
            'source-location* object location)
    (cond (validp
           location)
          ((null error)
           nil)
          ((eq error :error)
           (or swank-error
               `(:error ,(format nil "Source location of ~S not found."
                                 object))))
          (t
           (if swank-error
               (error "~S" swank-error)
               (error "Source location of ~S not found." object))))))

(defun swank-error-value-p (object)
  (and (listp object)
       (eq (first object) :error)))
