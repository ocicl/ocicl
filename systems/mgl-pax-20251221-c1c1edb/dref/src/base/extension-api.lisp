(in-package :dref)

(in-readtable pythonic-string-syntax)

(defsection @extending-dref (:title "Extending DRef")
  (@extension-tutorial section)
  (@locative-type-hierarchy section)
  (@defining-locative-types section)
  (@extending-locate section)
  (@extending-everything-else section)
  (@dref-classes section)
  (@source-locations section))

(defsection @extension-tutorial (:title "Extension Tutorial")
  "Let's see how to tell DRef about new kinds of definitions through
  the example of the implementation of the CLASS locative. Note that
  this is a verbatim [PAX:INCLUDE][locative] of the sources. Please
  ignore any internal machinery. The first step is to define the
  @LOCATIVE-TYPE:"
  (nil (include (:start (class locative)
                 :end (dref::locate* (method (class (eql class)))))
                :header-nl "```" :footer-nl "```"))
  "Then, we make it possible to look up CLASS definitions:"
  (nil (include (:start (dref::locate* (method (class (eql class))))
                 :end (resolve* (method (class-dref))))
                :header-nl "```" :footer-nl "```"))
  "DEFINE-LOCATOR makes `(LOCATE (FIND-CLASS 'DREF))` work, while
  DEFINE-LOOKUP is for `(DREF 'DREF 'CLASS)`. Naturally, for locative
  types that do not define first-class objects, the first method
  cannot be defined.

  Finally, we define a RESOLVE* method to recover the [CLASS][type]
  object from a CLASS-DREF. We also specialize DOCSTRING* and
  SOURCE-LOCATION*:"
  (nil (include (:start (resolve* (method (class-dref)))
                 :end (dref::%end-of-class-example variable))
                :header-nl "```" :footer-nl "```"))
  "We took advantage of having just made the class locative type being
  RESOLVEable, by specializing DOCSTRING* on the CLASS class.
  SOURCE-LOCATION* was specialized on CLASS-DREF to demonstrate how
  this can be done for non-RESOLVEable locative types.

  Classes have no arglist, so no ARGLIST* method is needed. In the
  following, we describe the pieces in detail.")

(defsection @locative-type-hierarchy (:title "Locative Type Hierarchy")
  "[Locative types][@LOCATIVE-TYPE] form their own hierarchy, that
  is only superficially similar to the Lisp CLASS hierarchy.
  [ check-lisp-and-pseudo-are-distinct function ][docstring]"
  (dref-class function)
  (locative-type-direct-supers function)
  (locative-type-direct-subs function))

(defun check-lisp-and-pseudo-are-distinct (pseudop locative-type superclasses)
  "The hierarchies of LISP-LOCATIVE-TYPES and PSEUDO-LOCATIVE-TYPES
  are distinct. That is, the DREF-CLASS of a Lisp locative type must
  not be a subclass of a PSEUDO one, and vice versa. This is enforced
  by DEFINE-LOCATIVE-TYPE and DEFINE-PSEUDO-LOCATIVE-TYPE."
  (dolist (l2 (if pseudop
                  *lisp-locative-types*
                  *pseudo-locative-types*))
    (let* ((d2 (dref-class l2))
           (subclass-superclass (find-subclass-of d2 superclasses)))
      (when subclass-superclass
        (cerror "Continue."
                "~@<~S of ~S with superclasses ~S is illegal ~
                because ~S~? is the ~S of ~S, ~
                one of ~S.~:@>"
                (if pseudop
                    'define-pseudo-locative-type
                    'define-locative-type)
                locative-type superclasses
                subclass-superclass
                (if (eq subclass-superclass d2) "" " is a subclass of ~S")
                (if (eq subclass-superclass d2) () `(,d2))
                'dref-class l2
                (if pseudop
                    'lisp-locative-types
                    'pseudo-locative-types))))))

(defun find-subclass-of (class classes)
  (loop for class-1 in classes
        when (subtypep class-1 class)
          return class-1))

(defun %declare-locative-type (pseudop locative-type)
  (if pseudop
      (declare-pseudo-locative-type locative-type)
      (declare-locative-type locative-type)))

;;; LOCATIVE-TYPE -> (DREF-CLASS-NAME SUPER-CLASS-NAMES
;;;                   LOCATIVE-TYPE-DIRECT-SUPERS LOCATIVE-TYPE-DIRECT-SUBS)
;;;
;;; E.g. READER -> (READER-DREF (METHOD-DREF) (METHOD) (ACCESSOR))
(defvar *locative-type-to-class-info* (make-hash-table))

(defvar *dref-class-to-locative-type* (make-hash-table))

(declaim (inline %locative-type-class-info))
(defun %locative-type-class-info (locative-type)
  (gethash locative-type *locative-type-to-class-info*))

(defun dref-class (locative-type)
  "Return the name of the CLASS used to represent @DEFINITIONs with
  LOCATIVE-TYPE. This is always a subclass of [DREF][class]. Returns
  NIL if LOCATIVE-TYPE is not a valid locative type.

  Note that the actual TYPE-OF a DREF is mostly intended for
  @EXTENDING-DREF. Hence, it is hidden when a DREF is printed:

  ```cl-transcript (:dynenv dref-std-env)
  (dref 'print 'function)
  ==> #<DREF PRINT FUNCTION>
  (type-of *)
  => FUNCTION-DREF
  ```

  Due to @CANONICALIZATION, the actual type may be a proper subtype of
  DREF-CLASS:

  ```cl-transcript (:dynenv dref-std-env)
  (dref 'documentation 'function)
  ==> #<DREF DOCUMENTATION GENERIC-FUNCTION>
  (type-of *)
  => GENERIC-FUNCTION-DREF
  (subtypep 'generic-function-dref 'function-dref)
  => T
  => T
  ```"
  (first (%locative-type-class-info locative-type)))

(defun dref-class-superclasses (locative-type)
  (second (%locative-type-class-info locative-type)))

(defun locative-type-direct-supers (locative-type)
  "List the @LOCATIVE-TYPEs whose DREF-CLASSes are direct superclasses
  of the DREF-CLASS of LOCATIVE-TYPE. These can be considered
  supertypes of LOCATIVE-TYPE in the sense of DTYPEP.

  This is ordered as in the corresponding definition."
  (third (%locative-type-class-info locative-type)))

(defun locative-type-direct-subs (locative-type)
  "List the @LOCATIVE-TYPEs whose DREF-CLASSes are direct subclasses
  of the DREF-CLASS of LOCATIVE-TYPE. These can be considered subtypes
  of LOCATIVE-TYPE in the sense of DTYPEP.

  This list is in @REVERSE-DEFINITION-ORDER."
  (fourth (%locative-type-class-info locative-type)))

(defun dref-class-to-locative-type (dref-class)
  (gethash (if (symbolp dref-class)
               dref-class
               (class-name dref-class))
           *dref-class-to-locative-type*))

(defun update-class-info (locative-type dref-class superclasses)
  (let ((old-supers (locative-type-direct-supers locative-type))
        (new-supers (%locative-type-direct-supers superclasses))
        (subs (%locative-type-direct-subs locative-type dref-class))
        (info-map *locative-type-to-class-info*))
    ;; Update the subs of deleted supers.
    (dolist (deleted-super (set-difference old-supers new-supers))
      (setf (fourth (gethash deleted-super info-map))
            (order-locative-types
             (remove locative-type (fourth (gethash deleted-super info-map))))))
    ;; Update the subs of newly added supers.
    (dolist (added-super (set-difference new-supers old-supers))
      (setf (fourth (gethash added-super info-map))
            (order-locative-types
             (cons locative-type (fourth (gethash added-super info-map))))))
    ;; Update LOCATIVE-TYPE's class info.
    (setf (gethash locative-type info-map)
          (list dref-class superclasses new-supers
                (order-locative-types subs))))
  (setf (gethash dref-class *dref-class-to-locative-type*) locative-type))

(defun %locative-type-direct-supers (superclasses)
  (remove nil (mapcar #'dref-class-to-locative-type superclasses)))

(defun %locative-type-direct-subs (locative-type dref-class)
  (loop for locative-type-1 being the hash-key in *locative-type-to-class-info*
          using (hash-value info)
        when (and (not (eq locative-type-1 locative-type))
                  (member dref-class (second info)))
          collect locative-type-1))

(defun locative-subtype-p (locative-type-1 locative-type-2)
  (cond ((or (eq locative-type-1 locative-type-2)
             (eq locative-type-1 nil)
             (eq locative-type-2 'top))
         (values t t))
        ((eq locative-type-2 t)
         (when (find locative-type-1 *lisp-locative-types*)
           (values t t)))
        (t
         (let ((class1 (or (dref-class locative-type-1)
                           (invalid-locative-type locative-type-1)))
               (class2 (or (dref-class locative-type-2)
                           (invalid-locative-type locative-type-2))))
           (subtypep class1 class2)))))


(defsection @defining-locative-types (:title "Defining Locative Types")
  (define-locative-type macro)
  (define-pseudo-locative-type macro)
  (define-locative-alias macro)
  (@symbol-locatives section))

(defmacro define-locative-type (locative-type-and-lambda-list
                                locative-supertypes
                                &optional docstring dref-defclass-form)
  """Declare [LOCATIVE-TYPE][argument] as a [LOCATIVE][locative],
  which is the first step in @EXTENDING-DREF.

  - _Simple example_

      To define a locative type called `DUMMY` that takes no arguments
      and is not a locative subtype of any other locative type:

      ```
      (define-locative-type dummy ()
        "Dummy docstring.")
      ```

      With this definition, only the locatives `DUMMY` and its
      equivalent form `(DUMMY)` are valid. The above defines a DREF
      subclass called `DUMMY-DREF` in the current package. All
      definitions with locative type `DUMMY` and its locatives
      subtypes must be instances of `DUMMY-DREF`.

      `(LOCATE 'DUMMY 'LOCATIVE)` refers to this definition. That is,
      ARGLIST, [DOCSTRING][function] and SOURCE-LOCATION all work on
      it.

  - _Complex example_

      `DUMMY` may have arguments `X` and `Y` and inherit from locative
      types `L1` and `L2`:

      ```
      (define-locative-type (dummy x &key y) (l1 l2)
        "Dummy docstring."
        (defclass dummy-dref ()
          ((xxx :initform nil :accessor dummy-xxx))))
      ```

      One may change name of `DUMMY-DREF`, specify superclasses and
      add slots as with DEFCLASS. Behind the scenes, the DREF classes
      of `L1` and `L2` are added automatically to the list of
      superclasses.

  Arguments:

  - The general form of LOCATIVE-TYPE-AND-LAMBDA-LIST
    is (LOCATIVE-TYPE &REST LAMBDA-LIST), where LOCATIVE-TYPE is a
    SYMBOL, and LAMBDA-LIST is a [destructuring lambda list][clhs].
    The LOCATIVE-ARGS of [DREF][class]s with @LOCATIVE-TYPE
    LOCATIVE-TYPE (the argument given to this macro) always conform to
    this lambda list. See CHECK-LOCATIVE-ARGS.

      If LOCATIVE-TYPE-AND-LAMBDA-LIST is a single symbol, then that's
      interpreted as LOCATIVE-TYPE, and LAMBDA-LIST is NIL.

  - LOCATIVE-SUPERTYPES is a list of @LOCATIVE-TYPEs whose
    DREF-CLASSes are added to prepended to the list of superclasses
    this definition.

  Locative types defined with DEFINE-LOCATIVE-TYPE can be listed with
  LISP-LOCATIVE-TYPES."""
  `(%define-locative-type nil ,locative-type-and-lambda-list
                          ,locative-supertypes ,docstring ,dref-defclass-form
                          nil))

(defmacro %define-locative-type (pseudop locative-type-and-lambda-list
                                 locative-supertypes docstring
                                 dref-defclass-form extra-superclasses)
  (destructuring-bind (locative-type &rest lambda-list)
      (ensure-list* locative-type-and-lambda-list)
    (destructuring-bind (&optional dref-defclass dref-class dref-superclasses
                           dref-slots)
        dref-defclass-form
      (declare (ignore dref-class))
      (when (and dref-defclass-form
                 (not (eq dref-defclass 'defclass)))
        (error "~@<When defining locative type ~S, the argument ~S ~
               should start with ~S~:@>"
               locative-type dref-defclass-form 'defclass))
      (let* ((superclasses
               (loop for superlocative in locative-supertypes
                     for dref-class = (dref-class superlocative)
                     do (check-locative-type superlocative)
                     when dref-class
                       collect dref-class))
             (extra-superclasses
               (or extra-superclasses
                   (when (notany (lambda (superclass)
                                   (subtypep superclass 'dref))
                                 superclasses)
                     '(dref))))
             (all-superclasses (append superclasses
                                       dref-superclasses
                                       extra-superclasses))
             (dref-class (maybe-default-dref-class locative-type-and-lambda-list
                                                   dref-defclass-form))
             (%xref (gensym "XREF")))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (check-lisp-and-pseudo-are-distinct ,pseudop ',locative-type
                                               ',all-superclasses)
           (%declare-locative-type ,pseudop ',locative-type)
           (let ((,%xref (xref ',locative-type 'locative)))
             (setf (definition-property ,%xref 'arglist)
                   (list ',lambda-list :destructuring))
             (setf (definition-property ,%xref 'docstring)
                   (list ,docstring ,*package*))
             (setf (definition-property ,%xref 'source-location)
                   (this-source-location)))
           (defclass ,dref-class ,all-superclasses
             ,dref-slots
             (:documentation
              ,(let ((*package* (find-package :cl)))
                 (format nil "~S of [~S][~S]."
                         'dref-class locative-type 'locative))))
           (update-class-info ',locative-type ',dref-class ',superclasses))))))

(defun maybe-default-dref-class (locative-type-and-lambda-list dref-class-def)
  (or (second dref-class-def)
      (intern
       (let ((locative-type (locative-type locative-type-and-lambda-list)))
         (format nil "~A-~A" (symbol-name locative-type) 'dref)))))

(defmacro define-pseudo-locative-type (locative-type-and-lambda-list
                                       locative-supertypes
                                       &optional docstring dref-defclass-form)
  """Like DEFINE-LOCATIVE-TYPE, but declare that
  [LOCATIVE-TYPE][argument] does not correspond to definitions in the
  running Lisp. Definitions with pseudo locatives are of DTYPE PSEUDO
  and are not listed by default by DEFINITIONS.

  Locative types defined with DEFINE-PSEUDO-LOCATIVE-TYPE can be
  listed with PSEUDO-LOCATIVE-TYPES."""
  `(%define-locative-type t ,locative-type-and-lambda-list ,locative-supertypes
                          ,docstring ,dref-defclass-form nil))

(defmacro check-locative-args (locative-type locative-args)
  "Signal a LOCATE-ERROR condition if LOCATIVE-ARGS do not match the
  LAMBDA-LIST argument of LOCATIVE-TYPE (not evaluated)."
  (let ((lambda-list (first (definition-property (xref locative-type 'locative)
                                                 'arglist))))
    `(unless (ignore-errors
              (destructuring-bind ,lambda-list ,locative-args
                (declare (ignore ,@(macro-arg-names lambda-list)))
                t))
       (locate-error "Bad arguments ~S for locative ~S with lambda list ~S."
                     ,locative-args ',locative-type ',lambda-list))))

;;; Same as CHECK-LOCATIVE-ARGS, but LOCATIVE-TYPE is evaluated and it
;;; signals a SIMPLE-ERROR.
(defun check-locative-args* (locative-type locative-args)
  (let ((lambda-list (first (definition-property (xref locative-type 'locative)
                                                 'arglist))))
    (unless (ignore-errors
             (handler-bind ((warning #'muffle-warning))
               (eval `(destructuring-bind ,lambda-list ',locative-args
                        (declare (ignore ,@(macro-arg-names lambda-list)))
                        t))))
      (error "~@<Bad arguments ~S for locative ~S with lambda list ~S.~:@>"
             locative-args locative-type lambda-list))))

(defmacro define-locative-alias (alias locative-type &body docstring)
  """Define ALIAS that can be substituted for LOCATIVE-TYPE (both
  SYMBOLs) for the purposes of LOCATEing. LOCATIVE-TYPE must
  exist (i.e. be among LOCATIVE-TYPES). For example, let's define
  OBJECT as an alias of the CLASS locative:

  ```
  (define-locative-alias object class)
  ```

  Then, LOCATEing with OBJECT will find the CLASS:

  ```
  (dref 'xref 'object)
  ==> #<DREF XREF CLASS>
  ```

  The LOCATIVE-ARGS of OBJECT (none in the above) are passed on to
  CLASS.

  ```
  (arglist (dref 'object 'locative))
  => (&REST ARGS)
  => :DESTRUCTURING
  ```

  Note that LOCATIVE-ALIASES are not LOCATIVE-TYPES and are not valid
  DTYPES.

  Also, see PAX::@LOCATIVE-ALIASES in PAX."""
  (check-docstring-only-body docstring)
  (let ((%xref (gensym "XREF")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,%xref (xref ',alias 'locative)))
         (setf (definition-property ,%xref 'arglist)
               (list '(&rest args) :destructuring))
         (setf (definition-property ,%xref 'docstring)
               (list ,(first docstring) ,*package*))
         (setf (definition-property ,%xref 'source-location)
               (this-source-location)))
       (defmethod dref* (name (locative-type (eql ',alias)) locative-args)
         (dref* name ',locative-type locative-args))
       (declare-locative-alias ',alias))))

(defun check-docstring-only-body (body)
  (assert (or (endp body)
              (and (= (length body) 1)
                   (stringp (first body))))
          () "BODY must be () or a single docstring."))


;;;; LOCATE's low-level implementation, most importantly the extension
;;;; points LOCATE* and DREF*, on top of which the public API rests

(defsection @extending-locate (:title "Extending LOCATE")
  "[ dref::%locate function][docstring]"
  (@initial-definition section)
  (@canonicalization section)
  (@defining-lookups-locators-and-casts section))

(defvar *locating-object*)

(defun %locate (object)
  "Internally, LOCATE finds an initial [DREF][class] of its OBJECT
  argument with a [lookup][define-lookup] or with a
  [locator][define-locator]. This initial DREF is then canonicalized
  with a series of [casts][define-cast]. In more detail, the process
  is as follows.

  - If the OBJECT argument of LOCATE is a DREF, then it is returned
    without processing.

  Else, LOCATE first needs to finds the initial definition."
  (let* ((*locating-object* object)
         (dref (locate-initial-definition object)))
    (declare (type dref dref))
    (unless (typep object 'dref)
      (setf (slot-value dref 'origin) object))
    (canonicalize-dref dref)))

(defsection @initial-definition (:title "Initial Definition")
  "[ dref::locate-initial-definition function][docstring]")

(defun locate-initial-definition (object)
  "LOCATE can find the initial definition in one of two ways:

  [ locate-with-lookup function][docstring]

  [ locate-with-first-locator function][docstring]"
  (if (typep object 'xref)
      (locate-with-lookup object)
      (or (locate-with-first-locator object)
          (locate-error))))

(defun locate-with-lookup (xref)
  "- _With direct
  lookup_

      If OBJECT is an XREF, then the [lookup][define-lookup]
      for (XREF-LOCATIVE-TYPE OBJECT) is invoked. For an XREF with the
      locative `(METHOD (NUMBER))`, this would be the lookup
      defined as

      ```
      (define-lookup method (name locative-args) ...)
      ```"
  ;; DEFINE-LOOKUP currently boils down to defining a DREF* method.
  (dref* (xref-name xref)
         (xref-locative-type xref)
         (xref-locative-args xref)))

(defun locate-with-first-locator (object)
  "- _With locator
  search_

      Else, OBJECT is a normal Lisp object, such as a [METHOD][class]
      object from FIND-METHOD. The first of LISP-LOCATIVE-TYPES whose
      [locator][ define-locator] succeeds provides the initial
      definition, which may be defined like this:

      ```
      (define-locator method ((obj method)) ...)
      ```

      This is a locator that returns definitions with the METHOD
      locative type and takes an argument named `OBJ` of class
      METHOD (which is like a specializer in DEFMETHOD).

      - LISP-LOCATIVE-TYPES are tried one by one in the order
        specified there.

      - For a given locative type, if there are multiple locators,
        standard CLOS method selection applies."
  (assert (not (typep object 'xref)))
  (let ((last-locate-error nil))
    (dolist (locative-type *lisp-locative-types*)
      (handler-case
          ;; Equivalent to CALL-LOCATOR
          (let ((dref (locate* object locative-type)))
            (when dref
              (return-from locate-with-first-locator dref)))
        (locate-error (e)
          (setq last-locate-error e))))
    (when last-locate-error
      (error last-locate-error))))

(defsection @canonicalization (:title "Canonicalization")
  "[ dref::canonicalize-dref function][docstring]"
  (@default-downcast section)
  (@cast-name-change section))

(defun canonicalize-dref (dref)
  "The initial definition thus found is then canonicalized so that
  there is a unique @DEFINITION under XREF=:

  ```
  (locate #'arglist*)
  ==> #<DREF ARGLIST* GENERIC-FUNCTION>
  (dref 'arglist* 'function)
  ==> #<DREF ARGLIST* GENERIC-FUNCTION>
  (dref 'arglist* 'generic-function)
  ==> #<DREF ARGLIST* GENERIC-FUNCTION>
  ```

  Canonicalization is performed by recursively attempting to
  [downcast][define-cast] the current definition to one of its
  LOCATIVE-TYPE-DIRECT-SUBS in a depth-first manner, backtracking if a
  cast fails."
  (labels ((done (d)
             (unless (eq d dref)
               (setf (slot-value d 'origin) (dref-origin dref)))
             (return-from canonicalize-dref d))
           (descend (d)
             (let ((subs (locative-type-direct-subs (dref-locative-type d))))
               (dolist (sub subs)
                 (let ((d (locate* d sub)))
                   (when d
                     (descend d))))
               (done d))))
    (descend dref)))

(defvar *check-locate* nil
  "Enable runtime verification of invariants during LOCATE calls.
  This carries a performance penalty and is intended for testing and
  debugging.

  In particular, enforce the rule of @CAST-NAME-CHANGE and that [
  check-locator-return-values function][docstring]")

(defsection @default-downcast (:title "Default Downcast")
  "[ dref::locate* (method (dref t))][docstring]")

;;; Return a @DEFINITION of OBJECT as an instance of the DREF-CLASS of
;;; LOCATIVE-TYPE. This function is for extending LOCATE and is behind
;;; DEFINE-LOCATOR, DEFINE-CAST and DEFINE-LOOKUP. Do not call it
;;; directly.
(defgeneric locate* (object locative-type)
  (:method :around (object locative-type)
    (cond (*check-locate*
           (let ((located (call-next-method)))
             (check-locator-return-values object locative-type located)
             (check-cast-name-change object locative-type located)
             located))
          (t
           (call-next-method))))
  (:method (object locative-type)
    nil)
  (:method ((dref dref) locative-type)
    "Downcasting to [direct locative subtypes][
    locative-type-direct-subs] is performed by default by looking up
    the definition where the locative type is replaced with its sub
    while the name and the locative args remain the same."
    (when (member locative-type (locative-type-direct-subs
                                 (dref-locative-type dref)))
      (with-locate-error-ignored
        (dref* (dref-name dref) locative-type
               (dref-locative-args dref))))))

(defgeneric dref* (name locative-type locative-args)
  (:method :around (name locative-type locative-args)
    (declare (ignorable name locative-type locative-args))
    (let ((located (call-next-method)))
      (assert (typep located 'dref))
      located))
  (:method (name locative-type locative-args)
    (declare (ignorable name locative-type locative-args))
    (if (locative-type-p locative-type)
        (locate-error "~@<~S ~S has no ~S method defined.~:@>"
                      '@locative-type locative-type 'dref*)
        (locate-error "~@<No such ~S as ~S.~:@>"
                      '@locative-type locative-type))))

(defun check-locator-return-values (object locative-type located)
  "[lookups][define-lookup], [locators][define-locator] and
  [casts][define-cast] obey the following:

  - The value returned must be either NIL or a DREF. Alternatively,
    LOCATE-ERROR may be signalled.

  - If a DREF is returned, then its DREF-LOCATIVE-TYPE must be
    LOCATIVE-TYPE, and its class must be the DREF-CLASS of
    LOCATIVE-TYPE.

  - LOCATIVE-ARGS must be congruent with the destructuring lambda list
    in the definition of LOCATIVE-TYPE."
  (when located
    (assert (typep located 'dref) ()
            "~@<A ~A for ~S as ~S returned ~S, ~
            which is not a ~S. See ~S.~:@>"
            (if (typep object 'dref) "cast" "locator")
            object locative-type located
            'dref '*check-locate*)
    (assert (eq (dref-locative-type located) locative-type) ()
            "~@<A ~A for ~S as ~S returned ~S, ~
            whose locative type is not ~S. See ~S.~:@>"
            (if (typep object 'dref) "cast" "locator")
            object locative-type located
            locative-type '*check-locate*)
    (assert (eq (class-name (class-of located)) (dref-class locative-type)) ()
            "~@<A ~A for ~S as ~S returned ~S, ~
            whose class is ~S, but the ~S of ~S is ~S. See ~S.~:@>"
            (if (typep object 'dref) "cast" "locator")
            object locative-type located
            (class-name (class-of located))
            'dref-class locative-type (dref-class locative-type)
            '*check-locate*)
    (check-locative-args* (dref-locative-type located)
                          (dref-locative-args located))))

(defsection @cast-name-change (:title "Cast Name Change")
  "[Casts][define-cast] must be careful about changing DREF-NAME.

  Their DREF argument and the [DREF][class] returned must have the
  same DREF-NAME (under EQUAL, see XREF=) or it must be possible to
  upcast the returned value to the DREF argument's DREF-LOCATIVE-TYPE.

  - _Implementation note_

      The purpose of this rule is to allow DTYPEP answer this correctly:

      ```cl-transcript (:dynenv dref-std-env)
      (defclass foo ()
        ((a :accessor foo-a)))
      (dref '(setf foo-a) '(method (t foo)))
      ==> #<DREF FOO-A (ACCESSOR FOO)>
      (dtypep * '(method (t foo)))
      => T
      ;; Internally, DTYPEP upcast #<DREF FOO-A (ACCESSOR FOO)>
      ;; and checks that the locative args of the resulting
      ;; definition match those in (METHOD (T FOO)).
      (locate* ** 'method)
      ==> #<DREF (SETF FOO-A) (METHOD (T FOO))>
      ```

      For even more background, also note that if the name remains the
      same but locative args change, then DTYPEP can simply check with
      [DREF][function] if there is a definition of the name with the
      given locative:

      ```cl-transcript (:dynenv dref-std-env)
      (defclass foo ()
        ((r :reader foo-r)))
      (dref 'foo-r '(reader foo))
      ==> #<DREF FOO-R (READER FOO)>
      (dtypep * '(method (foo)))
      => T
      ;; Behind the scenes, DTYPEP does this:
      (xref= ** (dref 'foo-r '(method (foo))))
      => T
      ```")

(defun check-cast-name-change (object locative-type located)
  (when (and located
             (typep object 'dref)
             ;; downcast
             (find locative-type (locative-type-direct-subs
                                  (dref-locative-type object)))
             (not (equal (dref-name located) (dref-name object))))
    (assert (locate* located (dref-locative-type object)) ()
            "~@<~S was cast to ~S, which is in violation of ~S.~:@>"
            object located '@cast-name-change)))


;;;; Public macros to define LOCATE* methods

(defsection @defining-lookups-locators-and-casts
    (:title "Defining Lookups, Locators and Casts")
  "As we have seen, the @INITIAL-DEFINITION is provided either by a
  lookup or a locator, then @CANONICALIZATION works with
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

;;; To speed LOCATE up, when we know that the actual condition object
;;; does not matter (because *IGNORE-LOCATE-ERROR* is true), use this
;;; premade one.
(defvar *dummy-locate-error*
  (make-condition 'locate-error :object nil :message "" :message-args nil))

(defun locate-error (&optional format-control &rest format-args)
  "Call this function to signal a LOCATE-ERROR condition from the
  [dynamic extent][clhs] of a LOCATE call, that is, from the `BODY`s
  of DEFINE-LOOKUP, DEFINE-LOCATOR and DEFINE-CAST. It is an error to
  call LOCATE-ERROR elsewhere.

  FORMAT-CONTROL, if non-NIL, is a [format control][clhs] for which
  FORMAT-ARGS are suitable."
  (if *locate-error-ignored*
      (error *dummy-locate-error*)
      (error 'locate-error
             :object *locating-object*
             :message (or format-control "")
             :message-args format-args)))

(defmacro define-lookup (locative-type (name locative-args) &body body)
  "Define a method of looking up @DEFINITIONs of LOCATIVE-TYPE
  with the given LOCATIVE-ARGS. Lookups are invoked by LOCATE when its
  OBJECT argument is an XREF with LOCATIVE-TYPE but it is not a DREF,
  as in the case of `(DREF 'PRINT 'FUNCTION)`. When called, the
  variables NAME and LOCATIVE-ARGS are bound to XREF-NAME and
  XREF-LOCATIVE-ARGS of the XREF. LOCATIVE-ARGS is validated with
  CHECK-LOCATIVE-ARGS before BODY is evaluated.

  ```
  (define-lookup variable (name locative-args)
    (unless (special-variable-name-p name)
      (locate-error))
    (make-instance 'variable-dref :name name :locative 'variable))
  ```

  - LOCATIVE-TYPE is a valid @LOCATIVE-TYPE.

  - NAME and LOCATIVE-ARGS are both SYMBOLs.

  The above are enforced at macro-expansion time.

  - BODY must follow the rules in *CHECK-LOCATE*."
  (check-locative-type locative-type)
  (check-type name symbol)
  (check-type locative-args symbol)
  (multiple-value-bind (declarations body) (parse-body-declare body)
    `(defmethod dref* (,name (,(gensym) (eql ',locative-type)) ,locative-args)
       ,@declarations
       (check-locative-args ,locative-type ,locative-args)
       ,@body)))

(defmacro call-lookup (name locative-type locative-args)
  "Call the [lookup][DEFINE-LOOKUP] for LOCATIVE-TYPE with NAME
  and LOCATIVE-ARGS."
  `(dref* ,name ,locative-type ,locative-args))

(defmacro define-locator (locative-type ((object class)) &body body)
  "Define a method of finding the @DEFINITION with LOCATIVE-TYPE of
  instances of CLASS. When a locator's BODY is evaluated, OBJECT is
  bound to such an instance.

  ```
  (define-locator class ((class class))
    (make-instance 'class-dref :name (class-name class) :locative 'class))
  ```

  - LOCATIVE-TYPE is one of LISP-LOCATIVE-TYPES. This is because
    PSEUDO-LOCATIVE-TYPES never RESOLVE to first-class objects.

  - OBJECT is a SYMBOL.

  - CLASS names a [CLASS][class] that is not a subtype of
    [XREF][class]. For how to convert definitions from one locative
    type to another, see DEFINE-CAST.

  The above are enforced at macro-expansion time.

  - BODY must follow the rules in *CHECK-LOCATE*.

  In contrast to when the @INITIAL-DEFINITION is created from an
  XREF (see DEFINE-LOOKUP), here LOCATIVE-ARGS are determined from
  OBJECT."
  (check-locative-type locative-type)
  (check-type object symbol)
  (check-type class symbol)
  (assert (find-class class nil) () "~@<~S does not name a class.~:@>" class)
  (assert (not (subtypep class 'xref)) ()
          "~@<~S must not be a subtype of ~S.~:@>" class 'xref)
  `(defmethod locate* ((,object ,class) (locative-type (eql ',locative-type)))
     ,@body))

(defmacro call-locator (object locative-type)
  "Call the [locator][DEFINE-LOCATOR] for LOCATIVE-TYPE with OBJECT."
  `(locate* ,object ,locative-type))

(defmacro define-cast (locative-type ((dref dref-class)) &body body)
  "Define a method of converting a @DEFINITION to another
  with LOCATIVE-TYPE. When a cast's BODY is evaluated, DREF is bound
  to an instance DREF-CLASS, which denotes a valid but potentially
  [non-canonical][ @canonicalization] definition.

  Note the @DEFAULT-DOWNCAST often suffices, and defining a cast is
  only necessary if the [name][ @cast-name-change] or the locative
  args change:

  ```
  (define-cast accessor ((dref reader-dref))
    (let ((name (dref-name dref))
          (class (second (dref-locative dref))))
      (when (ignore-errors (find-accessor-slot-definition name class))
        (make-instance 'accessor-dref :name name
                        :locative `(accessor ,class)))))
  ```

  - LOCATIVE-TYPE is a valid @LOCATIVE-TYPE.

  - If LOCATIVE-TYPE is one of PSEUDO-LOCATIVE-TYPES, then DREF-CLASS
    must be of another pseudo locative type.

  - DREF-CLASS is either a direct _downcast_ or an potentially
    non-direct _upcast_.

      - _Downcast:_ In this case, LOCATIVE-TYPE is one of
        LOCATIVE-TYPE-DIRECT-SUBS of (DREF-CLASS-TO-LOCATIVE-TYPE
        DREF-CLASS).

          Downcasting to non-direct subtypes is done in multiple
          steps. Consequently,the BODY of a downcast can rely on
          (CLASS-OF DREF) being CLASS, not any subclass thereof.

      - _Upcast:_ LOCATIVE-TYPE is different but reachable
        from (DREF-CLASS-TO-LOCATIVE-TYPE DREF-CLASS) by repeatedly
        choosing one of LOCATIVE-TYPE-DIRECT-SUPERS. Upcasting to
        non-direct supertypes is done in one step.

  The above are enforced at macro-expansion time.

  - BODY must follow the rules in *CHECK-LOCATE*, including those in
    @CAST-NAME-CHANGE."
  (check-type dref symbol)
  (check-type dref-class symbol)
  (let ((type-locative-type (dref-class-to-locative-type dref-class)))
    (assert (locative-type-p locative-type) ()
            "~@<~S: ~S is not a ~S.~:@>"
            'define-cast locative-type 'locative-type-p)
    (when (member locative-type (pseudo-locative-types))
      (assert (and (not (eq type-locative-type locative-type))
                   (member type-locative-type (pseudo-locative-types))) ()
                   "~@<~S: Casting to pseudo locative type ~S ~
                   from non-pseudo ~S is not legal.~:@>"
                   'define-cast locative-type
                   (or type-locative-type dref-class)))
    (when (and type-locative-type
               (not (or (member type-locative-type
                                (locative-type-direct-subs locative-type))
                        (and (not (eq type-locative-type locative-type))
                             (locative-subtype-p locative-type
                                                 type-locative-type)))))
      (assert nil () "~@<~S: ~S is neither a direct locative subtype ~
                     nor a locative supertype of ~S.~:@>"
              'define-cast type-locative-type locative-type)))
  `(defmethod locate* ((,dref ,dref-class)
                       (locative-type (eql ',locative-type)))
     ,@body))

(defmacro call-cast (locative-type dref)
  "Call the [cast][DEFINE-CAST] to LOCATIVE-TYPE with DREF."
  `(locate* ,dref ,locative-type))


(defsection @extending-everything-else (:title "Extending Everything Else")
  (resolve* generic-function)
  (resolve-error function)
  (map-definitions-of-name generic-function)
  (map-definitions-of-type generic-function)
  (arglist* generic-function)
  (docstring* generic-function)
  (source-location* generic-function)
  (@definition-properties section))

(defvar *resolving-dref*)

(defgeneric resolve* (dref)
  (:documentation "Return the object defined by the definition DREF
  refers to. Signal a RESOLVE-ERROR condition by calling the
  RESOLVE-ERROR function if the lookup fails.

  To keep RESOLVE a partial inverse of LOCATE, DEFINE-LOCATOR may be
  necessary for RESOLVEable definitions. This function is for
  extending RESOLVE. Do not call it directly.

  It is an error for methods of this generic function to return an
  [XREF][class].")
  (:method :around ((dref dref))
    (let* ((*resolving-dref* dref)
           (resolved (call-next-method)))
      (assert (not (typep resolved 'xref)))
      resolved))
  (:method ((dref dref))
    (resolve-error)))

(defun resolve-error (&rest format-and-args)
  "Call this function to signal a RESOLVE-ERROR condition from the
  [dynamic extent][clhs] of a RESOLVE* method. It is an error to call
  RESOLVE-ERROR elsewhere.

  FORMAT-AND-ARGS, if non-NIL, is a format string and arguments
  suitable for FORMAT."
  (error 'resolve-error
         :dref *resolving-dref*
         :message (if format-and-args
                      (apply #'format nil format-and-args)
                      nil)))

(defgeneric map-definitions-of-name (fn name locative-type)
  (:documentation "Call FN with [DREF][class]s which can be LOCATEd
  with an XREF with NAME, LOCATIVE-TYPE and some LOCATIVE-ARGS. The
  strange wording here is because there may be multiple ways (and thus
  XREFs) that refer to the same definition.

  For most locative types, there is at most one such definition, but
  for METHOD, for example, there may be many. The default method
  simply does `(DREF NAME LOCATIVE-TYPE NIL)` and calls FN with result
  if [DREF][function] succeeds.

  FN must not be called with the same (under XREF=) definition
  multiple times.

  This function is for extending DEFINITIONS and DREF-APROPOS. Do not
  call it directly.")
  ;; See DEFINITIONS for how the efficiency hack of returning the
  ;; magic symbol SWANK-DEFINITIONS instead of mapping with FN works.
  (:method (fn name locative-type)
    (let ((located (dref name locative-type nil)))
      (when located
        (funcall fn located)
        (values)))))

(defgeneric map-definitions-of-type (fn locative-type)
  (:documentation "Call FN with [DREF][class]s which can be LOCATEd
  with an XREF with LOCATIVE-TYPE with some NAME and LOCATIVE-ARGS.

  The default method forms XREFs by combining each interned symbol as
  @NAMEs with LOCATIVE-TYPE and no LOCATIVE-ARGS and calls FN if it
  LOCATEs a definition.

  FN may be called with DREFs that are XREF= but differ in the XREF in
  their DREF-ORIGIN.

  This function is for extending DREF-APROPOS. Do not call it
  directly.")
  (:method (fn locative-type)
    (declare (ignore fn locative-type))
    ;; See DREF-APROPOS about the magic symbol TRY-INTERNED-SYMBOLS.
    'try-interned-symbols))

(defun map-names-for-type (fn locative-type)
  ;; This is wasteful in that a DREF is created from an XREF while we
  ;; are only interested in the XREF-NAME.
  (map-definitions-of-type (lambda (dref)
                             (funcall fn (xref-name (dref-origin dref))))
                           locative-type))

(defgeneric arglist* (object)
  (:documentation "To extend ARGLIST, specialize OBJECT on a normal
  Lisp type or on a subclass of [DREF][class].

  ARGLIST first calls ARGLIST* with its OBJECT argument. If that
  doesn't work (i.e. the second value returned is NIL), then it calls
  ARGLIST* with OBJECT either RESOLVEd (if it's a DREF) or LOCATEd (if
  it's not a DREF).

  - The default method returns NIL, NIL.

  - There is also a method specialized on [DREFs][class], that looks
    up the DEFINITION-PROPERTY called ARGLIST and returns its value
    with VALUES-LIST. Thus, an arglist and its kind can be specified
    with something like

      ```
      (setf (definition-property xref 'arglist)
            (list arglist :destructuring))
      ```

  This function is for extension only. Do not call it directly.")
  (:method (object)
    (declare (ignorable object))
    (values nil nil))
  (:method ((dref dref))
    (values-list (definition-property dref 'arglist))))

(defgeneric docstring* (object)
  (:documentation "To extend DOCSTRING, specialize OBJECT on a normal
  Lisp type or on a subclass of [DREF][class].

  DOCSTRING first calls DOCSTRING* with its OBJECT argument. If that
  doesn't work (i.e. NIL is returned), then it calls DOCSTRING* with
  OBJECT either RESOLVEd (if it's a DREF) or LOCATEd (if it's not a
  DREF).

  - The default method returns NIL.

  - There is also a method specialized on [DREFs][class], that looks
    up the DEFINITION-PROPERTY called DOCSTRING and returns its value
    with VALUES-LIST. Thus, a docstring and a package can be specified
    with something like

      ```
      (setf (definition-property xref 'docstring)
            (list docstring *package*))
      ```

  This function is for extension only. Do not call it directly.")
  (:method (object)
    (declare (ignorable object))
    nil)
  (:method ((dref dref))
    (values-list (definition-property dref 'docstring))))

(defgeneric source-location* (object)
  (:documentation "To extend SOURCE-LOCATION, specialize OBJECT on a
  normal Lisp type or on a subclass of [DREF][class].

  SOURCE-LOCATION first calls SOURCE-LOCATION* with its OBJECT
  argument. If that doesn't work (i.e. NIL or `(:ERROR <MESSAGE>)` is
  returned), then it calls SOURCE-LOCATION* with OBJECT either
  RESOLVEd (if it's a DREF) or LOCATEd (if it's not a DREF).

  SOURCE-LOCATION returns the last of the `(:ERROR <MESSAGE>)`s
  encountered or a generic error message if only NILs were returned.

  - The default method returns NIL.

  - There is also a method specialized on [DREFs][class], that looks
    up the DEFINITION-PROPERTY called SOURCE-LOCATION. If present, it
    must be a function of no arguments that returns a source location
    or NIL. Typically, this is set up in the defining macro like this:

      ```
      (setf (definition-property xref 'source-location)
            (this-source-location))
      ```

  This function is for extension only. Do not call it directly.")
  (:method (object)
    (declare (ignorable object))
    nil)
  (:method ((dref dref))
    (let ((obj (definition-property dref 'source-location)))
      (when obj
        (cond ((functionp obj)
               (funcall obj))
              (t
               obj))))))


(defsection @symbol-locatives (:title "Symbol Locatives")
  "Let's see how the opaque DEFINE-SYMBOL-LOCATIVE-TYPE and the
  obscure DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE macros work together
  to simplify the common task of associating definition with a symbol
  in a certain context."
  (define-symbol-locative-type macro)
  (define-definer-for-symbol-locative-type macro))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass symbol-locative-dref (dref) ()
    (:documentation "All @LOCATIVE-TYPEs defined with
    DEFINE-SYMBOL-LOCATIVE-TYPE inherit from this class.")))

(defmacro define-symbol-locative-type
    (locative-type-and-lambda-list locative-supertypes
     &optional docstring dref-class-def)
  """Similar to DEFINE-LOCATIVE-TYPE, but it assumes that all things
  LOCATEable with LOCATIVE-TYPE are going to be symbols defined with a
  definer defined with DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE. Symbol
  locatives are for attaching a definition (along with arglist,
  documentation and source location) to a symbol in a particular
  context. An example will make everything clear:

  ```
  (define-symbol-locative-type direction ()
    "A direction is a symbol.")

  (define-definer-for-symbol-locative-type define-direction direction
    "With DEFINE-DIRECTION, one can document what a symbol means when
    interpreted as a DIRECTION.")

  (define-direction up ()
    "UP is equivalent to a coordinate delta of (0, -1).")
  ```

  After all this, `(DREF 'UP 'DIRECTION)` refers to the
  `DEFINE-DIRECTION` form above.

  The DREF-CLASS of the defined locative type inherits from
  SYMBOL-LOCATIVE-DREF, which may be used for specializing when
  implementing new operations."""
  (let ((locative-type (locative-type locative-type-and-lambda-list))
        (dref-class (maybe-default-dref-class locative-type-and-lambda-list
                                              dref-class-def)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (%define-locative-type nil ,locative-type-and-lambda-list
                              ,locative-supertypes ,docstring ,dref-class-def
                              (symbol-locative-dref))
       (defmethod dref* (symbol (locative-type (eql ',locative-type))
                         locative-args)
         (check-locative-args ,locative-type locative-args)
         ;; There is currently no way to undefine things defined with
         ;; the definer (defined with
         ;; DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE), so checking for
         ;; the properties it sets is fine.
         (unless (definition-properties
                  (xref symbol (cons locative-type locative-args)))
           (locate-error))
         (make-instance ',dref-class
                        :name symbol
                        :locative (cons locative-type locative-args))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun check-body-docstring (docstring)
    (assert (or (endp docstring)
                (and (= 1 (length docstring))
                     (string (first docstring)))))))

(defmacro define-definer-for-symbol-locative-type
    (name locative-type &body docstring)
  "Define a macro with NAME that can be used to attach a lambda list,
  documentation, and source location to a symbol in the context of
  LOCATIVE-TYPE. The defined macro's arglist is `(SYMBOL LAMBDA-LIST
  &OPTIONAL DOCSTRING)`. LOCATIVE-TYPE is assumed to have been defined
  with DEFINE-SYMBOL-LOCATIVE-TYPE."
  (check-body-docstring docstring)
  `(defmacro ,name (symbol lambda-list &body docstring)
     ,@docstring
     `,(expand-define-definer-for-symbol-as-locative-definer-body
        symbol ',locative-type lambda-list (first docstring))))

(defun expand-define-definer-for-symbol-as-locative-definer-body
    (symbol locative-type lambda-list docstring)
  (let ((%xref (gensym "XREF")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,%xref (xref ',symbol ',locative-type)))
         (setf (definition-property ,%xref 'arglist)
               (list ',lambda-list :macro))
         (setf (definition-property ,%xref 'docstring)
               (list ,docstring ,*package*))
         (setf (definition-property ,%xref 'source-location)
               (this-source-location))))))


(defsection @definition-properties (:title "Definition Properties")
  "Arbitrary data may be associated with definitions.
  This mechanism is used by `ARGLIST*`, `DOCSTRING*` and
  `SOURCE-LOCATION*` for easy extension.

  The following functions take an XREF argument and not a DREF to
  allow working with [non-canonical][@canonicalization] or
  non-existent definitions."
  (definition-property function)
  (delete-definition-property function)
  (definition-properties function)
  (delete-definition-properties function)
  (move-definition-properties function))

;;; Map (NAME LOCATIVE) to a list of (PROPERTY-NAME . PROPERTY-VALUE)
;;; elements. For example,
;;;
;;;     (:COMMON-LISP READTABLE) -> ((DOCSTRING "XXX" :CL))
;;;
;;; means that (DREF :COMMON-LISP :READTABLE)'s DOCSTRING property has
;;; the value ("XXX" :CL), which the default DOCSTRING* method uses.
;;;
;;; FIXME: If a definition is no longer, then perhaps its properties
;;; should be automatically deleted. They are just harmless garbage to
;;; be collected at that point.
(defvar *definition-properties* (make-hash-table :test #'equal))

(declaim (inline definition-property-key))
(defun definition-property-key (xref)
  (list (xref-name xref) (xref-locative xref)))

(defun definition-property (xref indicator)
  "Return the value of the property associated with XREF whose name
  is EQL to INDICATOR. The second return value indicates whether the
  property was found. SETFable."
  (let ((entry (assoc indicator (gethash (definition-property-key xref)
                                         *definition-properties*))))
    (if entry
        (values (cdr entry) t)
        (values nil nil))))

(defun set-definition-property (xref indicator value)
  (let* ((key (definition-property-key xref))
         (entry (assoc indicator (gethash key *definition-properties*))))
    (if entry
        (setf (cdr entry) value)
        (push (cons indicator value) (gethash key *definition-properties*))))
  value)

(defsetf definition-property set-definition-property)

(defun delete-definition-property (xref indicator)
  "Delete the property associated with XREF whose name is EQL to INDICATOR.
  Return true if the property was found."
  (let* ((key (definition-property-key xref))
         (entry (assoc indicator (gethash key *definition-properties*))))
    (when entry
      (setf (gethash key *definition-properties*)
            (remove entry (gethash key *definition-properties*)))
      t)))

(defun move-definition-properties (from-xref to-xref)
  "Associate all properties of FROM-XREF with TO-XREF, as if readding
  them one-by-one with `(SETF DEFINITION-PROPERTY)`, and
  deleting them from FROM-XREF with DELETE-DEFINITION-PROPERTY."
  (dolist (entry (definition-properties from-xref))
    (destructuring-bind (indicator . value) entry
      (setf (definition-property to-xref indicator) value)))
  (delete-definition-properties from-xref))

(defun delete-definition-properties (xref)
  "Delete all properties associated with XREF."
  (remhash (definition-property-key xref) *definition-properties*))

(defun definition-properties (xref)
  "Return the properties of XREF as an association list."
  (gethash (definition-property-key xref) *definition-properties*))


(declaim (ftype function translate-sb-source-location)
         (ftype function swank-source-location*))

(defmacro this-source-location ()
  "The value of this macro form is a function of no arguments that
  returns its own SOURCE-LOCATION."
  #+sbcl
  `(load-time-value
    (let ((sl
            ;; When evaluating, we have no meaningful source location.
            (when (or *compile-file-truename* *load-truename*)
              (sb-c:source-location))))
      (when sl
        (lambda ()
          (translate-sb-source-location sl)))))
  #-sbcl
  (let ((%dummy (gensym "SOURCE-LOCATION-DUMMY")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,%dummy ())
       (lambda ()
         (swank-source-location* #',%dummy ',%dummy '(function))))))
