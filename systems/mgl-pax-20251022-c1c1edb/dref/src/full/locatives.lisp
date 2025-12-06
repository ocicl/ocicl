(in-package :dref)

(in-readtable pythonic-string-syntax)

;;; This is not a function so that constant CLASS-NAMEs can be
;;; optimized by the compiler.
(defmacro %make-dref (name locative-type &optional locative-args &rest initargs)
  (let ((class (dref-class locative-type)))
    (assert class () "No DREF-CLASS for ~S." locative-type)
    `(make-instance ',class
                    :name ,name
                    :locative (cons ',locative-type ,locative-args)
                    ,@initargs)))


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


;;;; VARIABLE locative

(define-locative-type (variable &optional initform) ()
  """Refers to a global special variable.
  INITFORM, or if not specified, the global value of the variable is
  to be used for @PRESENTATION.

  ```cl-transcript (:dynenv dref-std-env)
  (dref '*print-length* 'variable)
  ==> #<DREF *PRINT-LENGTH* VARIABLE>
  ```

  VARIABLE references do not RESOLVE.""")

(define-lookup variable (name locative-args)
  (unless (special-variable-name-p name)
    (locate-error))
  (%make-dref name variable))

(defmethod map-definitions-of-name (fn name (locative-type (eql 'variable)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod docstring* ((dref variable-dref))
  (documentation* (dref-name dref) 'variable))

(defmethod source-location* ((dref variable-dref))
  (swank-source-location (dref-name dref) 'variable))


;;;; CONSTANT locative

(define-locative-type (constant &optional initform) (variable)
  "Refers to a constant variable defined with DEFCONSTANT. INITFORM,
  or if not specified, the value of the constant is included in the
  documentation. The CONSTANT locative is like the VARIABLE locative,
  but it also checks that its object is CONSTANTP.

  CONSTANT references do not RESOLVE.")

(define-lookup constant (name locative-args)
  (unless (constant-variable-name-p name)
    (locate-error "~S does not name a constant." name))
  (%make-dref name constant))

(defmethod docstring* ((dref constant-dref))
  (documentation* (dref-name dref) 'variable))

(defmethod source-location* ((dref constant-dref))
  (swank-source-location (dref-name dref) 'constant))


(defsection @macrolike-locatives (:title "Locatives for Macros")
  (setf locative)
  (macro locative)
  (symbol-macro locative)
  (compiler-macro locative)
  (setf-compiler-macro locative))


;;;; SETF locative

(define-locative-type (setf) ()
  "Refers to a [setf expander][clhs] (see DEFSETF and DEFINE-SETF-EXPANDER).

  [Setf functions][clhs] (e.g. `(DEFUN (SETF NAME) ...)` or the same
  with DEFGENERIC) are handled by the [SETF-FUNCTION][locative],
  [SETF-GENERIC-FUNCTION][locative], and SETF-METHOD locatives.

  SETF expander references do not RESOLVE.")

(define-lookup setf (symbol locative-args)
  (unless (and (symbolp symbol)
               (or (has-setf-p symbol)
                   ;; KLUDGE: On some implementations HAS-SETF-P may
                   ;; return NIL even though there is a
                   ;; DEFINE-SETF-EXPANDER.
                   (documentation* symbol 'setf)))
    (locate-error "~S does not have a SETF expansion." symbol))
  (%make-dref symbol setf locative-args))

;;; SWANK-BACKEND:FIND-DEFINITIONS does not support setf on CCL.
#-ccl
(defmethod map-definitions-of-name (fn name (locative-type (eql 'setf)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod arglist* ((dref setf-dref))
  #+sbcl
  (let ((info (sb-int:info :setf :expander (dref-name dref))))
    (when info
      (if (functionp info)
          ;; long-form DEFSETF
          (multiple-value-bind (arglist foundp)
              (function-arglist info)
            (when foundp
              (values arglist :ordinary)))
          ;; short-form DEFSETF
          (multiple-value-bind (arglist foundp)
              (handler-case (function-arglist (first info))
                (error ()))
            (when foundp
              (values arglist (if (macro-function (first info))
                                  :macro
                                  :ordinary))))))))

(defmethod docstring* ((dref setf-dref))
  (documentation* (dref-name dref) 'setf))

(defmethod source-location* ((dref setf-dref))
  (swank-source-location (dref-name dref) 'setf))


;;;; MACRO locative

(define-locative-type macro ()
  "Refers to a global macro, typically defined with DEFMACRO, or to a
  [special operator][SPECIAL-OPERATOR-P FUNCTION].

  MACRO references resolve to the MACRO-FUNCTION of their NAME or
  signal RESOLVE-ERROR if that's NIL.")

(define-locator macro ((fn function))
  (let ((name (function-name fn)))
    ;; LOCATE-WITH-FIRST-LOCATOR surfaces only the LOCATE-ERROR of the
    ;; last matching locator, and this is not the last one, so we act
    ;; lazily.
    (when (name-of-macro-p name fn)
      (%make-dref name macro))))

(define-lookup macro (name locative-args)
  (unless (or (name-of-macro-p name)
              (special-operator-p* name))
    (locate-error "~S does not name a macro." name))
  (%make-dref name macro))

(defmethod resolve* ((dref macro-dref))
  (or (macro-function (dref-name dref))
      (resolve-error)))

(defmethod arglist* ((dref macro-dref))
  (nth-value-or* 1
    (when-let (fn (macro-function (dref-name dref)))
      (function-arglist fn :macro))
    (function-arglist (dref-name dref) :macro)
    (values nil nil)))

(defmethod docstring* ((dref macro-dref))
  (documentation* (dref-name dref) 'function))

(defmethod source-location* ((dref macro-dref))
  (let ((symbol (dref-name dref)))
    (when-let (fn (macro-function symbol))
      (swank-source-location* fn symbol 'macro))))


;;;; SYMBOL-MACRO locative

(define-locative-type symbol-macro ()
  """Refers to a global symbol macro, defined with DEFINE-SYMBOL-MACRO.
  Note that since DEFINE-SYMBOL-MACRO does not support docstrings, PAX
  defines methods on the DOCUMENTATION generic function specialized on
  `(DOC-TYPE (EQL 'SYMBOL-MACRO))`.

  ```
  (define-symbol-macro my-mac 42)
  (setf (documentation 'my-mac 'symbol-macro)
        "This is MY-MAC.")
  (documentation 'my-mac 'symbol-macro)
  => "This is MY-MAC."
  ```

  SYMBOL-MACRO references do not RESOLVE.""")

(defvar *symbol-macro-docstrings* (make-hash-table :test #'eq))

(define-lookup symbol-macro (name locative-args)
  (unless (symbol-macro-p name)
    (locate-error "~S does not name a symbol macro." name))
  (%make-dref name symbol-macro))

;;; SWANK-BACKEND:FIND-DEFINITIONS does not support symbol macros on CCL.
#-ccl
(defmethod map-definitions-of-name (fn name (locative-type (eql 'symbol-macro)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod documentation ((symbol symbol) (doc-type (eql 'symbol-macro)))
  (gethash symbol *symbol-macro-docstrings*))

(defmethod (setf documentation) (docstring (symbol symbol)
                                 (doc-type (eql 'symbol-macro)))
  (setf (gethash symbol *symbol-macro-docstrings*) docstring))

(defmethod docstring* ((dref symbol-macro-dref))
  (documentation* (dref-name dref) 'symbol-macro))

(defmethod source-location* ((dref symbol-macro-dref))
  (swank-source-location (dref-name dref) 'symbol-macro))


;;;; Helper for [setf function names][clhs]

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass function-name-mixin ()
    ((function-name :initform nil :initarg :function-name
                    :reader dref-function-name))))

(defmethod initialize-instance :after ((dref function-name-mixin)
                                       &key &allow-other-keys)
  (unless (slot-value dref 'function-name)
    (setf (slot-value dref 'function-name) (dref-name dref))))


;;;; COMPILER-MACRO locative

(define-locative-type compiler-macro ()
  "Refers to a COMPILER-MACRO-FUNCTION, typically defined with
  DEFINE-COMPILER-MACRO."
  (defclass compiler-macro-dref (function-name-mixin)
    ()))

(define-locator compiler-macro ((fn function))
  (let ((name (function-name fn)))
    (when (and name (valid-function-name-p name)
               (eq (compiler-macro-function* name) fn))
      (%make-dref name compiler-macro))))

(define-lookup compiler-macro (name locative-args)
  (unless (and (valid-function-name-p name)
               (compiler-macro-function* name))
    (locate-error "~S does not name a compiler macro." name))
  (%make-dref name compiler-macro))

(defmethod arglist* ((dref compiler-macro-dref))
  (function-arglist (compiler-macro-function* (dref-function-name dref))
                    :macro))

(defmethod docstring* ((dref compiler-macro-dref))
  (documentation* (dref-name dref) 'compiler-macro))

(defmethod source-location* ((dref compiler-macro-dref))
  (let ((name (dref-function-name dref)))
    (swank-source-location* (compiler-macro-function* name) name
                            'compiler-macro)))

(defmethod resolve* ((dref compiler-macro-dref))
  (let* ((name (dref-function-name dref))
         (fn (compiler-macro-function* name)))
    (unless (equal (function-name fn) name)
      (resolve-error "The name of the definition cannot be recovered ~
                      from the function object."))
    fn))


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
  (structure-accessor locative)
  (defstruct* macro))


;;;; FUNCTION locative

(define-locative-type function ()
  "Refers to a global function, typically defined with DEFUN. The
  @NAME must be a [function name][clhs]. It is also allowed to
  reference GENERIC-FUNCTIONs as FUNCTIONs:

  ```cl-transcript (:dynenv dref-std-env)
  (dref 'docstring 'function)
  ==> #<DREF DOCSTRING FUNCTION>
  ```"
  (defclass function-dref (function-name-mixin)
    ()))

(define-locator function ((fn function))
  (let ((name (function-name fn)))
    (unless name
      (locate-error "~S does not have a name." fn))
    (let ((fn1 (ignore-errors (fdefinition* name))))
      (unless fn1
        (locate-error "Function's name ~S does not denote a function." name))
      (unless (eq fn1 (unencapsulated-function fn))
        (locate-error "Function's name ~S denotes a different function ~S"
                      name fn1)))
    (%make-dref name function)))

(define-lookup function (name locative-args)
  (when (or (name-of-macro-p name)
            (special-operator-p* name))
    (locate-error "~S names a macro not a function." name))
  (unless (has-fdefinition-p name)
    (locate-error "~S does not name a function." name))
  (%make-dref name function))

(defmethod arglist* ((dref function-dref))
  (function-arglist (dref-function-name dref) :ordinary))

(defmethod resolve* ((dref function-dref))
  (or (consistent-fdefinition (dref-function-name dref))
      (resolve-error "The name of the definition cannot be recovered ~
                       from the function object.")))

(defmethod docstring* ((dref function-dref))
  (documentation* (fdefinition* (dref-function-name dref)) 'function))

(defmethod source-location* ((dref function-dref))
  (let ((name (dref-function-name dref)))
    (swank-source-location* (fdefinition* name) name 'function)))


;;;; GENERIC-FUNCTION locative

(define-locative-type generic-function (function)
  "Refers to a [GENERIC-FUNCTION][class], typically defined with
  DEFGENERIC. The @NAME must be a [function name][clhs].")

(define-lookup generic-function (name locative-args)
  (let ((fn (ignore-errors (fdefinition* name))))
    (unless (typep fn 'generic-function)
      (locate-error "~S does not name a generic function." name))
    (%make-dref name generic-function)))

(defmethod resolve* ((dref generic-function-dref))
  (fdefinition* (dref-function-name dref)))

(defmethod docstring* ((dref generic-function-dref))
  (documentation* (dref-function-name dref) 'function))

(defmethod source-location* ((dref generic-function-dref))
  (let ((name (dref-function-name dref)))
    (swank-source-location* (fdefinition* name) name 'generic-function)))


;;;; METHOD locative

(define-locative-type (method &rest qualifiers-and-specializers) ()
  "Refers to a METHOD. @NAME must be a [function name][clhs].
  METHOD-QUALIFIERS-AND-SPECIALIZERS has the form

      (<QUALIFIER>* <SPECIALIZERS>)

  For example, the method

  ```cl-transcript (:dynenv dref-std-env)
  (defgeneric foo-gf (x y z)
    (:method :around (x (y (eql 'xxx)) (z string))
      (values x y z)))
  ```

  can be referred to as

  ```cl-transcript (:dynenv dref-std-env)
  (dref 'foo-gf '(method :around (t (eql xxx) string)))
  ==> #<DREF FOO-GF (METHOD :AROUND (T (EQL XXX) STRING))>
  ```

  METHOD is not EXPORTABLE-LOCATIVE-TYPE-P."
  (defclass method-dref (function-name-mixin)
    ()))

(define-locator method ((method method))
  (let ((name (swank-mop:generic-function-name
               (swank-mop:method-generic-function method)))
        (qualifiers (swank-mop:method-qualifiers method))
        (specializers (method-specializers-list method)))
    (%make-dref name method `(,@qualifiers ,specializers))))

(defun method-locative-specializer-and-qualifiers (locative-args)
  (values (butlast locative-args)
          (first (last locative-args))))

(define-lookup method (name locative-args)
  (unless (and locative-args (listp (first (last locative-args))))
    (locate-error "Bad arguments ~S for ~S locative: it should have ~
                  the form (<QUALIFIER>* <SPECIALIZERS>)."
                  locative-args 'method))
  (dolist (qualifier (butlast locative-args))
    (when (listp qualifier)
      (locate-error "Bad arguments ~S for ~S locative: ~
                    qualifiers cannot be lists."
                    locative-args 'method)))
  (multiple-value-bind (qualifiers specializers)
      (method-locative-specializer-and-qualifiers locative-args)
    (or (ignore-errors (find-method* name qualifiers specializers))
        (locate-error "Method does not exist.")))
  (%make-dref name method locative-args))

(defmethod map-definitions-of-name (fn name (locative-type (eql 'method)))
  (declare (ignore fn name))
  'swank-definitions)

;;; Return the specializers in a format suitable as the second
;;; argument to FIND-METHOD.
(defun method-specializers-list (method)
  (mapcar (lambda (spec)
            (typecase spec
              (swank-mop:eql-specializer
               `(eql ,(swank-mop:eql-specializer-object spec)))
              (t (swank-mop:class-name spec))))
          (swank-mop:method-specializers method)))

(defmethod resolve* ((dref method-dref))
  (multiple-value-bind (qualifiers specializers)
      (method-locative-specializer-and-qualifiers (dref-locative-args dref))
    (or (ignore-errors (find-method* (dref-function-name dref)
                                     qualifiers specializers))
        (resolve-error "Method does not exist."))))

(defmethod arglist* ((dref method-dref))
  (values (method-arglist (resolve dref)) :ordinary))

(defmethod docstring* ((dref method-dref))
  (documentation* (resolve dref) t))

(defmethod source-location* ((dref method-dref))
  (swank-source-location* (resolve dref)
                          (dref-function-name dref)
                          `(method ,@(dref-locative-args dref))))


;;;; SETF-COMPILER-MACRO locative

(define-locative-type setf-compiler-macro (compiler-macro)
  "Refers to a compiler macro with a [setf function name][clhs].

  SETF-COMPILER-MACRO references do not RESOLVE.")

(define-lookup setf-compiler-macro (name locative-args)
  (when-let (dref (call-lookup `(setf ,name) 'compiler-macro ()))
    (call-cast 'setf-compiler-macro dref)))

(define-cast setf-compiler-macro ((dref compiler-macro-dref))
  (let ((name (dref-name dref)))
    (when (setf-name-p name)
      ;; @CAST-NAME-CHANGE
      (%make-dref (second name) setf-compiler-macro () :function-name name))))

;;; Upcast for the @CAST-NAME-CHANGE above
(define-cast compiler-macro ((dref setf-compiler-macro-dref))
  (call-lookup `(setf ,(dref-name dref)) 'compiler-macro ()))


;;;; SETF-FUNCTION locative

(define-locative-type setf-function (function setf)
  "Refers to a global FUNCTION with a [setf function name][clhs].

  ```cl-transcript (:dynenv dref-std-env)
  (defun (setf ooh) ())
  (locate #'(setf ooh))
  ==> #<DREF OOH SETF-FUNCTION>
  (dref 'ooh 'setf-function)
  ==> #<DREF OOH SETF-FUNCTION>
  (dref '(setf ooh) 'function)
  ==> #<DREF OOH SETF-FUNCTION>
  ```")

(define-lookup setf-function (name locative-args)
  (when-let (dref (call-lookup `(setf ,name) 'function ()))
    (call-cast 'setf-function dref)))

(define-cast setf-function ((dref function-dref))
  (let ((name (dref-name dref)))
    (when (setf-name-p name)
      ;; @CAST-NAME-CHANGE
      (%make-dref (second name) setf-function () :function-name name))))

;;; Upcast for the @CAST-NAME-CHANGE above
(define-cast function ((dref setf-function-dref))
  (call-lookup `(setf ,(dref-name dref)) 'function ()))


;;;; SETF-GENERIC-FUNCTION locative

(define-locative-type setf-generic-function (generic-function setf-function)
  "Refers to a global GENERIC-FUNCTION with a [setf function name][clhs].

  ```cl-transcript (:dynenv dref-std-env)
  (defgeneric (setf oog) ())
  (locate #'(setf oog))
  ==> #<DREF OOG SETF-GENERIC-FUNCTION>
  (dref 'oog 'setf-function)
  ==> #<DREF OOG SETF-GENERIC-FUNCTION>
  (dref '(setf oog) 'function)
  ==> #<DREF OOG SETF-GENERIC-FUNCTION>
  ```")

(define-lookup setf-generic-function (name locative-args)
  (when-let (dref (call-lookup `(setf ,name) 'generic-function ()))
    (call-cast 'setf-generic-function dref)))

(define-cast setf-generic-function ((dref generic-function-dref))
  (let ((name (dref-name dref)))
    (when (setf-name-p name)
      ;; @CAST-NAME-CHANGE
      (%make-dref (second name) setf-generic-function () :function-name name))))

;;; Upcast for the @CAST-NAME-CHANGE above
(define-cast generic-function ((dref setf-generic-function-dref))
  (let ((name (dref-name dref)))
    (call-lookup `(setf ,name) 'generic-function ())))


;;;; SETF-METHOD locative

(define-locative-type (setf-method &rest method-qualifiers-and-specializers)
    (method setf)
  "Refers to a METHOD of a SETF-GENERIC-FUNCTION.

  ```cl-transcript (:dynenv dref-std-env)
  (defgeneric (setf oog) (v)
    (:method ((v string))))
  (locate (find-method #'(setf oog) () (list (find-class 'string))))
  ==> #<DREF OOG (SETF-METHOD (STRING))>
  (dref 'oog '(setf-method (string)))
  ==> #<DREF OOG (SETF-METHOD (STRING))>
  (dref '(setf oog) '(method (string)))
  ==> #<DREF OOG (SETF-METHOD (STRING))>
  ```")

(define-lookup setf-method (name locative-args)
  (when-let (dref (call-lookup `(setf ,name) 'method locative-args))
    (call-cast 'setf-method dref)))

(define-cast setf-method ((dref method-dref))
  (let ((name (dref-name dref)))
    (when (setf-name-p name)
      ;; @CAST-NAME-CHANGE
      (%make-dref (second name) setf-method (dref-locative-args dref)
                  :function-name name))))

;;; Upcast for the @CAST-NAME-CHANGE above
(define-cast method ((dref setf-method-dref))
  (call-lookup `(setf ,(dref-name dref)) 'method (dref-locative-args dref)))

(defmethod map-definitions-of-name (fn name (locative-type (eql 'setf-method)))
  (declare (ignore fn))
  (if (setf-name-p name)
      'swank-definitions
      (values 'swank-definitions `(setf ,name))))


;;;; METHOD-COMBINATION locative

(define-locative-type method-combination ()
  "Refers to a [METHOD-COMBINATION][class], defined with
  DEFINE-METHOD-COMBINATION.

  METHOD-COMBINATION references do not RESOLVE.")

(define-lookup method-combination (name locative-args)
  (unless (and (symbolp name)
               ;; FIXME
               #+ccl (ccl::method-combination-info name)
               #+sbcl (gethash name sb-pcl::**method-combinations**))
    (locate-error))
  (%make-dref name method-combination))

(defmethod map-definitions-of-name
    (fn name (locative-type (eql 'method-combination)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod docstring* ((dref method-combination-dref))
  (documentation* (dref-name dref) 'method-combination))

(defmethod source-location* ((dref method-combination-dref))
  (swank-source-location (dref-name dref) 'method-combination))


;;;; READER locative

(define-locative-type (reader class-name) (method)
  "Refers to a :READER method in a DEFCLASS:

  ```cl-transcript (:dynenv dref-std-env)
  (defclass foo ()
    ((xxx :reader foo-xxx)))

  (dref 'foo-xxx '(reader foo))
  ==> #<DREF FOO-XXX (READER FOO)>
  ```")

(define-cast reader ((dref method-dref))
  (let ((name (dref-name dref)))
    (multiple-value-bind (qualifiers specializers)
        (method-locative-specializer-and-qualifiers (dref-locative-args dref))
      (when (and (endp qualifiers)
                 (= (length specializers) 1)
                 (ignore-errors
                  (find-reader-slot-definition name (first specializers))))
        (%make-dref name reader `(,(first specializers)))))))

(define-lookup reader (name locative-args)
  (unless (ignore-errors
           (find-reader-slot-definition name (first locative-args)))
    (locate-error))
  (%make-dref name reader locative-args))

(defun find-reader-slot-definition (reader-symbol class-symbol)
  (when (class-slots-supported-p class-symbol)
    (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
      (when (find reader-symbol (swank-mop:slot-definition-readers slot-def))
        (return-from find-reader-slot-definition slot-def)))
    (locate-error "Could not find reader ~S for class ~S." reader-symbol
                  class-symbol)))

(defun class-slots-supported-p (class)
  #-cmucl (declare (ignore class))
  #-cmucl t
  #+cmucl (not (subtypep class 'condition)))

(defmethod map-definitions-of-name (fn name (locative-type (eql 'reader)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod resolve* ((dref reader-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (find-method* symbol () (list (first locative-args)))))

(defmethod docstring* ((dref reader-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (swank-mop:slot-definition-documentation
     (find-reader-slot-definition symbol (first locative-args)))))

(defmethod source-location* ((dref reader-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (swank-source-location* (find-method* symbol ()
                                          (list (first locative-args)))
                            symbol `(reader ,(first locative-args)))))


;;;; WRITER locative

(define-locative-type (writer class-name) (method)
  "Like [ACCESSOR][locative], but refers to a :WRITER method in a DEFCLASS.")

(define-cast writer ((dref method-dref))
  (let ((name (dref-name dref)))
    (multiple-value-bind (qualifiers specializers)
        (method-locative-specializer-and-qualifiers (dref-locative-args dref))
      (when (and (endp qualifiers)
                 (= (length specializers) 2)
                 (eq (first specializers) t))
        (let ((name (if (setf-name-p name)
                        ;; @CAST-NAME-CHANGE
                        (second name)
                        name))
              (class (second specializers)))
          (when (ignore-errors (find-writer-slot-definition name class))
            (%make-dref name writer `(,class))))))))

;;; Due to the @CAST-NAME-CHANGE above, we need to define an upcast.
(define-cast method ((dref writer-dref))
  (call-locator (resolve dref) 'method))

(define-lookup writer (name locative-args)
  (unless (ignore-errors
           (find-writer-slot-definition name (first locative-args)))
    (locate-error))
  (%make-dref name writer locative-args))

(defun find-writer-slot-definition (accessor-symbol class-symbol)
  (when (class-slots-supported-p class-symbol)
    (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
      (let ((writers (swank-mop:slot-definition-writers slot-def)))
        (when (or (find accessor-symbol writers)
                  (find `(setf ,accessor-symbol) writers :test #'equal))
          (return-from find-writer-slot-definition slot-def))))
    (locate-error "Could not find writer ~S for class ~S."
                  accessor-symbol class-symbol)))

(defmethod map-definitions-of-name (fn name (locative-type (eql 'writer)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod resolve* ((dref writer-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (or (ignore-errors
         (find-method* symbol () (list t (first locative-args))))
        (find-method* `(setf ,symbol) () (list t (first locative-args))))))

(defmethod docstring* ((dref writer-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (swank-mop:slot-definition-documentation
     (find-writer-slot-definition symbol (first locative-args)))))

(defmethod source-location* ((dref writer-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (swank-source-location* (find-method* symbol ()
                                          (list t (first locative-args)))
                            symbol `(writer ,(first locative-args)))))


;;;; ACCESSOR locative

(define-locative-type (accessor class-name) (reader writer setf-method)
  """Refers to an :ACCESSOR in a DEFCLASS.

  An :ACCESSOR in DEFCLASS creates a reader and a writer method.
  Somewhat arbitrarily, ACCESSOR references RESOLVE to the writer
  method but can be LOCATEd with either.""")

(define-cast accessor ((dref reader-dref))
  (let ((name (dref-name dref))
        (class (second (dref-locative dref))))
    (when (ignore-errors (find-accessor-slot-definition name class))
      (%make-dref name accessor `(,class)))))

(define-cast accessor ((dref writer-dref))
  (let ((name (dref-name dref))
        (class (second (dref-locative dref))))
    (when (ignore-errors (find-accessor-slot-definition name class))
      (%make-dref name accessor `(,class)))))

(define-cast accessor ((dref setf-method-dref))
  (let ((name (dref-name dref))
        (class (second (third (dref-locative dref)))))
    (when (ignore-errors (find-accessor-slot-definition name class))
      (%make-dref name accessor `(,class)))))

(define-lookup accessor (name locative-args)
  (unless (ignore-errors
           (find-accessor-slot-definition name (first locative-args)))
    (locate-error))
  (%make-dref name accessor locative-args))

(defun find-accessor-slot-definition (accessor-symbol class-symbol)
  (when (class-slots-supported-p class-symbol)
    (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
      (when (and (find accessor-symbol
                       (swank-mop:slot-definition-readers slot-def))
                 (find `(setf ,accessor-symbol)
                       (swank-mop:slot-definition-writers slot-def)
                       :test #'equal))
        (return-from find-accessor-slot-definition slot-def)))
    (locate-error "Could not find accessor ~S for class ~S."
                  accessor-symbol class-symbol)))

(defmethod map-definitions-of-name (fn name (locative-type (eql 'accessor)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod resolve* ((dref accessor-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (find-method* `(setf ,symbol) () (list t (first locative-args)))))

(defmethod docstring* ((dref accessor-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (swank-mop:slot-definition-documentation
     (find-accessor-slot-definition symbol (first locative-args)))))

(defmethod source-location* ((dref accessor-dref))
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (swank-source-location* (find-method* symbol ()
                                          (list (first locative-args)))
                            symbol `(accessor ,(first locative-args)))))


;;;; STRUCTURE-ACCESSOR locative

(define-locative-type (structure-accessor &optional structure-class-name)
    (setf-function function)
  "Refers to an accessor function generated by DEFSTRUCT or DEFSTRUCT*.
  A LOCATE-ERROR condition is signalled if the wrong
  STRUCTURE-CLASS-NAME is provided.

  Note that there is no portable way to detect structure accessors,
  and on some platforms, `(LOCATE #'MY-ACCESSOR)`, DEFINITIONS and
  DREF-APROPOS will return FUNCTION references instead. On such
  platforms, STRUCTURE-ACCESSOR references do not RESOLVE.")

(define-cast structure-accessor ((dref function-dref))
  (let ((name (dref-name dref)))
    (multiple-value-bind (structure-name certainp)
        (structure-name-of-accessor name)
      (when (and structure-name certainp)
        (%make-dref name structure-accessor `(,structure-name))))))

(define-cast structure-accessor ((dref setf-function-dref))
  (let ((name (dref-name dref)))
    (multiple-value-bind (structure-name certainp)
        (structure-name-of-accessor name)
      (when (and structure-name certainp)
        (%make-dref name structure-accessor `(,structure-name))))))

(defun structure-name-of-accessor (symbol)
  #-(or ccl sbcl) (declare (ignore symbol))
  #+ccl
  (values (let ((info (ccl::structref-info symbol)))
            (when (ccl::accessor-structref-info-p info)
              (cdr info)))
          t)
  #+sbcl
  (values (let ((info (sb-kernel:structure-instance-accessor-p symbol)))
            (when info
              (slot-value (first info) 'sb-kernel::name)))
          t))

(define-lookup structure-accessor (symbol locative-args)
  (unless (and (symbolp symbol)
               (ignore-errors (symbol-function* symbol)))
    (locate-error "~S is not a symbol that names a function." symbol))
  (multiple-value-bind (structure-name* certainp)
      (structure-name-of-accessor symbol)
    (when (and (null structure-name*) certainp)
      (locate-error "~S is not a structure accessor."
                    (symbol-function* symbol)))
    (let ((structure-name (first locative-args)))
      (when (and structure-name structure-name*
                 (not (eq structure-name structure-name*)))
        (locate-error "This accessor is on structure ~S not on ~S."
                      structure-name* structure-name))
      (%make-dref symbol structure-accessor
                  (if (or structure-name structure-name*)
                      `(,(or structure-name structure-name*))
                      ())))))

(defmethod map-definitions-of-name
    (fn name (locative-type (eql 'structure-accessor)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod resolve* ((dref structure-accessor-dref))
  #+(or ccl sbcl)
  (symbol-function* (dref-name dref))
  #-(or ccl sbcl)
  (resolve-error))

(defmethod docstring* ((dref structure-accessor-dref))
  (documentation* (dref-name dref) 'function))

(defmethod source-location* ((dref structure-accessor-dref))
  (let ((symbol (dref-name dref)))
    (swank-source-location* (symbol-function* symbol) symbol 'function)))


(defsection @typelike-locatives (:title "Locatives for Types and Declarations")
  (type locative)
  (class locative)
  (structure locative)
  (declaration locative))


;;;; TYPE locative

(define-locative-type type ()
  "This locative can refer to [types and classes][clhs] and
  [conditions][clhs], simply put, to things defined by DEFTYPE,
  DEFCLASS and DEFINE-CONDITION.

  ```cl-transcript (:dynenv dref-std-env)
  (deftype my-type () t)
  (dref 'my-type 'type)
  ==> #<DREF MY-TYPE TYPE>
  ```
  ```cl-transcript (:dynenv dref-std-env)
  (dref 'xref 'type)
  ==> #<DREF XREF CLASS>
  ```
  ```cl-transcript (:dynenv dref-std-env)
  (dref 'locate-error 'type)
  ==> #<DREF LOCATE-ERROR CONDITION>
  ```

  TYPE references do not RESOLVE.")

(define-lookup type (symbol locative-args)
  (unless (and (symbolp symbol)
               ;; On most Lisps, SWANK-BACKEND:TYPE-SPECIFIER-P is not
               ;; reliable.
               #-(or abcl allegro clisp cmucl ecl sbcl)
               (swank-backend:type-specifier-p symbol)
               #+sbcl
               (sb-ext:defined-type-name-p symbol))
    (locate-error "~S is not a valid type specifier." symbol))
  (%make-dref symbol type))

(defmethod map-definitions-of-name (fn name (locative-type (eql 'type)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod arglist* ((dref type-dref))
  (let ((name (dref-name dref)))
    (nth-value-or* 1
      #+ccl
      (function-arglist (gethash name ccl::%deftype-expanders%) :deftype)
      (let ((arglist (swank-backend:type-specifier-arglist name)))
        (if (listp arglist)
            (values arglist :deftype)
            nil)))))

(defmethod docstring* ((dref type-dref))
  (documentation* (dref-name dref) 'type))

(defmethod source-location* ((dref type-dref))
  (swank-source-location (dref-name dref) 'type 'class 'condition))


;;;; CLASS locative
;;;;
;;;; Be careful changing this because DREF-EXT::@DEFINING-LOCATIVE-TYPES
;;;; INCLUDEs the code in a rather fine-grained way.

(define-locative-type class (type)
  "Naturally, CLASS is the locative type for [CLASS][class]es.

  Also, see the related CONDITION locative.")

(define-locator class ((class class))
  (make-instance 'class-dref :name (class-name class) :locative 'class))

(define-lookup class (symbol locative-args)
  (unless (and (symbolp symbol)
               (find-class symbol nil))
    (locate-error "~S does not name a class." symbol))
  (make-instance 'class-dref :name symbol :locative 'class))

(defmethod resolve* ((dref class-dref))
  (find-class (dref-name dref)))

(defmethod docstring* ((class class))
  (documentation* class t))

(defmethod source-location* ((dref class-dref))
  (swank-source-location* (resolve dref) (dref-name dref) 'class))

(defvar %end-of-class-example)


;;;; STRUCTURE locative

(define-locative-type structure (class)
  "Refers to a STRUCTURE-CLASS, typically defined with DEFSTRUCT.

  Also, see DEFSTRUCT*.")

(define-lookup structure (symbol locative-args)
  (let ((class (and (symbolp symbol) (find-class symbol nil))))
    (unless (and class (subtypep class 'structure-object))
      (locate-error "~S does not name a ~S." symbol 'structure-class))
    (%make-dref symbol structure)))

(defmethod source-location* ((dref structure-dref))
  (swank-source-location* (resolve dref) (dref-name dref) 'structure))


;;;; DECLARATION locative

(define-locative-type declaration ()
  """Refers to a declaration, used in DECLARE, DECLAIM and PROCLAIM.

  User code may also define new declarations with CLTL2 functionality,
  but there is currently no way to provide a docstring, and their
  ARGLIST is always NIL.

  ```
  (cl-environments:define-declaration my-decl (&rest things)
    (values :declare (cons 'foo things)))
  ```

  DECLARATION references do not RESOLVE.

  Also, SOURCE-LOCATION on declarations currently only works on SBCL.""")

(defparameter *ansi-declarations*
  (plist-hash-table
   (loop for symbol in '(compilation-speed debug declaration dynamic-extent
                         ftype ignorable ignore inline notinline optimize
                         safety space special speed type)
         append (list symbol t))
   :test #'eq))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-cltl2))

(define-lookup declaration (symbol locative-args)
  (unless (and (symbolp symbol)
               (or (gethash symbol *ansi-declarations*)
                   #+allegro (find symbol (system:declaration-information
                                           'declaration))
                   #+ccl (find symbol (ccl:declaration-information
                                       'declaration))
                   #+sbcl (find symbol (sb-cltl2:declaration-information
                                        'declaration))
                   #-(or allegro ccl sbcl)
                   t))
    (locate-error "~S is not a known declaration." symbol))
  (%make-dref symbol declaration))

(defmethod map-definitions-of-name (fn name (locative-type (eql 'declaration)))
  #+(or ccl sbcl)
  (when-let (dref (dref name locative-type nil))
    (funcall fn dref))
  ;; Lacking DECLARATION-INFORMATION form CLTL2 on other Lisps,
  ;; DREF* always succeeds.
  #-(or ccl sbcl)
  (declare (ignore fn name))
  (values))

;;; FIXME: implement a new doc-type for DOCUMENTATION?

(defmethod source-location* ((dref declaration-dref))
  #+sbcl
  (swank-backend:find-source-location
   (sb-int:info :declaration :known (dref-name dref)))
  #-sbcl
  '(:error "Don't know how to find the source location of declarations."))


(defsection @condition-system-locatives
    (:title "Locatives for the Condition System")
  (condition locative)
  (restart locative)
  (define-restart macro))


;;;; CONDITION locative

(define-locative-type condition (class)
  "Although CONDITION is not SUBTYPEP of CLASS, actual condition
  objects are commonly instances of a condition class that is a CLOS
  class. HyperSpec [ISSUE:CLOS-CONDITIONS][clhs] and
  [ISSUE:CLOS-CONDITIONS-AGAIN][clhs] provide the relevant history.

  Whenever a CLASS denotes a CONDITION, its DREF-LOCATIVE-TYPE will be
  CONDITION:

  ```cl-transcript (:dynenv dref-std-env)
  (dref 'locate-error 'class)
  ==> #<DREF LOCATE-ERROR CONDITION>
  ```")

(define-lookup condition (symbol locative-args)
  (let ((class (and (symbolp symbol) (find-class symbol nil))))
    (unless (and class (subtypep class 'condition))
      (locate-error "~S does not name a condition class." symbol))
    (%make-dref symbol condition)))

(defmethod resolve* ((dref condition-dref))
  (find-class (dref-name dref)))

(defmethod docstring* ((dref condition-dref))
  (documentation* (find-class (dref-name dref)) t))

(defmethod source-location* ((dref condition-dref))
  (swank-source-location* (resolve dref) (dref-name dref) 'condition))


;;;; RESTART locative

;;; Provide definitions for standard CL restarts.
(define-restart use-value (value)
  "This is the name of the RESTART to which [USE-VALUE][function]
  transfers control.")
(define-restart store-value (value)
  "This is the name of the RESTART to which [STORE-VALUE][function]
  transfers control.")
(define-restart muffle-warning ()
  "This is the name of the RESTART to which [MUFFLE-WARNING][function]
  transfers control.")
(define-restart continue ()
  "This is the name of the RESTART to which [CONTINUE][function]
  transfers control.")
(define-restart abort ()
  "This is the name of the RESTART to which [ABORT][function]
  transfers control.")


(defsection @packagelike-locatives
    (:title "Locatives for Packages and Readtables")
  (asdf:system locative)
  (package locative)
  (readtable locative))


;;;; ASDF:SYSTEM locative

(define-locative-type asdf:system ()
  "Refers to an already loaded ASDF:SYSTEM (those in ASDF:REGISTERED-SYSTEMS).
  The @NAME may be anything ASDF:FIND-SYSTEM supports.

  ASDF:SYSTEM is not EXPORTABLE-LOCATIVE-TYPE-P."
  (defclass asdf-system-dref))

(define-locator asdf:system ((system asdf:system))
  (%make-dref (character-string (slot-value system 'asdf::name))
              asdf:system))

(define-lookup asdf:system (name locative-args)
  (let ((name (ignore-errors (string-downcase (string name)))))
    (unless (progn
              #+(or allegro clisp ecl)
              (when (member name (asdf:registered-systems) :test #'string=)
                (find-system* name))
              #-(or allegro clisp ecl)
              (asdf:registered-system name))
      (locate-error "~S does not name an ASDF:SYSTEM." name))
    (%make-dref (character-string name) asdf:system)))

(defmethod map-definitions-of-type (fn (locative-type (eql 'asdf:system)))
  (dolist (name (asdf:registered-systems))
    (funcall fn (dref name 'asdf:system))))

(defmethod resolve* ((dref asdf-system-dref))
  (handler-bind ((warning #'muffle-warning))
    (or (find-system* (dref-name dref) :errorp nil)
        (resolve-error))))

(defmethod source-location* ((dref asdf-system-dref))
  (let ((system (resolve dref)))
    (if-let (location (asdf/system:system-source-file system))
      `(:location
        (:file ,(namestring location))
        (:position 1)
        (:snippet ,(format nil "defsystem ~S" (dref-name dref))))
      `(:error ,(format nil "ASDF system ~A doesn't contain any location information."
                        (asdf:primary-system-name system))))))


;;;; PACKAGE locative

(define-locative-type package ()
  "Refers to a [PACKAGE][type], defined by DEFPACKAGE or MAKE-PACKAGE.
  The @NAME may be anything FIND-PACKAGE supports.

  PACKAGE is not EXPORTABLE-LOCATIVE-TYPE-P.")

(define-locator package ((package package))
  (%make-dref (character-string (package-name package)) package))

(define-lookup package (package-designator locative-args)
  (unless (and (or (symbolp package-designator)
                   (stringp package-designator))
               (find-package* package-designator))
    (locate-error "~S does not name a package." package-designator))
  (%make-dref (character-string
               (package-name (find-package* package-designator)))
              package))

(defmethod map-definitions-of-type (fn (locative-type (eql 'package)))
  (dolist (package (list-all-packages))
    (funcall fn (dref (package-name package) 'package))))

(defmethod resolve* ((dref package-dref))
  (find-package* (dref-name dref)))

(defmethod docstring* ((dref package-dref))
  (documentation* (resolve dref) t))

(defmethod source-location* ((dref package-dref))
  (let ((name (dref-name dref)))
    (swank-source-location* (find-package* name)
                            (if (stringp name)
                                (make-symbol name)
                                name)
                            'package)))


;;;; READTABLE locative

(define-locative-type readtable ()
  "Refers to a named [READTABLE][] defined with
  NAMED-READTABLES:DEFREADTABLE, which associates a global name and a
  docstring with the readtable object. The @NAME may be anything
  FIND-READTABLE supports.

  READTABLE references RESOLVE to FIND-READTABLE on their @NAME.")

(define-locator readtable ((readtable readtable))
  (let ((name (readtable-name readtable)))
    (unless name
      (locate-error "It does not have a name."))
    (when name
      (%make-dref name readtable))))

(define-lookup readtable (name locative-args)
  (let ((readtable (and (symbolp name)
                        (named-readtables:find-readtable name))))
    (if readtable
        (%make-dref (named-readtables:readtable-name readtable) readtable)
        (locate-error))))

(defmethod resolve* ((dref readtable-dref))
  (named-readtables:find-readtable (dref-name dref)))

(defmethod docstring* ((dref readtable-dref))
  (documentation* (dref-name dref) 'readtable))

(defmethod source-location* ((dref readtable-dref))
  (let ((readtable (named-readtables:find-readtable (dref-name dref)))
        (ht named-readtables::*readtable-to-dummy-with-source-location*))
    (when-let ((dummy (gethash readtable ht)))
      (source-location (dref dummy 'function nil) :error :error))))


(defsection @dref-locatives (:title "Locatives for DRef Constructs")
  (dtype locative)
  (locative locative)
  (lambda locative))


;;;; DTYPE locative

(define-locative-type dtype ()
  "Locative for @DTYPES defined with DEFINE-DTYPE and LOCATIVE types.
  DTYPE is to LOCATIVE as TYPE is to CLASS.

  The TOP of the DTYPE hierarchy:

  ```cl-transcript
  (dref 'top 'dtype)
  ==> #<DREF TOP DTYPE>
  ```

  This very definition:

  ```cl-transcript
  (dref 'dtype 'locative)
  ==> #<DREF DTYPE LOCATIVE>
  ```")

(define-lookup dtype (name locative-args)
  (if (gethash name *dtype-expanders*)
      (%make-dref name dtype)
      (dref name 'locative)))


;;;; LOCATIVE locative

(define-locative-type locative (dtype)
  "This is the locative for @LOCATIVE-TYPEs defined with
  DEFINE-LOCATIVE-TYPE, DEFINE-PSEUDO-LOCATIVE-TYPE and
  DEFINE-LOCATIVE-ALIAS.

  ```
  (first-line (source-location-snippet
               (source-location (dref 'macro 'locative))))
  => \"(define-locative-type macro ()\"
  ```")

(define-lookup locative (symbol locative-args)
  ;; Faster than calling LOCATIVE-TYPE-LAMBDA-LIST-METHOD-FOR-SYMBOL.
  (unless (or (locative-type-p symbol)
              (find symbol *locative-aliases*))
    (locate-error "~S is not a valid locative type or locative alias." symbol))
  (%make-dref symbol locative))


;;;; LAMBDA locative

(define-pseudo-locative-type
    (lambda &key arglist arglist-type docstring docstring-package
            file file-position snippet
            &allow-other-keys)
    ()
  """A [pseudo locative type][ pseudo-locative-types] that carries its
  ARGLIST, DOCSTRING and SOURCE-LOCATION in the locative itself. See
  MAKE-SOURCE-LOCATION for the description of FILE, FILE-POSITION, and
  SNIPPET. LAMBDA references do not RESOLVE. The @NAME must be NIL.

  ```cl-transcript (:dynenv dref-std-env)
  (arglist (dref nil '(lambda :arglist ((x y) z)
                              :arglist-type :macro)))
  => ((X Y) Z)
  => :MACRO
  ```

  ```cl-transcript (:dynenv dref-std-env)
  (docstring (dref nil '(lambda :docstring "xxx"
                                :docstring-package :dref)))
  => "xxx"
  ==> #<PACKAGE "DREF">
  ```

  ```cl-transcript (:dynenv dref-std-env)
  (source-location-file
   (source-location (dref nil '(lambda :file "xxx.el"))))
  => "xxx.el"
  ```

  Also, see the PAX:INCLUDE locative.""")

(define-lookup lambda (name locative-args)
  (when name
    (locate-error "The name ~S is not NIL." name))
  (%make-dref name lambda locative-args))

(defmethod arglist* ((dref lambda-dref))
  (let ((arglist (getf (dref-locative-args dref) :arglist '%not-there))
        (arglist-type (getf (dref-locative-args dref) :arglist-type t)))
    (unless (eq arglist '%not-there)
      (values arglist arglist-type))))

(defmethod docstring* ((dref lambda-dref))
  (let ((docstring (getf (dref-locative-args dref) :docstring '%not-there))
        (package (getf (dref-locative-args dref) :docstring-package)))
    (unless (eq docstring '%not-there)
      (values docstring (find-package* package)))))

(defmethod source-location* ((dref lambda-dref))
  (let* ((args (dref-locative-args dref))
         (file (getf args :file))
         (file-position (getf args :file-position))
         (snippet (getf args :snippet)))
    (make-source-location :file file :file-position file-position
                          :snippet snippet)))


(defsection @unknown-definitions (:title "Locatives for Unknown Definitions")
  (unknown locative))

(define-locative-type (unknown dspec) ()
  "This locative type allows PAX to work in a limited way with
  definition types it doesn't know. UNKNOWN definitions come from
  DEFINITIONS, which uses SWANK/BACKEND:FIND-DEFINITIONS. The
  following examples show PAX stuffing the Swank
  dspec `(:DEFINE-ALIEN-TYPE DOUBLE-FLOAT)` into an UNKNOWN locative
  on SBCL.

  ```cl-transcript (:dynenv dref-std-env)
  (definitions 'double-float)
  ==> (#<DREF DOUBLE-FLOAT CLASS>
  -->  #<DREF DOUBLE-FLOAT (UNKNOWN (:DEFINE-ALIEN-TYPE DOUBLE-FLOAT))>)
  ```

  ```cl-transcript (:dynenv dref-std-env)
  (dref 'double-float '(unknown (:define-alien-type double-float)))
  ==> #<DREF DOUBLE-FLOAT (UNKNOWN (:DEFINE-ALIEN-TYPE DOUBLE-FLOAT))>
  ```

  ARGLIST and DOCSTRING return NIL for UNKNOWNs, but SOURCE-LOCATION
  works.")

(define-lookup unknown (name locative-args)
  (unless (and (symbolp name)
               locative-args
               (find (first locative-args) (swank-dspecs name) :test #'equal))
    (locate-error))
  (%make-dref name unknown locative-args))

(defmethod map-definitions-of-name (fn name (locative-type (eql 'unknown)))
  (declare (ignore fn name))
  'swank-definitions)

(defmethod source-location* ((dref unknown-dref))
  (let ((dspec-and-location-list (swank-dspecs-and-locations (dref-name dref)))
        (dspec (first (dref-locative-args dref))))
    (second (find dspec dspec-and-location-list :key #'first :test #'equal))))


(defsection @dref-classes (:title "DREF-CLASSes")
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
