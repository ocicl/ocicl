(in-package :dref)

;;;; Swank utilities with no dependency on DREF

;;; Return the source location of OBJECT, which may be a FUNCTION,
;;; CLASS, etc. Anything that SWANK-BACKEND:FIND-SOURCE-LOCATION knows
;;; about. If no source location is found, then return NIL.
(defun swank-object-source-location (object)
  (when object
    (let ((location (swank-backend:find-source-location
                     (if (functionp object)
                         (unencapsulated-function object)
                         object))))
      (when (and (listp location)
                 (eq (first location) :location))
        location))))

;;; Like SWANK-BACKEND::FIND-DEFINITIONS, but OBJECT may be a STRING
;;; or a SYMBOL (including keyword symbols), and if ERRORP is NIL (the
;;; default), then errors treated as the empty list.
(defun swank-dspecs-and-locations (object &key errorp)
  (if errorp
      (swank-dspecs-and-locations-1 object)
      (error-location-to-nil (ignore-errors
                              (swank-dspecs-and-locations-1 object)))))

(defun swank-dspecs-and-locations-1 (object)
  ;; Source files may have #. in them.
  (let ((*read-eval* t))
    (swank-backend::find-definitions (swank-definition-name object))))

;;; Turn OBJECT into a symbol suitable as an argument to
;;; SWANK-BACKEND:FIND-DEFINITIONS.
(defun swank-definition-name (object)
  (cond ((stringp object)
         ;; E.g. to find the package when OBJECT is "DREF".
         (values (make-symbol (adjust-string-case object)) t))
        ((keywordp object)
         ;; E.g. to find the package when OBJECT is :DREF. On SBCL,
         ;; SWANK-BACKEND:FIND-DEFINITIONS barfs on keywords.
         (values (make-symbol (symbol-name object)) t))
        ((or (symbolp object)
             (and (listp object)
                  (extended-function-name-p object)))
         (values object t))))

(defun error-location-to-nil (dspec-and-locations)
  (if (eq (first dspec-and-locations) :error)
      ()
      #+allegro
      ;; (swank-backend::find-definitions :xxx)
      ;; => ((:XXX (:ERROR "Unknown source location for :XXX")))
      (remove-if (lambda (dspec-and-location)
                   (not (listp (first dspec-and-location))))
                 dspec-and-locations)
      #-allegro
      dspec-and-locations))

;;; This can be awfully slow because it may READ sources to get the
;;; source locations. The rest below, on the other hand, are Lisp
;;; implementation specific Swank implementations of
;;; SWANK-BACKEND:FIND-DEFINITIONS modified not to read files or
;;; buffers.
#-sbcl
(defun swank-dspecs (name)
  (multiple-value-bind (name foundp) (swank-definition-name name)
    (when foundp
      (mapcar #-abcl #'first
              #+abcl (lambda (dspec-and-location)
                       (if (eq (first dspec-and-location) :primitive)
                           '(function)
                           (first dspec-and-location)))
              (swank-dspecs-and-locations name)))))

#+sbcl
(defun swank-dspecs (name)
  (multiple-value-bind (name foundp) (swank-definition-name name)
    (when foundp
      (loop for type in swank/sbcl::*definition-types* by #'cddr
            for defsrcs = (sb-introspect:find-definition-sources-by-name
                           name type)
            for filtered-defsrcs
              = (if (eq type :generic-function)
                    (remove :invalid defsrcs
                            :key #'swank/sbcl::categorize-definition-source)
                    defsrcs)
            append (loop for defsrc in filtered-defsrcs
                         collect (swank/sbcl::make-dspec type name defsrc))))))


;;;; Swank utilities that depend on DREF

(defun swank-definitions (name locative-types)
  (when (and (or (symbolp name) (stringp name) (listp name))
             locative-types)
    (loop for dspec in (swank-dspecs name)
          for dref = (dspec-to-definition dspec name)
          when (member (dref-locative-type dref) locative-types)
            ;; FIXME:
            ;; (definitions 'sb-pcl:make-specializer-form-using-class)
            ;; puts the method combination among the specializers.
            collect dref)))

;;; Return a Swank source location for a definition of NAME. Try
;;; forming @REFERENCEs with NAME and one of LOCATIVES. Stop at the
;;; first locative with which a definition is found, and return its
;;; location. If no location was found or on any error, then return
;;; the usual Swank `(:ERROR ...)`. The implementation is based on the
;;; rather expensive SWANK-BACKEND:FIND-DEFINITIONS function.
(defun swank-source-location (name &rest locatives)
  (swank::converting-errors-to-error-location
    (let* ((dspecs (loop for locative in locatives
                         ;; Get the initial DREF, so that PAX:SECTIONs
                         ;; (canonicalized from VARIABLEs) can find
                         ;; their source location.
                         for dref = (ignore-errors
                                     (call-lookup name (locative-type locative)
                                                  (locative-args locative)))
                         when dref
                           collect (definition-to-dspec dref)))
           (dspec-and-location-list
             (mapcar (lambda (dspec-and-location)
                       (list (normalize-dspec (first dspec-and-location))
                             (second dspec-and-location)))
                     (swank-dspecs-and-locations name)))
           (entry (loop for dspec in dspecs
                          thereis (find dspec dspec-and-location-list
                                        :key #'first :test #'equal))))
      (if entry
          (second entry)
          `(:error ,(format nil "Could not find source location for ~S."
                            dspecs))))))

;;; Like FIND-DEFINITION, but tries to get the definition of OBJECT
;;; (for example a FUNCTION or METHOD object) with the fast but not
;;; widely supported SWANK-BACKEND:FIND-SOURCE-LOCATION before calling
;;; the much slower but more complete SWANK-BACKEND:FIND-DEFINITIONS.
(defun/autoloaded swank-source-location* (object name &rest locatives)
  (swank::converting-errors-to-error-location
    (or (swank-object-source-location object)
        (apply #'swank-source-location name locatives))))


#+sbcl
(defun/autoloaded translate-sb-source-location (sb-source-location)
  (swank/sbcl::definition-source-for-emacs
   (sb-introspect::translate-source-location sb-source-location)
   nil nil))


;;;; Conversions between DREFs and Swank dspecs

;;; We could convert most XREFs to dspecs except for some accessors on
;;; CCL, whose dpsecs have CLASSes in them (not class names), which
;;; must exist. However, for the conversion to make sense the XREF
;;; would need to be syntactically correct, which is a property of
;;; DREFs. So, instead we require DREFs.
(defun definition-to-dspec (dref)
  (let* ((name (dref-name dref))
         (setf-name `(setf ,name))
         (locative (dref-locative dref))
         (type (locative-type locative))
         (args (locative-args locative)))
    (case type
      (variable (swank-variable-dspec name))
      (constant (swank-constant-dspec name))
      (macro (swank-macro-dspec name))
      (compiler-macro (swank-compiler-macro-dspec name))
      (setf-compiler-macro (swank-compiler-macro-dspec setf-name))
      (symbol-macro (swank-symbol-macro-dspec name))
      (setf (swank-setf-dspec name))
      (function (swank-function-dspec name))
      (setf-function (swank-function-dspec setf-name))
      (generic-function (swank-generic-function-dspec name))
      (setf-generic-function (swank-generic-function-dspec setf-name))
      (method (swank-method-dspec name (butlast args) (first (last args))))
      (setf-method (swank-method-dspec setf-name (first args) (second args)))
      (accessor (swank-accessor-dspec name (first args) t))
      (reader (swank-accessor-dspec name (first args) nil))
      (writer (swank-accessor-dspec name (first args) t))
      (method-combination (swank-method-combination-dspec name))
      (type (swank-type-dspec name))
      (class (swank-class-dspec name))
      (structure (swank-structure-dspec name))
      (condition (swank-condition-dspec name))
      (package (swank-package-dspec name))
      ;; Reverse DSPEC-TO-DEFINITION's catch-all mechanism.
      (unknown (first args))
      (t
       ;; Maybe it's a PAX locative. Fake a dspec. It won't matter
       ;; that it's fake (i.e. it cannot be produced by
       ;; SWANK/BACKEND:FIND-DEFINITIONS) because Swank only uses it
       ;; as a label to show to the user.
       (list locative name)))))

(defmacro define-dspec (name lambda-list &body body)
  (multiple-value-bind (clauses declarations) (alexandria:parse-body body)
    (let ((dspec-forms (loop for (feature-expr dspec-form) on clauses
                             by #'cddr
                             when (alexandria:featurep feature-expr)
                               collect dspec-form)))
      (assert (<= (length dspec-forms) 1))
      `(defun ,name ,lambda-list
         ,@declarations
         ,(if dspec-forms
              (first dspec-forms)
              (progn
                (format *error-output* "!!! No definition for ~S." name)
                `'(,(gensym "NOTIMPLEMENTED") ,name)))))))

#-(or allegro ecl)
(defun normalize-dspec (dspec)
  dspec)

#+allegro
(defun normalize-dspec (dspec)
  ;; (DEFVAR *FOO*) without a global binding is a
  ;; :SPECIAL-DECLARATION.
  (if (and (listp dspec) (eq (first dspec) :special-declaration))
      (cons :variable (rest dspec))
      dspec))

#+ecl
(defun normalize-dspec (dspec)
  ;; (DEFMETHOD TEST-GF (X NUMBER) (Y (EQL 'F)) => (DEFMETHOD TEST-GF
  ;; NUMBER (EQL F)) so that it matches SWANK-METHOD-DSPEC.
  (flet ((remove-arg-name (specifier)
           (if (and (listp specifier)
                    (= (length specifier) 2))
               (second specifier)
               specifier))
         (remove-quote-from-eql (specifier)
           (if (and (listp specifier)
                    (= (length specifier) 2)
                    (eq (first specifier) 'eql)
                    ;; OBJ is (QUOTE F) in the above example.
                    (let ((obj (second specifier)))
                      (and (listp obj)
                           (= (length obj) 2)
                           (eq (first obj) 'quote))))
               `(eql ,(second (second specifier)))
               specifier)))
    (case (first dspec)
      ((defmethod)
       (list* 'defmethod (second dspec)
              (mapcar (lambda (specifier)
                        (remove-quote-from-eql
                         (remove-arg-name specifier)))
                      (cddr dspec))))
      ((defparameter)
       (cons 'defvar (rest dspec)))
      (t
       dspec))))

(define-dspec swank-variable-dspec (name)
  (:or :abcl :ecl :sbcl) `(defvar ,name)
  :allegro `(:variable ,name)
  :ccl `(variable ,name)
  :cmucl `(variable :special ,name))

(define-dspec swank-constant-dspec (name)
  (:or :abcl :ecl :sbcl) `(defconstant ,name)
  :allegro `(:variable ,name)
  #+ccl :ccl #+ccl `(ccl::constant ,name)
  :cmucl `(variable :constant ,name))

(define-dspec swank-macro-dspec (name)
  (:or :abcl :ecl :cmucl :sbcl) `(defmacro ,name)
  :allegro `(:operator ,name)
  :ccl `(function ,name))

(define-dspec swank-compiler-macro-dspec (name)
  (:or :abcl :ecl :cmucl :sbcl) `(define-compiler-macro ,name)
  :clisp `(,name define-compiler-macro)
  :allegro `(:compiler-macro ,name)
  :ccl `(compiler-macro ,name))

(define-dspec swank-symbol-macro-dspec (name)
  (:or :abcl :ecl :sbcl) `(define-symbol-macro ,name)
  :allegro `(:symbol-macro ,name)
  #+ccl :ccl #+ccl `(ccl::symbol-macro ,name)
  :cmucl `(variable :macro ,name))

(define-dspec swank-setf-dspec (name)
  (:or :abcl :sbcl) `(define-setf-expander ,name)
  :allegro `(:setf-method ,name)
  #+ccl :ccl #+ccl `(ccl::setf-expander ,name)
  :cmucl `(setf ,name))

(define-dspec swank-function-dspec (name)
  (declare (ignorable name))
  #+clisp :clisp #+clisp `(,name system::defun/defmacro)
  (:or :abcl :ecl :sbcl) `(defun ,name)
  :allegro `(:operator ,name)
  (:or :ccl :cmucl) `(function ,name))

(define-dspec swank-generic-function-dspec (name)
  (:or :abcl :cmucl :ecl :sbcl) `(defgeneric ,name)
  :allegro `(:operator ,name)
  :ccl `(function ,name))

;;; QUALIFIERS and SPECIALIZERS are straight from the DEFMETHOD form.
;;; That is, SPECIALIZERS contains names such as NUMBER or (EQL 7),
;;; not class objects and eql specializer objects.
(define-dspec swank-method-dspec (name qualifiers specializers)
  (:or :clisp :ecl :sbcl) `(defmethod ,name ,@qualifiers ,@specializers)
  :abcl `(defmethod ,name ,@qualifiers ,specializers)
  :allegro `(:operator (method ,name ,@qualifiers ,specializers))
  :ccl `(:method ,name ,@qualifiers
          ,(mapcar 'specializer-to-object specializers))
  :cmucl `(method ,name ,@qualifiers ,specializers))

(define-dspec swank-accessor-dspec (name class-name writerp)
  (declare (ignorable name class-name writerp))
  :allegro `(:type (method ,name (,@(when writerp '(t)) ,class-name)))
  #+ccl :ccl #+ccl `(,(if writerp
                          'ccl::writer-method
                          'ccl::reader-method)
                     (:method ,name
                       (,@(when writerp (list (find-class t)))
                        ,(find-class class-name))))
  :cmucl `(method ,name () (,@(when writerp '(t)) ,class-name))
  :sbcl `(defmethod ,name ,@(when writerp '(t)) ,class-name))

(define-dspec swank-method-combination-dspec (name)
  :allegro `(:define-method-combination ,name)
  :ccl `(method-combination ,name)
  :sbcl `(define-method-combination ,name))

(define-dspec swank-type-dspec (name)
  (:or :abcl :cmucl :sbcl) `(deftype ,name)
  :allegro `(:type ,name)
  :ccl `(type ,name))

(define-dspec swank-class-dspec (name)
  :abcl '(defclass)
  :allegro `(:type ,name)
  (:or :cmucl :ecl) `(defclass ,name)
  :ccl `(class ,name)
  :sbcl `(defclass ,name))

(define-dspec swank-structure-dspec (name)
  :abcl '(defstruct)
  :allegro `(:type ,name)
  (:or :cmucl :ecl) `(defstruct ,name)
  :ccl `(structure ,name)
  :sbcl `(defstruct ,name))

(define-dspec swank-condition-dspec (name)
  (:or :abcl :sbcl) `(define-condition ,name)
  :ecl `(defclass ,name)
  :allegro `(:type ,name)
  (:or :ccl :cmucl) `(class ,name))

(define-dspec swank-package-dspec (name)
  ;; These don't currently seem to have definitions for packages, so
  ;; the dspec is just a guess.
  (:or :abcl :allegro :ecl :clisp :cmucl) `(defpackage ,name)
  :ccl `(package ,name)
  :sbcl `(defpackage ,name))


;;; A helper for SWANK-DEFINITIONS. Return the DREF corresponding to
;;; DSPEC (that was returned by SWANK-DSPECS). This is the inverse of
;;; DEFINITION-TO-DSPEC. NAME is usually the same as (SECOND DSPEC).
(defun dspec-to-definition (dspec name)
  (or (handler-case
          (dspec-to-definition* dspec name)
        ;; Ideally, if the dspec says it's a function, then it would be
        ;; LOCATEable as such. Alas, for example on CCL, (SWANK-DSPECS
        ;; 'FUNCTION) returns (FUNCTION CCL::NX1-FUNCTION), but there is
        ;; no function with that name.
        (locate-error ()
          nil))
      ;; There are also dspecs that we just plain don't recognize.
      ;; Either way, let's just stuff it into an UNKNOWN.
      (make-instance 'unknown-dref :name name :locative `(unknown ,dspec))))

(defun dspec-to-definition* (dspec name)
  (let ((dspec (normalize-dspec dspec))
        (setf-name `(setf ,name)))
    (or (package-dspec-to-definition dspec)
        (method-dspec-to-definition name dspec)
        ;; Handle the symbol-based cases where the DPSEC is unique and
        ;; easy.
        (when (or (symbolp name) (listp name))
          (loop named lazy-wasteful-parsing
                for (locative-type* dspec*)
                  in `((variable ,(swank-variable-dspec name))
                       (constant ,(swank-constant-dspec name))
                       (function ,(swank-function-dspec name))
                       (setf-function ,(swank-function-dspec setf-name))
                       (macro ,(swank-macro-dspec name))
                       (compiler-macro ,(swank-compiler-macro-dspec name))
                       (setf-compiler-macro
                        ,(swank-compiler-macro-dspec setf-name))
                       (symbol-macro ,(swank-symbol-macro-dspec name))
                       (setf ,(swank-setf-dspec name))
                       (generic-function ,(swank-generic-function-dspec name))
                       (setf-generic-function
                        ,(swank-generic-function-dspec setf-name))
                       (method-combination
                        ,(swank-method-combination-dspec name))
                       (type ,(swank-type-dspec name))
                       (class ,(swank-class-dspec name))
                       (structure ,(swank-structure-dspec name))
                       (condition ,(swank-condition-dspec name)))
                do (assert dspec*)
                   ;; This could be just (EQUAL DSPEC* DSPEC), but
                   ;; GENERIC-FUNCTIONs sometimes have their arglist
                   ;; in DSPEC. Also, see SWANK-FUNCTION-DSPEC and
                   ;; SWANK-CLASS-DSPEC on ABCL.
                   (when (alexandria:starts-with-subseq dspec* dspec
                                                        :test #'equal)
                     #+allegro
                     (when (and (eq locative-type* 'variable)
                                (constantp name))
                       (setq locative-type* 'constant))
                     #+(or allegro ccl cmucl ecl)
                     (when (eq locative-type* 'function)
                       (cond ((or (name-of-macro-p name)
                                  (special-operator-p* name))
                              (setq locative-type* 'macro))
                             ((typep (ignore-errors (symbol-function name))
                                     'generic-function)
                              (setq locative-type* 'generic-function))))
                     #+allegro
                     (when (and (eq locative-type* 'type)
                                (find-class name nil))
                       (setq locative-type* 'class))
                     #+(or abcl allegro ccl cmucl ecl)
                     (when (eq locative-type* 'class)
                       (when (subtypep name 'condition)
                         (setq locative-type* 'condition)))
                     (return-from lazy-wasteful-parsing
                       (dref name locative-type*))))))))

(defun package-dspec-to-definition (dspec)
  (if (and (listp dspec)
           (= 2 (length dspec))
           (member (first dspec) '(package defpackage)))
      (dref (second dspec) 'package)
      nil))

;;; METHOD-DSPEC-TO-DEFINITION is the inverse of SWANK-METHOD-DSPEC and
;;; SWANK-ACCESSOR-DSPEC.
#-(or allegro ccl)
(defun method-dspec-to-definition (name dspec)
  (when (member (first dspec) '(or method defmethod))
    (let ((generic-fn (ignore-errors (fdefinition* name))))
      (when (typep generic-fn 'generic-function)
        (multiple-value-bind (qualifiers specializers)
            (parse-dspec-method-qualifiers-and-specializers (nthcdr 2 dspec)
                                                            generic-fn)
          (let ((name (second dspec))
                (locative `(method ,@qualifiers ,specializers)))
            (dref name locative)))))))

;;; (:AFTER (EQL 5) CLASS-NAME) => (:AFTER) ((EQL 5) CLASS-NAME)
;;;
;;; Allegro and others:
;;; (:AFTER ((EQL 5) CLASS-NAME)) => (:AFTER) ((EQL 5) CLASS-NAME)
(defun parse-dspec-method-qualifiers-and-specializers (list &optional fn)
  #+(or abcl allegro ccl cmucl)
  (declare (ignore fn))
  #+(or abcl allegro ccl cmucl)
  (values (butlast list) (first (last list)))
  #-(or abcl allegro ccl cmucl)
  ;; What's a qualifier and what's a specializer in (DEFMETHOD :OR
  ;; MY-COMB NUMBER) is ambiguous without knowing GF-N-REQUIRED-ARGS.
  (let ((n-qualifiers (- (length list) (gf-n-required-args fn))))
    (values (subseq list 0 n-qualifiers)
            (subseq list n-qualifiers))))

(defun gf-n-required-args (fn)
  (loop for arg in (function-arglist fn)
        while (not (member arg '(&optional &rest &key)))
        count 1))

#+allegro
(defun method-dspec-to-definition (name dspec)
  (declare (ignore name))
  ;; This screams for a pattern matcher.
  ;; (:OPERATOR (METHOD FOO :AFTER ((EQL 5) CLASS-NAME)))
  ;; (:TYPE (METHOD FOO (T CLASS-NAME)))
  (when (and (listp dspec)
             (member (first dspec) '(:operator :type))
             (= (length dspec) 2)
             (listp (second dspec)))
    (let* ((dspec (second dspec))
           (name (second dspec)))
      ;; (METHOD FOO :AFTER ((EQL 5) CLASS-NAME))
      ;; (METHOD FOO (T CLASS-NAME))
      (when (and (listp dspec)
                 (eq (first dspec) 'method)
                 (<= 3 (length dspec)))
        (multiple-value-bind (qualifiers specializers)
            (parse-dspec-method-qualifiers-and-specializers (nthcdr 2 dspec))
          (dref name `(method ,@qualifiers ,specializers)))))))

#+ccl
(defun method-dspec-to-definition (name dspec)
  (declare (ignore name))
  (cond ((eq (first dspec) :method)
         (multiple-value-bind (qualifiers specializers)
             (parse-dspec-method-qualifiers-and-specializers (nthcdr 2 dspec))
           (when (= (length specializers) 1)
             (dref (second dspec)
                   `(method ,@qualifiers ,(objects-to-specializers
                                           specializers))))))
        ((eq (first dspec) 'ccl::reader-method)
         ;; E.g. (CCL::READER-METHOD (:METHOD FOO (#<STANDARD-CLASS CCC>)))
         (destructuring-bind (method-keyword method-name classes) (second dspec)
           (when (eq method-keyword :method)
             (dref method-name
                   `(reader ,@(mapcar #'class-name classes))))))
        ((eq (first dspec) 'ccl::writer-method)
         ;; E.g. (CCL::WRITER-METHOD (:METHOD FOO (#<BUILT-IN-CLASS T>
         ;; #<STANDARD-CLASS CCC>)))
         (destructuring-bind (method-keyword method-name classes) (second dspec)
           (when (and (eq method-keyword :method)
                      (= (length classes) 2))
             (let ((specializers (mapcar #'class-name classes)))
               (dref method-name `(writer ,(second specializers)))))))))
