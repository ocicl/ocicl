(uiop:define-package #:40ants-doc/locatives/base
  (:use #:cl)
  (:import-from #:40ants-doc/source-api)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader)
  (:export
   #:locate
   #:locate-error
   #:locate-error-message
   #:locate-error-object
   #:locate-error-locative
   #:locative-type
   #:locative-args
   #:define-locative-type
   #:locate-object
   #:locate-and-find-source
   #:locative-equal))
(in-package #:40ants-doc/locatives/base)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(defmacro define-locative-type (locative-type lambda-list &body docstring)
  """Declare LOCATIVE-TYPE as a locative. One gets two
  things in return: first, a place to document the format and
  semantics of LOCATIVE-TYPE (in LAMBDA-LIST and DOCSTRING); second,
  being able to reference `(LOCATIVE-TYPE LOCATIVE)`. For example, if
  you have:

  ```lisp
  (define-locative-type variable (&optional initform)
    "Dummy docstring.")
  ```

  then `(VARIABLE LOCATIVE)` refers to this form."""
  (assert (or (endp docstring)
              (and (= 1 (length docstring))
                   (string (first docstring)))))
  `(defmethod locative-lambda-list ((symbol (eql ',locative-type)))
     ,@docstring
     ',lambda-list))


;;; A somewhat dummy generic function on which the docstring can be
;;; hung and which provides a source location. It returns LAMBDA-LIST
;;; from DEFINE-LOCATIVE-TYPE.
(defgeneric locative-lambda-list (symbol))

(defgeneric locate-object (object locative-type locative-args)
  (:documentation "Return the object, to which OBJECT and the locative
  refer. For example, if LOCATIVE-TYPE is the symbol PACKAGE, this
  returns `(FIND-PACKAGE SYMBOL)`. Signal a LOCATE-ERROR condition by
  calling the LOCATE-ERROR function if the lookup fails. Signal other
  errors if the types of the argument are bad, for instance
  LOCATIVE-ARGS is not the empty list in the package example. If a
  40ANTS-DOC/REFERENCE:REFERENCE is returned then it must be canonical in the sense that
  calling 40ANTS-DOC/REFERENCE-API:CANONICAL-REFERENCE on it will return the same reference.
  For extension only, don't call this directly."))

(define-condition locate-error (error)
  ((message :initarg :message :reader locate-error-message)
   (object :initarg :object :reader locate-error-object)
   (locative :initarg :locative :reader locate-error-locative))
  (:documentation "Signaled by LOCATE when the lookup fails and ERRORP
  is true.")
  (:report (lambda (condition stream)
             (format stream "~@<Could not locate ~A ~A.~@[ ~A~]~:@>"
                     (locate-error-locative condition)
                     (locate-error-object condition)
                     (locate-error-message condition)))))

(defun locate-error (&rest format-and-args)
  "Call this function to signal a LOCATE-ERROR condition from a
  LOCATE-OBJECT generic-function. FORMAT-AND-ARGS contains a format string and
  args suitable for FORMAT from which the LOCATE-ERROR-MESSAGE is
  constructed. If FORMAT-AND-ARGS is NIL, then the message will be NIL
  too.

  The object and the locative are not specified, they are added by
  LOCATE when it resignals the condition."
  (error 'locate-error :message (if format-and-args
                                    (apply #'format nil format-and-args)
                                    nil)))


(defun locate (object locative &key (errorp t))
  "Follow LOCATIVE from OBJECT and return the object it leads to or a
  40ANTS-DOC/REFERENCE:REFERENCE if there is no first class object corresponding to the
  location. If ERRORP, then a LOCATE-ERROR condition is signaled when
  the lookup fails."
  (handler-case
      (locate-object object (locative-type locative)
                     (locative-args locative))
    (locate-error (e)
      (when errorp
        (error 'locate-error :message (locate-error-message e)
               :object object :locative locative)))))


(defgeneric locate-and-find-source (object locative-type locative-args)
  (:documentation
   "Called by [40ANTS-DOC/SOURCE-API:FIND-SOURCE][(METHOD () (40ANTS-DOC/REFERENCE:REFERENCE))]
    on 40ANTS-DOC/REFERENCE:REFERENCE objects, this
    function has essentially the same purpose as 40ANTS-DOC/SOURCE-API:FIND-SOURCE generic-function but it has
    different arguments to allow specializing on LOCATIVE-TYPE."))

(defmethod locate-and-find-source (object locative-type locative-args)
  "This default implementation simply calls 40ANTS-DOC/SOURCE-API:FIND-SOURCE with OBJECT
  which should cover the common case of a macro expanding to, for
  instance, a defun but having its own locative type."
  (declare (ignore locative-type locative-args))
  (40ants-doc/source-api:find-source object))


;;; A somewhat dummy generic function whose methods are
;;; eql-specialized on SYMBOL and LOCATIVE-TYPE. The appropriate
;;; method's docstring is the docstring of SYMBOL as LOCATIVE-TYPE. As
;;; an afterthought, this method also returns the LAMBDA-LIST given in
;;; the definition.
(defgeneric symbol-lambda-list (symbol locative-type))


(defun symbol-lambda-list-method (symbol locative-type)
  (find-method #'symbol-lambda-list () `((eql ,symbol) (eql ,locative-type))
               nil))


(defgeneric locative-type (locative)
  (:documentation "The first element of LOCATIVE if it's a list. If it's a symbol then
  it's that symbol itself. Typically, methods of generic functions
  working with locatives take locative type and locative args as
  separate arguments to allow methods have eql specializers on the
  type symbol.")

  (:method ((locative list))
    (locative-type (first locative)))

  (:method ((locative symbol))
    locative))


(defgeneric locative-args (locative)
  (:documentation "The REST of LOCATIVE if it's a list. If it's a symbol then
  it's ().")
  (:method ((locative list))
    (rest locative))
  (:method ((locative t))
    nil))


(defun locative-equal (left right)
  "Compares two locatives.

   Each locative may be a symbol or a locative with arugments in a list form."
  (and (eql (locative-type left)
            (locative-type right))
       (equal (locative-args left)
              (locative-args right))))
