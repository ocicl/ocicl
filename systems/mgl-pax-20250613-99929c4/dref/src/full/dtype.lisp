(cl:in-package :dref)

(in-readtable pythonic-string-syntax)

(defun dtypexpand (dtype-specifier)
  ;; NIL expands to NIL
  (when dtype-specifier
    (destructuring-bind (name &rest args) (ensure-list dtype-specifier)
      (let ((expander (gethash name *dtype-expanders*)))
        (if expander
            (values (apply expander args) t)
            (values dtype-specifier nil))))))

(define-dtype top ()
  "This is the top of the DTYPE hierarchy, much like T for Lisp types.
  It expands to ([OR][type] T PSEUDO). While T matches every normal
  Lisp object and objectless definitions present in the running
  Lisp (see LISP-LOCATIVE-TYPES), TOP matches even pseudo
  definitions (see PSEUDO-LOCATIVE-TYPES)."
  '(or t pseudo))

(define-dtype pseudo ()
  "This is the union of all PSEUDO-LOCATIVE-TYPES. It expands
  to `(OR ,@(PSEUDO-LOCATIVE-TYPES))`."
  `(or ,@(pseudo-locative-types)))


(defun/autoloaded dtypep (dref dtype)
  """See if DREF is of DTYPE.

  - _[Atomic locatives][@locative]:_ If DTYPE is a @LOCATIVE-TYPE,
    then it matches definitions with that locative type and its
    locative subtypes.

      Because [CONSTANT][locative] is defined with VARIABLE among its
      [ LOCATIVE-SUPERTYPES] [ define-locative-type]:

      ```cl-transcript (:dynenv dref-std-env)
      (dtypep (dref 'pi 'constant) 'variable)
      => T
      ```

      ```cl-transcript (:dynenv dref-std-env)
      (dtypep (dref 'number 'class) 'type)
      => T
      ```

      It is an error if DTYPE is an ATOM but is not a @LOCATIVE-TYPE,
      but (the empty) argument list of bare locative types are not
      checked even if having no arguments makes them [invalid
      locatives][@locative].

  - _[Compound locatives][@locative]:_ Locatives in their compound
    form are validated and must match exactly (under EQUAL, as in
    XREF=).

      ```cl-transcript
      (defparameter *d* (dref 'dref* '(method (t t t))))
      (defparameter *d2* (dref 'dref* '(method :around (t t t))))
      (dtypep *d* 'method)
      => T
      (dtypep *d* '(accessor))
      .. debugger invoked on SIMPLE-ERROR:
      ..   Bad arguments NIL for locative ACCESSOR with lambda list (CLASS-NAME).
      (dtypep *d* '(method (t t t)))
      => T
      (dtypep *d2* '(method (t t t)))
      => NIL
      ```

  - DTYPE may be constructed with [AND][type], [OR][type] and
    [NOT][type] from Lisp types, locative types, full locatives and
    named DTYPEs:

      ```cl-transcript (:dynenv dref-std-env)
      (dtypep (dref 'locate-error 'condition) '(or condition class))
      => T
      (dtypep (dref nil 'type) '(and type (not class)))
      => T
      ```

  - For `(MEMBER &REST OBJS)`, each of OBJS is LOCATEd and DREF is
    matched against them with XREF=:

      ```cl-transcript (:dynenv dref-std-env)
      (dtypep (locate #'print) `(member ,#'print))
      => T
      ```

  - For `(SATISFIES PRED)`, the predicate PRED is funcalled with DREF.

  - DTYPE may be named by DEFINE-DTYPE:

      ```cl-transcript (:dynenv dref-std-env)
      (dtypep (locate #'car) 'top)
      => T
      ```"""
  (declare (type dref dref))
  (let* ((d-name (dref-name dref))
         (d-locative (dref-locative dref))
         (d-locative-type (locative-type d-locative)))
    (labels
        ((d-is-of-atomic-locative-p (locative-type)
           (locative-subtype-p d-locative-type locative-type))
         (d-is-of-compound-locative-p (locative)
           (or
            ;; Pick off the exact match case.
            (equal locative d-locative)
            ;; @CAST-NAME-CHANGE: Upcasting or
            ;; SAME-DEFINITION-WITH-LOCATIVE-P works.
            (and (locative-subtype-p d-locative-type (first locative))
                 (when-let (upcast (locate* dref (first locative)))
                   (equal (dref-locative upcast) locative)))
            (same-definition-with-locative-p locative)))
         (same-definition-with-locative-p (locative)
           (when-let (dref1 (dref d-name locative nil))
             (xref= dref1 dref)))
         (recurse (dtype)
           (setq dtype (dtypexpand dtype))
           (cond
             ;; E.g. FUNCTION or METHOD
             ((atom dtype)
              (d-is-of-atomic-locative-p dtype))
             ((eq (first dtype) 'and)
              (loop for child in (rest dtype) always (recurse child)))
             ((eq (first dtype) 'or)
              (loop for child in (rest dtype) thereis (recurse child)))
             ((eq (first dtype) 'not)
              (unless (= (length dtype) 2)
                (invalid-dtype dtype))
              (not (recurse (second dtype))))
             ((member-type-specifier-p dtype)
              (when (find dref (mapcar #'locate (rest dtype)) :test #'xref=)
                t))
             ((satisfies-type-specifier-p dtype)
              (unless (valid-satisisfies-type-specifier-args-p (rest dtype))
                (invalid-dtype dtype))
              (when (funcall (second dtype) dref)
                t))
             ;; E.g. (FUNCTION) or (METHOD (NUMBER))
             (t
              (check-locative-type (first dtype))
              (check-locative-args* (first dtype) (rest dtype))
              (d-is-of-compound-locative-p dtype)))))
      (values (recurse dtype)))))

(defun locative-subtypes (locative-type)
  (cond ((eq locative-type nil)
         ())
        ;; T is not a locative type but a DTYPE. This is just for the
        ;; convenience of COVER-BASIC-DTYPE.
        ((eq locative-type t)
         *lisp-locative-types*)
        (t
         (check-locative-type locative-type)
         (loop for locative-type-1 in (locative-types)
               when (locative-subtype-p locative-type-1 locative-type)
                 collect locative-type-1))))

;;; Return the largest Lisp type that's a subtype of of the class
;;; named LOCATIVE-TYPE. If LOCATIVE-TYPE does not name a class, then
;;; NIL is returned.
(defun widest-subtype-of-locative-type (locative-type)
  (let ((roots ()))
    (labels ((recurse (locative-type)
               (unless (member locative-type roots)
                 (if (find-class locative-type nil)
                     (push locative-type roots)
                     (mapc #'recurse
                           (locative-type-direct-subs locative-type))))))
      (recurse locative-type))
    (case (length roots)
      ((0) nil)
      ((1) (first roots))
      (t `(or ,@roots)))))

;;; Return the smallest containing Lisp type for LOCATIVE-TYPE.
(defun narrowest-supertype-of-locative-type (locative-type)
  (let ((roots ()))
    (labels ((recurse (locative-type)
               (unless (member locative-type roots)
                 (if (find-class locative-type nil)
                     (push locative-type roots)
                     (mapc #'recurse
                           (locative-type-direct-supers locative-type))))))
      (recurse locative-type))
    (case (length roots)
      ((0) t)
      ((1) (first roots))
      (t `(and ,@roots)))))

(defun eql-or-xref= (obj1 obj2)
  (or (eql obj1 obj2)
      (let ((dref1 (locate obj1 nil))
            (dref2 (locate obj2 nil)))
        (and dref1 dref2 (xref= dref1 dref2)))))


;;;; For DREF-APROPOS, querying all locative types is expensive and
;;;; often unnecessary (if its DTYPE argument rules certain locative
;;;; types out) since the set of all definitions is at least in the
;;;; tens of thousands. So, we query like this:
;;;;
;;;; 1. COVER-DTYPE gives a set of locative types whose union contains
;;;;    all definitions of DTYPE, but this upper bound may be loose
;;;;    when a locative's args restrict a locative type further as in
;;;;    (METHOD (NUMBER)).
;;;;
;;;; 2. We gather all definitions with these locative types.
;;;;
;;;; 3. FILTER-DREFS-BY-DTYPE discards the definitions that are not of
;;;;    DTYPE (possible if the upper bound is loose).

(defvar *cover-dtype-cache* nil)

(defmacro with-cover-dtype-cache (&body body)
  `(let ((*cover-dtype-cache* (make-hash-table :test #'equal)))
     ,@body))

(defun cover-dtype (dtype)
  (let ((cache *cover-dtype-cache*))
    (if cache
        (let ((cover (gethash dtype cache 'not-cached)))
          (if (eq cover 'not-cached)
              (setf (gethash dtype cache) (cover-dtype* dtype nil))
              cover))
        (values (cover-dtype* dtype nil)))))

(defun cover-dtype* (dtype negatep)
  ;; Expanding gets rid of one level of derived types (but children
  ;; may still be derived).
  (let ((dtype (dtypexpand dtype)))
    (flet ((child-sets (children)
             (loop for child in children
                   collect (cover-dtype* child negatep))))
      (case (and (not (atom dtype)) (first dtype))
        ((and)
         (reduce #'intersection (child-sets (rest dtype))
                 :initial-value *locative-types*))
        ((or)
         (reduce #'union (child-sets (rest dtype))
                 :initial-value ()))
        ((not)
         (unless (= (length dtype) 2)
           (invalid-dtype dtype))
         (set-difference *locative-types*
                         (cover-dtype* (second dtype) (not negatep))))
        ((member)
         (reduce #'union
                 (mapcar (rcurry #'cover-object-with-locative-types negatep)
                         (rest dtype))
                 :initial-value ()))
        ((satisfies)
         (unless (valid-satisisfies-type-specifier-args-p (rest dtype))
           (invalid-dtype dtype))
         (if negatep
             ()
             *locative-types*))
        (t
         (locative-subtypes (cover-dtype/single dtype negatep)))))))

(defun cover-object-with-locative-types (object negatep)
  (when-let ((dref (locate object nil)))
    (locative-subtypes (cover-dtype/single (dref-locative dref) negatep))))

;;; In the base case, we cover DTYPE with a single locative type, NIL
;;; or T (where NIL and T are not locative types).
(defun cover-dtype/single (dtype negatep)
  (let ((name (locative-type dtype)))
    (cond
      ((locative-type-p name)
       ;; Compound locatives must have valid args.
       (when (listp dtype)
         (let ((args (rest dtype)))
           (check-locative-args* name args)
           (when negatep
             ;; (NOT (METHOD (NUMBER))) is the universe minus an
             ;; atom.
             (return-from cover-dtype/single nil))))
       name)
      ((eq dtype nil) nil)
      ((eq dtype t) t)
      (t (invalid-dtype dtype)))))

(defun invalid-dtype (dtype)
  (error "~@<Invalid ~S ~S.~:@>" 'dtype dtype))


;;; Filter DREFS that match one of the locative types in (COVER-DTYPE
;;; DTYPE), such as when come from DEFINITIONS or DREF-APROPOS, to
;;; match DTYPE.
(defun filter-covered-drefs (drefs dtype)
  (if (inexact-dtype-cover-p dtype)
      (loop for dref in drefs
            when (dtypep dref dtype)
              collect dref)
      drefs))

(defun inexact-dtype-cover-p (dtype)
  (let ((dtype (dtypexpand dtype)))
    (unless (atom dtype)
      (if (member (first dtype) '(and or not))
          (loop for child in (rest dtype)
                  thereis (inexact-dtype-cover-p child))
          ;; This a bit conservative. For example, it deems (FUNCTION)
          ;; inexact.
          t))))
