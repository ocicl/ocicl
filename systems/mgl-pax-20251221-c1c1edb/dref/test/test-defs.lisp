(in-package :dref-test)

(define-locative-type my-loc ()
  "This is MY-LOC.")

(define-locative-type (loc-with-args (x y) &key z) ()
  "LOC-WITH-ARGS locative")

(defun foo2 (ook x)
  "FOO2 has args OOK and X."
  (declare (ignore ook x))
  nil)

(defun |Foo| ())

(defun traced-foo (x)
  "TRACED-FOO function"
  x)
(handler-bind ((warning #'muffle-warning))
  (trace traced-foo))
(defgeneric traced-gf (x)
  (:documentation "TRACED-GF generic-function"))
(handler-bind ((warning #'muffle-warning))
  (trace traced-gf))
(defun foo (ook x)
  "FOO function"
  (declare (ignore ook x))
  nil)
(define-compiler-macro foo ()
  "FOO compiler-macro"
  nil)
(define-compiler-macro cmac (x &rest y)
  "CMAC compiler-macro"
  (declare (ignore x y))
  nil)
(defclass foo (unexported-class)
  ((a :accessor foo-a
      :documentation "FOO-A (accessor foo)")
   (r :reader foo-r
      :documentation "FOO-R (reader foo)")
   (w :writer foo-w
      :documentation "FOO-W (writer foo)"))
  (:documentation "FOO class"))
(defclass unexported-class () ())
(defclass subclass (class) ())
(defvar foo-a nil
  "FOO-A variable")
(defvar foo-r)
(defvar foo-w)

(defparameter *test-variable*
  '(xxx 34)
  "*TEST-VARIABLE* is not a link.")
(defvar *some-var*)

(define-restart some-restart (arg1)
  "This is SOME-RESTART with ARG1.")

(define-condition my-error (error)
  ()
  (:documentation "MY-ERROR condition"))
(defun my-error ()
  "This is MY-ERROR."
  t)

(defmacro bar (x y &key (z 7))
  "BAR macro"
  (declare (ignore x y z))
  nil)
(deftype type-number () 'number)
(deftype type-subclass () 'subclass)
(deftype type-or-number-and-subclass () '(or number subclass))
(deftype bar (x &rest r)
  "BAR type"
  (declare (ignore x r))
  'null)
(defconstant bar 2
  "BAR constant")

(define-symbol-macro my-smac 42)
(setf (documentation 'my-smac 'symbol-macro)
      "MY-SMAC symbol-macro")

(defgeneric baz ())
;; KLUDGE: CMUCL clobbers the DEFVAR's source location with that of
;; the DEFSTRUCT if they have the same name.
(defvar bazz)
(defstruct* baz
  (aaa nil :documentation "BAZ-AAA (structure-accessor baz)"))

(defgeneric test-gf (x)
  (:documentation "TEST-GF generic-function"))
(defmethod test-gf ((x number))
  "TEST-GF (method (number))"
  nil)
(defmethod test-gf ((x (eql 7))))
(defmethod test-gf ((x (eql #.(find-package :common-lisp)))))

(defgeneric gf2 (x &key &allow-other-keys)
  (:method :around (x &key)
    (declare (ignore x)))
  (:method :after (x &key y)
    (declare (ignore x y)))
  (:method ((x number) &key ((:x y) t))
    (declare (ignore y))))

(define-method-combination my-comb  (&optional (order :most-specific-first))
  ((around (:around))
   (primary (my-comb) :order order :required t))
  "MY-COMB method-combination"
  (let ((form (if (rest primary)
                  `(and ,@(mapcar #'(lambda (method)
                                      `(call-method ,method))
                                  primary))
                  `(call-method ,(first primary)))))
    (if around
        `(call-method ,(first around)
                      (,@(rest around)
                       (make-method ,form)))
        form)))

(defgeneric gf3 (x &key z)
  (:method-combination my-comb)
  ;; On CLISP, this is a compile error.
  #-clisp
  (:method :after (x &key z)
    (declare (ignore x z)))
  #-clisp
  (:method my-comb :after ((x number) &key z)
    (declare (ignore z))))

(defun ->max ())

#+sbcl
(require :sb-cltl2)

(defmacro define-declaration (name)
  #-(or allegro ccl sbcl) (declare (ignore name))
  #+allegro
  `(system:define-declaration ,name () nil :both)
  #+ccl
  `(ccl:define-declaration ,name (decl-spec env)
     (declare (ignore env))
     (values :declare decl-spec))
  #+sbcl
  `(sb-cltl2:define-declaration ,name (decl-spec env)
     (declare (ignore env))
     (values :declare decl-spec)))

(define-declaration test-declaration)

(unless (named-readtables:find-readtable 'xxx-rt)
  (named-readtables:defreadtable xxx-rt
    ;; KLUDGE: ABCL bundles an older named-readtables version that
    ;; does not support docstrings.
    #-abcl
    "ddd"))

(defsetf short-setf-with-fn some-setter "SHORT-SETF-WITH-FN doc")
(defsetf short-setf-with-undefined-fn junk "SHORT-SETF-WITH-UNDEFINED-FN doc")
(defsetf short-setf-with-macro setter-macro "SHORT-SETF-WITH-MACRO doc")

(defun some-setter (x)
  (declare (ignore x)))
(defmacro setter-macro (x (&key y))
  (declare (ignore x y)))

(defun (setf setf-fn) (v)
  "SETF-FN setf"
  (declare (ignore v)))

;;; This is defined to maybe trip METHOD-DSPEC-TO-DEFINITION up.
(defgeneric setf-gf ()
  (:method ()))

(defgeneric (setf setf-gf) (v)
  (:documentation "SETF-GF setf"))

(defmethod (setf setf-gf) ((v string))
  "SETF-GF (setf-method (string))"
  ())

(define-compiler-macro (setf setf-fn) (v)
  v)

(define-setf-expander long-setf (x)
  (declare (ignore x)))

(defun long-setf ())

;;; On ECL, this lambda gets named SETFED-FUN when assigned.
(setf (symbol-function 'setfed-fun0) (lambda (x) (1+ x)))
;;; But its name is not changed here, so this not going to be a
;;; DREF::CONSISTENT-FDEFINITION.
(setf (symbol-function 'setfed-fun) (symbol-function 'setfed-fun0))

(setf (macro-function 'setfed-macro)
      (lambda (whole env)
        (declare (ignore whole env))))

(defmacro macro-with-fancy-args (x &optional (o 1) &key (k 2 kp))
  (declare #+sbcl (sb-ext:muffle-conditions style-warning)
           (ignore x o k kp))
  ())

(defmacro macro-with-local-key ((&key a) (b print))
  (declare #+sbcl (sb-ext:muffle-conditions style-warning)
           (ignore a b print))
  ())

(defun function-with-fancy-args (x &optional (o 1) &key (k 2 kp))
  (declare #+sbcl (sb-ext:muffle-conditions style-warning)
           (ignore x o k kp))
  nil)

(define-symbol-locative-type (sloc &optional nested) ()
  "SLOC locative")

(define-definer-for-symbol-locative-type define-sloc sloc)

(define-sloc sloc1 (&key z)
  "SLOC1 sloc")

(define-locative-alias object class)
(define-locative-alias var variable
  "VAR locative")


(defvar *x* 1)

(eval-when (:compile-toplevel)
  (declaim (optimize (debug 0))))

(deftype debug0-non-constant-type ()
  `(member ,*x*))


(defparameter *failure-on-long-setf*
  (and (not (alexandria:featurep '(:or :ccl :clisp :sbcl)))
       'failure))

(defparameter *failure-on-setf-arglist*
  (and (not (alexandria:featurep :sbcl))
       'failure))
