(in-package :mgl-pax-test)

(defsection @test-examples (:export nil)
  "example section")

(defsection @test-other (:title "test other title" :export nil)
  "backlink @TEST")

(defsection @test-section-with-link-to-other-page-in-title
    (:title "Link to @TEST-OTHER" :link-title-to (@test-other section)
     :export nil)
  "Same link in docstring to @TEST-OTHER.")

(defsection @test-section-with-link-to-same-page-in-title
    (:title "Link to @TEST" :link-title-to (@test section)
     :export nil)
  "Same link in docstring to @TEST.")

(defsection @parent-tricky-title
    (:title "`CODE` *italic* _italic2_ *bold* &quot;"
     :export nil)
  (@tricky-title section))

(defsection @tricky-title
    (:title "`CODE` *italic* _italic2_ *bold* &quot;"
     :export nil)
  (@subtricky section)
  (**subtricky** section))

(defsection @subtricky (:title "\\`\\_\\*\\&" :export nil))

(defsection **subtricky** (:export nil))

;;; LOCATIVE whose name is a symbol in another package.
(define-locative-type (pax::funny-loc some-arg) ()
  "This is SOME-ARG.")

(defun foo2 (ook x)
  "FOO2 has args [OOK][] and X."
  (declare (ignore ook x))
  nil)

(defun ook ())

(defun traced-foo (x)
  "XXX"
  x)
(handler-bind ((warning #'muffle-warning))
  (trace traced-foo))
(defun foo (ook x)
  "FOO has args OOK and X."
  (declare (ignore ook x))
  nil)
(defun |Foo| ())
(defun |F o| ())
(defun |F O| ())
(define-compiler-macro foo ()
  "Docstring of a compiler macro."
  nil)
(defclass foo (unexported-class)
  ((r :reader foo-r)
   (w :writer foo-w)
   (a :accessor foo-a)))
(defclass unexported-class () ())
(defvar foo-a)
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
  (:documentation "This is MY-ERROR."))
(defun my-error ()
  "This is MY-ERROR."
  t)

(defmacro bar (x y &key (z 7))
  "BAR has args X, Y and Z."
  (declare (ignore x y z))
  nil)
(deftype bar (x &rest r)
  "BAR has args X and R."
  (declare (ignore x r))
  'null)
(defconstant bar 2
  "BAR is not a link.")

(define-symbol-macro my-smac 42)
(setf (documentation 'my-smac 'symbol-macro)
      "This is MY-SMAC.")

(defgeneric baz ())
;; KLUDGE: CMUCL clobbers the DEFVAR's source location with that of
;; the DEFSTRUCT if they have the same name.
(defvar bazz)
(defstruct baz
  aaa)

(defgeneric test-gf (x)
  (:documentation "TEST-GF is not a link."))
(defmethod test-gf ((x number))
  "TEST-GF is not a link. X is not a link."
  nil)
(defmethod test-gf ((x (eql 7))))
(defmethod test-gf ((x (eql #.(find-package :common-lisp)))))
(defmethod test-gf ((x (eql :bar))))

(defsection @test-method-combination (:export nil)
  (my-comb method-combination))

(define-method-combination my-comb :identity-with-one-argument t
  :documentation "This is MY-COMB.")

(define-glossary-term @some-term ()
  "SOME-TERM is a link.
  ### heading")

(defun ->max ())

(defmacro define-declaration (decl-name (decl-spec env) &body body)
  #+sbcl
  `(sb-cltl2:define-declaration ,decl-name (,decl-spec ,env)
     ,@body)
  #-sbcl
  (declare (ignore decl-name decl-spec env body)))

(define-declaration test-declaration (decl-spec env)
  (declare (ignore env))
  (values :declare decl-spec))

(unless (named-readtables:find-readtable 'xxx-rt)
  (named-readtables:defreadtable xxx-rt
    ;; KLUDGE: ABCL bundles an older named-readtables version that
    ;; does not support docstrings.
    #-abcl
    "ddd"))

(defmethod exportable-reference-p
    ((package (eql (find-package '#:mgl-pax-test))) symbol
     locative-type locative-args)
  (declare (ignore symbol locative-type locative-args))
  nil)

(defsetf has-setf-expander some-setter "ddd")

(defun (setf setf-fn) (v)
  "eee"
  (declare (ignore v)))

(define-compiler-macro (setf setf-fn) (v)
  v)

(defgeneric (setf setf-gf) (v)
  (:documentation "fff"))

(defmethod (setf setf-gf) ((v string))
  "ggg"
  (declare (ignore v)))

(define-setf-expander full-setf (x)
  (declare (ignore x)))

(defun full-setf ())

(define-locative-type my-loc ())

(defpackage "X Y")

(defsection @inc ()
  (nil (include #.(asdf:system-relative-pathname :mgl-pax "test/inc.md"))))

(defun xxx ())
(defun xxxs ())

(note @1+*
  "### This is a seriously overdone example."
  (defun 1+* (x)
    "@1+*"
    (if (stringp x)
        (note (@1+*/1 :join #\Newline)
          "- If X is a STRING, then it is parsed as a REAL number."
          (let ((obj (read-from-string x)))
            (note "It is an error if X does not contain a REAL."
              (unless (realp obj)
                (assert nil)))
            (1+ obj)))
        (note "- Else, X is assumed to be REAL number, and we simply
                 add 1 to it."
          (1+ x)))))

(defun fn-with-inside-note ()
  (let ((x 7))
    (note @in-1 "dsf"
      (* x 7)
      (note @in-2 "aas"))))

(defun fn-with-mathjax ()
  "$\\hat{x}$_s"
  nil)

(defun $x_0$ ())

(define-glossary-term @mathjax-and-code-in-glossary-term-title
    (:title "hey `c` $x_0$"))

(defsection @mathjax-and-code-in-title (:title "hey `c` $x_0$")
  ($x_0$ function)
  (@mathjax-subsection section))

(defsection @mathjax-subsection ())

(define-glossary-term @illegal-title-1 (:title "x

y"))

(define-glossary-term @title-with-emph (:title "_x_ **y**"))
