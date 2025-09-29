(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.client
  :in :eclector.reader)

;;; Test customizing INTERPRET-SYMBOL

(defvar *mock-packages*)

(defclass mock-package ()
  ((%name    :initarg  :name
             :reader   name)
   (%symbols :initarg  :symbols
             :reader   %symbols
             :initform (make-hash-table :test #'equal))))

(defclass mock-symbol ()
  ((%name    :initarg :name
             :reader  name)
   (%package :initarg :package
             :reader  %package)))

(defun %make-symbol (name package)
  (make-instance 'mock-symbol :name name :package package))

(defun make-mock-packages ()
  (alexandria:alist-hash-table
   `((#1="CL" . ,(let ((package (make-instance 'mock-package :name #1#)))
                   (setf (gethash #2="NIL" (%symbols package))
                         (%make-symbol #2# package)
                         (gethash #3="LIST" (%symbols package))
                         (%make-symbol #3# package))
                   package))
     (#4="KEYWORD" . ,(make-instance 'mock-package :name #4#))
     (#5="BAR" . ,(let ((package (make-instance 'mock-package :name #5#)))
                    (setf (gethash #6="BAZ" (%symbols package))
                          (%make-symbol #6# package))
                    package)))
   :test #'equal))

(defclass mock-symbol-client ()
  ())

(defmethod eclector.reader:interpret-symbol
    ((client mock-symbol-client) input-stream
     (package-indicator null) symbol-name internp)
  (declare (ignore input-stream internp))
  (%make-symbol symbol-name nil))

(defmethod eclector.reader:interpret-symbol
    ((client mock-symbol-client) input-stream
     package-indicator symbol-name internp)
  (let ((package (case package-indicator
                   (:current (error "not implemented"))
                   (:keyword (gethash "KEYWORD" *mock-packages*))
                   (t (or (gethash package-indicator *mock-packages*)
                          (eclector.reader::package-does-not-exist
                           input-stream package-indicator symbol-name
                           internp))))))
    (if internp
        (alexandria:ensure-gethash symbol-name (%symbols package)
                                   (%make-symbol symbol-name package))
        (or (gethash symbol-name (%symbols package))
            (eclector.reader::symbol-does-not-exist
             input-stream package symbol-name)))))

(test interpret-symbol/customize
  "Test customizing the behavior of INTERPRET-SYMBOL."
  (let ((*mock-packages* (make-mock-packages)))
    (do-stream-input-cases ((input length) expected-package-or-condition
                            &optional (expected-symbol-or-position length)
                                      (expected-length 1))
      (flet ((do-it ()
               (let ((eclector.reader:*client* (make-instance 'mock-symbol-client)))
                 (with-stream (stream) (eclector.reader:read stream)))))
        (error-case (input expected-package-or-condition expected-symbol-or-position expected-length)
          (error (do-it))
          ((nil)
           (let ((result (do-it)))
             (is (null (%package result)))
             (expect "name" (equal expected-symbol-or-position (name result)))))
          (t
           (let* ((result (do-it))
                  (expected-package (gethash expected-package-or-condition
                                             *mock-packages*))
                  (expected-symbol (gethash expected-symbol-or-position
                                            (%symbols expected-package))))
             (expect "name"    (eq expected-symbol  result))
             (expect "package" (eq expected-package (%package result)))))))
      '(;; Uninterned
        ("#:foo"    nil       "FOO")
        ;; Non-existent package
        ("baz:baz"  eclector.reader:package-does-not-exist 0 3)
        ;; Keyword
        (":foo"     "KEYWORD" "FOO")
        ;; COMMON-LISP package
        ("cl:nil"   "CL"      "NIL")
        ("cl:list"  "CL"      "LIST")
        ;; User package
        ("bar:baz"  "BAR"     "BAZ")
        ("bar:fez"  eclector.reader:symbol-does-not-exist 4 3)
        ("bar::fez" "BAR"     "FEZ")))))

;;; Test customizing FIND-CHARACTER

(defclass find-character-client ()
  ())

(defmethod eclector.reader:find-character
    ((client find-character-client) (designator string))
  (if (string-equal designator "NO_SUCH_CHARACTER")
      nil
      #\a))

(defmethod eclector.reader:find-character
    ((client find-character-client) (designator (eql #\B)))
  #\C)

(test find-character/customize
  "Test customizing the behavior of FIND-CHARACTER."
  (do-stream-input-cases ((input length)
                          expected &optional (expected-position length)
                                             (expected-length 1))
    (flet ((do-it ()
             (let ((eclector.reader:*client*
                     (make-instance 'find-character-client)))
               (with-stream (stream) (eclector.reader:read stream)))))
      (error-case (input expected expected-position expected-length)
        (error (do-it))
        (t (expect "character" (equal expected (do-it))))))
    '(;; Errors
      ("#\\no_such_character" eclector.reader:unknown-character-name 2 17)
      ("#\\NO_SUCH_CHARACTER" eclector.reader:unknown-character-name 2 17)
      ;; Single character
      ("#\\a"                 #\a)
      ("#\\A"                 #\A)
      ("#\\b"                 #\b)
      ("#\\B"                 #\C)
      ;; Multiple characters
      ("#\\name"              #\a)
      ("#\\Name"              #\a)
      ("#\\NAME"              #\a))))

;;; Test customizing EVALUATE-EXPRESSION

(defclass evaluate-expression-client ()
  ())

(defmethod eclector.reader:evaluate-expression
    ((client evaluate-expression-client) (expression (eql 1)))
  (error "foo"))

(defmethod eclector.reader:evaluate-expression
    ((client evaluate-expression-client) (expression t))
  nil)

(test evaluate-expression/customize
  "Test customizing the behavior of EVALUATE-EXPRESSION."
  (do-stream-input-cases ((input length)
                          expected &optional (expected-position length))
    (flet ((do-it ()
             (let ((eclector.reader:*client*
                     (make-instance 'evaluate-expression-client)))
               (with-stream (stream) (eclector.reader:read stream)))))
      (error-case (input expected expected-position)
        (error (do-it))
        (t (is (equal expected (do-it))))))
    '(;; Errors
      ("(1 #.1 3)"          eclector.reader:read-time-evaluation-error 5)
      ;; No errors
      ("(1 #.2 3)"          (1 nil 3))
      ("(1 #.(list #.2) 3)" (1 nil 3)))))

;;; Test customizing {CHECK,EVALUATE}-FEATURE-EXPRESSION

(defclass feature-expression-client ()
  ())

(defmethod eclector.reader:check-feature-expression
    ((client feature-expression-client)
     (feature-expression t))
  (or (typep feature-expression '(cons (eql :version-at-least) (cons string null)))
      (call-next-method)))

(defmethod eclector.reader:evaluate-feature-expression
    ((client feature-expression-client)
     (feature-expression (eql :my-special-feature)))
  (eclector.reader:check-feature-expression client feature-expression)
  t)

(defmethod eclector.reader:evaluate-feature-expression
    ((client feature-expression-client)
     (feature-expression cons))
  (case (first feature-expression)
    (:not
     (eclector.reader:check-feature-expression client feature-expression)
     (eclector.reader:evaluate-feature-expression
      client (second feature-expression)))
    (:version-at-least
     (eclector.reader:check-feature-expression client feature-expression)
     t)
    (t
     (call-next-method))))

(test evaluate-feature-expression/customize
  "Test customizing the behavior of EVALUATE-FEATURE-EXPRESSION."
  (do-stream-input-cases ((input length)
                          expected &optional (expected-position length))
    (flet ((do-it ()
             (let ((eclector.reader:*client*
                     (make-instance 'feature-expression-client)))
               (with-stream (stream) (eclector.reader:read stream)))))
      (error-case (input expected expected-position)
        (error (do-it))
        (t (expect result (eq expected (do-it))))))
    '(;; Errors
      ("#+(not a b)                1 2" eclector.reader:single-feature-expected       10)
      ("#+(version-at-least)       1 2" eclector.reader:feature-expression-type-error 19)
      ("#+(version-at-least 1)     1 2" eclector.reader:feature-expression-type-error 21)
      ;; No errors
      ("#+common-lisp              1 2" 1)
      ("#+(not common-lisp)        1 2" 1)
      ("#+my-special-feature       1 2" 1)
      ("#+(and my-special-feature) 1 2" 1)
      ("#+(version-at-least \"1\") 1 2" 1))))

;;; Test customizing CALL-WITH-STATE-VALUE
;;;
;;; The client intercepts requests to change the current package and
;;; changes the current package to some other, fixed package.

(defclass with-state-value-client ()
  ())

(defmethod eclector.reader:call-with-state-value
    ((client with-state-value-client)
     (thunk t)
     (aspect (eql '*package*))
     (value t))
  ;; VALUE is a string designator, but the default method accepts
  ;; package designators, so supplying a package object is fine.
  (let ((package (find-package '#:eclector.reader.test)))
    (call-next-method client thunk aspect package)))

(defmethod eclector.reader:check-feature-expression
    ((client with-state-value-client)
     (feature-expression t))
  (labels ((check (expression)
             (typecase expression
               ((and symbol (not null))
                (is (eq (find-package '#:eclector.reader.test)
                        (symbol-package expression))))
               (cons
                (check (car expression))
                (check (cdr expression))))))
    (check feature-expression)))

(defmethod eclector.reader:evaluate-feature-expression
    ((client with-state-value-client)
     (feature-expression t))
  (eclector.reader:check-feature-expression client feature-expression)
  t)

(test call-with-state-value/customize
  "Test customizing the behavior of CALL-WITH-STATE-VALUE."
  ;; The custom client should intern the symbols within the feature
  ;; expressions into this test package instead of the keyword
  ;; package.  The check that this works is in the
  ;; CHECK-FEATURE-EXPRESSION method.
  (do-input-cases (input)
    (let ((eclector.reader:*client* (make-instance 'with-state-value-client)))
      (eclector.reader:read-from-string input))
    '(("#+foo       1 2")
      ("#+(bar baz) 1 2"))))

;;; Test (setf state-value)

(defun change-read-base (stream char)
  (declare (ignore char))
  (let ((new-base (eclector.reader:read stream t nil t)))
    (setf (eclector.reader:state-value eclector.base:*client* '*read-base*)
          new-base)
    (values)))

(defun change-package (stream char)
  (declare (ignore char))
  (let ((new-base (eclector.reader:read stream t nil t)))
    (setf (eclector.reader:state-value eclector.base:*client* '*package*)
          new-base)
    (values)))

(test setf-state-value/smoke
  "Smoke test for the (setf state-value) generic function."
  (let ((readtable (eclector.readtable:copy-readtable
                    eclector.reader:*readtable*)))
    (eclector.readtable:set-macro-character readtable #\! 'change-read-base)
    (eclector.readtable:set-macro-character readtable #\@ 'change-package)
    (flet ((test-case (expected input)
             ;; Bind `*read-base*' and `*package*' to protect the
             ;; global values.
             (let ((result (let ((*read-base* *read-base*)
                                 (*package* (find-package "CL-USER")))
                             (eclector.reader:call-with-state-value
                              eclector.base:*client*
                              (lambda ()
                                (eclector.reader:read-from-string input))
                              '*readtable* readtable))))
               (is (equal expected result)))))
      (test-case '(11 17)
                 "(11 !16 11)")
      (test-case '(11 17 23)
                 "(11 !16 11 !16 11)")
      (test-case '(cl-user::foo :foo)
                 "(foo @\"KEYWORD\" foo)")
      (test-case '(cl-user::foo :foo cl-user::baz)
                 "(foo @\"KEYWORD\" foo @\"CL-USER\" baz)"))))

;;; Test customizing labeled object processing

(defclass custom-labeled-objects-client () ())

(defvar *labels* nil)

(defmethod eclector.reader:call-with-label-tracking
    ((client custom-labeled-objects-client) (thunk function))
  (let ((*labels* '()))
    (funcall thunk)))

(defstruct (%labeled-object (:constructor %make-labeled-object (label)))
  (label (error "required"))
  (object nil)
  (state :defined))

(defmethod eclector.reader:note-labeled-object
    ((client custom-labeled-objects-client)
     (input-stream stream)
     (label integer)
     parent)
  (let ((labeled-object (eclector.reader:make-labeled-object
                         client input-stream label parent)))
    (push (cons label labeled-object) *labels*)
    labeled-object))

(defmethod eclector.reader:forget-labeled-object
    ((client custom-labeled-objects-client) (label integer))
  (setf *labels* (remove label *labels* :key #'car)))

(defmethod eclector.reader:find-labeled-object
    ((client custom-labeled-objects-client) (label integer))
  (alexandria:assoc-value *labels* label))

(defmethod eclector.reader:make-labeled-object
    ((client custom-labeled-objects-client)
     (input-stream stream)
     label
     parent)
  (declare (ignore parent))
  (%make-labeled-object label))

(defmethod eclector.reader:labeled-object-state
    ((client custom-labeled-objects-client) (object %labeled-object))
  (values (%labeled-object-state object)
          (%labeled-object-object object)))

(defmethod eclector.reader:finalize-labeled-object
    ((client custom-labeled-objects-client)
     (labeled-object %labeled-object)
     object)
  (let ((new-state (case (%labeled-object-state labeled-object)
                     (:defined  :final)
                     (:circular :final/circular))))
    (setf (%labeled-object-object labeled-object) object
          (%labeled-object-state labeled-object) new-state)
    (values labeled-object new-state)))

(defmethod eclector.reader:reference-labeled-object
    ((client custom-labeled-objects-client)
     (input-stream stream)
     (labeled-object %labeled-object))
  (multiple-value-bind (state object)
      (eclector.reader:labeled-object-state client labeled-object)
    (ecase state
      ((:final :final/circular) ; Use final object, if it has already been stored
       object)
      (:defined ; Else, use LABELED-OBJECT as a placeholder, fix up later
       (setf (%labeled-object-state labeled-object) :circular)
       labeled-object)
      (:circular ; Same but without changing the state
       labeled-object))))

(defmethod eclector.reader:fixup-graph-p ((client custom-labeled-objects-client)
                                          (root-labeled-object %labeled-object))
  (eq (%labeled-object-state root-labeled-object) :final/circular))

(test custom-labeled-objects/smoke
  "Smoke test for custom labeled object processing."
  (is (equal* '(a #1=(b #1# c #1# d) e #1# f)
              (let ((eclector.base:*client*
                      (make-instance 'custom-labeled-objects-client)))
                (eclector.reader:read-from-string
                 "(a #1=(b #1# c #1# d) e #1# f)")))))

;;; Test customizing labeled object references

(defclass label-reference-annotation-mixin () ())

;;; Clients may define various methods on REFERENCE-LABELED-OBJECT and
;;; make use of CALL-NEXT-METHOD as well as call
;;; REFERENCE-LABELED-OBJECT recursively. We want to annotate only
;;; once and only the result of the outermost call.
(defvar *outermostp* t)
(defmethod eclector.reader:reference-labeled-object
    :around ((client label-reference-annotation-mixin)
             (input-stream stream)
             (labeled-object t))
  (if *outermostp*
      (let ((state (eclector.reader:labeled-object-state
                    client labeled-object))
            (result (let ((*outermostp* nil))
                      (call-next-method))))
        (ecase state
          ((:final :final/circular)
           `(:ordinary-reference ,result))
          (:defined
           `(:circular-reference ,result))
          (:circular
           `(:another-circular-reference ,result))))
      (call-next-method)))

(defclass annotating-custom-labeled-objects-client
    (label-reference-annotation-mixin
     custom-labeled-objects-client)
  ())

(defmethod eclector.reader:reference-labeled-object
    ((client annotating-custom-labeled-objects-client)
     (input-stream stream)
     (labeled-object t))
  ;; Make sure multiple applicable methods do not interfere with the
  ;; annotation.
  (call-next-method))

(test custom-labeled-objects/annotation
  "Test custom labeled object reference processing."
  (is (equal* '(a #1=(b (:circular-reference #1#)
                      c (:another-circular-reference #1#)
                      d)
                e (:ordinary-reference #1#) f)
              (let ((eclector.base:*client*
                      (make-instance 'annotating-custom-labeled-objects-client)))
                (eclector.reader:read-from-string
                 "(A #1=(b #1# c #1# d) e #1# f)")))))
