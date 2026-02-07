(defpackage #:atomics-test
  (:nicknames #:org.shirakumo.atomics.test)
  (:use #:cl #:parachute)
  (:export
   #:atomics))
(in-package #:org.shirakumo.atomics.test)

(define-test atomics)

(atomics:defstruct struct
  (any NIL :type T)
  (fixnum 0 :type #-sbcl fixnum #+sbcl sb-ext:word))

(defclass clazz ()
  ((slot :initform NIL)))

(defvar *special* :root)

(define-test cas
  :parent atomics)

(defmacro expand-cas-tests (place old new)
  `(progn
     (is eql ,old ,place)
     (true (atomics:cas ,place ,old ,new))
     (is eql ,new ,place)
     (false (atomics:cas ,place ,old ,new))
     (is eql ,new ,place)))

(define-test cas-vector
  :parent cas
  (let ((vector (make-array 1 :initial-element NIL)))
    (expand-cas-tests (svref vector 0) NIL T)))

(define-test cas-struct
  :parent cas
  (let ((instance (make-struct)))
    (expand-cas-tests (struct-any instance) NIL T)
    (expand-cas-tests (struct-fixnum instance) 0 1)))

(define-test cas-class
  :parent cas
  (let ((instance (make-instance 'clazz)))
    (expand-cas-tests (slot-value instance 'slot) NIL T)))

(define-test cas-cons
  :parent cas
  (let ((cons (cons NIL NIL)))
    (expand-cas-tests (car cons) NIL T)
    (expand-cas-tests (cdr cons) NIL T)
    (expand-cas-tests (first cons) T NIL)
    (expand-cas-tests (rest cons) T NIL)))

(define-test cas-symbol-value
  :parent cas
  (let ((symbol (make-symbol "test")))
    (setf (symbol-value symbol) NIL)
    (expand-cas-tests (symbol-value symbol) NIL T)))

#+(or allegro ccl lispworks sbcl)
(define-test cas-special
  :parent cas
  (is eql *special* :root)
  (let ((*special* NIL))
    (expand-cas-tests *special* NIL T))
  (is eql *special* :root))

(define-test atomic-incf
  :parent atomics)

(defmacro expand-atomic-incf-tests (place old delta)
  `(progn
     (is = ,old ,place)
     (is = ,(+ old delta) (atomics:atomic-incf ,place ,delta))
     (is = ,(+ old delta) ,place)))

(define-test atomic-incf-cons
  :parent atomic-incf
  (let ((cons (cons 0 0)))
    (expand-atomic-incf-tests (car cons) 0 1)
    (expand-atomic-incf-tests (cdr cons) 0 10)
    (expand-atomic-incf-tests (first cons) 1 1)
    (expand-atomic-incf-tests (rest cons) 10 10)))

(define-test atomic-incf-vector
  :parent atomic-incf
  #+(or allegro ccl ecl mezzano lispworks)
  (let ((vector (make-array 1 :initial-element 0)))
    (expand-atomic-incf-tests (svref vector 0) 0 1))
  #+(or mezzano sbcl)
  (let ((vector (make-array 1 :initial-element 0 :element-type #-sbcl 'fixnum #+sbcl 'sb-ext:word)))
    (expand-atomic-incf-tests (aref vector 0) 0 1)))

#+(or allegro ccl lispworks mezzano sbcl)
(define-test atomic-incf-struct
  :parent atomic-incf
  (let ((instance (make-struct)))
    (expand-atomic-incf-tests (struct-fixnum instance) 0 1)))

(defmacro expand-atomic-pop-and-push-tests (place)
  `(progn
     (is equal nil (atomics:atomic-pop ,place))
     (is equal (list 1) (atomics:atomic-push 1 ,place))
     (is = 1 (atomics:atomic-pop ,place))
     (is equal nil ,place)
     (is equal (list 2) (atomics:atomic-push 2 ,place))
     (is equal (list 3 2) (atomics:atomic-push 3 ,place))
     (is = 3 (atomics:atomic-pop ,place))
     (is = 2 (atomics:atomic-pop ,place))
     (is equal nil ,place)))

(define-test atomic-pop-and-push
  :parent atomics
  (let ((cons (cons NIL NIL)))
    (expand-atomic-pop-and-push-tests (car cons))
    (expand-atomic-pop-and-push-tests (cdr cons))
    (expand-atomic-pop-and-push-tests (first cons))
    (expand-atomic-pop-and-push-tests (rest cons))))
