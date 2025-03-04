;;;; Anaphora: The Anaphoric Macro Package from Hell
;;;;
;;;; This been placed in Public Domain by the author,
;;;; Nikodemus Siivola <nikodemus@random-state.net>

(defpackage :anaphora-test
  (:use :cl :anaphora :rt))

(in-package :anaphora-test)

(deftest alet.1
    (alet (1+ 1)
      (1+ it))
  3)

(deftest alet.2
    (alet (1+ 1)
      it
      (1+ it))
  3)

(deftest slet.1
    (let ((x (list 1 2 3)))
      (slet (car x)
	(incf it) (values it x)))
  2 (2 2 3))

(deftest aand.1
    (aand (+ 1 1)
	  (+ 1 it))
  3)

(deftest aand.2
    (aand 1 t (values it 2))
  1 2)

(deftest aand.3
    (let ((x 1))
      (aand (incf x) t t (values t it)))
  t 2)

(deftest aand.4
    (aand 1 (values t it))
  t 1)

#+(or)
;;; bug or a feature? forms like this expand to
;;;
;;; (let ((it (values ...))) (and it ...))
;;;
(deftest aand.5
    (aand (values nil t) it)
  nil t)

(deftest sor.1
    (let ((x (list nil)))
      (sor (car x)
	   (setf it t))
      x)
  (t))

(deftest aif.1
    (aif (+ 1 1)
	 (+ 1 it)
	 :never)
  3)

(deftest aif.2
    (let ((x 0))
      (aif (incf x)
	   it
	   :never))
  1)

(deftest aif.3
    (let ((x 0))
      (aif (eval `(and ,(incf x) nil))
	   :never
	   (list it x)))
  (nil 1))

(deftest sif.1
    (let ((x (list nil)))
      (sif (car x)
	   (setf it :oops)
	   (setf it :yes!))
      (car x))
  :yes!)

(deftest sif.2
    (let ((x (list t)))
      (sif (car x)
	   (setf it :yes!)
	   (setf it :oops))
      (car x))
  :yes!)

(deftest sif.3
    (sif (list 1 2 3)
	 (sif (car it)
	      (setf it 'a)
	      :foo))
  a)

(deftest sif.4
    (progn
      (defclass sif.4 ()
	((a :initform (list :sif))))
      (with-slots (a)
	  (make-instance 'sif.4)
	(sif a
	     (sif (car it)
		  it))))
  :sif)

(deftest asif.1
    (let ((x (list 0)))
      (asif (incf (car x))
	    it
	    (list :oops it)))
  1)

(deftest asif.2
    (let ((x (list nil)))
      (asif (car x)
	    (setf x :oops)
	    (setf it :yes!))
      x)
  (:yes!))

(deftest awhen.1
    (let ((x 0))
      (awhen (incf x)
	(+ 1 it)))
  2)

(deftest awhen.2
    (let ((x 0))
      (or (awhen (not (incf x))
	    t)
	  x))
  1)

(deftest swhen.1
    (let ((x 0))
      (swhen x
	(setf it :ok))
      x)
  :ok)

(deftest swhen.2
    (let ((x nil))
      (swhen x
	(setf it :oops))
      x)
  nil)

(deftest sunless.1
    (let ((x nil))
      (sunless x
	(setf it :ok))
      x)
  :ok)

(deftest sunless.2
    (let ((x t))
      (sunless x
	(setf it :oops))
      x)
  t)

(deftest acase.1
    (let ((x 0))
      (acase (incf x)
	(0 :no)
	(1 (list :yes it))
	(2 :nono)))
  (:yes 1))

(deftest scase.1
    (let ((x (list 3)))
      (scase (car x)
	(0 (setf it :no))
	(3 (setf it :yes!))
	(t (setf it :nono)))
      x)
  (:yes!))

(deftest aecase.1
    (let ((x (list :x)))
      (aecase (car x)
	(:y :no)
        (:x (list it :yes))))
  (:x :yes))

(deftest aecase.2
    (nth-value 0 (ignore-errors
		   (let ((x (list :x)))
		     (secase (car x)
			     (:y :no)))
		   :oops))
  nil)

(deftest secase.1
    (let ((x (list :x)))
      (secase (car x)
	      (:y (setf it :no))
	      (:x (setf it :yes)))
      x)
  (:yes))

(deftest secase.2
    (nth-value 0 (ignore-errors
		   (let ((x (list :x)))
		     (secase (car x)
			     (:y (setf it :no)))
		     :oops)))
  nil)

(deftest accase.1
    (let ((x (list :x)))
      (accase (car x)
	      (:y :no)
	      (:x (list it :yes))))
  (:x :yes))

(deftest accase.2
    (let ((x (list :x)))
      (handler-bind ((type-error (lambda (e) (store-value :z e))))
	(accase (car x)
		(:y (setf x :no))
		(:z (setf x :yes))))
      x)
  :yes)

(deftest accase.3
    (let ((x (list :x)))
      (accase (car x)
	      (:x (setf it :foo)))
      x)
  (:x))

(deftest sccase.1
    (let ((x (list :x)))
      (sccase (car x)
	      (:y (setf it :no))
	      (:x (setf it :yes)))
      x)
  (:yes))

(deftest sccase.2
    (let ((x (list :x)))
      (handler-bind ((type-error (lambda (e) (store-value :z e))))
	(sccase (car x)
		(:y (setf it :no))
		(:z (setf it :yes))))
      x)
  (:yes))

(deftest atypecase.1
    (atypecase 1.0
       (integer (+ 2 it))
       (float (1- it)))
  0.0)

(deftest atypecase.2
    (atypecase "Foo"
       (fixnum :no)
       (hash-table :nono))
  nil)

(deftest stypecase.1
    (let ((x (list 'foo)))
      (stypecase (car x)
	 (vector (setf it :no))
	 (symbol (setf it :yes)))
      x)
  (:yes))

(deftest stypecase.2
    (let ((x (list :bar)))
      (stypecase (car x)
	 (fixnum (setf it :no)))
      x)
  (:bar))

(deftest aetypecase.1
    (aetypecase 1.0
       (fixnum (* 2 it))
       (float (+ 2.0 it))
       (symbol :oops))
  3.0)

(deftest aetypecase.2
    (nth-value 0 (ignore-errors
		   (aetypecase 1.0
		       (symbol :oops))))
  nil)

(deftest setypecase.1
    (let ((x (list "Foo")))
      (setypecase (car x)
	  (symbol (setf it :no))
	  (string (setf it "OK"))
	  (integer (setf it :noon)))
      x)
  ("OK"))

(deftest setypecase.2
    (nth-value 0 (ignore-errors
		   (setypecase 'foo
		       (string :nono))))
  nil)

(deftest actypecase.1
    (actypecase :foo
       (string (list :string it))
       (keyword (list :keyword it))
       (symbol (list :symbol it)))
  (:keyword :foo))

(deftest actypecase.2
    (handler-bind ((type-error (lambda (e) (store-value "OK" e))))
      (actypecase 0
	 (string it)))
  "OK")

(deftest sctypecase.1
    (let ((x (list 0)))
      (sctypecase (car x)
	 (symbol (setf it 'symbol))
	 (bit (setf it 'bit)))
      x)
  (bit))

(deftest sctypecase.2
    (handler-bind ((type-error (lambda (e) (store-value "OK" e))))
      (let ((x (list 0)))
	(sctypecase (car x)
	  (string (setf it :ok)))
	x))
  (:ok))

(deftest acond.1
    (acond (:foo))
  :foo)

(deftest acond.2
    (acond ((null 1) (list :no it))
	   ((+ 1 2) (list :yes it))
	   (t :nono))
  (:yes 3))

(deftest acond.3
    (acond ((= 1 2) :no)
	   (nil :nono)
	   (t :yes))
  :yes)

;; Test COND with multiple forms in the implicit progn.
(deftest acond.4
    (let ((foo))
      (acond ((+ 2 2) (setf foo 38) (incf foo it) foo)
	     (t nil)))
  42)

(deftest scond.1
    (let ((x (list nil))
	  (y (list t)))
      (scond ((car x) (setf it :nono))
	     ((car y) (setf it :yes)))
      (values x y))
  (nil)
  (:yes))

(deftest scond.2
    (scond ((= 1 2) :no!))
  nil)

(deftest scond.3
    (equal (scond ((list 'a 'b)))
           '(a b))
  t)

(deftest aprog.1
    (aprog1 :yes
        (unless (eql it :yes) (error "Broken."))
      :no)
  :yes)

(deftest aif.sif.1
    (sif 1 (aif it it))
  1)

(deftest aif.sif.2
    (aif 1 (sif it it))
  1)

(deftest aif.sif.3
    (aif (list 1 2 3)
        (sif (car it)
             (setf it 'a)
             :foo))
  a)

(deftest alet.slet.1
    (slet 42 (alet 43 (slet it it)))
  43)

(deftest alambda.1
    (funcall (alambda (x)
	       (if (zerop x)
		   x
		   (self (1- x))))
	     4)
  0)


(defun elt-like (index seq)
  (elt seq index))

(define-setf-expander elt-like (index seq)
  (let ((index-var (gensym "index"))
	(seq-var (gensym "seq"))
	(store (gensym "store")))
    (values (list index-var seq-var)
	    (list index seq)
	    (list store)
	    `(if (listp ,seq-var)
		 (setf (nth ,index-var ,seq-var) ,store)
		 (setf (aref ,seq-var ,index-var) ,store))
	    `(if (listp ,seq-var)
		 (nth ,index-var ,seq-var)
		 (aref ,seq-var ,index-var)))))

(deftest symbolic.setf-expansion.1
    (let ((cell (list nil)))
      (sor (elt-like 0 cell) (setf it 1))
      (equal cell '(1)))
  t)
