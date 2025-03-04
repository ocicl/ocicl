#|
  This file is a part of lisp-namespace project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :lisp-namespace.test
  (:use :cl
        :lisp-namespace
        :fiveam)
  (:export :mytest
           :symbol-mytest
           :unbound-mytest
           :mytest-let))
(in-package :lisp-namespace.test)



(def-suite :lisp-namespace)
(in-suite :lisp-namespace)

;; run test with (run! test-name) 
;;   test as you like ...

(test let
  ;; it may not signal error.
  #+nil
  (signals error
    (eval
     '(let ((b 0)
            (b 1))
       (print :x))))
  ;; does not merge (b 0) and (b 1)
  (finishes
    (namespace-let ((b 0))
      (let ((b 1))
        (print :x)))))

(test complicated
  (finishes
    `(namespace-let ((#'x (y) (1+ y))
                     ((macro x) (y) (1+ y))
                     ((macro y) (y) (1+ y))
                     ((label z) (y) (w y))
                     ((label w) (y) (z y))
                     ((macro y) (y) (1+ y))
                     ((symbol-macro sm) 0)
                     (b 0))
       (let ((b 1))
         (print :x)))))

(test restart
  (finishes
    (namespace-let (((restart continue)
                     (lambda (c)
                       (declare (ignore c))
                       (print :hi!))))
      (let ((b 1))
        (print :x)))))

(define-namespace mytest fixnum)

(test namespace
  (finishes
    (setf (symbol-mytest 'a) 0))
  (is (= (symbol-mytest 'a)))
  (signals unbound-mytest
    (symbol-mytest 'b))
  ;; lexical
  (is (= 1
         (funcall
          (namespace-let (((mytest a) 1))
            (lambda ()
              (symbol-mytest 'a))))))
  (let (x)
    (namespace-let (((mytest a) 1))
      (setf x 
            (lambda ()
              (symbol-mytest 'a))))
    (is (= 1 (funcall x))))
  ;; lexical, specialized
  (let (x)
    (mytest-let ((a 1))
      (setf x 
            (lambda ()
              (symbol-mytest 'a))))
    (is (= 1 (funcall x))))

  ;; nested
  (let (x y)
    (mytest-let ((a 1))
      (setf x (lambda () (symbol-mytest 'a)))
      (mytest-let ((a 2))
        (setf y (lambda () (symbol-mytest 'a)))))
    (is (= 1 (funcall x)))
    (is (= 2 (funcall y))))
  (finishes
   (describe 'mytest)))

(defpackage :other-package
  (:use :cl :lisp-namespace :fiveam :lisp-namespace.test))

(test export
  (in-package :other-package)
  (eval
   (read-from-string
    (princ-to-string
     '(progn
       (finishes
         (setf (symbol-mytest 'a) 0))
       (is (= 0 (symbol-mytest 'a)))
       (signals unbound-mytest
         (symbol-mytest 'b))
       ;; lexical
       (is (= 1
            (funcall
             (namespace-let (((mytest a) 1))
               (lambda ()
                 (symbol-mytest 'a))))))
       (let (x)
         (namespace-let (((mytest a) 1))
           (setf x 
                 (lambda ()
                   (symbol-mytest 'a))))
         (is (= 1 (funcall x))))
       ;; lexical, specialized
       (let (x)
         (mytest-let ((a 1))
           (setf x 
                 (lambda ()
                   (symbol-mytest 'a))))
         (is (= 1 (funcall x))))

       ;; nested
       (let (x y)
         (mytest-let ((a 1))
           (setf x (lambda () (symbol-mytest 'a)))
           (mytest-let ((a 2))
             (setf y (lambda () (symbol-mytest 'a)))))
         (is (= 1 (funcall x)))
         (is (= 2 (funcall y)))))))))
