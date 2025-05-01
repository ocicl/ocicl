;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:swap-bytes-test
  (:use #:cl #:fiveam #:swap-bytes)
  (:export #:run-tests))

(in-package #:swap-bytes-test)

(def-suite :swap-bytes)
(in-suite :swap-bytes)

(defun sb16p (integer)
  (declare (type (unsigned-byte 16) integer))
  (logior (ash (logand #xFF integer) 8)
          (ash integer -8)))

(defun sb32p (integer)
  (declare (type (unsigned-byte 32) integer))
  (logior (ash (logand #xFF integer) 24)
          (ash (logand #xFF00 integer) 8)
          (ash (logand #xFF0000 integer) -8)
          (ash integer -24)))

(defun sb64p (integer)
  (declare (type (unsigned-byte 64) integer))
  (macrolet ((shift (mask shift)
               `(ash (logand ,mask integer) ,shift)))
    (logior
     (shift #xFF 56)
     (shift #xFF00 40)
     (shift #xFF0000 24)
     (shift #xFF000000 8)
     (shift #xFF00000000 -8)
     (shift #xFF0000000000 -24)
     (shift #xFF000000000000 -40)
     (ash integer -56))))

(defparameter *test-table*
  '((#xcafe #xfeca swap-bytes-16 sb16p)
    (#xf457 #x57f4 swap-bytes-16 sb16p)
    (#x0000 #x0000 swap-bytes-16 sb16p)
    (#xffff #xffff swap-bytes-16 sb16p)
    (#xcafedead #xaddefeca swap-bytes-32 sb32p)
    (#xb116b00b #x0bb016b1 swap-bytes-32 sb32p)
    (#xbe47dead #xadde47be swap-bytes-32 sb32p)
    (#xdeadbeef #xefbeadde swap-bytes-32 sb32p)
    (#x00000000 #x00000000 swap-bytes-32 sb32p)
    (#xffffffff #xffffffff swap-bytes-32 sb32p)
    (#xb116b00b1ee7babe #xbebae71e0bb016b1 swap-bytes-64 sb64p)
    (#xdeadbeefcafebabe #xbebafecaefbeadde swap-bytes-64 sb64p)
    (#x0000000000000000 #x0000000000000000 swap-bytes-64 sb64p)
    (#xffffffffffffffff #xffffffffffffffff swap-bytes-64 sb64p)))

(test identity/funcall
  "Swapping a number twice gives the identity"
  (loop for (num nil fun) in *test-table*
     do (is (= (funcall fun (funcall fun num)) num))))

(test identity/compiled
  "Swapping a number twice gives the identity"
  (loop for (num nil fun) in *test-table*
     do (is (= (funcall fun (funcall fun num)) num))))

(test result/funcall
  "Simple values"
  (loop for (num snum fun) in *test-table*
     do (is (= (funcall fun num) snum))))

(test result/compiled
  "Simple values"
  (loop for (num snum fun) in *test-table*
     do (is (= (funcall (compile nil `(lambda (n) (,fun n)))
                        num)
               snum))))

(test portable/funcall
  "Simple values"
  (loop for (num nil fun pfun) in *test-table*
     do (is (= (funcall fun num)
               (funcall pfun num)))))

(test portable/compiled
  "Simple values"
  (loop for (num nil fun pfun) in *test-table*
     do (is (= (funcall (compile nil `(lambda (n) (,fun n)))
                        num)
               (funcall pfun num)))))
