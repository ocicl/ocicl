;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; arcfour.lisp -- implementation of the alleged RC4 stream cipher

(in-package :crypto)

;;; This is a rewrite of a C program by Christophe Devine.  Thanks,
;;; Mr. Devine.  This version is Copyright (C) 2005 Tim Daly Jr.
;;;
;;; Code modified for Ironclad by Nathan Froyd.

(deftype arcfour-sbox () '(simple-array (unsigned-byte 8) (256)))

(defclass arcfour (stream-cipher)
  ((x :accessor arcfour-x :initform 0 :type fixnum)
   (y :accessor arcfour-y :initform 0 :type fixnum)
   (m :accessor arcfour-sbox
      :initform (make-array 256 :element-type '(unsigned-byte 8))
      :type arcfour-sbox)))

(defun arcfour-keyify (context key)
  (declare (type arcfour context))
  (let ((m (arcfour-sbox context)))
    (declare (type arcfour-sbox m))
    (setf (arcfour-x context) 0
          (arcfour-y context) 0)
    (dotimes (i 256)
      (setf (aref m i) i))
    (let ((a 0)
          (j 0)
          (k 0)
          (key-length (length key)))
      (dotimes (i 256)
        (setf a (aref m i)
              j (ldb (byte 8 0) (+ j a (aref key k)))
              (aref m i) (aref m j)
              (aref m j) a)
        (when (>= (incf k) key-length)
          (setf k 0))))))

(define-stream-cryptor arcfour
  (let* ((m (arcfour-sbox context))
         (x (arcfour-x context))
         (y (arcfour-y context))
         (a 0)
         (b 0))
    (declare (type arcfour-sbox m)
             (type fixnum x y a b))
    (dotimes (i length (progn
                         (setf (arcfour-x context) x
                               (arcfour-y context) y)
                         (values)))
      (setf x (ldb (byte 8 0) (1+ x))
            a (aref m x)
            y (ldb (byte 8 0) (+ y a))
            b (aref m y)
            (aref m x) b
            (aref m y) a)
      (setf (aref ciphertext (+ ciphertext-start i))
            (logxor (aref plaintext (+ plaintext-start i))
                    (aref m (ldb (byte 8 0) (+ a b))))))))

(defmethod schedule-key ((cipher arcfour) key)
  (arcfour-keyify cipher key)
  cipher)

(defcipher arcfour
  (:mode :stream)
  (:crypt-function arcfour-crypt)
  (:key-length (:variable 1 256 1)))
