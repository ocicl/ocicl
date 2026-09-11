;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Types.
;;;

(in-package :iolib/base)

(deftype function-designator ()
  '(or symbol function))

(defun symbol-with-name-of-length-one (thing)
  (if (and (symbolp thing)
           (= 1 (length (symbol-name thing))))
      (char (symbol-name thing) 0)
      nil))

(deftype character-designator ()
  '(or character (string 1) (satisfies symbol-with-name-of-length-one)))


;; Vector types

(deftype ub8  () '(unsigned-byte 8))
(deftype ub16 () '(unsigned-byte 16))
(deftype ub32 () '(unsigned-byte 32))
(deftype ub64 () '(unsigned-byte 64))
(deftype sb8  () '(signed-byte 8))
(deftype sb16 () '(signed-byte 16))
(deftype sb32 () '(signed-byte 32))
(deftype sb64 () '(signed-byte 64))

(deftype ub8-sarray (&optional (size '*))
  `(simple-array ub8 (,size)))
(deftype ub8-vector (&optional (size '*))
  `(vector ub8 ,size))

(deftype ub16-sarray (&optional (size '*))
  `(simple-array ub16 (,size)))
(deftype ub16-vector (&optional (size '*))
  `(vector ub16 ,size))

(deftype ub32-sarray (&optional (size '*))
  `(simple-array ub32 (,size)))
(deftype ub32-vector (&optional (size '*))
  `(vector ub32 ,size))

(deftype ub64-sarray (&optional (size '*))
  `(simple-array ub64 (,size)))
(deftype ub64-vector (&optional (size '*))
  `(vector ub64 ,size))
