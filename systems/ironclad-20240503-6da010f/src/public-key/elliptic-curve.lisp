;;;; -*- mode: lisp; indent-tabs-mode: nil -*-

(in-package :crypto)


(defgeneric ec-point-on-curve-p (p)
  (:documentation "Return T if the point P is on the curve."))

(defgeneric ec-point-equal (p q)
  (:documentation "Return T if P and Q represent the same point."))

(defgeneric ec-double (p)
  (:documentation "Return the point 2P."))

(defgeneric ec-add (p q)
  (:documentation "Return the point P + Q."))

(defgeneric ec-scalar-mult (p e)
  (:documentation "Return the point e * P."))

(defgeneric ec-scalar-inv (kind n)
  (:documentation "Return the modular inverse of N."))

(defgeneric ec-make-point (kind &key &allow-other-keys)
  (:documentation "Return a point of KIND, initialized according to the
specified coordinates."))

(defgeneric ec-destructure-point (p)
  (:documentation "Return a plist containing the coordinates of the point P."))

(defgeneric ec-encode-scalar (kind n)
  (:documentation "Return an octet vector representing the integer N."))

(defgeneric ec-decode-scalar (kind octets)
  (:documentation "Return the integer represented by the OCTETS."))

(defgeneric ec-encode-point (p)
  (:documentation "Return an octet vector representing the point P."))

(defgeneric ec-decode-point (kind octets)
  (:documentation "Return the point represented by the OCTETS."))
