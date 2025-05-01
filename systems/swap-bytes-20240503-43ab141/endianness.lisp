;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :swap-bytes)

(deftype endianness ()
  '(member :big-endian :little-endian))

(deftype endianness-designator ()
  '(member :big-endian :little-endian :network :local))

(defconstant +endianness+
  #+big-endian    :big-endian
  #+little-endian :little-endian)

(defun endianness (endianness)
  (check-type endianness endianness-designator)
  (case endianness
    (:local   +endianness+)
    (:network :big-endian)
    (t        endianness)))

(defun find-swap-byte-function (&key size from (to :local))
  (let ((from (endianness from))
        (to   (endianness to)))
    (if (eql from to)
        'identity
        (ecase size
          (1 'identity)
          (2 'swap-bytes-16)
          (4 'swap-bytes-32)
          (8 'swap-bytes-64)))))
