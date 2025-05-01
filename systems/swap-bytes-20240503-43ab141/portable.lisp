;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :swap-bytes)

(declaim (inline swap-bytes-16 swap-bytes-32 swap-bytes-64))

(defun swap-bytes-16 (integer)
  (declare (type (unsigned-byte 16) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  (logior (ash (logand #xFF integer)  8)
          (ash integer -8)))

(defun swap-bytes-32 (integer)
  (declare (type (unsigned-byte 32) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  (logior (ash (logand #x0000FF integer)  24)
          (ash (logand #x00FF00 integer)   8)
          (ash (logand #xFF0000 integer)  -8)
          (ash                  integer  -24)))

(defun swap-bytes-64 (integer)
  (declare (type (unsigned-byte 64) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  (macrolet ((shift (mask shift)
               `(ash (logand ,mask integer) ,shift)))
    (logior
     (shift #x000000000000FF  56)
     (shift #x0000000000FF00  40)
     (shift #x00000000FF0000  24)
     (shift #x000000FF000000   8)
     (shift #x0000FF00000000  -8)
     (shift #x00FF0000000000 -24)
     (shift #xFF000000000000 -40)
     (ash integer -56))))
