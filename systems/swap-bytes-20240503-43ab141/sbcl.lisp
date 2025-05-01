;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :swap-bytes)

(defun swap-bytes-16 (integer)
  (declare (type (unsigned-byte 16) integer))
  (swap-bytes-16 integer))

(defun swap-bytes-32 (integer)
  (declare (type (unsigned-byte 32) integer))
  (swap-bytes-32 integer))

#+x86
(defun swap-bytes-64 (integer)
  (declare (type (unsigned-byte 64) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  (logior
   (swap-bytes-32 (ldb (byte 32 32) integer))
   (ash (swap-bytes-32 (ldb (byte 32 0) integer)) 32)))

#+x86-64
(defun swap-bytes-64 (integer)
  (declare (type (unsigned-byte 64) integer))
  (swap-bytes-64 integer))
