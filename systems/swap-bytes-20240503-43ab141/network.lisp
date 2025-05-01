;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :swap-bytes)

(declaim (inline htons ntohs htonl ntohl htonq ntohq))

(defun htons (integer)
  "Convert (unsigned-byte 16) from host order(little- or big-endian)
to network order(always big-endian)."
  (declare (type (unsigned-byte 16) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  #+little-endian (swap-bytes-16 integer)
  #+big-endian    integer)

(defun ntohs (integer)
  "Convert (unsigned-byte 16) from network order(always big-endian) to
host order(little- or big-endian)."
  (declare (type (unsigned-byte 16) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  #+little-endian (swap-bytes-16 integer)
  #+big-endian    integer)

(defun htonl (integer)
  "Convert (unsigned-byte 32) from host order(little- or big-endian)
to network order(always big-endian)."
  (declare (type (unsigned-byte 32) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  #+little-endian (swap-bytes-32 integer)
  #+big-endian    integer)

(defun ntohl (integer)
  "Convert (unsigned-byte 32) from network order(always big-endian) to
host order(little- or big-endian)."
  (declare (type (unsigned-byte 32) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  #+little-endian (swap-bytes-32 integer)
  #+big-endian    integer)

(defun htonq (integer)
  "Convert (unsigned-byte 64) from host order(little- or big-endian)
to network order(always big-endian)."
  (declare (type (unsigned-byte 64) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  #+little-endian (swap-bytes-64 integer)
  #+big-endian    integer)

(defun ntohq (integer)
  "Convert (unsigned-byte 64) from network order(always big-endian) to
host order(little- or big-endian)."
  (declare (type (unsigned-byte 64) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  #+little-endian (swap-bytes-64 integer)
  #+big-endian    integer)
