;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :ccl)

#+(or x8632-target x8664-target)
(defx8632lapfunction swap-bytes::%swap-bytes-16 ((fixnum arg_z))
  (unbox-fixnum fixnum imm0)
  (xchg (% ah) (% al))
  (box-fixnum imm0 fixnum)
  (single-value-return))

#+x8632-target
(defx8632lapfunction swap-bytes::%swap-bytes-32 ((number arg_z))
  ; Extract the u32 from the number, swap the bytes, make and return
  ; a bignum.
  (save-simple-frame)
  (call-subprim .SPgetu32)
  (bswapl (% imm0))
  (restore-simple-frame)
  (jmp-subprim .SPmakeu32))

#+x8664-target
(defx86lapfunction swap-bytes::%swap-bytes-32 ((fixnum arg_z))
  (unbox-fixnum fixnum imm0)
  (bswapl (% eax))
  (box-fixnum imm0 fixnum)
  (single-value-return))

#+x8632-target
(defun swap-bytes::%swap-bytes-64 (integer)
  (declare (type (unsigned-byte 64) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  (logior
   (swap-bytes:swap-bytes-32 (ldb (byte 32 32) integer))
   (ash (swap-bytes:swap-bytes-32 (ldb (byte 32 0) integer)) 32)))

#+x8664-target
(defx86lapfunction swap-bytes::%swap-bytes-64 ((number arg_z))
  ; Extract the u64 from the number (either a fixnum or bignum), swap the
  ; bytes, make and return a bignum.
  (save-simple-frame)
  (call-subprim .SPgetu64)
  (bswapq (% imm0))
  (restore-simple-frame)
  (jmp-subprim .SPmakeu64))


(in-package :swap-bytes)

(declaim (inline swap-bytes-16))
(defun swap-bytes-16 (integer)
  (declare (type (unsigned-byte 16) integer))
  (%swap-bytes-16 integer))

(declaim (inline swap-bytes-32))
(defun swap-bytes-32 (integer)
  (declare (type (unsigned-byte 32) integer))
  (%swap-bytes-32 integer))

(declaim (inline swap-bytes-64))
(defun swap-bytes-64 (integer)
  (declare (type (unsigned-byte 64) integer))
  (%swap-bytes-64 integer))
