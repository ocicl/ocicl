;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
#+sbcl
(in-package :sb-c)

#+(and sbcl (or x86 x86-64) ironclad-assembly)
(progn

(defknown (ironclad::fill-block-ub8-le ironclad::fill-block-ub8-be)
  ((simple-array (unsigned-byte 32) (*))
   (simple-array (unsigned-byte 8) (*))
   (integer 0 #.(- array-dimension-limit 64))) (values)
  (any) :overwrite-fndb-silently t)

#+x86-64
(defknown ironclad::fill-block-ub8-le/64
  ((simple-array (unsigned-byte 64) (*))
   (simple-array (unsigned-byte 8) (*))
   (integer 0 #.(- array-dimension-limit 64))) (values)
  (any) :overwrite-fndb-silently t)

#+x86-64
(defknown ironclad::fill-block-ub8-be/64
  ((simple-array (unsigned-byte 64) (*))
   (simple-array (unsigned-byte 8) (*))
   (integer 0 #.(- array-dimension-limit 128))) (values)
  (any) :overwrite-fndb-silently t)

(defknown ironclad::expand-block
  ((simple-array (unsigned-byte 32) (*)))
  (values)
  (any) :overwrite-fndb-silently t)

(defknown ironclad::%update-sha1-block
  ((simple-array (unsigned-byte 32) (*)) (simple-array (unsigned-byte 32) (*)))
  (simple-array (unsigned-byte 32) (*))
  (any) :overwrite-fndb-silently t)

(defknown ironclad::sha256-expand-block
  ((simple-array (unsigned-byte 32) (*)))
  (values)
  (any) :overwrite-fndb-silently t)

#+x86-64
(defknown ironclad::x-salsa-core
  ((signed-byte 61) (simple-array (unsigned-byte 8) (*))
   (simple-array (unsigned-byte 32) (*)))
  (values)
  (any) :overwrite-fndb-silently t)

#+x86-64
(defknown ironclad::x-chacha-core
  ((signed-byte 61) (simple-array (unsigned-byte 8) (*))
   (simple-array (unsigned-byte 32) (*)))
  (values)
  (any) :overwrite-fndb-silently t)

#+x86-64
(defknown ironclad::pclmulqdq-support-p
  ()
  (boolean)
  (any)
  :overwrite-fndb-silently t)

#+x86-64
(defknown ironclad::aes-ni-support-p
  ()
  (boolean)
  (any)
  :overwrite-fndb-silently t)

#+x86-64
(defknown ironclad::aes-ni-generate-round-keys
  ((simple-array (unsigned-byte 8) (*))
   (unsigned-byte 64)
   (simple-array (unsigned-byte 32) (*))
   (simple-array (unsigned-byte 32) (*)))
  (values)
  (any)
  :overwrite-fndb-silently t)

#+x86-64
(defknown ironclad::aes-ni-encrypt
  ((simple-array (unsigned-byte 8) (*))
   (unsigned-byte 64)
   (simple-array (unsigned-byte 8) (*))
   (unsigned-byte 64)
   (simple-array (unsigned-byte 32) (*))
   (integer 0 14))
  (values)
  (any)
  :overwrite-fndb-silently t)

#+x86-64
(defknown ironclad::aes-ni-decrypt
  ((simple-array (unsigned-byte 8) (*))
   (unsigned-byte 64)
   (simple-array (unsigned-byte 8) (*))
   (unsigned-byte 64)
   (simple-array (unsigned-byte 32) (*))
   (integer 0 14))
  (values)
  (any)
  :overwrite-fndb-silently t)

#+x86-64
(defknown ironclad::fast-blake2s-mixing
  ((unsigned-byte 32)
   (unsigned-byte 32)
   (unsigned-byte 32)
   (unsigned-byte 32)
   (unsigned-byte 32)
   (unsigned-byte 32))
  (values (unsigned-byte 32)
          (unsigned-byte 32)
          (unsigned-byte 32)
          (unsigned-byte 32))
  (any)
  :overwrite-fndb-silently t)

#+x86-64
(defknown ironclad::gmac-mul-fast
  ((simple-array (unsigned-byte 8) (*))
   (simple-array (unsigned-byte 8) (*)))
  (values)
  (any)
  :overwrite-fndb-silently t)

#+x86-64
(defknown ironclad::xor128
  ((simple-array (unsigned-byte 8) (*))
   (integer 0 #.array-dimension-limit)
   (simple-array (unsigned-byte 8) (*))
   (integer 0 #.array-dimension-limit)
   (simple-array (unsigned-byte 8) (*))
   (integer 0 #.array-dimension-limit))
  (values)
  (any)
  :overwrite-fndb-silently t)

#+x86-64
(defknown ironclad::mov128
  ((simple-array (unsigned-byte 8) (*))
   (integer 0 #.array-dimension-limit)
   (simple-array (unsigned-byte 8) (*))
   (integer 0 #.array-dimension-limit))
  (values)
  (any)
  :overwrite-fndb-silently t)

(defknown ironclad::swap32
  ((unsigned-byte 32))
  (unsigned-byte 32)
  (any)
  :overwrite-fndb-silently t)

#+x86-64
(defknown ironclad::swap64
  ((unsigned-byte 64))
  (unsigned-byte 64)
  (any)
  :overwrite-fndb-silently t)

(defknown ironclad::inc-counter-block
  ((integer 0 #.most-positive-fixnum)
   (simple-array (unsigned-byte 8) (*)))
  (values)
  (any)
  :overwrite-fndb-silently t)
);#+sbcl
