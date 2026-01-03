;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; rc2.lisp -- implementation of the RC2 cipher algorithm from RFC 2268

(in-package :crypto)
(in-ironclad-readtable)

;;; RC2 accepts a 1-byte to 128-byte key.  But it also lets you specify
;;; an "effective key length" in bits, which effectively lets you have a
;;; 1-bit to 1024-bit key.  The test vectors supplied in the RFC specify
;;; the effective key length as well as the bytes present in the key.
;;;
;;; This implementation does not support such generality: only effective
;;; key lengths from 8 bits to 1024 bits in multiples of eight are
;;; supported.  It would be nice to support the more general
;;; functionality from the RFC; an interface for such functionality
;;; (maybe an :EFFECTIVE-KEY-LENGTH argument to `MAKE-CIPHER',
;;; applicable only to RC2) would also support specifying the number of
;;; rounds for many ciphers.


;;; PITABLE from section 2.

(defconst +rc2-pitable+
  #8@(#xd9 #x78 #xf9 #xc4 #x19 #xdd #xb5 #xed #x28 #xe9 #xfd #x79 #x4a #xa0 #xd8 #x9d
#xc6 #x7e #x37 #x83 #x2b #x76 #x53 #x8e #x62 #x4c #x64 #x88 #x44 #x8b #xfb #xa2
#x17 #x9a #x59 #xf5 #x87 #xb3 #x4f #x13 #x61 #x45 #x6d #x8d #x09 #x81 #x7d #x32
#xbd #x8f #x40 #xeb #x86 #xb7 #x7b #x0b #xf0 #x95 #x21 #x22 #x5c #x6b #x4e #x82
#x54 #xd6 #x65 #x93 #xce #x60 #xb2 #x1c #x73 #x56 #xc0 #x14 #xa7 #x8c #xf1 #xdc
#x12 #x75 #xca #x1f #x3b #xbe #xe4 #xd1 #x42 #x3d #xd4 #x30 #xa3 #x3c #xb6 #x26
#x6f #xbf #x0e #xda #x46 #x69 #x07 #x57 #x27 #xf2 #x1d #x9b #xbc #x94 #x43 #x03
#xf8 #x11 #xc7 #xf6 #x90 #xef #x3e #xe7 #x06 #xc3 #xd5 #x2f #xc8 #x66 #x1e #xd7
#x08 #xe8 #xea #xde #x80 #x52 #xee #xf7 #x84 #xaa #x72 #xac #x35 #x4d #x6a #x2a
#x96 #x1a #xd2 #x71 #x5a #x15 #x49 #x74 #x4b #x9f #xd0 #x5e #x04 #x18 #xa4 #xec
#xc2 #xe0 #x41 #x6e #x0f #x51 #xcb #xcc #x24 #x91 #xaf #x50 #xa1 #xf4 #x70 #x39
#x99 #x7c #x3a #x85 #x23 #xb8 #xb4 #x7a #xfc #x02 #x36 #x5b #x25 #x55 #x97 #x31
#x2d #x5d #xfa #x98 #xe3 #x8a #x92 #xae #x05 #xdf #x29 #x10 #x67 #x6c #xba #xc9
#xd3 #x00 #xe6 #xcf #xe1 #x9e #xa8 #x2c #x63 #x16 #x01 #x3f #x58 #xe2 #x89 #xa9
#x0d #x38 #x34 #x1b #xab #x33 #xff #xb0 #xbb #x48 #x0c #x5f #xb9 #xb1 #xcd #x2e
#xc5 #xf3 #xdb #x47 #xe5 #xa5 #x9c #x77 #x0a #xa6 #x20 #x68 #xfe #x7f #xc1 #xad))

(deftype rc2-round-keys () '(simple-array (unsigned-byte 16) (64)))

(defclass rc2 (cipher 8-byte-block-mixin)
  ((round-keys :accessor round-keys :type rc2-round-keys)))

(declaim (inline rol16)
         (ftype (function ((unsigned-byte 16) (integer 0 15)) (unsigned-byte 16))))
(defun rol16 (x shift)
  (declare (type (unsigned-byte 16) x))
  (declare (type (integer 0 15) shift))
  (logior (ldb (byte 16 0) (ash x shift)) (ash x (- shift 16))))

(defun rc2-schedule-key (key effective-key-length)
  (declare (type (simple-array (unsigned-byte 8) (*)) key))
  (let* ((length (length key))
         (lbuf (make-array 128 :element-type '(unsigned-byte 8)
                           :initial-element 0))
         (scheduled-key (make-array 64 :element-type '(unsigned-byte 16)
                                    :initial-element 0))
         (t8 (truncate (+ effective-key-length 7) 8))
         (tm (mod 255 (expt 2 (+ 8 effective-key-length (- (* t8 8)))))))
    (declare (type (integer 1 128) length))
    (declare (type (simple-array (unsigned-byte 8) (128)) lbuf))
    (declare (type rc2-round-keys scheduled-key))
    (declare (dynamic-extent lbuf))
    (replace lbuf key)
    (loop for j from length below 128 do
          (setf (aref lbuf j)
                (aref +rc2-pitable+
                      (mod (+ (aref lbuf (1- j))
                              (aref lbuf (- j length)))
                           256))))
    (setf (aref lbuf (- 128 t8))
          (aref +rc2-pitable+ (logand (aref lbuf (- 128 t8)) tm)))
    (loop for j from (- 127 t8) downto 0 do
          (setf (aref lbuf j)
                (aref +rc2-pitable+
                      (logxor (aref lbuf (1+ j))
                              (aref lbuf (+ j t8))))))
    ;; If we wanted to really be a speed demon, we'd specialize this.
    (dotimes (i 64 scheduled-key)
      (setf (aref scheduled-key i)
            (ub16ref/le lbuf (* i 2))))))

(macrolet ((mix (index)
             (loop for i from 0 below 4
                   collect (let ((x0 (symbolicate '#:r i))
                                 (x1 (symbolicate '#:r (mod (- i 1) 4)))
                                 (x2 (symbolicate '#:r (mod (- i 2) 4)))
                                 (x3 (symbolicate '#:r (mod (- i 3) 4))))
                             `(progn
                                (setf ,x0 (ldb (byte 16 0)
                                               (+ ,x0
                                                  (logand ,x1 ,x2)
                                                  (aref round-keys (+ (* 4 ,index) ,i))
                                                  (logandc1 ,x1 ,x3))))
                                (setf ,x0 (rol16 ,x0 ,(case i
                                                        (0 1)
                                                        (1 2)
                                                        (2 3)
                                                        (3 5)))))) into forms
                   finally (return `(progn ,@forms))))
           (mash ()
             (loop for i from 0 below 4
                   collect (let ((x0 (symbolicate '#:r i))
                                 (x1 (symbolicate '#:r (mod (- i 1) 4))))
                             `(setf ,x0 (ldb (byte 16 0)
                                             (+ ,x0 (aref round-keys (ldb (byte 6 0) ,x1)))))) into forms
                   finally (return `(progn ,@forms))))
           (rmix (index)
             (loop for i from 0 below 4
                   collect (let ((x0 (symbolicate '#:r i))
                                 (x1 (symbolicate '#:r (mod (- i 1) 4)))
                                 (x2 (symbolicate '#:r (mod (- i 2) 4)))
                                 (x3 (symbolicate '#:r (mod (- i 3) 4))))
                             `(progn
                                (setf ,x0 (rol16 ,x0 ,(case i
                                                        (0 15)
                                                        (1 14)
                                                        (2 13)
                                                        (3 11))))
                                (setf ,x0 (ldb (byte 16 0)
                                               (- ,x0
                                                  (aref round-keys (+ (* 4 ,index) ,i))
                                                  (logand ,x1 ,x2)
                                                  (logandc1 ,x1 ,x3)))))) into forms
                   finally (return `(progn ,@(nreverse forms)))))
           (rmash ()
             (loop for i from 0 below 4
                   collect (let ((x0 (symbolicate '#:r i))
                                 (x1 (symbolicate '#:r (mod (- i 1) 4))))
                             `(setf ,x0 (ldb (byte 16 0)
                                             (- ,x0 (aref round-keys (ldb (byte 6 0) ,x1)))))) into forms
                   finally (return `(progn ,@(nreverse forms))))))
  (define-block-encryptor rc2 8
    (let ((round-keys (round-keys context)))
      (declare (type rc2-round-keys round-keys))
      (with-words ((r0 r1 r2 r3) plaintext plaintext-start
                   :size 2 :big-endian nil)
        #.(loop for i from 0 below 18
                collect (ecase i
                          ((0 1 2 3 4
                              6 7 8 9 10 11
                              13 14 15 16 17)
                           ;; mixing round
                           `(mix ,(cond
                                    ((<= i 4) i)
                                    ((<= 6 i 11) (- i 1))
                                    ((<= 13 i 17) (- i 2)))))
                          ((5 12)
                           ;; mashing round
                           `(mash))) into forms
                finally (return `(progn ,@forms)))
        (store-words ciphertext ciphertext-start r0 r1 r2 r3))))

  (define-block-decryptor rc2 8
    (let ((round-keys (round-keys context)))
      (declare (type rc2-round-keys round-keys))
      (with-words ((r0 r1 r2 r3) ciphertext ciphertext-start
                   :size 2 :big-endian nil)
        #.(loop for i from 0 below 18
                collect (ecase i
                          ((0 1 2 3 4
                              6 7 8 9 10 11
                              13 14 15 16 17)
                           ;; mixing round
                           `(rmix ,(cond
                                     ((<= i 4) i)
                                     ((<= 6 i 11) (- i 1))
                                     ((<= 13 i 17) (- i 2)))))
                          ((5 12)
                           ;; mashing round
                           `(rmash))) into forms
                finally (return `(progn ,@(nreverse forms))))
        (store-words plaintext plaintext-start r0 r1 r2 r3))))
  ) ; MACROLET

(defmethod schedule-key ((cipher rc2) key)
  (let* ((effective-key-length (* (length key) 8))
         (round-keys (rc2-schedule-key key effective-key-length)))
    (setf (round-keys cipher) round-keys)
    cipher))

(defcipher rc2
  (:encrypt-function rc2-encrypt-block)
  (:decrypt-function rc2-decrypt-block)
  (:block-length 8)
  (:key-length (:variable 1 128 1)))
