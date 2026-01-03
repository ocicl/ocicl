;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; sm4.lisp -- implementation of SM4 (GB/T 32907-2016)

(in-package :crypto)
(in-ironclad-readtable)


;;;
;;; Parameters
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconst +sm4-s+
    #8@(#xd6 #x90 #xe9 #xfe #xcc #xe1 #x3d #xb7
        #x16 #xb6 #x14 #xc2 #x28 #xfb #x2c #x05
        #x2b #x67 #x9a #x76 #x2a #xbe #x04 #xc3
        #xaa #x44 #x13 #x26 #x49 #x86 #x06 #x99
        #x9c #x42 #x50 #xf4 #x91 #xef #x98 #x7a
        #x33 #x54 #x0b #x43 #xed #xcf #xac #x62
        #xe4 #xb3 #x1c #xa9 #xc9 #x08 #xe8 #x95
        #x80 #xdf #x94 #xfa #x75 #x8f #x3f #xa6
        #x47 #x07 #xa7 #xfc #xf3 #x73 #x17 #xba
        #x83 #x59 #x3c #x19 #xe6 #x85 #x4f #xa8
        #x68 #x6b #x81 #xb2 #x71 #x64 #xda #x8b
        #xf8 #xeb #x0f #x4b #x70 #x56 #x9d #x35
        #x1e #x24 #x0e #x5e #x63 #x58 #xd1 #xa2
        #x25 #x22 #x7c #x3b #x01 #x21 #x78 #x87
        #xd4 #x00 #x46 #x57 #x9f #xd3 #x27 #x52
        #x4c #x36 #x02 #xe7 #xa0 #xc4 #xc8 #x9e
        #xea #xbf #x8a #xd2 #x40 #xc7 #x38 #xb5
        #xa3 #xf7 #xf2 #xce #xf9 #x61 #x15 #xa1
        #xe0 #xae #x5d #xa4 #x9b #x34 #x1a #x55
        #xad #x93 #x32 #x30 #xf5 #x8c #xb1 #xe3
        #x1d #xf6 #xe2 #x2e #x82 #x66 #xca #x60
        #xc0 #x29 #x23 #xab #x0d #x53 #x4e #x6f
        #xd5 #xdb #x37 #x45 #xde #xfd #x8e #x2f
        #x03 #xff #x6a #x72 #x6d #x6c #x5b #x51
        #x8d #x1b #xaf #x92 #xbb #xdd #xbc #x7f
        #x11 #xd9 #x5c #x41 #x1f #x10 #x5a #xd8
        #x0a #xc1 #x31 #x88 #xa5 #xcd #x7b #xbd
        #x2d #x74 #xd0 #x12 #xb8 #xe5 #xb4 #xb0
        #x89 #x69 #x97 #x4a #x0c #x96 #x77 #x7e
        #x65 #xb9 #xf1 #x09 #xc5 #x6e #xc6 #x84
        #x18 #xf0 #x7d #xec #x3a #xdc #x4d #x20
        #x79 #xee #x5f #x3e #xd7 #xcb #x39 #x48))

  (defconst +sm4-ck+
    #32@(#x00070e15 #x1c232a31 #x383f464d #x545b6269
         #x70777e85 #x8c939aa1 #xa8afb6bd #xc4cbd2d9
         #xe0e7eef5 #xfc030a11 #x181f262d #x343b4249
         #x50575e65 #x6c737a81 #x888f969d #xa4abb2b9
         #xc0c7ced5 #xdce3eaf1 #xf8ff060d #x141b2229
         #x30373e45 #x4c535a61 #x686f767d #x848b9299
         #xa0a7aeb5 #xbcc3cad1 #xd8dfe6ed #xf4fb0209
         #x10171e25 #x2c333a41 #x484f565d #x646b7279)))


;;;
;;; SM4 round
;;;

(defmacro sm4-h (x)
  `(logior (mod32ash (aref +sm4-s+ (logand (mod32ash ,x -24) #xff)) 24)
           (mod32ash (aref +sm4-s+ (logand (mod32ash ,x -16) #xff)) 16)
           (mod32ash (aref +sm4-s+ (logand (mod32ash ,x -8) #xff)) 8)
           (aref +sm4-s+ (logand ,x #xff))))

(defmacro sm4-g (x)
  (let ((y (gensym)))
    `(let ((,y (sm4-h ,x)))
       (declare (type (unsigned-byte 32) ,y))
       (logxor ,y (rol32 ,y 13) (rol32 ,y 23)))))

(defmacro sm4-f (x)
  (let ((y (gensym)))
    `(let ((,y (sm4-h ,x)))
       (declare (type (unsigned-byte 32) ,y))
       (logxor ,y (rol32 ,y 2) (rol32 ,y 10) (rol32 ,y 18) (rol32 ,y 24)))))

(defmacro sm4-round (w0 w1 w2 w3 round-keys r encrypt-p)
  `(setf ,w0 (logxor ,w0 (sm4-f (logxor ,w1 ,w2 ,w3
                                        (aref ,round-keys ,r))))
         ,w1 (logxor ,w1 (sm4-f (logxor ,w0 ,w2 ,w3
                                        (aref ,round-keys ,(if encrypt-p
                                                               (+ r 1)
                                                               (- r 1))))))
         ,w2 (logxor ,w2 (sm4-f (logxor ,w0 ,w1 ,w3
                                        (aref ,round-keys ,(if encrypt-p
                                                               (+ r 2)
                                                               (- r 2))))))
         ,w3 (logxor ,w3 (sm4-f (logxor ,w0 ,w1 ,w2
                                        (aref ,round-keys ,(if encrypt-p
                                                               (+ r 3)
                                                               (- r 3))))))))


;;;
;;; Key schedule
;;;

(defclass sm4 (cipher 16-byte-block-mixin)
  ((round-keys :accessor sm4-round-keys
               :initform (make-array 32 :element-type '(unsigned-byte 32))
               :type (simple-array (unsigned-byte 32) (32)))))

(defmethod schedule-key ((cipher sm4) key)
  (let ((round-keys (sm4-round-keys cipher))
        (k0 (logxor (ub32ref/be key 0) #xa3b1bac6))
        (k1 (logxor (ub32ref/be key 4) #x56aa3350))
        (k2 (logxor (ub32ref/be key 8) #x677d9197))
        (k3 (logxor (ub32ref/be key 12) #xb27022dc)))
    (declare (type (simple-array (unsigned-byte 32) (32)) round-keys)
             (type (unsigned-byte 32) k0 k1 k2 k3))
    (dotimes (i 8)
      (setf k0 (logxor k0 (sm4-g (logxor k1 k2 k3 (aref +sm4-ck+ (* 4 i)))))
            (aref round-keys (* 4 i)) k0
            k1 (logxor k1 (sm4-g (logxor k2 k3 k0 (aref +sm4-ck+ (+ (* 4 i) 1)))))
            (aref round-keys (+ (* 4 i) 1)) k1
            k2 (logxor k2 (sm4-g (logxor k3 k0 k1 (aref +sm4-ck+ (+ (* 4 i) 2)))))
            (aref round-keys (+ (* 4 i) 2)) k2
            k3 (logxor k3 (sm4-g (logxor k0 k1 k2 (aref +sm4-ck+ (+ (* 4 i) 3)))))
            (aref round-keys (+ (* 4 i) 3)) k3))
    cipher))


;;;
;;; Rounds
;;;

(define-block-encryptor sm4 16
  (let ((round-keys (sm4-round-keys context)))
    (declare (type (simple-array (unsigned-byte 32) (32)) round-keys))
    (with-words ((w0 w1 w2 w3) plaintext plaintext-start :size 4)
      (sm4-round w0 w1 w2 w3 round-keys 0 t)
      (sm4-round w0 w1 w2 w3 round-keys 4 t)
      (sm4-round w0 w1 w2 w3 round-keys 8 t)
      (sm4-round w0 w1 w2 w3 round-keys 12 t)
      (sm4-round w0 w1 w2 w3 round-keys 16 t)
      (sm4-round w0 w1 w2 w3 round-keys 20 t)
      (sm4-round w0 w1 w2 w3 round-keys 24 t)
      (sm4-round w0 w1 w2 w3 round-keys 28 t)
      (store-words ciphertext ciphertext-start w3 w2 w1 w0)))
  (values))

(define-block-decryptor sm4 16
  (let ((round-keys (sm4-round-keys context)))
    (declare (type (simple-array (unsigned-byte 32) (32)) round-keys))
    (with-words ((w0 w1 w2 w3) ciphertext ciphertext-start :size 4)
      (sm4-round w0 w1 w2 w3 round-keys 31 nil)
      (sm4-round w0 w1 w2 w3 round-keys 27 nil)
      (sm4-round w0 w1 w2 w3 round-keys 23 nil)
      (sm4-round w0 w1 w2 w3 round-keys 19 nil)
      (sm4-round w0 w1 w2 w3 round-keys 15 nil)
      (sm4-round w0 w1 w2 w3 round-keys 11 nil)
      (sm4-round w0 w1 w2 w3 round-keys 7 nil)
      (sm4-round w0 w1 w2 w3 round-keys 3 nil)
      (store-words plaintext plaintext-start w3 w2 w1 w0)))
  (values))

(defcipher sm4
  (:encrypt-function sm4-encrypt-block)
  (:decrypt-function sm4-decrypt-block)
  (:block-length 16)
  (:key-length (:fixed 16)))
