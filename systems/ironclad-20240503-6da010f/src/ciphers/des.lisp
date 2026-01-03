;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; des.lisp -- implementation of DES

;;; converted from the C code appearing in _Applied Cryptography_ by
;;; Bruce Schneier to Common Lisp.  Unfortunately, a lot of C-isms
;;; remain, so this is not the prettiest Common Lisp code ever.

(in-package :crypto)
(in-ironclad-readtable)


;;; the sboxes of DES

(declaim (type (simple-array (unsigned-byte 32) (64))
               des-sbox0 des-sbox1 des-sbox2 des-sbox3
               des-sbox4 des-sbox5 des-sbox6 des-sbox7))
(defconst des-sbox0
#32@(
    #x01010400 #x00000000 #x00010000 #x01010404
    #x01010004 #x00010404 #x00000004 #x00010000
    #x00000400 #x01010400 #x01010404 #x00000400
    #x01000404 #x01010004 #x01000000 #x00000004
    #x00000404 #x01000400 #x01000400 #x00010400
    #x00010400 #x01010000 #x01010000 #x01000404
    #x00010004 #x01000004 #x01000004 #x00010004
    #x00000000 #x00000404 #x00010404 #x01000000
    #x00010000 #x01010404 #x00000004 #x01010000
    #x01010400 #x01000000 #x01000000 #x00000400
    #x01010004 #x00010000 #x00010400 #x01000004
    #x00000400 #x00000004 #x01000404 #x00010404
    #x01010404 #x00010004 #x01010000 #x01000404
    #x01000004 #x00000404 #x00010404 #x01010400
    #x00000404 #x01000400 #x01000400 #x00000000
    #x00010004 #x00010400 #x00000000 #x01010004
))

(defconst des-sbox1
#32@(
    #x80108020 #x80008000 #x00008000 #x00108020
    #x00100000 #x00000020 #x80100020 #x80008020
    #x80000020 #x80108020 #x80108000 #x80000000
    #x80008000 #x00100000 #x00000020 #x80100020
    #x00108000 #x00100020 #x80008020 #x00000000
    #x80000000 #x00008000 #x00108020 #x80100000
    #x00100020 #x80000020 #x00000000 #x00108000
    #x00008020 #x80108000 #x80100000 #x00008020
    #x00000000 #x00108020 #x80100020 #x00100000
    #x80008020 #x80100000 #x80108000 #x00008000
    #x80100000 #x80008000 #x00000020 #x80108020
    #x00108020 #x00000020 #x00008000 #x80000000
    #x00008020 #x80108000 #x00100000 #x80000020
    #x00100020 #x80008020 #x80000020 #x00100020
    #x00108000 #x00000000 #x80008000 #x00008020
    #x80000000 #x80100020 #x80108020 #x00108000
))

(defconst des-sbox2
#32@(
    #x00000208 #x08020200 #x00000000 #x08020008
    #x08000200 #x00000000 #x00020208 #x08000200
    #x00020008 #x08000008 #x08000008 #x00020000
    #x08020208 #x00020008 #x08020000 #x00000208
    #x08000000 #x00000008 #x08020200 #x00000200
    #x00020200 #x08020000 #x08020008 #x00020208
    #x08000208 #x00020200 #x00020000 #x08000208
    #x00000008 #x08020208 #x00000200 #x08000000
    #x08020200 #x08000000 #x00020008 #x00000208
    #x00020000 #x08020200 #x08000200 #x00000000
    #x00000200 #x00020008 #x08020208 #x08000200
    #x08000008 #x00000200 #x00000000 #x08020008
    #x08000208 #x00020000 #x08000000 #x08020208
    #x00000008 #x00020208 #x00020200 #x08000008
    #x08020000 #x08000208 #x00000208 #x08020000
    #x00020208 #x00000008 #x08020008 #x00020200
))

(defconst des-sbox3
#32@(
    #x00802001 #x00002081 #x00002081 #x00000080
    #x00802080 #x00800081 #x00800001 #x00002001
    #x00000000 #x00802000 #x00802000 #x00802081
    #x00000081 #x00000000 #x00800080 #x00800001
    #x00000001 #x00002000 #x00800000 #x00802001
    #x00000080 #x00800000 #x00002001 #x00002080
    #x00800081 #x00000001 #x00002080 #x00800080
    #x00002000 #x00802080 #x00802081 #x00000081
    #x00800080 #x00800001 #x00802000 #x00802081
    #x00000081 #x00000000 #x00000000 #x00802000
    #x00002080 #x00800080 #x00800081 #x00000001
    #x00802001 #x00002081 #x00002081 #x00000080
    #x00802081 #x00000081 #x00000001 #x00002000
    #x00800001 #x00002001 #x00802080 #x00800081
    #x00002001 #x00002080 #x00800000 #x00802001
    #x00000080 #x00800000 #x00002000 #x00802080
))

(defconst des-sbox4
#32@(
    #x00000100 #x02080100 #x02080000 #x42000100
    #x00080000 #x00000100 #x40000000 #x02080000
    #x40080100 #x00080000 #x02000100 #x40080100
    #x42000100 #x42080000 #x00080100 #x40000000
    #x02000000 #x40080000 #x40080000 #x00000000
    #x40000100 #x42080100 #x42080100 #x02000100
    #x42080000 #x40000100 #x00000000 #x42000000
    #x02080100 #x02000000 #x42000000 #x00080100
    #x00080000 #x42000100 #x00000100 #x02000000
    #x40000000 #x02080000 #x42000100 #x40080100
    #x02000100 #x40000000 #x42080000 #x02080100
    #x40080100 #x00000100 #x02000000 #x42080000
    #x42080100 #x00080100 #x42000000 #x42080100
    #x02080000 #x00000000 #x40080000 #x42000000
    #x00080100 #x02000100 #x40000100 #x00080000
    #x00000000 #x40080000 #x02080100 #x40000100
))

(defconst des-sbox5
#32@(
    #x20000010 #x20400000 #x00004000 #x20404010
    #x20400000 #x00000010 #x20404010 #x00400000
    #x20004000 #x00404010 #x00400000 #x20000010
    #x00400010 #x20004000 #x20000000 #x00004010
    #x00000000 #x00400010 #x20004010 #x00004000
    #x00404000 #x20004010 #x00000010 #x20400010
    #x20400010 #x00000000 #x00404010 #x20404000
    #x00004010 #x00404000 #x20404000 #x20000000
    #x20004000 #x00000010 #x20400010 #x00404000
    #x20404010 #x00400000 #x00004010 #x20000010
    #x00400000 #x20004000 #x20000000 #x00004010
    #x20000010 #x20404010 #x00404000 #x20400000
    #x00404010 #x20404000 #x00000000 #x20400010
    #x00000010 #x00004000 #x20400000 #x00404010
    #x00004000 #x00400010 #x20004010 #x00000000
    #x20404000 #x20000000 #x00400010 #x20004010
))

(defconst des-sbox6
#32@(
    #x00200000 #x04200002 #x04000802 #x00000000
    #x00000800 #x04000802 #x00200802 #x04200800
    #x04200802 #x00200000 #x00000000 #x04000002
    #x00000002 #x04000000 #x04200002 #x00000802
    #x04000800 #x00200802 #x00200002 #x04000800
    #x04000002 #x04200000 #x04200800 #x00200002
    #x04200000 #x00000800 #x00000802 #x04200802
    #x00200800 #x00000002 #x04000000 #x00200800
    #x04000000 #x00200800 #x00200000 #x04000802
    #x04000802 #x04200002 #x04200002 #x00000002
    #x00200002 #x04000000 #x04000800 #x00200000
    #x04200800 #x00000802 #x00200802 #x04200800
    #x00000802 #x04000002 #x04200802 #x04200000
    #x00200800 #x00000000 #x00000002 #x04200802
    #x00000000 #x00200802 #x04200000 #x00000800
    #x04000002 #x04000800 #x00000800 #x00200002
))

(defconst des-sbox7
#32@(
    #x10001040 #x00001000 #x00040000 #x10041040
    #x10000000 #x10001040 #x00000040 #x10000000
    #x00040040 #x10040000 #x10041040 #x00041000
    #x10041000 #x00041040 #x00001000 #x00000040
    #x10040000 #x10000040 #x10001000 #x00001040
    #x00041000 #x00040040 #x10040040 #x10041000
    #x00001040 #x00000000 #x00000000 #x10040040
    #x10000040 #x10001000 #x00041040 #x00040000
    #x00041040 #x00040000 #x10041000 #x00001000
    #x00000040 #x10040040 #x00001000 #x00041040
    #x10001000 #x00000040 #x10000040 #x10040000
    #x10040040 #x10000000 #x00040000 #x10001040
    #x00000000 #x10041040 #x00040040 #x10000040
    #x10040000 #x10001000 #x10001040 #x00000000
    #x10041040 #x00041000 #x00041000 #x00001040
    #x00001040 #x00040040 #x10000000 #x10041000
))


;;; permutations and rotations for the key schedule
(defconst permutation1
  (make-array 56 :element-type '(unsigned-byte 8)
              :initial-contents (list 56 48 40 32 24 16 8 0
                                      57 49 41 33 25 17 9 1
                                      58 50 42 34 26 18 10 2
                                      59 51 43 35 62 54 46 38 30
                                      22 14 6 61 53 45 37 29
                                      21 13 5 60 52 44 36 28
                                      20 12 4 27 19 11 3)))

(defconst total-rotations
  (make-array 16 :element-type '(unsigned-byte 5)
              :initial-contents (list 1 2 4 6 8 10 12 14
                                      15 17 19 21 23 25 27 28)))

(defconst permutation2
  (make-array 48 :element-type '(unsigned-byte 8)
              :initial-contents (list 13 16 10 23 0 4
                                      2 27 14 5 20 9
                                      22 18 11 3 25 7
                                      15 6 26 19 12 1
                                      40 51 30 36 46 54
                                      29 39 50 44 32 47
                                      43 48 38 55 33 52
                                      45 41 49 35 28 31)))


;;; actual encryption and decryption guts

(deftype des-round-keys () '(simple-array (unsigned-byte 32) (32)))

(macrolet ((frob (left right shift-amount constant)
                   `(setf work (logand (logxor (mod32ash ,left
                                                         ,shift-amount) ,right)
                                ,constant)
                     ,right (logxor ,right work)
                     ,left (logxor (mod32ash work ,(- shift-amount)) ,left)))
           (6-bits (val offset) `(ldb (byte 6 ,offset) ,val))
           (sbox-subst (val sbox0 sbox1 sbox2 sbox3)
             `(logior (aref ,sbox0 (6-bits ,val 0))
               (aref ,sbox1 (6-bits ,val 8))
               (aref ,sbox2 (6-bits ,val 16))
               (aref ,sbox3 (6-bits ,val 24))))
           (des-round (left right keys index)
             `(let* ((work (logxor (rol32 ,right 28) (aref ,keys ,index)))
                     (fval (sbox-subst work des-sbox6 des-sbox4
                                      des-sbox2 des-sbox0)))
               (declare (type (unsigned-byte 32) work fval))
               (setf work (logxor ,right (aref ,keys (1+ ,index)))
                fval (logior fval (sbox-subst work des-sbox7 des-sbox5
                                              des-sbox3 des-sbox1))
                ,left (logxor ,left fval))))
           (des-initial-permutation (left right)
             `(progn
                (frob ,left ,right -4 #x0f0f0f0f)
                (frob ,left ,right -16 #x0000ffff)
                (frob ,right ,left -2 #x33333333)
                (frob ,right ,left -8 #x00ff00ff)
    
                (setf ,right (rol32 ,right 1)
                      work (logand (logxor ,left ,right) #xaaaaaaaa)
                      ,left (logxor ,left work)
                      ,right (logxor ,right work)
                      ,left (rol32 ,left 1))))
           (des-final-permutation (left right)
             `(progn
                (setf ,right (rol32 ,right 31)
                      work (logand (logxor ,left ,right) #xaaaaaaaa)
                      ,left (logxor ,left work)
                      ,right (logxor ,right work)
                      ,left (rol32 ,left 31))
                (frob ,left ,right -8 #x00ff00ff)
                (frob ,left ,right -2 #x33333333)
                (frob ,right ,left -16 #x0000ffff)
                (frob ,right ,left -4 #x0f0f0f0f)))
           (des-munge-core (left right keys)
             `(do ((round 0 (1+ round))
                   (key-index 0 (+ key-index 4)))
                  ((>= round 8))
                (des-round ,left ,right ,keys key-index)
                (des-round ,right ,left ,keys (+ key-index 2)))))

(defun des-munge-block (input input-start output output-start keys)
  (declare (type (simple-array (unsigned-byte 8) (*)) input output))
  (declare (type (integer 0 #.(- array-dimension-limit 8))
                 input-start output-start))
  (declare (type des-round-keys keys))
  (with-words ((left right) input input-start)
    (let ((work 0))
      (declare (type (unsigned-byte 32) work))
      (des-initial-permutation left right)
      ;; now the real work begins
      (des-munge-core left right keys)
      (des-final-permutation left right)
      (store-words output output-start right left))))

(defun 3des-munge-block (input input-start output output-start k1 k2 k3)
  (declare (type (simple-array (unsigned-byte 8) (*)) input output))
  (declare (type (integer 0 #.(- array-dimension-limit 8))
                 input-start output-start))
  (declare (type des-round-keys k1 k2 k3))
  (with-words ((left right) input input-start)
    (let ((work 0))
      (declare (type (unsigned-byte 32) work))
      (des-initial-permutation left right)
      ;; now the real work begins
      (des-munge-core left right k1)
      (des-munge-core right left k2)
      (des-munge-core left right k3)
      (des-final-permutation left right)
      (store-words output output-start right left))))

) ; MACROLET


;;; ECB mode encryption and decryption

(defclass des (cipher 8-byte-block-mixin)
  ((encryption-keys :accessor encryption-keys :type des-round-keys)
   (decryption-keys :accessor decryption-keys :type des-round-keys)))

(define-block-encryptor des 8
  (des-munge-block plaintext plaintext-start ciphertext ciphertext-start
                   (encryption-keys context)))

(define-block-decryptor des 8
  (des-munge-block ciphertext ciphertext-start plaintext plaintext-start
                   (decryption-keys context)))

(defclass 3des (cipher 8-byte-block-mixin)
  ((encryption-keys-1 :accessor encryption-keys-1 :type des-round-keys)
   (decryption-keys-1 :accessor decryption-keys-1 :type des-round-keys)
   (encryption-keys-2 :accessor encryption-keys-2 :type des-round-keys)
   (decryption-keys-2 :accessor decryption-keys-2 :type des-round-keys)
   (encryption-keys-3 :accessor encryption-keys-3 :type des-round-keys)
   (decryption-keys-3 :accessor decryption-keys-3 :type des-round-keys)))

(define-block-encryptor 3des 8
  (3des-munge-block plaintext plaintext-start ciphertext ciphertext-start
                   (encryption-keys-1 context) 
                   (decryption-keys-2 context)
                   (encryption-keys-3 context)))

(define-block-decryptor 3des 8
  (3des-munge-block ciphertext ciphertext-start plaintext plaintext-start
                   (decryption-keys-3 context) 
                   (encryption-keys-2 context)
                   (decryption-keys-1 context)))


;;; key scheduling

;;; `dough' being a cute pun from Schiener's code.
(defun des-cook-key-schedule (dough)
  (let ((schedule (make-array 32 :element-type '(unsigned-byte 32) :initial-element 0)))
    (declare (type des-round-keys dough schedule))
    (do ((dough-index 0 (+ dough-index 2))
         (schedule-index 0 (+ schedule-index 2)))
        ((>= dough-index 32) schedule)
      (declare (optimize (debug 3)))
      (let ((schedule-index+1 (1+ schedule-index))
            (dough-index+1 (1+ dough-index)))
        (setf (aref schedule schedule-index)
              (let ((dough0 (aref dough dough-index))
                    (dough1 (aref dough dough-index+1)))
                (logior (mod32ash (mask-field (byte 6 18) dough0) 6)
                        (mod32ash (mask-field (byte 6 6) dough0) 10)
                        (mod32ash (mask-field (byte 6 18) dough1) -10)
                        (mod32ash (mask-field (byte 6 6) dough1) -6)))
              (aref schedule schedule-index+1)
              (let ((dough0 (aref dough dough-index))
                    (dough1 (aref dough dough-index+1)))
                (logior (mod32ash (mask-field (byte 6 12) dough0) 12)
                        (mod32ash (mask-field (byte 6 0) dough0) 16)
                        (mod32ash (mask-field (byte 6 12) dough1) -4)
                        (mask-field (byte 6 0) dough1))))))))

(defun compute-des-encryption-keys (key)
  (declare (type (simple-array (unsigned-byte 8) (8)) key))
  (let ((pc1m (make-array 56 :element-type '(unsigned-byte 8) :initial-element 0))
        (pcr (make-array 56 :element-type '(unsigned-byte 8) :initial-element 0))
        (kn (make-array 32 :element-type '(unsigned-byte 32) :initial-element 0)))
    (dotimes (j 56)
      (let* ((l (aref permutation1 j))
             (m (logand l #x7)))
        (setf (aref pc1m j)
              (logand (aref key (ldb (byte 4 3) l))
                      (ash 1 (- 7 m))))))
    (dotimes (i 16)
      (let* ((m (ash i 1))
             (n (1+ m)))
        (dotimes (j 28)
          (let ((l (+ j (aref total-rotations i))))
            (if (< l 28)
                (setf (aref pcr j) (aref pc1m l))
                (setf (aref pcr j) (aref pc1m (- l 28))))))
        (do ((j 28 (1+ j)))
            ((= j 56))
          (let ((l (+ j (aref total-rotations i))))
            (if (< l 56)
                (setf (aref pcr j) (aref pc1m l))
                (setf (aref pcr j) (aref pc1m (- l 28))))))
        (dotimes (j 24)
          (unless (zerop (aref pcr (aref permutation2 j)))
            (setf (aref kn m) (logior (aref kn m) (ash 1 (- 24 (1+ j))))))
          (unless (zerop (aref pcr (aref permutation2 (+ j 24))))
            (setf (aref kn n) (logior (aref kn n) (ash 1 (- 24 (1+ j)))))))))
    (des-cook-key-schedule kn)))

(defun compute-round-keys-for-des-key (key)
  (let ((encryption-keys (compute-des-encryption-keys key))
        (decryption-keys (make-array 32 :element-type '(unsigned-byte 32))))
    (declare (type des-round-keys encryption-keys decryption-keys))
    (do ((i 0 (+ i 2)))
        ((= i 32)
         (values encryption-keys decryption-keys))
      (setf (aref decryption-keys (1+ i)) (aref encryption-keys (- 31 i))
            (aref decryption-keys i) (aref encryption-keys (- 31 (1+ i)))))))

(defmethod schedule-key ((cipher des) key)
  (multiple-value-bind (encryption-keys decryption-keys)
      (compute-round-keys-for-des-key key)
    (setf (encryption-keys cipher) encryption-keys
          (decryption-keys cipher) decryption-keys)
    cipher))

(defmethod schedule-key ((cipher 3des) key)
  (multiple-value-bind (ek1 dk1)
      (compute-round-keys-for-des-key (subseq key 0 8))
    (multiple-value-bind (ek2 dk2)
        (compute-round-keys-for-des-key (subseq key 8 16))
      (multiple-value-bind (ek3 dk3)
          (let ((length (length key)))
            (cond
              ((= length 16) (compute-round-keys-for-des-key (subseq key 0 8)))
              ((= length 24) (compute-round-keys-for-des-key (subseq key 16 24)))))
        (setf (encryption-keys-1 cipher) ek1
              (decryption-keys-1 cipher) dk1
              (encryption-keys-2 cipher) ek2
              (decryption-keys-2 cipher) dk2
              (encryption-keys-3 cipher) ek3
              (decryption-keys-3 cipher) dk3)
        cipher))))

(defcipher des
  (:encrypt-function des-encrypt-block)
  (:decrypt-function des-decrypt-block)
  (:block-length 8)
  (:key-length (:fixed 8)))

(defcipher 3des
  (:encrypt-function 3des-encrypt-block)
  (:decrypt-function 3des-decrypt-block)
  (:block-length 8)
  (:key-length (:fixed 16 24)))
