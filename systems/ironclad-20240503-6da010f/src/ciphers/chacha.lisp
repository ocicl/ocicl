;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;; chacha.lisp --implementation of the ChaCha stream cipher

(in-package :crypto)

(declaim (type (simple-octet-vector 16) chacha-sigma chacha-tau))
(defconst chacha-sigma
  #.(coerce (map 'vector #'char-code "expand 32-byte k") 'simple-octet-vector))

(defconst chacha-tau
  #.(coerce (map 'vector #'char-code "expand 16-byte k") 'simple-octet-vector))

(deftype chacha-state () '(simple-array (unsigned-byte 32) (16)))
(deftype chacha-keystream-buffer () '(simple-octet-vector 64))

(declaim (inline chacha-core))
(defun chacha-core (n-rounds buffer state)
  (declare (type chacha-keystream-buffer buffer))
  (declare (type chacha-state state))
  (declare (optimize speed))
  #+(or (and ecl ironclad-assembly)
        (and sbcl x86-64 ironclad-assembly))
  (x-chacha-core n-rounds buffer state)
  #-(or (and ecl ironclad-assembly)
        (and sbcl x86-64 ironclad-assembly))
  (let ((x (make-array 16 :element-type '(unsigned-byte 32))))
    (declare (dynamic-extent x))
    (replace x state)
    (macrolet ((combine (x y shift)
                 `(rol32 (logxor ,x ,y) ,shift))
               (ref (i)
                 `(aref x ,i))
               (quarter-round (y0 y1 y2 y3)
                 `(setf (ref ,y0) (mod32+ (ref ,y0) (ref ,y1))
                        (ref ,y3) (combine (ref ,y3) (ref ,y0) 16)
                        (ref ,y2) (mod32+ (ref ,y2) (ref ,y3))
                        (ref ,y1) (combine (ref ,y1) (ref ,y2) 12)
                        (ref ,y0) (mod32+ (ref ,y0) (ref ,y1))
                        (ref ,y3) (combine (ref ,y3) (ref ,y0) 8)
                        (ref ,y2) (mod32+ (ref ,y2) (ref ,y3))
                        (ref ,y1) (combine (ref ,y1) (ref ,y2) 7))))
      (dotimes (i n-rounds)
        (quarter-round 0 4 8 12)
        (quarter-round 1 5 9 13)
        (quarter-round 2 6 10 14)
        (quarter-round 3 7 11 15)

        (quarter-round 0 5 10 15)
        (quarter-round 1 6 11 12)
        (quarter-round 2 7 8 13)
        (quarter-round 3 4 9 14))
      (dotimes (i 16)
        (setf (ub32ref/le buffer (* i 4))
              (mod32+ (aref x i) (aref state i))))))
  (values))

(defun chacha/8-core (buffer state)
  (declare (type chacha-keystream-buffer buffer))
  (declare (type chacha-state state))
  (chacha-core 4 buffer state))

(defun chacha/12-core (buffer state)
  (declare (type chacha-keystream-buffer buffer))
  (declare (type chacha-state state))
  (chacha-core 6 buffer state))

(defun chacha/20-core (buffer state)
  (declare (type chacha-keystream-buffer buffer))
  (declare (type chacha-state state))
  (chacha-core 10 buffer state))

(defclass chacha (stream-cipher)
  ((state :reader chacha-state
          :initform (make-array 16 :element-type '(unsigned-byte 32)
                                :initial-element 0)
          :type chacha-state)
   (counter-size :accessor chacha-counter-size
                 :initform 2
                 :type (integer 1 2))
   (keystream-buffer :reader chacha-keystream-buffer
                     :initform (make-array 64 :element-type '(unsigned-byte 8))
                     :type chacha-keystream-buffer)
   (keystream-buffer-remaining :accessor chacha-keystream-buffer-remaining
                               :initform 0
                               :type (integer 0 64))
   (core-function :reader chacha-core-function
                  :initarg :core-function
                  :type function))
  (:default-initargs :core-function #'chacha/20-core))

(defclass chacha/12 (chacha)
  ()
  (:default-initargs :core-function #'chacha/12-core))

(defclass chacha/8 (chacha)
  ()
  (:default-initargs :core-function #'chacha/8-core))

(defun chacha-keyify (cipher key)
  (declare (type chacha cipher))
  (let ((state (chacha-state cipher)))
    (declare (type chacha-state state))
    (multiple-value-bind (constants offset)
        (if (= (length key) 16)
            (values chacha-tau 0)
            (values chacha-sigma 16))
      (setf (aref state 4) (ub32ref/le key 0)
            (aref state 5) (ub32ref/le key 4)
            (aref state 6) (ub32ref/le key 8)
            (aref state 7) (ub32ref/le key 12))
      (setf (aref state 8) (ub32ref/le key (+ offset 0))
            (aref state 9) (ub32ref/le key (+ offset 4))
            (aref state 10) (ub32ref/le key (+ offset 8))
            (aref state 11) (ub32ref/le key (+ offset 12)))
      (setf (aref state 0) (ub32ref/le constants 0)
            (aref state 1) (ub32ref/le constants 4)
            (aref state 2) (ub32ref/le constants 8)
            (aref state 3) (ub32ref/le constants 12))
      (values))))

(defmethod shared-initialize :after ((cipher chacha) slot-names
                                     &rest initargs
                                     &key (key nil key-p)
                                     (initialization-vector nil iv-p)
                                     &allow-other-keys)
  (declare (ignore initargs key key-p iv-p))
  (setf (chacha-keystream-buffer-remaining cipher) 0)
  (when initialization-vector
    (when (< (length initialization-vector) 8)
      (error 'invalid-initialization-vector
             :cipher (class-name (class-of cipher))
             :block-length 8))
    (let ((state (chacha-state cipher)))
      (declare (type chacha-state state))
      (case (length initialization-vector)
        ((12)
         ;; 32-bit counter and 96-bit nonce of the RFC 8439 variant
         (setf (chacha-counter-size cipher) 1)
         (setf (aref state 12) 0
               (aref state 13) (ub32ref/le initialization-vector 0)
               (aref state 14) (ub32ref/le initialization-vector 4)
               (aref state 15) (ub32ref/le initialization-vector 8)))
        (t
         ;; 64-bit counter and 64-bit nonce of the original algorithm
         (setf (chacha-counter-size cipher) 2)
         (setf (aref state 12) 0
               (aref state 13) 0
               (aref state 14) (ub32ref/le initialization-vector 0)
               (aref state 15) (ub32ref/le initialization-vector 4))))))
  cipher)

(defmethod schedule-key ((cipher chacha) key)
  (chacha-keyify cipher key)
  cipher)

(define-stream-cryptor chacha
  (let ((state (chacha-state context))
        (keystream-buffer (chacha-keystream-buffer context))
        (keystream-buffer-remaining (chacha-keystream-buffer-remaining context))
        (core-function (chacha-core-function context)))
    (declare (type chacha-state state)
             (type chacha-keystream-buffer keystream-buffer)
             (type (integer 0 64) keystream-buffer-remaining)
             (type function core-function))
    (unless (zerop length)
      (unless (zerop keystream-buffer-remaining)
        (let ((size (min length keystream-buffer-remaining)))
          (declare (type (integer 0 64) size))
          (xor-block size keystream-buffer (- 64 keystream-buffer-remaining)
                     plaintext plaintext-start
                     ciphertext ciphertext-start)
          (decf keystream-buffer-remaining size)
          (decf length size)
          (incf ciphertext-start size)
          (incf plaintext-start size)))
      (unless (zerop length)
        (loop
          (funcall core-function keystream-buffer state)
          (when (zerop (setf (aref state 12)
                             (mod32+ (aref state 12) 1)))
            (setf (aref state 13) (mod32+ (aref state 13) 1)))
          (when (<= length 64)
            (xor-block length keystream-buffer 0 plaintext plaintext-start
                       ciphertext ciphertext-start)
            (setf (chacha-keystream-buffer-remaining context) (- 64 length))
            (return-from chacha-crypt (values)))
          (xor-block 64 keystream-buffer 0 plaintext plaintext-start
                     ciphertext ciphertext-start)
          (decf length 64)
          (incf ciphertext-start 64)
          (incf plaintext-start 64)))
      (setf (chacha-keystream-buffer-remaining context) keystream-buffer-remaining))
    (values)))

(defcipher chacha
  (:mode :stream)
  (:crypt-function chacha-crypt)
  (:key-length (:fixed 16 32)))

(defcipher chacha/12
  (:mode :stream)
  (:crypt-function chacha-crypt)
  (:key-length (:fixed 16 32)))

(defcipher chacha/8
  (:mode :stream)
  (:crypt-function chacha-crypt)
  (:key-length (:fixed 16 32)))
