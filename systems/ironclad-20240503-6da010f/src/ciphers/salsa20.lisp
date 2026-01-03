;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;; salsa20.lisp --implementation of the Salsa20 stream cipher

(in-package :crypto)

(declaim (type (simple-octet-vector 16) salsa20-sigma salsa20-tau))
(defconst salsa20-sigma
  #.(coerce (map 'vector #'char-code "expand 32-byte k") 'simple-octet-vector))

(defconst salsa20-tau
  #.(coerce (map 'vector #'char-code "expand 16-byte k") 'simple-octet-vector))

(deftype salsa20-state () '(simple-array (unsigned-byte 32) (16)))
(deftype salsa20-keystream-buffer () '(simple-octet-vector 64))

(declaim (inline salsa-core))
(defun salsa-core (n-rounds buffer state)
  (declare (type salsa20-keystream-buffer buffer))
  (declare (type salsa20-state state))
  (declare (optimize speed))
  #+(or (and ecl ironclad-assembly)
        (and sbcl x86-64 ironclad-assembly))
  (x-salsa-core n-rounds buffer state)
  #-(or (and ecl ironclad-assembly)
        (and sbcl x86-64 ironclad-assembly))
  (let ((x (make-array 16 :element-type '(unsigned-byte 32))))
    (declare (dynamic-extent x))
    (replace x state)
    (macrolet ((combine (x y z shift)
                 `(logxor ,x (rol32 (mod32+ ,y ,z) ,shift)))
               (ref (i)
                 `(aref x ,i))
               (quarter-round (y0 y1 y2 y3)
                 `(setf (ref ,y0) (combine (ref ,y0) (ref ,y3) (ref ,y2) 7)
                        (ref ,y1) (combine (ref ,y1) (ref ,y0) (ref ,y3) 9)
                        (ref ,y2) (combine (ref ,y2) (ref ,y1) (ref ,y0) 13)
                        (ref ,y3) (combine (ref ,y3) (ref ,y2) (ref ,y1) 18))))
      (dotimes (i n-rounds)
        (quarter-round 4 8 12 0)
        (quarter-round 9 13 1 5)
        (quarter-round 14 2 6 10)
        (quarter-round 3 7 11 15)

        (quarter-round 1 2 3 0)
        (quarter-round 6 7 4 5)
        (quarter-round 11 8 9 10)
        (quarter-round 12 13 14 15))
      (dotimes (i 16)
        (setf (ub32ref/le buffer (* i 4))
              (mod32+ (aref x i) (aref state i))))))
  (values))

(defun salsa20/8-core (buffer state)
  (declare (type salsa20-keystream-buffer buffer))
  (declare (type salsa20-state state))
  (salsa-core 4 buffer state))

(defun salsa20/12-core (buffer state)
  (declare (type salsa20-keystream-buffer buffer))
  (declare (type salsa20-state state))
  (salsa-core 6 buffer state))

(defun salsa20/20-core (buffer state)
  (declare (type salsa20-keystream-buffer buffer))
  (declare (type salsa20-state state))
  (salsa-core 10 buffer state))

(defclass salsa20 (stream-cipher)
  ((state :reader salsa20-state
          :initform (make-array 16 :element-type '(unsigned-byte 32)
                                :initial-element 0)
          :type salsa20-state)
   (keystream-buffer :reader salsa20-keystream-buffer
                     :initform (make-array 64 :element-type '(unsigned-byte 8))
                     :type salsa20-keystream-buffer)
   (keystream-buffer-remaining :accessor salsa20-keystream-buffer-remaining
                               :initform 0
                               :type (integer 0 64))
   (core-function :reader salsa20-core-function
                  :initarg :core-function
                  :type function))
  (:default-initargs :core-function #'salsa20/20-core))

(defclass salsa20/12 (salsa20)
  ()
  (:default-initargs :core-function #'salsa20/12-core))

(defclass salsa20/8 (salsa20)
  ()
  (:default-initargs :core-function #'salsa20/8-core))

(defun salsa20-keyify (cipher key)
  (declare (type salsa20 cipher))
  (let ((state (salsa20-state cipher)))
    (declare (type salsa20-state state))
    (multiple-value-bind (constants offset)
        (if (= (length key) 16)
            (values salsa20-tau 0)
            (values salsa20-sigma 16))
      (setf (aref state 1) (ub32ref/le key 0)
            (aref state 2) (ub32ref/le key 4)
            (aref state 3) (ub32ref/le key 8)
            (aref state 4) (ub32ref/le key 12))
      (setf (aref state 11) (ub32ref/le key (+ offset 0))
            (aref state 12) (ub32ref/le key (+ offset 4))
            (aref state 13) (ub32ref/le key (+ offset 8))
            (aref state 14) (ub32ref/le key (+ offset 12)))
      (setf (aref state 0) (ub32ref/le constants 0)
            (aref state 5) (ub32ref/le constants 4)
            (aref state 10) (ub32ref/le constants 8)
            (aref state 15) (ub32ref/le constants 12))
      (values))))

(defmethod shared-initialize :after ((cipher salsa20) slot-names
                                     &rest initargs
                                     &key (key nil key-p)
                                     (initialization-vector nil iv-p)
                                     &allow-other-keys)
  (declare (ignore initargs key key-p iv-p))
  (setf (salsa20-keystream-buffer-remaining cipher) 0)
  (when initialization-vector
    (when (< (length initialization-vector) 8)
      (error 'invalid-initialization-vector
             :cipher (class-name (class-of cipher))
             :block-length 8))
    (let ((state (salsa20-state cipher)))
      (declare (type salsa20-state state))
      (setf (aref state 6) (ub32ref/le initialization-vector 0)
            (aref state 7) (ub32ref/le initialization-vector 4)
            (aref state 8) 0
            (aref state 9) 0)))
  cipher)

(defmethod schedule-key ((cipher salsa20) key)
  (salsa20-keyify cipher key)
  cipher)

(define-stream-cryptor salsa20
  (let ((state (salsa20-state context))
        (keystream-buffer (salsa20-keystream-buffer context))
        (keystream-buffer-remaining (salsa20-keystream-buffer-remaining context))
        (core-function (salsa20-core-function context)))
    (declare (type salsa20-state state)
             (type salsa20-keystream-buffer keystream-buffer)
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
          (when (zerop (setf (aref state 8)
                             (mod32+ (aref state 8) 1)))
            (setf (aref state 9) (mod32+ (aref state 9) 1)))
          (when (<= length 64)
            (xor-block length keystream-buffer 0 plaintext plaintext-start
                       ciphertext ciphertext-start)
            (setf (salsa20-keystream-buffer-remaining context) (- 64 length))
            (return-from salsa20-crypt (values)))
          (xor-block 64 keystream-buffer 0 plaintext plaintext-start
                     ciphertext ciphertext-start)
          (decf length 64)
          (incf ciphertext-start 64)
          (incf plaintext-start 64)))
      (setf (salsa20-keystream-buffer-remaining context) keystream-buffer-remaining))
    (values)))

(defcipher salsa20
  (:mode :stream)
  (:crypt-function salsa20-crypt)
  (:key-length (:fixed 16 32)))

(defcipher salsa20/12
  (:mode :stream)
  (:crypt-function salsa20-crypt)
  (:key-length (:fixed 16 32)))

(defcipher salsa20/8
  (:mode :stream)
  (:crypt-function salsa20-crypt)
  (:key-length (:fixed 16 32)))
