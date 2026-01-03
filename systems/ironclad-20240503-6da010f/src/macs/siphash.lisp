;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; siphash.lisp -- implementation of the SipHash MAC

(in-package :crypto)


(defclass siphash (mac)
  ((state :accessor siphash-state
          :initform (make-array 4 :element-type '(unsigned-byte 64))
          :type (simple-array (unsigned-byte 64) (4)))
   (compression-rounds :accessor siphash-compression-rounds
                       :initarg :compression-rounds
                       :initform 2
                       :type fixnum)
   (finalization-rounds :accessor siphash-finalization-rounds
                        :initarg :finalization-rounds
                        :initform 4
                        :type fixnum)
   (digest-length :accessor siphash-digest-length
                  :initarg :digest-length
                  :initform 8
                  :type fixnum)
   (data-length :accessor siphash-data-length
                :initform 0
                :type fixnum)
   (buffer :accessor siphash-buffer
           :initform (make-array 8 :element-type '(unsigned-byte 8))
           :type (simple-array (unsigned-byte 8) (8)))
   (buffer-length :accessor siphash-buffer-length
                  :initform 0
                  :type (integer 0 8))))

(defun make-siphash (key &key (compression-rounds 2) (finalization-rounds 4) (digest-length 8))
  (declare (type (simple-array (unsigned-byte 8) (*)) key))
  (unless (= (length key) 16)
    (error 'invalid-mac-parameter
           :mac-name 'siphash
           :message "The key length must be 16 bytes"))
  (unless (or (= digest-length 8) (= digest-length 16))
    (error 'invalid-mac-parameter
           :mac-name 'siphash
           :message "The digest length must be 8 or 16 bytes"))
  (make-instance 'siphash
                 :key key
                 :compression-rounds compression-rounds
                 :finalization-rounds finalization-rounds
                 :digest-length digest-length))

(defmethod shared-initialize :after ((mac siphash) slot-names &rest initargs &key key &allow-other-keys)
  (declare (ignore slot-names initargs)
           (type (simple-array (unsigned-byte 8) (16)) key))
  (let ((state (siphash-state mac))
        (k0 (ub64ref/le key 0))
        (k1 (ub64ref/le key 8)))
    (declare (type (simple-array (unsigned-byte 64) (4)) state)
             (type (unsigned-byte 64) k0 k1))
    (setf (aref state 0) (logxor k0 #x736f6d6570736575)
          (aref state 1) (logxor k1 #x646f72616e646f6d)
          (aref state 2) (logxor k0 #x6c7967656e657261)
          (aref state 3) (logxor k1 #x7465646279746573))
    (when (= (siphash-digest-length mac) 16)
      (setf (aref state 1) (logxor (aref state 1) #xee)))
    (setf (siphash-data-length mac) 0)
    (setf (siphash-buffer-length mac) 0)
    mac))

(defmacro siphash-round (v0 v1 v2 v3)
  `(setf ,v0 (mod64+ ,v0 ,v1)
         ,v2 (mod64+ ,v2 ,v3)
         ,v1 (rol64 ,v1 13)
         ,v3 (rol64 ,v3 16)
         ,v1 (logxor ,v1 ,v0)
         ,v3 (logxor ,v3 ,v2)
         ,v0 (rol64 ,v0 32)
         ,v2 (mod64+ ,v2 ,v1)
         ,v0 (mod64+ ,v0 ,v3)
         ,v1 (rol64 ,v1 17)
         ,v3 (rol64 ,v3 21)
         ,v1 (logxor ,v1 ,v2)
         ,v3 (logxor ,v3 ,v0)
         ,v2 (rol64 ,v2 32)))

(defun siphash-compress (state data start remaining data-length n-rounds)
  (declare (type (simple-array (unsigned-byte 64) (4)) state)
           (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start remaining data-length n-rounds)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((v0 (aref state 0))
        (v1 (aref state 1))
        (v2 (aref state 2))
        (v3 (aref state 3)))
    (declare (type (unsigned-byte 64) v0 v1 v2 v3))
    (do ((m 0))
        ((< remaining 8))
      (declare (type (unsigned-byte 64) m))
      (setf m (ub64ref/le data start))
      (setf v3 (logxor v3 m))
      (dotimes (i n-rounds)
        (siphash-round v0 v1 v2 v3))
      (setf v0 (logxor v0 m))
      (incf start 8)
      (incf data-length 8)
      (decf remaining 8))
    (setf (aref state 0) v0
          (aref state 1) v1
          (aref state 2) v2
          (aref state 3) v3)
    (values start remaining data-length)))

(defun siphash-finalize (state n-rounds tag)
  (declare (type (simple-array (unsigned-byte 64) (4)) state)
           (type (simple-array (unsigned-byte 8) (*)) tag)
           (type fixnum n-rounds)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((digest-length (length tag))
        (v0 (aref state 0))
        (v1 (aref state 1))
        (v2 (aref state 2))
        (v3 (aref state 3)))
    (declare (type fixnum digest-length)
             (type (unsigned-byte 64) v0 v1 v2 v3))
    (setf v2 (logxor v2 (if (= digest-length 16) #xee #xff)))
    (dotimes (i n-rounds)
      (siphash-round v0 v1 v2 v3))
    (setf (ub64ref/le tag 0) (logxor v0 v1 v2 v3))
    (when (= digest-length 16)
      (setf v1 (logxor v1 #xdd))
      (dotimes (i n-rounds)
        (siphash-round v0 v1 v2 v3))
      (setf (ub64ref/le tag 8) (logxor v0 v1 v2 v3)))
    (values)))

(defun update-siphash (mac data &key (start 0) (end (length data)))
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start end)
           (optimize (speed 3) (space 0) (safety 1) (debug 0)))
  (let ((buffer (siphash-buffer mac))
        (buffer-length (siphash-buffer-length mac))
        (state (siphash-state mac))
        (n-rounds (siphash-compression-rounds mac))
        (data-length (siphash-data-length mac))
        (remaining (- end start)))
    (declare (type (simple-array (unsigned-byte 8) (8)) buffer)
             (type (integer 0 8) buffer-length)
             (type (simple-array (unsigned-byte 64) (4)) state)
             (type fixnum n-rounds data-length remaining))

    ;; Fill the buffer with new data if necessary
    (when (plusp buffer-length)
      (let ((n (min remaining (- 8 buffer-length))))
        (declare (type (integer 0 8) n))
        (replace buffer data
                 :start1 buffer-length
                 :start2 start
                 :end2 (+ start n))
        (incf buffer-length n)
        (incf start n)
        (incf data-length n)
        (decf remaining n)))

    ;; Process the buffer
    (when (= buffer-length 8)
      (siphash-compress state buffer 0 8 data-length n-rounds)
      (setf buffer-length 0))

    ;; Process the data
    ;; TODO: (siphash-process-full-blocks ...)
    (multiple-value-setq (start remaining data-length)
      (siphash-compress state data start remaining data-length n-rounds))

    ;; Put the remaining data in the buffer
    (when (plusp remaining)
      (replace buffer data :start1 0 :start2 start :end2 end)
      (incf data-length remaining)
      (setf buffer-length remaining))

    ;; Save the state
    (setf (siphash-data-length mac) data-length)
    (setf (siphash-buffer-length mac) buffer-length)
    (values)))

(defun siphash-digest (mac)
  (let ((buffer (copy-seq (siphash-buffer mac)))
        (buffer-length (siphash-buffer-length mac))
        (state (copy-seq (siphash-state mac)))
        (compression-rounds (siphash-compression-rounds mac))
        (finalization-rounds (siphash-finalization-rounds mac))
        (digest-length (siphash-digest-length mac))
        (data-length (siphash-data-length mac)))
    (declare (type (simple-array (unsigned-byte 8) (8)) buffer)
             (type (integer 0 8) buffer-length)
             (type (simple-array (unsigned-byte 64) (4)) state)
             (type fixnum compression-rounds finalization-rounds digest-length data-length)
             (dynamic-extent buffer state))

    ;; Pad and process the buffer
    (fill buffer 0 :start buffer-length)
    (setf (aref buffer 7) (mod data-length 256))
    (siphash-compress state buffer 0 8 data-length compression-rounds)

    ;; Produce the tag
    (let ((tag (make-array digest-length :element-type '(unsigned-byte 8))))
      (siphash-finalize state finalization-rounds tag)
      tag)))

(defmac siphash
        make-siphash
        update-siphash
        siphash-digest)
