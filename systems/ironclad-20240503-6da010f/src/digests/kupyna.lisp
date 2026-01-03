;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; kupyna.lisp -- implementation of the Kupyna hash functions (DSTU 7564:2014)

(in-package :crypto)


;;;
;;; Constants
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconst +kupyna-t+ +kalyna-t+))

(declaim (type (simple-array (unsigned-byte 64) (8 256)) +kupyna-t+))


;;;
;;; Rounds for 256-bit output
;;;

(declaim (inline kupyna-g256))
(defun kupyna-g256 (x y)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c)
               `(aref +kupyna-t+ ,a (logand (mod64ash (aref x ,b) ,c) #xff))))
    (setf (aref y 0)
          (logxor (m 0 0 0) (m 1 7 -8) (m 2 6 -16) (m 3 5 -24)
                  (m 4 4 -32) (m 5 3 -40) (m 6 2 -48) (m 7 1 -56)))
    (setf (aref y 1)
          (logxor (m 0 1 0) (m 1 0 -8) (m 2 7 -16) (m 3 6 -24)
                  (m 4 5 -32) (m 5 4 -40) (m 6 3 -48) (m 7 2 -56)))
    (setf (aref y 2)
          (logxor (m 0 2 0) (m 1 1 -8) (m 2 0 -16) (m 3 7 -24)
                  (m 4 6 -32) (m 5 5 -40) (m 6 4 -48) (m 7 3 -56)))
    (setf (aref y 3)
          (logxor (m 0 3 0) (m 1 2 -8) (m 2 1 -16) (m 3 0 -24)
                  (m 4 7 -32) (m 5 6 -40) (m 6 5 -48) (m 7 4 -56)))
    (setf (aref y 4)
          (logxor (m 0 4 0) (m 1 3 -8) (m 2 2 -16) (m 3 1 -24)
                  (m 4 0 -32) (m 5 7 -40) (m 6 6 -48) (m 7 5 -56)))
    (setf (aref y 5)
          (logxor (m 0 5 0) (m 1 4 -8) (m 2 3 -16) (m 3 2 -24)
                  (m 4 1 -32) (m 5 0 -40) (m 6 7 -48) (m 7 6 -56)))
    (setf (aref y 6)
          (logxor (m 0 6 0) (m 1 5 -8) (m 2 4 -16) (m 3 3 -24)
                  (m 4 2 -32) (m 5 1 -40) (m 6 0 -48) (m 7 7 -56)))
    (setf (aref y 7)
          (logxor (m 0 7 0) (m 1 6 -8) (m 2 5 -16) (m 3 4 -24)
                  (m 4 3 -32) (m 5 2 -40) (m 6 1 -48) (m 7 0 -56))))
  (values))

(declaim (inline kupyna-round-p256))
(defun kupyna-round-p256 (x y n)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y)
           (type (unsigned-byte 64) n)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (dotimes-unrolled (i 8)
    (setf (aref x i) (logxor (aref x i) (mod64ash i 4) n)))
  (kupyna-g256 x y)
  (values))

(declaim (inline kupyna-round-q256))
(defun kupyna-round-q256 (x y n)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y)
           (type (unsigned-byte 64) n)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (dotimes-unrolled (i 8)
    (setf (aref x i) (mod64+ (aref x i)
                             (logxor #x00F0F0F0F0F0F0F3
                                     (mod64ash (logxor (mod64* (- 7 i) #x10)
                                                       (logand n #xff))
                                               56)))))
  (kupyna-g256 x y)
  (values))

(defun kupyna-output-transform256 (h)
  (declare (type (simple-array (unsigned-byte 64) (*)) h)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (let ((t1 (make-array 8 :element-type '(unsigned-byte 64)))
        (t2 (make-array 8 :element-type '(unsigned-byte 64))))
    (declare (type (simple-array (unsigned-byte 64) (8)) t1 t2)
             (dynamic-extent t1 t2))
    (replace t1 h)
    (loop for r from 0 below 10 by 2 do
      (kupyna-round-p256 t1 t2 r)
      (kupyna-round-p256 t2 t1 (1+ r)))
    (dotimes (i 8)
      (setf (aref h i) (logxor (aref h i) (aref t1 i)))))
  (values))

(defun kupyna-transform256 (h m start)
  (declare (type (simple-array (unsigned-byte 64) (16)) h)
           (type (simple-array (unsigned-byte 8) (*)) m)
           (type index start)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (let ((ap1 (make-array 8 :element-type '(unsigned-byte 64)))
        (aq1 (make-array 8 :element-type '(unsigned-byte 64)))
        (ap2 (make-array 8 :element-type '(unsigned-byte 64)))
        (aq2 (make-array 8 :element-type '(unsigned-byte 64))))
    (declare (type (simple-array (unsigned-byte 64) (8)) ap1 aq1 ap2 aq2)
             (dynamic-extent ap1 aq1 ap2 aq2))
    (dotimes (i 8)
      (setf (aref aq1 i) (ub64ref/le m (+ start (* 8 i)))
            (aref ap1 i) (logxor (aref h i) (aref aq1 i))))
    (loop for r from 0 below 10 by 2 do
      (kupyna-round-p256 ap1 ap2 r)
      (kupyna-round-p256 ap2 ap1 (1+ r))
      (kupyna-round-q256 aq1 aq2 r)
      (kupyna-round-q256 aq2 aq1 (1+ r)))
    (dotimes (i 8)
      (setf (aref h i) (logxor (aref h i) (aref ap1 i) (aref aq1 i)))))
  (values))


;;;
;;; Rounds for 512-bit output
;;;

(declaim (inline kupyna-g512))
(defun kupyna-g512 (x y)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c)
               `(aref +kupyna-t+ ,a (logand (mod64ash (aref x ,b) ,c) #xff))))
    (setf (aref y 0)
          (logxor (m 0 0 0) (m 1 15 -8) (m 2 14 -16) (m 3 13 -24)
                  (m 4 12 -32) (m 5 11 -40) (m 6 10 -48) (m 7 5 -56)))
    (setf (aref y 1)
          (logxor (m 0 1 0) (m 1 0 -8) (m 2 15 -16) (m 3 14 -24)
                  (m 4 13 -32) (m 5 12 -40) (m 6 11 -48) (m 7 6 -56)))
    (setf (aref y 2)
          (logxor (m 0 2 0) (m 1 1 -8) (m 2 0 -16) (m 3 15 -24)
                  (m 4 14 -32) (m 5 13 -40) (m 6 12 -48) (m 7 7 -56)))
    (setf (aref y 3)
          (logxor (m 0 3 0) (m 1 2 -8) (m 2 1 -16) (m 3 0 -24)
                  (m 4 15 -32) (m 5 14 -40) (m 6 13 -48) (m 7 8 -56)))
    (setf (aref y 4)
          (logxor (m 0 4 0) (m 1 3 -8) (m 2 2 -16) (m 3 1 -24)
                  (m 4 0 -32) (m 5 15 -40) (m 6 14 -48) (m 7 9 -56)))
    (setf (aref y 5)
          (logxor (m 0 5 0) (m 1 4 -8) (m 2 3 -16) (m 3 2 -24)
                  (m 4 1 -32) (m 5 0 -40) (m 6 15 -48) (m 7 10 -56)))
    (setf (aref y 6)
          (logxor (m 0 6 0) (m 1 5 -8) (m 2 4 -16) (m 3 3 -24)
                  (m 4 2 -32) (m 5 1 -40) (m 6 0 -48) (m 7 11 -56)))
    (setf (aref y 7)
          (logxor (m 0 7 0) (m 1 6 -8) (m 2 5 -16) (m 3 4 -24)
                  (m 4 3 -32) (m 5 2 -40) (m 6 1 -48) (m 7 12 -56)))
    (setf (aref y 8)
          (logxor (m 0 8 0) (m 1 7 -8) (m 2 6 -16) (m 3 5 -24)
                  (m 4 4 -32) (m 5 3 -40) (m 6 2 -48) (m 7 13 -56)))
    (setf (aref y 9)
          (logxor (m 0 9 0) (m 1 8 -8) (m 2 7 -16) (m 3 6 -24)
                  (m 4 5 -32) (m 5 4 -40) (m 6 3 -48) (m 7 14 -56)))
    (setf (aref y 10)
          (logxor (m 0 10 0) (m 1 9 -8) (m 2 8 -16) (m 3 7 -24)
                  (m 4 6 -32) (m 5 5 -40) (m 6 4 -48) (m 7 15 -56)))
    (setf (aref y 11)
          (logxor (m 0 11 0) (m 1 10 -8) (m 2 9 -16) (m 3 8 -24)
                  (m 4 7 -32) (m 5 6 -40) (m 6 5 -48) (m 7 0 -56)))
    (setf (aref y 12)
          (logxor (m 0 12 0) (m 1 11 -8) (m 2 10 -16) (m 3 9 -24)
                  (m 4 8 -32) (m 5 7 -40) (m 6 6 -48) (m 7 1 -56)))
    (setf (aref y 13)
          (logxor (m 0 13 0) (m 1 12 -8) (m 2 11 -16) (m 3 10 -24)
                  (m 4 9 -32) (m 5 8 -40) (m 6 7 -48) (m 7 2 -56)))
    (setf (aref y 14)
          (logxor (m 0 14 0) (m 1 13 -8) (m 2 12 -16) (m 3 11 -24)
                  (m 4 10 -32) (m 5 9 -40) (m 6 8 -48) (m 7 3 -56)))
    (setf (aref y 15)
          (logxor (m 0 15 0) (m 1 14 -8) (m 2 13 -16) (m 3 12 -24)
                  (m 4 11 -32) (m 5 10 -40) (m 6 9 -48) (m 7 4 -56))))
  (values))

(declaim (inline kupyna-round-p512))
(defun kupyna-round-p512 (x y n)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y)
           (type (unsigned-byte 64) n)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (dotimes-unrolled (i 16)
    (setf (aref x i) (logxor (aref x i) (mod64ash i 4) n)))
  (kupyna-g512 x y)
  (values))

(declaim (inline kupyna-round-q512))
(defun kupyna-round-q512 (x y n)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y)
           (type (unsigned-byte 64) n)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (dotimes-unrolled (i 16)
    (setf (aref x i) (mod64+ (aref x i)
                             (logxor #x00F0F0F0F0F0F0F3
                                     (mod64ash (logxor (mod64* (- 15 i) #x10)
                                                       (logand n #xff))
                                               56)))))
  (kupyna-g512 x y)
  (values))

(defun kupyna-output-transform512 (h)
  (declare (type (simple-array (unsigned-byte 64) (*)) h)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (let ((t1 (make-array 16 :element-type '(unsigned-byte 64)))
        (t2 (make-array 16 :element-type '(unsigned-byte 64))))
    (declare (type (simple-array (unsigned-byte 64) (16)) t1 t2)
             (dynamic-extent t1 t2))
    (replace t1 h)
    (loop for r from 0 below 14 by 2 do
      (kupyna-round-p512 t1 t2 r)
      (kupyna-round-p512 t2 t1 (1+ r)))
    (dotimes (i 16)
      (setf (aref h i) (logxor (aref h i) (aref t1 i)))))
  (values))

(defun kupyna-transform512 (h m start)
  (declare (type (simple-array (unsigned-byte 64) (16)) h)
           (type (simple-array (unsigned-byte 8) (*)) m)
           (type index start)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (let ((ap1 (make-array 16 :element-type '(unsigned-byte 64)))
        (aq1 (make-array 16 :element-type '(unsigned-byte 64)))
        (ap2 (make-array 16 :element-type '(unsigned-byte 64)))
        (aq2 (make-array 16 :element-type '(unsigned-byte 64))))
    (declare (type (simple-array (unsigned-byte 64) (16)) ap1 aq1 ap2 aq2)
             (dynamic-extent ap1 aq1 ap2 aq2))
    (dotimes (i 16)
      (setf (aref aq1 i) (ub64ref/le m (+ start (* 8 i)))
            (aref ap1 i) (logxor (aref h i) (aref aq1 i))))
    (loop for r from 0 below 14 by 2 do
      (kupyna-round-p512 ap1 ap2 r)
      (kupyna-round-p512 ap2 ap1 (1+ r))
      (kupyna-round-q512 aq1 aq2 r)
      (kupyna-round-q512 aq2 aq1 (1+ r)))
    (dotimes (i 16)
      (setf (aref h i) (logxor (aref h i) (aref ap1 i) (aref aq1 i)))))
  (values))


;;;
;;; Digest structures and functions
;;;

(defstruct (kupyna
            (:constructor %make-kupyna-digest nil)
            (:copier nil))
  (buffer (make-array 128 :element-type '(unsigned-byte 8))
          :type (simple-array (unsigned-byte 8) (128)))
  (buffer-index 0 :type (integer 0 128))
  (h (make-array 16 :element-type '(unsigned-byte 64)
                    :initial-contents '(128 0 0 0 0 0 0 0
                                        0 0 0 0 0 0 0 0))
     :type (simple-array (unsigned-byte 64) (16)))
  (total 0 :type (unsigned-byte 64)))

(defstruct (kupyna/256
            (:include kupyna)
            (:constructor %make-kupyna/256-digest
              (&aux (h (make-array 16 :element-type '(unsigned-byte 64)
                                      :initial-contents '(64 0 0 0 0 0 0 0
                                                          0 0 0 0 0 0 0 0)))))
            (:copier nil)))

(defmethod reinitialize-instance ((state kupyna) &rest initargs)
  (declare (ignore initargs))
  (setf (kupyna-buffer-index state) 0)
  (setf (aref (kupyna-h state) 0) (etypecase state
                                    (kupyna/256 64)
                                    (kupyna 128)))
  (fill (kupyna-h state) 0 :start 1)
  (setf (kupyna-total state) 0)
  state)

(defmethod copy-digest ((state kupyna) &optional copy)
  (check-type copy (or null kupyna))
  (let ((copy (if copy
                  copy
                  (etypecase state
                    (kupyna/256 (%make-kupyna/256-digest))
                    (kupyna (%make-kupyna-digest))))))
    (declare (type kupyna copy))
    (replace (kupyna-buffer copy) (kupyna-buffer state))
    (setf (kupyna-buffer-index copy) (kupyna-buffer-index state))
    (replace (kupyna-h copy) (kupyna-h state))
    (setf (kupyna-total copy) (kupyna-total state))
    copy))

(define-digest-updater kupyna
  (let ((block-length (block-length state))
        (buffer (kupyna-buffer state))
        (buffer-index (kupyna-buffer-index state))
        (h (kupyna-h state))
        (total (kupyna-total state))
        (length (- end start))
        (n 0))
    (declare (type (simple-array (unsigned-byte 8) (128)) buffer)
             (type (integer 0 128) block-length buffer-index n)
             (type (simple-array (unsigned-byte 64) (16)) h)
             (type (unsigned-byte 64) total)
             (type index length))
    (when (plusp buffer-index)
      (setf n (min length (- block-length buffer-index)))
      (replace buffer sequence
               :start1 buffer-index
               :start2 start
               :end2 (+ start n))
      (incf buffer-index n)
      (incf start n)
      (decf length n)
      (when (= buffer-index block-length)
        (ecase block-length
          (64 (kupyna-transform256 h buffer 0))
          (128 (kupyna-transform512 h buffer 0)))
        (incf total (* 8 block-length))
        (setf buffer-index 0)))

    (ecase block-length
      (64
       (loop until (< length 64) do
         (kupyna-transform256 h sequence start)
         (incf total 512)
         (incf start 64)
         (decf length 64)))
      (128
       (loop until (< length 128) do
         (kupyna-transform512 h sequence start)
         (incf total 1024)
         (incf start 128)
         (decf length 128))))

    (when (plusp length)
      (replace buffer sequence :start2 start :end2 end)
      (setf buffer-index length))

    (setf (kupyna-buffer-index state) buffer-index)
    (setf (kupyna-total state) total)
    (values)))

(define-digest-finalizer ((kupyna 64)
                          (kupyna/256 32))
  (let ((block-length (block-length state))
        (digest-length (digest-length state))
        (buffer (kupyna-buffer state))
        (buffer-index (kupyna-buffer-index state))
        (h (kupyna-h state))
        (total (kupyna-total state)))
    (incf total (* 8 buffer-index))
    (setf (aref buffer buffer-index) #x80)
    (incf buffer-index)
    (when (> (+ buffer-index 12) block-length)
      (fill buffer 0 :start buffer-index)
      (ecase block-length
        (64 (kupyna-transform256 h buffer 0))
        (128 (kupyna-transform512 h buffer 0)))
      (setf buffer-index 0))
    (fill buffer 0 :start buffer-index)
    (setf (ub64ref/le buffer (- block-length 12)) total)
    (ecase block-length
      (64
       (kupyna-transform256 h buffer 0)
       (kupyna-output-transform256 h))
      (128
       (kupyna-transform512 h buffer 0)
       (kupyna-output-transform512 h)))

    (let ((output (make-array 128 :element-type '(unsigned-byte 8))))
      (dotimes (i 16)
        (setf (ub64ref/le output (* 8 i)) (aref h i)))
      (replace digest output
               :start1 digest-start
               :start2 (- block-length digest-length)
               :end2 block-length)
      digest)))

(defdigest kupyna :digest-length 64 :block-length 128)
(defdigest kupyna/256 :digest-length 32 :block-length 64)
