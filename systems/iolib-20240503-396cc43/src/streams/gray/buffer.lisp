;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Foreign memory buffers.
;;;

(in-package :iolib/streams)

;;;; Foreign Buffers

(defconstant +bytes-per-iobuf+ (* 4 1024))

;;; FIXME: make this right
;;; probably not all SIMPLE-ARRAYs are admissible
;;; on all implementations
(deftype compatible-lisp-array ()
  '(simple-array * (*)))

(defun allocate-iobuf (&optional (size +bytes-per-iobuf+))
  (let ((b (%make-iobuf)))
    (setf (iobuf-data b) (foreign-alloc :uint8 :count size)
          (iobuf-size b) size)
    (values b)))

(defun free-iobuf (iobuf)
  (unless (null-pointer-p (iobuf-data iobuf))
    (foreign-free (iobuf-data iobuf)))
  (setf (iobuf-data iobuf) (null-pointer))
  (values iobuf))

(defun iobuf-length (iobuf)
  (- (iobuf-end iobuf)
     (iobuf-start iobuf)))

(defun iobuf-start-pointer (iobuf)
  (inc-pointer (iobuf-data iobuf)
               (iobuf-start iobuf)))

(defun iobuf-end-pointer (iobuf)
  (inc-pointer (iobuf-data iobuf)
               (iobuf-end iobuf)))

(defun iobuf-empty-p (iobuf)
  (= (iobuf-end iobuf)
     (iobuf-start iobuf)))

(defun iobuf-full-p (iobuf)
  (= (iobuf-end iobuf)
     (iobuf-size iobuf)))

(defun iobuf-end-space-length (iobuf)
  (- (iobuf-size iobuf)
     (iobuf-end iobuf)))

(defun iobuf-reset (iobuf)
  (setf (iobuf-start iobuf) 0
        (iobuf-end iobuf)   0))

(defun iobuf-peek (iobuf &optional (offset 0))
  (bref iobuf (+ (iobuf-start iobuf) offset)))

(defun iobuf-copy-data-to-start (iobuf)
  (declare (type iobuf iobuf))
  (isys:memmove
   (iobuf-data iobuf)
   (inc-pointer (iobuf-data iobuf)
                (iobuf-start iobuf))
   (iobuf-length iobuf))
  (setf (iobuf-end iobuf) (iobuf-length iobuf))
  (setf (iobuf-start iobuf) 0))

(defun iobuf-can-fit-slice-p (iobuf start end)
  (<= (- end start) (iobuf-end-space-length iobuf)))

(defun iobuf-append-slice (iobuf array start end)
  (let ((slice-length (- end start)))
    (iobuf-copy-from-lisp-array array start iobuf
                                (iobuf-end iobuf) slice-length)
    (incf (iobuf-end iobuf) slice-length)))

;;; BREF, (SETF BREF) and BUFFER-COPY *DO NOT* check boundaries
;;; that must be done by their callers
(defun bref (iobuf index)
  (declare (type iobuf iobuf)
           (type buffer-index index))
  (debug-only (assert (not (minusp index))))
  (mem-aref (iobuf-data iobuf) :uint8 index))

(defun (setf bref) (octet iobuf index)
  (declare (type (unsigned-byte 8) octet)
           (type iobuf iobuf)
           (type buffer-index index))
  (debug-only
    (assert (>= index 0))
    (assert (< index (iobuf-size iobuf))))
  (setf (mem-aref (iobuf-data iobuf) :uint8 index) octet))

(defun iobuf-copy-from-lisp-array (src soff dst doff length)
  (declare (type compatible-lisp-array src)
           (type iobuf dst)
           (type buffer-index soff doff length))
  (debug-only
    (assert (>= doff 0))
    (assert (>= soff 0))
    (assert (<= (+ doff length) (iobuf-size dst))))
  (let ((dst-ptr (iobuf-data dst)))
    (with-pointer-to-vector-data (src-ptr src)
      (isys:memcpy
       (inc-pointer dst-ptr doff)
       (inc-pointer src-ptr soff)
       length))))

(defun iobuf-copy-into-lisp-array (src soff dst doff length)
  (declare (type iobuf src)
           (type compatible-lisp-array dst)
           (type buffer-index soff doff length))
  (debug-only
    (assert (>= doff 0))
    (assert (>= soff 0))
    (assert (<= (+ doff length) (length dst))))
  (let ((src-ptr (iobuf-data src)))
    (with-pointer-to-vector-data (dst-ptr dst)
      (isys:memcpy
       (inc-pointer dst-ptr doff)
       (inc-pointer src-ptr soff)
       length))))

(defun iobuf-pop-octet (iobuf)
  (declare (type iobuf iobuf))
  (debug-only (assert (> (iobuf-length iobuf) 0)))
  (let ((start (iobuf-start iobuf)))
    (prog1 (bref iobuf start)
      (incf (iobuf-start iobuf)))))

(defun iobuf-push-octet (iobuf octet)
  (declare (type iobuf iobuf)
           (type (unsigned-byte 8) octet))
  (debug-only (assert (not (iobuf-full-p iobuf))))
  (let ((end (iobuf-end iobuf)))
    (prog1 (setf (bref iobuf end) octet)
      (incf (iobuf-end iobuf)))))
