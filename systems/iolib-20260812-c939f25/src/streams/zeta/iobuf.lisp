;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- I/O buffers.
;;;

(in-package :iolib/zstreams)

(eval-when (:compile-toplevel)
  (declaim (optimize speed)))

;;;-------------------------------------------------------------------------
;;; Foreign Buffers
;;;-------------------------------------------------------------------------

(defconstant +default-iobuf-size+ (* 8 1024))

;; almost 128 MB: large enough for a stream buffer,
;; but small enough to fit into a fixnum
(deftype iobuf-index () '(unsigned-byte 27))

(deftype iobuf-data-vector () 'ub8-simple-vector)

(defstruct (iobuf (:constructor %make-iobuf (data)))
  (lock  (bt:make-lock "IObuf lock") :read-only t)
  (data  nil :type iobuf-data-vector :read-only t)
  (start   0 :type iobuf-index)
  (end     0 :type iobuf-index))

(defun make-iobuf-data-vector (size)
  (declare (type iobuf-index size))
  (make-array size :element-type 'ub8 :initial-element 0))

(defun make-iobuf (&optional (size +default-iobuf-size+))
  (check-type size iobuf-index)
  (%make-iobuf (make-iobuf-data-vector size)))

(defun iobuf-size (iobuf)
  (declare (type iobuf iobuf))
  (the iobuf-index (length (iobuf-data iobuf))))

(defun iobuf-available-octets (iobuf)
  (declare (type iobuf iobuf))
  (- (iobuf-end iobuf)
     (iobuf-start iobuf)))

(defun iobuf-available-space (iobuf)
  (declare (type iobuf iobuf))
  (- (iobuf-size iobuf)
     (iobuf-end iobuf)))

(defun iobuf-empty-p (iobuf)
  (declare (type iobuf iobuf))
  (zerop (iobuf-available-octets iobuf)))

(defun iobuf-full-p (iobuf)
  (declare (type iobuf iobuf))
  (zerop (iobuf-available-space iobuf)))

(defun iobuf-reset (iobuf)
  (declare (type iobuf iobuf))
  (setf (iobuf-start iobuf) 0
        (iobuf-end iobuf)   0))

(defun iobuf-next-data-zone (iobuf)
  (declare (type iobuf iobuf))
  (values (iobuf-data iobuf)
          (iobuf-start iobuf)
          (iobuf-end iobuf)))

(defun iobuf-next-empty-zone (iobuf)
  (declare (type iobuf iobuf))
  (values (iobuf-data iobuf)
          (iobuf-end iobuf)
          (iobuf-size iobuf)))


;;;-------------------------------------------------------------------------
;;; UNSAFE functions which *DO NOT* check boundaries
;;; that must be done by their callers
;;;-------------------------------------------------------------------------

(defun bref (iobuf index)
  (declare (type iobuf iobuf)
           (type iobuf-index index))
  (aref (iobuf-data iobuf) index))

(defun (setf bref) (octet iobuf index)
  (declare (type ub8 octet)
           (type iobuf iobuf)
           (type iobuf-index index))
  (setf (aref (iobuf-data iobuf) index) octet))

(defun iobuf-pop-octet (iobuf)
  (declare (type iobuf iobuf))
  (let ((start (iobuf-start iobuf)))
    (prog1 (bref iobuf start)
      (setf (iobuf-start iobuf) (1+ start)))))

(defun iobuf-push-octet (iobuf octet)
  (declare (type iobuf iobuf)
           (type ub8 octet))
  (let ((end (iobuf-end iobuf)))
    (prog1 (setf (bref iobuf end) octet)
      (setf (iobuf-end iobuf) (1+ end)))))

(defun replace-ub8sv->ub8sv (destination source start1 end1 start2 end2)
  (declare (type ub8-simple-vector destination source)
           (type iobuf-index start1 start2 end1 end2))
  (let ((nbytes (min (- end1 start1)
                     (- end2 start2))))
    (replace destination source
             :start1 start1 :end1 end1
             :start2 start2 :end2 end2)
    (values nbytes)))

(defun replace-ub8sv->ub8cv (destination source start1 end1 start2 end2)
  (declare (type ub8-simple-vector source)
           (type ub8-complex-vector destination)
           (type iobuf-index start1 start2 end1 end2))
  (let ((nbytes (min (- end1 start1)
                     (- end2 start2))))
    (replace destination source
             :start1 start1 :end1 end1
             :start2 start2 :end2 end2)
    (values nbytes)))

(defun replace-ub8cv->ub8sv (destination source start1 end1 start2 end2)
  (declare (type ub8-complex-vector source)
           (type ub8-simple-vector destination)
           (type iobuf-index start1 start2 end1 end2))
  (let ((nbytes (min (- end1 start1)
                     (- end2 start2))))
    (replace destination source
             :start1 start1 :end1 end1
             :start2 start2 :end2 end2)
    (values nbytes)))

(defun iobuf->vector (iobuf vector start end)
  (declare (type iobuf iobuf)
           (type ub8-vector vector)
           (type iobuf-index start end))
  (when (iobuf-empty-p iobuf)
    (iobuf-reset iobuf))
  (multiple-value-bind (iobuf-data data-start data-end)
      (iobuf-next-data-zone iobuf)
    (declare (type iobuf-index data-start data-end))
    (let ((nbytes
           (etypecase vector
             (ub8-simple-vector
              (replace-ub8sv->ub8sv vector iobuf-data
                                    start end
                                    data-start data-end))
             (ub8-complex-vector
              (replace-ub8sv->ub8cv vector iobuf-data
                                    start end
                                    data-start data-end)))))
      (setf (iobuf-start iobuf) (+ data-start (the iobuf-index nbytes)))
      (values nbytes))))

(defun vector->iobuf (iobuf vector start end)
  (declare (type iobuf iobuf)
           (type ub8-vector vector)
           (type iobuf-index start end))
  (when (iobuf-empty-p iobuf)
    (iobuf-reset iobuf))
  (multiple-value-bind (iobuf-data data-start data-end)
      (iobuf-next-empty-zone iobuf)
    (declare (type iobuf-index data-start data-end))
    (let ((nbytes
           (etypecase vector
             (ub8-simple-vector
              (replace-ub8sv->ub8sv iobuf-data vector
                                    data-start data-end
                                    start end))
             (ub8-complex-vector
              (replace-ub8cv->ub8sv iobuf-data vector
                                    data-start data-end
                                    start end)))))
      (setf (iobuf-end iobuf) (+ data-start (the iobuf-index nbytes)))
      (values nbytes))))
