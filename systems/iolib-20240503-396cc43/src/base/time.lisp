;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Time utils.
;;;

(in-package :iolib/base)

(deftype timeout ()
  'double-float)

(deftype timeout-designator ()
  '(or non-negative-real (member t nil)))

(deftype positive-timeout-designator ()
  '(or non-negative-real (eql t)))

;;; Break a real timeout into seconds and microseconds.
(defun decode-timeout (timeout)
  (assert (or (not timeout)
              (and (typep timeout 'real)
                   (not (minusp timeout))))
          (timeout)
          "The timeout must be a non-negative real or NIL: ~S" timeout)
  (typecase timeout
    (null nil)
    (integer (values timeout 0))
    (real
     (multiple-value-bind (q r) (truncate (coerce timeout 'timeout))
       (declare (type unsigned-byte q)
                (type timeout r))
       (values q (the (values unsigned-byte t) (truncate (* r 1d6))))))))

(defun normalize-timeout (timeout)
  (assert (and (typep timeout 'real)
               (not (minusp timeout)))
          (timeout)
          "The timeout must be non-negative: ~A" timeout)
  (coerce timeout 'timeout))

(defun clamp-timeout (timeout &optional (min 0) (max most-positive-fixnum))
  (clamp (or timeout most-positive-fixnum)
         (if min (max min 0) 0) (or max most-positive-fixnum)))
