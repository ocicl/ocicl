;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Miscellaneous utilities.
;;;

(in-package :iolib/multiplex)

(defun timeout->timeval (timeout tv)
  (with-foreign-slots ((isys:sec isys:usec)
                       tv (:struct isys:timeval))
    (multiple-value-bind (%sec %usec)
        (decode-timeout timeout)
     (setf isys:sec  %sec
           isys:usec %usec))))

(defun timeout->timespec (timeout ts)
  (with-foreign-slots ((isys:sec isys:nsec)
                       ts (:struct isys:timespec))
    (multiple-value-bind (%sec %usec)
        (decode-timeout timeout)
      (setf isys:sec  %sec
            isys:nsec (* 1000 %usec)))))

(defun timeout->milliseconds (timeout)
  (if timeout
      (multiple-value-bind (sec usec)
          (decode-timeout timeout)
        (+ (* sec 1000)
           (truncate usec 1000)))
      -1))
