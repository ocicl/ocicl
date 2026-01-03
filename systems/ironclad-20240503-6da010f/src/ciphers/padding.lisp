;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; padding.lisp -- implementation of various padding algorithms

(in-package :crypto)

(defclass padding () ())

;;; PKCS7 padding

(defclass pkcs7-padding (padding) ())

(defmethod add-padding-bytes ((padding pkcs7-padding) text start block-offset block-size)
  (declare (type simple-octet-vector text))
  (declare (type index start block-offset))
  (let ((n-padding-bytes (- block-size block-offset)))
    (declare (type (unsigned-byte 8) n-padding-bytes))
    (when (plusp n-padding-bytes)
      (fill text n-padding-bytes :start (+ start block-offset) :end (+ start block-size)))
    (values)))

(defmethod count-padding-bytes ((padding pkcs7-padding) text start block-size)
  (declare (type simple-octet-vector text))
  (declare (type index start))
  (let* ((end (+ start block-size))
         (n-padding-bytes (aref text (1- end)))
         (offset (- end n-padding-bytes)))
    (declare (type index end offset))
    (declare (type (unsigned-byte 8) n-padding-bytes))
    (when (or (> n-padding-bytes block-size)
              (not (loop for i from offset below end
                         always (= (aref text i) n-padding-bytes))))
      (error 'invalid-padding :name 'pkcs7 :block text))
    n-padding-bytes))


;;; ANSI X.923 padding

(defclass ansi-x923-padding (padding) ())

(defmethod add-padding-bytes ((padding ansi-x923-padding) text start block-offset block-size)
  (declare (type simple-octet-vector text))
  (declare (type index start block-offset))
  (let ((end (+ start block-size))
        (n-padding-bytes (- block-size block-offset)))
    (declare (type index end))
    (declare (type (unsigned-byte 8) n-padding-bytes))
    (when (plusp n-padding-bytes)
      (fill text 0 :start (+ start block-offset) :end end)
      (setf (aref text (1- end)) n-padding-bytes))
    (values)))

(defmethod count-padding-bytes ((padding ansi-x923-padding) text start block-size)
  (declare (type simple-octet-vector text))
  (declare (type index start))
  (let* ((end (+ start block-size))
         (n-padding-bytes (aref text (1- end)))
         (offset (- end n-padding-bytes)))
    (declare (type index end))
    (declare (type (unsigned-byte 8) n-padding-bytes))
    (when (or (> n-padding-bytes block-size)
              (not (loop for i from offset below (1- end)
                         always (zerop (aref text i)))))
      (error 'invalid-padding :name 'ansi-x923 :block text))
    n-padding-bytes))


;;; ISO 7816-4 padding

(defclass iso-7816-4-padding (padding) ())

(defmethod add-padding-bytes ((padding iso-7816-4-padding) text start block-offset block-size)
  (declare (type simple-octet-vector text))
  (declare (type index start block-offset))
  (let ((end (+ start block-size))
        (offset (+ start block-offset)))
    (declare (type index end offset))
    (when (< block-offset block-size)
      (setf (aref text offset) #x80)
      (fill text 0 :start (1+ offset) :end end))
    (values)))

(defmethod count-padding-bytes ((padding iso-7816-4-padding) text start block-size)
  (declare (type simple-octet-vector text))
  (declare (type index start))
  (let* ((end (+ start block-size))
         (offset (position #x80 text :start start :end end :from-end t)))
    (declare (type index end))
    (when (or (null offset)
              (not (loop for i from (1+ offset) below end
                         always (zerop (aref text i)))))
      (error 'invalid-padding :name 'iso-7816-4 :block text))
    (- end offset)))
