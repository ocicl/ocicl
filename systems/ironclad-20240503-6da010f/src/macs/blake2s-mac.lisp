;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; blake2s-mac.lisp -- implementation of the Blake2s MAC

(in-package :crypto)


(defclass blake2s-mac (mac)
  ((digest :accessor blake2s-digest :initarg :digest)
   (digest-length :accessor digest-length :initarg :digest-length)))

(defun make-blake2s-mac (key &key (digest-length 32))
  (make-instance 'blake2s-mac
                 :key key
                 :digest-length digest-length))

(defmethod copy-blake2s-mac ((mac blake2s-mac) &optional copy)
  (declare (type (or null blake2s-mac) copy))
  (let ((copy (if copy
                  copy
                  (make-instance 'blake2s-mac
                                 :key (make-array 1 :element-type '(unsigned-byte 8))
                                 :digest-length 32))))
    (declare (type blake2s-mac copy))
    (setf (blake2s-digest copy) (copy-digest (blake2s-digest mac)))
    (setf (digest-length copy) (digest-length mac))
    copy))

(defmethod shared-initialize :after ((mac blake2s-mac) slot-names
                                     &rest initargs
                                     &key key &allow-other-keys)
  (declare (ignore slot-names initargs)
           (type (simple-array (unsigned-byte 8) (*)) key))
  (let ((digest-length (digest-length mac))
        (digest (make-digest :blake2s)))
    (setf (blake2s-state digest) (blake2s-make-initial-state digest-length (length key)))
    (when (plusp (length key))
      (let ((first-block (make-array +blake2s-block-size+
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0)))
        ;; Process the key block
        (replace first-block key)
        (blake2s-update digest first-block 0 +blake2s-block-size+ nil)))
    (setf (blake2s-digest mac) digest)))

(defun update-blake2s-mac (mac sequence &key (start 0) end)
  (blake2s-update (blake2s-digest mac) sequence start (or end (length sequence)) nil)
  mac)

(defun blake2s-mac-digest (mac)
  (let ((digest (make-array (digest-length mac)
                            :element-type '(unsigned-byte 8)))
        (mac-copy (copy-blake2s-mac mac)))
    (blake2s-finalize (blake2s-digest mac-copy) digest 0)
    digest))

(defmac blake2s-mac
        make-blake2s-mac
        update-blake2s-mac
        blake2s-mac-digest)
