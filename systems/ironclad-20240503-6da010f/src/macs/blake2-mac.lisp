;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; blake2-mac.lisp -- implementation of the Blake2b MAC

(in-package :crypto)


(defclass blake2-mac (mac)
  ((digest :accessor blake2-digest :initarg :digest)
   (digest-length :accessor digest-length :initarg :digest-length)))

(defun make-blake2-mac (key &key (digest-length 64))
  (make-instance 'blake2-mac
                 :key key
                 :digest-length digest-length))

(defmethod copy-blake2-mac ((mac blake2-mac) &optional copy)
  (declare (type (or null blake2-mac) copy))
  (let ((copy (if copy
                  copy
                  (make-instance 'blake2-mac
                                 :key (make-array 1 :element-type '(unsigned-byte 8))
                                 :digest-length 64))))
    (declare (type blake2-mac copy))
    (setf (blake2-digest copy) (copy-digest (blake2-digest mac)))
    (setf (digest-length copy) (digest-length mac))
    copy))

(defmethod shared-initialize :after ((mac blake2-mac) slot-names
                                     &rest initargs
                                     &key key &allow-other-keys)
  (declare (ignore slot-names initargs)
           (type (simple-array (unsigned-byte 8) (*)) key))
  (let ((digest-length (digest-length mac))
        (digest (make-digest :blake2)))
    (setf (blake2-state digest) (blake2-make-initial-state digest-length (length key)))
    (when (plusp (length key))
      (let ((first-block (make-array +blake2-block-size+
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0)))
        ;; Process the key block
        (replace first-block key)
        (blake2-update digest first-block 0 +blake2-block-size+ nil)))
    (setf (blake2-digest mac) digest)))

(defun update-blake2-mac (mac sequence &key (start 0) end)
  (blake2-update (blake2-digest mac) sequence start (or end (length sequence)) nil)
  mac)

(defun blake2-mac-digest (mac)
  (let ((digest (make-array (digest-length mac)
                            :element-type '(unsigned-byte 8)))
        (mac-copy (copy-blake2-mac mac)))
    (blake2-finalize (blake2-digest mac-copy) digest 0)
    digest))

(defmac blake2-mac
        make-blake2-mac
        update-blake2-mac
        blake2-mac-digest)
