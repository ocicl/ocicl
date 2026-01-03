;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; skein-mac.lisp -- implementation of the Skein MAC

(in-package :crypto)


(defclass skein-mac (mac)
  ((value :accessor skein-value :initarg :value)
   (tweak :accessor skein-tweak :initarg :tweak)
   (cfg :accessor skein-cfg :initarg :cfg)
   (buffer :accessor skein-buffer :initarg :buffer)
   (buffer-length :accessor skein-buffer-length :initarg :buffer-length)
   (cipher :accessor skein-cipher :initarg :cipher)
   (block-length :accessor block-length :initarg :block-length)
   (digest-length :accessor digest-length :initarg :digest-length)))

(defun make-skein-mac (key &key (block-length 64) (digest-length 64))
  (unless (or (= block-length 32)
              (= block-length 64)
              (= block-length 128))
    (error 'invalid-mac-parameter
           :mac-name 'skein-mac
           :message "The block length must be 32, 64 or 128 bytes"))

  (make-instance 'skein-mac
                 :key key
                 :block-length block-length
                 :digest-length digest-length))

(defmethod copy-skein-mac ((mac skein-mac) &optional copy)
  (declare (type (or null skein-mac) copy))
  (let ((copy (if copy
                  copy
                  (make-instance 'skein-mac
                                 :key (skein-value mac)
                                 :block-length (block-length mac)
                                 :digest-length (digest-length mac)))))
    (declare (type skein-mac copy))
    (replace (skein-value copy) (skein-value mac))
    (replace (skein-tweak copy) (skein-tweak mac))
    (replace (skein-cfg copy) (skein-cfg mac))
    (replace (skein-buffer copy) (skein-buffer mac))
    (setf (skein-buffer-length copy) (skein-buffer-length mac))
    (setf (skein-cipher copy) (skein-copy-cipher (skein-cipher mac)))
    copy))

(defmethod shared-initialize :after ((mac skein-mac) slot-names
                                     &rest initargs
                                     &key key &allow-other-keys)
  (declare (ignore slot-names initargs)
           (type (simple-array (unsigned-byte 8) (*)) key))
  (let* ((block-length (block-length mac))
         (digest-length (digest-length mac))
         (value (make-array block-length
                            :element-type '(unsigned-byte 8)
                            :initial-element 0))
         (tweak (skein-make-tweak t nil +skein-key+ 0))
         (cfg (skein-make-configuration-string (* 8 digest-length)))
         (cipher (ecase block-length
                   (32 (make-cipher :threefish256
                                    :key value
                                    :mode :ecb))
                   (64 (make-cipher :threefish512
                                    :key value
                                    :mode :ecb))
                   (128 (make-cipher :threefish1024
                                     :key value
                                     :mode :ecb)))))
    (setf (skein-cipher mac) cipher
          (skein-value mac) value
          (skein-cfg mac) cfg
          (skein-tweak mac) tweak
          (skein-buffer mac) (make-array block-length
                                         :element-type '(unsigned-byte 8))
          (skein-buffer-length mac) 0)

    ;; Process key
    (when (plusp (length key))
      (skein-ubi mac key 0 (length key))
      (let* ((padding-length (- block-length (skein-buffer-length mac)))
             (padding (make-array padding-length
                                  :element-type '(unsigned-byte 8)
                                  :initial-element 0)))
        (skein-update-tweak tweak
                            :final t
                            :position-increment (skein-buffer-length mac))
        (skein-ubi mac padding 0 padding-length t)))

    ;; Process configuration string
    (let ((padded-cfg (make-array block-length
                                  :element-type '(unsigned-byte 8)
                                  :initial-element 0)))
      (replace padded-cfg cfg :end2 32)
      (skein-update-tweak tweak
                          :first t
                          :final t
                          :type +skein-cfg+
                          :position 32)
      (skein-ubi mac padded-cfg 0 block-length t))

    ;; Prepare message processing
    (skein-update-tweak tweak
                        :first t
                        :final nil
                        :type +skein-msg+
                        :position 0)))

(defun update-skein-mac (mac sequence &key (start 0) end)
  (skein-ubi mac sequence start (or end (length sequence)))
  mac)

(defun skein-mac-digest (mac)
  (let ((digest (make-array (digest-length mac)
                            :element-type '(unsigned-byte 8)))
        (mac-copy (copy-skein-mac mac)))
    (skein-finalize mac-copy digest 0)
    digest))

(defmac skein-mac
        make-skein-mac
        update-skein-mac
        skein-mac-digest)
