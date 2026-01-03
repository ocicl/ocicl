;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; eax.lisp -- Encrypt then authenticate then translate

(in-package :crypto)


(defclass eax (aead-mode)
  ((cipher :accessor eax-cipher
           :initform nil)
   (mac-n :accessor eax-mac-n
          :initform nil)
   (mac-h :accessor eax-mac-h
          :initform nil)
   (mac-c :accessor eax-mac-c
          :initform nil)))

(defmethod shared-initialize :after ((mode eax) slot-names &rest initargs &key key cipher-name initialization-vector &allow-other-keys)
  (declare (ignore slot-names initargs))
  (let* ((mac-n (if (or (null (eax-mac-n mode)) cipher-name)
                    (make-mac :cmac key cipher-name)
                    (reinitialize-instance (eax-mac-n mode) :key key)))
         (mac-h (if (or (null (eax-mac-h mode)) cipher-name)
                    (make-mac :cmac key cipher-name)
                    (reinitialize-instance (eax-mac-h mode) :key key)))
         (mac-c (if (or (null (eax-mac-c mode)) cipher-name)
                    (make-mac :cmac key cipher-name)
                    (reinitialize-instance (eax-mac-c mode) :key key)))
         (block-length (block-length (or cipher-name (eax-cipher mode))))
         (buffer (make-array block-length
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (update-mac mac-n buffer)
    (update-mac mac-n initialization-vector)
    (setf (aref buffer (1- (length buffer))) 1)
    (update-mac mac-h buffer)
    (setf (aref buffer (1- (length buffer))) 2)
    (update-mac mac-c buffer)
    (let* ((n (produce-mac mac-n))
           (cipher (if (or (null (eax-cipher mode)) cipher-name)
                       (make-cipher cipher-name
                                    :key key
                                    :mode :ctr
                                    :initialization-vector n)
                       (reinitialize-instance (eax-cipher mode)
                                              :key key
                                              :mode :ctr
                                              :initialization-vector n))))
      (setf (eax-mac-n mode) mac-n
            (eax-mac-h mode) mac-h
            (eax-mac-c mode) mac-c
            (eax-cipher mode) cipher)))
  mode)

(defmethod process-associated-data ((mode eax) data &key (start 0) end)
  (let* ((end (or end (length data))))
    (update-mac (eax-mac-h mode) data :start start :end end)))

(defmethod produce-tag ((mode eax) &key tag (tag-start 0))
  (let* ((n (produce-mac (eax-mac-n mode)))
         (h (produce-mac (eax-mac-h mode)))
         (c (produce-mac (eax-mac-c mode)))
         (block-length (length c)))
    (etypecase tag
      (simple-octet-vector
       (when (> block-length (- (length tag) tag-start))
         (error 'insufficient-buffer-space
                :buffer tag
                :start tag-start
                :length block-length))
       (xor-block block-length n 0 c 0 c 0)
       (xor-block block-length h 0 c 0 tag tag-start)
       tag)
      (null
       (xor-block block-length n 0 c 0 c 0)
       (xor-block block-length h 0 c 0 c 0)
       c))))

(defmethod encrypt ((mode eax) plaintext ciphertext &key (plaintext-start 0) plaintext-end (ciphertext-start 0) handle-final-block)
  (declare (ignore handle-final-block))
  (let ((cipher (eax-cipher mode))
        (mac-c (eax-mac-c mode))
        (plaintext-end (or plaintext-end (length plaintext))))
    (multiple-value-bind (consumed-bytes produced-bytes)
        (encrypt cipher plaintext ciphertext
                 :plaintext-start plaintext-start :plaintext-end plaintext-end
                 :ciphertext-start ciphertext-start)
      (update-mac mac-c ciphertext
                  :start ciphertext-start :end (+ ciphertext-start produced-bytes))
      (values consumed-bytes produced-bytes))))

(defmethod decrypt ((mode eax) ciphertext plaintext &key (ciphertext-start 0) ciphertext-end (plaintext-start 0) handle-final-block)
  (let ((cipher (eax-cipher mode))
        (mac-c (eax-mac-c mode))
        (ciphertext-end (or ciphertext-end (length ciphertext))))
    (update-mac mac-c ciphertext
                :start ciphertext-start :end ciphertext-end)
    (multiple-value-bind (consumed-bytes produced-bytes)
        (decrypt cipher ciphertext plaintext
                 :ciphertext-start ciphertext-start :ciphertext-end ciphertext-end
                 :plaintext-start plaintext-start)
      (when (and handle-final-block (tag mode))
        (let ((correct-tag (tag mode))
              (tag (produce-tag mode)))
          (unless (constant-time-equal tag correct-tag)
            (error 'bad-authentication-tag))))
      (values consumed-bytes produced-bytes))))

(defmethod encrypt-message ((mode eax) message &key (start 0) end associated-data (associated-data-start 0) associated-data-end &allow-other-keys)
  (let* ((length (- (or end (length message)) start))
         (encrypted-message (make-array length :element-type '(unsigned-byte 8))))
    (when associated-data
      (process-associated-data mode associated-data
                               :start associated-data-start :end associated-data-end))
    (encrypt mode message encrypted-message
             :plaintext-start start :plaintext-end end)
    encrypted-message))

(defmethod decrypt-message ((mode eax) message &key (start 0) end associated-data (associated-data-start 0) associated-data-end &allow-other-keys)
  (let* ((length (- (or end (length message)) start))
         (decrypted-message (make-array length :element-type '(unsigned-byte 8))))
    (when associated-data
      (process-associated-data mode associated-data
                               :start associated-data-start :end associated-data-end))
    (decrypt mode message decrypted-message
             :plaintext-start start :plaintext-end end
             :handle-final-block t)
    decrypted-message))

(defaead eax)
