;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; etm.lisp -- Encrypt then MAC

(in-package :crypto)


(defclass etm (aead-mode)
  ((cipher :accessor etm-cipher
           :initform nil)
   (mac :accessor etm-mac
        :initform nil)))

(defmethod shared-initialize :after ((mode etm) slot-names &rest initargs &key cipher mac &allow-other-keys)
  (declare (ignore slot-names initargs))
  (when (or (null (etm-cipher mode)) cipher)
    (check-type cipher cipher)
    (setf (etm-cipher mode) cipher))
  (when (or (null (etm-mac mode)) mac)
    (check-type mac mac)
    (setf (etm-mac mode) mac))
  mode)

(defmethod process-associated-data ((mode etm) data &key (start 0) end)
  (if (encryption-started-p mode)
      (error 'ironclad-error :format-control "All associated data must be processed before the encryption begins.")
      (let* ((end (or end (length data))))
        (update-mac (etm-mac mode) data :start start :end end))))

(defmethod produce-tag ((mode etm) &key tag (tag-start 0))
  (produce-mac (etm-mac mode) :digest tag :digest-start tag-start))

(defmethod encrypt ((mode etm) plaintext ciphertext &key (plaintext-start 0) plaintext-end (ciphertext-start 0) handle-final-block)
  (let ((cipher (etm-cipher mode))
        (mac (etm-mac mode))
        (plaintext-end (or plaintext-end (length plaintext))))
    (unless (encryption-started-p mode)
      (setf (encryption-started-p mode) t))
    (multiple-value-bind (consumed-bytes produced-bytes)
        (encrypt cipher plaintext ciphertext
                 :plaintext-start plaintext-start :plaintext-end plaintext-end
                 :ciphertext-start ciphertext-start
                 :handle-final-block handle-final-block)
      (update-mac mac ciphertext
                  :start ciphertext-start :end (+ ciphertext-start produced-bytes))
      (values consumed-bytes produced-bytes))))

(defmethod decrypt ((mode etm) ciphertext plaintext &key (ciphertext-start 0) ciphertext-end (plaintext-start 0) handle-final-block)
  (let ((cipher (etm-cipher mode))
        (mac (etm-mac mode))
        (ciphertext-end (or ciphertext-end (length ciphertext))))
    (unless (encryption-started-p mode)
      (setf (encryption-started-p mode) t))
    (update-mac mac ciphertext
                :start ciphertext-start :end ciphertext-end)
    (multiple-value-bind (consumed-bytes produced-bytes)
        (decrypt cipher ciphertext plaintext
                 :ciphertext-start ciphertext-start :ciphertext-end ciphertext-end
                 :plaintext-start plaintext-start
                 :handle-final-block handle-final-block)
      (when (and handle-final-block (tag mode))
        (let ((correct-tag (tag mode))
              (tag (produce-mac mac)))
          (unless (constant-time-equal tag correct-tag)
            (error 'bad-authentication-tag))))
      (values consumed-bytes produced-bytes))))

(defmethod encrypt-message ((mode etm) message &key (start 0) end associated-data (associated-data-start 0) associated-data-end &allow-other-keys)
  (let* ((length (- (or end (length message)) start))
         (cipher (etm-cipher mode))
         (encrypted-length (encrypted-message-length cipher (mode cipher) length t))
         (encrypted-message (make-array encrypted-length :element-type '(unsigned-byte 8))))
    (when associated-data
      (process-associated-data mode associated-data
                               :start associated-data-start :end associated-data-end))
    (encrypt mode message encrypted-message
             :plaintext-start start :plaintext-end end
             :handle-final-block t)
    encrypted-message))

(defmethod decrypt-message ((mode etm) message &key (start 0) end associated-data (associated-data-start 0) associated-data-end &allow-other-keys)
  (let* ((length (- (or end (length message)) start))
         (decrypted-message (make-array length :element-type '(unsigned-byte 8))))
    (when associated-data
      (process-associated-data mode associated-data
                               :start associated-data-start :end associated-data-end))
    (multiple-value-bind (consumed-bytes produced-bytes)
        (decrypt mode message decrypted-message
                 :plaintext-start start :plaintext-end end
                 :handle-final-block t)
      (declare (ignore consumed-bytes))
      (if (< produced-bytes length)
          (subseq decrypted-message 0 produced-bytes)
          decrypted-message))))

(defaead etm)
