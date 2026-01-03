;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; gcm.lisp -- Galois counter mode

;; See nistspecialpublication800-38d.pdf about GCM and GMAC.

(in-package :crypto)


(defclass gcm (aead-mode)
  ((cipher :accessor gcm-cipher
           :initform nil)
   (mac :accessor gcm-mac
        :initform nil)
   (associated-data-length :accessor gcm-ad-length
                           :initform 0
                           :type (integer 0 *))
   (encrypted-data-length :accessor gcm-ed-length
                          :initform 0
                          :type (integer 0 *))))

(defmethod shared-initialize :after ((mode gcm) slot-names &rest initargs &key key cipher-name initialization-vector &allow-other-keys)
  (declare (ignore slot-names initargs)
           (type simple-octet-vector key initialization-vector))
  (let* ((mac (if (or (null (gcm-mac mode)) cipher-name)
                  (make-mac :gmac key cipher-name initialization-vector)
                  (reinitialize-instance (gcm-mac mode)
                                         :key key
                                         :initialization-vector initialization-vector)))
         (iv (let* ((iv (gmac-j0 mac))
                    (counter (subseq iv 12)))
               (increment-counter-block-1 4 counter)
               (replace iv counter :start1 12)))
         (cipher (if (or (null (gcm-cipher mode)) cipher-name)
                     (make-cipher cipher-name
                                  :key key
                                  :mode :ctr
                                  :initialization-vector iv)
                     (reinitialize-instance (gcm-cipher mode)
                                            :key key
                                            :mode :ctr
                                            :initialization-vector iv))))
    (setf (gcm-mac mode) mac
          (gcm-cipher mode) cipher
          (gcm-ad-length mode) 0
          (gcm-ed-length mode) 0))
  mode)

(defmethod process-associated-data ((mode gcm) data &key (start 0) end)
  (if (encryption-started-p mode)
      (error 'ironclad-error :format-control "All associated data must be processed before the encryption begins.")
      (let* ((end (or end (length data)))
             (length (- end start)))
        (incf (gcm-ad-length mode) length)
        (update-mac (gcm-mac mode) data :start start :end end))))

(defmethod produce-tag ((mode gcm) &key tag (tag-start 0))
  (let* ((encrypted-data-length (gcm-ed-length mode))
         (mac (gcm-mac mode))
         (mac-digest (gmac-digest mac encrypted-data-length))
         (digest-size (length mac-digest)))
    (etypecase tag
      (simple-octet-vector
       (if (<= digest-size (- (length tag) tag-start))
           (replace tag mac-digest :start1 tag-start)
           (error 'insufficient-buffer-space
                  :buffer tag
                  :start tag-start
                  :length digest-size)))
      (null
       mac-digest))))

(defmethod encrypt ((mode gcm) plaintext ciphertext &key (plaintext-start 0) plaintext-end (ciphertext-start 0) handle-final-block)
  (declare (ignore handle-final-block))
  (let ((cipher (gcm-cipher mode))
        (mac (gcm-mac mode))
        (plaintext-end (or plaintext-end (length plaintext)))
        (consumed-bytes 0)
        (produced-bytes 0))
    (when (< plaintext-start plaintext-end)
      (unless (encryption-started-p mode)
        (let* ((associated-data-length (gcm-ad-length mode))
               (remaining (mod associated-data-length 16))
               (padding-length (if (zerop remaining) 0 (- 16 remaining)))
               (padding (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
          (declare (dynamic-extent padding))
          (update-mac mac padding :end padding-length)
          (decf (gmac-total-length mac) padding-length))
        (setf (encryption-started-p mode) t))
      (multiple-value-setq (consumed-bytes produced-bytes)
        (encrypt cipher plaintext ciphertext
                 :plaintext-start plaintext-start :plaintext-end plaintext-end
                 :ciphertext-start ciphertext-start))
      (incf (gcm-ed-length mode) produced-bytes)
      (update-mac mac ciphertext
                  :start ciphertext-start :end (+ ciphertext-start produced-bytes)))
    (values consumed-bytes produced-bytes)))

(defmethod decrypt ((mode gcm) ciphertext plaintext &key (ciphertext-start 0) ciphertext-end (plaintext-start 0) handle-final-block)
  (let ((cipher (gcm-cipher mode))
        (mac (gcm-mac mode))
        (ciphertext-end (or ciphertext-end (length ciphertext)))
        (consumed-bytes 0)
        (produced-bytes 0))
    (when (< ciphertext-start ciphertext-end)
      (unless (encryption-started-p mode)
        (let* ((associated-data-length (gcm-ad-length mode))
               (remaining (mod associated-data-length 16))
               (padding-length (if (zerop remaining) 0 (- 16 remaining)))
               (padding (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
          (declare (dynamic-extent padding))
          (update-mac mac padding :end padding-length)
          (decf (gmac-total-length mac) padding-length))
        (setf (encryption-started-p mode) t))
      (update-mac mac ciphertext
                  :start ciphertext-start :end ciphertext-end)
      (multiple-value-setq (consumed-bytes produced-bytes)
        (decrypt cipher ciphertext plaintext
                 :ciphertext-start ciphertext-start :ciphertext-end ciphertext-end
                 :plaintext-start plaintext-start))
      (incf (gcm-ed-length mode) consumed-bytes))
    (when (and handle-final-block (tag mode))
      (let* ((correct-tag (tag mode))
             (encrypted-data-length (gcm-ed-length mode))
             (full-tag (gmac-digest mac encrypted-data-length))
             (tag (if (< (length correct-tag) (length full-tag))
                      (subseq full-tag 0 (length correct-tag))
                      full-tag)))
        (unless (constant-time-equal tag correct-tag)
          (error 'bad-authentication-tag))))
    (values consumed-bytes produced-bytes)))

(defmethod encrypt-message ((mode gcm) message &key (start 0) end associated-data (associated-data-start 0) associated-data-end &allow-other-keys)
  (let* ((length (- (or end (length message)) start))
         (encrypted-message (make-array length :element-type '(unsigned-byte 8))))
    (when associated-data
      (process-associated-data mode associated-data
                               :start associated-data-start :end associated-data-end))
    (encrypt mode message encrypted-message
             :plaintext-start start :plaintext-end end)
    encrypted-message))

(defmethod decrypt-message ((mode gcm) message &key (start 0) end associated-data (associated-data-start 0) associated-data-end &allow-other-keys)
  (let* ((length (- (or end (length message)) start))
         (decrypted-message (make-array length :element-type '(unsigned-byte 8))))
    (when associated-data
      (process-associated-data mode associated-data
                               :start associated-data-start :end associated-data-end))
    (decrypt mode message decrypted-message
             :plaintext-start start :plaintext-end end
             :handle-final-block t)
    decrypted-message))

(defaead gcm)
