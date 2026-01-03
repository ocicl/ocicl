;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
(in-package :crypto)


;;; PBKDF1 from RFC 2898, section 5.1

(defclass pbkdf1 ()
  ((digest :reader kdf-digest)))

(defmethod shared-initialize :after ((kdf pbkdf1) slot-names &rest initargs
                                     &key digest &allow-other-keys)
  (declare (ignore slot-names initargs))
  (let ((digest-name (massage-symbol digest)))
    (cond
      ;; Permit DIGEST to be NULL to indicate reinitializing the whole
      ;; instance.
      ((null digest)
       (reinitialize-instance (kdf-digest kdf)))
      ((not (digestp digest-name))
       (error 'unsupported-digest :name digest-name))
      ;; Don't cons unnecessarily.  (Although this depends how expensive
      ;; TYPEP is with a non-constant type...)
      ((and (slot-boundp kdf 'digest)
            (typep (digest kdf) digest-name))
       (reinitialize-instance (kdf-digest kdf)))
      ((member digest-name '(md2 md5 sha1))
       (setf (slot-value kdf 'digest)
             (funcall (the function (get digest-name '%make-digest)))))
      (t
       (error 'ironclad-error
              :format-control "Digest ~A not supported for PBKDF1"
              :format-arguments (list digest))))
    kdf))

(defmethod derive-key ((kdf pbkdf1) passphrase salt iteration-count key-length)
  (check-type iteration-count (integer 1 *))
  (check-type key-length (integer 1 *))
  (loop with digest = (kdf-digest kdf)
     with digest-length = (digest-length digest)
     with key = (make-array 20 :element-type '(unsigned-byte 8))
     initially
       (update-digest digest passphrase)
       (update-digest digest salt)
       (produce-digest digest :digest key)
     for i from 1 below iteration-count
     do
       (reinitialize-instance digest)
       (update-digest digest key :end digest-length)
       (produce-digest digest :digest key)
     finally
       (return (subseq key 0 (min key-length (length key))))))


;;; PBKDF2, from RFC 2898, section 5.2

(defclass pbkdf2 ()
  ((digest-name :initarg :digest :reader kdf-digest)))

(defun pbkdf2-derive-key (digest passphrase salt iteration-count key-length)
  (check-type iteration-count (integer 1 *))
  (check-type key-length (integer 1 *))
  (loop with count = 1
     with hmac = (make-hmac passphrase digest)
     with hmac-length = (digest-length digest)
     with key = (make-array key-length :element-type '(unsigned-byte 8)
                            :initial-element 0)
     with key-position = 0
     with count-buffer = (make-array 4 :element-type '(unsigned-byte 8))
     with hmac-out = (make-array hmac-length :element-type '(unsigned-byte 8))
     while (plusp key-length)
     do (let ((size (min hmac-length key-length)))
          (reinitialize-instance hmac :key passphrase)
          (update-hmac hmac salt)
          (setf (ub32ref/be count-buffer 0) count)
          (update-hmac hmac count-buffer)
          (hmac-digest hmac :buffer hmac-out)
          (xor-block size hmac-out 0 key key-position key key-position)
          (loop for i from 1 below iteration-count
             do
               (reinitialize-instance hmac :key passphrase)
               (update-hmac hmac hmac-out)
               (hmac-digest hmac :buffer hmac-out)
               (xor-block size hmac-out 0 key key-position key key-position)
             finally
               (decf key-length size)
               (incf key-position size)
               (incf count)))
     finally (return key)))

(defmethod derive-key ((kdf pbkdf2) passphrase salt iteration-count key-length)
  (pbkdf2-derive-key (kdf-digest kdf) passphrase salt iteration-count key-length))
