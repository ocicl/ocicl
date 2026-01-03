;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; conditions.lisp -- various error conditions

(in-package :ironclad)

(define-condition ironclad-error (simple-error)
  ())

(define-condition key-not-supplied (ironclad-error)
  ((cipher :initarg :cipher :reader cipher))
  (:report (lambda (condition stream)
             (format stream "Cipher ~A requires a key." (cipher condition))))
  (:documentation "Signaled when a key is not provided at the initialization
of a cipher."))

(define-condition initialization-vector-not-supplied (ironclad-error)
  ((mode :initarg :mode :reader mode))
  (:report (lambda (condition stream)
             (format stream "Mode ~A requires an initialization vector."
                     (mode condition))))
  (:documentation "Signaled when an initialization vector is required
for a particular mode of operation but not supplied."))

(define-condition invalid-initialization-vector (ironclad-error)
  ((cipher :initarg :cipher :reader cipher)
   (block-length :initarg :block-length :reader block-length))
  (:report (lambda (condition stream)
             (format stream "Cipher ~A requires an initialization vector of length ~D."
                     (cipher condition)
                     (block-length condition))))
  (:documentation "Signaled when an invalid initialization vector is supplied to MAKE-CIPHER."))

(define-condition invalid-key-length (ironclad-error)
  ((cipher :initarg :cipher :reader cipher)
   (lengths :initarg :accepted-lengths :reader accepted-lengths))
  (:report (lambda (condition stream)
             (format stream "Cipher ~A only accepts keys of these lengths: ~A."
                     (cipher condition)
                     (accepted-lengths condition))))
  (:documentation "Signaled when a key is not the proper length for a cipher."))

(define-condition unsupported-cipher (ironclad-error)
  ((cipher :initarg :name :reader cipher))
  (:report (lambda (condition stream)
             (format stream "Cipher ~A is not a supported cipher."
                     (cipher condition))))
  (:documentation "Signaled when an invalid cipher name is provided to MAKE-CIPHER."))

(define-condition unsupported-mode (ironclad-error)
  ((mode :initarg :mode :reader mode)
   (cipher :initarg :cipher :reader cipher))
  (:report (lambda (condition stream)
             (if (cipher condition)
                 (format stream "Mode ~A is not a supported mode for ~A."
                         (mode condition) (cipher condition))
                 (format stream "Mode ~A is not a supported mode."
                         (mode condition)))))
  (:documentation "Signaled when an invalid mode name is provided to MAKE-CIPHER."))

(define-condition unsupported-padding (ironclad-error)
  ((padding :initarg :name :reader padding))
  (:report (lambda (condition stream)
             (format stream "Padding ~A is not a supported padding."
                     (padding condition))))
  (:documentation "Signaled when an invalid padding name is provided to MAKE-CIPHER."))

(define-condition unsupported-digest (ironclad-error)
  ((digest :initarg :name :reader digest))
  (:report (lambda (condition stream)
             (format stream "Digest ~A is not a supported digest."
                     (digest condition))))
  (:documentation "Signaled when an invalid digest name is provided to MAKE-DIGEST."))

(define-condition unsupported-mac (ironclad-error)
  ((mac :initarg :name :reader mac))
  (:report (lambda (condition stream)
             (format stream "MAC ~A is not a supported MAC."
                     (mac condition))))
  (:documentation "Signaled when an invalid MAC name is provided to MAKE-MAC."))

(define-condition unsupported-kdf (ironclad-error)
  ((kdf :initarg :kdf :reader kdf))
  (:report (lambda (condition stream)
             (format stream "~A is not a supported key derivation function."
                     (kdf condition))))
  (:documentation "Signaled when an invalid key derivation function name is provided to MAKE-KDF."))

(define-condition unsupported-scrypt-cost-factors (ironclad-error)
  ((N :initarg :N :reader cost-N)
   (r :initarg :r :reader cost-r)
   (p :initarg :p :reader cost-p))
  (:report (lambda (condition stream)
             (format stream "Scrypt cost factors not supported. N=~A must be a power of two and (r=~A * p=~A) <= 2^30."
                     (cost-N condition) (cost-r condition) (cost-p condition))))
  (:documentation "Signaled when invalid Scrypt cost factors are provided to MAKE-KDF."))

(define-condition unsupported-argon2-parameters (ironclad-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
             (format stream "Argon2 parameters not supported. block-count must be at least 8, key-length must be at least 4, salt must be at least 8 bytes long and iteration-count must be at least 1.")))
  (:documentation "Signaled when invalid Argon2 parameters are provided to MAKE-KDF."))

(define-condition insufficient-buffer-space (ironclad-error)
  ((buffer :initarg :buffer :reader insufficient-buffer-space-buffer)
   (start :initarg :start :reader insufficient-buffer-space-start)
   (length :initarg :length :reader insufficient-buffer-space-length))
  (:report (lambda (condition stream)
             (format stream "Buffer ~A cannot accommodate ~D elements starting at index ~D."
                     (insufficient-buffer-space-buffer condition)
                     (insufficient-buffer-space-length condition)
                     (insufficient-buffer-space-start condition))))
  (:documentation "Signaled when insufficient buffer space exists for an operation."))

(define-condition invalid-padding (ironclad-error)
  ((padding-name :initarg :name :reader invalid-padding-padding-name)
   (block :initarg :block :reader invalid-padding-block))
  (:report (lambda (condition stream)
             (format stream "The ~A padding in block ~A is invalid."
                     (invalid-padding-padding-name condition)
                     (invalid-padding-block condition))))
  (:documentation "Signaled when padding in a block is determined to be invalid."))

(define-condition invalid-mac-parameter (ironclad-error)
  ((mac-name :initarg :mac-name :reader mac-name)
   (message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream "Invalid parameter for MAC ~A. ~A."
                     (mac-name condition)
                     (message condition))))
  (:documentation "Signaled when an invalid parameter is provided to MAKE-MAC."))

(define-condition invalid-signature-length (ironclad-error)
  ((kind :initarg :kind :reader kind))
  (:report (lambda (condition stream)
             (format stream "Invalid signature length for ~A." (kind condition))))
  (:documentation "Signaled when a signature with an invalid length is
provided to VERIFY-SIGNATURE or DESTRUCTURE-SIGNATURE."))

(define-condition invalid-message-length (ironclad-error)
  ((kind :initarg :kind :reader kind))
  (:report (lambda (condition stream)
             (format stream "Invalid message length for ~A." (kind condition))))
  (:documentation "Signaled when a message with an invalid length is
provided to ENCRYPT-MESSAGE, DECRYPT-MESSAGE or DESTRUCTURE-MESSAGE."))

(define-condition missing-key-parameter (ironclad-error)
  ((kind :initarg :kind :reader kind)
   (parameter :initarg :parameter :reader parameter)
   (description :initarg :description :reader description))
  (:report (lambda (condition stream)
             (format stream "Missing ~A ~A for ~A key."
                     (description condition)
                     (parameter condition)
                     (kind condition))))
  (:documentation "Signaled when it is determined that a parameter is
missing in a call to MAKE-PUBLIC-KEY or MAKE-PRIVATE-KEY."))

(define-condition missing-message-parameter (ironclad-error)
  ((kind :initarg :kind :reader kind)
   (parameter :initarg :parameter :reader parameter)
   (description :initarg :description :reader description))
  (:report (lambda (condition stream)
             (format stream "Missing ~A ~A for ~A message."
                     (description condition)
                     (parameter condition)
                     (kind condition))))
  (:documentation "Signaled when it is determined that a parameter is
missing in a call to MAKE-MESSAGE."))

(define-condition missing-point-parameter (ironclad-error)
  ((kind :initarg :kind :reader kind)
   (parameter :initarg :parameter :reader parameter)
   (description :initarg :description :reader description))
  (:report (lambda (condition stream)
             (format stream "Missing ~A ~A for ~A point."
                     (description condition)
                     (parameter condition)
                     (kind condition))))
  (:documentation "Signaled when it is determined that a parameter is
missing in a call to EC-MAKE-POINT."))

(define-condition missing-signature-parameter (ironclad-error)
  ((kind :initarg :kind :reader kind)
   (parameter :initarg :parameter :reader parameter)
   (description :initarg :description :reader description))
  (:report (lambda (condition stream)
             (format stream "Missing ~A ~A for ~A signature."
                     (description condition)
                     (parameter condition)
                     (kind condition))))
  (:documentation "Signaled when it is determined that a parameter is
missing in a call to MAKE-SIGNATURE."))

(define-condition incompatible-keys (ironclad-error)
  ((kind :initarg :kind :reader kind))
  (:report (lambda (condition stream)
             (format stream "The ~A keys are not compatible because they are not in the same group."
                     (kind condition))))
  (:documentation "Signaled when providing keys that are not compatible to DIFFIE-HELLMAN."))

(define-condition invalid-curve-point (ironclad-error)
  ((kind :initarg :kind :reader kind))
  (:report (lambda (condition stream)
             (format stream "Point not on curve ~A." (kind condition))))
  (:documentation "Signaled when trying to use an invalid curve point."))

(define-condition invalid-public-key-length (ironclad-error)
  ((kind :initarg :kind :reader kind))
  (:report (lambda (condition stream)
             (format stream "Invalid public key length for ~A." (kind condition))))
  (:documentation "Signaled when a public key with an invalid length is
provided to VERIFY-SIGNATURE."))

(define-condition oaep-decoding-error (ironclad-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "OAEP decoding of the message failed.")))
  (:documentation "Signaled when the OAEP decoding of a message fails."))

(define-condition unsupported-authenticated-encryption-mode (ironclad-error)
  ((name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream "~A is not a supported authenticated encryption mode."
                     (name condition))))
  (:documentation "Signaled when an invalid mode name is provided to MAKE-AUTHENTICATED-ENCRYPTION-MODE."))

(define-condition bad-authentication-tag (ironclad-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Bad authentication tag.")))
  (:documentation "Signaled when the verification of authenticity of a message fails."))
