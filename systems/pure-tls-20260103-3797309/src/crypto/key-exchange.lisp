;;; key-exchange.lisp --- Key exchange implementations for TLS 1.3
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements key exchange algorithms for TLS 1.3:
;;; - X25519 (Curve25519 ECDH)
;;; - secp256r1 (P-256 ECDH)

(in-package #:pure-tls)

;;;; Key Exchange Interface

(defstruct key-exchange
  "Key exchange state."
  (group 0 :type fixnum)
  (private-key nil)
  (public-key nil :type (or null octet-vector)))

;;;; X25519 Implementation

(defun make-x25519-key-exchange ()
  "Generate a new X25519 key pair for key exchange."
  (multiple-value-bind (private-key public-key)
      (ironclad:generate-key-pair :curve25519)
    (let ((public-key-bytes (ironclad:curve25519-key-y public-key)))
      (make-key-exchange :group +group-x25519+
                         :private-key private-key
                         :public-key public-key-bytes))))

(defun x25519-compute-shared-secret (key-exchange peer-public-key)
  "Compute the X25519 shared secret from our private key and peer's public key."
  (let* ((private-key (key-exchange-private-key key-exchange))
         (peer-key (ironclad:make-public-key :curve25519 :y peer-public-key)))
    (ironclad:diffie-hellman private-key peer-key)))

;;;; secp256r1 (P-256) Implementation

(defun make-secp256r1-key-exchange ()
  "Generate a new secp256r1 (P-256) key pair for key exchange."
  (multiple-value-bind (private-key public-key)
      (ironclad:generate-key-pair :secp256r1)
    ;; The public key Y slot contains the uncompressed point (04 || x || y)
    (let ((public-key-bytes (slot-value public-key 'ironclad::y)))
      (make-key-exchange :group +group-secp256r1+
                         :private-key private-key
                         :public-key public-key-bytes))))

(defun secp256r1-compute-shared-secret (key-exchange peer-public-key)
  "Compute the secp256r1 shared secret from our private key and peer's public key."
  ;; peer-public-key should be in uncompressed format: 04 || x || y
  (unless (and (>= (length peer-public-key) 65)
               (= (aref peer-public-key 0) #x04))
    (error 'tls-crypto-error
           :operation "secp256r1 key exchange"
           :message "Invalid peer public key format"))
  (let* ((private-key (key-exchange-private-key key-exchange))
         (peer-key (ironclad:make-public-key :secp256r1 :y peer-public-key)))
    (ironclad:diffie-hellman private-key peer-key)))

;;;; Generic Key Exchange Operations

(defun generate-key-exchange (group)
  "Generate a key exchange for the specified named group."
  (case group
    (#.+group-x25519+
     (make-x25519-key-exchange))
    (#.+group-secp256r1+
     (make-secp256r1-key-exchange))
    (t
     (error 'tls-crypto-error
            :operation "key exchange"
            :message (format nil "Unsupported group: ~X" group)))))

(defun compute-shared-secret (key-exchange peer-public-key)
  "Compute the shared secret using the key exchange and peer's public key."
  (let ((group (key-exchange-group key-exchange)))
    (case group
      (#.+group-x25519+
       (x25519-compute-shared-secret key-exchange peer-public-key))
      (#.+group-secp256r1+
       (secp256r1-compute-shared-secret key-exchange peer-public-key))
      (t
       (error 'tls-crypto-error
              :operation "compute shared secret"
              :message (format nil "Unsupported group: ~X" group))))))

(defun key-exchange-public-key-length (group)
  "Return the public key length in bytes for the given named group."
  (case group
    (#.+group-x25519+ 32)
    (#.+group-x448+ 56)
    (#.+group-secp256r1+ 65)  ; uncompressed point
    (#.+group-secp384r1+ 97)  ; uncompressed point
    (#.+group-secp521r1+ 133) ; uncompressed point
    (t 0)))

(defun named-group-name (group)
  "Return the human-readable name for a named group."
  (case group
    (#.+group-x25519+ "x25519")
    (#.+group-x448+ "x448")
    (#.+group-secp256r1+ "secp256r1")
    (#.+group-secp384r1+ "secp384r1")
    (#.+group-secp521r1+ "secp521r1")
    (#.+group-ffdhe2048+ "ffdhe2048")
    (#.+group-ffdhe3072+ "ffdhe3072")
    (#.+group-ffdhe4096+ "ffdhe4096")
    (t (format nil "unknown(~X)" group))))

;;;; Supported Groups for ClientHello

(defparameter *supported-groups*
  (list +group-x25519+ +group-secp256r1+)
  "List of named groups we support, in preference order.")

(defparameter *preferred-group* +group-x25519+
  "The preferred named group to offer in ClientHello key_share.")

;;;; Signature Algorithms

(defun supported-signature-algorithms ()
  "Return the list of supported signature algorithms for certificate verification."
  (list +sig-ecdsa-secp256r1-sha256+
        +sig-rsa-pss-rsae-sha256+
        +sig-rsa-pkcs1-sha256+
        +sig-ecdsa-secp384r1-sha384+
        +sig-rsa-pss-rsae-sha384+
        +sig-rsa-pkcs1-sha384+
        +sig-rsa-pss-rsae-sha512+
        +sig-rsa-pkcs1-sha512+
        +sig-ed25519+))

(defun signature-algorithm-name (sig-alg)
  "Return the human-readable name for a signature algorithm."
  (case sig-alg
    (#.+sig-rsa-pkcs1-sha256+ "rsa_pkcs1_sha256")
    (#.+sig-rsa-pkcs1-sha384+ "rsa_pkcs1_sha384")
    (#.+sig-rsa-pkcs1-sha512+ "rsa_pkcs1_sha512")
    (#.+sig-ecdsa-secp256r1-sha256+ "ecdsa_secp256r1_sha256")
    (#.+sig-ecdsa-secp384r1-sha384+ "ecdsa_secp384r1_sha384")
    (#.+sig-ecdsa-secp521r1-sha512+ "ecdsa_secp521r1_sha512")
    (#.+sig-rsa-pss-rsae-sha256+ "rsa_pss_rsae_sha256")
    (#.+sig-rsa-pss-rsae-sha384+ "rsa_pss_rsae_sha384")
    (#.+sig-rsa-pss-rsae-sha512+ "rsa_pss_rsae_sha512")
    (#.+sig-ed25519+ "ed25519")
    (#.+sig-ed448+ "ed448")
    (t (format nil "unknown(~X)" sig-alg))))
