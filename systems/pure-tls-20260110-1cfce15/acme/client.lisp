;;; client.lisp --- ACME protocol utilities
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Core ACME utilities: encoding, crypto, conditions.

(in-package #:pure-tls/acme)

;;; ----------------------------------------------------------------------------
;;; Configuration
;;; ----------------------------------------------------------------------------

(defparameter *staging-url*
  "https://acme-staging-v02.api.letsencrypt.org/directory"
  "Let's Encrypt staging directory for testing.")

(defparameter *production-url*
  "https://acme-v02.api.letsencrypt.org/directory"
  "Let's Encrypt production directory.")

(defparameter *acme-debug* nil
  "Enable verbose ACME debug logging when T.")

(defun acme-log (format-string &rest args)
  "Log an ACME debug message if *acme-debug* is enabled."
  (when *acme-debug*
    (apply #'format t format-string args)
    (force-output)))

;;; ----------------------------------------------------------------------------
;;; Conditions
;;; ----------------------------------------------------------------------------

(define-condition acme-error (error)
  ((message :initarg :message :reader acme-error-message))
  (:report (lambda (c s) (format s "ACME error: ~A" (acme-error-message c)))))

(define-condition acme-challenge-error (acme-error) ())
(define-condition acme-order-error (acme-error) ())
(define-condition acme-certificate-error (acme-error) ())

;;; ----------------------------------------------------------------------------
;;; Base64URL encoding (ACME requires this, not standard base64)
;;; ----------------------------------------------------------------------------

(defun base64url-encode (data)
  "Encode bytes to base64url (no padding)."
  (let* ((b64 (cl-base64:usb8-array-to-base64-string
               (if (stringp data)
                   (flexi-streams:string-to-octets data :external-format :utf-8)
                   data)))
         (url-safe (substitute #\- #\+ (substitute #\_ #\/ b64))))
    (string-right-trim "=" url-safe)))

(defun base64url-decode (string)
  "Decode base64url string to bytes."
  (let* ((padded (case (mod (length string) 4)
                   (2 (concatenate 'string string "=="))
                   (3 (concatenate 'string string "="))
                   (t string)))
         (standard (substitute #\+ #\- (substitute #\/ #\_ padded))))
    (cl-base64:base64-string-to-usb8-array standard)))

;;; ----------------------------------------------------------------------------
;;; Cryptographic operations
;;; ----------------------------------------------------------------------------

(defun get-public-key-jwk (private-key)
  "Convert EC private key's public component to JWK format."
  (let* ((key-data (ironclad:destructure-private-key private-key))
         (public-point (getf key-data :y))
         (x (subseq public-point 1 33))
         (y (subseq public-point 33 65)))
    `(("crv" . "P-256")
      ("kty" . "EC")
      ("x" . ,(base64url-encode x))
      ("y" . ,(base64url-encode y)))))

(defun get-jwk-thumbprint (jwk)
  "Calculate JWK thumbprint (SHA-256 of canonical JWK)."
  (let* ((canonical (cl-json:encode-json-to-string
                     `(("crv" . ,(cdr (assoc "crv" jwk :test #'string=)))
                       ("kty" . ,(cdr (assoc "kty" jwk :test #'string=)))
                       ("x" . ,(cdr (assoc "x" jwk :test #'string=)))
                       ("y" . ,(cdr (assoc "y" jwk :test #'string=))))))
         (hash (ironclad:digest-sequence :sha256
                (flexi-streams:string-to-octets canonical :external-format :utf-8))))
    (base64url-encode hash)))

(defun sign-payload (private-key payload)
  "Sign payload with ES256 (ECDSA P-256 + SHA-256)."
  (let* ((message (flexi-streams:string-to-octets payload :external-format :utf-8))
         (hash (ironclad:digest-sequence :sha256 message))
         (signature (ironclad:sign-message private-key hash)))
    (base64url-encode signature)))
