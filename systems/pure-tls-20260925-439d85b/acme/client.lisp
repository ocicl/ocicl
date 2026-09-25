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

(defparameter *default-profile* "tlsserver"
  "Default ACME profile for certificate orders.
   Valid profiles: \"classic\", \"tlsserver\", \"shortlived\".")

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
;;; HTTP-level conditions (recoverable via restarts)
;;;
;;; These map ACME HTTP responses onto typed conditions. A policy handler
;;; (see WITH-ACME-RETRIES) turns each recoverable condition into a RETRY
;;; restart that re-drives the offending request, without unwinding the stack.
;;; ----------------------------------------------------------------------------

(define-condition acme-http-error (acme-error)
  ((status :initarg :status :initform nil :reader acme-http-error-status
           :documentation "HTTP status code of the offending response.")
   (problem :initarg :problem :initform nil :reader acme-http-error-problem
            :documentation "Decoded RFC 7807 problem document alist, or NIL.")
   (headers :initarg :headers :initform nil :reader acme-http-error-headers
            :documentation "Response headers alist.")
   (url :initarg :url :initform nil :reader acme-http-error-url
        :documentation "URL of the request that produced this response.")
   (method :initarg :method :initform nil :reader acme-http-error-method
           :documentation "HTTP method keyword of the request (:get / :post).")
   (retry-after :initarg :retry-after :initform nil :reader acme-http-error-retry-after
                :documentation "Parsed Retry-After delay in seconds, or NIL."))
  (:default-initargs :message "ACME HTTP error")
  (:report
   (lambda (condition stream)
     (format stream "ACME HTTP ~A error for ~A ~A~@[: ~A~]"
             (acme-http-error-status condition)
             (acme-http-error-method condition)
             (acme-http-error-url condition)
             (let ((problem (acme-http-error-problem condition)))
               (and (consp problem) (rest (assoc :detail problem)))))))
  (:documentation "Base condition for a recoverable ACME HTTP response."))

(define-condition acme-bad-nonce (acme-http-error) ()
  (:documentation
   "The ACME server rejected the request nonce (error type badNonce).
    Recoverable: refresh the nonce and retry the request."))

(define-condition acme-not-ready (acme-http-error) ()
  (:documentation
   "The requested ACME resource is not yet ready (e.g. HTTP 202 Accepted).
    Recoverable: wait per Retry-After and retry the request."))

(define-condition acme-rate-limited (acme-http-error) ()
  (:documentation
   "The ACME server rate-limited the request (HTTP 429 or error type
    rateLimited). Recoverable: wait per Retry-After and retry the request."))

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
                   (otherwise string)))
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
                     `(("crv" . ,(rest (assoc "crv" jwk :test #'string=)))
                       ("kty" . ,(rest (assoc "kty" jwk :test #'string=)))
                       ("x" . ,(rest (assoc "x" jwk :test #'string=)))
                       ("y" . ,(rest (assoc "y" jwk :test #'string=))))))
         (hash (ironclad:digest-sequence :sha256
                (flexi-streams:string-to-octets canonical :external-format :utf-8))))
    (base64url-encode hash)))

(defun sign-payload (private-key payload)
  "Sign payload with ES256 (ECDSA P-256 + SHA-256)."
  (let* ((message (flexi-streams:string-to-octets payload :external-format :utf-8))
         (hash (ironclad:digest-sequence :sha256 message))
         (signature (ironclad:sign-message private-key hash)))
    (base64url-encode signature)))
