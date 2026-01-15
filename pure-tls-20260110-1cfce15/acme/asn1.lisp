;;; asn1.lisp --- ASN.1 DER encoding for ACME
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; ASN.1 DER encoding utilities for CSR and certificate generation.

(in-package #:pure-tls/acme)

;;; ----------------------------------------------------------------------------
;;; ASN.1 DER Tag Constants
;;; ----------------------------------------------------------------------------

(defconstant +asn1-integer+ #x02)
(defconstant +asn1-bit-string+ #x03)
(defconstant +asn1-octet-string+ #x04)
(defconstant +asn1-null+ #x05)
(defconstant +asn1-oid+ #x06)
(defconstant +asn1-utf8-string+ #x0c)
(defconstant +asn1-utc-time+ #x17)
(defconstant +asn1-printable-string+ #x13)
(defconstant +asn1-sequence+ #x30)
(defconstant +asn1-set+ #x31)

;;; ----------------------------------------------------------------------------
;;; Common OIDs
;;; ----------------------------------------------------------------------------

(defparameter *oid-rsa-encryption*
  '(1 2 840 113549 1 1 1)
  "RSA encryption OID: 1.2.840.113549.1.1.1")

(defparameter *oid-sha256-with-rsa*
  '(1 2 840 113549 1 1 11)
  "SHA-256 with RSA signature OID: 1.2.840.113549.1.1.11")

(defparameter *oid-sha1-with-rsa*
  '(1 2 840 113549 1 1 5)
  "SHA-1 with RSA signature OID: 1.2.840.113549.1.1.5")

(defparameter *oid-common-name*
  '(2 5 4 3)
  "Common Name (CN) OID: 2.5.4.3")

(defparameter *oid-extension-request*
  '(1 2 840 113549 1 9 14)
  "Extension request OID: 1.2.840.113549.1.9.14")

(defparameter *oid-subject-alt-name*
  '(2 5 29 17)
  "Subject Alternative Name OID: 2.5.29.17")

(defparameter *oid-acme-identifier*
  '(1 3 6 1 5 5 7 1 31)
  "ACME Identifier extension OID for TLS-ALPN-01: 1.3.6.1.5.5.7.1.31")

(defparameter *oid-ecdsa-with-sha256*
  '(1 2 840 10045 4 3 2)
  "ECDSA with SHA-256 OID: 1.2.840.10045.4.3.2")

(defparameter *oid-ec-public-key*
  '(1 2 840 10045 2 1)
  "EC Public Key OID: 1.2.840.10045.2.1")

(defparameter *oid-secp256r1*
  '(1 2 840 10045 3 1 7)
  "secp256r1 (P-256) curve OID: 1.2.840.10045.3.1.7")

;;; ----------------------------------------------------------------------------
;;; Basic DER Encoding
;;; ----------------------------------------------------------------------------

(defun encode-length (length)
  "Encode ASN.1 DER length bytes."
  (cond
    ((< length 128)
     (vector length))
    ((< length 256)
     (vector #x81 length))
    ((< length 65536)
     (vector #x82 (ash length -8) (logand length #xff)))
    (t
     (vector #x83 (ash length -16) (logand (ash length -8) #xff) (logand length #xff)))))

(defun encode-der (tag content)
  "Encode content with ASN.1 DER tag and length."
  (let* ((content-bytes (if (typep content '(vector (unsigned-byte 8)))
                            content
                            (coerce content '(vector (unsigned-byte 8)))))
         (length-bytes (encode-length (length content-bytes))))
    (concatenate '(vector (unsigned-byte 8))
                 (vector tag)
                 length-bytes
                 content-bytes)))

(defun encode-integer (value)
  "Encode an integer in ASN.1 DER format."
  (let ((bytes (if (zerop value)
                   (vector 0)
                   (let ((result '()))
                     (loop while (plusp value)
                           do (push (logand value #xff) result)
                              (setf value (ash value -8)))
                     ;; Add leading zero if high bit set (to ensure positive)
                     (when (>= (first result) 128)
                       (push 0 result))
                     (coerce result '(vector (unsigned-byte 8)))))))
    (encode-der +asn1-integer+ bytes)))

(defun encode-integer-bytes (bytes)
  "Encode a byte vector as ASN.1 INTEGER (adding leading 0 if needed)."
  (let ((content (if (and (> (length bytes) 0)
                          (>= (aref bytes 0) 128))
                     (concatenate '(vector (unsigned-byte 8)) #(0) bytes)
                     bytes)))
    (encode-der +asn1-integer+ content)))

(defun integer-to-bytes (n)
  "Convert a large integer to a byte vector."
  (if (zerop n)
      #(0)
      (let ((bytes '()))
        (loop while (plusp n)
              do (push (logand n #xff) bytes)
                 (setf n (ash n -8)))
        (coerce bytes '(vector (unsigned-byte 8))))))

(defun encode-oid (oid-list)
  "Encode OID as ASN.1 DER."
  (let ((bytes '()))
    ;; First two components: 40*first + second
    (push (+ (* 40 (first oid-list)) (second oid-list)) bytes)
    ;; Remaining components use base-128 encoding
    (dolist (component (cddr oid-list))
      (if (< component 128)
          (push component bytes)
          (let ((encoded '()))
            (loop while (plusp component)
                  do (push (logior (logand component #x7f)
                                   (if encoded #x80 0))
                           encoded)
                     (setf component (ash component -7)))
            (dolist (b encoded)
              (push b bytes)))))
    (encode-der +asn1-oid+ (coerce (nreverse bytes) '(vector (unsigned-byte 8))))))

(defun encode-utf8-string (string)
  "Encode string as ASN.1 UTF8String."
  (encode-der +asn1-utf8-string+
              (flexi-streams:string-to-octets string :external-format :utf-8)))

(defun encode-printable-string (string)
  "Encode string as ASN.1 PrintableString."
  (encode-der +asn1-printable-string+
              (flexi-streams:string-to-octets string :external-format :ascii)))

(defun encode-sequence (&rest items)
  "Encode items as ASN.1 SEQUENCE."
  (encode-der +asn1-sequence+
              (apply #'concatenate '(vector (unsigned-byte 8)) items)))

(defun encode-set (&rest items)
  "Encode items as ASN.1 SET."
  (encode-der +asn1-set+
              (apply #'concatenate '(vector (unsigned-byte 8)) items)))

(defun encode-bit-string (bytes &optional (unused-bits 0))
  "Encode bytes as ASN.1 BIT STRING."
  (encode-der +asn1-bit-string+
              (concatenate '(vector (unsigned-byte 8))
                           (vector unused-bits)
                           bytes)))

(defun encode-octet-string (bytes)
  "Encode bytes as ASN.1 OCTET STRING."
  (encode-der +asn1-octet-string+ bytes))

(defun encode-null ()
  "Encode ASN.1 NULL."
  #(#x05 #x00))

(defun encode-context-tag (tag-num content)
  "Encode with context-specific tag."
  (encode-der (logior #xa0 tag-num) content))

(defun encode-utc-time (universal-time)
  "Encode a universal time as ASN.1 UTCTime."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time 0)
    (let ((time-str (format nil "~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0DZ"
                            (mod year 100) month day hour min sec)))
      (encode-der +asn1-utc-time+
                  (flexi-streams:string-to-octets time-str :external-format :ascii)))))

;;; ----------------------------------------------------------------------------
;;; X.509 Structure Encoding
;;; ----------------------------------------------------------------------------

(defun encode-validity (not-before not-after)
  "Encode X.509 Validity as SEQUENCE of UTCTime."
  (encode-sequence
   (encode-utc-time not-before)
   (encode-utc-time not-after)))

(defun encode-subject (common-name)
  "Encode X.501 Name (subject) with Common Name."
  ;; Name ::= SEQUENCE OF RelativeDistinguishedName
  ;; RDN ::= SET OF AttributeTypeAndValue
  ;; AttributeTypeAndValue ::= SEQUENCE { type OID, value ANY }
  (encode-sequence
   (encode-set
    (encode-sequence
     (encode-oid *oid-common-name*)
     (encode-utf8-string common-name)))))

(defun encode-san-extension (domains)
  "Encode Subject Alternative Name extension with DNS names."
  ;; SubjectAltName ::= GeneralNames
  ;; GeneralNames ::= SEQUENCE OF GeneralName
  ;; GeneralName ::= dNSName [2] IA5String
  (let ((dns-names (mapcar (lambda (domain)
                             ;; dNSName is [2] implicit IA5String
                             (encode-der #x82  ; context tag 2
                                         (flexi-streams:string-to-octets
                                          domain :external-format :ascii)))
                           (if (listp domains) domains (list domains)))))
    (apply #'encode-sequence dns-names)))

(defun encode-x509-extensions (extensions)
  "Encode X.509 extensions as [3] EXPLICIT SEQUENCE."
  (encode-context-tag 3 (apply #'encode-sequence extensions)))

(defun encode-critical-extension (oid value-bytes)
  "Encode a critical X.509 extension."
  (encode-sequence
   (encode-oid oid)
   (encode-der #x01 #(#xff))  ; BOOLEAN TRUE (critical)
   (encode-octet-string value-bytes)))

(defun encode-extension (oid value-bytes)
  "Encode a non-critical X.509 extension."
  (encode-sequence
   (encode-oid oid)
   (encode-octet-string value-bytes)))

;;; ----------------------------------------------------------------------------
;;; RSA Key Encoding
;;; ----------------------------------------------------------------------------

(defun get-rsa-public-key-bytes (private-key public-key)
  "Extract RSA public key components (n, e) from key pair.
   Uses the actual public exponent from the key (ironclad uses random e, not 65537)."
  (let* ((priv-data (ironclad:destructure-private-key private-key))
         (pub-data (ironclad:destructure-public-key public-key))
         (n (getf priv-data :n))
         ;; Get E from public key - ironclad generates random e, so it can be large
         (e (getf pub-data :e)))
    (acme-log "~&[ACME] RSA modulus N bit-length: ~A~%" (integer-length n))
    (acme-log "~&[ACME] RSA exponent E bit-length: ~A~%" (integer-length e))
    (force-output)
    (values n e)))

(defun encode-rsa-public-key (private-key public-key)
  "Encode RSA public key in SubjectPublicKeyInfo format."
  (multiple-value-bind (n e)
      (get-rsa-public-key-bytes private-key public-key)
    (let* ((n-bytes (integer-to-bytes n))
           (e-bytes (integer-to-bytes e))
           ;; RSAPublicKey ::= SEQUENCE { modulus INTEGER, publicExponent INTEGER }
           (rsa-public-key (encode-sequence
                            (encode-integer-bytes n-bytes)
                            (encode-integer-bytes e-bytes)))
           ;; AlgorithmIdentifier for RSA
           (algorithm-id (encode-sequence
                          (encode-oid *oid-rsa-encryption*)
                          (encode-null))))
      ;; SubjectPublicKeyInfo ::= SEQUENCE { algorithm, subjectPublicKey BIT STRING }
      (encode-sequence algorithm-id
                       (encode-bit-string rsa-public-key)))))

(defun encode-rsa-private-key-der (private-key public-key)
  "Encode RSA private key in PKCS#1 DER format.
   PUBLIC-KEY is needed because ironclad's private key doesn't expose :e."
  (let* ((priv-data (ironclad:destructure-private-key private-key))
         (pub-data (ironclad:destructure-public-key public-key))
         (n (getf priv-data :n))
         ;; Get E from public key - ironclad uses random e
         (e (getf pub-data :e))
         (d (getf priv-data :d))
         (p (getf priv-data :p))
         (q (getf priv-data :q))
         ;; Calculate derived values
         (dp (mod d (1- p)))
         (dq (mod d (1- q)))
         (qinv (mod (ironclad:expt-mod q (- p 2) p) p)))
    ;; RSAPrivateKey ::= SEQUENCE { version, n, e, d, p, q, dp, dq, qinv }
    (encode-sequence
     (encode-integer 0)  ; version
     (encode-integer-bytes (integer-to-bytes n))
     (encode-integer-bytes (integer-to-bytes e))
     (encode-integer-bytes (integer-to-bytes d))
     (encode-integer-bytes (integer-to-bytes p))
     (encode-integer-bytes (integer-to-bytes q))
     (encode-integer-bytes (integer-to-bytes dp))
     (encode-integer-bytes (integer-to-bytes dq))
     (encode-integer-bytes (integer-to-bytes qinv)))))

;;; ----------------------------------------------------------------------------
;;; EC Key Encoding
;;; ----------------------------------------------------------------------------

(defun encode-ec-public-key (public-key)
  "Encode EC public key in SubjectPublicKeyInfo format."
  (let* ((key-data (ironclad:destructure-public-key public-key))
         (public-point (getf key-data :y)))  ; Uncompressed point: 04 || X || Y
    ;; SubjectPublicKeyInfo ::= SEQUENCE {
    ;;   algorithm AlgorithmIdentifier,
    ;;   subjectPublicKey BIT STRING
    ;; }
    ;; AlgorithmIdentifier for EC: SEQUENCE { ecPublicKey OID, namedCurve OID }
    (encode-sequence
     (encode-sequence
      (encode-oid *oid-ec-public-key*)
      (encode-oid *oid-secp256r1*))
     (encode-bit-string public-point))))

(defun encode-ecdsa-signature (raw-signature)
  "Convert raw ECDSA signature (r || s) to DER-encoded format.
   ECDSA-Sig-Value ::= SEQUENCE { r INTEGER, s INTEGER }"
  (let* ((half-len (/ (length raw-signature) 2))
         (r-bytes (subseq raw-signature 0 half-len))
         (s-bytes (subseq raw-signature half-len)))
    (encode-sequence
     (encode-integer-bytes r-bytes)
     (encode-integer-bytes s-bytes))))

(defun encode-ec-private-key-pem (private-key)
  "Encode EC private key in SEC1/PEM format for pure-tls."
  (let* ((key-data (ironclad:destructure-private-key private-key))
         (d-bytes (getf key-data :x))      ; Private key scalar
         (public-point (getf key-data :y)) ; Public point
         ;; ECPrivateKey ::= SEQUENCE {
         ;;   version INTEGER { ecPrivkeyVer1(1) },
         ;;   privateKey OCTET STRING,
         ;;   parameters [0] ECParameters OPTIONAL,
         ;;   publicKey [1] BIT STRING OPTIONAL
         ;; }
         (ec-private-key (encode-sequence
                          (encode-integer 1)  ; version
                          (encode-octet-string d-bytes)
                          ;; [0] curve OID
                          (encode-context-tag 0 (encode-oid *oid-secp256r1*))
                          ;; [1] public key
                          (encode-context-tag 1 (encode-bit-string public-point)))))
    (wrap-pem "EC PRIVATE KEY" ec-private-key)))

;;; ----------------------------------------------------------------------------
;;; PEM Encoding
;;; ----------------------------------------------------------------------------

(defun wrap-pem (tag data)
  "Wrap binary data in PEM format with the given tag."
  (let* ((b64 (cl-base64:usb8-array-to-base64-string data))
         (lines (loop for i from 0 below (length b64) by 64
                      collect (subseq b64 i (min (+ i 64) (length b64))))))
    (format nil "-----BEGIN ~A-----~%~{~A~%~}-----END ~A-----~%"
            tag lines tag)))
