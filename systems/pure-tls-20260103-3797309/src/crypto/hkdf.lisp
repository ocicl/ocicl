;;; hkdf.lisp --- HKDF key derivation for TLS 1.3
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements RFC 5869 HKDF (HMAC-based Key Derivation Function)
;;; as used in TLS 1.3 key schedule.

(in-package #:pure-tls)

;;;; HKDF Implementation

(defun hkdf-extract (salt ikm &key (digest :sha256))
  "HKDF-Extract: Extract a pseudorandom key from input keying material.

   SALT - Optional salt value (octet vector). If NIL, uses a string of zeros.
   IKM  - Input keying material (octet vector).
   DIGEST - Hash algorithm to use (default :sha256).

   Returns the pseudorandom key (PRK) as an octet vector."
  (let* ((hash-len (ironclad:digest-length digest))
         (actual-salt (if (or (null salt) (zerop (length salt)))
                          (make-octet-vector hash-len)
                          salt)))
    (let ((hmac (ironclad:make-hmac actual-salt digest)))
      (ironclad:update-mac hmac ikm)
      (ironclad:produce-mac hmac))))

(defun hkdf-expand (prk info length &key (digest :sha256))
  "HKDF-Expand: Expand a pseudorandom key to desired length.

   PRK    - Pseudorandom key from HKDF-Extract (octet vector).
   INFO   - Context/application-specific info (octet vector).
   LENGTH - Desired output length in bytes.
   DIGEST - Hash algorithm to use (default :sha256).

   Returns the output keying material (OKM) as an octet vector."
  (let* ((hash-len (ironclad:digest-length digest))
         (n (ceiling length hash-len))
         (okm (make-octet-vector (* n hash-len)))
         (t-prev (make-octet-vector 0)))
    (when (> n 255)
      (error 'tls-crypto-error
             :operation "HKDF-Expand"
             :message "Output length too large"))
    (dotimes (i n)
      (let ((mac (ironclad:make-hmac prk digest)))
        (ironclad:update-mac mac t-prev)
        (ironclad:update-mac mac info)
        (ironclad:update-mac mac (octet-vector (1+ i)))
        (setf t-prev (ironclad:produce-mac mac))
        (replace okm t-prev :start1 (* i hash-len))))
    (subseq okm 0 length)))

(defun hkdf-expand-label (secret label context length &key (digest :sha256))
  "TLS 1.3 HKDF-Expand-Label function (RFC 8446 Section 7.1).

   SECRET  - The secret to expand (octet vector).
   LABEL   - Label string (without 'tls13 ' prefix).
   CONTEXT - Context data (octet vector, can be empty).
   LENGTH  - Desired output length in bytes.
   DIGEST  - Hash algorithm to use (default :sha256).

   The HkdfLabel structure is:
     struct {
       uint16 length;
       opaque label<7..255>;
       opaque context<0..255>;
     } HkdfLabel;

   Returns the derived key material as an octet vector."
  (let* ((full-label (string-to-octets (concatenate 'string "tls13 " label)))
         (label-len (length full-label))
         (context-len (length context))
         ;; Build HkdfLabel structure
         (hkdf-label (make-octet-vector (+ 2 1 label-len 1 context-len))))
    ;; Length (2 bytes)
    (setf (aref hkdf-label 0) (ldb (byte 8 8) length))
    (setf (aref hkdf-label 1) (ldb (byte 8 0) length))
    ;; Label length (1 byte) + label
    (setf (aref hkdf-label 2) label-len)
    (replace hkdf-label full-label :start1 3)
    ;; Context length (1 byte) + context
    (setf (aref hkdf-label (+ 3 label-len)) context-len)
    (when (plusp context-len)
      (replace hkdf-label context :start1 (+ 4 label-len)))
    ;; Expand
    (hkdf-expand secret hkdf-label length :digest digest)))

(defun derive-secret (secret label messages &key (digest :sha256))
  "TLS 1.3 Derive-Secret function (RFC 8446 Section 7.1).

   SECRET   - The current secret (octet vector).
   LABEL    - Label string (e.g., 'derived', 'c hs traffic').
   MESSAGES - Concatenated handshake messages or empty.
   DIGEST   - Hash algorithm to use (default :sha256).

   Derive-Secret(Secret, Label, Messages) =
     HKDF-Expand-Label(Secret, Label, Transcript-Hash(Messages), Hash.length)

   Returns the derived secret as an octet vector."
  (let* ((hash-len (ironclad:digest-length digest))
         (transcript-hash (if (and messages (plusp (length messages)))
                              (ironclad:digest-sequence digest messages)
                              (ironclad:digest-sequence digest (make-octet-vector 0)))))
    (hkdf-expand-label secret label transcript-hash hash-len :digest digest)))

;;;; TLS 1.3 Specific Key Derivation

(defun derive-traffic-keys (traffic-secret cipher-suite)
  "Derive traffic keys from a traffic secret for the given cipher suite.

   Returns (VALUES key iv) where:
   - KEY is the encryption key
   - IV is the initialization vector (implicit nonce)."
  (let* ((digest (cipher-suite-digest cipher-suite))
         (key-len (cipher-suite-key-length cipher-suite))
         (iv-len +aead-nonce-length+)
         (key (hkdf-expand-label traffic-secret "key" #() key-len :digest digest))
         (iv (hkdf-expand-label traffic-secret "iv" #() iv-len :digest digest)))
    (values key iv)))

(defun cipher-suite-digest (cipher-suite)
  "Return the digest algorithm for a cipher suite."
  (case cipher-suite
    ((#.+tls-aes-128-gcm-sha256+
      #.+tls-chacha20-poly1305-sha256+)
     :sha256)
    (#.+tls-aes-256-gcm-sha384+
     :sha384)
    (t :sha256)))

(defun cipher-suite-key-length (cipher-suite)
  "Return the key length in bytes for a cipher suite."
  (case cipher-suite
    (#.+tls-aes-128-gcm-sha256+ 16)
    (#.+tls-aes-256-gcm-sha384+ 32)
    (#.+tls-chacha20-poly1305-sha256+ 32)
    (t 16)))

(defun cipher-suite-hash-length (cipher-suite)
  "Return the hash output length in bytes for a cipher suite."
  (case cipher-suite
    ((#.+tls-aes-128-gcm-sha256+
      #.+tls-chacha20-poly1305-sha256+)
     32)
    (#.+tls-aes-256-gcm-sha384+
     48)
    (t 32)))
