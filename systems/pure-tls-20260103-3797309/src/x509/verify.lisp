;;; verify.lisp --- X.509 Certificate Verification
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements X.509 certificate verification including hostname matching.

(in-package #:pure-tls)

;;;; Hostname Verification (RFC 6125)

(defun verify-hostname (cert hostname)
  "Verify that HOSTNAME matches the certificate.
   Returns T if verification succeeds, signals TLS-VERIFICATION-ERROR otherwise."
  ;; First check Subject Alternative Name extension
  (let ((san-names (certificate-dns-names cert)))
    (when san-names
      ;; If SAN is present, only use SAN (ignore CN)
      (if (some (lambda (san-name)
                  (hostname-matches-p san-name hostname))
                san-names)
          (return-from verify-hostname t)
          (error 'tls-verification-error
                 :hostname hostname
                 :message "Hostname does not match any SAN entry"))))
  ;; Fall back to Common Name if no SAN
  (let ((cns (certificate-subject-common-names cert)))
    (when cns
      (if (some (lambda (cn)
                  (hostname-matches-p cn hostname))
                cns)
          (return-from verify-hostname t)
          (error 'tls-verification-error
                 :hostname hostname
                 :message "Hostname does not match certificate CN"))))
  ;; No SAN or CN to check
  (error 'tls-verification-error
         :hostname hostname
         :message "Certificate has no DNS names to verify"))

(defun hostname-matches-p (pattern hostname)
  "Check if HOSTNAME matches PATTERN, supporting wildcards.
   Returns T if they match, NIL otherwise."
  (let ((pattern (string-downcase pattern))
        (hostname (string-downcase hostname)))
    (if (and (>= (length pattern) 2)
             (char= (char pattern 0) #\*)
             (char= (char pattern 1) #\.))
        ;; Wildcard pattern
        (wildcard-hostname-matches-p pattern hostname)
        ;; Exact match
        (string= pattern hostname))))

(defun wildcard-hostname-matches-p (pattern hostname)
  "Check if HOSTNAME matches wildcard PATTERN (e.g., *.example.com).
   Per RFC 6125, wildcard only matches a single label."
  ;; Pattern is *.suffix
  (let* ((suffix (subseq pattern 1))  ; .example.com
         (suffix-len (length suffix)))
    ;; Hostname must end with suffix
    (and (> (length hostname) suffix-len)
         (string= suffix (subseq hostname (- (length hostname) suffix-len)))
         ;; The part before suffix must be a single label (no dots)
         (not (find #\. hostname :end (- (length hostname) suffix-len))))))

;;;; Certificate Validity

(defun verify-certificate-dates (cert &optional (now (get-universal-time)))
  "Verify that the certificate is valid at time NOW.
   Signals TLS-CERTIFICATE-EXPIRED or TLS-CERTIFICATE-NOT-YET-VALID on failure."
  (let ((not-before (certificate-not-before cert))
        (not-after (certificate-not-after cert)))
    (when (and not-before (< now not-before))
      (error 'tls-certificate-not-yet-valid
             :not-before not-before
             :message "Certificate is not yet valid"))
    (when (and not-after (> now not-after))
      (error 'tls-certificate-expired
             :not-after not-after
             :message "Certificate has expired"))
    t))

;;;; Certificate Chain Verification
;;;
;;; Note: Full chain verification requires:
;;; 1. Building the chain from leaf to root
;;; 2. Verifying each signature
;;; 3. Checking basic constraints
;;; 4. Checking key usage
;;; 5. Checking against trusted roots
;;;
;;; For now, we provide basic building blocks.

(defun verify-certificate-chain (chain trusted-roots &optional (now (get-universal-time)) hostname)
  "Verify a certificate chain against trusted roots.
   CHAIN is a list of certificates, leaf first.
   TRUSTED-ROOTS is a list of trusted CA certificates (may be NIL on Windows/macOS with native verification).
   HOSTNAME is optional; if provided on Windows/macOS, enables native verification.
   Returns T if verification succeeds, signals an error otherwise."
  (declare (ignorable hostname))  ; Only used with native verification
  (when (null chain)
    (error 'tls-certificate-error :message "Empty certificate chain"))

  ;; On Windows with CryptoAPI enabled and hostname provided, use Windows verification
  #+windows
  (when (and hostname *use-windows-certificate-store*)
    ;; Windows CryptoAPI verification is authoritative when enabled
    (verify-certificate-chain-native chain hostname)
    (return-from verify-certificate-chain t))

  ;; On macOS with Keychain enabled and hostname provided, use macOS verification
  #+(or darwin macos)
  (when (and hostname *use-macos-keychain*)
    ;; macOS Security.framework verification is authoritative when enabled
    (verify-certificate-chain-native chain hostname)
    (return-from verify-certificate-chain t))

  ;; Pure Lisp verification requires trusted-roots
  (unless trusted-roots
    (error 'tls-verification-error
           :message "No trusted root certificates available for verification"
           :reason :unknown-ca))
  ;; Verify each certificate's dates
  (dolist (cert chain)
    (verify-certificate-dates cert now))
  ;; Verify the chain links (both name matching AND signature verification)
  (loop for i from 0 below (1- (length chain))
        for cert = (nth i chain)
        for issuer = (nth (1+ i) chain)
        do (unless (certificate-issued-by-p cert issuer)
             (error 'tls-certificate-error
                    :message "Certificate chain is broken (issuer mismatch)"))
           ;; Verify the cryptographic signature
           (unless (verify-certificate-signature cert issuer)
             (error 'tls-certificate-error
                    :message "Certificate signature verification failed in chain")))
  ;; Check if chain is anchored in trusted roots.
  (let* ((root (car (last chain)))
         (anchored (or (find-if (lambda (trusted)
                                  (find-if (lambda (cert)
                                             (or (certificate-equal-p cert trusted)
                                                 (certificate-issued-by-p cert trusted)))
                                           chain))
                                trusted-roots)
                       (find-if (lambda (trusted)
                                  (or (certificate-equal-p root trusted)
                                      (certificate-issued-by-p root trusted)))
                                trusted-roots))))
    (unless anchored
      (let ((debug (get-environment-variable "OCICL_TLS_DEBUG")))
        (when (and debug (string/= debug ""))
          (format *error-output* "; pure-tls: chain length=~D trusted-roots=~D~%"
                  (length chain) (length trusted-roots))
          (loop for cert in chain
                for idx from 0
                do (format *error-output*
                           "; pure-tls: chain[~D] subject-cns=~A issuer-cns=~A~%"
                           idx
                           (certificate-subject-common-names cert)
                           (certificate-issuer-common-names cert)))))
      (error 'tls-verification-error
             :message "Certificate chain not anchored in trusted root"
             :reason :unknown-ca))
    ;; If root is not self-signed and directly matches a trusted issuer, verify it.
    (when (and anchored
               (certificate-issued-by-p root anchored)
               (not (certificate-equal-p root anchored)))
      (unless (verify-certificate-signature root anchored)
        (error 'tls-certificate-error
               :message "Root certificate signature verification failed"))))
  t)

(defun certificate-issued-by-p (cert issuer-cert)
  "Check if CERT was issued by ISSUER-CERT (by comparing names)."
  (equal (x509-name-rdns (x509-certificate-issuer cert))
         (x509-name-rdns (x509-certificate-subject issuer-cert))))

(defun certificate-equal-p (cert1 cert2)
  "Check if two certificates are the same (by comparing DER)."
  (equalp (x509-certificate-raw-der cert1)
          (x509-certificate-raw-der cert2)))

;;;; Signature Verification

;; DigestInfo prefixes for PKCS#1 v1.5 (DER-encoded AlgorithmIdentifier + NULL params)
(defparameter *digest-info-prefixes*
  '((:sha256 . #(#x30 #x31 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03 #x04 #x02 #x01 #x05 #x00 #x04 #x20))
    (:sha384 . #(#x30 #x41 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03 #x04 #x02 #x02 #x05 #x00 #x04 #x30))
    (:sha512 . #(#x30 #x51 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03 #x04 #x02 #x03 #x05 #x00 #x04 #x40))
    (:sha1   . #(#x30 #x21 #x30 #x09 #x06 #x05 #x2b #x0e #x03 #x02 #x1a #x05 #x00 #x04 #x14))))

(defun verify-rsa-pkcs1v15-signature (public-key tbs signature hash-algo)
  "Verify an RSA PKCS#1 v1.5 signature.
   PUBLIC-KEY is an Ironclad RSA public key.
   TBS is the to-be-signed data (raw bytes).
   SIGNATURE is the signature bytes.
   HASH-ALGO is the hash algorithm keyword (:sha256, :sha384, :sha512)."
  (let* ((n (ironclad:rsa-key-modulus public-key))
         (e (ironclad:rsa-key-exponent public-key))
         (k (ceiling (integer-length n) 8))  ; Key length in bytes
         (s (ironclad:octets-to-integer signature))
         ;; RSA public operation: m = s^e mod n
         (m (ironclad:expt-mod s e n))
         (em (ironclad:integer-to-octets m :n-bits (* 8 k))))
    ;; Check PKCS#1 v1.5 padding: 0x00 0x01 [0xFF padding] 0x00 [DigestInfo]
    (when (< (length em) 11)
      (return-from verify-rsa-pkcs1v15-signature nil))
    (unless (and (zerop (aref em 0))
                 (= 1 (aref em 1)))
      (return-from verify-rsa-pkcs1v15-signature nil))
    ;; Find the 0x00 separator after padding
    (let ((separator-pos nil))
      (loop for i from 2 below (length em)
            do (cond
                 ((= (aref em i) #xff)) ; Padding byte, continue
                 ((zerop (aref em i))
                  (setf separator-pos i)
                  (return))
                 (t (return-from verify-rsa-pkcs1v15-signature nil))))
      (unless separator-pos
        (return-from verify-rsa-pkcs1v15-signature nil))
      ;; Extract DigestInfo (everything after the 0x00 separator)
      (let* ((digest-info (subseq em (1+ separator-pos)))
             (prefix (cdr (assoc hash-algo *digest-info-prefixes*)))
             (hash-len (ironclad:digest-length hash-algo)))
        (unless prefix
          (error "Unknown hash algorithm: ~A" hash-algo))
        ;; Check DigestInfo has correct prefix and length
        (unless (and (>= (length digest-info) (+ (length prefix) hash-len))
                     (equalp (subseq digest-info 0 (length prefix)) prefix))
          (return-from verify-rsa-pkcs1v15-signature nil))
        ;; Extract the hash from DigestInfo and compare with our computed hash
        (let ((embedded-hash (subseq digest-info (length prefix)))
              (computed-hash (ironclad:digest-sequence hash-algo tbs)))
          (equalp embedded-hash computed-hash))))))

(defun verify-certificate-signature (cert issuer-cert)
  "Verify that CERT's signature was made by ISSUER-CERT's key.
   Returns T on success, signals TLS-CERTIFICATE-ERROR on failure."
  (let* ((tbs (x509-certificate-tbs-raw cert))
         (signature (x509-certificate-signature cert))
         (algorithm (x509-certificate-signature-algorithm cert))
         (public-key-info (x509-certificate-subject-public-key-info issuer-cert))
         (key-algorithm (getf public-key-info :algorithm))
         (public-key-bytes (getf public-key-info :public-key)))
    (handler-case
        (cond
          ;; RSA PKCS#1 v1.5 signatures
          ((member algorithm '(:sha256-with-rsa-encryption
                               :sha256WithRSAEncryption
                               :sha384-with-rsa-encryption
                               :sha384WithRSAEncryption
                               :sha512-with-rsa-encryption
                               :sha512WithRSAEncryption
                               :sha1-with-rsa-encryption
                               :sha1WithRSAEncryption
                               :rsa-pkcs1-sha256
                               :rsa-pkcs1-sha384
                               :rsa-pkcs1-sha512))
           (let* ((hash-algo (cert-sig-algorithm-to-hash algorithm))
                  (public-key (parse-rsa-public-key public-key-bytes)))
             (verify-rsa-pkcs1v15-signature public-key tbs signature hash-algo)))
          ;; RSA-PSS
          ((member algorithm '(:rsa-pss :rsassa-pss))
           ;; RSA-PSS needs to extract hash from signature parameters
           ;; For now, default to SHA-256
           (let ((public-key (parse-rsa-public-key public-key-bytes)))
             (ironclad:verify-signature public-key tbs signature
                                        :pss :sha256)))
          ;; ECDSA signatures
          ((member algorithm '(:ecdsa-with-sha256
                               :ecdsa-with-SHA256
                               :ecdsa-with-sha384
                               :ecdsa-with-SHA384
                               :ecdsa-with-sha512
                               :ecdsa-with-SHA512))
           (let* ((hash-algo (cert-sig-algorithm-to-hash algorithm))
                  (hash (ironclad:digest-sequence hash-algo tbs))
                  (parsed-sig (parse-ecdsa-cert-signature signature))
                  (r (getf parsed-sig :r))
                  (s (getf parsed-sig :s))
                  (curve (cond
                           ((member key-algorithm '(:ecdsa-p256 :secp256r1 :prime256v1
                                                    :ec-public-key)) :secp256r1)
                           ((member key-algorithm '(:ecdsa-p384 :secp384r1)) :secp384r1)
                           ((member key-algorithm '(:ecdsa-p521 :secp521r1)) :secp521r1)
                           (t :secp256r1)))
                  (coord-size (ecase curve
                                (:secp256r1 32)
                                (:secp384r1 48)
                                (:secp521r1 66)))
                  (r-bytes (ironclad:integer-to-octets r :n-bits (* 8 coord-size)))
                  (s-bytes (ironclad:integer-to-octets s :n-bits (* 8 coord-size)))
                  (public-key (make-ecdsa-public-key-from-der key-algorithm public-key-bytes)))
             (when public-key
               (ironclad:verify-signature public-key hash
                                          (ironclad:make-signature curve :r r-bytes :s s-bytes)))))
          ;; Ed25519
          ((member algorithm '(:ed25519))
           (let ((public-key (ironclad:make-public-key :ed25519 :y public-key-bytes)))
             (ironclad:verify-signature public-key tbs signature)))
          (t
           (warn "Unknown certificate signature algorithm: ~A" algorithm)
           ;; Return NIL for unknown algorithms to indicate verification failure
           nil))
      (error (e)
        (error 'tls-certificate-error
               :message (format nil "Certificate signature verification failed: ~A" e))))))

(defun cert-sig-algorithm-to-hash (algorithm)
  "Map certificate signature algorithm to hash algorithm keyword."
  (cond
    ((member algorithm '(:sha1-with-rsa-encryption
                         :sha1WithRSAEncryption)) :sha1)
    ((member algorithm '(:sha256-with-rsa-encryption
                         :sha256WithRSAEncryption
                         :rsa-pkcs1-sha256
                         :ecdsa-with-sha256
                         :ecdsa-with-SHA256)) :sha256)
    ((member algorithm '(:sha384-with-rsa-encryption
                         :sha384WithRSAEncryption
                         :rsa-pkcs1-sha384
                         :ecdsa-with-sha384
                         :ecdsa-with-SHA384)) :sha384)
    ((member algorithm '(:sha512-with-rsa-encryption
                         :sha512WithRSAEncryption
                         :rsa-pkcs1-sha512
                         :ecdsa-with-sha512
                         :ecdsa-with-SHA512)) :sha512)
    (t :sha256)))

(defun parse-rsa-public-key (public-key-der)
  "Parse an RSA public key from DER-encoded bytes."
  (let ((parsed (parse-der public-key-der)))
    (when (asn1-sequence-p parsed)
      (let ((children (asn1-children parsed)))
        (when (>= (length children) 2)
          (ironclad:make-public-key :rsa
                                    :n (asn1-node-value (first children))
                                    :e (asn1-node-value (second children))))))))

(defun parse-ecdsa-cert-signature (signature)
  "Parse a DER-encoded ECDSA signature into r and s values."
  (let ((parsed (parse-der signature)))
    (when (asn1-sequence-p parsed)
      (let ((children (asn1-children parsed)))
        (when (>= (length children) 2)
          (list :r (asn1-node-value (first children))
                :s (asn1-node-value (second children))))))))

(defun make-ecdsa-public-key-from-der (key-algorithm public-key-bytes)
  "Create an ECDSA public key from DER-encoded bytes.
   PUBLIC-KEY-BYTES should be the full encoded point (04 || X || Y)."
  ;; Determine curve from algorithm
  (let ((curve (cond
                 ((member key-algorithm '(:ecdsa-p256 :secp256r1 :prime256v1
                                          :ec-public-key)) :secp256r1)
                 ((member key-algorithm '(:ecdsa-p384 :secp384r1)) :secp384r1)
                 ((member key-algorithm '(:ecdsa-p521 :secp521r1)) :secp521r1)
                 (t :secp256r1))))
    ;; Ironclad's secp256r1/secp384r1/secp521r1 make-public-key expects :y to be
    ;; the full encoded public key bytes (04 || X || Y), not separate coordinates.
    (when (and (plusp (length public-key-bytes))
               (= (aref public-key-bytes 0) 4))  ; Uncompressed point format
      (ironclad:make-public-key curve :y public-key-bytes))))

;;;; Platform-specific verification

#+windows
(defvar *use-windows-certificate-store* t
  "When T on Windows, use Windows CryptoAPI for certificate chain verification.
This uses the system's trusted root certificates and respects enterprise PKI
policies. Set to NIL to use pure Lisp verification instead.")

#-windows
(defvar *use-windows-certificate-store* nil
  "Always NIL on non-Windows platforms.")

#+(or darwin macos)
(defvar *use-macos-keychain* t
  "When T on macOS, use Security.framework for certificate chain verification.
This uses the system Keychain trusted roots and respects enterprise PKI
policies. Set to NIL to use pure Lisp verification instead.")

#-(or darwin macos)
(defvar *use-macos-keychain* nil
  "Always NIL on non-macOS platforms.")

(defun verify-certificate-chain-native (chain hostname)
  "Attempt native certificate chain verification.
Returns T if verification succeeded, NIL if native verification not available,
or signals an error on verification failure."
  (declare (ignorable chain hostname))
  #+windows
  (when *use-windows-certificate-store*
    (verify-certificate-chain-windows
     (mapcar #'x509-certificate-raw-der chain)
     hostname)
    (return-from verify-certificate-chain-native t))
  #+(or darwin macos)
  (when *use-macos-keychain*
    (verify-certificate-chain-macos
     (mapcar #'x509-certificate-raw-der chain)
     hostname)
    (return-from verify-certificate-chain-native t))
  ;; Not available on this platform or disabled
  nil)

;;;; Trust Store

(defstruct trust-store
  "A collection of trusted CA certificates."
  (certificates nil :type list))

(defun make-trust-store-from-directory (path)
  "Load all certificates from a directory into a trust store."
  (let ((certs nil))
    (dolist (file (directory (merge-pathnames "*.pem" path)))
      (handler-case
          (push (parse-certificate-from-file file) certs)
        (error (e)
          (warn "Failed to load certificate ~A: ~A" file e))))
    (dolist (file (directory (merge-pathnames "*.crt" path)))
      (handler-case
          (push (parse-certificate-from-file file) certs)
        (error (e)
          (warn "Failed to load certificate ~A: ~A" file e))))
    (make-trust-store :certificates (nreverse certs))))

(defun trust-store-find-issuer (store cert)
  "Find a certificate in STORE that could have issued CERT."
  (find-if (lambda (ca)
             (certificate-issued-by-p cert ca))
           (trust-store-certificates store)))

;;;; Full Verification Function

(defun verify-peer-certificate (cert hostname &key
                                               verify-mode
                                               trust-store
                                               (check-dates t))
  "Perform full verification of a peer certificate.

   CERT - The certificate to verify.
   HOSTNAME - The hostname to verify against.
   VERIFY-MODE - One of +VERIFY-NONE+, +VERIFY-PEER+, or +VERIFY-REQUIRED+.
   TRUST-STORE - Trust store for chain verification (optional).
   CHECK-DATES - Whether to check validity dates (default T).

   Returns T on success, signals appropriate error on failure."
  ;; Skip if verification disabled
  (when (= verify-mode +verify-none+)
    (return-from verify-peer-certificate t))
  ;; Check dates if requested
  (when check-dates
    (verify-certificate-dates cert))
  ;; Verify hostname
  (when hostname
    (verify-hostname cert hostname))
  ;; Chain verification (if trust store provided)
  (when trust-store
    (let ((issuer (trust-store-find-issuer trust-store cert)))
      (unless issuer
        (when (= verify-mode +verify-required+)
          (error 'tls-verification-error
                 :message "Cannot verify certificate chain"
                 :reason :unknown-ca)))))
  t)
