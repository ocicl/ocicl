;;; verify.lisp --- X.509 Certificate Verification
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements X.509 certificate verification including hostname matching.

(in-package #:pure-tls)

;;;; Hostname Verification (RFC 6125 / RFC 2818)

(defun verify-hostname (cert hostname)
  "Verify that HOSTNAME matches the certificate.
   Supports both DNS hostnames and IP address literals.
   Returns T if verification succeeds, signals TLS-VERIFICATION-ERROR otherwise."
  ;; Check if hostname is an IP address literal
  (let ((ip-bytes (parse-ip-address hostname)))
    (when ip-bytes
      ;; IP address connection - must match IP SAN, never match DNS names or CN
      (let ((cert-ips (certificate-ip-addresses cert)))
        (if (some (lambda (cert-ip) (equalp cert-ip ip-bytes)) cert-ips)
            (return-from verify-hostname t)
            (error 'tls-verification-error
                   :hostname hostname
                   :message "IP address does not match any IP SAN entry")))))

  ;; DNS hostname - check Subject Alternative Name extension first
  (let ((san-names (certificate-dns-names cert))
        (san-ips (certificate-ip-addresses cert)))
    (when (or san-names san-ips)
      ;; If SAN is present, only use SAN (ignore CN per RFC 6125)
      (if (some (lambda (san-name)
                  (hostname-matches-p san-name hostname))
                san-names)
          (return-from verify-hostname t)
          (error 'tls-verification-error
                 :hostname hostname
                 :message "Hostname does not match any SAN entry"))))

  ;; Fall back to Common Name if no SAN (deprecated but still supported)
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

;;;; IP Address Parsing

(defun parse-ip-address (string)
  "Parse an IP address string to octet vector.
   Returns 4-byte vector for IPv4, 16-byte vector for IPv6, or NIL if not an IP."
  (or (parse-ipv4-address string)
      (parse-ipv6-address string)))

(defun parse-ipv4-address (string)
  "Parse an IPv4 address string (e.g., '192.168.1.1') to 4-byte octet vector.
   Returns NIL if not a valid IPv4 address."
  (let ((parts (split-ip-string string #\.)))
    (when (= (length parts) 4)
      (let ((bytes (mapcar #'parse-integer-or-nil parts)))
        (when (and (every #'identity bytes)
                   (every (lambda (b) (and (>= b 0) (<= b 255))) bytes))
          (make-array 4 :element-type '(unsigned-byte 8)
                        :initial-contents bytes))))))

(defun parse-ipv6-address (string)
  "Parse an IPv6 address string to 16-byte octet vector.
   Handles full form and :: compression. Returns NIL if not valid IPv6."
  ;; Strip brackets if present (for URL-style [::1])
  (let ((s (if (and (> (length string) 2)
                    (char= (char string 0) #\[)
                    (char= (char string (1- (length string))) #\]))
               (subseq string 1 (1- (length string)))
               string)))
    ;; Quick check: must contain at least one colon
    (unless (find #\: s)
      (return-from parse-ipv6-address nil))
    ;; Handle :: expansion
    (let* ((double-colon-pos (search "::" s))
           (parts (if double-colon-pos
                      (expand-ipv6-double-colon s double-colon-pos)
                      (split-ip-string s #\:))))
      (when (and parts (= (length parts) 8))
        (let ((words (mapcar #'parse-hex-word parts)))
          (when (every #'identity words)
            (let ((result (make-array 16 :element-type '(unsigned-byte 8))))
              (loop for i from 0 below 8
                    for word = (nth i words)
                    do (setf (aref result (* i 2)) (ldb (byte 8 8) word))
                       (setf (aref result (1+ (* i 2))) (ldb (byte 8 0) word)))
              result)))))))

(defun expand-ipv6-double-colon (string pos)
  "Expand :: in IPv6 address to the right number of zero groups."
  (let* ((before (if (zerop pos) nil (split-ip-string (subseq string 0 pos) #\:)))
         (after-start (+ pos 2))
         (after (if (>= after-start (length string))
                    nil
                    (split-ip-string (subseq string after-start) #\:)))
         (zeros-needed (- 8 (length before) (length after))))
    (when (and (>= zeros-needed 1) (<= zeros-needed 7))
      (append before (make-list zeros-needed :initial-element "0") after))))

(defun split-ip-string (string delimiter)
  "Split STRING by DELIMITER character for IP address parsing."
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defun parse-integer-or-nil (string)
  "Parse STRING as integer, returning NIL on failure."
  (handler-case (parse-integer string :junk-allowed nil)
    (error () nil)))

(defun parse-hex-word (string)
  "Parse STRING as 16-bit hex value, returning NIL on failure."
  (when (and (> (length string) 0) (<= (length string) 4))
    (handler-case (parse-integer string :radix 16 :junk-allowed nil)
      (error () nil))))

;;;; DNS Hostname Matching

(defun normalize-hostname-to-ascii (hostname)
  "Normalize HOSTNAME to ASCII form using IDNA punycode encoding.
   Converts internationalized domain names (U-labels) to A-labels.
   Returns the ASCII form, or the original if already ASCII."
  ;; Check if hostname contains any non-ASCII characters
  (if (every (lambda (c) (< (char-code c) 128)) hostname)
      ;; Already ASCII - just downcase
      (string-downcase hostname)
      ;; Contains non-ASCII - convert to punycode using IDNA library
      (handler-case
          (string-downcase (idna:to-ascii hostname))
        (error ()
          ;; On encoding error, fall back to downcased original
          (string-downcase hostname)))))

(defun hostname-matches-p (pattern hostname)
  "Check if HOSTNAME matches PATTERN, supporting wildcards.
   Both names are normalized to ASCII (punycode) form before comparison.
   Returns T if they match, NIL otherwise."
  (let ((pattern (normalize-hostname-to-ascii pattern))
        (hostname (normalize-hostname-to-ascii hostname)))
    (if (and (>= (length pattern) 2)
             (char= (char pattern 0) #\*)
             (char= (char pattern 1) #\.))
        ;; Wildcard pattern
        (wildcard-hostname-matches-p pattern hostname)
        ;; Exact match
        (string= pattern hostname))))

(defun wildcard-hostname-matches-p (pattern hostname)
  "Check if HOSTNAME matches wildcard PATTERN (e.g., *.example.com).
   Per RFC 6125:
   - Wildcard only matches a single label
   - Wildcard must not match public suffixes (e.g., *.com is rejected)
   - Suffix must have at least 2 labels (e.g., *.example.com is OK)"
  ;; Pattern is *.suffix (e.g., *.example.com)
  (let* ((suffix (subseq pattern 1))  ; .example.com
         (suffix-len (length suffix)))
    (and
     ;; Suffix must have at least 2 labels (reject *.com, *.co.uk patterns)
     ;; Count dots in suffix - need at least 2 (e.g., .example.com has 2 dots)
     (>= (count #\. suffix) 2)
     ;; Hostname must be longer than suffix
     (> (length hostname) suffix-len)
     ;; Hostname must end with suffix
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

(defun verify-certificate-chain (chain trusted-roots &optional (now (get-universal-time)) hostname
                                 &key check-revocation (trust-anchor-mode :replace))
  "Verify a certificate chain against trusted roots.
   CHAIN is a list of certificates, leaf first.
   TRUSTED-ROOTS is a list of trusted CA certificates. When NIL on Windows/macOS,
   native OS verification uses the system trust store.
   HOSTNAME is optional; if provided, enables hostname verification on native platforms.
   CHECK-REVOCATION if T, checks certificate revocation via CRL/OCSP (default NIL).
   TRUST-ANCHOR-MODE controls how trusted-roots interact with system store:
     :replace (default) - Use ONLY trusted-roots, ignore system store
     :extend - Use trusted-roots IN ADDITION TO system store
   Returns T if verification succeeds, signals an error otherwise.
   CRL fetch timeout is computed from cl-cancel:*current-cancel-context* if set."
  (declare (ignorable hostname check-revocation trust-anchor-mode))  ; Only used conditionally
  (when (null chain)
    (error 'tls-certificate-error :message "Empty certificate chain"))

  ;; On Windows with CryptoAPI enabled, use Windows verification
  #+windows
  (when *use-windows-certificate-store*
    ;; Windows CryptoAPI verification
    ;; Hostname verification is optional (nil = no hostname check, useful for mTLS)
    (verify-certificate-chain-native chain hostname
                                     :check-revocation check-revocation
                                     :trusted-roots trusted-roots
                                     :trust-anchor-mode trust-anchor-mode)
    (return-from verify-certificate-chain t))

  ;; On macOS with Keychain enabled, use macOS verification
  #+(or darwin macos)
  (when *use-macos-keychain*
    ;; macOS Security.framework verification
    ;; Hostname verification is optional (nil = no hostname check, useful for mTLS)
    (verify-certificate-chain-native chain hostname
                                     :check-revocation check-revocation
                                     :trusted-roots trusted-roots
                                     :trust-anchor-mode trust-anchor-mode)
    (return-from verify-certificate-chain t))

  ;; Pure Lisp verification - combine roots based on trust-anchor-mode
  (let ((effective-roots
          (ecase trust-anchor-mode
            (:replace
             ;; Use only the provided trusted-roots
             trusted-roots)
            (:extend
             ;; Combine provided roots with system store
             (let ((system-store (load-system-trust-store)))
               (if trusted-roots
                   (append trusted-roots
                           (when system-store (trust-store-certificates system-store)))
                   (when system-store (trust-store-certificates system-store))))))))
    (unless effective-roots
      (error 'tls-verification-error
             :message "No trusted root certificates available for verification"
             :reason :unknown-ca))
    (setf trusted-roots effective-roots))

  ;; Verify each certificate's dates and check for unknown critical extensions
  (dolist (cert chain)
    (verify-certificate-dates cert now)
    ;; RFC 5280: MUST reject certificates with unrecognized critical extensions
    (let ((unknown-critical (certificate-has-unknown-critical-extensions-p cert)))
      (when unknown-critical
        (error 'tls-certificate-error
               :message (format nil "Certificate has unknown critical extension(s): ~A"
                               unknown-critical)))))

  ;; Verify the chain links (name matching, CA constraints, key usage, signatures)
  (loop for i from 0 below (1- (length chain))
        for cert = (nth i chain)
        for issuer = (nth (1+ i) chain)
        for certs-below = i  ; Number of certificates below issuer in chain
        do
           ;; Check issuer name matches
           (unless (certificate-issued-by-p cert issuer)
             (error 'tls-certificate-error
                    :message "Certificate chain is broken (issuer mismatch)"))

           ;; RFC 5280 4.2.1.9: Issuer MUST have BasicConstraints cA=true
           (unless (certificate-is-ca-p issuer)
             (error 'tls-certificate-error
                    :message "Issuing certificate is not a CA (BasicConstraints cA=false or missing)"))

           ;; RFC 5280 4.2.1.3: If KeyUsage present, issuer MUST have keyCertSign
           (let ((issuer-key-usage (certificate-key-usage issuer)))
             (when (and issuer-key-usage
                        (not (member :key-cert-sign issuer-key-usage)))
               (error 'tls-certificate-error
                      :message "Issuing certificate lacks keyCertSign key usage")))

           ;; RFC 5280 4.2.1.9: Check path length constraint
           ;; pathLenConstraint limits the number of CA certificates that may follow
           (let ((path-len (certificate-path-length-constraint issuer)))
             (when (and path-len (> certs-below path-len))
               (error 'tls-certificate-error
                      :message (format nil "Path length constraint violated: ~D certs below but limit is ~D"
                                      certs-below path-len))))

           ;; Verify the cryptographic signature
           (unless (verify-certificate-signature cert issuer)
             (error 'tls-certificate-error
                    :message "Certificate signature verification failed in chain"))

           ;; Check CRL revocation if requested (with signature verification)
           (when check-revocation
             (let ((status (check-certificate-revocation cert :issuer-cert issuer)))
               (when (eql status :revoked)
                 (error 'tls-certificate-error
                        :message (format nil "Certificate has been revoked (serial: ~X)"
                                        (x509-certificate-serial-number cert)))))))
  ;; Check if chain is anchored in trusted roots.
  (let* ((root (first (last chain)))
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
;; Note: SHA-1 intentionally excluded - it is cryptographically broken for certificates
(defparameter *digest-info-prefixes*
  '((:sha256 . #(#x30 #x31 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03 #x04 #x02 #x01 #x05 #x00 #x04 #x20))
    (:sha384 . #(#x30 #x41 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03 #x04 #x02 #x02 #x05 #x00 #x04 #x30))
    (:sha512 . #(#x30 #x51 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03 #x04 #x02 #x03 #x05 #x00 #x04 #x40))))

(defun verify-rsa-pkcs1v15-signature (public-key tbs signature hash-algo)
  "Verify an RSA PKCS#1 v1.5 signature per RFC 8017.
   PUBLIC-KEY is an Ironclad RSA public key.
   TBS is the to-be-signed data (raw bytes).
   SIGNATURE is the signature bytes.
   HASH-ALGO is the hash algorithm keyword (:sha256, :sha384, :sha512).

   Note: This implements PKCS#1 v1.5 verification using Ironclad's RSA
   primitives (expt-mod) for the core modular exponentiation. Ironclad's
   verify-signature does not support PKCS#1 v1.5 mode (only raw RSA and
   RSA-PSS), so custom padding verification is required.

   Security: SHA-1 is rejected as cryptographically broken. Padding is
   validated per RFC 8017 with minimum 8 bytes of 0xFF, and DigestInfo
   length must exactly match (no trailing garbage allowed)."
  ;; Reject SHA-1 - it's cryptographically broken for certificate signatures
  (when (eql hash-algo :sha1)
    (return-from verify-rsa-pkcs1v15-signature nil))
  (let* ((n (ironclad:rsa-key-modulus public-key))
         (e (ironclad:rsa-key-exponent public-key))
         (k (ceiling (integer-length n) 8))  ; Key length in bytes
         (s (ironclad:octets-to-integer signature))
         ;; RSA public operation: m = s^e mod n
         (m (ironclad:expt-mod s e n))
         (em (ironclad:integer-to-octets m :n-bits (* 8 k))))
    ;; Check PKCS#1 v1.5 padding: 0x00 0x01 [0xFF padding] 0x00 [DigestInfo]
    ;; Minimum size: 2 (header) + 8 (min padding) + 1 (separator) + DigestInfo
    (when (< (length em) 11)
      (return-from verify-rsa-pkcs1v15-signature nil))
    (unless (and (zerop (aref em 0))
                 (= 1 (aref em 1)))
      (return-from verify-rsa-pkcs1v15-signature nil))
    ;; Find the 0x00 separator after padding, counting 0xFF bytes
    (let ((separator-pos nil)
          (padding-count 0))
      (loop for i from 2 below (length em)
            do (cond
                 ((= (aref em i) #xff)
                  (incf padding-count)) ; Count padding bytes
                 ((zerop (aref em i))
                  (setf separator-pos i)
                  (return))
                 (t (return-from verify-rsa-pkcs1v15-signature nil))))
      (unless separator-pos
        (return-from verify-rsa-pkcs1v15-signature nil))
      ;; RFC 8017: Require at least 8 bytes of 0xFF padding
      (when (< padding-count 8)
        (return-from verify-rsa-pkcs1v15-signature nil))
      ;; Extract DigestInfo (everything after the 0x00 separator)
      (let* ((digest-info (subseq em (1+ separator-pos)))
             (prefix (rest (assoc hash-algo *digest-info-prefixes*)))
             (hash-len (ironclad:digest-length hash-algo)))
        (unless prefix
          ;; Unknown/unsupported hash algorithm
          (return-from verify-rsa-pkcs1v15-signature nil))
        ;; Check DigestInfo has EXACT correct length (not >=, to prevent trailing garbage)
        (unless (= (length digest-info) (+ (length prefix) hash-len))
          (return-from verify-rsa-pkcs1v15-signature nil))
        ;; Check DigestInfo has correct prefix
        (unless (equalp (subseq digest-info 0 (length prefix)) prefix)
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
          ;; Reject SHA-1 signatures - cryptographically broken
          ((member algorithm '(:sha1-with-rsa-encryption
                               :sha1WithRSAEncryption))
           (error 'tls-certificate-error
                  :message "SHA-1 certificate signatures are rejected (cryptographically broken)"))
          ;; RSA PKCS#1 v1.5 signatures (SHA-256, SHA-384, SHA-512 only)
          ((member algorithm '(:sha256-with-rsa-encryption
                               :sha256WithRSAEncryption
                               :sha384-with-rsa-encryption
                               :sha384WithRSAEncryption
                               :sha512-with-rsa-encryption
                               :sha512WithRSAEncryption
                               :rsa-pkcs1-sha256
                               :rsa-pkcs1-sha384
                               :rsa-pkcs1-sha512))
           (let* ((hash-algo (cert-sig-algorithm-to-hash algorithm))
                  (public-key (parse-rsa-public-key public-key-bytes)))
             (verify-rsa-pkcs1v15-signature public-key tbs signature hash-algo)))
          ;; RSA-PSS - use parameters from certificate
          ((member algorithm '(:rsa-pss :rsassa-pss))
           (let* ((params (x509-certificate-signature-algorithm-params cert))
                  (hash-algo (or (getf params :hash) :sha256))
                  (salt-length (or (getf params :salt-length) 32)))
             ;; Reject SHA-1 for RSA-PSS (cryptographically broken)
             (when (eql hash-algo :sha1)
               (error 'tls-certificate-error
                      :message "SHA-1 is not supported for RSA-PSS signatures (cryptographically broken)"))
             (let ((public-key (parse-rsa-public-key public-key-bytes)))
               ;; Ironclad's PSS verification uses hash algorithm and salt length
               (ironclad:verify-signature public-key
                                          (ironclad:digest-sequence hash-algo tbs)
                                          signature
                                          :pss hash-algo
                                          :salt-length salt-length))))
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
          ;; Ed448
          ((member algorithm '(:ed448))
           (let ((public-key (ironclad:make-public-key :ed448 :y public-key-bytes)))
             (ironclad:verify-signature public-key tbs signature)))
          ;; ML-DSA-65 post-quantum signatures
          ((member algorithm '(:mldsa65))
           (ml-dsa-65-verify public-key-bytes tbs signature))
          (t
           (warn "Unknown certificate signature algorithm: ~A" algorithm)
           ;; Return NIL for unknown algorithms to indicate verification failure
           nil))
      (error (e)
        (error 'tls-certificate-error
               :message (format nil "Certificate signature verification failed: ~A" e))))))

(defun cert-sig-algorithm-to-hash (algorithm)
  "Map certificate signature algorithm to hash algorithm keyword.
   SHA-1 is not supported - it is cryptographically broken for certificates."
  (cond
    ;; SHA-1 explicitly rejected
    ((member algorithm '(:sha1-with-rsa-encryption
                         :sha1WithRSAEncryption
                         :ecdsa-with-sha1
                         :ecdsa-with-SHA1))
     (error 'tls-certificate-error
            :message "SHA-1 signatures are not supported (cryptographically broken)"))
    ;; SHA-256
    ((member algorithm '(:sha256-with-rsa-encryption
                         :sha256WithRSAEncryption
                         :rsa-pkcs1-sha256
                         :ecdsa-with-sha256
                         :ecdsa-with-SHA256)) :sha256)
    ;; SHA-384
    ((member algorithm '(:sha384-with-rsa-encryption
                         :sha384WithRSAEncryption
                         :rsa-pkcs1-sha384
                         :ecdsa-with-sha384
                         :ecdsa-with-SHA384)) :sha384)
    ;; SHA-512
    ((member algorithm '(:sha512-with-rsa-encryption
                         :sha512WithRSAEncryption
                         :rsa-pkcs1-sha512
                         :ecdsa-with-sha512
                         :ecdsa-with-SHA512)) :sha512)
    ;; Unknown algorithm - default to SHA-256 for best compatibility
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

(defun verify-certificate-chain-native (chain hostname &key check-revocation trusted-roots
                                                           (trust-anchor-mode :replace))
  "Attempt native certificate chain verification.
Returns T if verification succeeded, NIL if native verification not available,
or signals an error on verification failure.
CHECK-REVOCATION if T, enables OCSP/CRL revocation checking on supported platforms.
TRUSTED-ROOTS if provided, is a list of x509-certificate objects to use as trust anchors.
TRUST-ANCHOR-MODE controls how trusted-roots interact with system store:
  :replace - Use ONLY trusted-roots (not supported on Windows, will error)
  :extend - Use trusted-roots IN ADDITION TO system store"
  (declare (ignorable chain hostname check-revocation trusted-roots trust-anchor-mode))
  (let ((trusted-roots-der (when trusted-roots
                             (mapcar #'x509-certificate-raw-der trusted-roots))))
    #+windows
    (when *use-windows-certificate-store*
      (verify-certificate-chain-windows
       (mapcar #'x509-certificate-raw-der chain)
       hostname
       :check-revocation check-revocation
       :trusted-roots trusted-roots-der
       :trust-anchor-mode trust-anchor-mode)
      (return-from verify-certificate-chain-native t))
    #+(or darwin macos)
    (when *use-macos-keychain*
      (verify-certificate-chain-macos
       (mapcar #'x509-certificate-raw-der chain)
       hostname
       :check-revocation check-revocation
       :trusted-roots trusted-roots-der
       :trust-anchor-mode trust-anchor-mode)
      (return-from verify-certificate-chain-native t)))
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
