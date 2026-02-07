;;; crl.lisp --- Certificate Revocation List Support
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements CRL parsing and revocation checking per RFC 5280.

(in-package #:pure-tls)

;;;; CRL Structure
;;;
;;; CertificateList  ::=  SEQUENCE  {
;;;      tbsCertList          TBSCertList,
;;;      signatureAlgorithm   AlgorithmIdentifier,
;;;      signatureValue       BIT STRING  }
;;;
;;; TBSCertList  ::=  SEQUENCE  {
;;;      version                 Version OPTIONAL (v2 if present),
;;;      signature               AlgorithmIdentifier,
;;;      issuer                  Name,
;;;      thisUpdate              Time,
;;;      nextUpdate              Time OPTIONAL,
;;;      revokedCertificates     SEQUENCE OF SEQUENCE  {
;;;           userCertificate         CertificateSerialNumber,
;;;           revocationDate          Time,
;;;           crlEntryExtensions      Extensions OPTIONAL } OPTIONAL,
;;;      crlExtensions           [0] EXPLICIT Extensions OPTIONAL }

(defstruct crl
  "Parsed Certificate Revocation List."
  ;; Raw DER bytes (for signature verification)
  (raw-der nil :type (or null octet-vector))
  (tbs-raw nil :type (or null octet-vector))
  ;; Parsed fields
  (version 1 :type fixnum)  ; 1=v1, 2=v2
  (signature-algorithm nil)
  (issuer nil)  ; x509-name
  (this-update nil)  ; universal-time
  (next-update nil)  ; universal-time or nil
  (revoked-certificates nil :type list)  ; list of crl-entry
  (extensions nil :type list)
  ;; Signature
  (signature nil))

(defstruct crl-entry
  "A single revoked certificate entry."
  (serial-number nil)
  (revocation-date nil)
  (extensions nil :type list))

;;;; CRL Parsing

(defun parse-crl (der-bytes)
  "Parse a DER-encoded CRL."
  (let* ((root (parse-der der-bytes))
         (crl (make-crl :raw-der der-bytes)))
    (unless (asn1-sequence-p root)
      (error 'tls-decode-error :message "CRL must be a SEQUENCE"))
    (let ((children (asn1-children root)))
      (unless (>= (length children) 3)
        (error 'tls-decode-error :message "CRL missing required fields"))
      ;; TBSCertList
      (let ((tbs (first children)))
        (setf (crl-tbs-raw crl) (asn1-node-raw-bytes tbs))
        (parse-tbs-cert-list crl tbs))
      ;; SignatureAlgorithm
      (setf (crl-signature-algorithm crl)
            (parse-algorithm-identifier (second children)))
      ;; SignatureValue (BIT STRING)
      (let ((sig-value (asn1-node-value (third children))))
        (setf (crl-signature crl) (getf sig-value :data))))
    crl))

(defun parse-tbs-cert-list (crl tbs)
  "Parse the TBSCertList portion of a CRL."
  (let ((children (asn1-children tbs))
        (idx 0))
    ;; Version (optional INTEGER, if present must be 1 for v2)
    (when (and children
               (= (asn1-node-class (nth idx children)) +asn1-class-universal+)
               (= (asn1-node-tag (nth idx children)) +asn1-integer+))
      (let ((version (asn1-node-value (nth idx children))))
        (setf (crl-version crl) (1+ version)))  ; 0=v1, 1=v2
      (incf idx))
    ;; Signature (AlgorithmIdentifier) - for inner validation
    (incf idx)  ; Skip - we use outer signature algorithm
    ;; Issuer (Name)
    (setf (crl-issuer crl) (parse-name (nth idx children)))
    (incf idx)
    ;; thisUpdate (Time)
    (setf (crl-this-update crl) (asn1-node-value (nth idx children)))
    (incf idx)
    ;; nextUpdate (Time) - optional
    (when (and (< idx (length children))
               (or (= (asn1-node-tag (nth idx children)) +asn1-utc-time+)
                   (= (asn1-node-tag (nth idx children)) +asn1-generalized-time+)))
      (setf (crl-next-update crl) (asn1-node-value (nth idx children)))
      (incf idx))
    ;; revokedCertificates (SEQUENCE OF) - optional
    (when (and (< idx (length children))
               (asn1-sequence-p (nth idx children))
               (not (asn1-context-p (nth idx children) 0)))
      (setf (crl-revoked-certificates crl)
            (parse-revoked-certificates (nth idx children)))
      (incf idx))
    ;; crlExtensions [0] EXPLICIT - optional
    (when (and (< idx (length children))
               (asn1-context-p (nth idx children) 0))
      ;; We don't parse CRL extensions for now - just skip
      (incf idx))))

(defun parse-revoked-certificates (node)
  "Parse the revokedCertificates SEQUENCE."
  (let ((entries nil))
    (dolist (child (asn1-children node))
      (push (parse-crl-entry child) entries))
    (nreverse entries)))

(defun parse-crl-entry (node)
  "Parse a single revoked certificate entry."
  (let ((children (asn1-children node)))
    (make-crl-entry
     :serial-number (asn1-node-value (first children))
     :revocation-date (asn1-node-value (second children))
     :extensions nil)))  ; We don't parse entry extensions for now

;;;; CRL Checking

(defun crl-contains-serial-p (crl serial-number)
  "Check if a CRL contains a revoked certificate with the given serial number."
  (find serial-number (crl-revoked-certificates crl)
        :key #'crl-entry-serial-number
        :test #'=))

(defun crl-valid-p (crl &optional (now (get-universal-time)))
  "Check if a CRL is currently valid (not expired).
   Returns T if thisUpdate <= now < nextUpdate (or nextUpdate is nil)."
  (and (<= (crl-this-update crl) now)
       (or (null (crl-next-update crl))
           (< now (crl-next-update crl)))))

(defun crl-issuer-matches-p (crl certificate)
  "Check if a CRL was issued by the same entity that issued the certificate.
   Compares CRL issuer with certificate issuer."
  (x509-name-equal-p (crl-issuer crl) (x509-certificate-issuer certificate)))

(defun x509-name-equal-p (name1 name2)
  "Compare two X.509 names for equality.
   Per RFC 5280, name comparison is complex (case-insensitive for some types, etc.)
   This is a simplified comparison that works for most cases."
  (let ((rdns1 (x509-name-rdns name1))
        (rdns2 (x509-name-rdns name2)))
    (and (= (length rdns1) (length rdns2))
         (every (lambda (rdn1 rdn2)
                  (and (eql (first rdn1) (first rdn2))
                       (string-equal (rest rdn1) (rest rdn2))))
                rdns1 rdns2))))

;;;; CRL Signature Verification

(defun verify-crl-signature (crl issuer-cert)
  "Verify that CRL's signature was made by ISSUER-CERT's key.
   Returns T on success, NIL on failure.

   This is critical for security: CRLs are fetched over HTTP, so without
   signature verification an attacker could serve a fake CRL."
  (let* ((tbs (crl-tbs-raw crl))
         (signature (crl-signature crl))
         (algorithm (crl-signature-algorithm crl))
         (public-key-info (x509-certificate-subject-public-key-info issuer-cert))
         (key-algorithm (getf public-key-info :algorithm))
         (public-key-bytes (getf public-key-info :public-key)))
    (handler-case
        (cond
          ;; Reject SHA-1 signatures - cryptographically broken
          ((member algorithm '(:sha1-with-rsa-encryption
                               :sha1WithRSAEncryption))
           (warn "CRL uses SHA-1 signature which is cryptographically broken")
           nil)
          ;; RSA PKCS#1 v1.5 signatures
          ((member algorithm '(:sha256-with-rsa-encryption
                               :sha256WithRSAEncryption
                               :sha384-with-rsa-encryption
                               :sha384WithRSAEncryption
                               :sha512-with-rsa-encryption
                               :sha512WithRSAEncryption
                               :rsa-pkcs1-sha256
                               :rsa-pkcs1-sha384
                               :rsa-pkcs1-sha512))
           (let* ((hash-algo (crl-sig-algorithm-to-hash algorithm))
                  (public-key (parse-rsa-public-key public-key-bytes)))
             (when public-key
               (verify-rsa-pkcs1v15-signature public-key tbs signature hash-algo))))
          ;; RSA-PSS
          ((member algorithm '(:rsa-pss :rsassa-pss))
           ;; For CRLs, we don't have parsed params like certs, use defaults
           (let* ((hash-algo :sha256)
                  (salt-length 32)
                  (public-key (parse-rsa-public-key public-key-bytes)))
             (when public-key
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
           (let* ((hash-algo (crl-sig-algorithm-to-hash algorithm))
                  (hash (ironclad:digest-sequence hash-algo tbs))
                  (parsed-sig (parse-ecdsa-signature signature))
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
                  (public-key (make-ecdsa-public-key key-algorithm public-key-bytes)))
             (when (and public-key r s)
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
           (warn "Unknown CRL signature algorithm: ~A" algorithm)
           nil))
      (error (e)
        (warn "CRL signature verification failed: ~A" e)
        nil))))

(defun crl-sig-algorithm-to-hash (algorithm)
  "Map CRL signature algorithm to hash algorithm keyword."
  (cond
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

(defun parse-ecdsa-signature (signature)
  "Parse a DER-encoded ECDSA signature into r and s values."
  (handler-case
      (let ((parsed (parse-der signature)))
        (when (asn1-sequence-p parsed)
          (let ((children (asn1-children parsed)))
            (when (>= (length children) 2)
              (list :r (asn1-node-value (first children))
                    :s (asn1-node-value (second children)))))))
    (error () nil)))

(defun make-ecdsa-public-key (key-algorithm public-key-bytes)
  "Create an ECDSA public key from encoded bytes."
  (let ((curve (cond
                 ((member key-algorithm '(:ecdsa-p256 :secp256r1 :prime256v1
                                          :ec-public-key)) :secp256r1)
                 ((member key-algorithm '(:ecdsa-p384 :secp384r1)) :secp384r1)
                 ((member key-algorithm '(:ecdsa-p521 :secp521r1)) :secp521r1)
                 (t :secp256r1))))
    (when (and (plusp (length public-key-bytes))
               (= (aref public-key-bytes 0) 4))  ; Uncompressed point format
      (handler-case
          (ironclad:make-public-key curve :y public-key-bytes)
        (error () nil)))))

;;;; CRL Cache

(defvar *crl-cache* (make-hash-table :test 'equal)
  "Cache of fetched CRLs, keyed by URI.")

(defvar *crl-cache-lock* (bt:make-lock "crl-cache-lock")
  "Lock for thread-safe CRL cache access.")

(defun get-cached-crl (uri)
  "Get a CRL from cache if it exists and is still valid."
  (bt:with-lock-held (*crl-cache-lock*)
    (let ((entry (gethash uri *crl-cache*)))
      (when entry
        (let ((crl (first entry))
              (fetch-time (rest entry)))
          ;; Check if CRL is still valid
          (when (crl-valid-p crl)
            crl))))))

(defun cache-crl (uri crl)
  "Store a CRL in the cache."
  (bt:with-lock-held (*crl-cache-lock*)
    (setf (gethash uri *crl-cache*)
          (cons crl (get-universal-time)))))

(defun clear-crl-cache ()
  "Clear all cached CRLs."
  (bt:with-lock-held (*crl-cache-lock*)
    (clrhash *crl-cache*)))

;;;; CRL Fetching - Minimal HTTP Client
;;;
;;; Implements a minimal HTTP/1.1 client using usocket for CRL fetching.
;;; Supports HTTP_PROXY environment variable and basic redirect following.

(defvar *crl-fetch-function* nil
  "Custom function for fetching CRL bytes from a URI.
   Should be (lambda (uri) ...) returning octet vector or NIL.
   Timeout computed from cl-cancel:*current-cancel-context*.
   If NIL, uses built-in HTTP client.")

(defvar *crl-max-redirects* 5
  "Maximum number of HTTP redirects to follow.")

(defvar *crl-max-response-size* (* 10 1024 1024)
  "Maximum CRL response size in bytes (10MB default).")

(defun parse-http-url (url)
  "Parse an HTTP(S) URL into (values scheme host port path).
   Returns NIL if URL is invalid or not HTTP(S)."
  (let ((url (string-trim '(#\Space #\Tab) url)))
    (cond
      ((alexandria:starts-with-subseq "http://" url)
       (parse-http-url-parts (subseq url 7) "http" 80))
      ((alexandria:starts-with-subseq "https://" url)
       (parse-http-url-parts (subseq url 8) "https" 443))
      (t nil))))

(defun parse-http-url-parts (url-remainder scheme default-port)
  "Parse host:port/path from URL remainder."
  (let* ((path-start (position #\/ url-remainder))
         (host-port (if path-start
                        (subseq url-remainder 0 path-start)
                        url-remainder))
         (path (if path-start
                   (subseq url-remainder path-start)
                   "/"))
         (colon-pos (position #\: host-port))
         (host (if colon-pos
                   (subseq host-port 0 colon-pos)
                   host-port))
         (port (if colon-pos
                   (parse-integer (subseq host-port (1+ colon-pos)) :junk-allowed t)
                   default-port)))
    (when (and host (plusp (length host)) port)
      (values scheme host port path))))

(defun get-http-proxy ()
  "Get HTTP proxy settings from environment.
   Returns (values host port) or NIL if no proxy configured."
  (let ((proxy-url (or (uiop:getenv "HTTP_PROXY")
                       (uiop:getenv "http_proxy"))))
    (when (and proxy-url (plusp (length proxy-url)))
      (multiple-value-bind (scheme host port path)
          (parse-http-url proxy-url)
        (declare (ignore scheme path))
        (when host
          (values host port))))))

(defun http-get-request (host port path &key proxy-host proxy-port)
  "Perform an HTTP GET request. Returns (values body status-code headers redirect-url).
   Uses proxy if proxy-host is specified. Timeout computed from current context."
  (let* ((connect-host (or proxy-host host))
         (connect-port (or proxy-port port))
         (request-path (if proxy-host
                           (format nil "http://~A:~A~A" host port path)
                           path))
         (socket nil)
         (stream nil))
    (unwind-protect
        (handler-case
            (progn
              ;; Check context before blocking connect
              (check-tls-context)
              ;; Connect with timeout from context
              (setf socket (usocket:socket-connect connect-host connect-port
                                                   :timeout (effective-timeout)
                                                   :element-type '(unsigned-byte 8)))
              (setf stream (usocket:socket-stream socket))
              ;; Send request (use explicit CRLF to avoid Windows line ending issues)
              (let* ((crlf (coerce '(#\Return #\Linefeed) 'string))
                     (request (concatenate 'string
                                           "GET " request-path " HTTP/1.1" crlf
                                           "Host: " host crlf
                                           "Connection: close" crlf
                                           "Accept: */*" crlf
                                           "User-Agent: pure-tls/1.0" crlf
                                           crlf)))
                (write-sequence (flexi-streams:string-to-octets request :external-format :latin-1)
                                stream)
                (force-output stream))
              ;; Read response
              (parse-http-response stream))
          (error ()
            (values nil nil nil nil)))
      ;; Cleanup
      (when socket
        (ignore-errors (usocket:socket-close socket))))))

(defun read-line-crlf (stream)
  "Read a line terminated by CRLF from a binary stream.
   Returns the line without the trailing CRLF, or NIL at end of stream."
  (let ((line (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for byte = (progn
                       ;; Check context before each blocking read
                       (check-tls-context)
                       (read-byte stream nil nil))
          while byte
          do (vector-push-extend byte line)
             ;; Check for CRLF terminator
             (when (and (>= (length line) 2)
                        (= (aref line (- (length line) 2)) 13)  ; CR
                        (= (aref line (1- (length line))) 10)) ; LF
               (return-from read-line-crlf
                 (flexi-streams:octets-to-string
                  (subseq line 0 (- (length line) 2))
                  :external-format :latin-1))))
    ;; Return what we have if stream ended without CRLF
    (when (plusp (length line))
      (flexi-streams:octets-to-string line :external-format :latin-1))))

(defun parse-http-response (stream)
  "Parse HTTP response from stream.
   Returns (values body status-code headers redirect-url)."
  (let ((status-line (read-line-crlf stream))
        (headers (make-hash-table :test 'equalp))
        (status-code nil)
        (content-length nil)
        (redirect-url nil))
    ;; Parse status line (e.g., "HTTP/1.1 200 OK")
    (when status-line
      (let* ((space1 (position #\Space status-line))
             (space2 (and space1 (position #\Space status-line :start (1+ space1)))))
        (when (and space1 space2)
          (setf status-code (parse-integer (subseq status-line (1+ space1) space2)
                                           :junk-allowed t)))))
    ;; Parse headers
    (loop for header-line = (read-line-crlf stream)
          while (and header-line (plusp (length header-line)))
          do (let ((colon (position #\: header-line)))
               (when colon
                 (let ((name (string-trim '(#\Space #\Tab) (subseq header-line 0 colon)))
                       (value (string-trim '(#\Space #\Tab) (subseq header-line (1+ colon)))))
                   (setf (gethash name headers) value)
                   (when (string-equal name "content-length")
                     (setf content-length (parse-integer value :junk-allowed t)))
                   (when (and (string-equal name "location")
                              (member status-code '(301 302 303 307 308)))
                     (setf redirect-url value))))))
    ;; Handle redirect
    (when redirect-url
      (return-from parse-http-response
        (values nil status-code headers redirect-url)))
    ;; Read body for 200 OK responses
    (if (eql status-code 200)
        (let ((body (if content-length
                        (let ((buf (make-array (min content-length *crl-max-response-size*)
                                               :element-type '(unsigned-byte 8))))
                          ;; Check context before blocking read
                          (check-tls-context)
                          (let ((bytes-read (read-sequence buf stream)))
                            (if (= bytes-read (length buf))
                                buf
                                (subseq buf 0 bytes-read))))
                        ;; No content-length, read until EOF
                        (let ((chunks nil)
                              (total 0))
                          (loop for chunk = (make-array 8192 :element-type '(unsigned-byte 8))
                                for bytes-read = (progn
                                                   ;; Check context before each chunk read
                                                   (check-tls-context)
                                                   (read-sequence chunk stream))
                                while (plusp bytes-read)
                                do (push (subseq chunk 0 bytes-read) chunks)
                                   (incf total bytes-read)
                                when (> total *crl-max-response-size*)
                                  do (return nil))
                          (when chunks
                            (let ((result (make-array total :element-type '(unsigned-byte 8)))
                                  (pos 0))
                              (dolist (chunk (nreverse chunks))
                                (replace result chunk :start1 pos)
                                (incf pos (length chunk)))
                              result))))))
          (values body status-code headers nil))
        ;; Non-200 response
        (values nil status-code headers nil))))

(defun fetch-crl (uri)
  "Fetch a CRL from the given URI.
   Returns the parsed CRL or NIL on failure.
   Uses cache if available. Timeout computed from current context."
  ;; Check cache first
  (let ((cached (get-cached-crl uri)))
    (when cached
      (return-from fetch-crl cached)))
  ;; Fetch from network
  (handler-case
      (let ((der-bytes (fetch-crl-bytes uri)))
        (when der-bytes
          (let ((crl (parse-crl der-bytes)))
            ;; Cache the CRL
            (cache-crl uri crl)
            crl)))
    (error (e)
      (warn "Failed to fetch CRL from ~A: ~A" uri e)
      nil)))

(defun fetch-crl-bytes (uri)
  "Fetch raw CRL bytes from a URI. Returns octet vector or NIL.
   Uses *crl-fetch-function* if set, otherwise uses built-in HTTP client."
  (cond
    ;; Use custom fetch function if provided
    (*crl-fetch-function*
     (funcall *crl-fetch-function* uri))
    ;; Use built-in HTTP client
    (t
     (fetch-crl-bytes-http uri :redirects-remaining *crl-max-redirects*))))

(defun fetch-crl-bytes-http (uri &key (redirects-remaining 5))
  "Fetch CRL bytes using built-in HTTP client with redirect support."
  (multiple-value-bind (scheme host port path)
      (parse-http-url uri)
    (unless (and scheme host port path)
      (warn "Invalid CRL URI: ~A" uri)
      (return-from fetch-crl-bytes-http nil))
    ;; Only support HTTP (CRLs are signed, HTTPS not needed)
    (unless (string= scheme "http")
      (warn "Only HTTP URIs supported for CRL fetching: ~A" uri)
      (return-from fetch-crl-bytes-http nil))
    ;; Get proxy settings
    (multiple-value-bind (proxy-host proxy-port)
        (get-http-proxy)
      ;; Make request
      (multiple-value-bind (body status-code headers redirect-url)
          (http-get-request host port path
                            :proxy-host proxy-host
                            :proxy-port proxy-port)
        (declare (ignore headers))
        (cond
          ;; Success
          (body body)
          ;; Redirect
          ((and redirect-url (plusp redirects-remaining))
           ;; Handle relative redirects
           (let ((full-url (if (or (alexandria:starts-with-subseq "http://" redirect-url)
                                   (alexandria:starts-with-subseq "https://" redirect-url))
                               redirect-url
                               (format nil "http://~A:~A~A" host port redirect-url))))
             (fetch-crl-bytes-http full-url :redirects-remaining (1- redirects-remaining))))
          ;; Failure
          (t
           (when status-code
             (warn "CRL fetch failed with status ~A: ~A" status-code uri))
           nil))))))

;;;; High-Level Revocation Checking

(defun check-certificate-revocation (certificate &key issuer-cert (verify-signature t))
  "Check if a certificate has been revoked.
   Returns :valid, :revoked, :unknown, or :error.

   - :valid means we checked and certificate is not revoked
   - :revoked means certificate is on a CRL
   - :unknown means we couldn't check (no CDP, fetch failed, etc.)
   - :error means something went wrong

   ISSUER-CERT is the certificate of the CA that issued CERTIFICATE.
   If provided and VERIFY-SIGNATURE is T (the default), the CRL signature
   will be verified. This is important for security since CRLs are fetched
   over plain HTTP.

   If ISSUER-CERT is not provided, CRL signatures cannot be verified,
   and the function will return :unknown (unless VERIFY-SIGNATURE is NIL).

   Timeout is computed from the current context (cl-cancel:*current-cancel-context*)."
  (let ((cdp-uris (certificate-crl-distribution-points certificate)))
    (unless cdp-uris
      ;; No CRL Distribution Points - can't check
      (return-from check-certificate-revocation :unknown))
    ;; If signature verification is required but no issuer cert provided,
    ;; we cannot securely check revocation
    (when (and verify-signature (null issuer-cert))
      (return-from check-certificate-revocation :unknown))
    ;; Try each distribution point
    (dolist (uri cdp-uris)
      ;; Only support HTTP URIs for now
      (when (alexandria:starts-with-subseq "http://" uri)
        (let ((crl (fetch-crl uri)))
          (when crl
            ;; Check if CRL issuer matches certificate issuer
            (when (crl-issuer-matches-p crl certificate)
              ;; Verify CRL signature if issuer cert provided
              (let ((sig-ok (or (not verify-signature)
                                (not issuer-cert)
                                (verify-crl-signature crl issuer-cert))))
                (cond
                  ((not sig-ok)
                   (warn "CRL signature verification failed for ~A" uri))
                  ;; Signature OK - check if certificate is on the CRL
                  ((crl-contains-serial-p crl (x509-certificate-serial-number certificate))
                   (return-from check-certificate-revocation :revoked))
                  (t
                   (return-from check-certificate-revocation :valid)))))))))
    ;; Couldn't get any valid CRL
    :unknown))
