;;; certificate.lisp --- X.509 Certificate Parsing
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements X.509 certificate parsing using the ASN.1 parser.

(in-package #:pure-tls)

;;;; X.509 Certificate Structure
;;;
;;; Certificate  ::=  SEQUENCE  {
;;;      tbsCertificate       TBSCertificate,
;;;      signatureAlgorithm   AlgorithmIdentifier,
;;;      signatureValue       BIT STRING  }
;;;
;;; TBSCertificate  ::=  SEQUENCE  {
;;;      version         [0]  EXPLICIT Version DEFAULT v1,
;;;      serialNumber         CertificateSerialNumber,
;;;      signature            AlgorithmIdentifier,
;;;      issuer               Name,
;;;      validity             Validity,
;;;      subject              Name,
;;;      subjectPublicKeyInfo SubjectPublicKeyInfo,
;;;      issuerUniqueID  [1]  IMPLICIT UniqueIdentifier OPTIONAL,
;;;      subjectUniqueID [2]  IMPLICIT UniqueIdentifier OPTIONAL,
;;;      extensions      [3]  EXPLICIT Extensions OPTIONAL }

(defstruct x509-certificate
  "Parsed X.509 certificate."
  ;; Raw DER bytes (for signature verification)
  (raw-der nil :type (or null octet-vector))
  (tbs-raw nil :type (or null octet-vector))
  ;; Parsed fields
  (version 1 :type fixnum)
  (serial-number nil)
  (signature-algorithm nil)
  (signature-algorithm-params nil)  ; For RSA-PSS: (:hash :salt-length)
  (issuer nil)
  (validity-not-before nil)
  (validity-not-after nil)
  (subject nil)
  (subject-public-key-info nil)
  (extensions nil :type list)
  ;; Signature
  (signature nil))

(defstruct x509-name
  "X.509 Distinguished Name."
  (rdns nil :type list))  ; List of (oid . value) pairs

(defstruct x509-extension
  "X.509 Extension."
  (oid nil)
  (critical nil :type boolean)
  (value nil))

;;;; Certificate Parsing

(defun known-extension-p (oid)
  "Check if an extension OID is known (has a symbolic name).
Per RFC 5280 s4.2, unknown critical extensions must cause rejection."
  (symbolp oid))

(defun validate-implicit-bit-string (raw-bytes)
  "Validate BIT STRING encoding for IMPLICIT BIT STRING fields.
Per DER (X.690): unused bits count must be 0-7 and padding bits must be zero."
  (when (and raw-bytes (> (length raw-bytes) 0))
    (let ((unused-bits (aref raw-bytes 0)))
      (when (> unused-bits 7)
        (error 'tls-decode-error
               :message "BIT STRING unused bits count must be 0-7"))
      (when (and (> unused-bits 0) (> (length raw-bytes) 1))
        (let* ((last-byte (aref raw-bytes (1- (length raw-bytes))))
               (mask (1- (ash 1 unused-bits))))
          (unless (zerop (logand last-byte mask))
            (error 'tls-decode-error
                   :message "BIT STRING has non-zero padding bits (invalid DER)")))))))

(defun parse-certificate (der-bytes)
  "Parse a DER-encoded X.509 certificate."
  (let* ((root (parse-der der-bytes))
         (cert (make-x509-certificate :raw-der der-bytes)))
    (unless (asn1-sequence-p root)
      (error 'tls-decode-error :message "Certificate must be a SEQUENCE"))
    (let ((children (asn1-children root)))
      (unless (>= (length children) 3)
        (error 'tls-decode-error :message "Certificate missing required fields"))
      ;; TBSCertificate - returns inner signature algorithm
      (let* ((tbs (first children))
             (inner-sig-algorithm (progn
                                    (setf (x509-certificate-tbs-raw cert)
                                          (asn1-node-raw-bytes tbs))
                                    (parse-tbs-certificate cert tbs))))
        ;; SignatureAlgorithm (with params for RSA-PSS)
        (multiple-value-bind (algorithm params)
            (parse-algorithm-identifier-with-params (second children))
          (setf (x509-certificate-signature-algorithm cert) algorithm)
          (setf (x509-certificate-signature-algorithm-params cert) params)
          ;; Per RFC 5280 s4.1.1.2: inner and outer signature algorithms must match
          (unless (equal inner-sig-algorithm algorithm)
            (error 'tls-decode-error
                   :message (format nil "Signature algorithm mismatch: inner=~A outer=~A"
                                    inner-sig-algorithm algorithm)))))
      ;; SignatureValue (BIT STRING)
      (let ((sig-value (asn1-node-value (third children))))
        (setf (x509-certificate-signature cert)
              (getf sig-value :data))))
    cert))

(defun parse-tbs-certificate (cert tbs)
  "Parse the TBSCertificate portion. Returns the inner signature algorithm."
  (let ((children (asn1-children tbs))
        (idx 0)
        (inner-sig-algorithm nil))
    ;; Version [0] EXPLICIT (optional, default v1)
    (when (and children (asn1-context-p (nth idx children) 0))
      (let ((version-node (first (asn1-children (nth idx children)))))
        (setf (x509-certificate-version cert)
              (1+ (asn1-node-value version-node))))
      (incf idx))
    ;; SerialNumber - RFC 5280 s4.1.2.2: must be positive
    (let ((serial (asn1-node-value (nth idx children))))
      (when (< serial 0)
        (error 'tls-decode-error
               :message "Certificate serial number must be positive (RFC 5280 s4.1.2.2)"))
      (setf (x509-certificate-serial-number cert) serial))
    (incf idx)
    ;; Signature (AlgorithmIdentifier) - capture for validation
    (setf inner-sig-algorithm (parse-algorithm-identifier (nth idx children)))
    (incf idx)
    ;; Issuer
    (setf (x509-certificate-issuer cert)
          (parse-name (nth idx children)))
    (incf idx)
    ;; Validity
    (let ((validity (asn1-children (nth idx children))))
      (setf (x509-certificate-validity-not-before cert)
            (asn1-node-value (first validity)))
      (setf (x509-certificate-validity-not-after cert)
            (asn1-node-value (second validity))))
    (incf idx)
    ;; Subject
    (setf (x509-certificate-subject cert)
          (parse-name (nth idx children)))
    (incf idx)
    ;; SubjectPublicKeyInfo
    (setf (x509-certificate-subject-public-key-info cert)
          (parse-subject-public-key-info (nth idx children)))
    (incf idx)
    ;; Validate and skip optional issuerUniqueID [1] and subjectUniqueID [2]
    ;; These are IMPLICIT BIT STRING, so we need to validate the BIT STRING encoding
    (loop while (and (< idx (length children))
                     (or (asn1-context-p (nth idx children) 1)
                         (asn1-context-p (nth idx children) 2)))
          do (let ((unique-id-node (nth idx children)))
               ;; Validate BIT STRING encoding for IMPLICIT BIT STRING
               (validate-implicit-bit-string (asn1-node-value unique-id-node)))
             (incf idx))
    ;; Extensions [3] EXPLICIT
    (when (and (< idx (length children))
               (asn1-context-p (nth idx children) 3))
      (setf (x509-certificate-extensions cert)
            (parse-extensions-seq (first (asn1-children (nth idx children)))))
      ;; Per RFC 5280 s4.2: reject certificates with unknown critical extensions
      (dolist (ext (x509-certificate-extensions cert))
        (when (and (x509-extension-critical ext)
                   (not (known-extension-p (x509-extension-oid ext))))
          (error 'tls-decode-error
                 :message (format nil "Unknown critical extension: ~A"
                                  (x509-extension-oid ext))))))
    ;; Return inner signature algorithm for validation
    inner-sig-algorithm))

(defun parse-name (node)
  "Parse an X.509 Name (sequence of RDNs)."
  (let ((rdns nil))
    (dolist (rdn-set (asn1-children node))
      (dolist (attr (asn1-children rdn-set))
        (let* ((children (asn1-children attr))
               (oid (asn1-node-value (first children)))
               (value (asn1-node-value (second children))))
          (push (cons (oid-name oid) value) rdns))))
    (make-x509-name :rdns (nreverse rdns))))

(defun parse-algorithm-identifier (node)
  "Parse an AlgorithmIdentifier.
   Returns the algorithm OID name, or for EC algorithms, the curve OID."
  (let ((children (asn1-children node)))
    (let ((algorithm (oid-name (asn1-node-value (first children)))))
      ;; For EC public keys, the second parameter is the curve OID
      (if (and (member algorithm '(:ec-public-key :ecdsa))
               (second children))
          (oid-name (asn1-node-value (second children)))
          algorithm))))

(defun parse-algorithm-identifier-with-params (node)
  "Parse an AlgorithmIdentifier, returning (algorithm . params).
   For RSA-PSS, params is a plist with :hash and :salt-length.
   For other algorithms, params is NIL."
  (let* ((children (asn1-children node))
         (algorithm (oid-name (asn1-node-value (first children))))
         (params nil))
    ;; For EC public keys, the second parameter is the curve OID
    (when (and (member algorithm '(:ec-public-key :ecdsa))
               (second children))
      (setf algorithm (oid-name (asn1-node-value (second children)))))
    ;; For RSA-PSS, parse the parameters
    (when (and (member algorithm '(:rsa-pss :rsassa-pss))
               (second children))
      (setf params (parse-rsa-pss-params (second children))))
    (values algorithm params)))

(defun parse-rsa-pss-params (node)
  "Parse RSA-PSS AlgorithmIdentifier parameters.
   RSASSA-PSS-params ::= SEQUENCE {
     hashAlgorithm      [0] HashAlgorithm DEFAULT sha1,
     maskGenAlgorithm   [1] MaskGenAlgorithm DEFAULT mgf1SHA1,
     saltLength         [2] INTEGER DEFAULT 20,
     trailerField       [3] TrailerField DEFAULT trailerFieldBC }
   Returns a plist with :hash and :salt-length."
  (let ((hash :sha1)        ; Default per RFC 4055
        (salt-length 20))   ; Default per RFC 4055
    (when (and node (asn1-sequence-p node))
      (dolist (child (asn1-children node))
        (when (asn1-node-p child)
          (let ((tag (asn1-node-tag child)))
            (cond
              ;; [0] hashAlgorithm
              ((and (asn1-context-p child 0)
                    (asn1-children child))
               (let* ((hash-seq (first (asn1-children child)))
                      (hash-children (when (asn1-sequence-p hash-seq)
                                      (asn1-children hash-seq))))
                 (when hash-children
                   (let ((hash-oid (oid-name (asn1-node-value (first hash-children)))))
                     (setf hash (case hash-oid
                                  (:sha256 :sha256)
                                  (:sha384 :sha384)
                                  (:sha512 :sha512)
                                  (:sha1 :sha1)
                                  (otherwise hash-oid)))))))
              ;; [2] saltLength
              ((asn1-context-p child 2)
               (let ((salt-node (first (asn1-children child))))
                 (when (and salt-node (integerp (asn1-node-value salt-node)))
                   (setf salt-length (asn1-node-value salt-node))))))))))
    (list :hash hash :salt-length salt-length)))

(defun parse-subject-public-key-info (node)
  "Parse SubjectPublicKeyInfo."
  (let* ((children (asn1-children node))
         (algorithm (parse-algorithm-identifier (first children)))
         (public-key-bits (asn1-node-value (second children))))
    (list :algorithm algorithm
          :public-key (getf public-key-bits :data))))

(defun parse-extensions-seq (node)
  "Parse a SEQUENCE of Extensions.
Per RFC 5280 Section 4.2: A certificate MUST NOT include more than one
instance of a particular extension."
  (let ((extensions nil)
        (seen-oids (make-hash-table :test 'equal)))
    (dolist (child (asn1-children node))
      (let* ((ext-children (asn1-children child))
             (oid (asn1-node-value (first ext-children))))
        ;; Check for duplicate extensions
        (when (gethash oid seen-oids)
          (error 'tls-decode-error
                 :message (format nil "Duplicate extension: ~A" (oid-name oid))))
        (setf (gethash oid seen-oids) t)
        (push (parse-x509-extension child) extensions)))
    (nreverse extensions)))

(defun parse-x509-extension (node)
  "Parse a single Extension."
  (let* ((children (asn1-children node))
         (oid (asn1-node-value (first children)))
         (idx 1)
         (critical nil)
         (value nil))
    ;; Check for critical flag
    (when (and (< idx (length children))
               (= (asn1-node-tag (nth idx children)) +asn1-boolean+))
      (setf critical (asn1-node-value (nth idx children)))
      (incf idx))
    ;; Value is an OCTET STRING containing DER
    (when (< idx (length children))
      (let ((value-bytes (asn1-node-value (nth idx children))))
        (setf value (parse-extension-value oid value-bytes))))
    (make-x509-extension :oid (oid-name oid)
                         :critical critical
                         :value value)))

(defun parse-extension-value (oid value-bytes)
  "Parse extension value based on OID."
  (let ((name (oid-name oid)))
    (case name
      (:subject-alt-name
       (parse-subject-alt-name value-bytes))
      (:basic-constraints
       (parse-basic-constraints value-bytes))
      (:key-usage
       (parse-key-usage value-bytes))
      (:crl-distribution-points
       (parse-crl-distribution-points value-bytes))
      (otherwise
       ;; Return raw bytes for unknown extensions
       value-bytes))))

(defun parse-subject-alt-name (bytes)
  "Parse SubjectAltName extension value."
  (let* ((node (parse-der bytes))
         (names nil))
    (dolist (child (asn1-children node))
      (let ((tag (asn1-node-tag child))
            (value (asn1-node-value child)))
        (case tag
          (2 ; dNSName
           (push (list :dns (octets-to-string value)) names))
          (7 ; iPAddress
           (push (list :ip value) names))
          (1 ; rfc822Name
           (push (list :email (octets-to-string value)) names))
          (6 ; uniformResourceIdentifier
           (push (list :uri (octets-to-string value)) names)))))
    (nreverse names)))

(defun parse-basic-constraints (bytes)
  "Parse BasicConstraints extension value."
  (let* ((node (parse-der bytes))
         (children (asn1-children node))
         (ca nil)
         (path-len nil))
    (when children
      (when (= (asn1-node-tag (first children)) +asn1-boolean+)
        (setf ca (asn1-node-value (first children)))
        (setf children (rest children)))
      (when (and children (= (asn1-node-tag (first children)) +asn1-integer+))
        (setf path-len (asn1-node-value (first children)))))
    (list :ca ca :path-length-constraint path-len)))

(defun parse-key-usage (bytes)
  "Parse KeyUsage extension value."
  (let* ((node (parse-der bytes))
         (bits (asn1-node-value node))
         (unused (getf bits :unused-bits))
         (data (getf bits :data))
         (usages nil))
    (when data
      (let ((byte0 (if (plusp (length data)) (aref data 0) 0))
            (byte1 (if (> (length data) 1) (aref data 1) 0)))
        (when (logbitp 7 byte0) (push :digital-signature usages))
        (when (logbitp 6 byte0) (push :non-repudiation usages))
        (when (logbitp 5 byte0) (push :key-encipherment usages))
        (when (logbitp 4 byte0) (push :data-encipherment usages))
        (when (logbitp 3 byte0) (push :key-agreement usages))
        (when (logbitp 2 byte0) (push :key-cert-sign usages))
        (when (logbitp 1 byte0) (push :crl-sign usages))
        (when (logbitp 0 byte0) (push :encipher-only usages))
        (when (logbitp 7 byte1) (push :decipher-only usages))))
    (nreverse usages)))

(defun parse-crl-distribution-points (bytes)
  "Parse CRLDistributionPoints extension value (RFC 5280 s4.2.1.13).
   Returns a list of distribution point URIs.

   CRLDistributionPoints ::= SEQUENCE SIZE (1..MAX) OF DistributionPoint
   DistributionPoint ::= SEQUENCE {
       distributionPoint       [0]     DistributionPointName OPTIONAL,
       reasons                 [1]     ReasonFlags OPTIONAL,
       cRLIssuer               [2]     GeneralNames OPTIONAL }
   DistributionPointName ::= CHOICE {
       fullName                [0]     GeneralNames,
       nameRelativeToCRLIssuer [1]     RelativeDistinguishedName }"
  (let* ((node (parse-der bytes))
         (uris nil))
    (dolist (dp (asn1-children node))
      ;; Each DistributionPoint is a SEQUENCE
      (dolist (child (asn1-children dp))
        ;; Look for [0] distributionPoint
        (when (asn1-context-p child 0)
          ;; Inside distributionPoint, look for [0] fullName (GeneralNames)
          (dolist (dp-name-child (asn1-children child))
            (when (asn1-context-p dp-name-child 0)
              ;; GeneralNames is a SEQUENCE of GeneralName
              ;; Each GeneralName is context-tagged
              (dolist (general-name (asn1-children dp-name-child))
                ;; Tag 6 = uniformResourceIdentifier (URI)
                (when (and (= (asn1-node-class general-name) +asn1-class-context-specific+)
                           (= (asn1-node-tag general-name) 6))
                  (push (octets-to-string (asn1-node-value general-name)) uris))))))))
    (nreverse uris)))

;;;; Certificate Accessors

(defun certificate-subject-common-names (cert)
  "Get all Common Name values from the certificate subject."
  (loop for (oid . value) in (x509-name-rdns (x509-certificate-subject cert))
        when (eql oid :common-name)
          collect value))

(defun certificate-issuer-common-names (cert)
  "Get all Common Name values from the certificate issuer."
  (loop for (oid . value) in (x509-name-rdns (x509-certificate-issuer cert))
        when (eql oid :common-name)
          collect value))

(defun certificate-dns-names (cert)
  "Get all DNS names from Subject Alternative Name extension."
  (let ((san-ext (find :subject-alt-name (x509-certificate-extensions cert)
                       :key #'x509-extension-oid)))
    (when san-ext
      (loop for (type value) in (x509-extension-value san-ext)
            when (eql type :dns)
              collect value))))

(defun certificate-ip-addresses (cert)
  "Get all IP addresses from Subject Alternative Name extension.
   Returns a list of octet vectors (4 bytes for IPv4, 16 bytes for IPv6)."
  (let ((san-ext (find :subject-alt-name (x509-certificate-extensions cert)
                       :key #'x509-extension-oid)))
    (when san-ext
      (loop for (type value) in (x509-extension-value san-ext)
            when (eql type :ip)
              collect value))))

(defun certificate-not-before (cert)
  "Get the notBefore validity time as a universal-time."
  (x509-certificate-validity-not-before cert))

(defun certificate-not-after (cert)
  "Get the notAfter validity time as a universal-time."
  (x509-certificate-validity-not-after cert))

(defun certificate-fingerprint (cert &optional (algorithm :sha256))
  "Compute the fingerprint of the certificate."
  (ironclad:digest-sequence algorithm (x509-certificate-raw-der cert)))

(defun certificate-is-ca-p (cert)
  "Check if certificate is a CA certificate (BasicConstraints cA=true)."
  (let ((bc-ext (find :basic-constraints (x509-certificate-extensions cert)
                      :key #'x509-extension-oid)))
    (when bc-ext
      (getf (x509-extension-value bc-ext) :ca))))

(defun certificate-path-length-constraint (cert)
  "Get the path length constraint from BasicConstraints, or NIL if not set."
  (let ((bc-ext (find :basic-constraints (x509-certificate-extensions cert)
                      :key #'x509-extension-oid)))
    (when bc-ext
      (getf (x509-extension-value bc-ext) :path-length-constraint))))

(defun certificate-key-usage (cert)
  "Get the KeyUsage extension value as a list of keywords, or NIL if not present.
   Possible values: :digital-signature, :non-repudiation, :key-encipherment,
   :data-encipherment, :key-agreement, :key-cert-sign, :crl-sign,
   :encipher-only, :decipher-only."
  (let ((ku-ext (find :key-usage (x509-certificate-extensions cert)
                      :key #'x509-extension-oid)))
    (when ku-ext
      (x509-extension-value ku-ext))))

(defun certificate-crl-distribution-points (cert)
  "Get the CRL Distribution Points URIs from the certificate.
   Returns a list of URI strings where CRLs can be fetched, or NIL if not present."
  (let ((cdp-ext (find :crl-distribution-points (x509-certificate-extensions cert)
                       :key #'x509-extension-oid)))
    (when cdp-ext
      (x509-extension-value cdp-ext))))

(defun certificate-has-key-usage-p (cert usage)
  "Check if certificate has a specific key usage bit set.
   USAGE is a keyword like :key-cert-sign or :digital-signature."
  (member usage (certificate-key-usage cert)))

(defun certificate-can-sign-certificates-p (cert)
  "Check if certificate can sign other certificates.
   Requires BasicConstraints cA=true AND KeyUsage keyCertSign (if KeyUsage present)."
  (and (certificate-is-ca-p cert)
       ;; If KeyUsage extension is present, keyCertSign must be set
       ;; If KeyUsage is absent, we allow signing (per RFC 5280 - absence means all usages)
       (let ((key-usage (certificate-key-usage cert)))
         (or (null key-usage)
             (member :key-cert-sign key-usage)))))

(defun certificate-critical-extensions (cert)
  "Get list of critical extensions from the certificate."
  (loop for ext in (x509-certificate-extensions cert)
        when (x509-extension-critical ext)
          collect (x509-extension-oid ext)))

(defun certificate-has-unknown-critical-extensions-p (cert)
  "Check if certificate has any critical extensions we don't understand.
   Returns a list of unknown critical extension OIDs, or NIL if all are known."
  (let ((known-critical-extensions '(:basic-constraints
                                     :key-usage
                                     :subject-alt-name
                                     :name-constraints
                                     :certificate-policies
                                     :policy-mappings
                                     :policy-constraints
                                     :inhibit-any-policy
                                     :extended-key-usage
                                     :crl-distribution-points)))
    (loop for ext in (x509-certificate-extensions cert)
          when (and (x509-extension-critical ext)
                    (not (member (x509-extension-oid ext) known-critical-extensions)))
            collect (x509-extension-oid ext))))

;;;; Certificate Loading

(defun parse-certificate-from-file (path)
  "Load and parse a certificate from a file.
   Supports DER and PEM formats."
  (let ((bytes (read-file-bytes path)))
    (if (pem-encoded-p bytes)
        (parse-certificate (pem-decode bytes "CERTIFICATE"))
        (parse-certificate bytes))))

(defun read-file-bytes (path)
  "Read file contents as octet vector."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((bytes (make-octet-vector (file-length stream))))
      (read-sequence bytes stream)
      bytes)))

(defun pem-encoded-p (bytes)
  "Check if bytes look like PEM encoding.
   Handles files that start with comments before the PEM block."
  (and (>= (length bytes) 27)  ; Length of -----BEGIN CERTIFICATE-----
       (let ((text (octets-to-string bytes)))
         (search "-----BEGIN" text))))

(defun pem-decode (bytes label)
  "Decode PEM-encoded data, extracting the block with the given LABEL."
  (let* ((text (octets-to-string bytes))
         (begin-marker (format nil "-----BEGIN ~A-----" label))
         (end-marker (format nil "-----END ~A-----" label))
         (begin-pos (search begin-marker text))
         (end-pos (search end-marker text)))
    (unless (and begin-pos end-pos)
      (error 'tls-decode-error :message (format nil "PEM block '~A' not found" label)))
    (let* ((base64-start (+ begin-pos (length begin-marker)))
           (base64-text (subseq text base64-start end-pos))
           ;; Remove whitespace
           (clean-base64 (remove-if (lambda (c) (member c '(#\Newline #\Return #\Space)))
                                    base64-text)))
      (base64-decode clean-base64))))

(defun base64-decode (string)
  "Decode a Base64-encoded string to octets."
  (cl-base64:base64-string-to-usb8-array string))
