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

(defun parse-certificate (der-bytes)
  "Parse a DER-encoded X.509 certificate."
  (let* ((root (parse-der der-bytes))
         (cert (make-x509-certificate :raw-der der-bytes)))
    (unless (asn1-sequence-p root)
      (error 'tls-decode-error :message "Certificate must be a SEQUENCE"))
    (let ((children (asn1-children root)))
      (unless (>= (length children) 3)
        (error 'tls-decode-error :message "Certificate missing required fields"))
      ;; TBSCertificate
      (let ((tbs (first children)))
        (setf (x509-certificate-tbs-raw cert) (asn1-node-raw-bytes tbs))
        (parse-tbs-certificate cert tbs))
      ;; SignatureAlgorithm
      (setf (x509-certificate-signature-algorithm cert)
            (parse-algorithm-identifier (second children)))
      ;; SignatureValue (BIT STRING)
      (let ((sig-value (asn1-node-value (third children))))
        (setf (x509-certificate-signature cert)
              (getf sig-value :data))))
    cert))

(defun parse-tbs-certificate (cert tbs)
  "Parse the TBSCertificate portion."
  (let ((children (asn1-children tbs))
        (idx 0))
    ;; Version [0] EXPLICIT (optional, default v1)
    (when (and children (asn1-context-p (nth idx children) 0))
      (let ((version-node (first (asn1-children (nth idx children)))))
        (setf (x509-certificate-version cert)
              (1+ (asn1-node-value version-node))))
      (incf idx))
    ;; SerialNumber
    (setf (x509-certificate-serial-number cert)
          (asn1-node-value (nth idx children)))
    (incf idx)
    ;; Signature (AlgorithmIdentifier) - skip, use outer one
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
    ;; Skip optional issuerUniqueID [1] and subjectUniqueID [2]
    (loop while (and (< idx (length children))
                     (asn1-context-p (nth idx children) 1)
                     (asn1-context-p (nth idx children) 2))
          do (incf idx))
    ;; Extensions [3] EXPLICIT
    (when (and (< idx (length children))
               (asn1-context-p (nth idx children) 3))
      (setf (x509-certificate-extensions cert)
            (parse-extensions-seq (first (asn1-children (nth idx children))))))))

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

(defun parse-subject-public-key-info (node)
  "Parse SubjectPublicKeyInfo."
  (let* ((children (asn1-children node))
         (algorithm (parse-algorithm-identifier (first children)))
         (public-key-bits (asn1-node-value (second children))))
    (list :algorithm algorithm
          :public-key (getf public-key-bits :data))))

(defun parse-extensions-seq (node)
  "Parse a SEQUENCE of Extensions."
  (mapcar #'parse-x509-extension (asn1-children node)))

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
      (t
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

;;;; Certificate Accessors

(defun certificate-subject-common-names (cert)
  "Get all Common Name values from the certificate subject."
  (loop for (oid . value) in (x509-name-rdns (x509-certificate-subject cert))
        when (eq oid :common-name)
          collect value))

(defun certificate-issuer-common-names (cert)
  "Get all Common Name values from the certificate issuer."
  (loop for (oid . value) in (x509-name-rdns (x509-certificate-issuer cert))
        when (eq oid :common-name)
          collect value))

(defun certificate-dns-names (cert)
  "Get all DNS names from Subject Alternative Name extension."
  (let ((san-ext (find :subject-alt-name (x509-certificate-extensions cert)
                       :key #'x509-extension-oid)))
    (when san-ext
      (loop for (type value) in (x509-extension-value san-ext)
            when (eq type :dns)
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
  "Check if certificate is a CA certificate."
  (let ((bc-ext (find :basic-constraints (x509-certificate-extensions cert)
                      :key #'x509-extension-oid)))
    (when bc-ext
      (getf (x509-extension-value bc-ext) :ca))))

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
