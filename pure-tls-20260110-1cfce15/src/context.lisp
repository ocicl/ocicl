;;; context.lisp --- TLS Context Management
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements TLS context for configuration and session management.

(in-package #:pure-tls)

;;;; TLS Context

(defstruct (tls-context (:constructor make-tls-context-struct))
  "TLS configuration context (similar to SSL_CTX)."
  ;; Verification settings
  (verify-mode +verify-required+ :type fixnum)
  (verify-depth 100 :type fixnum)
  ;; Certificate chain for server mode
  (certificate-chain nil :type list)
  ;; Private key for server mode
  (private-key nil)
  ;; Trusted CA certificates
  (trust-store nil)
  ;; Supported cipher suites
  (cipher-suites (list +tls-aes-128-gcm-sha256+
                       +tls-chacha20-poly1305-sha256+)
                 :type list)
  ;; ALPN protocols
  (alpn-protocols nil :type list)
  ;; Session cache (for session resumption - future)
  (session-cache nil))

;;;; Default Context

(defvar *default-tls-context* nil
  "The default TLS context used when none is specified.")

(defvar *auto-load-system-trust-store* t
  "When true, automatically load system CA trust store for verify-required mode.")

(defun ensure-default-context ()
  "Ensure a default TLS context exists.
   If verify mode is +verify-required+ and auto-load is enabled,
   automatically loads the system trust store."
  (unless *default-tls-context*
    (let ((ctx (make-tls-context)))
      ;; Auto-load system trust store if verify-required and no trust store set
      (when (and *auto-load-system-trust-store*
                 (= (tls-context-verify-mode ctx) +verify-required+)
                 (null (tls-context-trust-store ctx)))
        (setf (tls-context-trust-store ctx) (load-system-trust-store)))
      (setf *default-tls-context* ctx)))
  *default-tls-context*)

;;;; Context Creation

(defun make-tls-context (&key
                           (verify-mode +verify-required+)
                           (verify-depth 100)
                           certificate-chain-file
                           private-key-file
                           ca-file
                           ca-directory
                           cipher-suites
                           alpn-protocols
                           (auto-load-system-ca t))
  "Create a new TLS context with the specified configuration.

   VERIFY-MODE - Certificate verification mode:
     +VERIFY-NONE+ - No verification
     +VERIFY-PEER+ - Verify if certificate presented
     +VERIFY-REQUIRED+ - Require and verify certificate

   VERIFY-DEPTH - Maximum certificate chain depth.

   CERTIFICATE-CHAIN-FILE - PEM file containing certificate chain (for servers).

   PRIVATE-KEY-FILE - PEM file containing private key (for servers).

   CA-FILE - PEM file containing trusted CA certificates.

   CA-DIRECTORY - Directory containing trusted CA certificates.

   CIPHER-SUITES - List of allowed cipher suites.

   ALPN-PROTOCOLS - List of ALPN protocol names.

   AUTO-LOAD-SYSTEM-CA - If T (default), automatically load system CA store
     when verify-mode is +VERIFY-REQUIRED+ and no CA file/directory is specified."
  (let ((ctx (make-tls-context-struct
              :verify-mode verify-mode
              :verify-depth verify-depth
              :alpn-protocols alpn-protocols)))
    ;; Load certificate chain if specified
    (when certificate-chain-file
      (setf (tls-context-certificate-chain ctx)
            (load-certificate-chain certificate-chain-file)))
    ;; Load private key if specified
    (when private-key-file
      (setf (tls-context-private-key ctx)
            (load-private-key private-key-file)))
    ;; Load trusted CAs
    (cond
      ;; Explicit CA file or directory specified
      ((or ca-file ca-directory)
       (setf (tls-context-trust-store ctx)
             (make-trust-store-from-sources ca-file ca-directory)))
      ;; Auto-load system CAs if verify-required and enabled
      ((and auto-load-system-ca
            (= verify-mode +verify-required+))
       (setf (tls-context-trust-store ctx)
             (load-system-trust-store))))
    ;; Set cipher suites
    (when cipher-suites
      (setf (tls-context-cipher-suites ctx) cipher-suites))
    ctx))

;;;; Context Binding

(defmacro with-tls-context ((context &key auto-free-p) &body body)
  "Execute BODY with *DEFAULT-TLS-CONTEXT* bound to CONTEXT.
   If AUTO-FREE-P is true, free the context when done."
  `(let ((*default-tls-context* ,context))
     (unwind-protect
          (progn ,@body)
       (when ,auto-free-p
         (tls-context-free ,context)))))

(defun tls-context-free (context)
  "Free resources associated with a TLS context."
  (declare (ignore context))
  ;; Currently no external resources to free
  nil)

;;;; Certificate Loading

(defun load-certificate-chain (path)
  "Load a certificate chain from a PEM file.
   Skips certificates that fail to parse (with a warning)."
  (let ((bytes (read-file-bytes path))
        (certs nil))
    (if (pem-encoded-p bytes)
        ;; Parse all certificate blocks
        (let ((text (octets-to-string bytes))
              (start 0))
          (loop
            (let ((begin-pos (search "-----BEGIN CERTIFICATE-----" text :start2 start)))
              (unless begin-pos (return))
              (let ((end-pos (search "-----END CERTIFICATE-----" text :start2 begin-pos)))
                (unless end-pos (return))
                (let* ((block-end (+ end-pos (length "-----END CERTIFICATE-----")))
                       (pem-block (subseq text begin-pos block-end)))
                  (handler-case
                      (let ((der (pem-decode (string-to-octets pem-block) "CERTIFICATE")))
                        (push (parse-certificate der) certs))
                    (error (e)
                      (warn "Failed to parse certificate at position ~D: ~A" begin-pos e)))
                  (setf start block-end))))))
        ;; Single DER certificate
        (handler-case
            (push (parse-certificate bytes) certs)
          (error (e)
            (warn "Failed to parse certificate from ~A: ~A" path e))))
    (nreverse certs)))

(defun load-private-key (path)
  "Load a private key from a PEM file.
   Returns an Ironclad private key object."
  (let ((bytes (read-file-bytes path)))
    (if (pem-encoded-p bytes)
        (let ((text (octets-to-string bytes)))
          ;; Try different PEM labels and parse into Ironclad key
          (cond
            ((search "-----BEGIN PRIVATE KEY-----" text)
             (parse-pkcs8-private-key (pem-decode bytes "PRIVATE KEY")))
            ((search "-----BEGIN RSA PRIVATE KEY-----" text)
             (parse-rsa-private-key (pem-decode bytes "RSA PRIVATE KEY")))
            ((search "-----BEGIN EC PRIVATE KEY-----" text)
             (parse-ec-private-key (pem-decode bytes "EC PRIVATE KEY")))
            (t
             (error 'tls-error :message "Unknown private key format"))))
        ;; Assume DER - try PKCS#8 format first
        (parse-pkcs8-private-key bytes))))

(defun parse-pkcs8-private-key (der)
  "Parse a PKCS#8 encoded private key."
  (let ((parsed (parse-der der)))
    (when (asn1-sequence-p parsed)
      (let* ((children (asn1-children parsed))
             (algorithm-seq (second children))
             (key-octet-string (third children)))
        (when (and (asn1-sequence-p algorithm-seq)
                   (asn1-octet-string-p key-octet-string))
          (let* ((alg-children (asn1-children algorithm-seq))
                 (alg-oid (when alg-children (asn1-node-value (first alg-children)))))
            ;; Dispatch based on algorithm OID
            (cond
              ;; RSA: 1.2.840.113549.1.1.1
              ((equal alg-oid '(1 2 840 113549 1 1 1))
               (parse-rsa-private-key (asn1-node-value key-octet-string)))
              ;; EC: 1.2.840.10045.2.1
              ((equal alg-oid '(1 2 840 10045 2 1))
               (let ((curve-oid (when (>= (length alg-children) 2)
                                  (asn1-node-value (second alg-children)))))
                 (parse-ec-private-key-with-curve
                  (asn1-node-value key-octet-string)
                  curve-oid)))
              ;; Ed25519: 1.3.101.112
              ((equal alg-oid '(1 3 101 112))
               (parse-ed25519-private-key (asn1-node-value key-octet-string)))
              (t
               (error 'tls-error
                      :message (format nil "Unsupported private key algorithm: ~A" alg-oid))))))))))

(defun parse-rsa-private-key (der)
  "Parse an RSA private key from DER."
  (let ((parsed (parse-der der)))
    (when (asn1-sequence-p parsed)
      (let ((children (asn1-children parsed)))
        ;; RSAPrivateKey ::= SEQUENCE {
        ;;   version INTEGER,
        ;;   modulus INTEGER,
        ;;   publicExponent INTEGER,
        ;;   privateExponent INTEGER,
        ;;   prime1 INTEGER,
        ;;   prime2 INTEGER,
        ;;   exponent1 INTEGER,
        ;;   exponent2 INTEGER,
        ;;   coefficient INTEGER
        ;; }
        (when (>= (length children) 9)
          (let ((n (asn1-node-value (second children)))   ; modulus
                (e (asn1-node-value (third children)))    ; public exponent
                (d (asn1-node-value (fourth children)))   ; private exponent
                (p (asn1-node-value (fifth children)))    ; prime1
                (q (asn1-node-value (sixth children))))   ; prime2
            (ironclad:make-private-key :rsa :n n :e e :d d :p p :q q)))))))

(defun parse-ec-private-key (der)
  "Parse an EC private key from DER (SEC 1 format)."
  (let ((parsed (parse-der der)))
    (when (asn1-sequence-p parsed)
      (let ((children (asn1-children parsed)))
        ;; ECPrivateKey ::= SEQUENCE {
        ;;   version INTEGER,
        ;;   privateKey OCTET STRING,
        ;;   parameters [0] ECParameters OPTIONAL,
        ;;   publicKey [1] BIT STRING OPTIONAL
        ;; }
        (when (>= (length children) 2)
          (let* ((private-key-bytes (asn1-node-value (second children)))
                 ;; Look for curve OID in parameters
                 (params (find-if (lambda (c)
                                    (and (listp c)
                                         (eq (car c) :context)
                                         (= (second c) 0)))
                                  children))
                 (curve-oid (when params
                              (let ((param-children (asn1-children (third params))))
                                (when param-children
                                  (asn1-node-value (first param-children)))))))
            (parse-ec-private-key-with-curve private-key-bytes curve-oid)))))))

(defun parse-ec-private-key-with-curve (private-key-bytes curve-oid)
  "Parse an EC private key with a known curve OID.
   The private-key-bytes may be either:
   - Raw private key scalar bytes, or
   - DER-encoded ECPrivateKey structure (from PKCS#8 wrapper)"
  ;; Map curve OIDs to Ironclad curve names
  (let ((curve (cond
                 ;; secp256r1 / prime256v1: 1.2.840.10045.3.1.7
                 ((equal curve-oid '(1 2 840 10045 3 1 7)) :secp256r1)
                 ;; secp384r1: 1.3.132.0.34
                 ((equal curve-oid '(1 3 132 0 34)) :secp384r1)
                 ;; secp521r1: 1.3.132.0.35
                 ((equal curve-oid '(1 3 132 0 35)) :secp521r1)
                 ;; Default to secp256r1
                 (t :secp256r1))))
    ;; Check if this is DER-encoded ECPrivateKey (starts with SEQUENCE tag 0x30)
    ;; If so, extract the raw private key from the structure
    (let ((raw-key (if (and (> (length private-key-bytes) 2)
                            (= (aref private-key-bytes 0) #x30))  ; SEQUENCE tag
                       ;; Parse ECPrivateKey structure:
                       ;; SEQUENCE { version INTEGER, privateKey OCTET STRING, ... }
                       (handler-case
                           (let ((parsed (parse-der private-key-bytes)))
                             (if (and (asn1-sequence-p parsed)
                                      (>= (length (asn1-children parsed)) 2))
                                 (asn1-node-value (second (asn1-children parsed)))
                                 ;; Parsing failed - use bytes as-is (might be raw key)
                                 private-key-bytes))
                         (error () private-key-bytes))
                       ;; Already raw bytes
                       private-key-bytes)))
      (unless (and raw-key (> (length raw-key) 0))
        (error 'tls-error :message "Failed to extract EC private key"))
      (ironclad:make-private-key curve :x raw-key))))

(defun parse-ed25519-private-key (key-bytes)
  "Parse an Ed25519 private key."
  ;; Ed25519 private key is 32 bytes, but may be wrapped in OCTET STRING
  (let ((key (if (and (> (length key-bytes) 32)
                      (= (aref key-bytes 0) #x04))  ; OCTET STRING tag
                 (subseq key-bytes 2)  ; Skip tag and length
                 key-bytes)))
    (ironclad:make-private-key :ed25519 :x key)))

(defun asn1-octet-string-p (node)
  "Check if ASN.1 node is an OCTET STRING."
  (and (asn1-node-p node)
       (= (asn1-node-class node) +asn1-class-universal+)
       (= (asn1-node-tag node) +asn1-octet-string+)))

(defun make-trust-store-from-sources (ca-file ca-directory)
  "Create a trust store from a CA file and/or directory."
  (let ((certs nil))
    ;; Load from file
    (when ca-file
      (setf certs (append certs (load-certificate-chain ca-file))))
    ;; Load from directory
    (when ca-directory
      (let ((dir-store (make-trust-store-from-directory ca-directory)))
        (setf certs (append certs (trust-store-certificates dir-store)))))
    (let ((debug (get-environment-variable "OCICL_TLS_DEBUG")))
      (when (and debug (string/= debug ""))
        (format *error-output* "; pure-tls: trust store certs=~D ca-file=~A ca-dir=~A~%"
                (length certs) ca-file ca-directory)))
    (make-trust-store :certificates certs)))

;;;; System CA Certificates

(defun windows-p ()
  "Check if running on Windows."
  (member :windows *features*))

(defun load-system-trust-store ()
  "Load the system's default trusted CA certificates.

   Checks in order:
   1. SSL_CERT_FILE environment variable (OpenSSL compatible)
   2. SSL_CERT_DIR environment variable (OpenSSL compatible)
   3. Platform-specific default locations"
  ;; 1. Check SSL_CERT_FILE environment variable
  (let ((cert-file (get-environment-variable "SSL_CERT_FILE")))
    (when (and cert-file (probe-file cert-file))
      (return-from load-system-trust-store
        (make-trust-store-from-sources cert-file nil))))
  ;; 2. Check SSL_CERT_DIR environment variable
  (let ((cert-dir (get-environment-variable "SSL_CERT_DIR")))
    (when (and cert-dir (probe-file cert-dir))
      (return-from load-system-trust-store
        (make-trust-store-from-directory cert-dir))))
  ;; 3. Platform-specific paths
  (if (windows-p)
      (load-windows-trust-store)
      (load-unix-trust-store)))

(defun load-unix-trust-store ()
  "Load trust store from Unix/macOS default locations."
  ;; Try bundle files first
  (let ((paths '("/etc/ssl/certs/ca-certificates.crt"      ; Debian/Ubuntu
                 "/etc/pki/tls/certs/ca-bundle.crt"        ; RHEL/CentOS
                 "/etc/ssl/ca-bundle.pem"                   ; OpenSUSE
                 "/usr/local/share/certs/ca-root-nss.crt"  ; FreeBSD
                 "/etc/ssl/cert.pem"                        ; macOS
                 "/usr/local/etc/openssl/cert.pem"          ; Homebrew OpenSSL
                 "/usr/local/etc/openssl@1.1/cert.pem"      ; Homebrew OpenSSL 1.1
                 "/opt/homebrew/etc/openssl/cert.pem")))    ; Homebrew on Apple Silicon
    (dolist (path paths)
      (when (probe-file path)
        (return-from load-unix-trust-store
          (make-trust-store-from-sources path nil)))))
  ;; Try directory-based stores
  (let ((dirs '("/etc/ssl/certs"
                "/etc/pki/tls/certs")))
    (dolist (dir dirs)
      (when (probe-file dir)
        (return-from load-unix-trust-store
          (make-trust-store-from-directory dir)))))
  ;; No system store found
  (warn "Could not find system CA certificates. Set SSL_CERT_FILE environment variable or use :ca-file parameter.")
  (make-trust-store))

(defun load-windows-trust-store ()
  "Load trust store from common Windows CA bundle locations.
   Note: OpenSSL itself does not ship CA certificates. These paths are for
   tools that bundle Mozilla's CA certificates (Git, MSYS2, Cygwin)."
  (let ((paths '(;; Git for Windows (very common, includes Mozilla CA bundle)
                 "C:/Program Files/Git/mingw64/ssl/certs/ca-bundle.crt"
                 "C:/Program Files/Git/usr/ssl/certs/ca-bundle.crt"
                 "C:/Program Files (x86)/Git/mingw64/ssl/certs/ca-bundle.crt"
                 "C:/Program Files (x86)/Git/mingw32/ssl/certs/ca-bundle.crt"
                 ;; MSYS2 (includes ca-certificates package)
                 "C:/msys64/usr/ssl/certs/ca-bundle.crt"
                 "C:/msys64/mingw64/ssl/certs/ca-bundle.crt"
                 "C:/msys32/usr/ssl/certs/ca-bundle.crt"
                 ;; Cygwin (includes ca-certificates package)
                 "C:/cygwin64/usr/ssl/certs/ca-bundle.crt"
                 "C:/cygwin/usr/ssl/certs/ca-bundle.crt"
                 ;; Scoop curl package includes certs
                 "~/scoop/apps/curl/current/bin/curl-ca-bundle.crt"
                 "~/scoop/persist/curl/bin/curl-ca-bundle.crt")))
    (dolist (path paths)
      (let ((expanded-path (if (and (> (length path) 0)
                                    (char= (char path 0) #\~))
                               (merge-pathnames (subseq path 2)
                                                (user-homedir-pathname))
                               path)))
        (when (probe-file expanded-path)
          (return-from load-windows-trust-store
            (make-trust-store-from-sources (namestring expanded-path) nil))))))
  ;; No Windows CA bundle found
  (warn "Could not find CA certificates on Windows. ~
         Install Git for Windows or set SSL_CERT_FILE environment variable.")
  (make-trust-store))

(defun context-with-system-trust (context)
  "Return a new context with system trust store loaded."
  (let ((new-ctx (copy-structure context)))
    (setf (tls-context-trust-store new-ctx)
          (load-system-trust-store))
    new-ctx))
