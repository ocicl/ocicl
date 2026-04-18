;;; windows-verify.lisp --- Windows certificate chain verification via CryptoAPI
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Minimal CFFI bindings to Windows CryptoAPI for certificate chain
;;; verification using the system certificate store. This allows pure-tls
;;; to validate certificates against Windows trusted roots without bundling
;;; CA certificates.

(in-package #:pure-tls)

#+windows
(progn

(cffi:define-foreign-library crypt32
  (:windows "Crypt32.dll"))

(cffi:use-foreign-library crypt32)

;;; Constants

(defconstant +x509-asn-encoding+ #x1)
(defconstant +cert-chain-policy-ssl+ 4)
(defconstant +authtype-server+ 2)
(defconstant +authtype-client+ 1)
(defconstant +cert-chain-revocation-check-chain-exclude-root+ #x40000000)
(defconstant +cert-store-prov-memory+ 2)
(defconstant +cert-store-add-replace-existing+ 3)
(defconstant +cert-store-create-new-flag+ #x2000)

;;; Structures
;;; Note: Windows BOOL is 32-bit, not 8-bit boolean

(cffi:defcstruct cert-context
  (encoding-type :uint32)
  (encoded :pointer)
  (cencoded :uint32)
  (info :pointer)
  (store :pointer))

(cffi:defcstruct cert-trust-status
  (error-status :uint32)
  (info-status :uint32))

(cffi:defcstruct cert-chain-context
  (size :uint32)
  (trust-status (:struct cert-trust-status))
  (num-chains :uint32)
  (chains :pointer)
  (num-lower-quality :uint32)
  (lower-quality-chains :pointer)
  (has-revocation-freshness-time :int32)  ; Windows BOOL is 32-bit
  (revocation-freshness-time :uint32)
  (create-flags :uint32)
  (chain-id :uint8 :count 16))

;; CERT_ENHKEY_USAGE / CTL_USAGE
(cffi:defcstruct cert-enhkey-usage
  (usage-identifier-count :uint32)
  (usage-identifiers :pointer))

;; CERT_USAGE_MATCH embeds CERT_ENHKEY_USAGE
(cffi:defcstruct cert-usage-match
  (type :uint32)
  (usage (:struct cert-enhkey-usage)))

;; CERT_CHAIN_PARA (full struct per Microsoft Learn)
(cffi:defcstruct cert-chain-para
  (size :uint32)
  (requested-usage (:struct cert-usage-match))
  (requested-issuance-policy (:struct cert-usage-match))
  (url-retrieval-timeout :uint32)
  (check-revocation-freshness-time :int32)  ; BOOL (32-bit)
  (revocation-freshness-time :uint32)
  (cache-resync :pointer)        ; LPFILETIME
  (strong-sign-para :pointer)    ; PCCERT_STRONG_SIGN_PARA
  (strong-sign-flags :uint32))

;; SSL_EXTRA_CERT_CHAIN_POLICY_PARA / HTTPSPolicyCallbackData
;; Note: No pszUsageIdentifier for SSL policy (ends at pwszServerName)
(cffi:defcstruct ssl-extra-cert-chain-policy-para
  (size :uint32)
  (auth-type :uint32)
  (checks :uint32)
  (server-name :pointer))

(cffi:defcstruct cert-chain-policy-para
  (size :uint32)
  (flags :uint32)
  (extra-policy-para :pointer))

(cffi:defcstruct cert-chain-policy-status
  (size :uint32)
  (error :uint32)
  (chain-index :int32)
  (element-index :int32)
  (extra-policy-status :pointer))

;; CERT_CHAIN_ENGINE_CONFIG for creating custom chain engines
;; This structure allows specifying exclusive trust anchors (Windows 7+)
(cffi:defcstruct cert-chain-engine-config
  (size :uint32)
  (hrestricted-root :pointer)           ; HCERTSTORE - restricted root store
  (hrestricted-trust :pointer)          ; HCERTSTORE - restricted trust store
  (hrestricted-other :pointer)          ; HCERTSTORE - restricted other store
  (cadditional-store :uint32)           ; count of additional stores
  (rghadditional-store :pointer)        ; array of HCERTSTORE
  (flags :uint32)                       ; CERT_CHAIN_ENGINE_CONFIG flags
  (url-retrieval-timeout :uint32)       ; milliseconds
  (max-cached-certs :uint32)            ; MaximumCachedCertificates
  (cycle-detection-modulus :uint32)     ; CycleDetectionModulus
  (hexclusive-root :pointer)            ; HCERTSTORE - Windows 7+: exclusive root store
  (hexclusive-trusted-people :pointer)  ; HCERTSTORE - Windows 8+
  (exclusive-flags :uint32))            ; Windows 8.1+

;;; Foreign Functions

(cffi:defcfun ("CertOpenStore" %cert-open-store) :pointer
  (store-prov :pointer)
  (encoding :uint32)
  (hprov :pointer)
  (flags :uint32)
  (para :pointer))

(cffi:defcfun ("CertCloseStore" %cert-close-store) :boolean
  (hstore :pointer)
  (flags :uint32))

(cffi:defcfun ("CertCreateCertificateContext" %cert-create-context) :pointer
  (encoding-type :uint32)
  (encoded :pointer)
  (count :uint32))

(cffi:defcfun ("CertFreeCertificateContext" %cert-free-context) :boolean
  (hcert :pointer))

(cffi:defcfun ("CertAddCertificateContextToStore" %cert-add-to-store) :boolean
  (hstore :pointer)
  (hcert :pointer)
  (flags :uint32)
  (phcert :pointer))

(cffi:defcfun ("CertGetCertificateChain" %cert-get-chain) :boolean
  (chain-engine :pointer)
  (cert-context :pointer)
  (time :pointer)
  (additional-store :pointer)
  (chain-para :pointer)
  (flags :uint32)
  (reserved :pointer)
  (chain-context :pointer))

(cffi:defcfun ("CertVerifyCertificateChainPolicy" %cert-verify-policy) :boolean
  (policy-oid :pointer)
  (chain-context :pointer)
  (policy-para :pointer)
  (policy-status :pointer))

(cffi:defcfun ("CertFreeCertificateChain" %cert-free-chain) :void
  (chain-context :pointer))

(cffi:defcfun ("CertCreateCertificateChainEngine" %cert-create-chain-engine) :int
  (config :pointer)
  (engine :pointer))

(cffi:defcfun ("CertFreeCertificateChainEngine" %cert-free-chain-engine) :void
  (engine :pointer))

;;; Helper Functions

(defun %memset (ptr count &optional (val 0))
  "Zero out COUNT bytes at PTR."
  (dotimes (i count)
    (setf (cffi:mem-aref ptr :uint8 i) val)))

(defun %open-memory-store ()
  "Open an in-memory certificate store."
  (let ((hstore (%cert-open-store (cffi:make-pointer +cert-store-prov-memory+)
                                   0
                                   (cffi:null-pointer)
                                   +cert-store-create-new-flag+
                                   (cffi:null-pointer))))
    (when (cffi:null-pointer-p hstore)
      (error "Failed to open memory certificate store"))
    hstore))

(defun %create-cert-context (der-bytes)
  "Create a certificate context from DER-encoded bytes."
  (let ((count (length der-bytes)))
    (cffi:with-foreign-object (buf :uint8 count)
      (dotimes (i count)
        (setf (cffi:mem-aref buf :uint8 i) (aref der-bytes i)))
      (let ((hcert (%cert-create-context +x509-asn-encoding+ buf count)))
        (when (cffi:null-pointer-p hcert)
          (error "Failed to create certificate context"))
        hcert))))

(defun %create-exclusive-root-store (trusted-roots-der)
  "Create an in-memory certificate store containing only the specified roots.
Returns the HCERTSTORE handle. Caller must close with %cert-close-store."
  (let ((hstore (%open-memory-store)))
    (handler-case
        (progn
          (dolist (der trusted-roots-der)
            (let ((ctx (%create-cert-context der)))
              (unless (%cert-add-to-store hstore ctx +cert-store-add-replace-existing+
                                           (cffi:null-pointer))
                (%cert-free-context ctx)
                (error "Failed to add trusted root to exclusive store"))
              (%cert-free-context ctx)))
          hstore)
      (error (c)
        (%cert-close-store hstore 0)
        (error c)))))

(defun %create-custom-chain-engine (exclusive-root-store)
  "Create a custom certificate chain engine that uses ONLY the specified root store.
Returns the HCERTCHAINENGINE handle. Caller must free with %cert-free-chain-engine.
EXCLUSIVE-ROOT-STORE is an HCERTSTORE containing the exclusive trust anchors."
  (cffi:with-foreign-objects ((config '(:struct cert-chain-engine-config))
                              (engine-ptr :pointer))
    (%memset config (cffi:foreign-type-size '(:struct cert-chain-engine-config)))
    (setf (cffi:foreign-slot-value config '(:struct cert-chain-engine-config) 'size)
          (cffi:foreign-type-size '(:struct cert-chain-engine-config)))
    ;; Set the exclusive root store - this makes the engine use ONLY these roots
    (setf (cffi:foreign-slot-value config '(:struct cert-chain-engine-config) 'hexclusive-root)
          exclusive-root-store)
    (when (zerop (%cert-create-chain-engine config engine-ptr))
      (error "Failed to create custom certificate chain engine"))
    (cffi:mem-aref engine-ptr :pointer)))

;;; Public API

(defun verify-certificate-chain-windows (der-certificates hostname
                                         &key check-revocation trusted-roots
                                              (trust-anchor-mode :extend))
  "Verify a certificate chain using Windows CryptoAPI.

DER-CERTIFICATES is a list of DER-encoded certificate byte vectors,
with the end-entity (server) certificate first.

HOSTNAME if provided, enables hostname verification as part of the SSL policy.
When NIL, validates the certificate chain without hostname checking (useful for
client certificate verification in mTLS).

CHECK-REVOCATION if true, enables CRL/OCSP revocation checking (slower).

TRUSTED-ROOTS if provided, is a list of DER-encoded certificate byte vectors
to use as trust anchors.

TRUST-ANCHOR-MODE controls how trusted-roots interact with the system store:
  :extend - Use trusted-roots IN ADDITION TO system store (default)
  :replace - Use ONLY trusted-roots, ignoring system store (requires Windows 7+)

Returns T if the chain is valid and trusted by Windows.
Signals an error with details on verification failure."
  (declare (ignorable trusted-roots))  ; Added to store below
  (unless der-certificates
    (error "No certificates provided"))

  (let ((hstore (%open-memory-store))
        (hcert nil)
        (chain-context nil)
        (custom-engine nil)
        (exclusive-root-store nil))
    (unwind-protect
         (progn
           ;; Handle trust anchors based on trust-anchor-mode
           (ecase trust-anchor-mode
             (:extend
              ;; Add custom trusted roots to the memory store (extends system trust)
              (dolist (der trusted-roots)
                (let ((ctx (%create-cert-context der)))
                  (unless (%cert-add-to-store hstore ctx +cert-store-add-replace-existing+
                                               (cffi:null-pointer))
                    (%cert-free-context ctx)
                    (error "Failed to add trusted root to store"))
                  (%cert-free-context ctx))))
             (:replace
              ;; Create custom chain engine with exclusive roots (replaces system trust)
              (when trusted-roots
                (setf exclusive-root-store (%create-exclusive-root-store trusted-roots))
                (setf custom-engine (%create-custom-chain-engine exclusive-root-store)))))

           ;; Add all certificates to the memory store
           (dolist (der der-certificates)
             (let ((ctx (%create-cert-context der)))
               (unless hcert
                 (setf hcert ctx))  ; Keep first cert for chain building
               (unless (%cert-add-to-store hstore ctx +cert-store-add-replace-existing+
                                            (cffi:null-pointer))
                 (unless (cffi:pointer-eq ctx hcert)
                   (%cert-free-context ctx))
                 (error "Failed to add certificate to store"))
               (unless (cffi:pointer-eq ctx hcert)
                 (%cert-free-context ctx))))

           ;; Build the certificate chain
           ;; Use custom-engine if :replace mode, otherwise use default (null)
           (cffi:with-foreign-objects ((chain-para '(:struct cert-chain-para))
                                       (chain-ptr :pointer))
             (%memset chain-para (cffi:foreign-type-size '(:struct cert-chain-para)))
             (setf (cffi:foreign-slot-value chain-para '(:struct cert-chain-para) 'size)
                   (cffi:foreign-type-size '(:struct cert-chain-para)))

             (unless (%cert-get-chain (or custom-engine (cffi:null-pointer))
                                       hcert
                                       (cffi:null-pointer)
                                       hstore
                                       chain-para
                                       (if check-revocation
                                           +cert-chain-revocation-check-chain-exclude-root+
                                           0)
                                       (cffi:null-pointer)
                                       chain-ptr)
               (error "Failed to build certificate chain"))
             (setf chain-context (cffi:mem-aref chain-ptr :pointer)))

           ;; Verify against SSL policy (with optional hostname check)
           (cffi:with-foreign-objects ((ssl-extra '(:struct ssl-extra-cert-chain-policy-para))
                                       (policy-para '(:struct cert-chain-policy-para))
                                       (policy-status '(:struct cert-chain-policy-status)))
             (flet ((do-verify (server-name-ptr)
                      (%memset ssl-extra (cffi:foreign-type-size '(:struct ssl-extra-cert-chain-policy-para)))
                      (setf (cffi:foreign-slot-value ssl-extra '(:struct ssl-extra-cert-chain-policy-para) 'size)
                            (cffi:foreign-type-size '(:struct ssl-extra-cert-chain-policy-para))
                            (cffi:foreign-slot-value ssl-extra '(:struct ssl-extra-cert-chain-policy-para) 'auth-type)
                            +authtype-server+
                            (cffi:foreign-slot-value ssl-extra '(:struct ssl-extra-cert-chain-policy-para) 'checks)
                            0
                            (cffi:foreign-slot-value ssl-extra '(:struct ssl-extra-cert-chain-policy-para) 'server-name)
                            server-name-ptr)

                      (%memset policy-para (cffi:foreign-type-size '(:struct cert-chain-policy-para)))
                      (setf (cffi:foreign-slot-value policy-para '(:struct cert-chain-policy-para) 'size)
                            (cffi:foreign-type-size '(:struct cert-chain-policy-para))
                            (cffi:foreign-slot-value policy-para '(:struct cert-chain-policy-para) 'extra-policy-para)
                            ssl-extra)

                      (%memset policy-status (cffi:foreign-type-size '(:struct cert-chain-policy-status)))
                      (setf (cffi:foreign-slot-value policy-status '(:struct cert-chain-policy-status) 'size)
                            (cffi:foreign-type-size '(:struct cert-chain-policy-status)))

                      (unless (%cert-verify-policy (cffi:make-pointer +cert-chain-policy-ssl+)
                                                    chain-context
                                                    policy-para
                                                    policy-status)
                        (error "CertVerifyCertificateChainPolicy call failed"))

                      (let ((err (cffi:foreign-slot-value policy-status '(:struct cert-chain-policy-status) 'error)))
                        (unless (zerop err)
                          (error 'tls-certificate-error
                                 :format-control "Windows certificate verification failed: ~A"
                                 :format-arguments (list (%decode-cert-error err))))
                        t)))
               ;; When hostname provided, create foreign string; otherwise use null pointer
               (if hostname
                   (cffi:with-foreign-string (whostname hostname :encoding :utf-16le)
                     (do-verify whostname))
                   (do-verify (cffi:null-pointer))))))

      ;; Cleanup (order matters: engine before its root store)
      (when chain-context
        (%cert-free-chain chain-context))
      (when custom-engine
        (%cert-free-chain-engine custom-engine))
      (when exclusive-root-store
        (%cert-close-store exclusive-root-store 0))
      (when hcert
        (%cert-free-context hcert))
      (%cert-close-store hstore 0))))

(defun %decode-cert-error (code)
  "Decode Windows certificate error code to human-readable string."
  (case code
    (#x800B0101 "CERT_E_EXPIRED - Certificate has expired")
    (#x800B0102 "CERT_E_VALIDITYPERIODNESTING - Validity period nesting error")
    (#x800B0103 "CERT_E_ROLE - Certificate role error")
    (#x800B0104 "CERT_E_PATHLENCONST - Path length constraint exceeded")
    (#x800B0105 "CERT_E_CRITICAL - Unhandled critical extension")
    (#x800B0106 "CERT_E_PURPOSE - Certificate not valid for requested usage")
    (#x800B0107 "CERT_E_ISSUERCHAINING - Parent certificate invalid")
    (#x800B0108 "CERT_E_MALFORMED - Certificate is malformed")
    (#x800B0109 "CERT_E_UNTRUSTEDROOT - Untrusted root certificate")
    (#x800B010A "CERT_E_CHAINING - Certificate chain error")
    (#x800B010B "CERT_E_REVOKED - Certificate has been revoked")
    (#x800B010C "CERT_E_UNTRUSTEDTESTROOT - Untrusted test root")
    (#x800B010D "CERT_E_REVOCATION_FAILURE - Revocation check failed")
    (#x800B010E "CERT_E_CN_NO_MATCH - Certificate CN does not match hostname")
    (#x800B010F "CERT_E_WRONG_USAGE - Certificate wrong usage")
    (otherwise (format nil "Error code: #x~X" code))))) ; end #+windows progn
