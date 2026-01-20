;;; macos-verify.lisp --- macOS certificate chain verification via Security.framework
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Minimal CFFI bindings to macOS Security.framework for certificate chain
;;; verification using the system Keychain. This allows pure-tls to validate
;;; certificates against macOS trusted roots without bundling CA certificates.

(in-package #:pure-tls)

#+(and (or darwin macos) (not windows))
(progn

(cffi:define-foreign-library security-framework
  (t (:framework "Security")))

(cffi:define-foreign-library core-foundation
  (t (:framework "CoreFoundation")))

(cffi:use-foreign-library security-framework)
(cffi:use-foreign-library core-foundation)

;;; Constants

(defconstant +ksec-trust-result-proceed+ 1)
(defconstant +ksec-trust-result-unspecified+ 4)
(defconstant +err-sec-success+ 0)

;;; Revocation checking flags for SecPolicyCreateRevocation
(defconstant +ksec-revocation-ocsp-method+ (ash 1 0))        ; Use OCSP
(defconstant +ksec-revocation-crl-method+ (ash 1 1))         ; Use CRL
(defconstant +ksec-revocation-prefer-crl+ (ash 1 2))         ; Prefer CRL over OCSP
(defconstant +ksec-revocation-require-positive-response+ (ash 1 3)) ; Require definitive response
(defconstant +ksec-revocation-network-access-disabled+ (ash 1 4))   ; No network access
(defconstant +ksec-revocation-use-any-method+                ; OCSP or CRL
  (logior +ksec-revocation-ocsp-method+ +ksec-revocation-crl-method+))

;;; Core Foundation Functions

(cffi:defcfun ("CFRelease" %cf-release) :void
  (cf :pointer))

(cffi:defcfun ("CFDataCreate" %cf-data-create) :pointer
  (allocator :pointer)
  (bytes :pointer)
  (length :long))

(cffi:defcfun ("CFArrayCreate" %cf-array-create) :pointer
  (allocator :pointer)
  (values :pointer)
  (num-values :long)
  (callbacks :pointer))

(cffi:defcfun ("CFArrayGetCount" %cf-array-get-count) :long
  (array :pointer))

(cffi:defcfun ("CFArrayGetValueAtIndex" %cf-array-get-value-at-index) :pointer
  (array :pointer)
  (idx :long))

;;; Security Framework Functions

(cffi:defcfun ("SecCertificateCreateWithData" %sec-certificate-create) :pointer
  (allocator :pointer)
  (data :pointer))

(cffi:defcfun ("SecPolicyCreateSSL" %sec-policy-create-ssl) :pointer
  (server :boolean)
  (hostname :pointer))

(cffi:defcfun ("SecPolicyCreateRevocation" %sec-policy-create-revocation) :pointer
  (revocation-flags :uint32))

(cffi:defcfun ("SecTrustCreateWithCertificates" %sec-trust-create) :int32
  (certificates :pointer)
  (policies :pointer)
  (trust :pointer))

(cffi:defcfun ("SecTrustSetAnchorCertificates" %sec-trust-set-anchors) :int32
  (trust :pointer)
  (anchor-certificates :pointer))

(cffi:defcfun ("SecTrustSetAnchorCertificatesOnly" %sec-trust-set-anchors-only) :int32
  (trust :pointer)
  (anchors-only :boolean))

(cffi:defcfun ("SecTrustEvaluateWithError" %sec-trust-evaluate) :boolean
  (trust :pointer)
  (error :pointer))

(cffi:defcfun ("SecTrustGetTrustResult" %sec-trust-get-result) :int32
  (trust :pointer)
  (result :pointer))

;;; CFString for hostname

(cffi:defcfun ("CFStringCreateWithCString" %cf-string-create) :pointer
  (allocator :pointer)
  (cstring :string)
  (encoding :uint32))

(defconstant +kcf-string-encoding-utf8+ #x08000100)

;;; Error handling

(cffi:defcfun ("SecCopyErrorMessageString" %sec-copy-error-string) :pointer
  (status :int32)
  (reserved :pointer))

(cffi:defcfun ("CFStringGetCStringPtr" %cf-string-get-cstring-ptr) :pointer
  (string :pointer)
  (encoding :uint32))

(cffi:defcfun ("CFStringGetCString" %cf-string-get-cstring) :boolean
  (string :pointer)
  (buffer :pointer)
  (buffer-size :long)
  (encoding :uint32))

(cffi:defcfun ("CFErrorCopyDescription" %cf-error-copy-description) :pointer
  (cf-error :pointer))

(defun %cfstring-to-lisp (cf-string)
  "Convert a CFString to a Lisp string. Returns NIL if conversion fails."
  (when (and cf-string (not (cffi:null-pointer-p cf-string)))
    (let ((cstr (%cf-string-get-cstring-ptr cf-string +kcf-string-encoding-utf8+)))
      (if (and cstr (not (cffi:null-pointer-p cstr)))
          (cffi:foreign-string-to-lisp cstr)
          ;; Try the buffer method
          (cffi:with-foreign-object (buffer :char 512)
            (when (%cf-string-get-cstring cf-string buffer 512 +kcf-string-encoding-utf8+)
              (cffi:foreign-string-to-lisp buffer)))))))

(defun %get-cf-error-description (cf-error)
  "Get a human-readable description from a CFErrorRef."
  (when (and cf-error (not (cffi:null-pointer-p cf-error)))
    (let ((desc (%cf-error-copy-description cf-error)))
      (when (and desc (not (cffi:null-pointer-p desc)))
        (unwind-protect
             (%cfstring-to-lisp desc)
          (%cf-release desc))))))

(defun %get-security-error-message (status)
  "Get a human-readable error message for a Security framework status code."
  (let ((cf-string (%sec-copy-error-string status (cffi:null-pointer))))
    (if (cffi:null-pointer-p cf-string)
        (format nil "Security error: ~D" status)
        (unwind-protect
             (let ((cstr (%cf-string-get-cstring-ptr cf-string +kcf-string-encoding-utf8+)))
               (if (cffi:null-pointer-p cstr)
                   ;; Try the buffer method
                   (cffi:with-foreign-object (buffer :char 256)
                     (if (%cf-string-get-cstring cf-string buffer 256 +kcf-string-encoding-utf8+)
                         (cffi:foreign-string-to-lisp buffer)
                         (format nil "Security error: ~D" status)))
                   (cffi:foreign-string-to-lisp cstr)))
          (%cf-release cf-string)))))

;;; Helper Functions

(defun %create-cf-data (bytes)
  "Create a CFData object from a byte vector."
  (let ((count (length bytes)))
    (cffi:with-foreign-object (buf :uint8 count)
      (dotimes (i count)
        (setf (cffi:mem-aref buf :uint8 i) (aref bytes i)))
      (%cf-data-create (cffi:null-pointer) buf count))))

(defun %create-certificate (der-bytes)
  "Create a SecCertificateRef from DER-encoded bytes."
  (let ((cf-data (%create-cf-data der-bytes)))
    (when (cffi:null-pointer-p cf-data)
      (error "Failed to create CFData for certificate"))
    (unwind-protect
         (let ((cert (%sec-certificate-create (cffi:null-pointer) cf-data)))
           (when (cffi:null-pointer-p cert)
             (error "Failed to create SecCertificate from DER data"))
           cert)
      (%cf-release cf-data))))

(defun %create-certificate-array (der-certificates)
  "Create a CFArray of SecCertificateRef from a list of DER byte vectors.
Returns two values: the CFArray and a list of the created SecCertificateRefs.
Caller is responsible for releasing both the array AND each certificate
(CFArray with NULL callbacks does not retain its elements)."
  (let* ((count (length der-certificates))
         (certs nil))
    ;; Create certificates one by one, tracking for cleanup on error
    (unwind-protect
         (progn
           (dolist (der der-certificates)
             (let ((cert (%create-certificate der)))
               (push cert certs)))
           (setf certs (nreverse certs))
           ;; All certs created successfully, now create the array
           (cffi:with-foreign-object (cert-ptrs :pointer count)
             (loop for cert in certs
                   for i from 0
                   do (setf (cffi:mem-aref cert-ptrs :pointer i) cert))
             (let ((array (%cf-array-create (cffi:null-pointer)
                                             cert-ptrs
                                             count
                                             (cffi:null-pointer))))
               (when (cffi:null-pointer-p array)
                 (error "Failed to create certificate array"))
               ;; Success - return array and certs (caller must release both)
               (let ((result-certs certs))
                 (setf certs nil)  ; Prevent cleanup in unwind-protect
                 (values array result-certs)))))
      ;; Cleanup on error: release any certs we created
      (dolist (cert certs)
        (when cert
          (%cf-release cert))))))

;;; Public API

(defun verify-certificate-chain-macos (der-certificates hostname
                                       &key check-revocation trusted-roots
                                            (trust-anchor-mode :replace))
  "Verify a certificate chain using macOS Security.framework.

DER-CERTIFICATES is a list of DER-encoded certificate byte vectors,
with the end-entity (server) certificate first.

HOSTNAME if provided, enables hostname verification as part of the SSL policy.
When NIL, validates the certificate chain without hostname checking (useful for
client certificate verification in mTLS).

CHECK-REVOCATION if true, enables OCSP/CRL revocation checking via
Security.framework. Network access is required for revocation checks.

TRUSTED-ROOTS if provided, is a list of DER-encoded certificate byte vectors
to use as trust anchors.

TRUST-ANCHOR-MODE controls how trusted-roots interact with system Keychain:
  :replace - Use ONLY trusted-roots, ignore system Keychain (default)
  :extend - Use trusted-roots IN ADDITION TO system Keychain

Returns T if the chain is valid and trusted by macOS.
Signals an error with details on verification failure."
  (unless der-certificates
    (error "No certificates provided"))

  (let ((cert-array nil)
        (certs nil)
        (anchor-array nil)
        (anchor-certs nil)
        (ssl-policy nil)
        (revocation-policy nil)
        (policy-array nil)
        (trust nil)
        (hostname-cfstr nil))
    (unwind-protect
         (progn
           ;; Create certificate array
           (multiple-value-setq (cert-array certs)
             (%create-certificate-array der-certificates))

           ;; Create SSL policy (with optional hostname verification)
           ;; When hostname is nil, SecPolicyCreateSSL with NULL hostname
           ;; validates chain without hostname checking
           (when hostname
             (setf hostname-cfstr (%cf-string-create (cffi:null-pointer)
                                                      hostname
                                                      +kcf-string-encoding-utf8+))
             (when (cffi:null-pointer-p hostname-cfstr)
               (error "Failed to create CFString for hostname")))

           (setf ssl-policy (%sec-policy-create-ssl t (or hostname-cfstr
                                                          (cffi:null-pointer))))
           (when (cffi:null-pointer-p ssl-policy)
             (error "Failed to create SSL policy"))

           ;; Create revocation policy if requested
           (when check-revocation
             (setf revocation-policy
                   (%sec-policy-create-revocation +ksec-revocation-use-any-method+))
             (when (cffi:null-pointer-p revocation-policy)
               (error "Failed to create revocation policy")))

           ;; Create policy array (SSL + optional revocation)
           (let ((policy-count (if revocation-policy 2 1)))
             (cffi:with-foreign-object (policy-ptrs :pointer policy-count)
               (setf (cffi:mem-aref policy-ptrs :pointer 0) ssl-policy)
               (when revocation-policy
                 (setf (cffi:mem-aref policy-ptrs :pointer 1) revocation-policy))
               (setf policy-array (%cf-array-create (cffi:null-pointer)
                                                     policy-ptrs
                                                     policy-count
                                                     (cffi:null-pointer)))
               (when (cffi:null-pointer-p policy-array)
                 (error "Failed to create policy array"))))

           ;; Create trust object
           (cffi:with-foreign-object (trust-ptr :pointer)
             (let ((status (%sec-trust-create cert-array policy-array trust-ptr)))
               (unless (zerop status)
                 (error 'tls-certificate-error
                        :format-control "Failed to create SecTrust: ~A"
                        :format-arguments (list (%get-security-error-message status))))
               (setf trust (cffi:mem-aref trust-ptr :pointer))))

           ;; Configure trust anchors based on trust-anchor-mode
           (if trusted-roots
               (progn
                 (multiple-value-setq (anchor-array anchor-certs)
                   (%create-certificate-array trusted-roots))
                 (let ((status (%sec-trust-set-anchors trust anchor-array)))
                   (unless (zerop status)
                     (error 'tls-certificate-error
                            :format-control "Failed to set anchor certificates: ~A"
                            :format-arguments (list (%get-security-error-message status)))))
                 ;; Control whether to use ONLY custom anchors or extend system roots
                 (ecase trust-anchor-mode
                   (:replace (%sec-trust-set-anchors-only trust t))   ; Custom only
                   (:extend (%sec-trust-set-anchors-only trust nil)))) ; Custom + system
               ;; No custom roots - use system anchors
               (%sec-trust-set-anchors-only trust nil))

           ;; Evaluate trust
           (cffi:with-foreign-object (error-ptr :pointer)
             (setf (cffi:mem-aref error-ptr :pointer) (cffi:null-pointer))
             (let ((trusted (%sec-trust-evaluate trust error-ptr)))
               (unless trusted
                 ;; Get the error details from CFErrorRef
                 (let* ((cf-error (cffi:mem-aref error-ptr :pointer))
                        (error-desc (when (and cf-error (not (cffi:null-pointer-p cf-error)))
                                      (prog1 (%get-cf-error-description cf-error)
                                        (%cf-release cf-error)))))
                   (error 'tls-certificate-error
                          :format-control "macOS certificate verification failed: ~A"
                          :format-arguments (list (or error-desc "unknown error")))))))

           ;; Success
           t)

      ;; Cleanup
      (when trust
        (%cf-release trust))
      (when policy-array
        (%cf-release policy-array))
      (when revocation-policy
        (%cf-release revocation-policy))
      (when ssl-policy
        (%cf-release ssl-policy))
      (when hostname-cfstr
        (%cf-release hostname-cfstr))
      (when anchor-array
        (%cf-release anchor-array))
      ;; Release anchor certs
      (dolist (cert anchor-certs)
        (when cert
          (%cf-release cert)))
      (when cert-array
        (%cf-release cert-array))
      ;; Release individual certs (CFArray doesn't retain with NULL callbacks)
      (dolist (cert certs)
        (when cert
          (%cf-release cert))))))) ; end #+(and (or darwin macos) (not windows)) progn
