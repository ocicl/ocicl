;;; store.lisp --- Certificate storage abstraction for ACME
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Certificate and account key storage with proper directory layout.

(in-package #:pure-tls/acme)

;;; ----------------------------------------------------------------------------
;;; Platform-specific paths
;;; ----------------------------------------------------------------------------

(defun default-cert-store-path ()
  "Get the default certificate store path.
   - Windows: %LOCALAPPDATA%/pure-tls/
   - macOS: ~/Library/Application Support/pure-tls/
   - Linux/Unix: XDG_STATE_HOME/pure-tls/ or ~/.local/state/pure-tls/"
  #+windows
  (let ((localappdata (uiop:getenv "LOCALAPPDATA")))
    (if localappdata
        (merge-pathnames "pure-tls/" (pathname (concatenate 'string localappdata "/")))
        (merge-pathnames "pure-tls/" (user-homedir-pathname))))
  #+(or darwin macos)
  (merge-pathnames "Library/Application Support/pure-tls/" (user-homedir-pathname))
  #-(or windows darwin macos)
  (let ((xdg (uiop:getenv "XDG_STATE_HOME")))
    (if (and xdg (> (length xdg) 0))
        (merge-pathnames "pure-tls/" (pathname (concatenate 'string xdg "/")))
        (merge-pathnames ".local/state/pure-tls/" (user-homedir-pathname)))))

;;; ----------------------------------------------------------------------------
;;; Certificate Store Structure
;;; ----------------------------------------------------------------------------

(defstruct (cert-store (:constructor %make-cert-store))
  "Certificate and account key storage manager.

   Directory layout:
     <base-path>/
       account/
         account.key     - EC P-256 account private key (PEM)
       domains/
         <domain>/
           privkey.pem   - Domain RSA private key
           fullchain.pem - Certificate chain"
  (base-path nil :type (or null pathname))
  (account-key nil)       ; Cached EC P-256 private key
  (account-url nil :type (or null string)))  ; Cached account URL

(defun make-cert-store (&key path)
  "Create a certificate store.
   PATH defaults to XDG_STATE_HOME/pure-tls/"
  (let ((base (or path (default-cert-store-path))))
    (%make-cert-store :base-path (pathname base))))

;;; ----------------------------------------------------------------------------
;;; Path Helpers
;;; ----------------------------------------------------------------------------

(defun store-account-dir (store)
  "Get the account directory path."
  (merge-pathnames "account/" (cert-store-base-path store)))

(defun store-account-key-path (store)
  "Get the account key file path."
  (merge-pathnames "account.key" (store-account-dir store)))

(defun store-domains-dir (store)
  "Get the domains directory path."
  (merge-pathnames "domains/" (cert-store-base-path store)))

(defun store-domain-dir (store domain)
  "Get the directory for a specific domain."
  (merge-pathnames (format nil "~A/" domain) (store-domains-dir store)))

(defun store-domain-key-path (store domain)
  "Get the private key path for a domain."
  (merge-pathnames "privkey.pem" (store-domain-dir store domain)))

(defun store-domain-cert-path (store domain)
  "Get the certificate chain path for a domain."
  (merge-pathnames "fullchain.pem" (store-domain-dir store domain)))

;;; ----------------------------------------------------------------------------
;;; Directory and Permission Management
;;; ----------------------------------------------------------------------------

(defun ensure-directory-with-mode (path mode)
  "Ensure directory exists with specified mode (Unix only).
   PATH can be either a file path (ensures parent dir) or directory path (ensures that dir)."
  ;; Get the directory path string - works for both file and directory pathnames
  (let* ((dir-namestring (directory-namestring path))
         ;; Create a file pathname within that directory to force creation
         (file-path (merge-pathnames "x" (pathname dir-namestring))))
    ;; ensure-directories-exist creates all directories for a file path
    (ensure-directories-exist file-path)
    #+sbcl
    ;; Remove trailing slash for chmod if present (sb-posix:chmod doesn't always need it)
    (let ((dir-path (string-right-trim "/" dir-namestring)))
      (sb-posix:chmod dir-path mode))
    #-sbcl
    (declare (ignore mode))))

(defun write-key-file (path content)
  "Write a key file with restrictive permissions (0600)."
  (ensure-directory-with-mode path #o700)
  (with-open-file (out path :direction :output
                            :if-exists :supersede
                            :element-type 'character)
    (write-string content out))
  #+sbcl (sb-posix:chmod (namestring path) #o600))

;;; ----------------------------------------------------------------------------
;;; Account Key Management
;;; ----------------------------------------------------------------------------

(defun store-load-account-key (store)
  "Load account key from store, generating if necessary.
   Caches the key in the store structure."
  (when (cert-store-account-key store)
    (return-from store-load-account-key (cert-store-account-key store)))
  (let ((key-path (store-account-key-path store)))
    (if (probe-file key-path)
        ;; Load existing key
        (let ((key (load-ec-private-key key-path)))
          (setf (cert-store-account-key store) key)
          key)
        ;; Generate new key
        (let ((key (ironclad:generate-key-pair :secp256r1)))
          (save-ec-private-key key key-path)
          (setf (cert-store-account-key store) key)
          key))))

(defun load-ec-private-key (path)
  "Load an EC private key from a PEM file."
  (let* ((pem-text (alexandria:read-file-into-string path))
         (der (extract-pem-der pem-text "EC PRIVATE KEY")))
    (parse-ec-private-key-der der)))

(defun extract-pem-der (pem-text label)
  "Extract DER bytes from PEM text with given label."
  (let* ((begin-marker (format nil "-----BEGIN ~A-----" label))
         (end-marker (format nil "-----END ~A-----" label))
         (begin-pos (search begin-marker pem-text))
         (end-pos (search end-marker pem-text)))
    (unless (and begin-pos end-pos)
      (error "PEM block '~A' not found" label))
    (let* ((b64-start (+ begin-pos (length begin-marker)))
           (b64-text (subseq pem-text b64-start end-pos))
           (clean-b64 (remove-if (lambda (c)
                                   (member c '(#\Newline #\Return #\Space #\Tab)))
                                 b64-text)))
      (cl-base64:base64-string-to-usb8-array clean-b64))))

(defun parse-ec-private-key-der (der)
  "Parse DER-encoded EC private key (SEC 1 format).
   ECPrivateKey ::= SEQUENCE {
     version INTEGER { ecPrivkeyVer1(1) },
     privateKey OCTET STRING,
     parameters [0] ECParameters OPTIONAL,
     publicKey [1] BIT STRING OPTIONAL }"
  (let* ((root (pure-tls::parse-der der))
         (children (pure-tls::asn1-children root))
         (private-key-bytes (pure-tls::asn1-node-value (second children))))
    ;; Convert private key bytes to ironclad key
    (ironclad:make-private-key :secp256r1 :x private-key-bytes)))

(defun save-ec-private-key (private-key path)
  "Save an EC private key to PEM file."
  (let* ((key-data (ironclad:destructure-private-key private-key))
         (d (getf key-data :x))  ; Private scalar
         (public-point (getf key-data :y))  ; Public point (uncompressed)
         (der (encode-ec-private-key-der d public-point))
         (pem (wrap-pem "EC PRIVATE KEY" der)))
    (write-key-file path pem)))

(defun encode-ec-private-key-der (d public-point)
  "Encode EC private key to DER (SEC 1 format)."
  ;; ECPrivateKey ::= SEQUENCE {
  ;;   version INTEGER (1),
  ;;   privateKey OCTET STRING,
  ;;   parameters [0] EXPLICIT namedCurve OPTIONAL,
  ;;   publicKey [1] EXPLICIT BIT STRING OPTIONAL }
  (encode-sequence
   (encode-integer 1)  ; version
   (encode-octet-string d)  ; privateKey
   (encode-context-tag 0 (encode-oid *oid-secp256r1*))  ; parameters
   (encode-context-tag 1 (encode-bit-string public-point))))  ; publicKey

;;; ----------------------------------------------------------------------------
;;; Domain Certificate Management
;;; ----------------------------------------------------------------------------

(defun store-has-certificate-p (store domain)
  "Check if store has both certificate and key for domain."
  (and (probe-file (store-domain-cert-path store domain))
       (probe-file (store-domain-key-path store domain))))

(defun store-save-certificate (store domain cert-pem key-pem)
  "Save certificate chain and private key for domain."
  (let ((cert-path (store-domain-cert-path store domain))
        (key-path (store-domain-key-path store domain)))
    ;; Ensure domain directory exists with proper permissions
    (ensure-directory-with-mode (store-domain-dir store domain) #o700)
    ;; Save certificate (world-readable is OK)
    (with-open-file (out cert-path :direction :output
                                   :if-exists :supersede)
      (write-string cert-pem out))
    ;; Save key with restrictive permissions
    (write-key-file key-path key-pem)))

(defun store-load-certificate (store domain)
  "Load certificate chain for domain.
   Returns list of x509-certificate objects."
  (let ((cert-path (store-domain-cert-path store domain)))
    (when (probe-file cert-path)
      (pure-tls:load-certificate-chain (namestring cert-path)))))

(defun store-certificate-expires-soon-p (store domain &optional (days 30))
  "Check if domain's certificate expires within DAYS days."
  (certificate-expires-soon-p (store-domain-cert-path store domain) days))
