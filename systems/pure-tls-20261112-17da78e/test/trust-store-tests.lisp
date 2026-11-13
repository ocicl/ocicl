;;; test/trust-store-tests.lisp --- Trust store auto-load regression tests
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Regression tests for system trust-store auto-loading in MAKE-TLS-CONTEXT.
;;;
;;; Bug: MAKE-TLS-CONTEXT auto-loaded the system CA store only for
;;; +VERIFY-REQUIRED+, leaving a +VERIFY-PEER+ context with an empty trust
;;; store.  +VERIFY-PEER+ still verifies a presented certificate (servers
;;; always present one), so such a context failed every chain with UNKNOWN-CA.
;;; This bit the cl+ssl compatibility layer, whose default context is
;;; +VERIFY-PEER+ -- e.g. drakma-based HTTPS (ocicl's self-update path) failed
;;; on Linux with "No trusted root certificates available for verification".
;;;
;;; These tests pin the trust source with SSL_CERT_FILE (a fixture bundle) so
;;; they do not depend on the host's real system trust store.

(in-package #:pure-tls/test)

(def-suite trust-store-tests
  :description "System trust-store auto-loading in make-tls-context")

(in-suite trust-store-tests)

(defmacro with-ssl-cert-file ((path) &body body)
  "Run BODY with the SSL_CERT_FILE environment variable bound to PATH,
restoring the previous value afterwards.

Uses (SETF UIOP:GETENV) rather than SB-POSIX so this file loads on any
implementation ASDF supports: UIOP's setf expander already unsets the variable
when the new value is NIL, which is exactly what restoring an absent
SSL_CERT_FILE needs."
  (let ((old (gensym)) (p (gensym)))
    `(let ((,old (uiop:getenv "SSL_CERT_FILE"))
           (,p (namestring ,path)))
       (unwind-protect
            (progn (setf (uiop:getenv "SSL_CERT_FILE") ,p) ,@body)
         (setf (uiop:getenv "SSL_CERT_FILE") ,old)))))

(defun context-root-count (ctx)
  "Number of trusted roots loaded into CTX (0 when the trust store is NIL)."
  (let ((ts (pure-tls::tls-context-trust-store ctx)))
    (if ts (length (pure-tls::trust-store-certificates ts)) 0)))

(test verify-peer-auto-loads-system-roots
  "+verify-peer+ must auto-load the system trust store (UNKNOWN-CA regression)."
  (with-ssl-cert-file ((test-cert-path "self-signed-valid.pem"))
    (let ((ctx (pure-tls:make-tls-context :verify-mode pure-tls:+verify-peer+)))
      (is (plusp (context-root-count ctx))
          "+verify-peer+ context has an empty trust store; a presented ~
           certificate would fail with UNKNOWN-CA"))))

(test verify-required-auto-loads-system-roots
  "+verify-required+ still auto-loads the system trust store."
  (with-ssl-cert-file ((test-cert-path "self-signed-valid.pem"))
    (let ((ctx (pure-tls:make-tls-context :verify-mode pure-tls:+verify-required+)))
      (is (plusp (context-root-count ctx))))))

(test verify-none-does-not-auto-load
  "+verify-none+ must NOT auto-load a trust store."
  (with-ssl-cert-file ((test-cert-path "self-signed-valid.pem"))
    (let ((ctx (pure-tls:make-tls-context :verify-mode pure-tls:+verify-none+)))
      (is (zerop (context-root-count ctx))))))

(test stream-verify-falls-back-to-system-roots
  "cl+ssl puts the verify mode on the STREAM, not the context: drakma builds
its context with +ssl-verify-none+ and then asks make-ssl-client-stream to
verify.  A context that does not verify therefore still has to yield system
roots to a stream that does, or every chain fails with UNKNOWN-CA."
  (with-ssl-cert-file ((test-cert-path "self-signed-valid.pem"))
    (let ((ctx (pure-tls:make-tls-context :verify-mode pure-tls:+verify-none+)))
      (is (zerop (context-root-count ctx))
          "the context itself must still not auto-load; that policy is deliberate")
      (dolist (verify (list pure-tls:+verify-peer+ pure-tls:+verify-required+))
        (let ((store (pure-tls::effective-trust-store ctx verify)))
          (is (and store (plusp (length (pure-tls::trust-store-certificates store))))
              "a stream asked to verify got no roots from a verify-none context"))))))

(test stream-without-verify-gets-no-roots
  "A stream that does not verify must not quietly acquire a trust store."
  (with-ssl-cert-file ((test-cert-path "self-signed-valid.pem"))
    (let ((ctx (pure-tls:make-tls-context :verify-mode pure-tls:+verify-none+)))
      (is (null (pure-tls::effective-trust-store ctx pure-tls:+verify-none+))))))

(test stream-verify-keeps-the-context-store
  "An explicit trust store on the context wins; the fallback only fills a gap."
  (let ((ctx (pure-tls:make-tls-context
              :verify-mode pure-tls:+verify-peer+
              :ca-file (namestring (test-cert-path "self-signed-valid.pem")))))
    (is (eq (pure-tls::tls-context-trust-store ctx)
            (pure-tls::effective-trust-store ctx pure-tls:+verify-required+)))))

(test explicit-ca-file-overrides-auto-load
  "An explicit :ca-file is honored regardless of verify mode."
  (let ((ctx (pure-tls:make-tls-context
              :verify-mode pure-tls:+verify-peer+
              :ca-file (namestring (test-cert-path "self-signed-valid.pem")))))
    (is (plusp (context-root-count ctx)))))

;;; Trust stores built from CA files are cached per source and write date.
;;; Drakma makes a new context for every connection, so without the cache each
;;; connection re-parsed the whole CA bundle before its ClientHello.

(defmacro with-ca-file-copy ((var) &body body)
  "Run BODY with VAR bound to a fresh copy of the self-signed fixture."
  `(uiop:with-temporary-file (:pathname ,var :type "pem")
     (uiop:copy-file (test-cert-path "self-signed-valid.pem") ,var)
     ,@body))

;;; Portable: FILE-WRITE-DATE has one-second resolution, so wait for the next
;;; second before rewriting the file to be sure it gets a new write date.
(defun rewrite-with-new-write-date (path &optional (fixture "self-signed-valid.pem"))
  "Rewrite PATH in place with the contents of FIXTURE.  In place: unlike
UIOP:COPY-FILE, which renames the old file away and creates a new one, this
leaves the entries of PATH's directory, and so its write date, as they were."
  (let ((before (file-write-date path))
        (bytes (alexandria:read-file-into-byte-vector (test-cert-path fixture))))
    (loop do (sleep 0.25)
             (with-open-file (out path :direction :output
                                       :element-type '(unsigned-byte 8)
                                       :if-exists :supersede)
               (write-sequence bytes out))
          until (/= before (file-write-date path)))))

(test trust-store-cached-per-source
  "The same CA file yields the same trust store object."
  (with-ca-file-copy (path)
    (let ((a (pure-tls::make-trust-store-from-sources path nil))
          (b (pure-tls::make-trust-store-from-sources path nil)))
      (is (plusp (length (pure-tls::trust-store-certificates a))))
      (is (eq a b)))))

(test trust-store-rebuilt-when-source-changes
  "A new write date on the CA file builds a new trust store."
  (with-ca-file-copy (path)
    (let ((a (pure-tls::make-trust-store-from-sources path nil)))
      (rewrite-with-new-write-date path)
      (let ((b (pure-tls::make-trust-store-from-sources path nil)))
        (is (not (eq a b)))
        (is (plusp (length (pure-tls::trust-store-certificates b))))))))

(test empty-trust-store-not-cached
  "A source that yields no certificates is not cached, so it fails closed
until it holds some."
  (uiop:with-temporary-file (:pathname path :type "pem")
    (is (zerop (length (pure-tls::trust-store-certificates
                        (pure-tls::make-trust-store-from-sources path nil)))))
    (is (null (gethash (pure-tls::trust-store-cache-key path nil)
                       pure-tls::*trust-store-cache*)))))

;;; A CA directory named without a trailing slash, as SSL_CERT_DIR and
;;; OpenSSL's CApath conventionally are (Guix System sets
;;; SSL_CERT_DIR=/etc/ssl/certs), used to be read as a file name: "*.pem"
;;; merged into it named the parent directory, and the trust store came out
;;; empty, so every verifying connection failed with UNKNOWN-CA.

(defun call-with-environment (bindings thunk)
  "Call THUNK with each (NAME VALUE) of BINDINGS set in the environment, a
NIL VALUE unsetting NAME, and restore the previous values afterwards.

Uses (SETF UIOP:GETENV), as WITH-SSL-CERT-FILE does, rather than SB-POSIX, so
this file loads on any implementation ASDF supports: UIOP's setf expander
already unsets the variable when the new value is NIL."
  (flet ((set-all (pairs)
           (loop for (name value) in pairs
                 do (setf (uiop:getenv name) value))))
    (let ((old (loop for (name) in bindings
                     collect (list name (uiop:getenv name)))))
      (unwind-protect (progn (set-all bindings) (funcall thunk))
        (set-all old)))))

(defmacro with-environment ((&rest bindings) &body body)
  `(call-with-environment (list ,@(loop for (name value) in bindings
                                        collect `(list ,name ,value)))
                          (lambda () ,@body)))

(defmacro with-ca-directory ((var) &body body)
  "Run BODY with VAR bound to the namestring, without a trailing slash, of
a fresh directory holding a copy of the self-signed fixture."
  (let ((dir (gensym "DIR")))
    `(let ((,dir (uiop:ensure-directory-pathname
                  (merge-pathnames
                   (format nil "pure-tls-ca-dir-~36R"
                           (random (expt 36 8) (make-random-state t)))
                   (uiop:temporary-directory)))))
       (ensure-directories-exist ,dir)
       (unwind-protect
            (progn
              (uiop:copy-file (test-cert-path "self-signed-valid.pem")
                              (merge-pathnames "self-signed-valid.pem" ,dir))
              (let ((,var (string-right-trim "/\\" (namestring ,dir))))
                ,@body))
         (uiop:delete-directory-tree ,dir :validate t)))))

(test trust-store-from-directory-without-trailing-slash
  "A CA directory is read with or without a trailing slash."
  (with-ca-directory (dir)
    (dolist (name (list dir (concatenate 'string dir "/")))
      (is (plusp (length (pure-tls::trust-store-certificates
                          (pure-tls::make-trust-store-from-directory name))))
          "no certificate read from ~S" name))))

(test ssl-cert-dir-without-trailing-slash
  "SSL_CERT_DIR named without a trailing slash yields its certificates."
  (with-ca-directory (dir)
    (with-environment (("SSL_CERT_FILE" nil) ("SSL_CERT_DIR" dir))
      (let ((ctx (pure-tls:make-tls-context :verify-mode pure-tls:+verify-required+)))
        (is (plusp (context-root-count ctx)))))))

(test ssl-cert-dir-trust-store-cached
  "SSL_CERT_DIR's trust store is built once and shared, as a CA file's is."
  (with-ca-directory (dir)
    (with-environment (("SSL_CERT_FILE" nil) ("SSL_CERT_DIR" dir))
      (let ((a (pure-tls::load-system-trust-store))
            (b (pure-tls::load-system-trust-store)))
        (is (plusp (length (pure-tls::trust-store-certificates a))))
        (is (eq a b))))))

;;; A CA directory's own write date changes when an entry is added, removed
;;; or renamed, but not when a certificate file in it is rewritten in place,
;;; so a cache keyed on the directory alone kept trusting a rotated-out root,
;;; and kept rejecting its replacement, until the process restarted.

(test ca-directory-trust-store-rebuilt-when-certificate-rewritten
  "Rewriting a certificate file in a CA directory in place builds a new trust
store, holding the new certificate."
  (with-ca-directory (dir)
    (let ((file (merge-pathnames "self-signed-valid.pem"
                                 (uiop:ensure-directory-pathname dir)))
          (dir-date (file-write-date dir))
          (a (pure-tls::make-trust-store-from-sources nil dir)))
      (rewrite-with-new-write-date file "resumption-ca.pem")
      (is (= dir-date (file-write-date dir))
          "the rewrite changed the directory's write date, so this test no ~
           longer exercises an in-place rewrite")
      (let ((b (pure-tls::make-trust-store-from-sources nil dir)))
        (is (not (eq a b)))
        (is (equalp (list (pure-tls::x509-certificate-raw-der
                           (pure-tls::parse-certificate-from-file
                            (test-cert-path "resumption-ca.pem"))))
                    (mapcar #'pure-tls::x509-certificate-raw-der
                            (pure-tls::trust-store-certificates b))))))))
