;;;; minimal-pure-tls-test.lisp
;;;; Diagnostic test for pure-tls trust store and TLS connectivity
;;;;
;;;; Usage: sbcl --load test/minimal-pure-tls-test.lisp

(defpackage #:ocicl.test.pure-tls-diagnostic
  (:use #:cl))

(in-package #:ocicl.test.pure-tls-diagnostic)

(format t "~%=== Pure-TLS Diagnostic Test ===~%~%")

;;; Load pure-tls
(format t "Loading pure-tls...~%")
(handler-case
    (progn
      (require :asdf)
      (asdf:load-system :pure-tls :verbose nil))
  (error (e)
    (format t "ERROR: Failed to load pure-tls: ~A~%" e)
    (uiop:quit 1)))

(format t "pure-tls loaded successfully.~%~%")

;;; Test 1: Check environment variables
(defun test-environment ()
  (format t "--- Test 1: Environment Variables ---~%")
  (let ((vars '("SSL_CERT_FILE" "SSL_CERT_DIR"
                "OCICL_CA_FILE" "OCICL_CA_DIR"
                "OCICL_TLS_DEBUG")))
    (dolist (var vars)
      (let ((value (uiop:getenv var)))
        (format t "  ~A = ~A~%" var (or value "(not set)")))))
  (format t "~%"))

;;; Test 2: Check CA certificate paths
(defun test-ca-paths ()
  (format t "--- Test 2: CA Certificate Paths ---~%")
  (let ((paths '("/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem"  ; Fedora/RHEL 9+
                 "/etc/pki/tls/certs/ca-bundle.crt"                    ; RHEL/CentOS (older)
                 "/etc/ssl/certs/ca-certificates.crt"                  ; Debian/Ubuntu
                 "/etc/ssl/ca-bundle.pem"                               ; OpenSUSE
                 "/usr/local/share/certs/ca-root-nss.crt"              ; FreeBSD
                 "/etc/ssl/cert.pem"))                                  ; OpenBSD
        (dirs '("/etc/ssl/certs"                                        ; Debian/Ubuntu
                "/etc/pki/tls/certs")))                                 ; RHEL/CentOS
    (format t "  Checking CA bundle files:~%")
    (dolist (path paths)
      (let ((exists (probe-file path)))
        (format t "    ~A: ~A~%"
                path
                (if exists
                    (format nil "EXISTS (~D bytes)" (with-open-file (f path) (file-length f)))
                    "not found"))))
    (format t "~%  Checking CA certificate directories:~%")
    (dolist (dir dirs)
      (let ((exists (probe-file dir)))
        (format t "    ~A: ~A~%"
                dir
                (if exists "EXISTS" "not found"))))
    (format t "~%")))

;;; Test 3: Test trust store loading
(defun test-trust-store-loading ()
  (format t "--- Test 3: Trust Store Loading ---~%")
  (handler-case
      (let* ((ctx (pure-tls:make-tls-context :trust-store :default))
             (trust-store (slot-value ctx 'pure-tls::trust-store)))
        (format t "  TLS context created successfully~%")
        (cond
          ((null trust-store)
           (format t "  WARNING: Trust store is NIL~%")
           nil)
          ((zerop (length trust-store))
           (format t "  WARNING: Trust store is empty (0 certificates)~%")
           nil)
          (t
           (format t "  SUCCESS: Trust store loaded with ~D certificate(s)~%"
                   (length trust-store))
           t)))
    (error (e)
      (format t "  ERROR: Failed to create TLS context: ~A~%" e)
      nil))
  (format t "~%"))

;;; Test 4: Test TLS connection to ghcr.io
(defun test-ghcr-connection ()
  (format t "--- Test 4: TLS Connection to ghcr.io ---~%")
  (handler-case
      (let* ((hostname "ghcr.io")
             (port 443)
             (socket nil)
             (tls-stream nil))
        (format t "  Connecting to ~A:~D...~%" hostname port)

        ;; Create TCP socket
        (setf socket (usocket:socket-connect hostname port
                                            :element-type '(unsigned-byte 8)))
        (format t "  TCP connection established~%")

        ;; Create TLS context
        (let ((ctx (pure-tls:make-tls-context :trust-store :default)))
          (format t "  TLS context created~%")

          ;; Wrap socket in TLS
          (setf tls-stream
                (pure-tls:make-tls-stream
                 (usocket:socket-stream socket)
                 :context ctx
                 :hostname hostname))
          (format t "  TLS stream created~%")

          ;; Send a simple HTTP request
          (format tls-stream "GET /v2/ HTTP/1.1~%")
          (format tls-stream "Host: ~A~%" hostname)
          (format tls-stream "Connection: close~%")
          (format tls-stream "~%")
          (force-output tls-stream)
          (format t "  HTTP request sent~%")

          ;; Read response (just first line)
          (let ((response (read-line tls-stream nil nil)))
            (format t "  Response: ~A~%" response)
            (when (search "HTTP/" response)
              (format t "  SUCCESS: TLS connection works!~%")))

          ;; Clean up
          (close tls-stream)
          (usocket:socket-close socket)
          t))
    (error (e)
      (format t "  ERROR: TLS connection failed: ~A~%" e)
      (format t "  Error type: ~A~%" (type-of e))
      nil))
  (format t "~%"))

;;; Run all tests
(defun run-all-tests ()
  (test-environment)
  (test-ca-paths)
  (let ((trust-store-ok (test-trust-store-loading)))
    (if trust-store-ok
        (progn
          (test-ghcr-connection)
          (format t "=== All tests completed ===~%~%")
          (format t "RESULT: Trust store loaded successfully.~%")
          0)
        (progn
          (format t "=== Tests stopped early ===~%~%")
          (format t "RESULT: Trust store failed to load.~%")
          (format t "~%Possible solutions:~%")
          (format t "  1. Install ca-certificates: sudo dnf install ca-certificates~%")
          (format t "  2. Set SSL_CERT_FILE to point to your CA bundle~%")
          (format t "  3. Rebuild with USE_LEGACY_OPENSSL=1~%")
          1))))

;;; Load dependencies quietly
(format t "Loading dependencies...~%")
(handler-case
    (asdf:load-system :usocket :verbose nil)
  (error (e)
    (format t "ERROR: Failed to load usocket: ~A~%" e)
    (uiop:quit 1)))

(format t "~%")

;;; Run tests and exit with appropriate code
(let ((exit-code (run-all-tests)))
  (uiop:quit exit-code))
