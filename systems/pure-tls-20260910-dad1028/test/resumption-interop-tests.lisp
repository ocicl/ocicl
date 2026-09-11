;;; test/resumption-interop-tests.lisp --- Session resumption interop tests
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Regression tests for TLS 1.3 session resumption (PSK) against a real
;;; OpenSSL server.  These exist because two binder bugs shipped in the
;;; past despite the RFC 8448 vector tests passing:
;;;
;;; 1. (fixed in 1.11.0) The binder transcript was built by truncating the
;;;    ClientHello body before wrapping it in a handshake header, so the
;;;    header length field covered the truncated length instead of the
;;;    original one (RFC 8446 Section 4.2.11.2).
;;;
;;; 2. The binder transcript for a ClientHello sent in response to a
;;;    HelloRetryRequest omitted the message_hash(ClientHello1) and
;;;    HelloRetryRequest prefix.  Because pure-tls offers only an
;;;    X25519MLKEM768 key share, every server without ML-KEM support
;;;    triggers HRR, so resumption against such servers always failed.
;;;    Servers reject the bad binder with illegal_parameter (the alert
;;;    RFC 8446 prescribes; OpenSSL uses decrypt_error).
;;;
;;; Both bugs are invisible to loopback (pure-tls to pure-tls) testing
;;; when client and server share the same wrong transcript computation,
;;; and invisible to vector tests that feed COMPUTE-BINDER a precomputed
;;; hash.  Only interop with a foreign stack catches them, hence these
;;; tests.  They are skipped when the openssl CLI is unavailable.

(in-package #:pure-tls/test)

(def-suite resumption-interop-tests
  :description "TLS 1.3 session resumption interop against OpenSSL s_server")

(in-suite resumption-interop-tests)

(defvar *resumption-test-port* 14430
  "Base port for resumption interop test servers.")

(defun openssl-tls13-available-p ()
  "Return T if an openssl CLI with TLS 1.3 s_server support is available."
  (handler-case
      (let ((version (uiop:run-program '("openssl" "version")
                                       :output '(:string :stripped t)
                                       :ignore-error-status t)))
        (and version (search "OpenSSL" version) t))
    (error () nil)))

(defun make-resumption-test-cert (dir)
  "Generate a throwaway self-signed cert/key pair in DIR.
   Returns (values cert-path key-path)."
  (let ((cert (merge-pathnames "cert.pem" dir))
        (key (merge-pathnames "key.pem" dir)))
    (uiop:run-program
     (list "openssl" "req" "-x509" "-newkey" "rsa:2048"
           "-keyout" (namestring key) "-out" (namestring cert)
           "-days" "1" "-nodes" "-subj" "/CN=localhost"
           "-addext" "subjectAltName=DNS:localhost")
     :error-output nil)
    (values cert key)))

(defun wait-for-port (port &key (timeout 10))
  "Wait until a TCP connection to localhost:PORT succeeds."
  (loop repeat (* timeout 10)
        do (handler-case
               (let ((s (usocket:socket-connect "localhost" port)))
                 (usocket:socket-close s)
                 (return t))
             (error () (sleep 0.1)))
        finally (return nil)))

(defun call-with-openssl-s-server (port extra-args body-fn)
  "Run BODY-FN with an openssl s_server listening on PORT.
   EXTRA-ARGS is a list of additional s_server command line arguments."
  (let ((dir (uiop:ensure-directory-pathname
              (format nil "~Apure-tls-resumption-test-~D/"
                      (uiop:temporary-directory) port))))
    (ensure-directories-exist dir)
    (unwind-protect
         (multiple-value-bind (cert key) (make-resumption-test-cert dir)
           (let ((process (uiop:launch-program
                           (append (list "openssl" "s_server"
                                         "-accept" (format nil "~D" port)
                                         "-cert" (namestring cert)
                                         "-key" (namestring key)
                                         "-tls1_3" "-www")
                                   extra-args)
                           :output nil :error-output nil)))
             (unwind-protect
                  (progn
                    (unless (wait-for-port port)
                      (error "openssl s_server did not start on port ~D" port))
                    (funcall body-fn))
               (ignore-errors (uiop:terminate-process process :urgent t))
               (ignore-errors (uiop:wait-process process)))))
      (ignore-errors (uiop:delete-directory-tree
                      dir :validate t :if-does-not-exist :ignore)))))

(defun resumption-test-connect (port)
  "Connect to localhost:PORT with pure-tls, issue an HTTP request, and read
   the response (which also drains NewSessionTicket messages).  Returns the
   client handshake object on success, signals on failure."
  (let ((socket (usocket:socket-connect "localhost" port
                                        :element-type '(unsigned-byte 8))))
    (unwind-protect
         (let ((tls (pure-tls:make-tls-client-stream
                     (usocket:socket-stream socket)
                     :hostname "localhost"
                     :verify pure-tls:+verify-none+)))
           (write-sequence (map '(vector (unsigned-byte 8)) #'char-code
                                (format nil "GET / HTTP/1.0~C~C~C~C"
                                        #\Return #\Linefeed #\Return #\Linefeed))
                           tls)
           (force-output tls)
           ;; Read a byte of the response; post-handshake NewSessionTicket
           ;; messages are processed while waiting for application data.
           (handler-case (read-byte tls nil nil) (error () nil))
           (prog1 (pure-tls::tls-stream-handshake tls)
             (handler-case (close tls) (error () nil))))
      (ignore-errors (usocket:socket-close socket)))))

(defun run-resumption-scenario (port &rest s-server-args)
  "Connect twice to an openssl s_server started with S-SERVER-ARGS.
   Returns (values second-handshake-ok psk-offered psk-accepted)."
  (pure-tls::session-ticket-cache-clear "localhost")
  (call-with-openssl-s-server
   port s-server-args
   (lambda ()
     ;; First connection: full handshake, caches the session ticket.
     (resumption-test-connect port)
     (sleep 0.2)
     ;; Second connection: offers the cached ticket as a PSK.
     (let ((hs (resumption-test-connect port)))
       (values t
               (and (pure-tls::client-handshake-offered-psk hs) t)
               (pure-tls::client-handshake-psk-accepted hs))))))

(test resumption-against-openssl
  "Session resumption against OpenSSL without HelloRetryRequest.
   Guards against binder transcript regressions (RFC 8446 s4.2.11.2)."
  (if (openssl-tls13-available-p)
      (multiple-value-bind (ok offered accepted)
          (run-resumption-scenario *resumption-test-port*
                                   "-ciphersuites" "TLS_AES_128_GCM_SHA256")
        (is-true ok "Second handshake failed (server rejected our PSK binder)")
        (is-true offered "Client did not offer the cached session ticket")
        (is-true accepted "Server did not accept the PSK (binder mismatch?)"))
      (skip "openssl CLI not available")))

(test resumption-against-openssl-with-hrr
  "Session resumption across a HelloRetryRequest.  The server is limited
   to X25519, and pure-tls's initial key share is X25519MLKEM768, so the
   server must send HRR.  The binder in the second ClientHello must then
   cover message_hash(ClientHello1) || HelloRetryRequest || Truncate(CH2).
   Any server without ML-KEM support (e.g. Java-based servers such as
   JFrog Artifactory) exercises this path on every resumption attempt."
  (if (openssl-tls13-available-p)
      (multiple-value-bind (ok offered accepted)
          (run-resumption-scenario (+ *resumption-test-port* 1)
                                   "-groups" "X25519")
        (is-true ok "Second handshake failed (server rejected our PSK binder)")
        (is-true offered "Client did not offer the cached session ticket")
        (is-true accepted "Server did not accept the PSK (binder mismatch?)"))
      (skip "openssl CLI not available")))
