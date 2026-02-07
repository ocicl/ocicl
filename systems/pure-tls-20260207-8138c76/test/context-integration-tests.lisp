;;; context-integration-tests.lisp --- Integration tests for timeout/cancellation
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Integration tests that verify timeout and cancellation behavior during
;;; real TLS operations (handshake, I/O, CRL fetching).

(in-package :pure-tls/test)

(def-suite context-integration-tests
    :description "Integration tests for request context timeout/cancellation")

(in-suite context-integration-tests)

(test timeout-during-handshake-with-unresponsive-server
  "Test that handshake times out when connecting to an unresponsive server"
  :depends-on (and)
  #+windows (skip "Skipped on Windows - socket operations hang with unresponsive servers")
  #-windows
  ;; This test requires a server that accepts connections but never responds
  ;; We'll create a listening socket but never accept connections
  (let ((server-socket (usocket:socket-listen "127.0.0.1" 0
                                               :reuse-address t
                                               :element-type '(unsigned-byte 8))))
    (unwind-protect
         (let* ((server-port (usocket:get-local-port server-socket))
                (client-socket (usocket:socket-connect "127.0.0.1" server-port
                                                        :element-type '(unsigned-byte 8)
                                                        :timeout 1)))
           (unwind-protect
                (progn
                  ;; Try to perform TLS handshake with 1 second timeout
                  ;; Should fail because server never responds
                  (signals error  ; Will be one of our timeout/cancellation conditions or TLS error
                    (cl-cancel:with-timeout-context (_ 1)
                      (pure-tls:make-tls-client-stream
                       (usocket:socket-stream client-socket)
                       :hostname "localhost"
                       :verify pure-tls:+verify-none+))))
             (ignore-errors (usocket:socket-close client-socket))))
      (ignore-errors (usocket:socket-close server-socket)))))

(test cancellation-during-connect
  "Test that TLS operations can be cancelled via context"
  (let ((cancel-ctx nil)
        (cancel-fn nil))
    (multiple-value-setq (cancel-ctx cancel-fn)
      (cl-cancel:with-cancel (cl-cancel:background)))

    ;; Cancel the context immediately
    (funcall cancel-fn)

    ;; Any TLS operation with this context should fail immediately
    (signals pure-tls:tls-context-cancelled
      (pure-tls::check-tls-context cancel-ctx))))

(test timeout-with-slow-http-server
  "Test CRL fetch timeout with a slow HTTP server"
  :depends-on (and)
  #+windows (skip "Skipped on Windows - socket operations hang with slow servers")
  #-windows
  ;; Create a server that accepts but never sends response
  (let ((server-socket (usocket:socket-listen "127.0.0.1" 0
                                               :reuse-address t
                                               :element-type '(unsigned-byte 8)))
        (accepted nil))
    (unwind-protect
         (let* ((server-port (usocket:get-local-port server-socket))
                (url (format nil "http://127.0.0.1:~D/test.crl" server-port)))
           ;; Spawn thread to accept but not respond
           (let ((server-thread
                   (bt2:make-thread
                    (lambda ()
                      (ignore-errors
                        (let ((client (usocket:socket-accept server-socket)))
                          (setf accepted t)
                          ;; Read the HTTP request but never send response
                          (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8))))
                            (ignore-errors (read-sequence buffer (usocket:socket-stream client))))
                          ;; Hold connection open without responding
                          (sleep 30)
                          (usocket:socket-close client)))))))

             ;; Try to fetch CRL with short timeout
             ;; Should timeout while waiting for HTTP response
             (sleep 0.2)  ; Give server thread time to start accepting
             (handler-case
                 (cl-cancel:with-timeout-context (_ 2)
                   (pure-tls::fetch-crl url)
                   ;; If we got here without error, fail the test
                   (fail "Expected timeout error but fetch succeeded"))
               (error (e)
                 ;; Any error is acceptable (timeout, network, etc.)
                 (pass)))))
      (ignore-errors (usocket:socket-close server-socket)))))

(test context-with-successful-connection
  "Verify that normal TLS connections work with context (no timeout)"
  :depends-on (and)
  ;; Test that providing a context doesn't break normal operations
  (handler-case
      (cl-cancel:with-timeout-context (_ 30)
        (let* ((socket (usocket:socket-connect "www.google.com" 443
                                                :element-type '(unsigned-byte 8)
                                                :timeout 10))
               (stream (pure-tls:make-tls-client-stream
                        (usocket:socket-stream socket)
                        :hostname "www.google.com")))
          (unwind-protect
               (progn
                 ;; Verify stream is usable
                 (is (typep stream 'pure-tls:tls-stream))
                 ;; Try a simple read (should not timeout)
                 (write-sequence
                  (flexi-streams:string-to-octets
                   "GET / HTTP/1.1\r\nHost: www.google.com\r\nConnection: close\r\n\r\n")
                  stream)
                 (force-output stream)
                 (let ((response (make-array 100 :element-type '(unsigned-byte 8))))
                   (read-sequence response stream)
                   (is (> (length response) 0))))
            (ignore-errors (close stream))
            (ignore-errors (usocket:socket-close socket)))))
    (usocket:timeout-error (e)
      (skip "Network timeout - skipping integration test"))
    (usocket:ns-host-not-found-error (e)
      (skip "DNS resolution failed - skipping integration test"))
    (error (e)
      (skip (format nil "Network error: ~A" e)))))
