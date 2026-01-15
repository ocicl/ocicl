;;; hunchentoot-test.lisp --- Hunchentoot TLS 1.3 integration test
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Standalone test demonstrating Hunchentoot serving HTTP over TLS 1.3
;;; using pure-tls with self-signed certificates.

(require :asdf)

;; Load dependencies
(asdf:load-system :hunchentoot :verbose nil)
(asdf:load-system :pure-tls :verbose nil)
(asdf:load-system :usocket :verbose nil)

(defpackage #:pure-tls-hunchentoot-test
  (:use #:cl)
  (:export #:run-test
           #:start-server
           #:stop-server))

(in-package #:pure-tls-hunchentoot-test)

;;;; Configuration

(defvar *test-port* 8443)
(defvar *test-host* "127.0.0.1")

;; Paths to test certificates (ECDSA P-256 for TLS 1.3)
(defvar *cert-dir*
  (merge-pathnames "certs/openssl/"
                   (asdf:system-relative-pathname :pure-tls/test "test/")))

(defvar *server-cert* (merge-pathnames "server-ecdsa-cert.pem" *cert-dir*))
(defvar *server-key* (merge-pathnames "server-ecdsa-key.pem" *cert-dir*))

;;;; Pure-TLS Hunchentoot Acceptor

(defclass pure-tls-acceptor (hunchentoot:easy-acceptor)
  ((ssl-certificate-file
    :initarg :ssl-certificate-file
    :accessor ssl-certificate-file
    :documentation "Path to the SSL certificate file.")
   (ssl-privatekey-file
    :initarg :ssl-privatekey-file
    :accessor ssl-privatekey-file
    :documentation "Path to the SSL private key file."))
  (:default-initargs
   :address *test-host*
   :port *test-port*)
  (:documentation "A Hunchentoot acceptor that uses pure-tls for TLS 1.3."))

(defmethod hunchentoot:acceptor-ssl-p ((acceptor pure-tls-acceptor))
  "This acceptor uses SSL/TLS."
  t)

(defmethod hunchentoot:initialize-connection-stream ((acceptor pure-tls-acceptor) stream)
  "Wrap the connection stream with pure-tls for TLS 1.3."
  (pure-tls:make-tls-server-stream
   stream
   :certificate (ssl-certificate-file acceptor)
   :key (ssl-privatekey-file acceptor)))

;;;; Test Handler

(defvar *acceptor* nil)

(hunchentoot:define-easy-handler (hello-handler :uri "/") ()
  (setf (hunchentoot:content-type*) "text/plain")
  "Hello from Hunchentoot over TLS 1.3 with pure-tls!")

(hunchentoot:define-easy-handler (info-handler :uri "/info") ()
  (setf (hunchentoot:content-type*) "application/json")
  (format nil "{\"tls_version\": \"1.3\", \"server\": \"pure-tls + hunchentoot\"}"))

;;;; Server Management

(defun start-server (&key (port *test-port*) (cert *server-cert*) (key *server-key*))
  "Start the Hunchentoot server with pure-tls."
  (when *acceptor*
    (stop-server))
  (format t "~%Starting Hunchentoot server on https://~A:~D/~%" *test-host* port)
  (format t "  Certificate: ~A~%" cert)
  (format t "  Private key: ~A~%" key)
  (setf *acceptor*
        (make-instance 'pure-tls-acceptor
                       :port port
                       :ssl-certificate-file cert
                       :ssl-privatekey-file key))
  (hunchentoot:start *acceptor*)
  (format t "  Server started successfully.~%")
  *acceptor*)

(defun stop-server ()
  "Stop the Hunchentoot server."
  (when *acceptor*
    (format t "Stopping Hunchentoot server...~%")
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)
    (format t "Server stopped.~%")))

;;;; Client Test

(defun test-https-request (&key (port *test-port*))
  "Make a test HTTPS request to the server using pure-tls."
  (format t "~%Connecting to https://~A:~D/...~%" *test-host* port)
  (let ((socket (usocket:socket-connect *test-host* port
                                        :element-type '(unsigned-byte 8))))
    (unwind-protect
         (let ((tls-stream (pure-tls:make-tls-client-stream
                            (usocket:socket-stream socket)
                            :hostname "localhost"
                            :verify pure-tls:+verify-none+)))  ; Self-signed cert
           (unwind-protect
                (progn
                  ;; Verify TLS 1.3
                  (let ((tls-version (pure-tls:tls-version tls-stream))
                        (cipher-suite (pure-tls:tls-cipher-suite tls-stream)))
                    ;; TLS 1.3 = 0x0304 = 772
                    (format t "  TLS Version: ~A (~A)~%"
                            (if (= tls-version #x0304) "TLS 1.3" "Unknown")
                            tls-version)
                    (format t "  Cipher Suite: ~A~%" cipher-suite)
                    (unless (= tls-version #x0304)
                      (format t "~%ERROR: Expected TLS 1.3 (0x0304) but got ~A~%" tls-version)
                      (return-from test-https-request nil)))

                  ;; Send HTTP request
                  (let ((request (format nil "GET / HTTP/1.1~C~CHost: localhost~C~CConnection: close~C~C~C~C"
                                         #\Return #\Linefeed
                                         #\Return #\Linefeed
                                         #\Return #\Linefeed
                                         #\Return #\Linefeed)))
                    (write-sequence (map 'vector #'char-code request) tls-stream)
                    (force-output tls-stream))

                  ;; Read response
                  (let ((response (make-array 4096 :element-type '(unsigned-byte 8) :fill-pointer 0)))
                    (loop for byte = (read-byte tls-stream nil nil)
                          while byte
                          do (vector-push-extend byte response))
                    (let ((response-str (map 'string #'code-char response)))
                      (format t "~%Response received:~%~A~%" response-str)
                      ;; Check for success
                      (when (search "200 OK" response-str)
                        (format t "~%SUCCESS: TLS 1.3 connection verified!~%")
                        t))))
             (close tls-stream)))
      (usocket:socket-close socket))))

;;;; Main Test Runner

(defun run-test ()
  "Run the complete Hunchentoot TLS 1.3 test."
  (format t "~%")
  (format t "========================================~%")
  (format t "  Hunchentoot + pure-tls TLS 1.3 Test  ~%")
  (format t "========================================~%")

  ;; Verify certificate files exist
  (unless (probe-file *server-cert*)
    (error "Certificate file not found: ~A" *server-cert*))
  (unless (probe-file *server-key*)
    (error "Private key file not found: ~A" *server-key*))

  (let ((success nil))
    (unwind-protect
         (progn
           ;; Start server
           (start-server)

           ;; Give server time to start
           (sleep 0.5)

           ;; Test connection
           (setf success (test-https-request)))

      ;; Always stop server
      (stop-server))

    (format t "~%========================================~%")
    (if success
        (format t "  TEST PASSED~%")
        (format t "  TEST FAILED~%"))
    (format t "========================================~%~%")

    success))

;;;; Entry point for standalone execution
(when (member "--run" sb-ext:*posix-argv* :test #'string=)
  (run-test)
  (sb-ext:exit :code (if (run-test) 0 1)))
