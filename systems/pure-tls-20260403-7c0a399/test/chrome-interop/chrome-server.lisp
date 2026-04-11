;;; chrome-server.lisp --- Chrome interop test server for X25519MLKEM768
;;;
;;; Usage:
;;;   1. Generate certificates (run once):
;;;      ./generate-localhost-cert.sh
;;;
;;;   2. Start the server:
;;;      sbcl --load chrome-server.lisp
;;;
;;;   3. Open Chrome to: https://localhost:8443/
;;;      (Accept the self-signed certificate warning)
;;;
;;;   4. Check the server output for the negotiated key exchange group
;;;
;;; Requirements:
;;;   - usocket (for TCP server)
;;;   - Chrome 124+ (has X25519MLKEM768 enabled by default)

(require :asdf)

;; Load pure-tls from parent directory
(let ((pure-tls-dir (make-pathname :directory (butlast (pathname-directory *load-truename*) 2))))
  (push pure-tls-dir asdf:*central-registry*))

(asdf:load-system :pure-tls)
(asdf:load-system :usocket)

(defpackage #:chrome-server
  (:use #:cl))

(in-package #:chrome-server)

(defparameter *port* 8443)
(defparameter *cert-file* nil)
(defparameter *key-file* nil)

;; Parse command line arguments
(let ((args #+sbcl sb-ext:*posix-argv* #-sbcl nil))
  (loop for (arg val) on (cdr args) by #'cddr
        do (cond
             ((string= arg "--port") (setf *port* (parse-integer val)))
             ((string= arg "--cert") (setf *cert-file* val))
             ((string= arg "--key") (setf *key-file* val)))))

;; Use localhost certificates if not specified
(unless *cert-file*
  (let ((test-dir (directory-namestring *load-truename*)))
    (setf *cert-file* (concatenate 'string test-dir "localhost-cert.pem"))
    (setf *key-file* (concatenate 'string test-dir "localhost-key.pem"))))

(defun group-name (group-code)
  "Return human-readable name for key exchange group."
  (case group-code
    (#x0017 "secp256r1 (P-256)")
    (#x0018 "secp384r1 (P-384)")
    (#x0019 "secp521r1 (P-521)")
    (#x001D "x25519")
    (#x001E "x448")
    (#x11EC "X25519MLKEM768 (hybrid post-quantum)")  ; IANA registered
    (#x6399 "X25519Kyber768Draft00 (draft hybrid)")  ; Old draft codepoint
    (otherwise (format nil "unknown (0x~4,'0X)" group-code))))

(defun format-cipher-suite (cs)
  "Return human-readable cipher suite name."
  (case cs
    (#x1301 "TLS_AES_128_GCM_SHA256")
    (#x1302 "TLS_AES_256_GCM_SHA384")
    (#x1303 "TLS_CHACHA20_POLY1305_SHA256")
    (otherwise (format nil "0x~4,'0X" cs))))

(defun html-response (title body)
  "Generate a simple HTML response."
  (format nil "<!DOCTYPE html>
<html>
<head>
  <title>~A</title>
  <style>
    body { font-family: -apple-system, sans-serif; max-width: 800px; margin: 50px auto; padding: 20px; }
    h1 { color: #2d7d46; }
    .success { background: #d4edda; border: 1px solid #c3e6cb; padding: 15px; border-radius: 5px; }
    .info { background: #e7f3ff; border: 1px solid #b6d4fe; padding: 15px; border-radius: 5px; margin: 10px 0; }
    code { background: #f4f4f4; padding: 2px 6px; border-radius: 3px; }
    pre { background: #f4f4f4; padding: 15px; border-radius: 5px; overflow-x: auto; }
  </style>
</head>
<body>
~A
</body>
</html>" title body))

(defun connection-info-html (group cipher)
  "Generate HTML showing connection information."
  (html-response "pure-tls Chrome Interop Test"
    (format nil "
<h1>TLS 1.3 Connection Successful</h1>

<div class=\"success\">
  <strong>Connection established with pure-tls server!</strong>
</div>

<div class=\"info\">
  <strong>Key Exchange:</strong> <code>~A</code>
</div>

<div class=\"info\">
  <strong>Cipher Suite:</strong> <code>~A</code>
</div>

<h2>What This Means</h2>
~A

<h2>Verify in Chrome DevTools</h2>
<ol>
  <li>Press F12 to open DevTools</li>
  <li>Go to the Security tab</li>
  <li>Look for \"Key exchange\" - should show the negotiated group</li>
</ol>

<h2>Server Info</h2>
<pre>pure-tls TLS 1.3 server
Supported groups: X25519MLKEM768, X25519, P-256, P-384
Cipher suites: ChaCha20-Poly1305, AES-256-GCM, AES-128-GCM</pre>
"
            (group-name group)
            (format-cipher-suite cipher)
            (if (or (= group #x11EC) (= group #x6399))
                "<p><strong style=\"color: #2d7d46;\">Post-quantum hybrid key exchange is active!</strong>
                 Your connection uses X25519MLKEM768, which combines classical X25519 with the
                 ML-KEM-768 post-quantum algorithm for protection against future quantum computers.</p>"
                "<p>Classical key exchange is being used. This may be because:
                 <ul>
                   <li>Chrome's post-quantum support is disabled</li>
                   <li>The server didn't offer X25519MLKEM768 first</li>
                   <li>There was a HelloRetryRequest that selected a different group</li>
                 </ul></p>"))))

(defun http-response (status content-type body)
  "Generate an HTTP/1.1 response."
  (let ((body-bytes (pure-tls::string-to-octets body)))
    (pure-tls::string-to-octets
     (format nil "HTTP/1.1 ~A~C~CContent-Type: ~A~C~CContent-Length: ~D~C~CConnection: close~C~C~C~C~A"
             status #\Return #\Linefeed
             content-type #\Return #\Linefeed
             (length body-bytes) #\Return #\Linefeed
             #\Return #\Linefeed
             #\Return #\Linefeed
             body))))

(defun handle-client (socket)
  "Handle a single TLS client connection."
  (let ((stream (usocket:socket-stream socket)))
    (handler-case
        (let* ((cert-chain (pure-tls:load-certificate-chain *cert-file*))
               (private-key (pure-tls:load-private-key *key-file*))
               (tls-stream (pure-tls:make-tls-server-stream
                            stream
                            :certificate cert-chain
                            :key private-key)))
          (unwind-protect
               (let* ((handshake (pure-tls::tls-stream-handshake tls-stream))
                      (selected-group (pure-tls::server-handshake-selected-group handshake))
                      (cipher-suite (pure-tls::server-handshake-selected-cipher-suite handshake)))
                 ;; Log connection details
                 (format t "~%========================================~%")
                 (format t "New TLS 1.3 connection established!~%")
                 (format t "  Key Exchange: ~A~%" (group-name selected-group))
                 (format t "  Cipher Suite: ~A~%" (format-cipher-suite cipher-suite))
                 (when (or (= selected-group #x11EC) (= selected-group #x6399))
                   (format t "  *** POST-QUANTUM HYBRID ACTIVE! ***~%"))
                 (format t "========================================~%")
                 (force-output)

                 ;; Read HTTP request (just consume it)
                 (let ((buf (make-array 4096 :element-type '(unsigned-byte 8))))
                   (handler-case
                       (read-sequence buf tls-stream)
                     (error () nil)))

                 ;; Send HTTP response with connection info
                 (let ((response (http-response "200 OK" "text/html; charset=utf-8"
                                                (connection-info-html selected-group cipher-suite))))
                   (write-sequence response tls-stream)
                   (force-output tls-stream)))
            (close tls-stream)))
      (pure-tls:tls-error (e)
        (format *error-output* "TLS error: ~A~%" e))
      (error (e)
        (format *error-output* "Error: ~A~%" e)))))

(defun run-server ()
  "Run the HTTPS server."
  (format t "~%")
  (format t "==============================================~%")
  (format t "  pure-tls Chrome Interop Test Server~%")
  (format t "==============================================~%")
  (format t "~%")
  (format t "Port: ~D~%" *port*)
  (format t "Certificate: ~A~%" *cert-file*)
  (format t "Key: ~A~%" *key-file*)
  (format t "~%")

  ;; Verify cert files exist
  (unless (probe-file *cert-file*)
    (format t "ERROR: Certificate file not found!~%")
    (format t "Run ./generate-localhost-cert.sh first.~%")
    (return-from run-server))
  (unless (probe-file *key-file*)
    (format t "ERROR: Key file not found!~%")
    (format t "Run ./generate-localhost-cert.sh first.~%")
    (return-from run-server))

  (format t "Open Chrome to: https://localhost:~D/~%" *port*)
  (format t "(Accept the self-signed certificate warning)~%")
  (format t "~%")
  (format t "Waiting for connections...~%")
  (format t "Press Ctrl+C to stop.~%")
  (format t "~%")
  (force-output)

  (let ((server-socket (usocket:socket-listen "0.0.0.0" *port*
                                              :reuse-address t
                                              :element-type '(unsigned-byte 8))))
    (unwind-protect
         (loop
           (handler-case
               (let ((client-socket (usocket:socket-accept server-socket)))
                 (format t "Connection from ~A:~D~%"
                         (usocket:get-peer-address client-socket)
                         (usocket:get-peer-port client-socket))
                 (force-output)
                 (unwind-protect
                      (handle-client client-socket)
                   (usocket:socket-close client-socket)))
             (error (e)
               (format *error-output* "Accept error: ~A~%" e))))
      (usocket:socket-close server-socket))))

;; Start the server
(run-server)
