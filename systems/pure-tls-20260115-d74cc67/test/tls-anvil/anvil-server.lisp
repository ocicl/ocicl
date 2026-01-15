;;; anvil-server.lisp --- Simple TLS server for TLS-Anvil testing
;;;
;;; Usage:
;;;   sbcl --load anvil-server.lisp
;;;
;;; This starts a TLS 1.3 server on port 4433 that TLS-Anvil can test.

(require :asdf)

;; Load pure-tls from parent directory
(let ((pure-tls-dir (make-pathname :directory (butlast (pathname-directory *load-truename*) 2))))
  (push pure-tls-dir asdf:*central-registry*))

(asdf:load-system :pure-tls)
(require :usocket)

(defpackage #:anvil-server
  (:use #:cl))

(in-package #:anvil-server)

(defparameter *port* 4433)
(defparameter *cert-file* nil)
(defparameter *key-file* nil)

;; Parse command line arguments
(let ((args sb-ext:*posix-argv*))
  (loop for (arg val) on (cdr args) by #'cddr
        do (cond
             ((string= arg "--port") (setf *port* (parse-integer val)))
             ((string= arg "--cert") (setf *cert-file* val))
             ((string= arg "--key") (setf *key-file* val)))))

;; Use test certificates if not specified
(unless *cert-file*
  (let ((test-dir (merge-pathnames "test/certs/openssl/"
                                   (make-pathname :directory (butlast (pathname-directory *load-truename*) 2)))))
    (setf *cert-file* (namestring (merge-pathnames "p256-server-cert.pem" test-dir)))
    (setf *key-file* (namestring (merge-pathnames "p256-server-key.pem" test-dir)))))

(defun load-certificate-chain (cert-file)
  "Load certificate chain from PEM file."
  (pure-tls:load-certificate-chain cert-file))

(defun load-private-key (key-file)
  "Load private key from PEM file."
  (pure-tls:load-private-key key-file))

(defun handle-client (socket)
  "Handle a single TLS client connection."
  (let ((stream (usocket:socket-stream socket)))
    (handler-case
        (let* ((cert-chain (load-certificate-chain *cert-file*))
               (private-key (load-private-key *key-file*))
               (tls-stream (pure-tls:make-tls-server-stream
                            stream
                            :certificate cert-chain
                            :key private-key)))
          (unwind-protect
               (progn
                 ;; Simple echo server - read and echo back
                 (let ((buf (make-array 16384 :element-type '(unsigned-byte 8))))
                   (loop
                     (let ((n (handler-case
                                  (read-sequence buf tls-stream)
                                (end-of-file () 0)
                                (pure-tls:tls-error () 0))))
                       (when (zerop n)
                         (return))
                       (write-sequence buf tls-stream :end n)
                       (force-output tls-stream)))))
            (close tls-stream)))
      (error (e)
        (format *error-output* "Client error: ~A~%" e)))))

(defun run-server ()
  "Run the TLS server."
  (format t "Starting TLS server on port ~D~%" *port*)
  (format t "Certificate: ~A~%" *cert-file*)
  (format t "Key: ~A~%" *key-file*)

  (let ((server-socket (usocket:socket-listen "0.0.0.0" *port*
                                              :reuse-address t
                                              :element-type '(unsigned-byte 8))))
    (unwind-protect
         (loop
           (let ((client-socket (usocket:socket-accept server-socket)))
             (format t "Accepted connection from ~A~%"
                     (usocket:get-peer-address client-socket))
             ;; Handle client in same thread for simplicity
             ;; (For production, would spawn a thread)
             (unwind-protect
                  (handle-client client-socket)
               (usocket:socket-close client-socket))))
      (usocket:socket-close server-socket))))

;; Start the server
(run-server)
