;;; messages.lisp --- TLS 1.3 Handshake Message Structures
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements TLS 1.3 handshake message parsing and serialization.

(in-package #:pure-tls)

;;;; Handshake Message Structure
;;;
;;; struct {
;;;   HandshakeType msg_type;
;;;   uint24 length;
;;;   select (msg_type) {
;;;     case client_hello:         ClientHello;
;;;     case server_hello:         ServerHello;
;;;     case encrypted_extensions: EncryptedExtensions;
;;;     case certificate:          Certificate;
;;;     case certificate_verify:   CertificateVerify;
;;;     case finished:             Finished;
;;;     ...
;;;   } body;
;;; } Handshake;

(defstruct handshake-message
  "Base structure for all handshake messages."
  (type 0 :type octet)
  (body nil))

;;;; ClientHello

(defstruct client-hello
  "ClientHello message."
  (legacy-version +tls-1.2+ :type fixnum)
  (random nil :type (or null octet-vector))  ; 32 bytes
  (legacy-session-id nil :type (or null octet-vector))
  (cipher-suites nil :type list)
  (legacy-compression-methods nil :type (or null octet-vector))
  (extensions nil :type list))

(defun serialize-client-hello (hello)
  "Serialize a ClientHello to bytes."
  (let ((buf (make-tls-write-buffer)))
    ;; ProtocolVersion legacy_version = 0x0303
    (write-buffer-append-uint16 buf (client-hello-legacy-version hello))
    ;; Random random (32 bytes)
    (write-buffer-append buf (client-hello-random hello))
    ;; opaque legacy_session_id<0..32>
    (write-buffer-append-vector8 buf (or (client-hello-legacy-session-id hello)
                                          (make-octet-vector 0)))
    ;; CipherSuite cipher_suites<2..2^16-2>
    (let ((suites-buf (make-tls-write-buffer)))
      (dolist (suite (client-hello-cipher-suites hello))
        (write-buffer-append-uint16 suites-buf suite))
      (write-buffer-append-vector16 buf (write-buffer-contents suites-buf)))
    ;; opaque legacy_compression_methods<1..2^8-1>
    (write-buffer-append-vector8 buf (or (client-hello-legacy-compression-methods hello)
                                          (octet-vector 0)))  ; null compression
    ;; Extension extensions<8..2^16-1>
    (let ((ext-buf (make-tls-write-buffer)))
      (dolist (ext (client-hello-extensions hello))
        (write-buffer-append ext-buf (serialize-extension ext)))
      (write-buffer-append-vector16 buf (write-buffer-contents ext-buf)))
    (write-buffer-contents buf)))

(defun parse-client-hello (data)
  "Parse a ClientHello from bytes."
  (let ((buf (make-tls-buffer data)))
    (make-client-hello
     :legacy-version (buffer-read-uint16 buf)
     :random (buffer-read-octets buf 32)
     :legacy-session-id (buffer-read-vector8 buf)
     :cipher-suites (let ((suites-data (buffer-read-vector16 buf))
                          (suites nil))
                      (loop for i from 0 below (length suites-data) by 2
                            do (push (decode-uint16 suites-data i) suites))
                      (nreverse suites))
     :legacy-compression-methods (buffer-read-vector8 buf)
     :extensions (parse-extensions (buffer-read-vector16 buf)))))

;;;; ServerHello

(defstruct server-hello
  "ServerHello message."
  (legacy-version +tls-1.2+ :type fixnum)
  (random nil :type (or null octet-vector))  ; 32 bytes
  (legacy-session-id-echo nil :type (or null octet-vector))
  (cipher-suite 0 :type fixnum)
  (legacy-compression-method 0 :type octet)
  (extensions nil :type list))

;; HelloRetryRequest is indicated by this special random value
(defparameter +hello-retry-request-random+
  (hex-to-octets "CF21AD74E59A6111BE1D8C021E65B891C2A211167ABB8C5E079E09E2C8A8339C")
  "Magic random value that indicates HelloRetryRequest.")

(defun hello-retry-request-p (server-hello)
  "Check if a ServerHello is actually a HelloRetryRequest."
  (constant-time-equal (server-hello-random server-hello)
                       +hello-retry-request-random+))

(defun parse-server-hello (data)
  "Parse a ServerHello from bytes."
  (let ((buf (make-tls-buffer data)))
    (make-server-hello
     :legacy-version (buffer-read-uint16 buf)
     :random (buffer-read-octets buf 32)
     :legacy-session-id-echo (buffer-read-vector8 buf)
     :cipher-suite (buffer-read-uint16 buf)
     :legacy-compression-method (buffer-read-octet buf)
     :extensions (when (plusp (buffer-remaining buf))
                   (parse-extensions (buffer-read-vector16 buf))))))

(defun serialize-server-hello (hello)
  "Serialize a ServerHello to bytes."
  (let ((buf (make-tls-write-buffer)))
    ;; ProtocolVersion legacy_version = 0x0303
    (write-buffer-append-uint16 buf (server-hello-legacy-version hello))
    ;; Random random (32 bytes)
    (write-buffer-append buf (server-hello-random hello))
    ;; opaque legacy_session_id_echo<0..32>
    (write-buffer-append-vector8 buf (or (server-hello-legacy-session-id-echo hello)
                                          (make-octet-vector 0)))
    ;; CipherSuite cipher_suite (2 bytes)
    (write-buffer-append-uint16 buf (server-hello-cipher-suite hello))
    ;; uint8 legacy_compression_method = 0
    (write-buffer-append-octet buf (server-hello-legacy-compression-method hello))
    ;; Extension extensions<6..2^16-1>
    (let ((ext-buf (make-tls-write-buffer)))
      (dolist (ext (server-hello-extensions hello))
        (write-buffer-append ext-buf (serialize-extension ext)))
      (write-buffer-append-vector16 buf (write-buffer-contents ext-buf)))
    (write-buffer-contents buf)))

;;;; EncryptedExtensions

(defstruct encrypted-extensions
  "EncryptedExtensions message."
  (extensions nil :type list))

(defun parse-encrypted-extensions (data)
  "Parse EncryptedExtensions from bytes."
  (let ((buf (make-tls-buffer data)))
    (make-encrypted-extensions
     :extensions (parse-extensions (buffer-read-vector16 buf)))))

(defun serialize-encrypted-extensions (ee)
  "Serialize an EncryptedExtensions message to bytes."
  (let ((ext-buf (make-tls-write-buffer)))
    (dolist (ext (encrypted-extensions-extensions ee))
      (write-buffer-append ext-buf (serialize-extension ext)))
    (let ((buf (make-tls-write-buffer)))
      (write-buffer-append-vector16 buf (write-buffer-contents ext-buf))
      (write-buffer-contents buf))))

;;;; CertificateRequest

(defstruct certificate-request
  "CertificateRequest message (TLS 1.3)."
  (certificate-request-context nil :type (or null octet-vector))
  (extensions nil :type list))

(defun parse-certificate-request (data)
  "Parse a CertificateRequest message from bytes."
  (let ((buf (make-tls-buffer data)))
    (make-certificate-request
     :certificate-request-context (buffer-read-vector8 buf)
     :extensions (parse-extensions (buffer-read-vector16 buf)))))

(defun serialize-certificate-request (req)
  "Serialize a CertificateRequest message to bytes."
  (let ((buf (make-tls-write-buffer)))
    ;; opaque certificate_request_context<0..2^8-1>
    (write-buffer-append-vector8 buf (or (certificate-request-certificate-request-context req)
                                          (make-octet-vector 0)))
    ;; Extension extensions<2..2^16-1>
    (let ((ext-buf (make-tls-write-buffer)))
      (dolist (ext (certificate-request-extensions req))
        (write-buffer-append ext-buf (serialize-extension ext)))
      (write-buffer-append-vector16 buf (write-buffer-contents ext-buf)))
    (write-buffer-contents buf)))

;;;; Certificate

(defstruct certificate-message
  "Certificate message."
  (certificate-request-context nil :type (or null octet-vector))
  (certificate-list nil :type list))

(defstruct certificate-entry
  "A single certificate in the chain."
  (cert-data nil :type (or null octet-vector))
  (extensions nil :type list))

(defun parse-certificate-message (data)
  "Parse a Certificate message from bytes."
  (let ((buf (make-tls-buffer data)))
    (make-certificate-message
     :certificate-request-context (buffer-read-vector8 buf)
     :certificate-list
     (let ((certs-data (buffer-read-vector24 buf))
           (certs nil))
       (let ((cert-buf (make-tls-buffer certs-data)))
         (loop while (plusp (buffer-remaining cert-buf))
               do (let ((cert-data (buffer-read-vector24 cert-buf))
                        (extensions (parse-extensions (buffer-read-vector16 cert-buf))))
                    (push (make-certificate-entry
                           :cert-data cert-data
                           :extensions extensions)
                          certs))))
       (nreverse certs)))))

(defun serialize-certificate-message (cert-msg)
  "Serialize a Certificate message to bytes."
  (let ((buf (make-tls-write-buffer)))
    ;; opaque certificate_request_context<0..2^8-1>
    (write-buffer-append-vector8 buf (or (certificate-message-certificate-request-context cert-msg)
                                          (make-octet-vector 0)))
    ;; CertificateEntry certificate_list<0..2^24-1>
    (let ((certs-buf (make-tls-write-buffer)))
      (dolist (entry (certificate-message-certificate-list cert-msg))
        ;; opaque cert_data<1..2^24-1>
        (write-buffer-append-vector24 certs-buf (certificate-entry-cert-data entry))
        ;; Extension extensions<0..2^16-1>
        (let ((ext-buf (make-tls-write-buffer)))
          (dolist (ext (certificate-entry-extensions entry))
            (write-buffer-append ext-buf (serialize-extension ext)))
          (write-buffer-append-vector16 certs-buf (write-buffer-contents ext-buf))))
      (write-buffer-append-vector24 buf (write-buffer-contents certs-buf)))
    (write-buffer-contents buf)))

;;;; CertificateVerify

(defstruct certificate-verify
  "CertificateVerify message."
  (algorithm 0 :type fixnum)
  (signature nil :type (or null octet-vector)))

(defun parse-certificate-verify (data)
  "Parse a CertificateVerify from bytes."
  (let ((buf (make-tls-buffer data)))
    (make-certificate-verify
     :algorithm (buffer-read-uint16 buf)
     :signature (buffer-read-vector16 buf))))

(defun serialize-certificate-verify (cv)
  "Serialize a CertificateVerify message to bytes."
  (let ((buf (make-tls-write-buffer)))
    ;; SignatureScheme algorithm (2 bytes)
    (write-buffer-append-uint16 buf (certificate-verify-algorithm cv))
    ;; opaque signature<0..2^16-1>
    (write-buffer-append-vector16 buf (certificate-verify-signature cv))
    (write-buffer-contents buf)))

;;;; Finished

(defstruct finished-message
  "Finished message."
  (verify-data nil :type (or null octet-vector)))

(defun parse-finished (data hash-length)
  "Parse a Finished message from bytes."
  (make-finished-message
   :verify-data (subseq data 0 hash-length)))

(defun serialize-finished (finished)
  "Serialize a Finished message to bytes."
  (finished-message-verify-data finished))

;;;; NewSessionTicket

(defstruct new-session-ticket
  "NewSessionTicket message."
  (ticket-lifetime 0 :type (unsigned-byte 32))
  (ticket-age-add 0 :type (unsigned-byte 32))
  (ticket-nonce nil :type (or null octet-vector))
  (ticket nil :type (or null octet-vector))
  (extensions nil :type list))

(defun parse-new-session-ticket (data)
  "Parse a NewSessionTicket from bytes."
  (let ((buf (make-tls-buffer data)))
    (make-new-session-ticket
     :ticket-lifetime (buffer-read-uint32 buf)
     :ticket-age-add (buffer-read-uint32 buf)
     :ticket-nonce (buffer-read-vector8 buf)
     :ticket (buffer-read-vector16 buf)
     :extensions (parse-extensions (buffer-read-vector16 buf)))))

;;;; KeyUpdate

(defstruct key-update
  "KeyUpdate message."
  (request-update 0 :type octet))

(defconstant +key-update-not-requested+ 0)
(defconstant +key-update-requested+ 1)

(defun parse-key-update (data)
  "Parse a KeyUpdate from bytes."
  (make-key-update :request-update (aref data 0)))

(defun serialize-key-update (msg)
  "Serialize a KeyUpdate to bytes."
  (octet-vector (key-update-request-update msg)))

;;;; Generic Handshake Message Handling

(defun wrap-handshake-message (msg-type body)
  "Wrap a handshake message body with header."
  (let ((length (length body)))
    (concat-octet-vectors
     (octet-vector msg-type
                   (ldb (byte 8 16) length)
                   (ldb (byte 8 8) length)
                   (ldb (byte 8 0) length))
     body)))

(defun parse-handshake-header (data)
  "Parse a handshake message header.
   Returns (VALUES msg-type length body-start)."
  (when (< (length data) 4)
    (error 'tls-decode-error :message "Handshake message too short"))
  (values (aref data 0)
          (decode-uint24 data 1)
          4))

(defun parse-handshake-message (data &key hash-length)
  "Parse a handshake message from bytes.
   HASH-LENGTH is needed for Finished message parsing."
  (multiple-value-bind (msg-type length body-start)
      (parse-handshake-header data)
    (let ((body (subseq data body-start (+ body-start length))))
      (make-handshake-message
       :type msg-type
       :body (case msg-type
               (#.+handshake-client-hello+
                (parse-client-hello body))
               (#.+handshake-server-hello+
                (parse-server-hello body))
               (#.+handshake-encrypted-extensions+
                (parse-encrypted-extensions body))
               (#.+handshake-certificate-request+
                (parse-certificate-request body))
               (#.+handshake-certificate+
                (parse-certificate-message body))
               (#.+handshake-certificate-verify+
                (parse-certificate-verify body))
               (#.+handshake-finished+
                (parse-finished body (or hash-length 32)))
               (#.+handshake-new-session-ticket+
                (parse-new-session-ticket body))
               (#.+handshake-key-update+
                (parse-key-update body))
               (t body))))))  ; Return raw bytes for unknown types

(defun handshake-message-name (msg-type)
  "Return a human-readable name for a handshake message type."
  (case msg-type
    (#.+handshake-client-hello+ "ClientHello")
    (#.+handshake-server-hello+ "ServerHello")
    (#.+handshake-new-session-ticket+ "NewSessionTicket")
    (#.+handshake-end-of-early-data+ "EndOfEarlyData")
    (#.+handshake-encrypted-extensions+ "EncryptedExtensions")
    (#.+handshake-certificate+ "Certificate")
    (#.+handshake-certificate-request+ "CertificateRequest")
    (#.+handshake-certificate-verify+ "CertificateVerify")
    (#.+handshake-finished+ "Finished")
    (#.+handshake-key-update+ "KeyUpdate")
    (#.+handshake-message-hash+ "MessageHash")
    (t (format nil "Unknown(~D)" msg-type))))
