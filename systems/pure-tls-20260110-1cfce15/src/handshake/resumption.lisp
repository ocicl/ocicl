;;; resumption.lisp --- TLS 1.3 Session Resumption (PSK)
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements TLS 1.3 session resumption using Pre-Shared Keys (PSK)
;;; derived from NewSessionTicket messages.

(in-package #:pure-tls)

;;;; Session Ticket Structure
;;;
;;; A session ticket contains all the information needed to resume a session.

(defstruct session-ticket
  "A TLS 1.3 session ticket for resumption."
  ;; The ticket identity (opaque to client, sent back to server)
  (identity nil :type (or null octet-vector))
  ;; The resumption master secret from the original handshake
  (resumption-master-secret nil :type (or null octet-vector))
  ;; Cipher suite used in the original session
  (cipher-suite 0 :type fixnum)
  ;; Ticket lifetime in seconds
  (lifetime 0 :type (unsigned-byte 32))
  ;; Ticket age add (for obfuscation)
  (age-add 0 :type (unsigned-byte 32))
  ;; Ticket nonce (for PSK derivation)
  (nonce nil :type (or null octet-vector))
  ;; When the ticket was received (internal-real-time)
  (received-at 0 :type integer)
  ;; Server hostname (for matching)
  (hostname nil :type (or null string))
  ;; Max early data size (0 if not allowed)
  (max-early-data-size 0 :type (unsigned-byte 32)))

;;;; Session Ticket Cache

(defvar *session-ticket-cache* (make-hash-table :test 'equal)
  "Cache of session tickets keyed by hostname.")

(defvar *session-ticket-cache-lock* nil
  "Lock for thread-safe access to ticket cache (if available).")

(defun session-ticket-cache-get (hostname)
  "Get a valid session ticket for HOSTNAME, or NIL if none available."
  (let ((ticket (gethash hostname *session-ticket-cache*)))
    (when ticket
      ;; Check if ticket is still valid
      (let* ((now (get-internal-real-time))
             (age-ms (/ (* 1000 (- now (session-ticket-received-at ticket)))
                        internal-time-units-per-second)))
        (cond ((< age-ms (* 1000 (session-ticket-lifetime ticket))) ticket)
      (t 
              (remhash hostname *session-ticket-cache*)
              nil))))))

(defun session-ticket-cache-put (hostname ticket)
  "Store a session ticket for HOSTNAME."
  (setf (session-ticket-hostname ticket) hostname)
  (setf (session-ticket-received-at ticket) (get-internal-real-time))
  (setf (gethash hostname *session-ticket-cache*) ticket))

(defun session-ticket-cache-clear (&optional hostname)
  "Clear session tickets. If HOSTNAME is provided, only clear that one."
  (if hostname
      (remhash hostname *session-ticket-cache*)
      (clrhash *session-ticket-cache*)))

;;;; PSK Derivation
;;;
;;; The PSK for resumption is derived from the resumption master secret
;;; and the ticket nonce using HKDF-Expand-Label.

(defun derive-resumption-psk (resumption-master-secret nonce cipher-suite)
  "Derive the PSK from resumption master secret and ticket nonce.
   PSK = HKDF-Expand-Label(resumption_master_secret, 'resumption', nonce, Hash.length)"
  (let* ((digest (cipher-suite-digest cipher-suite))
         (hash-len (ironclad:digest-length digest)))
    (hkdf-expand-label resumption-master-secret "resumption" nonce hash-len
                       :digest digest)))

;;;; Binder Calculation
;;;
;;; The binder is an HMAC over the transcript hash (up to but not including
;;; the binders themselves) using a key derived from the PSK.

(defun derive-binder-key (psk cipher-suite)
  "Derive the binder key from PSK.
   binder_key = Derive-Secret(Early Secret, 'ext binder', '')"
  (let* ((digest (cipher-suite-digest cipher-suite))
         (hash-len (ironclad:digest-length digest))
         (zeros (make-octet-vector hash-len))
         ;; Early Secret = HKDF-Extract(0, PSK)
         (early-secret (hkdf-extract zeros psk :digest digest)))
    ;; binder_key = Derive-Secret(Early Secret, "ext binder" | "res binder", "")
    ;; For external PSKs use "ext binder", for resumption use "res binder"
    (derive-secret early-secret "res binder" (make-octet-vector 0) :digest digest)))

(defun compute-binder (psk transcript-hash cipher-suite)
  "Compute the PSK binder.
   TRANSCRIPT-HASH is the hash of the partial ClientHello (up to but not
   including the binders)."
  (let* ((digest (cipher-suite-digest cipher-suite))
         (binder-key (derive-binder-key psk cipher-suite))
         (finished-key (derive-finished-key binder-key cipher-suite))
         (hmac (ironclad:make-hmac finished-key digest)))
    (ironclad:update-mac hmac transcript-hash)
    (ironclad:produce-mac hmac)))

(defun verify-binder (psk transcript-hash binder cipher-suite)
  "Verify a PSK binder. Returns T if valid, NIL otherwise."
  (let ((expected (compute-binder psk transcript-hash cipher-suite)))
    (constant-time-equal expected binder)))

;;;; NewSessionTicket Handling

(defun process-new-session-ticket (nst resumption-master-secret cipher-suite hostname)
  "Process a NewSessionTicket message and cache the ticket.
   NST is the parsed new-session-ticket structure.
   Returns the session-ticket structure."
  (let ((ticket (make-session-ticket
                 :identity (new-session-ticket-ticket nst)
                 :resumption-master-secret resumption-master-secret
                 :cipher-suite cipher-suite
                 :lifetime (new-session-ticket-ticket-lifetime nst)
                 :age-add (new-session-ticket-ticket-age-add nst)
                 :nonce (new-session-ticket-ticket-nonce nst)
                 :hostname hostname)))
    ;; Check for early_data extension
    ;; RFC 8446 Section 4.6.1: early_data in NewSessionTicket contains uint32 max_early_data_size
    (let ((early-data-ext (find-extension (new-session-ticket-extensions nst)
                                          +extension-early-data+)))
      (when early-data-ext
        (let ((data (tls-extension-data early-data-ext)))
          ;; Must be exactly 4 bytes (uint32)
          (unless (and data (= (length data) 4))
            (error 'tls-handshake-error
                   :message ":DECODE_ERROR: Invalid early_data extension size in NewSessionTicket"))
          (setf (session-ticket-max-early-data-size ticket)
                (decode-uint32 data 0)))))
    ;; Cache the ticket
    (when hostname
      (session-ticket-cache-put hostname ticket))
    ticket))

(defun serialize-new-session-ticket (nst)
  "Serialize a NewSessionTicket message to bytes."
  (let ((buf (make-tls-write-buffer)))
    ;; uint32 ticket_lifetime
    (write-buffer-append-uint32 buf (new-session-ticket-ticket-lifetime nst))
    ;; uint32 ticket_age_add
    (write-buffer-append-uint32 buf (new-session-ticket-ticket-age-add nst))
    ;; opaque ticket_nonce<0..255>
    (write-buffer-append-vector8 buf (or (new-session-ticket-ticket-nonce nst)
                                          (make-octet-vector 0)))
    ;; opaque ticket<1..2^16-1>
    (write-buffer-append-vector16 buf (new-session-ticket-ticket nst))
    ;; Extension extensions<0..2^16-2>
    (let ((ext-buf (make-tls-write-buffer)))
      (dolist (ext (new-session-ticket-extensions nst))
        (write-buffer-append ext-buf (serialize-extension ext)))
      (write-buffer-append-vector16 buf (write-buffer-contents ext-buf)))
    (write-buffer-contents buf)))

;;;; Obfuscated Ticket Age

(defun compute-obfuscated-ticket-age (ticket)
  "Compute the obfuscated ticket age for a PSK offer."
  (let* ((now (get-internal-real-time))
         (age-ms (truncate (* 1000 (- now (session-ticket-received-at ticket)))
                           internal-time-units-per-second)))
    ;; obfuscated_ticket_age = (age_ms + ticket_age_add) mod 2^32
    (logand (+ age-ms (session-ticket-age-add ticket))
            #xFFFFFFFF)))

;;;; Server-Side Ticket Encryption
;;;
;;; For stateless session resumption, the server encrypts all session
;;; state into the ticket itself. The ticket contains:
;;; - Resumption master secret
;;; - Cipher suite
;;; - Ticket creation time
;;; - Ticket nonce
;;;
;;; The ticket is encrypted with a server-side key using AES-256-GCM.

(defvar *server-ticket-key* nil
  "Server-side key for encrypting session tickets.
   Should be set to a 32-byte random key on server startup.
   If nil, a random key is generated on first use.")

(defun ensure-server-ticket-key ()
  "Ensure the server ticket key is initialized."
  (unless *server-ticket-key*
    (setf *server-ticket-key* (random-bytes 32)))
  *server-ticket-key*)

(defun encode-uint32-be (n)
  "Encode a 32-bit unsigned integer as 4 bytes big-endian."
  (octet-vector
   (ldb (byte 8 24) n)
   (ldb (byte 8 16) n)
   (ldb (byte 8 8) n)
   (ldb (byte 8 0) n)))

(defun decode-uint32-be (data offset)
  "Decode a 32-bit unsigned integer from 4 bytes big-endian."
  (logior (ash (aref data offset) 24)
          (ash (aref data (1+ offset)) 16)
          (ash (aref data (+ offset 2)) 8)
          (aref data (+ offset 3))))

(defun encode-uint64-be (n)
  "Encode a 64-bit unsigned integer as 8 bytes big-endian."
  (octet-vector
   (ldb (byte 8 56) n)
   (ldb (byte 8 48) n)
   (ldb (byte 8 40) n)
   (ldb (byte 8 32) n)
   (ldb (byte 8 24) n)
   (ldb (byte 8 16) n)
   (ldb (byte 8 8) n)
   (ldb (byte 8 0) n)))

(defun decode-uint64-be (data offset)
  "Decode a 64-bit unsigned integer from 8 bytes big-endian."
  (logior (ash (aref data offset) 56)
          (ash (aref data (1+ offset)) 48)
          (ash (aref data (+ offset 2)) 40)
          (ash (aref data (+ offset 3)) 32)
          (ash (aref data (+ offset 4)) 24)
          (ash (aref data (+ offset 5)) 16)
          (ash (aref data (+ offset 6)) 8)
          (aref data (+ offset 7))))

(defun encrypt-session-ticket (resumption-master-secret cipher-suite nonce creation-time)
  "Encrypt session state into an opaque ticket.
   Returns the encrypted ticket bytes suitable for sending to client.
   Format: iv (12 bytes) || ciphertext || tag (16 bytes)"
  (let* ((key (ensure-server-ticket-key))
         (iv (random-bytes 12))
         ;; Plaintext: resumption_master_secret (32/48) || cipher_suite (2) || nonce_len (1) || nonce (var) || creation_time (8)
         (nonce-len (length nonce))
         (rms-len (length resumption-master-secret))
         (plaintext (make-octet-vector (+ rms-len 2 1 nonce-len 8))))
    ;; Pack the plaintext
    (replace plaintext resumption-master-secret)
    (setf (aref plaintext rms-len) (ldb (byte 8 8) cipher-suite))
    (setf (aref plaintext (1+ rms-len)) (ldb (byte 8 0) cipher-suite))
    (setf (aref plaintext (+ rms-len 2)) nonce-len)
    (replace plaintext nonce :start1 (+ rms-len 3))
    (let ((time-bytes (encode-uint64-be creation-time)))
      (replace plaintext time-bytes :start1 (+ rms-len 3 nonce-len)))
    ;; Encrypt with AES-256-GCM using empty AAD
    ;; aes-gcm-encrypt returns ciphertext || tag
    (let ((encrypted (aes-gcm-encrypt key iv plaintext (make-octet-vector 0))))
      ;; Return: iv || encrypted (which is ciphertext || tag)
      (concat-octet-vectors iv encrypted))))

(defun decrypt-session-ticket (encrypted-ticket)
  "Decrypt an encrypted session ticket.
   Returns (VALUES resumption-master-secret cipher-suite nonce creation-time) on success,
   or NIL if decryption fails."
  (when (< (length encrypted-ticket) 28) ; 12 iv + 0 min plaintext + 16 tag
    (return-from decrypt-session-ticket nil))
  (let* ((key (ensure-server-ticket-key))
         (iv (subseq encrypted-ticket 0 12))
         ;; ciphertext-with-tag is everything after the iv
         (ciphertext-with-tag (subseq encrypted-ticket 12)))
    (handler-case
        (let ((plaintext (aes-gcm-decrypt key iv ciphertext-with-tag (make-octet-vector 0))))
          (when plaintext
            ;; Parse the plaintext
            ;; The RMS length depends on cipher suite - we'll detect from remaining size
            (let* ((total-len (length plaintext))
                   ;; After cipher suite (2) and nonce-len (1) and time (8), remaining is RMS + nonce
                   ;; Minimum: 32 (rms) + 2 (cs) + 1 (nonce-len) + 0 (nonce) + 8 (time) = 43
                   ;; We need to figure out rms-len from cipher suite
                   ;; Try assuming 32-byte RMS first (SHA-256 based suites)
                   (test-cs-offset 32)
                   (test-nonce-len-offset 34)
                   (cipher-suite (when (> total-len test-nonce-len-offset)
                                   (logior (ash (aref plaintext test-cs-offset) 8)
                                           (aref plaintext (1+ test-cs-offset)))))
                   (rms-len (if (and cipher-suite
                                     (= cipher-suite +tls-aes-256-gcm-sha384+))
                                48
                                32)))
              ;; Re-parse with correct RMS length
              (let* ((cs-offset rms-len)
                     (nonce-len-offset (+ rms-len 2))
                     (nonce-offset (+ rms-len 3)))
                (when (> total-len nonce-len-offset)
                  (let* ((cipher-suite (logior (ash (aref plaintext cs-offset) 8)
                                               (aref plaintext (1+ cs-offset))))
                         (nonce-len (aref plaintext nonce-len-offset))
                         (time-offset (+ nonce-offset nonce-len)))
                    (when (>= total-len (+ time-offset 8))
                      (let ((resumption-master-secret (subseq plaintext 0 rms-len))
                            (nonce (subseq plaintext nonce-offset (+ nonce-offset nonce-len)))
                            (creation-time (decode-uint64-be plaintext time-offset)))
                        (values resumption-master-secret cipher-suite nonce creation-time)))))))))
      (error () nil))))

(defun validate-ticket-age (creation-time ticket-lifetime obfuscated-age age-add)
  "Validate that the ticket age is within acceptable bounds.
   Returns T if valid, NIL otherwise."
  (let* ((now (get-internal-real-time))
         (actual-age-ms (truncate (* 1000 (- now creation-time))
                                  internal-time-units-per-second))
         ;; Deobfuscate the client-reported age
         (reported-age-ms (logand (- obfuscated-age age-add) #xFFFFFFFF)))
    ;; Check that ticket hasn't expired
    (when (> actual-age-ms (* 1000 ticket-lifetime))
      (return-from validate-ticket-age nil))
    ;; Allow some tolerance (10 seconds) for clock skew
    (< (abs (- actual-age-ms reported-age-ms)) 10000)))
