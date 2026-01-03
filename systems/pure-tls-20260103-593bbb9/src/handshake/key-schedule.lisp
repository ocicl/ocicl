;;; key-schedule.lisp --- TLS 1.3 Key Schedule
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements the TLS 1.3 key schedule (RFC 8446 Section 7.1).

(in-package #:pure-tls)

;;;; Key Schedule State
;;;
;;; The TLS 1.3 key schedule proceeds as follows:
;;;
;;;              0
;;;              |
;;;              v
;;;    PSK ->  HKDF-Extract = Early Secret
;;;              |
;;;              v
;;;        Derive-Secret(., "derived", "")
;;;              |
;;;              v
;;;    (EC)DHE -> HKDF-Extract = Handshake Secret
;;;              |
;;;              +-> Derive-Secret(., "c hs traffic", CH..SH)
;;;              |                 = client_handshake_traffic_secret
;;;              +-> Derive-Secret(., "s hs traffic", CH..SH)
;;;              |                 = server_handshake_traffic_secret
;;;              v
;;;        Derive-Secret(., "derived", "")
;;;              |
;;;              v
;;;    0 -> HKDF-Extract = Master Secret
;;;              |
;;;              +-> Derive-Secret(., "c ap traffic", CH..SF)
;;;              |                 = client_application_traffic_secret_0
;;;              +-> Derive-Secret(., "s ap traffic", CH..SF)
;;;              |                 = server_application_traffic_secret_0
;;;              +-> Derive-Secret(., "exp master", CH..SF)
;;;              |                 = exporter_master_secret
;;;              +-> Derive-Secret(., "res master", CH..CF)
;;;                                = resumption_master_secret

(defstruct key-schedule
  "TLS 1.3 key schedule state."
  ;; Cipher suite determines hash algorithm
  (cipher-suite 0 :type fixnum)
  ;; Client random (for SSLKEYLOGFILE)
  (client-random nil :type (or null octet-vector))
  ;; Secrets at various stages
  (early-secret nil :type (or null octet-vector))
  (handshake-secret nil :type (or null octet-vector))
  (master-secret nil :type (or null octet-vector))
  ;; Traffic secrets
  (client-handshake-traffic-secret nil :type (or null octet-vector))
  (server-handshake-traffic-secret nil :type (or null octet-vector))
  (client-application-traffic-secret nil :type (or null octet-vector))
  (server-application-traffic-secret nil :type (or null octet-vector))
  ;; Other secrets
  (exporter-master-secret nil :type (or null octet-vector))
  (resumption-master-secret nil :type (or null octet-vector))
  ;; Transcript hash accumulator
  (transcript-hash nil))

;;;; SSLKEYLOGFILE Support
;;;
;;; When the SSLKEYLOGFILE environment variable is set, TLS secrets are
;;; logged to that file in NSS keylog format for Wireshark debugging.
;;;
;;; To use with Wireshark:
;;; 1. Set SSLKEYLOGFILE=/path/to/keylog.txt before starting your Lisp
;;; 2. In Wireshark: Edit -> Preferences -> Protocols -> TLS
;;; 3. Set "(Pre)-Master-Secret log filename" to the same path

(defun keylog-write-secret (label client-random secret)
  "Write a secret to the keylog file if SSLKEYLOGFILE is set.
   LABEL is the secret type (e.g., 'CLIENT_HANDSHAKE_TRAFFIC_SECRET').
   CLIENT-RANDOM is the 32-byte client random value.
   SECRET is the secret bytes."
  (when (and client-random secret)
    (let ((filename (get-environment-variable "SSLKEYLOGFILE")))
      (when (and filename (plusp (length filename)))
        (with-open-file (stream filename
                                :direction :output
                                :if-exists :append
                                :if-does-not-exist :create)
          (format stream "~A ~A ~A~%"
                  label
                  (octets-to-hex client-random)
                  (octets-to-hex secret)))))))

(defun keylog-write-handshake-secrets (ks)
  "Write handshake traffic secrets to keylog."
  (let ((client-random (key-schedule-client-random ks)))
    (keylog-write-secret "CLIENT_HANDSHAKE_TRAFFIC_SECRET"
                         client-random
                         (key-schedule-client-handshake-traffic-secret ks))
    (keylog-write-secret "SERVER_HANDSHAKE_TRAFFIC_SECRET"
                         client-random
                         (key-schedule-server-handshake-traffic-secret ks))))

(defun keylog-write-application-secrets (ks)
  "Write application traffic secrets to keylog."
  (let ((client-random (key-schedule-client-random ks)))
    (keylog-write-secret "CLIENT_TRAFFIC_SECRET_0"
                         client-random
                         (key-schedule-client-application-traffic-secret ks))
    (keylog-write-secret "SERVER_TRAFFIC_SECRET_0"
                         client-random
                         (key-schedule-server-application-traffic-secret ks))
    (keylog-write-secret "EXPORTER_SECRET"
                         client-random
                         (key-schedule-exporter-master-secret ks))))

(defun make-key-schedule-state (cipher-suite)
  "Create a new key schedule state for the given cipher suite."
  (let ((digest (cipher-suite-digest cipher-suite)))
    (make-key-schedule
     :cipher-suite cipher-suite
     :transcript-hash (ironclad:make-digest digest))))

;;;; Transcript Hash

(defun key-schedule-update-transcript (ks data)
  "Update the transcript hash with additional data."
  (ironclad:update-digest (key-schedule-transcript-hash ks) data))

(defun key-schedule-transcript-hash-value (ks)
  "Get the current transcript hash value without finalizing."
  (let* ((digest (key-schedule-transcript-hash ks))
         (copy (ironclad:copy-digest digest)))
    (ironclad:produce-digest copy)))

;;;; Key Schedule Derivation

(defun key-schedule-init (ks &optional psk)
  "Initialize the key schedule with Early Secret.
   PSK is the pre-shared key (nil for no PSK)."
  (let* ((digest (cipher-suite-digest (key-schedule-cipher-suite ks)))
         (hash-len (ironclad:digest-length digest))
         (zeros (make-octet-vector hash-len))
         (ikm (or psk zeros)))
    ;; Early Secret = HKDF-Extract(0, PSK or 0)
    (setf (key-schedule-early-secret ks)
          (hkdf-extract zeros ikm :digest digest))
    ks))

(defun key-schedule-derive-handshake-secret (ks shared-secret)
  "Derive the Handshake Secret from (EC)DHE shared secret."
  (let* ((digest (cipher-suite-digest (key-schedule-cipher-suite ks)))
         ;; Derive-Secret(Early Secret, "derived", "")
         (derived (derive-secret (key-schedule-early-secret ks)
                                 "derived"
                                 (make-octet-vector 0)
                                 :digest digest)))
    ;; Handshake Secret = HKDF-Extract(derived, shared_secret)
    (setf (key-schedule-handshake-secret ks)
          (hkdf-extract derived shared-secret :digest digest))
    ks))

(defun key-schedule-derive-handshake-traffic-secrets (ks transcript)
  "Derive client and server handshake traffic secrets.
   TRANSCRIPT is ClientHello...ServerHello."
  (let ((digest (cipher-suite-digest (key-schedule-cipher-suite ks)))
        (hs-secret (key-schedule-handshake-secret ks)))
    ;; client_handshake_traffic_secret
    (setf (key-schedule-client-handshake-traffic-secret ks)
          (derive-secret hs-secret "c hs traffic" transcript :digest digest))
    ;; server_handshake_traffic_secret
    (setf (key-schedule-server-handshake-traffic-secret ks)
          (derive-secret hs-secret "s hs traffic" transcript :digest digest))
    ks))

(defun key-schedule-derive-master-secret (ks)
  "Derive the Master Secret."
  (let* ((digest (cipher-suite-digest (key-schedule-cipher-suite ks)))
         (hash-len (ironclad:digest-length digest))
         (zeros (make-octet-vector hash-len))
         ;; Derive-Secret(Handshake Secret, "derived", "")
         (derived (derive-secret (key-schedule-handshake-secret ks)
                                 "derived"
                                 (make-octet-vector 0)
                                 :digest digest)))
    ;; Master Secret = HKDF-Extract(derived, 0)
    (setf (key-schedule-master-secret ks)
          (hkdf-extract derived zeros :digest digest))
    ks))

(defun key-schedule-derive-application-traffic-secrets (ks transcript)
  "Derive client and server application traffic secrets.
   TRANSCRIPT is ClientHello...server Finished."
  (let ((digest (cipher-suite-digest (key-schedule-cipher-suite ks)))
        (master (key-schedule-master-secret ks)))
    ;; client_application_traffic_secret_0
    (setf (key-schedule-client-application-traffic-secret ks)
          (derive-secret master "c ap traffic" transcript :digest digest))
    ;; server_application_traffic_secret_0
    (setf (key-schedule-server-application-traffic-secret ks)
          (derive-secret master "s ap traffic" transcript :digest digest))
    ;; exporter_master_secret
    (setf (key-schedule-exporter-master-secret ks)
          (derive-secret master "exp master" transcript :digest digest))
    ks))

(defun key-schedule-derive-resumption-master-secret (ks transcript)
  "Derive the resumption master secret.
   TRANSCRIPT is ClientHello...client Finished."
  (let ((digest (cipher-suite-digest (key-schedule-cipher-suite ks)))
        (master (key-schedule-master-secret ks)))
    (setf (key-schedule-resumption-master-secret ks)
          (derive-secret master "res master" transcript :digest digest))
    ks))

;;;; Finished Key and Verify Data

(defun derive-finished-key (base-key cipher-suite)
  "Derive the finished key from a base key."
  (let* ((digest (cipher-suite-digest cipher-suite))
         (hash-len (ironclad:digest-length digest)))
    (hkdf-expand-label base-key "finished" #() hash-len :digest digest)))

(defun compute-finished-verify-data (base-key transcript cipher-suite)
  "Compute the verify_data for a Finished message.
   BASE-KEY is the appropriate traffic secret.
   TRANSCRIPT is the hash of handshake messages up to this point."
  (let* ((digest (cipher-suite-digest cipher-suite))
         (finished-key (derive-finished-key base-key cipher-suite))
         (transcript-hash (if (typep transcript 'octet-vector)
                              transcript
                              (ironclad:digest-sequence digest transcript))))
    (let ((hmac (ironclad:make-hmac finished-key digest)))
      (ironclad:update-mac hmac transcript-hash)
      (ironclad:produce-mac hmac))))

;;;; Traffic Key Derivation

(defun key-schedule-derive-read-keys (ks direction)
  "Derive traffic keys for reading.
   DIRECTION is :handshake or :application."
  (let* ((cipher-suite (key-schedule-cipher-suite ks))
         (secret (ecase direction
                   (:handshake (key-schedule-server-handshake-traffic-secret ks))
                   (:application (key-schedule-server-application-traffic-secret ks)))))
    (derive-traffic-keys secret cipher-suite)))

(defun key-schedule-derive-write-keys (ks direction)
  "Derive traffic keys for writing.
   DIRECTION is :handshake or :application."
  (let* ((cipher-suite (key-schedule-cipher-suite ks))
         (secret (ecase direction
                   (:handshake (key-schedule-client-handshake-traffic-secret ks))
                   (:application (key-schedule-client-application-traffic-secret ks)))))
    (derive-traffic-keys secret cipher-suite)))

;;;; Key Update

(defun key-schedule-update-traffic-secret (traffic-secret cipher-suite)
  "Compute the next generation traffic secret for key update."
  (let* ((digest (cipher-suite-digest cipher-suite))
         (hash-len (ironclad:digest-length digest)))
    (hkdf-expand-label traffic-secret "traffic upd" #() hash-len :digest digest)))
