;;; record-layer.lisp --- TLS 1.3 Record Layer Protocol
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements the TLS 1.3 record layer (RFC 8446 Section 5).

(in-package #:pure-tls)

;;;; TLS Record Structure
;;;
;;; Plaintext record (before encryption):
;;;   struct {
;;;     ContentType type;
;;;     ProtocolVersion legacy_record_version = 0x0303;  /* TLS 1.2 */
;;;     uint16 length;
;;;     opaque fragment[TLSPlaintext.length];
;;;   } TLSPlaintext;
;;;
;;; Ciphertext record (after encryption, TLS 1.3):
;;;   struct {
;;;     ContentType opaque_type = application_data; /* 23 */
;;;     ProtocolVersion legacy_record_version = 0x0303;
;;;     uint16 length;
;;;     opaque encrypted_record[TLSCiphertext.length];
;;;   } TLSCiphertext;

(defstruct tls-record
  "A TLS record."
  (content-type 0 :type octet)
  (version +tls-1.2+ :type fixnum)
  (fragment nil :type (or null octet-vector)))

;;;; Record Layer I/O

(defun read-exact-bytes (stream buffer count &optional request-context)
  "Read exactly COUNT bytes from STREAM into BUFFER.
   Loops until all bytes are read or EOF is reached.
   Returns the number of bytes actually read (may be less than COUNT at EOF).
   If REQUEST-CONTEXT is provided, checks for deadline/cancellation before each read."
  (let ((total-read 0))
    (loop while (< total-read count)
          do (check-tls-context)
             (let ((bytes-read (read-sequence buffer stream
                                              :start total-read
                                              :end count)))
               (when (= bytes-read total-read)
                 ;; No progress - EOF reached
                 (return total-read))
               (setf total-read bytes-read)))
    total-read))

(defun read-tls-record (stream &optional request-context)
  "Read a TLS record from STREAM.
   Returns a TLS-RECORD structure or signals an error.
   Properly handles short reads from the underlying stream.
   Validates legacy_record_version per RFC 8446 Section 5.1.
   If REQUEST-CONTEXT is provided, checks for deadline/cancellation during reads."
  (let ((header (make-octet-vector 5)))
    ;; Read 5-byte header (loop until complete or EOF)
    (let ((bytes-read (read-exact-bytes stream header 5 request-context)))
      (when (zerop bytes-read)
        (error 'tls-connection-closed :clean nil))
      (when (< bytes-read 5)
        (error 'tls-decode-error
               :message (format nil "Incomplete record header: expected 5 bytes, got ~D"
                                bytes-read))))
    ;; Parse header
    (let* ((content-type (aref header 0))
           (version (decode-uint16 header 1))
           (length (decode-uint16 header 3)))
      ;; Validate content type - must be a valid TLS content type (20-24)
      ;; This quickly rejects SSLv2 records which have high-bit-set bytes
      ;; in position 0, preventing us from waiting forever for invalid lengths.
      (unless (and (>= content-type +content-type-change-cipher-spec+)  ; 20
                   (<= content-type 24))  ; heartbeat is 24
        (error 'tls-decode-error
               :message (format nil ":WRONG_VERSION_NUMBER: Invalid content type ~D (not a valid TLS record)"
                                content-type)))
      ;; RFC 8446 Section 5.1: legacy_record_version SHOULD be 0x0303 for
      ;; all TLS 1.3 records, but implementations MUST NOT check this field.
      ;; The version is legacy and version negotiation happens at handshake level.
      ;; Accept any record version for maximum compatibility.
      ;; Validate length
      (when (> length +max-record-size-with-padding+)
        (error 'tls-record-overflow :size length))
      ;; Read fragment (loop until complete or EOF)
      (let ((fragment (make-octet-vector length)))
        (let ((bytes-read (read-exact-bytes stream fragment length request-context)))
          (when (< bytes-read length)
            (error 'tls-decode-error
                   :message (format nil "Incomplete record fragment: expected ~D bytes, got ~D"
                                    length bytes-read))))
        (make-tls-record :content-type content-type
                         :version version
                         :fragment fragment)))))

(defun write-tls-record (stream record)
  "Write a TLS record to STREAM."
  (let* ((fragment (tls-record-fragment record))
         (length (length fragment))
         (header (make-octet-vector 5)))
    ;; Validate length
    (when (> length +max-record-size-with-padding+)
      (error 'tls-record-overflow :size length))
    ;; Build header
    (setf (aref header 0) (tls-record-content-type record))
    (setf (aref header 1) (ldb (byte 8 8) (tls-record-version record)))
    (setf (aref header 2) (ldb (byte 8 0) (tls-record-version record)))
    (setf (aref header 3) (ldb (byte 8 8) length))
    (setf (aref header 4) (ldb (byte 8 0) length))
    ;; Write header and fragment
    (write-sequence header stream)
    (write-sequence fragment stream)
    (force-output stream)))

(defun make-plaintext-record (content-type data)
  "Create a plaintext TLS record."
  (make-tls-record :content-type content-type
                   :version +tls-1.2+
                   :fragment data))

;;;; Record Encryption/Decryption

(defconstant +max-ccs-messages+ 32
  "Maximum number of change_cipher_spec messages allowed (DoS protection).")

(defstruct (record-layer (:constructor %make-record-layer))
  "TLS record layer state."
  (read-cipher nil :type (or null aead-cipher))
  (write-cipher nil :type (or null aead-cipher))
  (cipher-suite 0 :type fixnum)
  (stream nil)
  (max-send-fragment +max-record-size+ :type fixnum)
  (ccs-count 0 :type fixnum)
  (request-context nil :type t))

(defun make-record-layer (stream &key (max-send-fragment +max-record-size+)
                                      request-context)
  "Create a new record layer for the given stream.
   MAX-SEND-FRAGMENT sets the maximum plaintext size for outgoing records.
   REQUEST-CONTEXT is an optional cl-cancel context for timeout/cancellation support."
  (%make-record-layer :stream stream
                      :max-send-fragment max-send-fragment
                      :request-context request-context))

(defun record-layer-install-keys (layer direction key iv cipher-suite)
  "Install encryption keys for the specified direction (:read or :write)."
  (let ((cipher (make-aead cipher-suite key iv)))
    (ecase direction
      (:read (setf (record-layer-read-cipher layer) cipher))
      (:write (setf (record-layer-write-cipher layer) cipher)))
    (setf (record-layer-cipher-suite layer) cipher-suite)))

(defun record-layer-read (layer)
  "Read and potentially decrypt a record from the record layer.
   Returns (VALUES content-type plaintext).
   Checks request-context for deadline/cancellation if present."
  (check-tls-context)
  (let* ((record (read-tls-record (record-layer-stream layer)
                                   (record-layer-request-context layer)))
         (content-type (tls-record-content-type record))
         (fragment (tls-record-fragment record))
         (cipher (record-layer-read-cipher layer)))
    ;; Handle change_cipher_spec (ignored in TLS 1.3 but may be sent)
    (when (= content-type +content-type-change-cipher-spec+)
      ;; Count CCS messages to prevent DoS
      (incf (record-layer-ccs-count layer))
      (when (> (record-layer-ccs-count layer) +max-ccs-messages+)
        (record-layer-write-alert layer +alert-level-fatal+ +alert-unexpected-message+)
        (error 'tls-handshake-error
               :message ":TOO_MANY_EMPTY_FRAGMENTS: Too many change_cipher_spec messages"))
      ;; Just return and let caller handle/ignore
      (return-from record-layer-read
        (values content-type fragment)))
    ;; If encryption is established, all records MUST be encrypted (content-type 23)
    ;; RFC 8446 Section 5.1: After the handshake keys are installed, all records
    ;; except CCS must use the encrypted record format (application_data wrapper)
    (when cipher
      (unless (= content-type +content-type-application-data+)
        (record-layer-write-alert layer +alert-level-fatal+ +alert-unexpected-message+)
        (error 'tls-handshake-error
               :message (format nil ":INVALID_OUTER_RECORD_TYPE: Expected encrypted record (23), got ~D"
                               content-type)))
      ;; Decrypt the record
      (let ((header (octet-vector content-type
                                  (ldb (byte 8 8) (tls-record-version record))
                                  (ldb (byte 8 0) (tls-record-version record))
                                  (ldb (byte 8 8) (length fragment))
                                  (ldb (byte 8 0) (length fragment)))))
        ;; tls13-decrypt-record returns (plaintext, content-type)
        ;; We need to return (content-type, plaintext)
        ;; Catch record overflow to send alert before re-raising
        (handler-bind ((tls-record-overflow
                         (lambda (c)
                           (declare (ignore c))
                           (record-layer-write-alert layer
                                                     +alert-level-fatal+
                                                     +alert-record-overflow+))))
          (multiple-value-bind (plaintext inner-content-type)
              (tls13-decrypt-record cipher fragment header)
            (return-from record-layer-read
              (values inner-content-type plaintext))))))
    ;; No encryption - return plaintext record
    (values content-type fragment)))

(defun record-layer-write (layer content-type data)
  "Write and potentially encrypt a record to the record layer."
  (let* ((cipher (record-layer-write-cipher layer))
         (stream (record-layer-stream layer)))
    (if cipher
        ;; Encrypted write
        (let* ((encrypted (tls13-encrypt-record cipher content-type data))
               (record (make-tls-record
                        :content-type +content-type-application-data+
                        :version +tls-1.2+
                        :fragment encrypted)))
          (write-tls-record stream record))
        ;; Plaintext write
        (let ((record (make-plaintext-record content-type data)))
          (write-tls-record stream record)))))

(defun record-layer-write-alert (layer level description)
  "Write an alert record."
  (let ((data (octet-vector level description)))
    (record-layer-write layer +content-type-alert+ data)))

(defun record-layer-write-handshake (layer handshake-data)
  "Write a handshake record, fragmenting if necessary."
  (record-layer-write-fragmented layer +content-type-handshake+ handshake-data))

(defun record-layer-write-application-data (layer data)
  "Write application data, fragmenting if necessary."
  (record-layer-write-fragmented layer +content-type-application-data+ data))

(defun record-layer-write-change-cipher-spec (layer)
  "Write a dummy change_cipher_spec record for middlebox compatibility.
   Per RFC 8446 Appendix D.4, TLS 1.3 implementations SHOULD send
   a single CCS record immediately after the first ClientHello (client)
   or ServerHello (server) for compatibility with broken middleboxes.
   The CCS record is always sent unencrypted with content byte 0x01."
  (let* ((ccs-data (octet-vector 1))  ; Single byte 0x01
         (record (make-tls-record :content-type +content-type-change-cipher-spec+
                                  :version +tls-1.2+
                                  :fragment ccs-data)))
    (write-tls-record (record-layer-stream layer) record)))

;;;; Record Fragmentation

(defun fragment-data (data max-size)
  "Split DATA into fragments of at most MAX-SIZE bytes.
   Returns a list of octet vectors."
  (if (<= (length data) max-size)
      (list data)
      (loop for start from 0 below (length data) by max-size
            collect (subseq data start (min (+ start max-size) (length data))))))

(defun record-layer-write-fragmented (layer content-type data)
  "Write DATA as potentially multiple records, fragmenting if necessary.
   Respects the max-send-fragment setting of the record layer.
   MAX-SEND-FRAGMENT is the maximum plaintext payload size before encryption."
  (let ((max-size (record-layer-max-send-fragment layer)))
    (dolist (fragment (fragment-data data max-size))
      (record-layer-write layer content-type fragment))))

;;;; Alert Processing

(defun process-alert (content &optional record-layer)
  "Process an alert record and signal appropriate condition.
   RECORD-LAYER, if provided, is used to send response alerts before erroring."
  (when (< (length content) 2)
    ;; Send decode_error alert for malformed alerts
    (when record-layer
      (handler-case
          (record-layer-write-alert record-layer +alert-level-fatal+ +alert-decode-error+)
        (error () nil)))
    (error 'tls-error :message ":BAD_ALERT: Alert too short"))
  ;; An alert record must be exactly 2 bytes - reject "double alerts"
  (when (> (length content) 2)
    (when record-layer
      (handler-case
          (record-layer-write-alert record-layer +alert-level-fatal+ +alert-decode-error+)
        (error () nil)))
    (error 'tls-error :message ":BAD_ALERT: Alert record too long"))
  (let ((level (aref content 0))
        (description (aref content 1)))
    ;; close_notify is a clean shutdown; respond with our own close_notify if possible
    (when (= description +alert-close-notify+)
      (when record-layer
        (handler-case
            (record-layer-write-alert record-layer +alert-level-warning+ +alert-close-notify+)
          (error () nil)))
      (error 'tls-connection-closed :clean t))
    ;; Check for invalid alert level or unknown alert description
    ;; Valid alert levels are 1 (warning) and 2 (fatal)
    (unless (or (= level +alert-level-warning+) (= level +alert-level-fatal+))
      (when record-layer
        (handler-case
            (record-layer-write-alert record-layer +alert-level-fatal+ +alert-illegal-parameter+)
          (error () nil)))
      (error 'tls-error
             :message (format nil ":UNKNOWN_ALERT_TYPE: Unknown alert level ~D" level)))
    ;; Check for unknown alert description - send illegal_parameter
    (unless (known-alert-description-p description)
      (when record-layer
        (handler-case
            (record-layer-write-alert record-layer +alert-level-fatal+ +alert-illegal-parameter+)
          (error () nil)))
      (error 'tls-error
             :message (format nil ":UNKNOWN_ALERT_TYPE: Unknown alert type ~D" description)))
    ;; RFC 8446 Section 6: In TLS 1.3, all alerts except close_notify and
    ;; user_canceled MUST be sent at fatal level.
    (when (= level +alert-level-warning+)
      (cond
        ;; user_canceled warning is allowed in TLS 1.3 - ignore it
        ((= description +alert-user-canceled+)
         (return-from process-alert nil))
        ;; All other warning alerts are forbidden in TLS 1.3
        (t
         ;; Send decode_error alert for invalid warning alerts (per BoringSSL expectation)
         (when record-layer
           (handler-case
               (record-layer-write-alert record-layer +alert-level-fatal+ +alert-decode-error+)
             (error () nil)))
         ;; Treat forbidden warning alerts as fatal protocol error
         (error 'tls-error
                :message (format nil ":BAD_ALERT: Invalid warning-level alert in TLS 1.3: ~A"
                                (alert-description-name description))))))
    ;; All other alerts (fatal level) signal an error
    (error 'tls-alert-error :level level :description description)))
