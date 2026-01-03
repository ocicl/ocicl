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

(defun read-exact-bytes (stream buffer count)
  "Read exactly COUNT bytes from STREAM into BUFFER.
   Loops until all bytes are read or EOF is reached.
   Returns the number of bytes actually read (may be less than COUNT at EOF)."
  (let ((total-read 0))
    (loop while (< total-read count)
          do (let ((bytes-read (read-sequence buffer stream
                                              :start total-read
                                              :end count)))
               (when (= bytes-read total-read)
                 ;; No progress - EOF reached
                 (return total-read))
               (setf total-read bytes-read)))
    total-read))

(defun read-tls-record (stream)
  "Read a TLS record from STREAM.
   Returns a TLS-RECORD structure or signals an error.
   Properly handles short reads from the underlying stream."
  (let ((header (make-octet-vector 5)))
    ;; Read 5-byte header (loop until complete or EOF)
    (let ((bytes-read (read-exact-bytes stream header 5)))
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
      ;; Validate length
      (when (> length +max-record-size-with-padding+)
        (error 'tls-record-overflow :size length))
      ;; Read fragment (loop until complete or EOF)
      (let ((fragment (make-octet-vector length)))
        (let ((bytes-read (read-exact-bytes stream fragment length)))
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

(defstruct (record-layer (:constructor %make-record-layer))
  "TLS record layer state."
  (read-cipher nil :type (or null aead-cipher))
  (write-cipher nil :type (or null aead-cipher))
  (cipher-suite 0 :type fixnum)
  (stream nil))

(defun make-record-layer (stream)
  "Create a new record layer for the given stream."
  (%make-record-layer :stream stream))

(defun record-layer-install-keys (layer direction key iv cipher-suite)
  "Install encryption keys for the specified direction (:read or :write)."
  (let ((cipher (make-aead cipher-suite key iv)))
    (ecase direction
      (:read (setf (record-layer-read-cipher layer) cipher))
      (:write (setf (record-layer-write-cipher layer) cipher)))
    (setf (record-layer-cipher-suite layer) cipher-suite)))

(defun record-layer-read (layer)
  "Read and potentially decrypt a record from the record layer.
   Returns (VALUES content-type plaintext)."
  (let* ((record (read-tls-record (record-layer-stream layer)))
         (content-type (tls-record-content-type record))
         (fragment (tls-record-fragment record))
         (cipher (record-layer-read-cipher layer)))
    ;; Handle change_cipher_spec (ignored in TLS 1.3 but may be sent)
    (when (= content-type +content-type-change-cipher-spec+)
      ;; Just return and let caller handle/ignore
      (return-from record-layer-read
        (values content-type fragment)))
    ;; If encrypted, decrypt
    (if (and cipher (= content-type +content-type-application-data+))
        (let ((header (octet-vector content-type
                                    (ldb (byte 8 8) (tls-record-version record))
                                    (ldb (byte 8 0) (tls-record-version record))
                                    (ldb (byte 8 8) (length fragment))
                                    (ldb (byte 8 0) (length fragment)))))
          ;; tls13-decrypt-record returns (plaintext, content-type)
          ;; We need to return (content-type, plaintext)
          (multiple-value-bind (plaintext inner-content-type)
              (tls13-decrypt-record cipher fragment header)
            (values inner-content-type plaintext)))
        (values content-type fragment))))

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
  "Write a handshake record."
  (record-layer-write layer +content-type-handshake+ handshake-data))

(defun record-layer-write-application-data (layer data)
  "Write application data."
  (record-layer-write layer +content-type-application-data+ data))

;;;; Record Fragmentation

(defun fragment-data (data max-size)
  "Split DATA into fragments of at most MAX-SIZE bytes.
   Returns a list of octet vectors."
  (if (<= (length data) max-size)
      (list data)
      (loop for start from 0 below (length data) by max-size
            collect (subseq data start (min (+ start max-size) (length data))))))

(defun record-layer-write-fragmented (layer content-type data)
  "Write DATA as potentially multiple records, fragmenting if necessary."
  (let ((max-size (if (record-layer-write-cipher layer)
                      (- +max-record-size+ +aead-tag-length+ 1)  ; room for tag and content type
                      +max-record-size+)))
    (dolist (fragment (fragment-data data max-size))
      (record-layer-write layer content-type fragment))))

;;;; Alert Processing

(defun process-alert (content)
  "Process an alert record and signal appropriate condition."
  (when (< (length content) 2)
    (error 'tls-decode-error :message "Alert too short"))
  (let ((level (aref content 0))
        (description (aref content 1)))
    ;; close_notify is a clean shutdown
    (when (= description +alert-close-notify+)
      (error 'tls-connection-closed :clean t))
    ;; All other alerts signal an error
    (error 'tls-alert-error :level level :description description)))
