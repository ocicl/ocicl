;;; asn1.lisp --- ASN.1/DER Parser for X.509 Certificates
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements ASN.1 DER (Distinguished Encoding Rules) parsing
;;; for X.509 certificate processing.

(in-package #:pure-tls)

;;;; ASN.1 Tag Classes and Types

(defconstant +asn1-class-universal+ 0)
(defconstant +asn1-class-application+ 1)
(defconstant +asn1-class-context-specific+ 2)
(defconstant +asn1-class-private+ 3)

;; Universal tags
(defconstant +asn1-boolean+ 1)
(defconstant +asn1-integer+ 2)
(defconstant +asn1-bit-string+ 3)
(defconstant +asn1-octet-string+ 4)
(defconstant +asn1-null+ 5)
(defconstant +asn1-object-identifier+ 6)
(defconstant +asn1-utf8-string+ 12)
(defconstant +asn1-sequence+ 16)
(defconstant +asn1-set+ 17)
(defconstant +asn1-printable-string+ 19)
(defconstant +asn1-ia5-string+ 22)
(defconstant +asn1-utc-time+ 23)
(defconstant +asn1-generalized-time+ 24)

;;;; ASN.1 Node Structure

(defstruct asn1-node
  "An ASN.1 parsed node."
  (class +asn1-class-universal+ :type fixnum)
  (constructed nil :type boolean)
  (tag 0 :type fixnum)
  (value nil)
  ;; For raw access
  (raw-bytes nil :type (or null octet-vector)))

;;;; DER Parsing

(defun parse-der (data)
  "Parse DER-encoded data into an ASN1-NODE tree.
   DATA should be an octet vector."
  (let ((buf (make-tls-buffer data)))
    (parse-der-node buf)))

(defun parse-der-node (buf)
  "Parse a single DER node from the buffer."
  (when (zerop (buffer-remaining buf))
    (return-from parse-der-node nil))
  (let* ((start-pos (tls-buffer-position buf))
         ;; Parse identifier octet
         (id-byte (buffer-read-octet buf))
         (class (ldb (byte 2 6) id-byte))
         (constructed (logbitp 5 id-byte))
         (tag (ldb (byte 5 0) id-byte)))
    ;; Handle long-form tags
    (when (= tag 31)
      (setf tag 0)
      (loop for b = (buffer-read-octet buf)
            do (setf tag (logior (ash tag 7) (logand b #x7f)))
            while (logbitp 7 b)))
    ;; Parse length
    (let ((length (parse-der-length buf)))
      (when (null length)
        (error 'tls-decode-error :message "Indefinite length not supported in DER"))
      ;; Parse value
      (let* ((value-start (tls-buffer-position buf))
             (value (if constructed
                        ;; Parse contained elements
                        (parse-der-contents buf length)
                        ;; Primitive: read raw bytes
                        (buffer-read-octets buf length)))
             (end-pos (tls-buffer-position buf))
             (raw (subseq (tls-buffer-data buf) start-pos end-pos)))
        (make-asn1-node :class class
                        :constructed constructed
                        :tag tag
                        :value (if constructed
                                   value
                                   (decode-primitive-value class tag value))
                        :raw-bytes raw)))))

(defun parse-der-length (buf)
  "Parse DER length field. Returns nil for indefinite length."
  (let ((first-byte (buffer-read-octet buf)))
    (cond
      ;; Short form: length < 128
      ((not (logbitp 7 first-byte))
       first-byte)
      ;; Indefinite length (not valid in DER)
      ((zerop (logand first-byte #x7f))
       nil)
      ;; Long form
      (t
       (let ((num-octets (logand first-byte #x7f))
             (length 0))
         (dotimes (i num-octets)
           (setf length (logior (ash length 8) (buffer-read-octet buf))))
         length)))))

(defun parse-der-contents (buf length)
  "Parse the contents of a constructed type."
  (let ((end-pos (+ (tls-buffer-position buf) length))
        (nodes nil))
    (loop while (< (tls-buffer-position buf) end-pos)
          do (push (parse-der-node buf) nodes))
    (nreverse nodes)))

(defun decode-primitive-value (class tag raw-bytes)
  "Decode a primitive ASN.1 value."
  (if (= class +asn1-class-universal+)
      (case tag
        (#.+asn1-boolean+
         (not (zerop (aref raw-bytes 0))))
        (#.+asn1-integer+
         (decode-der-integer raw-bytes))
        (#.+asn1-bit-string+
         ;; First byte is unused bits count
         (list :unused-bits (aref raw-bytes 0)
               :data (subseq raw-bytes 1)))
        (#.+asn1-octet-string+
         raw-bytes)
        (#.+asn1-null+
         nil)
        (#.+asn1-object-identifier+
         (decode-der-oid raw-bytes))
        ((#.+asn1-utf8-string+ #.+asn1-printable-string+ #.+asn1-ia5-string+)
         (octets-to-string raw-bytes))
        (#.+asn1-utc-time+
         (decode-utc-time (octets-to-string raw-bytes)))
        (#.+asn1-generalized-time+
         (decode-generalized-time (octets-to-string raw-bytes)))
        (t raw-bytes))
      ;; For non-universal, return raw bytes
      raw-bytes))

;;;; Integer Decoding

(defun decode-der-integer (bytes)
  "Decode a DER-encoded integer (two's complement, big-endian)."
  (let ((value 0)
        (negative (logbitp 7 (aref bytes 0))))
    (loop for byte across bytes
          do (setf value (logior (ash value 8) byte)))
    (if negative
        (- value (ash 1 (* 8 (length bytes))))
        value)))

;;;; OID Decoding

(defun decode-der-oid (bytes)
  "Decode a DER-encoded Object Identifier."
  (when (zerop (length bytes))
    (return-from decode-der-oid nil))
  (let ((components nil)
        (first-byte (aref bytes 0)))
    ;; First byte encodes first two components
    (push (floor first-byte 40) components)
    (push (mod first-byte 40) components)
    ;; Remaining bytes encode subsequent components
    (let ((value 0))
      (loop for i from 1 below (length bytes)
            for byte = (aref bytes i)
            do (setf value (logior (ash value 7) (logand byte #x7f)))
            when (not (logbitp 7 byte))
              do (push value components)
                 (setf value 0)))
    (nreverse components)))

(defun oid-to-string (oid)
  "Convert an OID list to dotted string notation."
  (format nil "~{~D~^.~}" oid))

(defun string-to-oid (string)
  "Convert a dotted string to an OID list."
  (mapcar #'parse-integer (split-string string #\.)))

(defun split-string (string delimiter)
  "Split STRING by DELIMITER character."
  (loop for start = 0 then (1+ pos)
        for pos = (position delimiter string :start start)
        collect (subseq string start (or pos (length string)))
        while pos))

;;;; Time Decoding

(defun decode-utc-time (string)
  "Decode UTCTime string to universal-time."
  ;; Format: YYMMDDHHMMSSZ
  (let* ((year (parse-integer string :start 0 :end 2))
         (month (parse-integer string :start 2 :end 4))
         (day (parse-integer string :start 4 :end 6))
         (hour (parse-integer string :start 6 :end 8))
         (minute (parse-integer string :start 8 :end 10))
         (second (if (>= (length string) 12)
                     (parse-integer string :start 10 :end 12)
                     0)))
    ;; UTCTime uses 2-digit year: 00-49 = 2000-2049, 50-99 = 1950-1999
    (if (< year 50)
        (incf year 2000)
        (incf year 1900))
    (encode-universal-time second minute hour day month year 0)))

(defun decode-generalized-time (string)
  "Decode GeneralizedTime string to universal-time."
  ;; Format: YYYYMMDDHHMMSSZ or YYYYMMDDHHMMSS.fffZ
  (let ((year (parse-integer string :start 0 :end 4))
        (month (parse-integer string :start 4 :end 6))
        (day (parse-integer string :start 6 :end 8))
        (hour (parse-integer string :start 8 :end 10))
        (minute (parse-integer string :start 10 :end 12))
        (second (if (>= (length string) 14)
                    (parse-integer string :start 12 :end 14)
                    0)))
    (encode-universal-time second minute hour day month year 0)))

;;;; ASN.1 Navigation Utilities

(defun asn1-sequence-p (node)
  "Check if node is a SEQUENCE."
  (and (= (asn1-node-class node) +asn1-class-universal+)
       (= (asn1-node-tag node) +asn1-sequence+)))

(defun asn1-set-p (node)
  "Check if node is a SET."
  (and (= (asn1-node-class node) +asn1-class-universal+)
       (= (asn1-node-tag node) +asn1-set+)))

(defun asn1-context-p (node tag)
  "Check if node is context-specific with given tag."
  (and (= (asn1-node-class node) +asn1-class-context-specific+)
       (= (asn1-node-tag node) tag)))

(defun asn1-get-child (node index)
  "Get child at index from a constructed node."
  (when (asn1-node-constructed node)
    (nth index (asn1-node-value node))))

(defun asn1-children (node)
  "Get all children of a constructed node."
  (when (asn1-node-constructed node)
    (asn1-node-value node)))

(defun asn1-find-child (node class tag)
  "Find first child with given class and tag."
  (find-if (lambda (child)
             (and (= (asn1-node-class child) class)
                  (= (asn1-node-tag child) tag)))
           (asn1-children node)))

;;;; Well-Known OIDs

(defparameter *well-known-oids*
  '(;; X.500 AttributeTypes
    ((2 5 4 3) . :common-name)
    ((2 5 4 6) . :country-name)
    ((2 5 4 7) . :locality-name)
    ((2 5 4 8) . :state-or-province-name)
    ((2 5 4 10) . :organization-name)
    ((2 5 4 11) . :organizational-unit-name)
    ;; X.509 Extensions
    ((2 5 29 14) . :subject-key-identifier)
    ((2 5 29 15) . :key-usage)
    ((2 5 29 17) . :subject-alt-name)
    ((2 5 29 19) . :basic-constraints)
    ((2 5 29 35) . :authority-key-identifier)
    ((2 5 29 37) . :extended-key-usage)
    ;; Signature Algorithms
    ((1 2 840 113549 1 1 1) . :rsa-encryption)
    ((1 2 840 113549 1 1 5) . :sha1-with-rsa-encryption)
    ((1 2 840 113549 1 1 11) . :sha256-with-rsa-encryption)
    ((1 2 840 113549 1 1 12) . :sha384-with-rsa-encryption)
    ((1 2 840 113549 1 1 13) . :sha512-with-rsa-encryption)
    ((1 2 840 10045 4 3 2) . :ecdsa-with-sha256)
    ((1 2 840 10045 4 3 3) . :ecdsa-with-sha384)
    ((1 2 840 10045 4 3 4) . :ecdsa-with-sha512)
    ;; EC Public Key
    ((1 2 840 10045 2 1) . :ec-public-key)
    ;; EC Curves
    ((1 2 840 10045 3 1 7) . :prime256v1)
    ((1 3 132 0 34) . :secp384r1)
    ((1 3 132 0 35) . :secp521r1))
  "Mapping of well-known OIDs to symbolic names.")

(defun oid-name (oid)
  "Get the symbolic name for an OID, or the OID itself if unknown."
  (or (cdr (assoc oid *well-known-oids* :test #'equal))
      oid))
