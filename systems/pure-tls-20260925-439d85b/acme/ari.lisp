;;; ari.lisp --- ACME Renewal Information (RFC 9773)
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; ARI lets the ACME server suggest a renewal window for each certificate,
;;; so clients renew at the right time for any certificate lifetime and the
;;; CA can pull renewals forward during incidents. The server publishes a
;;; "renewalInfo" URL in its directory; GET <renewalInfo>/<certID> returns a
;;; suggested window. ARI is advisory: every failure here degrades to "no
;;; window available" rather than an error, and callers fall back to a
;;; lifetime-based renewal heuristic.

(in-package #:pure-tls/acme)

;;; ----------------------------------------------------------------------------
;;; Certificate identifier (RFC 9773 section 4.1)
;;; ----------------------------------------------------------------------------

(defun der-integer-content-octets (n)
  "Return the content octets of the DER INTEGER encoding of the non-negative
   integer N: minimal big-endian two's complement, with a leading zero octet
   when the high bit of the first octet is set."
  (when (minusp n)
    (error 'acme-error :message "Cannot encode a negative serial number"))
  (if (zerop n)
      (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)
      (let ((octets (ironclad:integer-to-octets n)))
        (if (logbitp 7 (aref octets 0))
            (concatenate '(simple-array (unsigned-byte 8) (*)) #(0) octets)
            octets))))

(defun ari-cert-id (aki-key-identifier serial-number)
  "Build an RFC 9773 certificate identifier from the Authority Key Identifier
   keyIdentifier octets and the serial number integer:
   base64url(keyIdentifier) \".\" base64url(DER serial content octets)."
  (format nil "~A.~A"
          (base64url-encode
           (coerce aki-key-identifier '(simple-array (unsigned-byte 8) (*))))
          (base64url-encode (der-integer-content-octets serial-number))))

(defun certificate-aki-key-identifier (cert)
  "Return the keyIdentifier octets from CERT's Authority Key Identifier
   extension. Signals ACME-ERROR when the extension or its keyIdentifier
   field is absent (as in some self-signed certificates)."
  (let ((ext (find :authority-key-identifier
                   (pure-tls::x509-certificate-extensions cert)
                   :key #'pure-tls::x509-extension-oid)))
    (unless ext
      (error 'acme-error
             :message "Certificate has no Authority Key Identifier extension"))
    (let* ((node (pure-tls::parse-der (pure-tls::x509-extension-value ext)))
           (keyid (find 0 (pure-tls::asn1-children node)
                        :key #'pure-tls::asn1-node-tag)))
      (unless keyid
        (error 'acme-error
               :message "Authority Key Identifier extension has no keyIdentifier"))
      (pure-tls::asn1-node-value keyid))))

(defun certificate-ari-cert-id (cert)
  "Return CERT's RFC 9773 certificate identifier, used to query renewal
   information and to link a renewal order to the certificate it replaces."
  (ari-cert-id (certificate-aki-key-identifier cert)
               (pure-tls::x509-certificate-serial-number cert)))

;;; ----------------------------------------------------------------------------
;;; RFC 3339 timestamps
;;; ----------------------------------------------------------------------------

(defun parse-rfc3339-time (string)
  "Parse an RFC 3339 timestamp (e.g. \"2026-05-13T04:00:00Z\") into a
   universal time. Fractional seconds are truncated. Signals ACME-ERROR on
   malformed input."
  (flet ((bad ()
           (error 'acme-error
                  :message (format nil "Malformed RFC 3339 timestamp: ~S" string)))
         (digits (start end)
           (parse-integer string :start start :end end)))
    (handler-case
        (progn
          (unless (and (>= (length string) 20)
                       (char= (char string 4) #\-)
                       (char= (char string 7) #\-)
                       (char-equal (char string 10) #\T)
                       (char= (char string 13) #\:)
                       (char= (char string 16) #\:))
            (bad))
          (let ((year (digits 0 4)) (month (digits 5 7)) (day (digits 8 10))
                (hour (digits 11 13)) (minute (digits 14 16)) (sec (digits 17 19))
                (pos 19))
            (when (char= (char string pos) #\.)
              (let ((frac-start (incf pos)))
                (loop while (and (< pos (length string))
                                 (digit-char-p (char string pos)))
                      do (incf pos))
                (when (= pos frac-start) (bad))))
            (when (>= pos (length string)) (bad))
            (let* ((c (char string pos))
                   (offset-seconds
                     (cond
                       ((char-equal c #\Z)
                        (unless (= (1+ pos) (length string)) (bad))
                        0)
                       ((or (char= c #\+) (char= c #\-))
                        (unless (and (= (+ pos 6) (length string))
                                     (char= (char string (+ pos 3)) #\:))
                          (bad))
                        (* (if (char= c #\-) -1 1)
                           (+ (* 3600 (digits (+ pos 1) (+ pos 3)))
                              (* 60 (digits (+ pos 4) (+ pos 6))))))
                       (t (bad)))))
              (- (encode-universal-time sec minute hour day month year 0)
                 offset-seconds))))
      (acme-error (e) (error e))
      (error () (bad)))))

;;; ----------------------------------------------------------------------------
;;; Renewal information query
;;; ----------------------------------------------------------------------------

(defun client-renewal-info (client cert-id)
  "Query the ACME server's renewal information for CERT-ID (a string from
   CERTIFICATE-ARI-CERT-ID). Returns (VALUES window-start window-end) as
   universal times when the server advertises a renewalInfo endpoint and
   returns a valid suggested window; otherwise NIL. ARI is advisory, so all
   failures (missing endpoint, network errors, malformed responses) return
   NIL rather than signalling."
  (let ((base (rest (assoc :renewal-info (acme-client-directory client)))))
    (when (stringp base)
      (handler-case
          (let ((url (format nil "~A~:[/~;~]~A" base
                             (char= (char base (1- (length base))) #\/)
                             cert-id)))
            (multiple-value-bind (response status) (client-get client url)
              (when (eql status 200)
                (let* ((window (rest (assoc :suggested-window response)))
                       (start (rest (assoc :start window)))
                       (end (rest (assoc :end window))))
                  (when (and (stringp start) (stringp end))
                    (values (parse-rfc3339-time start)
                            (parse-rfc3339-time end)))))))
        (error () nil)))))

;;; ----------------------------------------------------------------------------
;;; Renewal decision
;;; ----------------------------------------------------------------------------

(defun renewal-due-p (not-before not-after now &key window-start)
  "True when a certificate valid from NOT-BEFORE to NOT-AFTER should be
   renewed at NOW: at or past the ARI suggested window's start when one is
   given, otherwise once a third of the certificate's lifetime remains.
   The fraction adapts to any lifetime -- 30 days on a classic 90-day
   certificate, 2 days on a ~6-day short-lived one."
  (if window-start
      (>= now window-start)
      (<= (- not-after now) (/ (- not-after not-before) 3))))
