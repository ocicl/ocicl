;;; extensions.lisp --- TLS 1.3 Extensions
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements TLS 1.3 extension parsing and serialization.

(in-package #:pure-tls)

;;;; Extension Structure
;;;
;;; struct {
;;;   ExtensionType extension_type;
;;;   opaque extension_data<0..2^16-1>;
;;; } Extension;

(defstruct tls-extension
  "A TLS extension."
  (type 0 :type fixnum)
  (data nil))

;;;; Extension Parsing

(defun tls12-only-extension-p (ext-type)
  "Return T if ext-type is a TLS 1.2-only extension forbidden in TLS 1.3.
   Per RFC 8446, these extensions MUST NOT be sent in TLS 1.3 handshakes."
  (or (= ext-type +extension-extended-master-secret+)
      (= ext-type +extension-renegotiation-info+)
      (= ext-type +extension-next-protocol-negotiation+)))

(defun known-extension-p (ext-type)
  "Return T if ext-type is a known TLS extension type.
   Known extensions are those defined in the TLS specifications."
  (member ext-type (list +extension-server-name+               ; 0
                         +extension-max-fragment-length+       ; 1
                         +extension-status-request+            ; 5
                         +extension-supported-groups+          ; 10
                         11  ; ec_point_formats
                         +extension-signature-algorithms+      ; 13
                         +extension-use-srtp+                  ; 14
                         +extension-heartbeat+                 ; 15
                         +extension-application-layer-protocol-negotiation+ ; 16
                         +extension-signed-certificate-timestamp+  ; 18
                         +extension-client-certificate-type+   ; 19
                         +extension-server-certificate-type+   ; 20
                         +extension-padding+                   ; 21
                         +extension-extended-master-secret+    ; 23
                         +extension-pre-shared-key+            ; 41
                         +extension-early-data+                ; 42
                         +extension-supported-versions+        ; 43
                         +extension-cookie+                    ; 44
                         +extension-psk-key-exchange-modes+    ; 45
                         +extension-certificate-authorities+   ; 47
                         +extension-oid-filters+               ; 48
                         +extension-post-handshake-auth+       ; 49
                         +extension-signature-algorithms-cert+ ; 50
                         +extension-key-share+                 ; 51
                         +extension-ech+                       ; 65037 (0xfe0d)
                         +extension-renegotiation-info+        ; 65281
                         +extension-next-protocol-negotiation+)))

(defun parse-extensions (data &key validate-tls13 (context :client-hello))
  "Parse a list of extensions from bytes.
   If VALIDATE-TLS13 is true, reject TLS 1.2-only extensions with an error.
   CONTEXT is :client-hello (default) or :encrypted-extensions."
  (let ((buf (make-tls-buffer data))
        (extensions nil)
        (seen-types nil))
    (loop while (plusp (buffer-remaining buf))
          do (let* ((ext-type (buffer-read-uint16 buf))
                    (ext-data (buffer-read-vector16 buf)))
               ;; RFC 8446 Section 4.2: There MUST NOT be more than one extension
               ;; of the same type in a given extension block
               (when (member ext-type seen-types)
                 (error 'tls-handshake-error
                        :message (format nil ":DUPLICATE_EXTENSION: Duplicate extension type ~D" ext-type)))
               (push ext-type seen-types)
               ;; Check for TLS 1.2-only extensions when validating
               (when (and validate-tls13 (tls12-only-extension-p ext-type))
                 (error 'tls-handshake-error
                        :message (format nil ":ERROR_PARSING_EXTENSION: TLS 1.2-only extension ~D received in TLS 1.3"
                                        ext-type)))
               (push (parse-extension ext-type ext-data :context context) extensions)))
    (nreverse extensions)))

(defun parse-extension (ext-type data &key (context :client-hello))
  "Parse a single extension.
   CONTEXT is :client-hello (default) or :encrypted-extensions."
  (make-tls-extension
   :type ext-type
   :data (case ext-type
           (#.+extension-supported-versions+
            (parse-supported-versions-extension data))
           (#.+extension-key-share+
            (parse-key-share-extension data))
           (#.+extension-supported-groups+
            (parse-supported-groups-extension data))
           (#.+extension-signature-algorithms+
            (parse-signature-algorithms-extension data))
           (#.+extension-server-name+
            (parse-server-name-extension data))
           (#.+extension-application-layer-protocol-negotiation+
            (parse-alpn-extension data))
           (#.+extension-psk-key-exchange-modes+
            (parse-psk-key-exchange-modes-extension data))
           (#.+extension-pre-shared-key+
            ;; Parse as ClientHello format (identities + binders)
            ;; ServerHello format (just selected index) is only 2 bytes
            (parse-pre-shared-key-extension data :server-hello-p (= (length data) 2)))
           (#.+extension-ech+
            (parse-ech-extension data :context context))
           (otherwise data))))  ; Return raw bytes for unknown extensions

(defun serialize-extension (ext)
  "Serialize an extension to bytes."
  (let ((ext-type (tls-extension-type ext))
        (ext-data (tls-extension-data ext)))
    ;; Serialize the data based on type if it's structured
    (let ((serialized-data
            (case ext-type
              (#.+extension-supported-versions+
               (serialize-supported-versions-extension ext-data))
              (#.+extension-key-share+
               (serialize-key-share-extension ext-data))
              (#.+extension-supported-groups+
               (serialize-supported-groups-extension ext-data))
              (#.+extension-signature-algorithms+
               (serialize-signature-algorithms-extension ext-data))
              (#.+extension-server-name+
               (serialize-server-name-extension ext-data))
              (#.+extension-application-layer-protocol-negotiation+
               (serialize-alpn-extension ext-data))
              (#.+extension-psk-key-exchange-modes+
               (serialize-psk-key-exchange-modes-extension ext-data))
              (#.+extension-pre-shared-key+
               (serialize-pre-shared-key-extension ext-data))
              (#.+extension-ech+
               (serialize-ech-extension ext-data))
              (otherwise
               (cond
                 ((typep ext-data 'grease-ext)
                  (serialize-grease-extension ext-data))
                 ((typep ext-data 'octet-vector)
                  ext-data)
                 (t (make-octet-vector 0)))))))
      (concat-octet-vectors
       (encode-uint16 ext-type)
       (encode-uint16 (length serialized-data))
       serialized-data))))

;;;; Supported Versions Extension

(defstruct supported-versions-ext
  "supported_versions extension data."
  (versions nil :type list)
  ;; For ServerHello, only one version
  (selected-version nil))

(defun parse-supported-versions-extension (data)
  "Parse supported_versions extension."
  (let ((buf (make-tls-buffer data)))
    (if (= (length data) 2)
        ;; ServerHello format: single selected version
        (make-supported-versions-ext
         :selected-version (buffer-read-uint16 buf))
        ;; ClientHello format: list of versions
        (let ((versions-data (buffer-read-vector8 buf))
              (versions nil))
          (loop for i from 0 below (length versions-data) by 2
                do (push (decode-uint16 versions-data i) versions))
          (make-supported-versions-ext :versions (nreverse versions))))))

(defun serialize-supported-versions-extension (ext)
  "Serialize supported_versions extension for ClientHello."
  (let ((buf (make-tls-write-buffer)))
    (if (supported-versions-ext-selected-version ext)
        ;; ServerHello format
        (write-buffer-append-uint16 buf (supported-versions-ext-selected-version ext))
        ;; ClientHello format
        (let ((versions-buf (make-tls-write-buffer)))
          (dolist (v (supported-versions-ext-versions ext))
            (write-buffer-append-uint16 versions-buf v))
          (write-buffer-append-vector8 buf (write-buffer-contents versions-buf))))
    (write-buffer-contents buf)))

;;;; Key Share Extension

(defstruct key-share-ext
  "key_share extension data."
  ;; For ClientHello: list of key-share-entry
  (client-shares nil :type list)
  ;; For ServerHello: single key-share-entry
  (server-share nil)
  ;; For HelloRetryRequest: selected group
  (selected-group nil))

(defstruct key-share-entry
  "A key share entry."
  (group 0 :type fixnum)
  (key-exchange nil :type (or null octet-vector)))

(defun parse-key-share-extension (data)
  "Parse key_share extension."
  (let ((buf (make-tls-buffer data)))
    (cond
      ;; HelloRetryRequest: just a group
      ((= (length data) 2)
       (make-key-share-ext :selected-group (buffer-read-uint16 buf)))
      ;; ServerHello: single entry (group + key)
      ((< (length data) 6)
       (error 'tls-decode-error :message ":DECODE_ERROR: Invalid key_share extension"))
      ;; Try to parse as ServerHello first (no length prefix)
      ((let ((potential-group (decode-uint16 data 0))
             (potential-len (decode-uint16 data 2)))
         (= (length data) (+ 4 potential-len)))
       ;; ServerHello format
       (let* ((group (buffer-read-uint16 buf))
              (key-exchange (buffer-read-vector16 buf)))
         (make-key-share-ext
          :server-share (make-key-share-entry :group group
                                              :key-exchange key-exchange))))
      ;; ClientHello: list of entries
      (t
       (let ((shares-data (buffer-read-vector16 buf))
             (shares nil)
             (seen-groups nil))
         (let ((shares-buf (make-tls-buffer shares-data)))
           (loop while (plusp (buffer-remaining shares-buf))
                 do (let* ((group (buffer-read-uint16 shares-buf))
                           (key-exchange (buffer-read-vector16 shares-buf)))
                      ;; RFC 8446 Section 4.2.8: Clients MUST NOT offer multiple
                      ;; KeyShareEntry values for the same group
                      (when (member group seen-groups)
                        (error 'tls-handshake-error
                               :message ":DUPLICATE_KEY_SHARE: Multiple key shares for same group"))
                      (push group seen-groups)
                      (push (make-key-share-entry :group group
                                                  :key-exchange key-exchange)
                            shares))))
         (make-key-share-ext :client-shares (nreverse shares)))))))

(defun serialize-key-share-extension (ext)
  "Serialize key_share extension."
  (let ((buf (make-tls-write-buffer)))
    (cond
      ;; HelloRetryRequest
      ((key-share-ext-selected-group ext)
       (write-buffer-append-uint16 buf (key-share-ext-selected-group ext)))
      ;; ServerHello
      ((key-share-ext-server-share ext)
       (let ((entry (key-share-ext-server-share ext)))
         (write-buffer-append-uint16 buf (key-share-entry-group entry))
         (write-buffer-append-vector16 buf (key-share-entry-key-exchange entry))))
      ;; ClientHello
      (t
       (let ((entries-buf (make-tls-write-buffer)))
         (dolist (entry (key-share-ext-client-shares ext))
           (write-buffer-append-uint16 entries-buf (key-share-entry-group entry))
           (write-buffer-append-vector16 entries-buf (key-share-entry-key-exchange entry)))
         (write-buffer-append-vector16 buf (write-buffer-contents entries-buf)))))
    (write-buffer-contents buf)))

;;;; Supported Groups Extension

(defstruct supported-groups-ext
  "supported_groups extension data."
  (groups nil :type list))

(defun parse-supported-groups-extension (data)
  "Parse supported_groups extension."
  (let ((buf (make-tls-buffer data))
        (groups nil))
    (let ((groups-data (buffer-read-vector16 buf)))
      (loop for i from 0 below (length groups-data) by 2
            do (push (decode-uint16 groups-data i) groups)))
    (make-supported-groups-ext :groups (nreverse groups))))

(defun serialize-supported-groups-extension (ext)
  "Serialize supported_groups extension."
  (let ((buf (make-tls-write-buffer)))
    (let ((groups-buf (make-tls-write-buffer)))
      (dolist (g (supported-groups-ext-groups ext))
        (write-buffer-append-uint16 groups-buf g))
      (write-buffer-append-vector16 buf (write-buffer-contents groups-buf)))
    (write-buffer-contents buf)))

;;;; Signature Algorithms Extension

(defstruct signature-algorithms-ext
  "signature_algorithms extension data."
  (algorithms nil :type list))

(defun parse-signature-algorithms-extension (data)
  "Parse signature_algorithms extension."
  (let ((buf (make-tls-buffer data))
        (algs nil))
    (let ((algs-data (buffer-read-vector16 buf)))
      (loop for i from 0 below (length algs-data) by 2
            do (push (decode-uint16 algs-data i) algs)))
    (make-signature-algorithms-ext :algorithms (nreverse algs))))

(defun serialize-signature-algorithms-extension (ext)
  "Serialize signature_algorithms extension."
  (let ((buf (make-tls-write-buffer)))
    (let ((algs-buf (make-tls-write-buffer)))
      (dolist (a (signature-algorithms-ext-algorithms ext))
        (write-buffer-append-uint16 algs-buf a))
      (write-buffer-append-vector16 buf (write-buffer-contents algs-buf)))
    (write-buffer-contents buf)))

;;;; Server Name Extension (SNI)

(defstruct server-name-ext
  "server_name extension data."
  (host-name nil :type (or null string)))

(defconstant +server-name-type-hostname+ 0)

(defun parse-server-name-extension (data)
  "Parse server_name extension.
   In server responses, this may be empty (just acknowledging SNI)."
  ;; Empty extension is valid - server acknowledges SNI
  (when (zerop (length data))
    (return-from parse-server-name-extension
      (make-server-name-ext :host-name nil)))
  (let ((buf (make-tls-buffer data)))
    (let ((list-data (buffer-read-vector16 buf)))
      (when (plusp (length list-data))
        (let ((list-buf (make-tls-buffer list-data)))
          (let ((name-type (buffer-read-octet list-buf))
                (name-data (buffer-read-vector16 list-buf)))
            (when (= name-type +server-name-type-hostname+)
              (make-server-name-ext
               :host-name (octets-to-string name-data)))))))))

(defun serialize-server-name-extension (ext)
  "Serialize server_name extension."
  (let ((buf (make-tls-write-buffer))
        (hostname (server-name-ext-host-name ext)))
    (when hostname
      (let ((name-bytes (string-to-octets hostname))
            (entry-buf (make-tls-write-buffer)))
        (write-buffer-append-octet entry-buf +server-name-type-hostname+)
        (write-buffer-append-vector16 entry-buf name-bytes)
        (write-buffer-append-vector16 buf (write-buffer-contents entry-buf))))
    (write-buffer-contents buf)))

;;;; ALPN Extension

(defstruct alpn-ext
  "application_layer_protocol_negotiation extension data."
  (protocol-list nil :type list)
  ;; For server response
  (selected-protocol nil :type (or null string)))

(defun parse-alpn-extension (data)
  "Parse ALPN extension."
  (let ((buf (make-tls-buffer data))
        (protocols nil))
    (let ((list-data (buffer-read-vector16 buf)))
      (let ((list-buf (make-tls-buffer list-data)))
        (loop while (plusp (buffer-remaining list-buf))
              do (let ((proto-data (buffer-read-vector8 list-buf)))
                   ;; RFC 7301: Empty protocol names are not permitted
                   (when (zerop (length proto-data))
                     (error 'tls-handshake-error
                            :message ":DECODE_ERROR: Empty ALPN protocol name"))
                   (push (octets-to-string proto-data) protocols)))))
    (make-alpn-ext :protocol-list (nreverse protocols))))

(defun serialize-alpn-extension (ext)
  "Serialize ALPN extension."
  (let ((buf (make-tls-write-buffer)))
    (let ((list-buf (make-tls-write-buffer)))
      (dolist (proto (or (and (alpn-ext-selected-protocol ext)
                              (list (alpn-ext-selected-protocol ext)))
                         (alpn-ext-protocol-list ext)))
        (write-buffer-append-vector8 list-buf (string-to-octets proto)))
      (write-buffer-append-vector16 buf (write-buffer-contents list-buf)))
    (write-buffer-contents buf)))

;;;; PSK Key Exchange Modes Extension

(defconstant +psk-ke+ 0
  "PSK-only key establishment.")
(defconstant +psk-dhe-ke+ 1
  "PSK with (EC)DHE key establishment.")

(defstruct psk-key-exchange-modes-ext
  "psk_key_exchange_modes extension data."
  (modes nil :type list))

(defun parse-psk-key-exchange-modes-extension (data)
  "Parse psk_key_exchange_modes extension."
  (let ((buf (make-tls-buffer data))
        (modes nil))
    (let ((modes-data (buffer-read-vector8 buf)))
      (loop for i from 0 below (length modes-data)
            do (push (aref modes-data i) modes)))
    (make-psk-key-exchange-modes-ext :modes (nreverse modes))))

(defun serialize-psk-key-exchange-modes-extension (ext)
  "Serialize psk_key_exchange_modes extension."
  (let ((buf (make-tls-write-buffer))
        (modes (psk-key-exchange-modes-ext-modes ext)))
    (write-buffer-append-vector8 buf (coerce modes 'octet-vector))
    (write-buffer-contents buf)))

;;;; Pre-Shared Key Extension

(defstruct psk-identity
  "A PSK identity offered by the client."
  (identity nil :type (or null octet-vector))
  (obfuscated-ticket-age 0 :type (unsigned-byte 32)))

(defstruct pre-shared-key-ext
  "pre_shared_key extension data."
  ;; ClientHello: list of identities and binders
  (identities nil :type list)
  (binders nil :type list)
  ;; ServerHello: selected identity index
  (selected-identity nil))

(defun parse-pre-shared-key-extension (data &key server-hello-p)
  "Parse pre_shared_key extension.
   SERVER-HELLO-P indicates if this is from ServerHello (just an index)."
  (let ((buf (make-tls-buffer data)))
    (if server-hello-p
        ;; ServerHello: uint16 selected_identity
        (make-pre-shared-key-ext
         :selected-identity (buffer-read-uint16 buf))
        ;; ClientHello: identities and binders
        (let ((identities nil)
              (binders nil))
          ;; Parse identities
          (let ((id-data (buffer-read-vector16 buf)))
            (let ((id-buf (make-tls-buffer id-data)))
              (loop while (plusp (buffer-remaining id-buf))
                    do (let ((identity (buffer-read-vector16 id-buf))
                             (age (buffer-read-uint32 id-buf)))
                         (push (make-psk-identity :identity identity
                                                  :obfuscated-ticket-age age)
                               identities)))))
          ;; Parse binders
          (let ((binder-data (buffer-read-vector16 buf)))
            (let ((binder-buf (make-tls-buffer binder-data)))
              (loop while (plusp (buffer-remaining binder-buf))
                    do (push (buffer-read-vector8 binder-buf) binders))))
          (make-pre-shared-key-ext
           :identities (nreverse identities)
           :binders (nreverse binders))))))

(defun serialize-pre-shared-key-extension (ext)
  "Serialize pre_shared_key extension."
  (let ((buf (make-tls-write-buffer)))
    (cond
      ;; ServerHello: just the selected identity
      ((pre-shared-key-ext-selected-identity ext)
       (write-buffer-append-uint16 buf (pre-shared-key-ext-selected-identity ext)))
      ;; ClientHello: identities and binders
      (t
       ;; Identities
       (let ((id-buf (make-tls-write-buffer)))
         (dolist (id (pre-shared-key-ext-identities ext))
           (write-buffer-append-vector16 id-buf (psk-identity-identity id))
           (write-buffer-append-uint32 id-buf (psk-identity-obfuscated-ticket-age id)))
         (write-buffer-append-vector16 buf (write-buffer-contents id-buf)))
       ;; Binders
       (let ((binder-buf (make-tls-write-buffer)))
         (dolist (binder (pre-shared-key-ext-binders ext))
           (write-buffer-append-vector8 binder-buf binder))
         (write-buffer-append-vector16 buf (write-buffer-contents binder-buf)))))
    (write-buffer-contents buf)))

(defun pre-shared-key-ext-binders-length (ext)
  "Calculate the length of the binders portion of a pre_shared_key extension.
   This is needed for binder calculation (transcript up to but not including binders)."
  (let ((total 2)) ; 2 bytes for binders length prefix
    (dolist (binder (pre-shared-key-ext-binders ext))
      (incf total (1+ (length binder)))) ; 1 byte length + binder data
    total))

;;;; Extension Lookup Utilities

(defun find-extension (extensions type)
  "Find an extension of the given type in a list of extensions."
  (find type extensions :key #'tls-extension-type))

(defun extension-name (ext-type)
  "Return a human-readable name for an extension type."
  (case ext-type
    (#.+extension-server-name+ "server_name")
    (#.+extension-max-fragment-length+ "max_fragment_length")
    (#.+extension-status-request+ "status_request")
    (#.+extension-supported-groups+ "supported_groups")
    (#.+extension-signature-algorithms+ "signature_algorithms")
    (#.+extension-application-layer-protocol-negotiation+ "alpn")
    (#.+extension-pre-shared-key+ "pre_shared_key")
    (#.+extension-early-data+ "early_data")
    (#.+extension-supported-versions+ "supported_versions")
    (#.+extension-cookie+ "cookie")
    (#.+extension-psk-key-exchange-modes+ "psk_key_exchange_modes")
    (#.+extension-key-share+ "key_share")
    (#.+extension-ech+ "encrypted_client_hello")
    (otherwise (format nil "unknown(~D)" ext-type))))

;;;; GREASE Extension (RFC 8701)

(defstruct grease-ext
  "GREASE extension - contains random data to prevent protocol ossification."
  (data (random-bytes 1) :type octet-vector))

(defun serialize-grease-extension (ext)
  "Serialize GREASE extension data."
  (grease-ext-data ext))

;;;; ECH Extension (RFC 9639)
;;;
;;; The ECH extension has different formats depending on context:
;;; - ClientHelloOuter: type(0=outer) + config_id + cipher_suite + enc + payload
;;; - ClientHelloInner: type(1=inner) with empty contents
;;; - EncryptedExtensions (retry): retry_configs (ECHConfigList)

(defconstant +ech-client-type-outer+ 0
  "ECH extension type for ClientHelloOuter")
(defconstant +ech-client-type-inner+ 1
  "ECH extension type for ClientHelloInner")

(defstruct ech-ext
  "encrypted_client_hello extension data.
   For ClientHelloOuter: type=:outer, has config-id, cipher-suite, enc, payload
   For ClientHelloInner: type=:inner, empty
   For HelloRetryRequest: type=:hrr, has config-id and enc (echoed from CH1)
   For EncryptedExtensions: type=:retry, has retry-configs"
  (type :outer :type (member :outer :inner :hrr :retry))
  ;; For outer and hrr
  (config-id 0 :type (unsigned-byte 8))
  (cipher-suite nil :type (or null ech-hpke-cipher-suite))
  (enc nil :type (or null octet-vector))
  (payload nil :type (or null octet-vector))
  ;; For retry (EncryptedExtensions)
  (retry-configs nil :type (or null list)))

(defun parse-ech-extension (data &key (context :client-hello))
  "Parse ECH extension.
   CONTEXT should be :client-hello for ClientHello extensions,
   :server-hello for ServerHello/HelloRetryRequest,
   or :encrypted-extensions for server's EncryptedExtensions.

   In ClientHello:
   - 1 byte with value 1: ClientHelloInner marker
   - Starts with 0: ClientHelloOuter format

   In ServerHello/HelloRetryRequest:
   - Empty (0 bytes): ECH confirmation not sent
   - 1 byte with value 0: ECH confirmation (draft-ietf-tls-esni)

   In EncryptedExtensions:
   - Empty (0 bytes): Server acceptance confirmation
   - Otherwise: retry_configs (ECHConfigList)"
  (cond
    ;; Empty extension - server acceptance confirmation
    ((zerop (length data))
     (make-ech-ext :type :retry :retry-configs nil))
    ;; In ServerHello/HRR context: 1 byte = 0 means ECH confirmation (legacy draft)
    ((and (eql context :server-hello)
          (= (length data) 1)
          (zerop (aref data 0)))
     (make-ech-ext :type :retry :retry-configs nil))
    ;; In ServerHello/HRR context: 8 bytes = ECH acceptance confirmation
    ;; RFC 9639 Section 7.1.1: Server sends 8-byte confirmation when ECH accepted
    ((and (eql context :server-hello)
          (= (length data) 8))
     ;; Store the confirmation for later validation
     (make-ech-ext :type :hrr :enc data))  ; Use enc slot to store confirmation
    ;; In ServerHello context with config_id + enc: HRR echoing our values
    ;; RFC 9639 Section 7.1: Format: uint8 config_id; opaque enc<0..2^16-1>;
    ;; The length would be 1 + 2 + enc_length, so at least 35 bytes for X25519
    ((and (eql context :server-hello)
          (>= (length data) 35))
     (let ((buf (make-tls-buffer data)))
       (let ((config-id (buffer-read-octet buf))
             (enc (buffer-read-vector16 buf)))
         (make-ech-ext :type :hrr :config-id config-id :enc enc))))
    ;; ServerHello context with unexpected size - wrap as raw data
    ((eql context :server-hello)
     (make-ech-ext :type :hrr :enc data))
    ;; In EncryptedExtensions context, non-empty means retry_configs
    ((eql context :encrypted-extensions)
     (make-ech-ext
      :type :retry
      :retry-configs (parse-ech-config-list data)))
    ;; ClientHelloInner marker
    ((and (= (length data) 1)
          (= (aref data 0) +ech-client-type-inner+))
     (make-ech-ext :type :inner))
    ;; ClientHelloOuter format
    ((= (aref data 0) +ech-client-type-outer+)
     (let ((buf (make-tls-buffer data)))
       (buffer-read-octet buf)  ; Skip type byte
       (let* ((kdf-id (buffer-read-uint16 buf))
              (aead-id (buffer-read-uint16 buf))
              (config-id (buffer-read-octet buf))
              (enc (buffer-read-vector16 buf))
              (payload (buffer-read-vector16 buf)))
         (make-ech-ext
          :type :outer
          :config-id config-id
          :cipher-suite (make-ech-hpke-cipher-suite :kdf-id kdf-id :aead-id aead-id)
          :enc enc
          :payload payload))))
    ;; Fallback - try as retry_configs
    (t
     (make-ech-ext
      :type :retry
      :retry-configs (parse-ech-config-list data)))))

(defun serialize-ech-extension (ext)
  "Serialize ECH extension."
  (case (ech-ext-type ext)
    ;; ClientHelloInner - just the type byte
    (:inner
     (octet-vector +ech-client-type-inner+))
    ;; ClientHelloOuter
    (:outer
     (let* ((cs (ech-ext-cipher-suite ext))
            (buf (make-tls-write-buffer))
            (enc (or (ech-ext-enc ext) (make-octet-vector 0)))
            (payload (or (ech-ext-payload ext) (make-octet-vector 0))))
       (write-buffer-append-octet buf +ech-client-type-outer+)
       (write-buffer-append-uint16 buf (ech-hpke-cipher-suite-kdf-id cs))
       (write-buffer-append-uint16 buf (ech-hpke-cipher-suite-aead-id cs))
       (write-buffer-append-octet buf (ech-ext-config-id ext))
       (write-buffer-append-vector16 buf enc)
       (write-buffer-append-vector16 buf payload)
       (write-buffer-contents buf)))
    ;; retry_configs
    (:retry
     (let ((configs (ech-ext-retry-configs ext)))
       (if configs
           (serialize-ech-config-list configs)
           (make-octet-vector 0))))))
