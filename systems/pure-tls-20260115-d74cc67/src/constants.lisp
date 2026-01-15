;;; constants.lisp --- TLS 1.3 protocol constants
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:pure-tls)

;;;; TLS Version Constants

(defconstant +tls-1.3+ #x0304
  "TLS 1.3 version identifier")

(defconstant +tls-1.2+ #x0303
  "TLS 1.2 version identifier (used in record layer for compatibility)")

(defconstant +tls-1.0+ #x0301
  "TLS 1.0 version identifier (allowed in initial ClientHello per RFC 8446)")

;;;; Content Types (RFC 8446 Section 5.1)

(defconstant +content-type-invalid+ 0)
(defconstant +content-type-change-cipher-spec+ 20)
(defconstant +content-type-alert+ 21)
(defconstant +content-type-handshake+ 22)
(defconstant +content-type-application-data+ 23)

;;;; Handshake Message Types (RFC 8446 Section 4)

(defconstant +handshake-client-hello+ 1)
(defconstant +handshake-server-hello+ 2)
(defconstant +handshake-new-session-ticket+ 4)
(defconstant +handshake-end-of-early-data+ 5)
(defconstant +handshake-encrypted-extensions+ 8)
(defconstant +handshake-certificate+ 11)
(defconstant +handshake-certificate-request+ 13)
(defconstant +handshake-certificate-verify+ 15)
(defconstant +handshake-finished+ 20)
(defconstant +handshake-key-update+ 24)
(defconstant +handshake-message-hash+ 254)

;;;; Alert Levels (RFC 8446 Section 6)

(defconstant +alert-level-warning+ 1)
(defconstant +alert-level-fatal+ 2)

;;;; Alert Descriptions (RFC 8446 Section 6.2)

(defconstant +alert-close-notify+ 0)
(defconstant +alert-unexpected-message+ 10)
(defconstant +alert-bad-record-mac+ 20)
(defconstant +alert-record-overflow+ 22)
(defconstant +alert-handshake-failure+ 40)
(defconstant +alert-bad-certificate+ 42)
(defconstant +alert-unsupported-certificate+ 43)
(defconstant +alert-certificate-revoked+ 44)
(defconstant +alert-certificate-expired+ 45)
(defconstant +alert-certificate-unknown+ 46)
(defconstant +alert-illegal-parameter+ 47)
(defconstant +alert-unknown-ca+ 48)
(defconstant +alert-access-denied+ 49)
(defconstant +alert-decode-error+ 50)
(defconstant +alert-decrypt-error+ 51)
(defconstant +alert-protocol-version+ 70)
(defconstant +alert-insufficient-security+ 71)
(defconstant +alert-internal-error+ 80)
(defconstant +alert-inappropriate-fallback+ 86)
(defconstant +alert-user-canceled+ 90)
(defconstant +alert-missing-extension+ 109)
(defconstant +alert-unsupported-extension+ 110)
(defconstant +alert-unrecognized-name+ 112)
(defconstant +alert-bad-certificate-status-response+ 113)
(defconstant +alert-unknown-psk-identity+ 115)
(defconstant +alert-certificate-required+ 116)
(defconstant +alert-no-application-protocol+ 120)

;;;; Extension Types (RFC 8446 Section 4.2)

(defconstant +extension-server-name+ 0)
(defconstant +extension-max-fragment-length+ 1)
(defconstant +extension-status-request+ 5)
(defconstant +extension-supported-groups+ 10)
(defconstant +extension-signature-algorithms+ 13)
(defconstant +extension-use-srtp+ 14)
(defconstant +extension-heartbeat+ 15)
(defconstant +extension-application-layer-protocol-negotiation+ 16)
(defconstant +extension-signed-certificate-timestamp+ 18)
(defconstant +extension-client-certificate-type+ 19)
(defconstant +extension-server-certificate-type+ 20)
(defconstant +extension-padding+ 21)
;; TLS 1.2-only extensions (forbidden in TLS 1.3 per RFC 8446)
(defconstant +extension-extended-master-secret+ 23)
(defconstant +extension-next-protocol-negotiation+ 13172)  ; 0x3374 - NPN, superseded by ALPN
(defconstant +extension-renegotiation-info+ 65281)  ; 0xFF01
(defconstant +extension-pre-shared-key+ 41)
(defconstant +extension-early-data+ 42)
(defconstant +extension-supported-versions+ 43)
(defconstant +extension-cookie+ 44)
(defconstant +extension-psk-key-exchange-modes+ 45)
(defconstant +extension-certificate-authorities+ 47)
(defconstant +extension-oid-filters+ 48)
(defconstant +extension-post-handshake-auth+ 49)
(defconstant +extension-signature-algorithms-cert+ 50)
(defconstant +extension-key-share+ 51)
;; QUIC transport parameters (RFC 9001) - only valid in QUIC, reject in TLS
(defconstant +extension-quic-transport-parameters+ 57)        ; 0x39 - standard
(defconstant +extension-quic-transport-parameters-legacy+ 65445)  ; 0xffa5 - legacy/draft
;; ALPS (Application-Layer Protocol Settings) - Google extension, not implemented
(defconstant +extension-alps+ 17513)  ; 0x4469
;; ECH (Encrypted Client Hello) - RFC 9639
(defconstant +extension-ech+ #xfe0d)

;;;; Named Groups (RFC 8446 Section 4.2.7)

(defconstant +group-secp256r1+ #x0017)
(defconstant +group-secp384r1+ #x0018)
(defconstant +group-secp521r1+ #x0019)
(defconstant +group-x25519+ #x001d)
(defconstant +group-x448+ #x001e)
(defconstant +group-ffdhe2048+ #x0100)
(defconstant +group-ffdhe3072+ #x0101)
(defconstant +group-ffdhe4096+ #x0102)
(defconstant +group-ffdhe6144+ #x0103)
(defconstant +group-ffdhe8192+ #x0104)

;; Hybrid Post-Quantum Key Exchange
;; X25519MLKEM768 uses FIPS 203 ML-KEM-768 (draft-kwiatkowski-tls-ecdhe-mlkem)
(defconstant +group-x25519-mlkem768+ #x11ec
  "X25519MLKEM768 hybrid post-quantum key exchange (FIPS 203).")
;; Legacy Kyber drafts (not compatible with ML-KEM)
(defconstant +group-x25519-kyber768-draft00+ #x6399
  "X25519Kyber768Draft00 - pre-FIPS draft, NOT compatible with ML-KEM.")
(defconstant +group-secp256r1-kyber768-draft00+ #x639a
  "SecP256r1Kyber768Draft00 - pre-FIPS draft, NOT compatible with ML-KEM.")

;;;; Signature Algorithms (RFC 8446 Section 4.2.3)

(defconstant +sig-rsa-pkcs1-md5+ #x0101)
(defconstant +sig-rsa-pkcs1-sha1+ #x0201)
(defconstant +sig-ecdsa-sha1+ #x0203)
(defconstant +sig-rsa-pkcs1-sha256+ #x0401)
(defconstant +sig-rsa-pkcs1-sha384+ #x0501)
(defconstant +sig-rsa-pkcs1-sha512+ #x0601)
(defconstant +sig-ecdsa-secp256r1-sha256+ #x0403)
(defconstant +sig-ecdsa-secp384r1-sha384+ #x0503)
(defconstant +sig-ecdsa-secp521r1-sha512+ #x0603)
(defconstant +sig-rsa-pss-rsae-sha256+ #x0804)
(defconstant +sig-rsa-pss-rsae-sha384+ #x0805)
(defconstant +sig-rsa-pss-rsae-sha512+ #x0806)
(defconstant +sig-ed25519+ #x0807)
(defconstant +sig-ed448+ #x0808)
(defconstant +sig-rsa-pss-pss-sha256+ #x0809)
(defconstant +sig-rsa-pss-pss-sha384+ #x080a)
(defconstant +sig-rsa-pss-pss-sha512+ #x080b)

;; ML-DSA (Post-Quantum) Signatures (draft-ietf-tls-mldsa)
(defconstant +sig-mldsa44+ #x0904)
(defconstant +sig-mldsa65+ #x0905)
(defconstant +sig-mldsa87+ #x0906)

;;;; Cipher Suites (RFC 8446 Section B.4)

(defconstant +tls-aes-128-gcm-sha256+ #x1301)
(defconstant +tls-aes-256-gcm-sha384+ #x1302)
(defconstant +tls-chacha20-poly1305-sha256+ #x1303)
(defconstant +tls-aes-128-ccm-sha256+ #x1304)
(defconstant +tls-aes-128-ccm-8-sha256+ #x1305)

;;;; PSK Key Exchange Modes (RFC 8446 Section 4.2.9)

(defconstant +psk-ke-mode-psk+ 0
  "PSK-only key exchange mode")
(defconstant +psk-ke-mode-dhe+ 1
  "PSK with (EC)DHE key exchange mode")

;;;; GREASE Values (RFC 8701)
;;;; GREASE values are of the form 0x?A?A where ? is any hex digit

(defparameter *grease-extension-values*
  '(#x0A0A #x1A1A #x2A2A #x3A3A #x4A4A #x5A5A #x6A6A #x7A7A
    #x8A8A #x9A9A #xAAAA #xBABA #xCACA #xDADA #xEAEA #xFAFA)
  "Reserved GREASE extension type values")

(defparameter *grease-cipher-suite-values*
  '(#x0A0A #x1A1A #x2A2A #x3A3A #x4A4A #x5A5A #x6A6A #x7A7A
    #x8A8A #x9A9A #xAAAA #xBABA #xCACA #xDADA #xEAEA #xFAFA)
  "Reserved GREASE cipher suite values")

(defparameter *grease-group-values*
  '(#x0A0A #x1A1A #x2A2A #x3A3A #x4A4A #x5A5A #x6A6A #x7A7A
    #x8A8A #x9A9A #xAAAA #xBABA #xCACA #xDADA #xEAEA #xFAFA)
  "Reserved GREASE named group values")

(defun random-grease-value (values)
  "Return a random GREASE value from the list."
  (nth (random (length values)) values))

;;;; Verification Modes

(defconstant +verify-none+ 0
  "Do not verify peer certificate")
(defconstant +verify-peer+ 1
  "Verify peer certificate if presented")
(defconstant +verify-required+ 2
  "Require and verify peer certificate")

;;;; Record Layer Constants

(defconstant +max-record-size+ 16384
  "Maximum size of a TLS record payload (2^14)")

(defconstant +max-record-size-with-padding+ 16640
  "Maximum encrypted record size per RFC 8446 §5.4:
   'The length MUST NOT exceed 2^14 + 256 bytes.' = 16640")

;;;; AEAD Constants

(defconstant +aead-tag-length+ 16
  "Length of AEAD authentication tag in bytes")

(defconstant +aead-nonce-length+ 12
  "Length of AEAD nonce in bytes")

(defconstant +aead-key-length-128+ 16
  "Key length for AES-128-GCM")

(defconstant +aead-key-length-256+ 32
  "Key length for AES-256-GCM and ChaCha20-Poly1305")

;;;; Configuration Defaults

(defparameter *default-buffer-size* 4096
  "Default buffer size for TLS streams")

(defconstant +max-warning-alerts+ 4
  "Maximum number of consecutive warning alerts before closing connection.")

(defconstant +max-key-updates+ 32
  "Maximum number of KeyUpdate messages before closing connection.
   This prevents denial-of-service via excessive rekeying.")

(defconstant +max-empty-records+ 32
  "Maximum number of consecutive empty records before closing connection.
   This prevents denial-of-service via excessive empty record flooding.")

(defparameter *default-verify-mode* +verify-required+
  "Default certificate verification mode")

(defparameter *max-certificate-list-size* 0
  "Maximum size in bytes for certificate list in Certificate message.
   0 means no limit. Set to a positive value to enforce a limit.
   Used to prevent DoS via excessively large certificate chains.")

;;;; HPKE Constants (RFC 9180)

;; HPKE Modes
(defconstant +hpke-mode-base+ #x00
  "Base mode: encryption with public key")
(defconstant +hpke-mode-psk+ #x01
  "PSK mode: pre-shared key only")
(defconstant +hpke-mode-auth+ #x02
  "Auth mode: authenticated sender")
(defconstant +hpke-mode-auth-psk+ #x03
  "AuthPSK mode: authenticated sender with PSK")

;; HPKE KEM IDs (Key Encapsulation Mechanism)
(defconstant +hpke-kem-p256-sha256+ #x0010
  "DHKEM(P-256, HKDF-SHA256)")
(defconstant +hpke-kem-p384-sha384+ #x0011
  "DHKEM(P-384, HKDF-SHA384)")
(defconstant +hpke-kem-p521-sha512+ #x0012
  "DHKEM(P-521, HKDF-SHA512)")
(defconstant +hpke-kem-x25519-sha256+ #x0020
  "DHKEM(X25519, HKDF-SHA256)")
(defconstant +hpke-kem-x448-sha512+ #x0021
  "DHKEM(X448, HKDF-SHA512)")

;; HPKE KDF IDs (Key Derivation Function)
(defconstant +hpke-kdf-hkdf-sha256+ #x0001
  "HKDF-SHA256")
(defconstant +hpke-kdf-hkdf-sha384+ #x0002
  "HKDF-SHA384")
(defconstant +hpke-kdf-hkdf-sha512+ #x0003
  "HKDF-SHA512")

;; HPKE AEAD IDs
(defconstant +hpke-aead-aes-128-gcm+ #x0001
  "AES-128-GCM")
(defconstant +hpke-aead-aes-256-gcm+ #x0002
  "AES-256-GCM")
(defconstant +hpke-aead-chacha20-poly1305+ #x0003
  "ChaCha20-Poly1305")
(defconstant +hpke-aead-export-only+ #xFFFF
  "Export-only (no AEAD)")

;;;; ECH Constants (RFC 9639)

(defconstant +ech-version+ #xfe0d
  "ECH version identifier")

(defconstant +ech-accept-confirmation-length+ 8
  "Length of ECH accept confirmation in ServerHello random")
