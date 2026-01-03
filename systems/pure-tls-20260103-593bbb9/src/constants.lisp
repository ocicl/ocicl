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

;;;; Signature Algorithms (RFC 8446 Section 4.2.3)

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
  "Maximum size with expansion (2^14 + 256)")

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

(defparameter *default-verify-mode* +verify-required+
  "Default certificate verification mode")
