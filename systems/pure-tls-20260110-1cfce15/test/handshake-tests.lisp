;;; test/handshake-tests.lisp --- TLS 1.3 handshake tests
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Tests for TLS 1.3 handshake using RFC 8448 test vectors.
;;; RFC 8448 provides example TLS 1.3 handshakes with complete
;;; cryptographic values for validation.

(in-package #:pure-tls/test)

(def-suite handshake-tests
  :description "Tests for TLS 1.3 handshake state machine")

(in-suite handshake-tests)

;;;; Note: hex-to-bytes and bytes-equal are defined in crypto-tests.lisp

;;;; RFC 8448 Section 3: Simple 1-RTT Handshake
;;;;
;;;; This section tests key derivation values from the simple handshake example.

;;; Shared secret from ECDHE (X25519)
(defparameter *rfc8448-shared-secret*
  (hex-to-bytes
   "8bd4054fb55b9d63fdfbacf9f04b9f0d35e6d63f537563efd46272900f89492d"))

;;; Early secret (from PSK = 0)
(defparameter *rfc8448-early-secret*
  (hex-to-bytes
   "33ad0a1c607ec03b09e6cd9893680ce210adf300aa1f2660e1b22e10f170f92a"))

;;; Derived secret (for handshake secret derivation)
(defparameter *rfc8448-derived-secret*
  (hex-to-bytes
   "6f2615a108c702c5678f54fc9dbab69716c076189c48250cebeac3576c3611ba"))

;;; Handshake secret
(defparameter *rfc8448-handshake-secret*
  (hex-to-bytes
   "1dc826e93606aa6fdc0aadc12f741b01046aa6b99f691ed221a9f0ca043fbeac"))

;;; Client handshake traffic secret
(defparameter *rfc8448-client-handshake-traffic-secret*
  (hex-to-bytes
   "b3eddb126e067f35a780b3abf45e2d8f3b1a950738f52e9600746a0e27a55a21"))

;;; Server handshake traffic secret
(defparameter *rfc8448-server-handshake-traffic-secret*
  (hex-to-bytes
   "b67b7d690cc16c4e75e54213cb2d37b4e9c912bcded9105d42befd59d391ad38"))

;;; Master secret
(defparameter *rfc8448-master-secret*
  (hex-to-bytes
   "18df06843d13a08bf2a449844c5f8a478001bc4d4c627984d5a41da8d0402919"))

;;; Client application traffic secret
(defparameter *rfc8448-client-app-traffic-secret*
  (hex-to-bytes
   "9e40646ce79a7f9dc05af8889bce6552875afa0b06df0087f792ebb7c17504a5"))

;;; Server application traffic secret
(defparameter *rfc8448-server-app-traffic-secret*
  (hex-to-bytes
   "a11af9f05531f856ad47116b45a950328204b4f44bfb6b3a4b4f1f3fcb631643"))

;;;; Key Schedule Tests

(test rfc8448-early-secret
  "RFC 8448: Derive early secret from zero PSK"
  (let* ((zeros (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
         (early-secret (pure-tls::hkdf-extract zeros zeros :digest :sha256)))
    (is (bytes-equal early-secret *rfc8448-early-secret*)
        "Early secret should match RFC 8448 value")))

(test rfc8448-derived-secret
  "RFC 8448: Derive 'derived' secret for handshake"
  ;; Derive-Secret(Secret, "derived", "") = HKDF-Expand-Label(Secret, "derived", SHA-256(""), 32)
  (let* ((empty-hash (hex-to-bytes
                      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
         (derived (pure-tls::hkdf-expand-label
                   *rfc8448-early-secret*
                   "derived"
                   empty-hash
                   32
                   :digest :sha256)))
    (is (bytes-equal derived *rfc8448-derived-secret*)
        "Derived secret should match RFC 8448 value")))

(test rfc8448-handshake-secret
  "RFC 8448: Derive handshake secret"
  (let ((handshake-secret (pure-tls::hkdf-extract
                           *rfc8448-derived-secret*
                           *rfc8448-shared-secret*
                           :digest :sha256)))
    (is (bytes-equal handshake-secret *rfc8448-handshake-secret*)
        "Handshake secret should match RFC 8448 value")))

;;;; ClientHello/ServerHello Transcript Hash
;;;
;;; The transcript hash for these tests is the SHA-256 hash of
;;; ClientHello || ServerHello messages.

(defparameter *rfc8448-hello-transcript-hash*
  (hex-to-bytes
   "860c06edc07858ee8e78f0e7428c58edd6b43f2ca3e6e95f02ed063cf0e1cad8"))

(test rfc8448-client-handshake-traffic-secret
  "RFC 8448: Derive client handshake traffic secret"
  ;; Use hkdf-expand-label directly since we have the pre-computed transcript hash
  ;; derive-secret would hash the input again, causing double-hashing
  (let ((client-hs-secret (pure-tls::hkdf-expand-label
                           *rfc8448-handshake-secret*
                           "c hs traffic"
                           *rfc8448-hello-transcript-hash*
                           32
                           :digest :sha256)))
    (is (bytes-equal client-hs-secret *rfc8448-client-handshake-traffic-secret*)
        "Client handshake traffic secret should match RFC 8448 value")))

(test rfc8448-server-handshake-traffic-secret
  "RFC 8448: Derive server handshake traffic secret"
  ;; Use hkdf-expand-label directly since we have the pre-computed transcript hash
  (let ((server-hs-secret (pure-tls::hkdf-expand-label
                           *rfc8448-handshake-secret*
                           "s hs traffic"
                           *rfc8448-hello-transcript-hash*
                           32
                           :digest :sha256)))
    (is (bytes-equal server-hs-secret *rfc8448-server-handshake-traffic-secret*)
        "Server handshake traffic secret should match RFC 8448 value")))

;;;; Key/IV Derivation Tests

(test rfc8448-handshake-key-derivation
  "RFC 8448: Derive handshake traffic keys from traffic secret"
  (let* ((traffic-secret *rfc8448-server-handshake-traffic-secret*)
         (expected-key (hex-to-bytes "3fce516009c21727d0f2e4e86ee403bc"))
         (expected-iv (hex-to-bytes "5d313eb2671276ee13000b30")))
    (let ((key (pure-tls::hkdf-expand-label traffic-secret "key"
                                             (make-array 0 :element-type '(unsigned-byte 8))
                                             16 :digest :sha256))
          (iv (pure-tls::hkdf-expand-label traffic-secret "iv"
                                            (make-array 0 :element-type '(unsigned-byte 8))
                                            12 :digest :sha256)))
      (is (bytes-equal key expected-key)
          "Server handshake key should match RFC 8448")
      (is (bytes-equal iv expected-iv)
          "Server handshake IV should match RFC 8448"))))

;;;; Finished Message Tests

(test rfc8448-finished-key-derivation
  "RFC 8448: Derive finished key"
  (let* ((base-key *rfc8448-server-handshake-traffic-secret*)
         (expected-finished-key (hex-to-bytes
                                 "008d3b66f816ea559f96b537e885c31fc068bf492c652f01f288a1d8cdc19fc8")))
    (let ((finished-key (pure-tls::hkdf-expand-label
                         base-key "finished"
                         (make-array 0 :element-type '(unsigned-byte 8))
                         32 :digest :sha256)))
      (is (bytes-equal finished-key expected-finished-key)
          "Server finished key should match RFC 8448"))))

;;;; Extension Parsing Tests

(test parse-supported-versions-extension
  "Test parsing supported_versions extension"
  ;; ClientHello supported_versions: length (1 byte) + versions (2 bytes each)
  ;; 04 = 4 bytes of version data (2 versions × 2 bytes)
  ;; 03 04 = TLS 1.3 (0x0304)
  ;; 03 03 = TLS 1.2 (0x0303)
  (let* ((data (hex-to-bytes "04 0304 0303"))  ; length=4, TLS 1.3, TLS 1.2
         (ext (pure-tls::parse-supported-versions-extension data)))
    (is (member pure-tls::+tls-1.3+ (pure-tls::supported-versions-ext-versions ext))
        "Should contain TLS 1.3")
    (is (member pure-tls::+tls-1.2+ (pure-tls::supported-versions-ext-versions ext))
        "Should contain TLS 1.2")))

(test parse-key-share-extension
  "Test parsing key_share extension with X25519"
  ;; Client key_share: length (2) + group (2) + key length (2) + key (32)
  (let* ((key-data (hex-to-bytes "001d0020"))  ; X25519, 32 bytes
         (public-key (pure-tls:random-bytes 32))
         (full-data (concatenate '(vector (unsigned-byte 8))
                                 (hex-to-bytes "0024")  ; Total length 36
                                 key-data
                                 public-key))
         (ext (pure-tls::parse-key-share-extension full-data)))
    (is (pure-tls::key-share-ext-client-shares ext)
        "Should have client shares")
    (let ((share (first (pure-tls::key-share-ext-client-shares ext))))
      (is (= (pure-tls::key-share-entry-group share) pure-tls::+group-x25519+)
          "Group should be X25519"))))

;;;; Cipher Suite Tests

(test cipher-suite-constants
  "Verify cipher suite constants"
  (is (= pure-tls:+tls-aes-128-gcm-sha256+ #x1301))
  (is (= pure-tls:+tls-aes-256-gcm-sha384+ #x1302))
  (is (= pure-tls:+tls-chacha20-poly1305-sha256+ #x1303)))

(test cipher-suite-properties
  "Test cipher suite property functions"
  ;; AES-128-GCM-SHA256
  (is (= (pure-tls::cipher-suite-key-length pure-tls:+tls-aes-128-gcm-sha256+) 16))
  (is (= (pure-tls::cipher-suite-hash-length pure-tls:+tls-aes-128-gcm-sha256+) 32))
  (is (eql (pure-tls::cipher-suite-digest pure-tls:+tls-aes-128-gcm-sha256+) :sha256))

  ;; AES-256-GCM-SHA384
  (is (= (pure-tls::cipher-suite-key-length pure-tls:+tls-aes-256-gcm-sha384+) 32))
  (is (= (pure-tls::cipher-suite-hash-length pure-tls:+tls-aes-256-gcm-sha384+) 48))
  (is (eql (pure-tls::cipher-suite-digest pure-tls:+tls-aes-256-gcm-sha384+) :sha384))

  ;; ChaCha20-Poly1305-SHA256
  (is (= (pure-tls::cipher-suite-key-length pure-tls:+tls-chacha20-poly1305-sha256+) 32))
  (is (= (pure-tls::cipher-suite-hash-length pure-tls:+tls-chacha20-poly1305-sha256+) 32))
  (is (eql (pure-tls::cipher-suite-digest pure-tls:+tls-chacha20-poly1305-sha256+) :sha256)))

;;;; Named Group Tests

(test named-group-constants
  "Verify named group constants"
  (is (= pure-tls::+group-secp256r1+ #x0017))
  (is (= pure-tls::+group-x25519+ #x001d)))

;;;; Signature Algorithm Tests

(test signature-algorithm-constants
  "Verify signature algorithm constants"
  (is (= pure-tls::+sig-rsa-pkcs1-sha256+ #x0401))
  (is (= pure-tls::+sig-ecdsa-secp256r1-sha256+ #x0403))
  (is (= pure-tls::+sig-rsa-pss-rsae-sha256+ #x0804))
  (is (= pure-tls::+sig-ed25519+ #x0807)))

;;;; Key Exchange Tests

(test x25519-key-exchange
  "Test X25519 key exchange generates valid keys"
  (let ((kex (pure-tls::generate-key-exchange pure-tls::+group-x25519+)))
    (is (= (pure-tls::key-exchange-group kex) pure-tls::+group-x25519+)
        "Group should be X25519")
    (is (= (length (pure-tls::key-exchange-public-key kex)) 32)
        "X25519 public key should be 32 bytes")))

(test x25519-shared-secret
  "Test X25519 shared secret computation"
  (let* ((alice (pure-tls::generate-key-exchange pure-tls::+group-x25519+))
         (bob (pure-tls::generate-key-exchange pure-tls::+group-x25519+))
         (alice-shared (pure-tls::compute-shared-secret
                        alice (pure-tls::key-exchange-public-key bob)))
         (bob-shared (pure-tls::compute-shared-secret
                      bob (pure-tls::key-exchange-public-key alice))))
    (is (bytes-equal alice-shared bob-shared)
        "Both parties should compute same shared secret")
    (is (= (length alice-shared) 32)
        "Shared secret should be 32 bytes")))

(test secp256r1-key-exchange
  "Test secp256r1 key exchange generates valid keys"
  (let ((kex (pure-tls::generate-key-exchange pure-tls::+group-secp256r1+)))
    (is (= (pure-tls::key-exchange-group kex) pure-tls::+group-secp256r1+)
        "Group should be secp256r1")
    (is (= (length (pure-tls::key-exchange-public-key kex)) 65)
        "secp256r1 uncompressed public key should be 65 bytes")))

;;;; HelloRetryRequest Tests

(test hello-retry-request-magic-random
  "Test that HRR is detected by magic random value"
  (let* ((hrr-random pure-tls::+hello-retry-request-random+)
         (normal-random (pure-tls::random-bytes 32))
         ;; Create a mock ServerHello with HRR random
         (hrr-hello (pure-tls::make-server-hello
                     :legacy-version pure-tls::+tls-1.2+
                     :random hrr-random
                     :legacy-session-id-echo (make-array 0 :element-type '(unsigned-byte 8))
                     :cipher-suite pure-tls::+tls-aes-128-gcm-sha256+
                     :legacy-compression-method 0
                     :extensions nil))
         ;; Create a normal ServerHello
         (normal-hello (pure-tls::make-server-hello
                        :legacy-version pure-tls::+tls-1.2+
                        :random normal-random
                        :legacy-session-id-echo (make-array 0 :element-type '(unsigned-byte 8))
                        :cipher-suite pure-tls::+tls-aes-128-gcm-sha256+
                        :legacy-compression-method 0
                        :extensions nil)))
    (is (pure-tls::hello-retry-request-p hrr-hello)
        "ServerHello with magic random should be detected as HRR")
    (is (not (pure-tls::hello-retry-request-p normal-hello))
        "Normal ServerHello should not be detected as HRR")))

(test hello-retry-request-key-share-serialization
  "Test that HRR key_share extension serializes correctly (just selected_group)"
  (let* ((ext (pure-tls::make-key-share-ext :selected-group pure-tls::+group-x25519+))
         (serialized (pure-tls::serialize-key-share-extension ext)))
    ;; HRR key_share should be just 2 bytes (the group)
    (is (= (length serialized) 2)
        "HRR key_share should be 2 bytes (just the group)")
    (is (= (pure-tls::decode-uint16 serialized 0) pure-tls::+group-x25519+)
        "Serialized group should match")))

;;;; Test Runner

(defun run-handshake-tests ()
  "Run all handshake tests."
  (run! 'handshake-tests))
