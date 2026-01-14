# pure-tls

A pure Common Lisp implementation of TLS 1.3 (RFC 8446).

## Quick Start

### Server

HTTPS server with automatic Let's Encrypt certificates:

```lisp
(asdf:load-system :pure-tls/acme+hunchentoot)

(hunchentoot:start
  (pure-tls/acme:make-acme-acceptor "example.com" "admin@example.com"))
```

The server obtains a certificate on first start and renews it automatically.

### Client

Use with drakma via cl+ssl compatibility layer (drop-in OpenSSL replacement):

```lisp
(asdf:load-system :pure-tls/cl+ssl-compat)
(asdf:register-immutable-system "cl+ssl")
(asdf:load-system :drakma)

(drakma:http-request "https://example.com/")
```

## Features

- **Pure Common Lisp** - No foreign libraries or OpenSSL dependency
- **TLS 1.3 only** - Modern, secure protocol with simplified handshake
- **Post-quantum ready** - X25519MLKEM768 hybrid key exchange (FIPS 203)
- **Automatic certificates** - Built-in ACME client for Let's Encrypt
- **Gray streams** - Seamless integration with existing I/O code
- **cl+ssl compatible** - Drop-in replacement API available
- **Native trust store** - Uses Windows CryptoAPI and macOS Security.framework

### Supported Cipher Suites

- `TLS_CHACHA20_POLY1305_SHA256` (0x1303) - Preferred for side-channel resistance
- `TLS_AES_256_GCM_SHA384` (0x1302)
- `TLS_AES_128_GCM_SHA256` (0x1301)

### Supported Key Exchange

- **X25519MLKEM768** (hybrid post-quantum) - Combines X25519 with ML-KEM-768 (FIPS 203)
- X25519 (Curve25519)
- secp256r1 (P-256)
- secp384r1 (P-384)

## Installation

Using [ocicl](https://github.com/ocicl/ocicl):

```sh
ocicl install pure-tls
```

Or add to your ASDF system:

```lisp
:depends-on (#:pure-tls)
```

## Usage

### HTTPS Client

```lisp
(let ((socket (usocket:socket-connect "example.com" 443
                                       :element-type '(unsigned-byte 8))))
  (pure-tls:with-tls-client-stream (tls (usocket:socket-stream socket)
                                        :hostname "example.com")
    ;; Send HTTP request
    (write-sequence (flexi-streams:string-to-octets
                      "GET / HTTP/1.1\r\nHost: example.com\r\nConnection: close\r\n\r\n"
                      :external-format :utf-8)
                    tls)
    (force-output tls)
    ;; Read response
    (loop for byte = (read-byte tls nil nil)
          while byte
          do (write-char (code-char byte)))))
;; Stream automatically closed
```

### With Certificate Verification

```lisp
(pure-tls:with-tls-client-stream (tls socket
                                      :hostname "example.com"
                                      :verify pure-tls:+verify-peer+)
  (do-something-with tls))
```

### ALPN Protocol Negotiation

```lisp
(pure-tls:with-tls-client-stream (tls socket
                                      :hostname "example.com"
                                      :alpn-protocols '("h2" "http/1.1"))
  (format t "Selected protocol: ~A~%" (pure-tls:tls-selected-alpn tls)))
```

### TLS Server

```lisp
(let ((server (usocket:socket-listen "0.0.0.0" 8443)))
  (loop
    (let ((client (usocket:socket-accept server :element-type '(unsigned-byte 8))))
      (pure-tls:with-tls-server-stream (tls (usocket:socket-stream client)
                                            :certificate "/path/to/cert.pem"
                                            :key "/path/to/key.pem")
        (handle-request tls)))))
```

### Server with Client Certificate Authentication (mTLS)

```lisp
(pure-tls:make-tls-server-stream stream
  :certificate "/path/to/server-cert.pem"
  :key "/path/to/server-key.pem"
  :verify pure-tls:+verify-required+)  ; Require client certificate
```

### Server with SNI Callback (Virtual Hosting)

```lisp
(defun my-sni-callback (hostname)
  "Return certificate and key based on client-requested hostname.
   Return :reject to send an unrecognized_name alert and abort the handshake."
  (cond
    ((string= hostname "site-a.example.com")
     (values (pure-tls:load-certificate-chain "/certs/site-a.pem")
             (pure-tls:load-private-key "/certs/site-a-key.pem")))
    ((string= hostname "site-b.example.com")
     (values (pure-tls:load-certificate-chain "/certs/site-b.pem")
             (pure-tls:load-private-key "/certs/site-b-key.pem")))
    ((string= hostname "blocked.example.com")
     :reject)  ; Reject unknown/blocked hostnames with unrecognized_name alert
    (t nil)))  ; Use default certificate

(pure-tls:make-tls-server-stream stream
  :certificate "/path/to/default-cert.pem"
  :key "/path/to/default-key.pem"
  :sni-callback #'my-sni-callback)
```

### Using the cl+ssl Compatibility Layer

The `pure-tls/cl+ssl-compat` system provides a drop-in replacement for cl+ssl,
allowing existing code using cl+ssl to work with pure-tls without modification.

```lisp
(asdf:load-system :pure-tls/cl+ssl-compat)

;; Use familiar cl+ssl API
(cl+ssl:make-ssl-client-stream stream
  :hostname "example.com"
  :verify :optional)
```

The compatibility layer supports:
- `cl+ssl:make-ssl-client-stream` / `cl+ssl:make-ssl-server-stream`
- `cl+ssl:make-context` / `cl+ssl:with-global-context` / `cl+ssl:call-with-global-context`
- `cl+ssl:stream-fd` (converts file descriptors back to streams)
- Certificate functions and verification constants

### Replacing cl+ssl in Existing Applications

To use pure-tls instead of cl+ssl in an application that depends on libraries
requiring cl+ssl (such as drakma), use `asdf:register-immutable-system` to
prevent ASDF from loading the real cl+ssl:

```lisp
;;; In your .asd file, before the defsystem:
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Load pure-tls compatibility layer first
  (asdf:load-system :pure-tls/cl+ssl-compat)
  ;; Tell ASDF that "cl+ssl" is already satisfied - never load the real one
  (asdf:register-immutable-system "cl+ssl"))

(asdf:defsystem "my-application"
  :depends-on (:drakma ...)  ; drakma depends on cl+ssl, but won't load it
  ...)
```

This technique:
1. Loads the pure-tls compatibility layer, which defines the `CL+SSL` package
2. Registers "cl+ssl" as an immutable system, so ASDF treats it as already loaded
3. When drakma (or any library) requests `:cl+ssl`, ASDF skips loading it

This allows you to eliminate OpenSSL as a dependency entirely, making your
application fully portable pure Common Lisp for TLS.

## ACME Client (Let's Encrypt)

The `pure-tls/acme` system provides automatic certificate management using the ACME protocol (RFC 8555), compatible with Let's Encrypt and other ACME-compliant certificate authorities.

### Multi-Domain Certificates

```lisp
(pure-tls/acme:make-acme-acceptor
  '("example.com" "www.example.com" "api.example.com")
  "admin@example.com"
  :renewal-days 30)
```

### ACME Systems

The ACME functionality is split into two systems:

- **`pure-tls/acme`** - Core ACME client (no web server dependency)
- **`pure-tls/acme+hunchentoot`** - Hunchentoot integration with `acme-acceptor`

Use `pure-tls/acme` directly if you're using a different web server.

### Non-Hunchentoot Usage

For other web servers, use the ACME client directly with the `:certificate-provider` callback:

```lisp
(asdf:load-system :pure-tls/acme)

;; Create store and client
(defvar *store* (pure-tls/acme:make-cert-store))
(defvar *client* (pure-tls/acme:make-acme-client
                   :directory-url pure-tls/acme:*production-url*
                   :store *store*))

;; Thread-safe validation state for challenges
(defvar *validation-lock* (bt:make-lock "validation"))
(defvar *validation-cert* nil)
(defvar *validation-key* nil)

;; Certificate provider for your TLS server
(defun my-certificate-provider (hostname alpn-list)
  (when (member "acme-tls/1" alpn-list :test #'string=)
    (bt:with-lock-held (*validation-lock*)
      (when (and *validation-cert* *validation-key*)
        (values (list *validation-cert*) *validation-key* "acme-tls/1")))))

;; Use with pure-tls server streams
(pure-tls:make-tls-server-stream stream
  :certificate "/path/to/cert.pem"
  :key "/path/to/key.pem"
  :certificate-provider #'my-certificate-provider)
```

### Certificate Storage

Certificates are stored in platform-appropriate locations:

| Platform | Default Path |
|----------|-------------|
| Linux    | `~/.local/state/pure-tls/` |
| macOS    | `~/Library/Application Support/pure-tls/` |
| Windows  | `%LOCALAPPDATA%\pure-tls\` |

To use a custom location:

```lisp
(pure-tls/acme:make-cert-store :base-path #p"/etc/ssl/acme/")
```

### TLS-ALPN-01 Challenge

pure-tls/acme uses the TLS-ALPN-01 challenge type, which validates domain ownership by serving a special self-signed certificate on port 443. This is ideal for:

- Servers that already run on port 443 (challenges handled inline)
- Environments where HTTP port 80 is not available
- Automatic renewal without service interruption

**Requirements:**
- Port 443 must be accessible from the internet
- The domain must resolve to your server's IP address

### Configuration Options

```lisp
(pure-tls/acme:make-acme-acceptor domains email
  :port 443              ; HTTPS port (default 443)
  :production nil        ; Use Let's Encrypt staging for testing (default T = production)
  :renewal-days 30       ; Renew when cert expires within N days
  :store store           ; Custom cert-store (optional)
  :logger #'my-logger)   ; Custom logging function (optional)
```

### Debugging

Enable debug logging:

```lisp
(setf pure-tls/acme:*acme-debug* t)
```

### Testing with Pebble

For local development, use [Pebble](https://github.com/letsencrypt/pebble), a small ACME test server:

```bash
# Start Pebble (requires podman or docker)
cd test/acme
./run-pebble.sh start

# Run tests
sbcl --load quick-pebble-test.lisp

# Stop Pebble
./run-pebble.sh stop
```

## API Reference

### Stream Creation

#### `with-tls-client-stream` ((var stream &rest args) &body body)

Execute BODY with VAR bound to a TLS client stream. The stream is automatically closed when BODY exits (normally or via non-local exit).

```lisp
(pure-tls:with-tls-client-stream (tls socket :hostname "example.com")
  (write-sequence data tls)
  (force-output tls)
  (read-sequence buffer tls))
;; tls is automatically closed here
```

#### `with-tls-server-stream` ((var stream &rest args) &body body)

Execute BODY with VAR bound to a TLS server stream. The stream is automatically closed when BODY exits.

#### `make-tls-client-stream` (socket &key hostname sni-hostname context verify alpn-protocols close-callback external-format buffer-size)

Create a TLS client stream over a TCP socket.

- `socket` - The underlying TCP stream
- `hostname` - Server hostname for SNI and certificate verification
- `sni-hostname` - Override hostname for SNI only (no certificate hostname verification)
- `context` - TLS context for configuration (optional)
- `verify` - Certificate verification mode: `+verify-none+`, `+verify-peer+`, or `+verify-required+`
- `alpn-protocols` - List of ALPN protocol names to offer
- `close-callback` - Function called when stream is closed
- `external-format` - If specified, wrap in a flexi-stream for character I/O
- `buffer-size` - Size of I/O buffers (default 16384)

#### `make-tls-server-stream` (socket &key context certificate key verify alpn-protocols sni-callback close-callback external-format buffer-size)

Create a TLS server stream over a TCP socket.

- `socket` - The underlying TCP stream
- `context` - TLS context for configuration (optional)
- `certificate` - Certificate chain (list of certificates or path to PEM file)
- `key` - Private key (Ironclad key object or path to PEM file)
- `verify` - Client certificate verification mode: `+verify-none+`, `+verify-peer+`, or `+verify-required+`
- `alpn-protocols` - List of ALPN protocol names the server supports
- `sni-callback` - Function called with client's requested hostname, returns (VALUES cert-chain private-key), NIL to use defaults, or :REJECT to abort with unrecognized_name alert
- `close-callback` - Function called when stream is closed
- `external-format` - If specified, wrap in a flexi-stream for character I/O
- `buffer-size` - Size of I/O buffers (default 16384)

### Stream Accessors

- `(tls-peer-certificate stream)` - Returns the peer's X.509 certificate
- `(tls-peer-certificate-chain stream)` - Returns the peer's full certificate chain
- `(tls-selected-alpn stream)` - Returns the negotiated ALPN protocol
- `(tls-cipher-suite stream)` - Returns the negotiated cipher suite
- `(tls-version stream)` - Returns the TLS version (always 1.3)
- `(tls-client-hostname stream)` - Returns the client's SNI hostname (server-side only)
- `(tls-request-key-update stream &key request-peer-update)` - Request a TLS 1.3 key update

### Context Management

#### `make-tls-context` (&key verify-mode certificate-chain private-key alpn-protocols ca-certificates)

Create a reusable TLS context for configuration.

### Verification Modes

- `+verify-none+` (0) - No certificate verification
- `+verify-peer+` (1) - Verify peer certificate if provided
- `+verify-required+` (2) - Require and verify peer certificate

## Certificate Verification

### Windows

On Windows, pure-tls uses the Windows CryptoAPI to validate certificates
against the system certificate store. This is the authoritative verification
method on Windows - there is no fallback to pure Lisp verification:

- **No CA bundle needed** - Uses Windows trusted root certificates
- **Enterprise PKI support** - Respects Group Policy certificate deployments
- **Automatic updates** - Trust store is maintained by Windows Update
- **Authoritative** - CryptoAPI verdict is final; if it rejects a certificate, the connection fails

To disable native verification and use pure Lisp verification instead
(requires providing CA certificates manually):

```lisp
(setf pure-tls:*use-windows-certificate-store* nil)
```

### macOS

On macOS, pure-tls uses the Security.framework to validate certificates
against the system Keychain. This is the authoritative verification
method on macOS - there is no fallback to pure Lisp verification:

- **No CA bundle needed** - Uses macOS Keychain trusted root certificates
- **Enterprise PKI support** - Respects MDM-deployed certificates
- **Automatic updates** - Trust store is maintained by macOS updates
- **Authoritative** - Keychain verdict is final; if it rejects a certificate, the connection fails

To disable native verification and use pure Lisp verification instead
(requires providing CA certificates manually):

```lisp
(setf pure-tls:*use-macos-keychain* nil)
```

### Linux

On Linux, pure-tls uses pure Lisp certificate verification
and automatically searches for CA certificates:

1. `SSL_CERT_FILE` environment variable
2. `SSL_CERT_DIR` environment variable
3. Platform-specific locations:
   - `/etc/ssl/certs/ca-certificates.crt` (Debian/Ubuntu)
   - `/etc/pki/tls/certs/ca-bundle.crt` (RHEL/CentOS)
   - Homebrew OpenSSL paths

If CA certificates are not found automatically:

```sh
export SSL_CERT_FILE=/path/to/cacert.pem
```

Or download the Mozilla CA bundle from https://curl.se/ca/cacert.pem

### Custom CA Certificates

For corporate environments or testing with custom CAs:

```lisp
;; Use a specific CA bundle file
(pure-tls:make-tls-context :ca-file "/path/to/ca-bundle.crt")

;; Use a directory of certificates
(pure-tls:make-tls-context :ca-directory "/path/to/certs/")

;; Add corporate CA alongside system certificates
(pure-tls:make-tls-context :ca-file "/path/to/corporate-ca.pem")

;; Use only custom CA (skip system certificates)
(pure-tls:make-tls-context
  :ca-file "/path/to/custom-ca.pem"
  :auto-load-system-ca nil)
```

## Side-Channel Hardening

pure-tls implements several measures to mitigate side-channel attacks:

### Constant-Time Operations

All security-sensitive comparisons (MAC verification, key comparison) use Ironclad's constant-time comparison functions to prevent timing attacks. The implementation avoids early-return patterns that could leak information about secret data.

### Uniform Error Handling

All decryption failures produce identical error conditions (`tls-mac-error`) regardless of the failure cause, as required by RFC 8446. This prevents padding oracle attacks by ensuring attackers cannot distinguish between different types of decryption failures.

### Secret Zeroization

Sensitive cryptographic material can be explicitly cleared from memory using the `zeroize` function or the `with-zeroized-vector` macro:

```lisp
;; Explicit zeroization
(let ((key (derive-key ...)))
  (unwind-protect
      (use-key key)
    (pure-tls:zeroize key)))

;; RAII-style zeroization
(pure-tls:with-zeroized-vector (key (derive-key ...))
  (use-key key))
;; key is automatically zeroed here, even if an error occurs
```

Note: In a garbage-collected runtime, zeroization is best-effort as the GC may have already copied the data. For highest security requirements, consider foreign memory that can be mlock'd.

### TLS 1.3 Record Padding

Record padding helps mitigate traffic analysis by hiding the true length of application data. Configure padding via `*record-padding-policy*`:

```lisp
;; Pad all records to 256-byte boundaries
(setf pure-tls:*record-padding-policy* :block-256)

;; Pad to 1024-byte boundaries
(setf pure-tls:*record-padding-policy* :block-1024)

;; Fixed-size records (4096 bytes)
(setf pure-tls:*record-padding-policy* :fixed-4096)

;; Custom padding function
(setf pure-tls:*record-padding-policy*
      (lambda (plaintext-length)
        (* 128 (ceiling plaintext-length 128))))

;; No padding (default)
(setf pure-tls:*record-padding-policy* nil)
```

### Side-Channel Considerations

- **ChaCha20-Poly1305 (Recommended)**: This cipher suite uses only ARX (add-rotate-xor) operations, which are inherently constant-time and resistant to cache-timing attacks. It is the preferred cipher suite for pure software implementations.
- **AES-GCM**: Since Ironclad implements AES in pure Common Lisp using table lookups (rather than hardware AES-NI instructions), the AES-GCM cipher suites may be susceptible to cache-timing attacks. When possible, prefer ChaCha20-Poly1305 for better side-channel resistance.

## Post-Quantum Key Exchange

pure-tls supports **X25519MLKEM768**, a hybrid post-quantum key exchange that combines classical X25519 with the ML-KEM-768 lattice-based algorithm (FIPS 203). This provides defense against "harvest now, decrypt later" attacks where adversaries collect encrypted traffic today to decrypt with future quantum computers.

### How It Works

X25519MLKEM768 performs two key exchanges in parallel:

1. **X25519** - Classical elliptic curve Diffie-Hellman (128-bit security)
2. **ML-KEM-768** - Lattice-based key encapsulation (192-bit post-quantum security)

The shared secrets are concatenated, ensuring security even if one algorithm is broken.

### Automatic Negotiation

Post-quantum key exchange is negotiated automatically when both client and server support it:

```lisp
;; Client and server negotiate X25519MLKEM768 if both support it
;; No configuration needed - it's the preferred key exchange
(pure-tls:make-tls-client-stream stream :hostname "example.com")
```

### Browser Compatibility

Major browsers support X25519MLKEM768:
- **Chrome 124+** - Enabled by default
- **Firefox** - Behind flag
- **Safari** - Not yet supported

### Testing Post-Quantum with Chrome

A test server is included for Chrome interoperability testing:

```bash
cd test/chrome-interop
./generate-localhost-cert.sh  # Generate self-signed cert (once)
sbcl --load chrome-server.lisp

# Open Chrome to https://localhost:8443/
# The page shows whether post-quantum key exchange was negotiated
```

### FIPS 203 Compliance

The ML-KEM-768 implementation:
- Passes all 1000 NIST FIPS 203 Known Answer Test (KAT) vectors
- Uses constant-time modular arithmetic (Barrett reduction)
- Implements implicit rejection for CCA security

To run the KAT tests:

```bash
# Download test vectors
curl -sL https://raw.githubusercontent.com/post-quantum-cryptography/KAT/main/MLKEM/kat_MLKEM_768.rsp \
     -o test/vectors/kat_MLKEM_768.rsp

# Run tests
sbcl --eval '(asdf:load-system :pure-tls)' \
     --load test/ml-kem-kat.lisp \
     --eval '(ml-kem-kat:run-tests)'
```

### Security Considerations

- **Hybrid design** - Security relies on the stronger of X25519 or ML-KEM-768
- **Larger key shares** - Client sends 1216 bytes, server sends 1120 bytes (vs 32 bytes for X25519 alone)
- **Constant-time** - All secret-dependent operations use constant-time arithmetic
- **Implicit rejection** - Invalid ciphertexts produce pseudorandom output (CCA security)

## Debugging with Wireshark

pure-tls supports the NSS Key Log format via the `SSLKEYLOGFILE` environment variable. This allows you to decrypt TLS traffic in Wireshark for debugging purposes.

### Setup

1. Set the `SSLKEYLOGFILE` environment variable to a writable file path:
   ```sh
   export SSLKEYLOGFILE=/tmp/tls-keys.log
   ```

2. Start your Lisp application that uses pure-tls

3. In Wireshark:
   - Go to **Edit > Preferences > Protocols > TLS**
   - Set **(Pre)-Master-Secret log filename** to the same path (`/tmp/tls-keys.log`)
   - Capture traffic and Wireshark will automatically decrypt TLS 1.3 sessions

### Logged Secrets

The following secrets are logged (compatible with Wireshark TLS 1.3 dissector):

- `CLIENT_HANDSHAKE_TRAFFIC_SECRET` - Client handshake traffic key
- `SERVER_HANDSHAKE_TRAFFIC_SECRET` - Server handshake traffic key
- `CLIENT_TRAFFIC_SECRET_0` - Client application traffic key
- `SERVER_TRAFFIC_SECRET_0` - Server application traffic key
- `EXPORTER_SECRET` - Exporter master secret

## Dependencies

- [ironclad](https://github.com/sharplispers/ironclad) - Cryptographic primitives
- [trivial-gray-streams](https://github.com/trivial-gray-streams/trivial-gray-streams) - Gray stream support
- [flexi-streams](https://github.com/edicl/flexi-streams) - Character encoding (optional)
- [alexandria](https://github.com/keithj/alexandria) - Utilities
- [trivial-features](https://github.com/trivial-features/trivial-features) - Portable platform detection
- [cffi](https://github.com/cffi/cffi) - Windows and macOS only, for native trust store bindings

## Session Resumption (PSK)

pure-tls supports TLS 1.3 session resumption using Pre-Shared Keys (PSK) derived from NewSessionTicket messages. This allows clients to reconnect to servers more quickly by skipping the certificate exchange.

### How It Works

1. After a successful handshake, the server sends a NewSessionTicket message
2. The client caches the ticket (keyed by hostname)
3. On subsequent connections, the client offers the cached PSK
4. If the server accepts, the handshake completes without certificate exchange

### Client Usage

Session resumption is automatic. The client caches session tickets and offers them on subsequent connections:

```lisp
;; First connection - full handshake
(let ((tls (pure-tls:make-tls-client-stream stream :hostname "example.com")))
  ;; ... use connection ...
  (close tls))

;; Second connection - resumed session (faster)
(let ((tls (pure-tls:make-tls-client-stream stream :hostname "example.com")))
  ;; ... uses cached PSK if available ...
  (close tls))
```

### Managing the Session Cache

```lisp
;; Clear all cached session tickets
(pure-tls:session-ticket-cache-clear)

;; Clear ticket for a specific hostname
(pure-tls:session-ticket-cache-clear "example.com")
```

### Server Configuration

For servers, session tickets are encrypted with a server-side key. You can set a persistent key for session tickets to survive server restarts:

```lisp
;; Set a 32-byte key for ticket encryption
;; (If not set, a random key is generated on first use)
(setf pure-tls:*server-ticket-key* (pure-tls:random-bytes 32))
```

### Security Considerations

- Session tickets are encrypted with AES-256-GCM
- Ticket lifetime is 24 hours by default
- Only PSK with (EC)DHE key exchange is supported (provides forward secrecy)
- PSK-only mode (without (EC)DHE) is not supported

## Testing

### Running the Test Suite

```bash
# Run all tests (unit, network, BoringSSL)
make test

# Or from Lisp:
(asdf:load-system :pure-tls/test)
(pure-tls/test:run-tests)          ; Offline tests
(pure-tls/test:run-network-tests)  ; Network tests (requires internet)
```

### Test Coverage

The test suite validates:

- **Cryptographic primitives**: HKDF (RFC 5869), AES-GCM, ChaCha20-Poly1305 (RFC 8439)
- **TLS 1.3 key schedule**: RFC 8448 test vectors for all key derivation steps
- **Record layer**: Header format, content types, AEAD nonce construction
- **X.509 certificates**: ASN.1 parsing, hostname verification, OID handling
- **Bundled bad certificates**: Offline tests using certificates from [badssl.com](https://github.com/chromium/badssl.com) (expired, self-signed, known malware CAs)
- **X.509 validation**: Certificate validation tests from Google's [x509test](https://github.com/google/x509test) project (RFC 5280 compliance, X.690 DER encoding)
- **OpenSSL test suite**: Live TLS handshake tests adapted from OpenSSL's ssl-tests (basic handshakes, ALPN, SNI, key update, curves, mTLS)
- **BoringSSL test suite**: Protocol compliance testing via shim binary (65% pass rate; failures are TLS 1.2 tests which pure-tls does not implement)
- **Live validation**: TLS 1.3 connections to major sites (Google, Cloudflare, GitHub, etc.)

### BoringSSL Test Suite

The BoringSSL test runner provides comprehensive protocol compliance testing:

```bash
# Build the shim binary
make boringssl-shim

# Run tests (requires BoringSSL checkout)
export BORINGSSL_DIR=/path/to/boringssl
make boringssl-tests
```

The shim implements the BoringSSL test protocol, allowing pure-tls to be tested against 6500+ test cases covering edge cases, malformed messages, and protocol violations.

### Individual Test Suites

```lisp
(pure-tls/test:run-crypto-tests)       ; Cryptographic primitives
(pure-tls/test:run-record-tests)       ; Record layer
(pure-tls/test:run-handshake-tests)    ; Key schedule, extensions
(pure-tls/test:run-certificate-tests)  ; X.509 parsing
(pure-tls/test:run-x509test-tests)     ; X.509 validation (RFC 5280)
(pure-tls/test:run-network-tests)      ; Network tests (requires internet)

;; OpenSSL-adapted tests
(fiveam:run! 'pure-tls/test::openssl-tests)
```

## Limitations

### Not Supported

- **0-RTT early data** - Disabled for security (replay attack concerns)
- **DTLS** - Datagram TLS (UDP-based) is not implemented
- **Certificate compression** - RFC 8879 is not implemented
- **Post-quantum signatures** - ML-DSA (FIPS 204) is not yet supported for certificates

### Limited Support

- **Elliptic curves** - Only X25519, secp256r1 (P-256), and secp384r1 (P-384) are supported. The following are not implemented:
  - P-521 (secp521r1)
  - Brainpool curves (brainpoolP256r1, brainpoolP384r1, brainpoolP512r1)
  - Legacy curves (sect233k1, sect283k1, secp224r1, etc.)

- **Signature algorithms** - Only RSA-PSS, ECDSA-P256, and ECDSA-P384 are supported. Not implemented:
  - Ed25519, Ed448
  - DSA
  - RSA-PKCS1 (deprecated in TLS 1.3 but still seen in some certificates)

## Acknowledgments

This project includes test files derived from the [OpenSSL](https://github.com/openssl/openssl) project:

- `test/ssl-tests/` - TLS test configuration files
- `test/certs/openssl/` - Test certificates

These files are used under the Apache License 2.0. Copyright (c) OpenSSL Project Authors.

This project also includes test key material derived from the [BoringSSL](https://boringssl.googlesource.com/boringssl) project:

- `test/certs/boringssl/` - BoringSSL test keys used by the shim and local tests

These files are used under the BoringSSL license. Copyright (c) BoringSSL Authors.

This project also includes test certificates from Google's [x509test](https://github.com/google/x509test) project:

- `test/certs/x509test/` - X.509 certificate validation test cases

These files are used under the Apache License 2.0. Copyright (c) Google Inc.

## License

MIT License

Copyright (c) 2026 Anthony Green <green@moxielogic.com>

## See Also

- [RFC 8446](https://tools.ietf.org/html/rfc8446) - TLS 1.3 specification
- [FIPS 203](https://csrc.nist.gov/pubs/fips/203/final) - ML-KEM (Module-Lattice-Based Key-Encapsulation Mechanism)
- [RFC 8555](https://tools.ietf.org/html/rfc8555) - ACME protocol specification
- [RFC 8737](https://tools.ietf.org/html/rfc8737) - TLS-ALPN-01 challenge
- [Let's Encrypt](https://letsencrypt.org/) - Free, automated certificate authority
- [cl+ssl](https://github.com/cl-plus-ssl/cl-plus-ssl) - OpenSSL-based TLS for Common Lisp
