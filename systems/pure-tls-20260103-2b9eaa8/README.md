# pure-tls

A pure Common Lisp implementation of TLS 1.3 (RFC 8446).

## Features

- **Pure Common Lisp** - No foreign libraries or OpenSSL dependency
- **TLS 1.3 only** - Modern, secure protocol with simplified handshake
- **Gray streams** - Seamless integration with existing I/O code
- **cl+ssl compatible** - Drop-in replacement API available
- **Native Windows trust store** - Uses Windows CryptoAPI for certificate validation

### Supported Cipher Suites

- `TLS_CHACHA20_POLY1305_SHA256` (0x1303) - Preferred for side-channel resistance
- `TLS_AES_256_GCM_SHA384` (0x1302)
- `TLS_AES_128_GCM_SHA256` (0x1301)

### Supported Key Exchange

- X25519 (Curve25519)
- secp256r1 (P-256)

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

### Basic HTTPS Client

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
  "Return certificate and key based on client-requested hostname."
  (cond
    ((string= hostname "site-a.example.com")
     (values (pure-tls:load-certificate-chain "/certs/site-a.pem")
             (pure-tls:load-private-key "/certs/site-a-key.pem")))
    ((string= hostname "site-b.example.com")
     (values (pure-tls:load-certificate-chain "/certs/site-b.pem")
             (pure-tls:load-private-key "/certs/site-b-key.pem")))
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

#### `make-tls-client-stream` (socket &key hostname context verify alpn-protocols close-callback external-format buffer-size)

Create a TLS client stream over a TCP socket.

- `socket` - The underlying TCP stream
- `hostname` - Server hostname for SNI and certificate verification
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
- `sni-callback` - Function called with client's requested hostname, returns (VALUES cert-chain private-key) or NIL
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

### macOS and Linux

On non-Windows platforms, pure-tls uses pure Lisp certificate verification
and automatically searches for CA certificates:

1. `SSL_CERT_FILE` environment variable
2. `SSL_CERT_DIR` environment variable
3. Platform-specific locations:
   - `/etc/ssl/certs/ca-certificates.crt` (Debian/Ubuntu)
   - `/etc/pki/tls/certs/ca-bundle.crt` (RHEL/CentOS)
   - `/etc/ssl/cert.pem` (macOS)
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
- [cffi](https://github.com/cffi/cffi) - Windows only, for CryptoAPI bindings

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

```lisp
(asdf:load-system :pure-tls/test)

;; Run all offline tests (crypto, record layer, handshake, certificates)
(pure-tls/test:run-tests)

;; Run network tests (requires internet)
(pure-tls/test:run-network-tests)
```

### Test Coverage

The test suite validates:

- **Cryptographic primitives**: HKDF (RFC 5869), AES-GCM, ChaCha20-Poly1305 (RFC 8439)
- **TLS 1.3 key schedule**: RFC 8448 test vectors for all key derivation steps
- **Record layer**: Header format, content types, AEAD nonce construction
- **X.509 certificates**: ASN.1 parsing, hostname verification, OID handling
- **Bundled bad certificates**: Offline tests using certificates from [badssl.com](https://github.com/chromium/badssl.com) (expired, self-signed, known malware CAs)
- **Live validation**: TLS 1.3 connections to major sites (Google, Cloudflare, GitHub, etc.)

### Individual Test Suites

```lisp
(pure-tls/test:run-crypto-tests)       ; Cryptographic primitives
(pure-tls/test:run-record-tests)       ; Record layer
(pure-tls/test:run-handshake-tests)    ; Key schedule, extensions
(pure-tls/test:run-certificate-tests)  ; X.509 parsing
(pure-tls/test:run-network-tests)      ; Network tests (requires internet)
```

## Limitations

- No 0-RTT early data

## License

MIT License

Copyright (c) 2026 Anthony Green <green@moxielogic.com>

## See Also

- [RFC 8446](https://tools.ietf.org/html/rfc8446) - TLS 1.3 specification
- [cl+ssl](https://github.com/cl-plus-ssl/cl-plus-ssl) - OpenSSL-based TLS for Common Lisp
