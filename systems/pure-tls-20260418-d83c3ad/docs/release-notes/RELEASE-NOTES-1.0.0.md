# pure-tls 1.0.0 Release Notes

**Release Date:** January 2026

## Summary

Initial release of pure-tls, a pure Common Lisp implementation of TLS 1.3 (RFC 8446).

## Features

### Core TLS 1.3 Support
- **Pure Common Lisp** - No foreign libraries or OpenSSL dependency
- **TLS 1.3 only** - Modern, secure protocol with simplified handshake
- **Gray streams** - Seamless integration with existing I/O code
- **Client and server support** - Full bidirectional TLS connections
- **cl+ssl drop-in replacement** - Use with existing cl+ssl-based libraries (drakma, dexador, etc.)

### Cipher Suites
- `TLS_CHACHA20_POLY1305_SHA256` (0x1303) - Preferred for side-channel resistance
- `TLS_AES_256_GCM_SHA384` (0x1302)
- `TLS_AES_128_GCM_SHA256` (0x1301)

### Key Exchange
- X25519 (Curve25519)
- secp256r1 (P-256)

### Certificate Handling
- X.509 certificate parsing (DER and PEM formats)
- Certificate chain verification
- Hostname verification (including wildcards)
- Automatic system CA certificate discovery (Linux, macOS, Windows)
- Subject Alternative Names (SAN) support

### Server Features
- SNI (Server Name Indication) callback for virtual hosting
- Client certificate authentication (mTLS)
- Configurable verification modes

### Session Resumption
- PSK-based session resumption via NewSessionTicket
- Automatic ticket caching (client-side)
- Forward secrecy maintained via (EC)DHE

### Security Features
- Constant-time MAC verification
- Secret zeroization utilities
- TLS 1.3 record padding for traffic analysis mitigation
- SSLKEYLOGFILE support for Wireshark debugging

### cl+ssl Drop-in Replacement
- **Eliminate OpenSSL dependency** - Use pure-tls with existing cl+ssl-based code
- `pure-tls/cl+ssl-compat` provides the `CL+SSL` package with compatible API
- Works with libraries expecting cl+ssl (drakma, dexador, etc.)
- Use `asdf:register-immutable-system` to prevent loading real cl+ssl:
  ```lisp
  (asdf:load-system :pure-tls/cl+ssl-compat)
  (asdf:register-immutable-system "cl+ssl")
  ;; Now load your application - cl+ssl calls go to pure-tls
  ```

## API Highlights

### Stream Creation
```lisp
;; Client with automatic cleanup
(pure-tls:with-tls-client-stream (tls socket :hostname "example.com")
  (write-sequence data tls)
  (read-sequence buffer tls))

;; Server with SNI
(pure-tls:make-tls-server-stream socket
  :certificate "/path/to/cert.pem"
  :key "/path/to/key.pem"
  :sni-callback #'select-certificate)
```

### Verification Modes
- `+verify-none+` - No certificate verification
- `+verify-peer+` - Verify peer certificate if provided
- `+verify-required+` - Require and verify peer certificate

## Dependencies

- ironclad - Cryptographic primitives
- trivial-gray-streams - Gray stream support
- flexi-streams - Character encoding
- alexandria - Utilities
- cl-base64 - Base64 encoding

## Test Coverage

- RFC 5869 HKDF test vectors
- RFC 8448 TLS 1.3 key schedule test vectors
- RFC 8439 ChaCha20-Poly1305 test vectors
- X.509 certificate parsing and validation
- Bundled bad certificates from badssl.com for offline testing
- Network tests against major TLS 1.3 sites

## Known Limitations

- No 0-RTT early data support
- TLS 1.3 only (no fallback to TLS 1.2)

## Installation

Using [ocicl](https://github.com/ocicl/ocicl):

```bash
ocicl install pure-tls
```
