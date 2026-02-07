# pure-tls 1.5.0 Release Notes

**Release Date:** January 2026

## Summary

This release adds an ACME client for automatic certificate management (Let's Encrypt), Hunchentoot web server integration, secp384r1 (P-384) curve support, strict X.509 certificate validation, and extensive RFC compliance improvements identified through BoringSSL and TLS-Anvil test suites.

## New Features

### ACME Client for Automatic Certificate Management

New `pure-tls/acme` system providing automatic certificate provisioning:

- Full ACME v2 protocol support (RFC 8555) for Let's Encrypt and compatible CAs
- HTTP-01 challenge solver with automatic validation
- Certificate storage with automatic renewal before expiry
- Thread-safe design for concurrent certificate operations
- CSR generation with proper ASN.1 encoding

### Hunchentoot Integration

New `pure-tls/acme+hunchentoot` system for seamless web server TLS:

- Drop-in TLS 1.3 support for Hunchentoot web servers
- Automatic certificate management with Let's Encrypt
- No OpenSSL dependency required

### secp384r1 (P-384) Elliptic Curve Support

- Added NIST P-384 curve for key exchange
- Complements existing X25519 and secp256r1 support
- Enables compliance with stricter security policies requiring 384-bit curves

### DoS Protection

- Configurable maximum send fragment size via `:max-send-fragment`
- Protects against resource exhaustion from oversized records

### Strict X.509 Certificate Validation

- Full RFC 5280 compliance for certificate path validation
- X.690 DER encoding validation
- Key usage and extended key usage enforcement
- Certificate chain verification improvements
- New `:skip-hostname-verify` option for certificate chain validation

## Protocol Improvements

### HelloRetryRequest (HRR)

- Server-side HRR support for key share negotiation
- Client-side HRR validation per RFC 8446
- Correct ECDH x-coordinate extraction per RFC 8446 Section 7.4.2

### QUIC Transport Parameters

- Proper handling of QUIC transport parameters extension (57)
- ALPS extension now ignored per RFC 8446 (previously rejected)

### Alert Handling

- Flush `close_notify` alert before closing socket
- Improved alert sending for protocol errors

## Test Infrastructure

### x509test Integration

- Added x509test certificate validation test suite
- Validates certificate parsing and chain verification against known-bad certificates

### BoringSSL Test Improvements

- Regression tracking with CI integration (`track-regressions.sh`)
- Improved test result categorization and reporting
- Many protocol validation fixes identified through BoringSSL's ProtocolBugs tests

### TLS-Anvil Fixes

- Fixed client test failures for improved interoperability testing

## RFC Compliance Fixes

### Extension Validation (RFC 8446 Section 4.2)

- Per-certificate extension validation
- TLS 1.2-only extension validation for TLS 1.3 connections
- Allow TLS 1.2 extensions in ClientHello per RFC 8446

### ALPN Validation (RFC 7301)

- Strict ALPN extension validation
- Proper error handling for ALPN negotiation failures

### Cryptographic Validation

- Ed25519/Ed448 OID support
- Key usage validation for CertificateVerify signatures
- ECDSA curve validation to prevent cross-curve attacks
- Session ticket validation per RFC 8446

### Record Layer (RFC 8446 Section 5)

- Enhanced record layer validation
- Proper handling of unexpected record types in post-handshake data

### Error Messages

- BoringSSL-compatible error message formats for test compatibility
- Improved decode error messages with `:DECODE_ERROR:` prefix
- Better error messages for extension, curve, and validation failures

## Bug Fixes

- Fixed `with-tls-*-stream` macro body expansion
- Fixed RSA signature algorithm selection for standard keys
- Fixed paren scoping bug in `process-client-hello` causing unbound EXTENSIONS
- Fixed empty string crash in shim argument parsing
- Fixed test synchronization race condition on macOS/Windows
- Fixed Windows CRLF line ending issue in OpenSSL test parser

## Installation

Using [ocicl](https://github.com/ocicl/ocicl):

```bash
ocicl install pure-tls
```

## Running Tests

```bash
# Run unit and network tests
make test

# Run BoringSSL test suite (requires BoringSSL checkout)
make boringssl-tests

# Run x509test certificate validation tests
sbcl --eval '(asdf:load-system :pure-tls/test)' \
     --eval '(fiveam:run! '\''pure-tls/test::x509test-tests)'
```

## Upgrade Notes

This release is backwards compatible. New features are opt-in:

1. **ACME Client** - Load `pure-tls/acme` system to enable automatic certificate management
2. **Hunchentoot** - Load `pure-tls/acme+hunchentoot` for web server integration
3. **P-384** - Automatically negotiated when supported by peer
4. **Stricter validation** - Some previously tolerated malformed messages may now be rejected

### New Dependencies

The ACME subsystems add dependencies on:
- `drakma` - HTTP client for ACME API
- `cl-json` - JSON parsing for ACME protocol
- `hunchentoot` - Web server (for `pure-tls/acme+hunchentoot` only)
