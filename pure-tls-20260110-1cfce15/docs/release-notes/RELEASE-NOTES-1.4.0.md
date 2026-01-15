# pure-tls 1.4.0 Release Notes

**Release Date:** January 2026

## Summary

This release adds mutual TLS (mTLS) client certificate support, comprehensive test suite integration with BoringSSL and OpenSSL, and numerous RFC 8446 compliance fixes identified through rigorous protocol testing.

## New Features

### mTLS Client Certificate Authentication

- `make-tls-client-stream` now accepts `:client-certificate` and `:client-key` parameters
- Supports file paths (PEM format) or pre-loaded certificate/key objects
- Certificate chain is automatically split: first cert as client cert, remainder as chain
- Server can request or require client certificates via `:verify` mode

### SNI Hostname Rejection

- SNI callback can return `:reject` to abort handshake with `unrecognized_name` alert
- New `:sni-hostname` parameter for client-side SNI without hostname verification
- Enables server-side virtual hosting with strict hostname policies

### GREASE Support (RFC 8701)

- Server now sends GREASE extension in NewSessionTicket messages
- Improves interoperability with clients that validate GREASE handling

## Test Infrastructure

### BoringSSL Integration

- Full shim binary (`pure-tls-shim`) for BoringSSL's Go test runner
- **65.4% pass rate** (4274 passed, 2259 failed out of 6533 tests)
- Failures are expected: ~35% of tests target TLS 1.2 which pure-tls does not implement (TLS 1.3 only)
- Remaining failures are unimplemented optional features (ALPS, 0-RTT, peek)
- Validates protocol compliance against 300+ edge cases from ProtocolBugs

### OpenSSL Test Framework

- INI-style configuration parser for OpenSSL `.cnf` test files
- 13 test suites integrated with FiveAM
- **100% pass rate** on all 32 enabled TLS 1.3 tests
- Covers: basic handshakes, ALPN, SNI, key update, curves, compression, client auth

## RFC Compliance Fixes

### Alert Handling (RFC 8446 Section 6)

- Invalid alert levels (not 1 or 2) now rejected with `illegal_parameter`
- Double/oversized alert records (> 2 bytes) rejected with `decode_error`
- Unknown alert types rejected with `illegal_parameter`
- Warning alerts (except `close_notify` and `user_canceled`) now rejected per TLS 1.3

### Record Layer (RFC 8446 Section 5)

- Invalid content types (outside 20-24) rejected immediately
- Prevents SSLv2 ClientHello hangs by validating content type before reading length
- Inner plaintext size validation for padded records
- Record size limit corrected to 16640 bytes (2^14 + 256)

### Handshake (RFC 8446 Section 4)

- KeyUpdate validation: sends `illegal_parameter` for unknown request modes
- Compression method validation: requires `legacy_compression_methods` to be `[0]`
- CertificateVerify transcript ordering corrected
- Handshake message reassembly across multiple TLS records

### Error Codes

Added BoringSSL-compatible error code prefixes for test compatibility:
- `:TLSV1_ALERT_RECORD_OVERFLOW:`
- `:UNEXPECTED_RECORD:`
- `:BAD_ALERT:`
- `:INVALID_COMPRESSION_LIST:`
- `:UNKNOWN_ALERT_TYPE:`

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

# Run OpenSSL-adapted tests
sbcl --eval '(asdf:load-system :pure-tls/test)' \
     --eval '(fiveam:run! '\''pure-tls/test::openssl-tests)'
```

## Upgrade Notes

This release is backwards compatible. New features are opt-in:

1. **mTLS** - Existing client code continues to work; add `:client-certificate` and `:client-key` to enable client auth
2. **SNI rejection** - Existing SNI callbacks returning `NIL` continue to use default certificate
3. **Stricter validation** - Some malformed TLS messages that were previously tolerated may now be rejected

Applications connecting to non-compliant servers may need to handle new alert conditions.
