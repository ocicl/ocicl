# pure-tls 1.8.0 Release Notes

**Release Date:** January 2026

## Summary

This release adds ML-DSA-65 post-quantum digital signature support (FIPS 204) and fixes HelloRetryRequest extension validation per RFC 8446 and RFC 9639.

## New Features

### ML-DSA-65 Post-Quantum Signatures (FIPS 204)

pure-tls now supports ML-DSA-65 (Module-Lattice Digital Signature Algorithm), providing quantum-resistant digital signatures:

- **Full FIPS 204 compliance** - Implements the complete ML-DSA-65 specification
- **Key generation** - Generate ML-DSA-65 key pairs from seed
- **Sign and verify** - Create and validate post-quantum signatures
- **TLS 1.3 ready** - Prepared for post-quantum certificate authentication

Combined with the existing X25519MLKEM768 key exchange, pure-tls now offers both post-quantum key exchange and post-quantum signatures.

## Bug Fixes

### HelloRetryRequest Extension Validation

- **RFC 8446 compliance** - HRR now correctly rejects unknown extensions with `unsupported_extension` alert
- **RFC 9639 compliance** - ECH extension in HRR is only accepted when the client offered ECH
- **Improved interoperability** - Passes BoringSSL regression test suite

## Installation

Using [ocicl](https://github.com/ocicl/ocicl):

```bash
ocicl install pure-tls
```

## Upgrade Notes

This release is backwards compatible. ML-DSA-65 functions are available in the `pure-tls` package:

- `ml-dsa-65-keygen` - Generate key pair from 32-byte seed
- `ml-dsa-65-sign` - Sign a message
- `ml-dsa-65-verify` - Verify a signature
