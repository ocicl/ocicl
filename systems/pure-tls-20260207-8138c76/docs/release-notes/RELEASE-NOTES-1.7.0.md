# pure-tls 1.7.0 Release Notes

**Release Date:** January 2026

## Summary

This release adds Encrypted Client Hello (ECH) support per RFC 9639, Ed448 signature algorithm support, Certificate Revocation List (CRL) checking, and fixes for the cl+ssl compatibility layer.

## New Features

### Encrypted Client Hello (ECH) - RFC 9639

pure-tls now supports ECH to protect the SNI (Server Name Indication) from network observers:

- **Full RFC 9639 compliance** - Implements the complete ECH specification
- **Automatic negotiation** - ECH is used when the server provides ECHConfig
- **GREASE support** - Sends GREASE ECH extensions when ECH is unavailable

### Ed448 Signature Algorithm

Added support for Ed448 signatures in certificate chains:

- **Ed448 signature verification** - Validates certificates signed with Ed448
- **TLS 1.3 integration** - Advertises ed448 in signature_algorithms extension

### Certificate Revocation List (CRL) Support

New CRL checking for enhanced certificate validation:

- **CRL parsing** - Full ASN.1 CRL parsing with signature verification
- **Revocation checking** - Validates certificates against CRLs
- **Multiple CRL support** - Handles certificate chains with multiple CRL distribution points

## Bug Fixes

### cl+ssl Compatibility Layer

- **Fixed drakma integration** - Resolved TLS connection failures when using pure-tls as a drop-in cl+ssl replacement
  - Fixed `stream-fd` to return the stream itself instead of extracting the file descriptor (avoids dual-buffering issues)
  - Fixed `close-callback` wrapper to handle arity mismatch between cl+ssl (0 args) and pure-tls (1 arg)

### Platform Fixes

- **Windows build fixes** - Updated BoringSSL baseline and fixed Windows-specific issues
- **BoringSSL interop fixes** - Fixed test regressions from post-quantum cryptography work

## Installation

Using [ocicl](https://github.com/ocicl/ocicl):

```bash
ocicl install pure-tls
```

## Upgrade Notes

This release is backwards compatible. New features are automatically available:

1. **ECH** - Automatically used when server provides ECHConfig via DNS
2. **Ed448** - Automatically supported in certificate chains
3. **CRL** - Enable with `:check-crl t` in context options

The cl+ssl compatibility fixes ensure that applications using drakma or other cl+ssl-based HTTP clients work correctly with pure-tls.
