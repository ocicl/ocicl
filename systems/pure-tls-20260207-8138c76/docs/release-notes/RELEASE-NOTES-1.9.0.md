# pure-tls 1.9.0 Release Notes

**Release Date:** January 2026

## Summary

This release adds the `:trust-anchor-mode` parameter for consistent trust anchor behavior across platforms, implements Windows custom chain engine support for exclusive trust anchors, and adds revocation checking for macOS Security.framework.

## New Features

### Trust Anchor Mode

A new `:trust-anchor-mode` parameter provides consistent control over how custom trust anchors interact with system certificate stores:

- **`:replace`** - Use ONLY the provided trusted roots, ignoring the system store
- **`:extend`** - Use provided roots IN ADDITION TO system roots (default)

This parameter is supported across all platforms:
- **macOS**: Uses `SecTrustSetAnchorCertificatesOnly`
- **Windows**: Creates a custom chain engine with `CertCreateCertificateChainEngine` and `hExclusiveRoot`
- **Pure-Lisp**: Direct control over trust anchor set

### Windows Custom Chain Engine

Full `:replace` mode support on Windows using the CryptoAPI custom chain engine:

- Creates exclusive root store for custom trust anchors
- Uses `CERT_CHAIN_ENGINE_CONFIG` with `hExclusiveRoot` field (Windows 7+)
- Proper cleanup of chain engine and certificate stores

### macOS Revocation Checking

Added OCSP/CRL revocation checking support via Security.framework:

- Uses `SecPolicyCreateRevocation` with configurable methods
- Supports OCSP, CRL, or both (default)
- Network access required for revocation checks

### ACME Certificate Profile Support

The ACME client now supports certificate profiles for requesting specific certificate types from ACME servers that support this extension.

## Bug Fixes

- Fixed native verification to respect explicit `trusted-roots` parameter
- Fixed revocation checking pass-through to native verifiers
- Fixed self-signed certificate test to use pure-Lisp verification
- Skip macOS-incompatible tests (name-constraints-no-san, ct-permissive-with-scts)

## Installation

Using [ocicl](https://github.com/ocicl/ocicl):

```bash
ocicl install pure-tls
```

## Upgrade Notes

This release is backwards compatible. The new `:trust-anchor-mode` parameter defaults to `:extend`, preserving existing behavior.

Example usage:

```lisp
;; Use only custom CA, ignore system roots
(make-tls-client-stream socket
  :hostname "example.com"
  :trusted-roots (list my-ca-cert)
  :trust-anchor-mode :replace)

;; Use custom CA in addition to system roots (default)
(make-tls-client-stream socket
  :hostname "example.com"
  :trusted-roots (list my-ca-cert)
  :trust-anchor-mode :extend)
```
