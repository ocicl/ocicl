# pure-tls 1.3.0 Release Notes

**Release Date:** January 2026

## Summary

This release addresses multiple security audit findings, significantly hardening X.509 certificate validation, key exchange, hostname verification, and record layer compliance.

## Security Fixes

### Critical

- **X25519 all-zero shared secret rejection** - Now rejects all-zero shared secrets per RFC 7748/8446, preventing small-subgroup attacks
- **P-256 ECDH point validation** - Validates peer public keys are on the secp256r1 curve, preventing invalid curve attacks

### High

- **X.509 chain validation hardened** - Enforces BasicConstraints CA flag, KeyUsage (keyCertSign), pathLenConstraint, and rejects unknown critical extensions per RFC 5280
- **Hostname verification improved** - Rejects overly broad wildcards (e.g., `*.com`), supports IP address SANs, and normalizes IDNA/punycode hostnames
- **SHA-1 certificate signatures rejected** - SHA-1 is cryptographically broken and no longer accepted for certificate signatures
- **PKCS#1 v1.5 padding hardened** - Enforces minimum 8-byte 0xFF padding and exact DigestInfo length per RFC 8017
- **AES-GCM key validation** - Rejects invalid key lengths; removed dead cipher selection code
- **RSA-PSS parameters enforced** - Parses and validates hash algorithm and salt length from signature parameters

### Medium

- **Low-level API bypass prevented** - `perform-client-handshake` now enforces certificate chain and hostname verification when `verify-mode` requires it
- **Server mTLS trust-store required** - Server now fails fast if client certificate verification is enabled but no trust store is configured
- **PSK binder truncation fixed** - Server-side PSK binder verification now correctly includes the 2-byte binders list length prefix

### Low

- **TLS record length limit corrected** - Updated from 16640 to 16656 bytes per RFC 8446 Section 5.2
- **legacy_record_version validated** - Rejects records with invalid version fields (must be 0x0301 or 0x0303)

## New Dependencies

- `idna` - For internationalized domain name (punycode) normalization in hostname verification

## API Changes

### New Constants
- `+tls-1.0+` (#x0301) - TLS 1.0 version identifier for legacy_record_version validation

### Behavioral Changes
- `verify-hostname` now normalizes hostnames to ASCII/punycode before comparison
- `verify-certificate-chain` enforces RFC 5280 constraints (BasicConstraints, KeyUsage, pathLen, critical extensions)
- `perform-client-handshake` performs full verification during handshake, not just in stream wrapper
- Server handshake requires trust-store when `verify-mode` is `+verify-peer+` or `+verify-required+`

## Installation

Using [ocicl](https://github.com/ocicl/ocicl):

```bash
ocicl install pure-tls
```

## Upgrade Notes

This release contains breaking behavioral changes for security. Applications that previously:

1. **Used `perform-client-handshake` directly** - Will now get certificate verification during the handshake (previously only in `make-tls-client-stream`)
2. **Configured server mTLS without a trust-store** - Will now fail with an error instead of silently accepting any client certificate
3. **Connected to servers with SHA-1 signed certificates** - Will now fail certificate verification
4. **Connected to servers with overly broad wildcard certificates** - May fail hostname verification

These changes improve security posture but may require configuration updates in some deployments.
