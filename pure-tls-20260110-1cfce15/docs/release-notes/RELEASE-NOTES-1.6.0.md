# pure-tls 1.6.0 Release Notes

**Release Date:** January 2026

## Summary

This release adds **post-quantum cryptography support** with X25519MLKEM768 hybrid key exchange, implementing ML-KEM-768 per FIPS 203. This protects TLS connections against future quantum computer attacks using a hybrid design that combines classical X25519 with the lattice-based ML-KEM algorithm.

## New Features

### X25519MLKEM768 Post-Quantum Hybrid Key Exchange

pure-tls now supports post-quantum key exchange using the X25519MLKEM768 hybrid algorithm:

- **ML-KEM-768 implementation** - Full FIPS 203 compliant lattice-based key encapsulation
- **Hybrid design** - Combines X25519 (classical) with ML-KEM-768 (post-quantum)
- **Automatic negotiation** - Preferred key exchange when both peers support it
- **Browser compatible** - Interoperates with Chrome 124+ and other modern clients

```lisp
;; Post-quantum is automatic - no configuration needed
(pure-tls:make-tls-client-stream stream :hostname "example.com")
```

### Chrome Interoperability Test Server

New test infrastructure for validating post-quantum key exchange with browsers:

```bash
cd test/chrome-interop
./generate-localhost-cert.sh
sbcl --load chrome-server.lisp
# Open Chrome to https://localhost:8443/
```

### FIPS 203 Known Answer Tests

ML-KEM-768 implementation validated against all 1000 official NIST test vectors:

```bash
curl -sL https://raw.githubusercontent.com/post-quantum-cryptography/KAT/main/MLKEM/kat_MLKEM_768.rsp \
     -o test/vectors/kat_MLKEM_768.rsp

sbcl --eval '(asdf:load-system :pure-tls)' \
     --load test/ml-kem-kat.lisp \
     --eval '(ml-kem-kat:run-tests)'
```

## Security Improvements

### Constant-Time ML-KEM Operations

All ML-KEM modular arithmetic uses constant-time implementations:

- **Barrett reduction** - Constant-time modular reduction with correct v=20158
- **Conditional subtraction** - Branch-free mod q operations
- **NTT operations** - Constant-time Number Theoretic Transform
- **Implicit rejection** - CCA-secure decapsulation with pseudorandom failure output

### Hardened Rejection Sampling

- Fixed buffer bounds checking in polynomial sampling
- Explicit error on insufficient samples (defense in depth)
- Coefficient canonicalization for malformed input defense

## Protocol Details

### Key Share Sizes

| Direction | X25519 | X25519MLKEM768 |
|-----------|--------|----------------|
| Client → Server | 32 bytes | 1216 bytes |
| Server → Client | 32 bytes | 1120 bytes |

### Shared Secret Composition

The hybrid shared secret is computed as:
```
shared_secret = ML-KEM-shared-secret || X25519-shared-secret
```

This ensures security even if one algorithm is compromised.

### IANA Codepoint

X25519MLKEM768 uses IANA registered codepoint `0x11EC` (4588).

## Test Results

- **FIPS 203 KAT**: 1000/1000 decapsulation tests pass
- **FIPS 203 KAT**: 1000/1000 implicit rejection tests pass
- **Unit tests**: All 232+ tests pass
- **OpenSSL interop**: All tests pass
- **Chrome interop**: Verified with Chrome 124+

## Installation

Using [ocicl](https://github.com/ocicl/ocicl):

```bash
ocicl install pure-tls
```

## Upgrade Notes

This release is backwards compatible. Post-quantum key exchange is:

1. **Automatic** - Negotiated when both peers support it
2. **Preferred** - Listed first in supported groups
3. **Hybrid** - Falls back to X25519 if peer doesn't support post-quantum

No configuration changes are required. Existing applications automatically gain post-quantum protection when connecting to compatible peers.

### Performance Considerations

ML-KEM operations are more computationally intensive than classical key exchange:
- Key generation: ~1ms additional
- Encapsulation/Decapsulation: ~0.5ms additional
- Key share size: ~1KB additional per direction

For most applications, this overhead is negligible compared to network latency.

## References

- [FIPS 203](https://csrc.nist.gov/pubs/fips/203/final) - ML-KEM Standard
- [IETF Draft](https://datatracker.ietf.org/doc/draft-kwiatkowski-tls-ecdhe-mlkem/) - X25519MLKEM768 in TLS
- [NIST KAT Vectors](https://github.com/post-quantum-cryptography/KAT) - Test vectors
