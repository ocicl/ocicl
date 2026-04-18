# pure-tls 1.11.1

**Release date:** 2026-04-18

Security patch release fixing two vulnerabilities in certificate chain
verification and constant-time comparison.

## Security Fixes

### CL-SEC-2026-0201 — Certificate chain trust anchor verified by name only (HIGH)

Trust-anchor matching accepted a certificate chain if any chain
member's issuer distinguished name matched a trusted root, without
verifying the cryptographic signature. An attacker could forge an
intermediate with a spoofed issuer DN and have it accepted as trusted.

**Fix:** `verify-certificate-chain` now requires
`verify-certificate-signature` to succeed against the trust anchor,
not just `certificate-issued-by-p` (name equality).

### CL-SEC-2026-0202 — Constant-time comparison false equality on length differences (HIGH)

`ct-equal-mask` masked the length XOR to 8 bits before accumulation,
so inputs differing in length by a multiple of 256 compared as equal
when their shared prefix matched. This function is used in ML-KEM-768
decapsulation for implicit rejection (FIPS 203).

**Fix:** The full fixnum length XOR is now folded across all bytes
before the mask derivation, ensuring any non-zero length difference
produces a "not equal" result.

## Additional Fixes

- Post-handshake messages now use a reassembly buffer to correctly
  handle TLS 1.3 message fragmentation and coalescing across records.
- Wildcard hostname validation rejects known multi-label public
  suffixes (e.g., `*.co.uk`).

## Acknowledgments

Security issues identified by the [CL-SEC initiative](https://github.com/CL-SEC/CL-SEC).
