# pure-tls 1.11.2

**Release date:** 2026-06-21

Security patch release hardening certificate ExtendedKeyUsage
enforcement and the Encrypted Client Hello (ECH) configuration parser.

## Security Fixes

### CL-SEC-2026-0207 — ExtendedKeyUsage not enforced during chain verification (LOW)

`verify-certificate-chain` validated dates, names, BasicConstraints,
key usage, path length, and signatures, but never inspected the
ExtendedKeyUsage (EKU) extension. A leaf certificate restricted to a
different purpose (for example `clientAuth` only) was accepted as a TLS
server certificate, removing a relied-upon technical constraint on
delegated and purpose-limited certificates.

**Fix:** `verify-certificate-chain` now takes a `:purpose` keyword. The
TLS client path requests `:server-auth` and the server path requests
`:client-auth`; a leaf whose EKU extension is present but lists neither
the requested purpose nor `anyExtendedKeyUsage` is rejected. Per RFC
5280, a certificate with no EKU extension remains unrestricted.

### CL-SEC-2026-0206 — Out-of-bounds read parsing a hostile ECHConfig (LOW)

The ECH configuration parser read attacker-controlled length fields and
sliced the input before bounds-checking them, so a malformed
`ECHConfigList` (for example an oversized `public_key` length) raised an
uncaught, non-TLS Lisp error. A malicious server could abort a client's
handshake with a crafted EncryptedExtensions message.

**Fix:** ECHConfig structures are now parsed through the bounds-checked
buffer readers, so malformed input signals a graceful `tls-decode-error`
instead of an uncaught condition.

## Acknowledgments

Security issues identified by the [CL-SEC initiative](https://cl-sec.github.io/cl-sec-advisories/).
