# pure-tls 1.11.3

**Release date:** 2026-06-21

Conformance and CI hardening release.

## Hardening

- Strictly validate several TLS 1.3 handshake extensions and reject
  malformed encodings with a `decode_error` alert, per RFC 8446:
  - `server_name`: reject trailing data after the ServerNameList and
    malformed ServerName entries.
  - `certificate_authorities`: reject trailing data after the
    DistinguishedName list and reject an empty list.

  These are parsed from untrusted peer handshake messages; previously the
  surplus/invalid bytes were ignored rather than rejected. No security
  impact is known (the extra bytes were never acted upon and the TLS 1.3
  transcript hash binds the exact bytes exchanged), so this is a
  robustness/conformance improvement rather than a security advisory.

## Testing / CI

- Pinned the BoringSSL test suite to a fixed upstream commit so the
  conformance baseline is reproducible (BoringSSL is unversioned and adds
  tests continuously, which otherwise reports new upstream tests as
  spurious regressions). Refreshed `test/boringssl-baseline.txt` to match.
- Added a security-regression test suite covering the fixes shipped in
  1.11.2 (EKU enforcement, ECH config bounds checking).
