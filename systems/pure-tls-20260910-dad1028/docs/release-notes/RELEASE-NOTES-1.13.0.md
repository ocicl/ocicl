# pure-tls 1.13.0

**Release date:** 2026-07-25

Feature and hardening release. TLS clients can opt into a stricter RFC 6125
hostname-verification profile, and the ACME client is substantially more
robust: it recovers automatically from transient CA errors, stops leaking
the account contact address to logs, and can present the `tls-alpn-01`
challenge certificates it generates. The record and handshake layers are
also hardened against a memory-amplification denial of service and trimmed
of per-record allocation on the steady-state data path.

The hostname-policy and ACME robustness work was contributed by Brian
O'Reilly (@fade).

## New features

- **Opt-in hostname-verification policy** (#16). `make-tls-context`
  accepts a `:hostname-policy` argument carrying two orthogonal RFC 6125
  knobs: `:allow-wildcards` (when `nil`, wildcard-pattern `*.` SANs are
  excluded from matching) and `:allow-cn-fallback` (when `nil`, a
  certificate with no subjectAltName is rejected rather than matched
  against its Subject Common Name). Both default to the permissive
  value — `*general-hostname-policy*`, the general web profile — so
  existing callers see no behavior change. `verify-hostname` and
  `verify-peer-certificate` take the policy as a keyword argument.
  Embedded-NUL/non-LDH name rejection and IP-literal handling remain
  unconditional under every policy. New exports: `hostname-policy`,
  `make-hostname-policy`, `hostname-policy-allow-wildcards`,
  `hostname-policy-allow-cn-fallback`, `*general-hostname-policy*`.

- **ACME transient-error recovery via the condition system** (#17).
  Every ACME HTTP request now flows through a single recovery layer:
  recoverable responses signal typed conditions (`acme-http-error` and
  subtypes `acme-bad-nonce`, `acme-rate-limited`, `acme-not-ready`)
  offering a `retry` restart that re-drives the request without
  unwinding the stack — refreshing the nonce for `badNonce` (RFC 8555
  §6.5), or waiting per `Retry-After` for `429`/`202`. Recovery is
  bounded by both a retry count and a total-wait ceiling. The exported
  `with-acme-retries` macro lets an issuance driver place one policy
  around a whole issuance.

  This fixes a real failure mode: `client-download-certificate` issued
  its POST-as-GET outside the retried request path, so a `badNonce` at
  the download step silently collapsed an already-issued certificate to
  `nil`. Validated against Pebble with server-side `badNonce` injection:
  zero silent drops after the change. Public return contracts are
  unchanged for success and terminal responses.

## Security

- **Harden handshake reassembly against oversized fragmented messages**
  (#20). A peer controls the uint24 length in the handshake message
  header and could force pure-tls to buffer up to 16 MiB per message via
  many record-sized fragments, with O(n²) copy work from repeated
  full-buffer concatenation — roughly 8 GiB of aggregate copying for a
  single maximal message. Following BoringSSL's
  `tls_can_accept_handshake_data` pattern, an excessive advertised length
  is now rejected as soon as the 4-byte header is visible — before any
  further fragments are buffered — with a fatal `illegal_parameter`
  alert. Non-certificate messages are capped by the new exported
  `*max-handshake-message-size*` (default 16384); Certificate messages
  honor `*max-certificate-list-size*` plus framing overhead. Reassembly
  now uses a geometrically grown fill-pointer vector, so each fragment is
  copied O(1) times instead of re-copying the accumulated buffer per
  fragment.

- **Stop logging the ACME account contact address** (#18).
  `client-register-account` logged the contact address verbatim at
  `:info` before the registration POST, so the address reached every
  configured log sink — files, syslog, and any downstream aggregator.
  The registration event is now logged without the address; the address
  is still carried, unchanged, in the `contact` field of the registration
  POST body in `mailto:` form as RFC 8555 §7.3 requires.

## Bug fixes

- **Accept the RFC 8737 `id-pe-acmeIdentifier` critical extension** (#19).
  An ACME `tls-alpn-01` challenge certificate carries the
  key-authorization digest in a critical extension at OID
  `1.3.6.1.5.5.7.1.31`. The X.509 parser rejected that OID as an unknown
  critical extension, so a pure-Lisp TLS server could not present the very
  challenge certificate the library's own ACME path generates. The OID is
  now registered as `:acme-identifier` and admitted to the recognized
  critical-extension set. The allowance is scoped to this single OID:
  every other unrecognized critical extension is still rejected, so
  RFC 5280 §4.2 enforcement is unchanged for all non-ACME certificates.

## Performance

- **Reduce per-record consing on the data path.** Several
  full-record-sized allocations were removed from the steady-state
  encrypt/decrypt hot paths. On the read side, AES-GCM and
  ChaCha20-Poly1305 now decrypt in place and ChaCha computes its Poly1305
  tag incrementally, and `tls13-decrypt-record` decrypts into a pooled
  scratch buffer (wiped after use) so the trimmed plaintext is the only
  per-record heap allocation. On the write side, the pending payload is no
  longer copied out on flush; bounds are threaded through the record-layer
  write path instead. External contracts are unchanged.

## Testing

- New `pure-tls/acme/test` system (fiveam) covering nonce
  refresh-and-retry with JWS re-signing, `Retry-After` waits, the
  certificate-download `badNonce` path, bounded retries under a
  persistently hostile server, non-retry of terminal errors, and the
  no-contact-address-in-logs assertion. The HTTP transport and sleep are
  stubbed via seams, so no network is needed.
- New handshake-buffer security-regression tests proving the
  non-certificate and Certificate length caps, acceptance at the cap,
  fragmented reassembly, and in-place buffer growth.
- New hostname-policy security-regression tests proving each policy knob
  independently and that the general RFC 6125 matcher is unchanged.
- Static X.509 fixtures for both `id-pe-acmeIdentifier` directions: a real
  `tls-alpn-01` challenge certificate that must parse, and an unrelated
  critical extension that must still fail.
