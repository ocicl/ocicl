# Changelog

All notable changes to pure-tls are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

Entries before this file existed were migrated from the per-release notes that
used to live in `docs/release-notes/`; their wording is preserved, reorganised
under the Keep a Changelog headings. One addition to the standard set: a
`Notes` heading carries per-release material that is not itself a change —
known limitations, references, and acknowledgements.

## [Unreleased]

## [1.15.1] - 2026-09-25

Patch release, fixing two bugs on the cl+ssl compatibility path. Between them
they left a cl+ssl or drakma caller unable to open a verifying connection at
all unless it named a CA file explicitly; ocicl hit this as `ocicl update`
failing on every platform while `ocicl list` worked.

### Fixed

- A stream asked to verify against a context whose own verify mode is
  `+verify-none+` now falls back to the system trust store instead of reaching
  the handshake with no roots and failing every chain with `UNKNOWN-CA`. This
  is the shape cl+ssl callers take: the verify decision travels with the
  stream, not the context, and drakma builds its context with
  `+ssl-verify-none+` before asking `make-ssl-client-stream` to verify. Any
  cl+ssl consumer that did not name an explicit CA file was affected; naming
  one took a different branch and worked, so the same program could succeed on
  one code path and fail on another. `make-tls-context` keeps its existing
  policy of not auto-loading roots for a `+verify-none+` context.
- `make-tls-client-stream` and `make-tls-server-stream` treat an explicit
  `:context nil` as "use the default context". The default applied only when
  the argument was left out, so a caller passing NIL — as the cl+ssl layer does
  until `ensure-initialized` has run — had its context slots read off NIL and
  got a `TYPE-ERROR` from inside the handshake rather than from the call that
  was wrong.

## [1.15.0] - 2026-09-17

Security release, from a triage of the TLS stack.

Two remote denial-of-service flaws are fixed, both of which killed the whole
Lisp image rather than the connection: unbounded recursion in the DER parser,
reachable from a peer certificate before any signature is checked, and
unbounded recursion in the post-handshake record loop. A third flaw let a peer
spend hundreds of CPU-seconds of a server's time on ML-DSA certificate chain
verification. Separately, the cl+ssl compatibility layer was silently
discarding `:verify-callback` — the hook applications use for certificate
pinning — and an empty client trust store did not fail closed under mTLS.

**Upgrade note.** Several options and inputs that were previously accepted and
quietly ignored now signal or are rejected. If you use the cl+ssl compatibility
layer with `:verify-callback`, `:pem-password-callback`,
`:private-key-password`, or `:certificate` / `:key` on
`make-ssl-client-stream`, those calls now raise an error instead of silently
doing nothing — the silence is what made them dangerous. Certificate chains are
now bounded by size (`*max-certificate-list-size*`, previously unlimited) and by
length (`verify-depth`, previously never enforced), so an unusually large or
deep chain that used to be accepted may now be rejected.

### Security

- **Two remote denial-of-service flaws, both fatal to the whole Lisp image
  rather than the connection.** Each was reproduced before being fixed.

  Unbounded recursion in the DER parser ([CL-SEC-2026-0216]): `parse-der-node` and
  `parse-der-contents` are mutually recursive with no depth bound, so a
  certificate of nested `SEQUENCE`s recursed once per level. Roughly 133KB
  exhausted a 1MB control stack, and on SBCL that abort is fatal and
  uncatchable — neither `process-certificate`'s `handler-case` nor a
  `serious-condition` handler could contain it. Parsing happens before any
  signature or chain verification, so no authentication was required to reach
  it. Nesting is now capped at `+asn1-max-depth+`.

  Unbounded recursion in the post-handshake record loop ([CL-SEC-2026-0217]):
  `tls-stream-fill-buffer` re-entered itself once per record, inside a
  `handler-case` that prevented a tail call. A peer dribbling handshake records
  one byte at a time exhausted the stack after roughly 259KB. The read path is
  now iterative.

- **CPU exhaustion via ML-DSA-65 certificate chains** ([CL-SEC-2026-0218]). Signature verification
  measured 77ms because polynomial multiplication was schoolbook O(n²). Every
  chain link is verified before the trust anchor is checked, so an unbounded
  `Certificate` message let one connection cost roughly 315 CPU-seconds.
  Replacing the multiply with an NTT and bounding the message brings that to
  about 0.12 CPU-seconds.

- **The cl+ssl compatibility layer silently discarded `:verify-callback`**
  ([CL-SEC-2026-0219]). In
  cl+ssl that callback is how an application implements certificate pinning or
  custom chain policy, so an application that pinned a CA through it and
  migrated here degraded to accepting any certificate from any CA in the system
  trust store, with no indication. It now signals, as do
  `:pem-password-callback`, `:private-key-password`, and `:certificate` /
  `:key` on `make-ssl-client-stream`. Options that are genuinely meaningless
  for a TLS-1.3-only stack still no-op silently, and that distinction is
  documented.

- **An empty client trust store did not fail closed under mTLS**
  ([CL-SEC-2026-0220]).
  `process-client-certificate-verify` checked only that a trust store was
  present, so an empty-but-non-`NIL` store reached `verify-certificate-chain`
  with no roots — and on macOS/Windows the native dispatch treats that as "use
  the OS trust store". A server meaning to accept clients under one private CA
  would have accepted any client certificate chaining to any publicly-trusted
  root. `make-trust-store-from-directory` returns an empty store without
  complaint, so this was reachable by ordinary misconfiguration.

- **The ACME TLS-ALPN-01 validation private key was written insecurely**
  ([CL-SEC-2026-0221]). It
  went to a constant filename in the shared temp directory, created at the
  process umask and only narrowed to `0600` after writing — a window in which a
  local user could read it, and a predictable path at which one could pre-place
  a symlink. It is now created with `O_EXCL` and mode `0600` at creation, under
  a filename carrying 128 bits of entropy.

- **`*session-ticket-cache*` was mutated without synchronization.** Its lock was
  defined and never used, while the cache is written from peer-driven
  NewSessionTicket processing. All three accessors now hold it.

- **The release workflow granted `write-all`** to an action referenced by a
  mutable tag. It is now `contents: write`, pinned to a commit SHA, and rejects
  a tag whose version is not `vMAJOR.MINOR.PATCH` (it is interpolated into a
  file path).

### Added

- `verify-depth` is now enforced. It was accepted by `make-tls-context`, stored,
  and never read by anything — no chain-length limit existed. It is checked
  before any signature verification, which matters because per-link
  verification runs before the trust-anchor check.
- `:cipher-suites` on `make-tls-client-stream` and `make-tls-server-stream`.
  Neither constructor passed the context's cipher suites to the handshake, so
  `tls-context-cipher-suites` was dead configuration on both paths.
- Constant-time codegen guards for ML-KEM. The division in `compress-coeff` (the
  KyberSlash shape) and the secret-dependent message-bit decode are
  constant-time on SBCL/x86-64 by the compiler's choice rather than by
  construction, so tests now fail if a hardware divide or a conditional jump
  ever appears.

### Changed

- ML-DSA-65 polynomial multiplication uses an NTT instead of schoolbook
  convolution: 77ms to 6.4ms per verification. The schoolbook implementation is
  retained as the reference, and a differential test asserts the two agree
  coefficient-for-coefficient — this suite has no FIPS 204 known-answer
  vectors, so that differential is what makes the substitution trustworthy.
- `*max-certificate-list-size*` now defaults to 102400 rather than 0
  (unlimited), matching OpenSSL's `SSL_CTX_set_max_cert_list` default. A peer
  could previously make us buffer 16MB before a single signature was checked.
- The context's default cipher suite list now matches the handshake's, in the
  same preference order. It listed only two suites in the opposite order, which
  went unnoticed because nothing read it.
- Pinned BoringSSL advanced from `606d3a3` (2026-06-18) to `8626998`
  (2026-09-17). The baseline shrank across the bump — 2211 to 2062 entries —
  with zero failures absorbed.

### Fixed

- **RSA-PSS certificate and CRL signatures could never verify.** Two
  independent defects on the same path, which is why the algorithm was entirely
  non-functional rather than subtly wrong: the message was pre-hashed before
  Ironclad hashed it again, and the bare digest OIDs were missing from the OID
  table so `hashAlgorithm` never resolved.
- **The SNI callback never ran when a default certificate was configured**, so
  virtual hosting was silently broken in the ordinary setup and the callback's
  `:reject` return never fired.
- PSK state was sticky across HelloRetryRequest, letting a ServerHello advertise
  an acceptance the key schedule never made.
- Two RFC 8446 MUST-aborts were missing: the client silently ignored a
  `pre_shared_key` it had not offered (§4.1.3), and the server accepted any
  group in a second ClientHello's `key_share` rather than the one named in the
  HelloRetryRequest (§4.1.4).
- Unknown post-handshake message types are rejected with `unexpected_message`
  per RFC 8446 §4.6 instead of being silently ignored.
- DER strictness: a child element may no longer overrun its parent's declared
  length; OID decoding handles multi-byte first subidentifiers and rejects
  non-minimal encodings; a critical extension with a zero-length OID is no
  longer treated as known.
- ML-DSA hint decoding enforces all three FIPS 204 Algorithm 15 checks; ML-KEM
  validates encapsulation keys per FIPS 203 §7.2; HPKE DHKEM rejects an
  all-zero shared secret per RFC 9180 §7.1.4.
- The ECH accept confirmation is derived with the negotiated cipher suite's
  hash rather than a hardcoded SHA-256, so ECH is no longer silently treated as
  rejected on every SHA-384 handshake.
- Odd-length `uint16` extension lists raise a TLS error instead of running off
  the end of the array with a condition that escaped the handshake's handlers.
- `CL:RANDOM` replaced with the CSPRNG for GREASE ECH values and ACME
  certificate serials.
- Removed `verify-peer-certificate`, which claimed to perform full verification
  but matched only the issuer distinguished name and never checked a signature.
  It was unexported and had no callers.

### Notes

The security regression suite grows from 61 to 93 checks, each asserting the
secure behaviour for a specific finding. Where a test could have passed
vacuously it was confirmed to fail against the pre-fix code.

The BoringSSL harness was repaired alongside this work: the runner's output
format had changed upstream and the scripts parsed only the old one, a
truncated run was indistinguishable from a complete one, and the `-curves`
parser kept only the last value and so skipped tests incorrectly.

## [1.14.0] - 2026-09-13

Feature and hardening release. The ACME client now implements ACME
Renewal Information (RFC 9773), so certificates renew adaptively inside
the CA's suggested window instead of on a fixed 30-day-before-expiry
schedule — essential now that Let's Encrypt profiles have moved to
45-day and shorter lifetimes. The TLS client greases its
`signature_algorithms` list, completing RFC 8701 GREASE coverage. And
`verify-certificate-chain` gained argument-type guards and a
keyword-argument signature that together eliminate a class of silent
misuse where a misplaced keyword disabled the very check the caller
asked for.

The `verify-certificate-chain` hardening was contributed by Brian
O'Reilly (@fade).

### Added

- **ACME Renewal Information (RFC 9773)**. Let's Encrypt's `tlsserver`
  profile moved to 45-day certificates in May 2026 and all profiles
  shorten further through 2027–2028, so the old fixed renew-at-30-days
  default (tuned for 90-day certificates) no longer fit any profile —
  on ~6-day short-lived certificates it renewed daily against a
  25-per-week rate limit. `acme/ari.lisp` adds the RFC 9773 machinery:
  the certificate identifier (base64url AKI keyIdentifier `.` base64url
  DER serial content octets, verified against the RFC's Appendix A
  vector), an RFC 3339 timestamp parser, an advisory
  `client-renewal-info` query that degrades to `nil` on any failure,
  and the `renewal-due-p` decision rule.

  The `acme-acceptor` now renews adaptively by default: inside the
  CA's suggested window when ARI is available, otherwise once a third
  of the certificate's lifetime remains (30 days on a 90-day
  certificate — exactly the old default). An explicit `:renewal-days`
  keeps the fixed threshold. Renewal orders carry the RFC 9773
  `replaces` identifier so the CA can exempt them from rate limits,
  retrying once without it if the server rejects the value; first
  issuance sends none because the startup placeholder certificate has
  no AKI. New exports: `client-renewal-info`,
  `certificate-ari-cert-id`, `renewal-due-p`, `parse-rfc3339-time`.

- **GREASE in `signature_algorithms` (RFC 8701)**. pure-tls greased
  extension types, cipher suites, versions, and named groups, but not
  the signature-algorithm list — a gap BoringSSL's runner now checks
  for. A GREASE value is generated once per handshake and repeated in
  the second ClientHello after HelloRetryRequest, consistent with the
  other GREASE values.

### Changed

- README factual refresh: 2026 Let's Encrypt profile lifetimes and the
  clientAuth EKU removal, ECH's real RFC number (9849), ML-DSA-65
  moved to the supported list to match the code, the `make-cert-store`
  `:path` keyword, and the actual dependency list.
- The `verify-certificate-chain` docstring no longer claims OCSP on
  the pure Lisp path: revocation checking there is CRL-only; OCSP
  happens only when the native Windows/macOS paths delegate to the OS.

- 34 new ARI tests over the existing stubbed ACME transport, covering
  the certificate identifier (including the RFC 9773 Appendix A
  vector), RFC 3339 parsing, the advisory-query failure modes, the
  renewal decision rule, and the `replaces` retry.
- The `pure-tls/acme/test` suites now actually run in CI on all three
  platforms via a new `make acme-tests` target wired into `all-tests`
  (cl-json was also missing from the CI dependency install).
- `pure-tls-shim` is now `.PHONY`, so `make boringssl-shim` rebuilds
  the shim instead of silently reusing a stale binary.
- `.gitattributes` pins Lisp sources to LF on all platforms: a CRLF
  checkout (Git for Windows default) broke SBCL's compile-time
  checking of literal `format` control strings with tilde-newline
  continuations.
- New and renamed verification-time regression tests pin the type
  rules the guards actually enforce, including
  `negative-verification-time-is-rejected`, which asserts the signaled
  condition is not `tls-certificate-not-yet-valid`.

### Fixed

- **`verify-certificate-chain` takes its verification time and
  hostname as keyword arguments** (#24). `now` and `hostname` were
  positional `&optional` parameters ahead of the `&key` parameters, so
  a caller who went straight to a keyword had it silently consumed as
  a positional: `(verify-certificate-chain chain roots
  :check-revocation t)` bound `now` to `:check-revocation`, bound
  `hostname` to `t`, and never ran the revocation check the call asked
  for. Moving both parameters to `&key` removes that class of misuse
  outright. The function is internal (not exported), but the README's
  CRL example reaches it by its double-colon name — anyone who copied
  that example needs the new keyword shape.

- **Argument-type guards on `now` and `hostname`** (#23). A `now`
  that is not a non-negative real, or a `hostname` that is neither a
  string nor `nil`, now signals `tls-certificate-error` at entry,
  naming the offending argument — instead of surfacing downstream as a
  bare `type-error`, a foreign-string error in the native verifiers, a
  validity failure blaming the certificate, or (worst) a chain that
  verifies with no hostname checked at all.

## [1.13.0] - 2026-07-25

Feature and hardening release. TLS clients can opt into a stricter RFC 6125
hostname-verification profile, and the ACME client is substantially more
robust: it recovers automatically from transient CA errors, stops leaking
the account contact address to logs, and can present the `tls-alpn-01`
challenge certificates it generates. The record and handshake layers are
also hardened against a memory-amplification denial of service and trimmed
of per-record allocation on the steady-state data path.

The hostname-policy and ACME robustness work was contributed by Brian
O'Reilly (@fade).

### Added

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

### Changed

- **Reduce per-record consing on the data path.** Several
  full-record-sized allocations were removed from the steady-state
  encrypt/decrypt hot paths. On the read side, AES-GCM and
  ChaCha20-Poly1305 now decrypt in place and ChaCha computes its Poly1305
  tag incrementally, and `tls13-decrypt-record` decrypts into a pooled
  scratch buffer (wiped after use) so the trimmed plaintext is the only
  per-record heap allocation. On the write side, the pending payload is no
  longer copied out on flush; bounds are threaded through the record-layer
  write path instead. External contracts are unchanged.

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

### Fixed

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

### Security

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

## [1.12.1] - 2026-07-06

Bug-fix release. A TLS context created with `+verify-peer+` no longer
comes up with an empty trust store, fixing spurious `UNKNOWN-CA`
("No trusted root certificates available for verification") failures on
the cl+ssl compatibility path — most visibly drakma-based HTTPS on
Linux.

### Changed

- Added `trust-store-tests`: hermetic regression tests that pin the
  trust source with `SSL_CERT_FILE` (a fixture bundle) and assert that
  `+verify-peer+` and `+verify-required+` contexts auto-load roots,
  `+verify-none+` does not, and an explicit `:ca-file` is still honored.

### Fixed

- **Auto-load the system trust store for `+verify-peer+`, not only
  `+verify-required+`.** `make-tls-context` (and `ensure-default-context`)
  loaded the system CA store only when `verify-mode` was
  `+verify-required+`. A `+verify-peer+` context — which still verifies a
  certificate whenever the peer presents one, and servers always do — was
  left with a `nil` trust store, so pure-Lisp chain verification ran
  against zero roots and failed every certificate with `UNKNOWN-CA`.
  Auto-load now fires whenever verification is enabled at all
  (`+verify-peer+` or `+verify-required+`) and no explicit `:ca-file` /
  `:ca-directory` was given; `+verify-none+` still loads nothing.

  This bit the cl+ssl compatibility layer, whose default context is
  built with `+verify-peer+`: drakma-based clients (for example ocicl's
  GitHub self-update path) failed on Linux with
  `No trusted root certificates available for verification`, while the
  same host's direct `+verify-required+` connections succeeded. Windows
  and macOS were unaffected because their native verifiers supply the
  OS trust store when the Lisp root set is empty.

## [1.12.0] - 2026-07-06

Security-hardening release. Two fail-closed guards tighten certificate
handling: hostname verification now rejects unsafe DNS names outright,
and naming an unusable explicit trust source is now an error rather
than a silent trust-nothing store. Both were contributed by @fade.

### Security

- **Reject embedded-NUL and non-LDH bytes in hostname verification.**
  A name-safety guard is now applied to the requested identity and to
  each candidate SAN `dNSName`. A name containing an embedded NUL, or
  any byte outside the LDH set (letters, digits, hyphen, dot), is
  rejected rather than reaching a silent unequal compare — closing the
  classic `www.bank.com\0.evil.com` truncation-confusion class. A
  single leading `*.` wildcard label is still permitted, so legitimate
  wildcard SAN patterns continue to validate. The check runs after
  IDNA normalization, so Unicode (U-label) hostnames convert to their
  `xn--` A-label form and verify as before, while an embedded NUL or
  other non-LDH byte survives normalization and is still rejected
  (#14).

- **Fail closed on an unusable explicit CA source.** Naming an explicit
  `:ca-file` or `:ca-directory` that is unreadable, empty, or malformed
  previously produced a silent trust-nothing store: verification then
  failed every certificate with no indication that the configured trust
  source was the cause. pure-tls now signals `tls-certificate-error` at
  context creation, so a misconfigured explicit source is a clear error
  rather than a confusing downstream verification failure. The
  system-CA auto-load path is unchanged (still warn-and-continue); only
  an explicitly named source is treated as fail-closed (#13).

### Notes

Both hardening fixes were contributed by @fade.

## [1.11.4] - 2026-07-03

Interoperability fix release. TLS 1.3 session resumption now works
correctly against other TLS implementations; previously every
reconnection that offered a cached session ticket to a server without
ML-KEM support failed with a fatal alert (`illegal_parameter` from
Java/JSSE-based servers such as JFrog Artifactory, `decrypt_error` from
OpenSSL-based servers). Users who saw connections to a host fail after
the first successful one should upgrade.

### Changed

- Added a session-resumption interoperability regression suite that
  performs two-connection resumption against a real `openssl s_server`,
  with and without a forced HelloRetryRequest, asserting the server
  actually accepted the PSK. The binder bugs above were invisible to
  loopback tests (both sides shared the same incorrect transcript
  computation) and to RFC 8448 vector tests (which bypass transcript
  construction); only interop with a foreign stack catches them.
- Added loopback regression tests for verify-required resumption,
  including fail-closed coverage for unverified and cross-hostname
  tickets (#10).
- Interop verified against OpenSSL 3.6, Go 1.26, and JDK 8/17/26.
  BoringSSL conformance against the pinned upstream ref: two more tests
  pass than in 1.11.3, no regressions.

### Fixed

- **Fix the PSK binder transcript for ClientHellos sent in response to a
  HelloRetryRequest.** Per RFC 8446 §4.2.11.2 the binder must cover
  `message_hash(ClientHello1) || HelloRetryRequest || Truncate(ClientHello2)`;
  pure-tls hashed only the truncated second ClientHello. Because
  pure-tls offers a single X25519MLKEM768 key share, every server
  without ML-KEM support forces a HelloRetryRequest, so resumption
  against such servers always failed with a fatal alert on every
  connection after the first. Fixed on both the client and server sides.

- **Send `psk_key_exchange_modes` in every ClientHello**, not only when
  offering a PSK. RFC 8446 §4.6.1 forbids servers from issuing
  NewSessionTicket to a client that did not offer `psk_dhe_ke`, and
  strict implementations (e.g. JSSE) enforce this — so pure-tls could
  never obtain a session ticket from them and resumption silently never
  happened. Mainstream clients include this extension unconditionally.

- **Accept `supported_groups` in EncryptedExtensions.** RFC 8446 §4.2
  permits it there (§4.2.7: clients MUST NOT act upon it) and Java/JSSE
  servers send it, so the first connection to such servers failed with
  an `UNEXPECTED_EXTENSION` error.

- **Accept authenticated TLS 1.3 PSK resumption under
  `+verify-required+`.** A resumed session legitimately omits the
  server Certificate (RFC 8446 §2.2, §4.2.11); pure-tls demanded one
  and failed the handshake. Each cached ticket now records the hostname
  the minting handshake certificate-verified, and a certificate-less
  resumed Finished is accepted only when the accepted PSK's ticket
  proves verification of the same host — anything less fails closed as
  before. Contributed by @fade (#10).

- **Fix duplicate function definitions that clobbered each other at
  load time.** `known-extension-p`, `parse-ecdsa-signature`, and
  `make-ecdsa-public-key` were each defined twice with different
  signatures or semantics (once in the handshake module, again in the
  x509 module, which loads later and silently won). The x509 variants
  are now named distinctly, so handshake-module callers get the
  functions they were written against.

## [1.11.3] - 2026-06-21

Conformance and CI hardening release.

### Changed

- Pinned the BoringSSL test suite to a fixed upstream commit so the
  conformance baseline is reproducible (BoringSSL is unversioned and adds
  tests continuously, which otherwise reports new upstream tests as
  spurious regressions). Refreshed `test/boringssl-baseline.txt` to match.
- Added a security-regression test suite covering the fixes shipped in
  1.11.2 (EKU enforcement, ECH config bounds checking).

### Fixed

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

## [1.11.2] - 2026-06-21

Security patch release hardening certificate ExtendedKeyUsage
enforcement and the Encrypted Client Hello (ECH) configuration parser.

### Security

#### CL-SEC-2026-0207 — ExtendedKeyUsage not enforced during chain verification (LOW)

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

#### CL-SEC-2026-0206 — Out-of-bounds read parsing a hostile ECHConfig (LOW)

The ECH configuration parser read attacker-controlled length fields and
sliced the input before bounds-checking them, so a malformed
`ECHConfigList` (for example an oversized `public_key` length) raised an
uncaught, non-TLS Lisp error. A malicious server could abort a client's
handshake with a crafted EncryptedExtensions message.

**Fix:** ECHConfig structures are now parsed through the bounds-checked
buffer readers, so malformed input signals a graceful `tls-decode-error`
instead of an uncaught condition.

### Notes

Security issues identified by the [CL-SEC initiative](https://cl-sec.github.io/cl-sec-advisories/).

## [1.11.1] - 2026-04-18

Security patch release fixing two vulnerabilities in certificate chain
verification and constant-time comparison.

### Fixed

- Post-handshake messages now use a reassembly buffer to correctly
  handle TLS 1.3 message fragmentation and coalescing across records.
- Wildcard hostname validation rejects known multi-label public
  suffixes (e.g., `*.co.uk`).

### Security

#### CL-SEC-2026-0201 — Certificate chain trust anchor verified by name only (HIGH)

Trust-anchor matching accepted a certificate chain if any chain
member's issuer distinguished name matched a trusted root, without
verifying the cryptographic signature. An attacker could forge an
intermediate with a spoofed issuer DN and have it accepted as trusted.

**Fix:** `verify-certificate-chain` now requires
`verify-certificate-signature` to succeed against the trust anchor,
not just `certificate-issued-by-p` (name equality).

#### CL-SEC-2026-0202 — Constant-time comparison false equality on length differences (HIGH)

`ct-equal-mask` masked the length XOR to 8 bits before accumulation,
so inputs differing in length by a multiple of 256 compared as equal
when their shared prefix matched. This function is used in ML-KEM-768
decapsulation for implicit rejection (FIPS 203).

**Fix:** The full fixnum length XOR is now folded across all bytes
before the mask derivation, ensuring any non-zero length difference
produces a "not equal" result.

### Notes

Security issues identified by the [CL-SEC initiative](https://cl-sec.github.io/cl-sec-advisories/).

## [1.11.0] - 2026-03-31

This release addresses **4 security findings** (plus 3 bonus fixes)
identified by the [CL-SEC initiative](https://github.com/CL-SEC/CL-SEC).

### Fixed

- **RSA signature hash comparison** now uses `constant-time-equal`
  instead of `equalp` for defense-in-depth.
- **Unknown certificate signature algorithms** now signal an error
  instead of silently falling back to SHA-256.
- **`with-tls-client-stream`/`with-tls-server-stream` macros** fixed
  to use standard `(progn ,@body)` instead of non-portable
  `(unquote-splicing body)`.

### Security

#### CL-SEC-2026-0112 — Missing TLS 1.3 downgrade sentinel check (HIGH)

RFC 8446 Section 4.1.3 requires checking the last 8 bytes of
ServerHello.random for downgrade sentinel values.

**Fix:** Added downgrade sentinel detection in `process-server-hello`.
Aborts with `illegal_parameter` if sentinel bytes for TLS 1.2 or TLS
1.1 downgrade are detected.

#### CL-SEC-2026-0113 — Missing legacy_session_id_echo validation (MEDIUM)

RFC 8446 requires the server to echo back the client's
`legacy_session_id`.

**Fix:** Added comparison of `server-hello-legacy-session-id-echo`
against `client-handshake-legacy-session-id`. Aborts with
`illegal_parameter` on mismatch.

#### CL-SEC-2026-0114 — Non-cryptographic PRNG for ticket_age_add (MEDIUM)

`ticket_age_add` was generated using `CL:RANDOM` (Mersenne Twister),
which is predictable.

**Fix:** Now uses `random-bytes` (ironclad CSPRNG) for
cryptographically secure ticket age obfuscation.

#### CL-SEC-2026-0115 — Secret key material not zeroized (MEDIUM)

The `zeroize` and `with-zeroized-vector` functions existed but were
never called.

**Fix:** Fixed the `with-zeroized-vector` macro (was using invalid
`unquote-splicing` instead of `,@body`). Also fixed the same issue
in `with-tls-client-stream` and `with-tls-server-stream`. The macros
are now correct and usable for secret cleanup.

### Notes

Security issues identified by the CLSEC (Common Lisp Security
Initiative) automated audit.

## [1.10.0] - 2026-02-05

**Release Date:** February 2026

This release adds integrated timeout and cancellation support via the [`cl-context`](https://github.com/atgreen/cl-context) library. TLS operations can now be bounded with deadlines, cancelled cooperatively, and benefit from automatic context propagation through dynamic scoping.

### Changed

- **BREAKING CHANGES**

None. This release is fully backward compatible.

### Added

#### Timeout and Cancellation Support

Integration with the `cl-context` library provides cooperative timeout and cancellation for TLS operations:

- **Automatic context propagation** - Uses `cl-context:*current-context*` dynamic variable, no explicit parameter passing required
- **Deadline enforcement** - Check timeouts at I/O boundaries (before each TLS record read, between handshake states)
- **Cooperative cancellation** - Cancel in-flight operations at next check point
- **Composable timeouts** - Parent deadlines automatically propagate to nested operations
- **Close-on-cancel watcher** - Optional background thread for immediate socket closure on cancellation

#### New API Parameters

#### `make-tls-client-stream` and `make-tls-server-stream`

New optional `:request-context` parameter:

```lisp
(make-tls-client-stream socket
  :hostname "example.com"
  :request-context ctx)  ; Optional cl-context for timeout/cancellation
```

**Note:** Explicit context passing is rarely needed. The context automatically propagates via `*current-context*` when using `cl-context:with-timeout-context` or `cl-context:with-cancel`.

#### New Conditions

Two new error conditions exported in the `pure-tls` package:

- **`tls-context-cancelled`** - Signaled when operation is cancelled via request context
- **`tls-deadline-exceeded`** - Signaled when operation exceeds its deadline

Both inherit from `tls-error` and can be caught as part of normal error handling.

#### Usage Examples

**Basic timeout:**

```lisp
;; Timeout entire TLS operation (handshake + I/O) after 30 seconds
(cl-context:with-timeout-context (_ 30)
  (let ((socket (usocket:socket-connect "slow-server.com" 443
                                         :element-type '(unsigned-byte 8))))
    (pure-tls:with-tls-client-stream (tls (usocket:socket-stream socket)
                                          :hostname "slow-server.com")
      (read-line tls))))
```

**User cancellation:**

```lisp
;; Cooperative cancellation - checked at I/O boundaries
(multiple-value-bind (cancel-ctx cancel-fn)
    (cl-context:with-cancel (cl-context:background))
  (bt2:make-thread
    (lambda ()
      (let ((cl-context:*current-context* cancel-ctx))
        (pure-tls:make-tls-client-stream socket :hostname "example.com"))))
  ;; Later, when user clicks "Cancel":
  (funcall cancel-fn))
```

**Composable deadlines:**

```lisp
;; Parent deadline automatically propagates to all operations
(cl-context:with-timeout-context (_ 60)
  (pure-tls:with-tls-client-stream (tls socket :hostname "example.com")
    (write-http-request tls)
    (read-http-response tls)))  ; All I/O shares same 60s budget
```

### Changed

This release is fully backwards compatible. The new `:request-context` parameter is optional and defaults to `nil` (no timeout/cancellation).

#### Migration from Explicit Context Passing

If you were using explicit context passing patterns, you can simplify to use automatic propagation:

**Before (verbose):**
```lisp
(cl-context:with-timeout-context (ctx 30)
  (make-tls-client-stream socket
                          :hostname "example.com"
                          :request-context ctx))  ; Explicit passing
```

**After (idiomatic):**
```lisp
(cl-context:with-timeout-context (_ 30)
  (make-tls-client-stream socket
                          :hostname "example.com"))  ; Uses *current-context*
```

#### Benefits

- **Bounded operations** - Timeouts checked at I/O boundaries
- **Responsive UIs** - Cancel long-running connections between operations
- **DoS protection** - Enforce per-connection time limits
- **Better testing** - Deterministic timeout behavior without sleep/polling

New dependency added:
- [`cl-context`](https://github.com/atgreen/cl-context) - Cooperative cancellation and deadline propagation

Updated threading library:
- `bordeaux-threads` → `bordeaux-threads-2` for improved portability and atomic operations

- README updated with comprehensive timeout/cancellation examples
- New Features section highlights cl-context integration
- API documentation updated for new parameters and conditions

### Fixed

- Fixed iparse API compatibility (now handles `IPARSE/UTIL:METAOBJECT` structs)
- Removed unsupported `:timeout` parameter from `usocket:socket-accept`
- Fixed unused context variable warnings in tests and examples

### Notes

#### Timeout Behavior (Cooperative Checking)

Timeouts are checked cooperatively at safe points:

- **Checks occur before** each blocking operation, not during
- Existing blocking reads complete before timeout is detected
- Effective for slow servers (long waits between messages)
- Not effective for slow reads (partial data trickling in)

#### When Timeout Checks Occur

- Before each TLS record read
- Between handshake state transitions
- Before stream read operations (`stream-read-byte`, `stream-read-sequence`)
- Currently **NOT** implemented for CRL fetching

#### Close-on-Cancel Watcher Thread

When a context is provided, an optional watcher thread monitors for cancellation:

- Polls context state every 100ms
- Closes underlying socket when context is cancelled/deadline exceeded
- Enables immediate interruption of blocking I/O operations
- Thread automatically exits when context is done

Context integration design and implementation based on [`cl-context`](https://github.com/atgreen/cl-context) by Anthony Green.

## [1.9.0] - 2026-01-18

**Release Date:** January 2026

This release adds the `:trust-anchor-mode` parameter for consistent trust anchor behavior across platforms, implements Windows custom chain engine support for exclusive trust anchors, and adds revocation checking for macOS Security.framework.

### Added

#### Trust Anchor Mode

A new `:trust-anchor-mode` parameter provides consistent control over how custom trust anchors interact with system certificate stores:

- **`:replace`** - Use ONLY the provided trusted roots, ignoring the system store
- **`:extend`** - Use provided roots IN ADDITION TO system roots (default)

This parameter is supported across all platforms:
- **macOS**: Uses `SecTrustSetAnchorCertificatesOnly`
- **Windows**: Creates a custom chain engine with `CertCreateCertificateChainEngine` and `hExclusiveRoot`
- **Pure-Lisp**: Direct control over trust anchor set

#### Windows Custom Chain Engine

Full `:replace` mode support on Windows using the CryptoAPI custom chain engine:

- Creates exclusive root store for custom trust anchors
- Uses `CERT_CHAIN_ENGINE_CONFIG` with `hExclusiveRoot` field (Windows 7+)
- Proper cleanup of chain engine and certificate stores

#### macOS Revocation Checking

Added OCSP/CRL revocation checking support via Security.framework:

- Uses `SecPolicyCreateRevocation` with configurable methods
- Supports OCSP, CRL, or both (default)
- Network access required for revocation checks

#### ACME Certificate Profile Support

The ACME client now supports certificate profiles for requesting specific certificate types from ACME servers that support this extension.

### Changed

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

### Fixed

- Fixed native verification to respect explicit `trusted-roots` parameter
- Fixed revocation checking pass-through to native verifiers
- Fixed self-signed certificate test to use pure-Lisp verification
- Skip macOS-incompatible tests (name-constraints-no-san, ct-permissive-with-scts)

## [1.8.0] - 2026-01-15

**Release Date:** January 2026

This release adds ML-DSA-65 post-quantum digital signature support (FIPS 204) and fixes HelloRetryRequest extension validation per RFC 8446 and RFC 9639.

### Added

#### ML-DSA-65 Post-Quantum Signatures (FIPS 204)

pure-tls now supports ML-DSA-65 (Module-Lattice Digital Signature Algorithm), providing quantum-resistant digital signatures:

- **Full FIPS 204 compliance** - Implements the complete ML-DSA-65 specification
- **Key generation** - Generate ML-DSA-65 key pairs from seed
- **Sign and verify** - Create and validate post-quantum signatures
- **TLS 1.3 ready** - Prepared for post-quantum certificate authentication

Combined with the existing X25519MLKEM768 key exchange, pure-tls now offers both post-quantum key exchange and post-quantum signatures.

### Changed

This release is backwards compatible. ML-DSA-65 functions are available in the `pure-tls` package:

- `ml-dsa-65-keygen` - Generate key pair from 32-byte seed
- `ml-dsa-65-sign` - Sign a message
- `ml-dsa-65-verify` - Verify a signature

### Fixed

#### HelloRetryRequest Extension Validation

- **RFC 8446 compliance** - HRR now correctly rejects unknown extensions with `unsupported_extension` alert
- **RFC 9639 compliance** - ECH extension in HRR is only accepted when the client offered ECH
- **Improved interoperability** - Passes BoringSSL regression test suite

## [1.7.0] - 2026-01-14

**Release Date:** January 2026

This release adds Encrypted Client Hello (ECH) support per RFC 9639, Ed448 signature algorithm support, Certificate Revocation List (CRL) checking, and fixes for the cl+ssl compatibility layer.

### Added

#### Encrypted Client Hello (ECH) - RFC 9639

pure-tls now supports ECH to protect the SNI (Server Name Indication) from network observers:

- **Full RFC 9639 compliance** - Implements the complete ECH specification
- **Automatic negotiation** - ECH is used when the server provides ECHConfig
- **GREASE support** - Sends GREASE ECH extensions when ECH is unavailable

#### Ed448 Signature Algorithm

Added support for Ed448 signatures in certificate chains:

- **Ed448 signature verification** - Validates certificates signed with Ed448
- **TLS 1.3 integration** - Advertises ed448 in signature_algorithms extension

#### Certificate Revocation List (CRL) Support

New CRL checking for enhanced certificate validation:

- **CRL parsing** - Full ASN.1 CRL parsing with signature verification
- **Revocation checking** - Validates certificates against CRLs
- **Multiple CRL support** - Handles certificate chains with multiple CRL distribution points

### Changed

This release is backwards compatible. New features are automatically available:

1. **ECH** - Automatically used when server provides ECHConfig via DNS
2. **Ed448** - Automatically supported in certificate chains
3. **CRL** - Enable with `:check-crl t` in context options

The cl+ssl compatibility fixes ensure that applications using drakma or other cl+ssl-based HTTP clients work correctly with pure-tls.

### Fixed

#### cl+ssl Compatibility Layer

- **Fixed drakma integration** - Resolved TLS connection failures when using pure-tls as a drop-in cl+ssl replacement
  - Fixed `stream-fd` to return the stream itself instead of extracting the file descriptor (avoids dual-buffering issues)
  - Fixed `close-callback` wrapper to handle arity mismatch between cl+ssl (0 args) and pure-tls (1 arg)

#### Platform Fixes

- **Windows build fixes** - Updated BoringSSL baseline and fixed Windows-specific issues
- **BoringSSL interop fixes** - Fixed test regressions from post-quantum cryptography work

## [1.6.0] - 2026-01-10

**Release Date:** January 2026

This release adds **post-quantum cryptography support** with X25519MLKEM768 hybrid key exchange, implementing ML-KEM-768 per FIPS 203. This protects TLS connections against future quantum computer attacks using a hybrid design that combines classical X25519 with the lattice-based ML-KEM algorithm.

### Added

#### X25519MLKEM768 Post-Quantum Hybrid Key Exchange

pure-tls now supports post-quantum key exchange using the X25519MLKEM768 hybrid algorithm:

- **ML-KEM-768 implementation** - Full FIPS 203 compliant lattice-based key encapsulation
- **Hybrid design** - Combines X25519 (classical) with ML-KEM-768 (post-quantum)
- **Automatic negotiation** - Preferred key exchange when both peers support it
- **Browser compatible** - Interoperates with Chrome 124+ and other modern clients

```lisp
;; Post-quantum is automatic - no configuration needed
(pure-tls:make-tls-client-stream stream :hostname "example.com")
```

#### Chrome Interoperability Test Server

New test infrastructure for validating post-quantum key exchange with browsers:

```bash
cd test/chrome-interop
./generate-localhost-cert.sh
sbcl --load chrome-server.lisp
```

#### FIPS 203 Known Answer Tests

ML-KEM-768 implementation validated against all 1000 official NIST test vectors:

```bash
curl -sL https://raw.githubusercontent.com/post-quantum-cryptography/KAT/main/MLKEM/kat_MLKEM_768.rsp \
     -o test/vectors/kat_MLKEM_768.rsp

sbcl --eval '(asdf:load-system :pure-tls)' \
     --load test/ml-kem-kat.lisp \
     --eval '(ml-kem-kat:run-tests)'
```

### Changed

- **FIPS 203 KAT**: 1000/1000 decapsulation tests pass
- **FIPS 203 KAT**: 1000/1000 implicit rejection tests pass
- **Unit tests**: All 232+ tests pass
- **OpenSSL interop**: All tests pass
- **Chrome interop**: Verified with Chrome 124+

This release is backwards compatible. Post-quantum key exchange is:

1. **Automatic** - Negotiated when both peers support it
2. **Preferred** - Listed first in supported groups
3. **Hybrid** - Falls back to X25519 if peer doesn't support post-quantum

No configuration changes are required. Existing applications automatically gain post-quantum protection when connecting to compatible peers.

#### Performance Considerations

ML-KEM operations are more computationally intensive than classical key exchange:
- Key generation: ~1ms additional
- Encapsulation/Decapsulation: ~0.5ms additional
- Key share size: ~1KB additional per direction

For most applications, this overhead is negligible compared to network latency.

### Security

#### Constant-Time ML-KEM Operations

All ML-KEM modular arithmetic uses constant-time implementations:

- **Barrett reduction** - Constant-time modular reduction with correct v=20158
- **Conditional subtraction** - Branch-free mod q operations
- **NTT operations** - Constant-time Number Theoretic Transform
- **Implicit rejection** - CCA-secure decapsulation with pseudorandom failure output

#### Hardened Rejection Sampling

- Fixed buffer bounds checking in polynomial sampling
- Explicit error on insufficient samples (defense in depth)
- Coefficient canonicalization for malformed input defense

### Notes

#### Key Share Sizes

| Direction | X25519 | X25519MLKEM768 |
|-----------|--------|----------------|
| Client → Server | 32 bytes | 1216 bytes |
| Server → Client | 32 bytes | 1120 bytes |

#### Shared Secret Composition

The hybrid shared secret is computed as:
```
shared_secret = ML-KEM-shared-secret || X25519-shared-secret
```

This ensures security even if one algorithm is compromised.

#### IANA Codepoint

X25519MLKEM768 uses IANA registered codepoint `0x11EC` (4588).

- [FIPS 203](https://csrc.nist.gov/pubs/fips/203/final) - ML-KEM Standard
- [IETF Draft](https://datatracker.ietf.org/doc/draft-kwiatkowski-tls-ecdhe-mlkem/) - X25519MLKEM768 in TLS
- [NIST KAT Vectors](https://github.com/post-quantum-cryptography/KAT) - Test vectors

## [1.5.0] - 2026-01-09

**Release Date:** January 2026

This release adds an ACME client for automatic certificate management (Let's Encrypt), Hunchentoot web server integration, secp384r1 (P-384) curve support, strict X.509 certificate validation, and extensive RFC compliance improvements identified through BoringSSL and TLS-Anvil test suites.

### Added

#### ACME Client for Automatic Certificate Management

New `pure-tls/acme` system providing automatic certificate provisioning:

- Full ACME v2 protocol support (RFC 8555) for Let's Encrypt and compatible CAs
- HTTP-01 challenge solver with automatic validation
- Certificate storage with automatic renewal before expiry
- Thread-safe design for concurrent certificate operations
- CSR generation with proper ASN.1 encoding

#### Hunchentoot Integration

New `pure-tls/acme+hunchentoot` system for seamless web server TLS:

- Drop-in TLS 1.3 support for Hunchentoot web servers
- Automatic certificate management with Let's Encrypt
- No OpenSSL dependency required

#### secp384r1 (P-384) Elliptic Curve Support

- Added NIST P-384 curve for key exchange
- Complements existing X25519 and secp256r1 support
- Enables compliance with stricter security policies requiring 384-bit curves

#### DoS Protection

- Configurable maximum send fragment size via `:max-send-fragment`
- Protects against resource exhaustion from oversized records

#### Strict X.509 Certificate Validation

- Full RFC 5280 compliance for certificate path validation
- X.690 DER encoding validation
- Key usage and extended key usage enforcement
- Certificate chain verification improvements
- New `:skip-hostname-verify` option for certificate chain validation

### Changed

#### HelloRetryRequest (HRR)

- Server-side HRR support for key share negotiation
- Client-side HRR validation per RFC 8446
- Correct ECDH x-coordinate extraction per RFC 8446 Section 7.4.2

#### QUIC Transport Parameters

- Proper handling of QUIC transport parameters extension (57)
- ALPS extension now ignored per RFC 8446 (previously rejected)

#### Alert Handling

- Flush `close_notify` alert before closing socket
- Improved alert sending for protocol errors

#### x509test Integration

- Added x509test certificate validation test suite
- Validates certificate parsing and chain verification against known-bad certificates

#### BoringSSL Test Improvements

- Regression tracking with CI integration (`track-regressions.sh`)
- Improved test result categorization and reporting
- Many protocol validation fixes identified through BoringSSL's ProtocolBugs tests

#### TLS-Anvil Fixes

- Fixed client test failures for improved interoperability testing

This release is backwards compatible. New features are opt-in:

1. **ACME Client** - Load `pure-tls/acme` system to enable automatic certificate management
2. **Hunchentoot** - Load `pure-tls/acme+hunchentoot` for web server integration
3. **P-384** - Automatically negotiated when supported by peer
4. **Stricter validation** - Some previously tolerated malformed messages may now be rejected

#### New Dependencies

The ACME subsystems add dependencies on:
- `drakma` - HTTP client for ACME API
- `cl-json` - JSON parsing for ACME protocol
- `hunchentoot` - Web server (for `pure-tls/acme+hunchentoot` only)

### Fixed

#### Extension Validation (RFC 8446 Section 4.2)

- Per-certificate extension validation
- TLS 1.2-only extension validation for TLS 1.3 connections
- Allow TLS 1.2 extensions in ClientHello per RFC 8446

#### ALPN Validation (RFC 7301)

- Strict ALPN extension validation
- Proper error handling for ALPN negotiation failures

#### Cryptographic Validation

- Ed25519/Ed448 OID support
- Key usage validation for CertificateVerify signatures
- ECDSA curve validation to prevent cross-curve attacks
- Session ticket validation per RFC 8446

#### Record Layer (RFC 8446 Section 5)

- Enhanced record layer validation
- Proper handling of unexpected record types in post-handshake data

#### Error Messages

- BoringSSL-compatible error message formats for test compatibility
- Improved decode error messages with `:DECODE_ERROR:` prefix
- Better error messages for extension, curve, and validation failures

- Fixed `with-tls-*-stream` macro body expansion
- Fixed RSA signature algorithm selection for standard keys
- Fixed paren scoping bug in `process-client-hello` causing unbound EXTENSIONS
- Fixed empty string crash in shim argument parsing
- Fixed test synchronization race condition on macOS/Windows
- Fixed Windows CRLF line ending issue in OpenSSL test parser

## [1.4.0] - 2026-01-05

**Release Date:** January 2026

This release adds mutual TLS (mTLS) client certificate support, comprehensive test suite integration with BoringSSL and OpenSSL, and numerous RFC 8446 compliance fixes identified through rigorous protocol testing.

### Added

#### mTLS Client Certificate Authentication

- `make-tls-client-stream` now accepts `:client-certificate` and `:client-key` parameters
- Supports file paths (PEM format) or pre-loaded certificate/key objects
- Certificate chain is automatically split: first cert as client cert, remainder as chain
- Server can request or require client certificates via `:verify` mode

#### SNI Hostname Rejection

- SNI callback can return `:reject` to abort handshake with `unrecognized_name` alert
- New `:sni-hostname` parameter for client-side SNI without hostname verification
- Enables server-side virtual hosting with strict hostname policies

#### GREASE Support (RFC 8701)

- Server now sends GREASE extension in NewSessionTicket messages
- Improves interoperability with clients that validate GREASE handling

### Changed

#### BoringSSL Integration

- Full shim binary (`pure-tls-shim`) for BoringSSL's Go test runner
- **65.4% pass rate** (4274 passed, 2259 failed out of 6533 tests)
- Failures are expected: ~35% of tests target TLS 1.2 which pure-tls does not implement (TLS 1.3 only)
- Remaining failures are unimplemented optional features (ALPS, 0-RTT, peek)
- Validates protocol compliance against 300+ edge cases from ProtocolBugs

#### OpenSSL Test Framework

- INI-style configuration parser for OpenSSL `.cnf` test files
- 13 test suites integrated with FiveAM
- **100% pass rate** on all 32 enabled TLS 1.3 tests
- Covers: basic handshakes, ALPN, SNI, key update, curves, compression, client auth

This release is backwards compatible. New features are opt-in:

1. **mTLS** - Existing client code continues to work; add `:client-certificate` and `:client-key` to enable client auth
2. **SNI rejection** - Existing SNI callbacks returning `NIL` continue to use default certificate
3. **Stricter validation** - Some malformed TLS messages that were previously tolerated may now be rejected

Applications connecting to non-compliant servers may need to handle new alert conditions.

### Fixed

#### Alert Handling (RFC 8446 Section 6)

- Invalid alert levels (not 1 or 2) now rejected with `illegal_parameter`
- Double/oversized alert records (> 2 bytes) rejected with `decode_error`
- Unknown alert types rejected with `illegal_parameter`
- Warning alerts (except `close_notify` and `user_canceled`) now rejected per TLS 1.3

#### Record Layer (RFC 8446 Section 5)

- Invalid content types (outside 20-24) rejected immediately
- Prevents SSLv2 ClientHello hangs by validating content type before reading length
- Inner plaintext size validation for padded records
- Record size limit corrected to 16640 bytes (2^14 + 256)

#### Handshake (RFC 8446 Section 4)

- KeyUpdate validation: sends `illegal_parameter` for unknown request modes
- Compression method validation: requires `legacy_compression_methods` to be `[0]`
- CertificateVerify transcript ordering corrected
- Handshake message reassembly across multiple TLS records

#### Error Codes

Added BoringSSL-compatible error code prefixes for test compatibility:
- `:TLSV1_ALERT_RECORD_OVERFLOW:`
- `:UNEXPECTED_RECORD:`
- `:BAD_ALERT:`
- `:INVALID_COMPRESSION_LIST:`
- `:UNKNOWN_ALERT_TYPE:`

## [1.3.0] - 2026-01-04

**Release Date:** January 2026

This release addresses multiple security audit findings, significantly hardening X.509 certificate validation, key exchange, hostname verification, and record layer compliance.

### Changed

- `idna` - For internationalized domain name (punycode) normalization in hostname verification

#### New Constants
- `+tls-1.0+` (#x0301) - TLS 1.0 version identifier for legacy_record_version validation

#### Behavioral Changes
- `verify-hostname` now normalizes hostnames to ASCII/punycode before comparison
- `verify-certificate-chain` enforces RFC 5280 constraints (BasicConstraints, KeyUsage, pathLen, critical extensions)
- `perform-client-handshake` performs full verification during handshake, not just in stream wrapper
- Server handshake requires trust-store when `verify-mode` is `+verify-peer+` or `+verify-required+`

This release contains breaking behavioral changes for security. Applications that previously:

1. **Used `perform-client-handshake` directly** - Will now get certificate verification during the handshake (previously only in `make-tls-client-stream`)
2. **Configured server mTLS without a trust-store** - Will now fail with an error instead of silently accepting any client certificate
3. **Connected to servers with SHA-1 signed certificates** - Will now fail certificate verification
4. **Connected to servers with overly broad wildcard certificates** - May fail hostname verification

These changes improve security posture but may require configuration updates in some deployments.

### Security

#### Critical

- **X25519 all-zero shared secret rejection** - Now rejects all-zero shared secrets per RFC 7748/8446, preventing small-subgroup attacks
- **P-256 ECDH point validation** - Validates peer public keys are on the secp256r1 curve, preventing invalid curve attacks

#### High

- **X.509 chain validation hardened** - Enforces BasicConstraints CA flag, KeyUsage (keyCertSign), pathLenConstraint, and rejects unknown critical extensions per RFC 5280
- **Hostname verification improved** - Rejects overly broad wildcards (e.g., `*.com`), supports IP address SANs, and normalizes IDNA/punycode hostnames
- **SHA-1 certificate signatures rejected** - SHA-1 is cryptographically broken and no longer accepted for certificate signatures
- **PKCS#1 v1.5 padding hardened** - Enforces minimum 8-byte 0xFF padding and exact DigestInfo length per RFC 8017
- **AES-GCM key validation** - Rejects invalid key lengths; removed dead cipher selection code
- **RSA-PSS parameters enforced** - Parses and validates hash algorithm and salt length from signature parameters

#### Medium

- **Low-level API bypass prevented** - `perform-client-handshake` now enforces certificate chain and hostname verification when `verify-mode` requires it
- **Server mTLS trust-store required** - Server now fails fast if client certificate verification is enabled but no trust store is configured
- **PSK binder truncation fixed** - Server-side PSK binder verification now correctly includes the 2-byte binders list length prefix

#### Low

- **TLS record length limit corrected** - Updated from 16640 to 16656 bytes per RFC 8446 Section 5.2
- **legacy_record_version validated** - Rejects records with invalid version fields (must be 0x0301 or 0x0303)

## [1.2.0] - 2026-01-03

**Release Date:** January 2026

This release adds native macOS Keychain support for certificate verification, matching the Windows CryptoAPI integration added in 1.1.0.

### Added

#### macOS Keychain Integration
- **Native trust store** - Uses macOS Security.framework for certificate chain verification
- **No CA bundle needed** - Automatically uses system Keychain trusted roots
- **Enterprise PKI support** - Respects MDM-deployed certificates
- **Configurable** - Set `*use-macos-keychain*` to NIL to use pure Lisp verification

#### Improved +verify-peer+ Behavior
- `+verify-peer+` now verifies the certificate chain, not just the hostname
- Previously only hostname was checked; chain verification required `+verify-required+`
- Since servers always present certificates, this change ensures proper validation

### Changed

#### New Exports
- `*use-macos-keychain*` - Control whether to use native macOS verification (default T on macOS)
- `verify-certificate-chain-macos` - Direct access to Security.framework verification (macOS only)

- CFFI is now required on macOS (in addition to Windows) for native trust store bindings

### Fixed

- Fixed potential memory leak in macOS certificate array creation on partial failure
- Improved CFError handling for actionable error messages on macOS

## [1.1.0] - 2026-01-03

### Added

pure-tls now uses Windows CryptoAPI for certificate chain verification on Windows:

- **No CA bundle needed** - Uses Windows trusted root certificates automatically
- **Enterprise PKI support** - Respects Group Policy certificate deployments
- **Automatic updates** - Trust store is maintained by Windows Update
- **Authoritative** - CryptoAPI verdict is final; if it rejects a certificate, the connection fails

To disable and use pure Lisp verification instead:
```lisp
(setf pure-tls:*use-windows-certificate-store* nil)
```

### Changed

- Windows-specific offline tests for bad certificate rejection
- Tests expired certificates, self-signed certificates, and known malware CAs (Superfish, eDellRoot)

- Added `src/x509/windows-verify.lisp` with CFFI bindings to Windows CryptoAPI
- Added `*use-windows-certificate-store*` variable to control Windows native verification
- Certificate verification on Windows is now authoritative (no fallback to pure Lisp)
- Pure Lisp verification requires trusted roots when `+verify-required+` is used
- Added feature in README top-level features list

## [1.0.0] - 2026-01-03

**Release Date:** January 2026

Initial release of pure-tls, a pure Common Lisp implementation of TLS 1.3 (RFC 8446).

### Added

#### Core TLS 1.3 Support
- **Pure Common Lisp** - No foreign libraries or OpenSSL dependency
- **TLS 1.3 only** - Modern, secure protocol with simplified handshake
- **Gray streams** - Seamless integration with existing I/O code
- **Client and server support** - Full bidirectional TLS connections
- **cl+ssl drop-in replacement** - Use with existing cl+ssl-based libraries (drakma, dexador, etc.)

#### Cipher Suites
- `TLS_CHACHA20_POLY1305_SHA256` (0x1303) - Preferred for side-channel resistance
- `TLS_AES_256_GCM_SHA384` (0x1302)
- `TLS_AES_128_GCM_SHA256` (0x1301)

#### Key Exchange
- X25519 (Curve25519)
- secp256r1 (P-256)

#### Certificate Handling
- X.509 certificate parsing (DER and PEM formats)
- Certificate chain verification
- Hostname verification (including wildcards)
- Automatic system CA certificate discovery (Linux, macOS, Windows)
- Subject Alternative Names (SAN) support

#### Server Features
- SNI (Server Name Indication) callback for virtual hosting
- Client certificate authentication (mTLS)
- Configurable verification modes

#### Session Resumption
- PSK-based session resumption via NewSessionTicket
- Automatic ticket caching (client-side)
- Forward secrecy maintained via (EC)DHE

#### Security Features
- Constant-time MAC verification
- Secret zeroization utilities
- TLS 1.3 record padding for traffic analysis mitigation
- SSLKEYLOGFILE support for Wireshark debugging

#### cl+ssl Drop-in Replacement
- **Eliminate OpenSSL dependency** - Use pure-tls with existing cl+ssl-based code
- `pure-tls/cl+ssl-compat` provides the `CL+SSL` package with compatible API
- Works with libraries expecting cl+ssl (drakma, dexador, etc.)
- Use `asdf:register-immutable-system` to prevent loading real cl+ssl:
  ```lisp
  (asdf:load-system :pure-tls/cl+ssl-compat)
  (asdf:register-immutable-system "cl+ssl")
  ;; Now load your application - cl+ssl calls go to pure-tls
  ```

#### Stream Creation
```lisp
;; Client with automatic cleanup
(pure-tls:with-tls-client-stream (tls socket :hostname "example.com")
  (write-sequence data tls)
  (read-sequence buffer tls))

;; Server with SNI
(pure-tls:make-tls-server-stream socket
  :certificate "/path/to/cert.pem"
  :key "/path/to/key.pem"
  :sni-callback #'select-certificate)
```

#### Verification Modes
- `+verify-none+` - No certificate verification
- `+verify-peer+` - Verify peer certificate if provided
- `+verify-required+` - Require and verify peer certificate

### Changed

- ironclad - Cryptographic primitives
- trivial-gray-streams - Gray stream support
- flexi-streams - Character encoding
- alexandria - Utilities
- cl-base64 - Base64 encoding

- RFC 5869 HKDF test vectors
- RFC 8448 TLS 1.3 key schedule test vectors
- RFC 8439 ChaCha20-Poly1305 test vectors
- X.509 certificate parsing and validation
- Bundled bad certificates from badssl.com for offline testing
- Network tests against major TLS 1.3 sites

### Notes

- No 0-RTT early data support
- TLS 1.3 only (no fallback to TLS 1.2)

[CL-SEC-2026-0216]: https://cl-sec.github.io/cl-sec-advisories/#CL-SEC-2026-0216
[CL-SEC-2026-0217]: https://cl-sec.github.io/cl-sec-advisories/#CL-SEC-2026-0217
[CL-SEC-2026-0218]: https://cl-sec.github.io/cl-sec-advisories/#CL-SEC-2026-0218
[CL-SEC-2026-0219]: https://cl-sec.github.io/cl-sec-advisories/#CL-SEC-2026-0219
[CL-SEC-2026-0220]: https://cl-sec.github.io/cl-sec-advisories/#CL-SEC-2026-0220
[CL-SEC-2026-0221]: https://cl-sec.github.io/cl-sec-advisories/#CL-SEC-2026-0221
[Unreleased]: https://github.com/atgreen/pure-tls/compare/v1.15.1...HEAD
[1.15.1]: https://github.com/atgreen/pure-tls/compare/v1.15.0...v1.15.1
[1.15.0]: https://github.com/atgreen/pure-tls/compare/v1.14.0...v1.15.0
[1.14.0]: https://github.com/atgreen/pure-tls/compare/v1.13.0...v1.14.0
[1.13.0]: https://github.com/atgreen/pure-tls/compare/v1.12.0...v1.13.0
[1.12.1]: https://github.com/atgreen/pure-tls/commit/974eb5d8d1b07967f04814d28acfa3557303721e
[1.12.0]: https://github.com/atgreen/pure-tls/compare/v1.11.4...v1.12.0
[1.11.4]: https://github.com/atgreen/pure-tls/compare/v1.11.3...v1.11.4
[1.11.3]: https://github.com/atgreen/pure-tls/compare/v1.11.2...v1.11.3
[1.11.2]: https://github.com/atgreen/pure-tls/compare/v1.11.1...v1.11.2
[1.11.1]: https://github.com/atgreen/pure-tls/compare/v1.11.0...v1.11.1
[1.11.0]: https://github.com/atgreen/pure-tls/compare/v1.10.0...v1.11.0
[1.10.0]: https://github.com/atgreen/pure-tls/compare/v1.9.0...v1.10.0
[1.9.0]: https://github.com/atgreen/pure-tls/compare/v1.8.0...v1.9.0
[1.8.0]: https://github.com/atgreen/pure-tls/compare/v1.7.0...v1.8.0
[1.7.0]: https://github.com/atgreen/pure-tls/compare/v1.6.0...v1.7.0
[1.6.0]: https://github.com/atgreen/pure-tls/compare/v1.5.0...v1.6.0
[1.5.0]: https://github.com/atgreen/pure-tls/compare/v1.4.0...v1.5.0
[1.4.0]: https://github.com/atgreen/pure-tls/compare/v1.3.0...v1.4.0
[1.3.0]: https://github.com/atgreen/pure-tls/compare/v1.2.0...v1.3.0
[1.2.0]: https://github.com/atgreen/pure-tls/compare/v1.1.0...v1.2.0
[1.1.0]: https://github.com/atgreen/pure-tls/compare/v1.0.0...v1.1.0
[1.0.0]: https://github.com/atgreen/pure-tls/releases/tag/v1.0.0
