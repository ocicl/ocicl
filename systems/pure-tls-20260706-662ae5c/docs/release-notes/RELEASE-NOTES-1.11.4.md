# pure-tls 1.11.4

**Release date:** 2026-07-03

Interoperability fix release. TLS 1.3 session resumption now works
correctly against other TLS implementations; previously every
reconnection that offered a cached session ticket to a server without
ML-KEM support failed with a fatal alert (`illegal_parameter` from
Java/JSSE-based servers such as JFrog Artifactory, `decrypt_error` from
OpenSSL-based servers). Users who saw connections to a host fail after
the first successful one should upgrade.

## Bug fixes

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

## Testing

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
