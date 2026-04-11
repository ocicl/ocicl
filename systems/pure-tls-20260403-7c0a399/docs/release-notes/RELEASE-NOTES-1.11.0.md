# pure-tls 1.11.0

**Release date:** 2026-03-31

This release addresses **4 security findings** (plus 3 bonus fixes)
identified by the [CL-SEC initiative](https://github.com/CL-SEC/CL-SEC).

## Security Fixes

### CL-SEC-2026-0112 — Missing TLS 1.3 downgrade sentinel check (HIGH)

RFC 8446 Section 4.1.3 requires checking the last 8 bytes of
ServerHello.random for downgrade sentinel values.

**Fix:** Added downgrade sentinel detection in `process-server-hello`.
Aborts with `illegal_parameter` if sentinel bytes for TLS 1.2 or TLS
1.1 downgrade are detected.

### CL-SEC-2026-0113 — Missing legacy_session_id_echo validation (MEDIUM)

RFC 8446 requires the server to echo back the client's
`legacy_session_id`.

**Fix:** Added comparison of `server-hello-legacy-session-id-echo`
against `client-handshake-legacy-session-id`. Aborts with
`illegal_parameter` on mismatch.

### CL-SEC-2026-0114 — Non-cryptographic PRNG for ticket_age_add (MEDIUM)

`ticket_age_add` was generated using `CL:RANDOM` (Mersenne Twister),
which is predictable.

**Fix:** Now uses `random-bytes` (ironclad CSPRNG) for
cryptographically secure ticket age obfuscation.

### CL-SEC-2026-0115 — Secret key material not zeroized (MEDIUM)

The `zeroize` and `with-zeroized-vector` functions existed but were
never called.

**Fix:** Fixed the `with-zeroized-vector` macro (was using invalid
`unquote-splicing` instead of `,@body`). Also fixed the same issue
in `with-tls-client-stream` and `with-tls-server-stream`. The macros
are now correct and usable for secret cleanup.

## Additional Fixes

- **RSA signature hash comparison** now uses `constant-time-equal`
  instead of `equalp` for defense-in-depth.
- **Unknown certificate signature algorithms** now signal an error
  instead of silently falling back to SHA-256.
- **`with-tls-client-stream`/`with-tls-server-stream` macros** fixed
  to use standard `(progn ,@body)` instead of non-portable
  `(unquote-splicing body)`.

## Acknowledgments

Security issues identified by the CLSEC (Common Lisp Security
Initiative) automated audit.
