# pure-tls 1.12.1

**Release date:** 2026-07-06

Bug-fix release. A TLS context created with `+verify-peer+` no longer
comes up with an empty trust store, fixing spurious `UNKNOWN-CA`
("No trusted root certificates available for verification") failures on
the cl+ssl compatibility path — most visibly drakma-based HTTPS on
Linux.

## Bug fixes

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

## Testing

- Added `trust-store-tests`: hermetic regression tests that pin the
  trust source with `SSL_CERT_FILE` (a fixture bundle) and assert that
  `+verify-peer+` and `+verify-required+` contexts auto-load roots,
  `+verify-none+` does not, and an explicit `:ca-file` is still honored.
