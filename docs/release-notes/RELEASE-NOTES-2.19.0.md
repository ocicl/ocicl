# ocicl 2.19.0 Release Notes

**Release Date:** September 2026

## Summary

Feature and reliability release: `ocicl install` now downloads registry artifacts in parallel with colored, terminal-width progress bars, the `--template-dir` option documented in the README now actually exists, and `ocicl latest NAME` now downloads dependencies newly added by an update. Registry downloads now check HTTP status codes and retry transient failures throughout the transfer, release checksums are published in a form `ocicl update` can verify, and disabling TLS verification produces a loud warning.

## New Features

### Parallel registry downloads

Registry artifacts fetched by `ocicl install` now download concurrently. On an interactive terminal, ocicl displays colored, fixed-width progress bars sized to the current terminal and uses up to 75% of the visible rows, without exceeding twice the online processor count. The first frame measures the terminal immediately, and later frames adapt to resizes. Redirected output keeps ordinary log lines instead of terminal control sequences. Set `OCICL_DOWNLOAD_CONCURRENCY` to a positive integer to lower the automatic limit; use `1` for serial downloads.

Dependency discovery and final installation remain coordinated and deterministic: workers stage and verify independent downloads in private directories, then the main thread publishes the staged work and writes `ocicl.csv` once.

### `--template-dir` option

`ocicl new` template directories can now be supplied on the command line with the global `--template-dir DIR` option, repeatable to add several directories. The README documented this option for some time, but passing it was an unknown-option error; it is now implemented as described. The template search order is: `--template-dir` directories (in the order given), then `OCICL_TEMPLATE_PATH`, then the `ocicl-templates.cfg` config file, then the built-in templates. The README's stale reference to the config file's location was also corrected (it is `ocicl-templates.cfg` in the ocicl config directory).

## Bug Fixes

- **`ocicl latest NAME` never downloaded newly added dependencies.** Due to a misplaced form, the dependency download step after updating a named system was unreachable, so an update that introduced new dependencies left them missing until some other command happened to fetch them. Updating a named system now pulls its full dependency closure (verified live: `ocicl latest str` now fetches `str` plus `cl-ppcre`, `cl-unicode`, and `cl-change-case`; previously only `str`).
- **HTTP errors from registries are no longer treated as content.** ocicl never inspected HTTP status codes, so a transient 429 or 5xx from a registry was handed to callers as if it were a manifest or blob, surfacing later as a bogus parse failure or `MISSING-DEPENDENCY`. Responses with status ≥ 400 now signal a proper error, and transient failures (connection errors, HTTP 408/429/5xx) are retried up to 3 times with exponential backoff, with a note on stderr. TLS verification failures are reported distinctly and never retried.
- **Interrupted blob transfers are now retried from scratch.** The original retry loop covered connection setup and response headers but could not see a reset while the caller was reading a streamed body. A failed or digest-mismatched blob download now restarts as a complete, idempotent download-and-verify unit; extraction begins only after its sha256 digest matches the manifest.
- **Feature-gated ASDF dependencies no longer cause spurious downloads.** ocicl previously unwrapped every `(:feature ...)` dependency and tried to fetch it even when the optional feature was irrelevant, producing misleading “not found” errors for systems such as `dotcl-float`. Because the feature set of the Lisp that eventually loads a project may differ from the Lisp running ocicl, these dependencies are now left for that target environment to satisfy.
- **`ocicl update` can now verify release downloads.** The consolidated checksum file attached to releases was named and formatted in a way the self-updater did not recognize, so every `ocicl update` printed "Download will not be verified" and fell back to a size check. Releases now upload a standard `SHA256SUMS` file with bare asset names, which `ocicl update` verifies against. (Updates *to* this release from an older ocicl still use the old unverified path; updates *from* this release onward are checksum-verified.)

## Security

- **Disabling TLS verification now warns loudly.** `--insecure` and the `OCICL_INSECURE` environment variable turn off certificate verification for all registry traffic, including token requests carrying credentials — previously without a word. ocicl now prints a prominent warning to stderr naming which switch is responsible, so an `OCICL_INSECURE` lingering in a CI environment no longer goes unnoticed.
- **Download temp directories are created exclusively.** The temporary directory used for downloads was created with a call that succeeds silently if the directory already exists, so a pre-created directory of the same name would be silently shared and its planted contents merged into `ocicl/`. Not practically exploitable (names draw on OS entropy), but the directory is now required to be freshly created, retrying with a new name otherwise.

## Internal Improvements

- Substantial internal refactoring of `src/ocicl.lisp` for readability: dead code removed, duplicated logic consolidated into shared helpers, and `main` decomposed into named phases. Behavior-preserving.
- The lint tool's test suite was repaired (symbol-package comparison bugs in the redundant-progn rule and fixers, a stale test API, and a broken runner exit).
- `ocicl tree` output methods now honor the stream they are passed instead of always writing to standard output.

## Dependency Updates

`bordeaux-threads` is now a declared runtime dependency for portable download workers and synchronization. Its bundled version, and all other bundled dependency systems, are unchanged from 2.18.0.

## Breaking Changes

None.

## Upgrade Notes

Drop-in replacement for 2.18.0.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.19.0).
