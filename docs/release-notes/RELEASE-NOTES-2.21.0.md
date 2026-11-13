# ocicl 2.21.0 Release Notes

**Release Date:** November 2026

## Summary

Reliability fixes for installs against a flaky registry, two git-source install fixes, Evergreen Common Lisp build support, and refreshed bundled dependencies.

## Bug Fixes

- **git+https installs now fetch submodules.** `fetch-git-tree` deleted `.git` and copied the tree without initialising submodules, so a system that vendors a dependency as a submodule (for example cl-webview) installed with an empty submodule directory and failed to load. It now runs `git submodule update --init --recursive` after checkout — a no-op without `.gitmodules`, and skipped for sparse subdirectory checkouts. A submodule that cannot be fetched now fails the install rather than leaving an empty directory. ([#216](https://github.com/ocicl/ocicl/issues/216))

- **`ocicl install` no longer hangs on its first download.** The response-to-disk copy read until end of file. Drakma sets no length bound on a `:want-stream` response, so the copy finished only when the server closed the connection — and GitHub's CDN can hold the connection open past the body, leaving the final read to block forever. ocicl now reads exactly the declared Content-Length when present, and to end of file otherwise.

- **Concurrent downloads no longer retry in lockstep.** Retries used a fixed `(expt 2 attempt)` backoff, so parallel downloads reset by the same CDN edge all slept the same interval and retried together. The backoff is now half fixed and half randomized, so workers fan out. ([#210](https://github.com/ocicl/ocicl/issues/210))

- **Unattended runs get a longer retry budget.** Three retries (about 7 seconds) suit an interactive prompt; in CI a reset connection fails the whole run. ocicl now uses six retries (about 63 seconds) when `CI` is set, and three otherwise.

These changes do not stop a CDN from resetting connections; they let ocicl ride resets out instead of retrying into the same instant that just failed.

## Changed Defaults

- **`OCICL_HTTP_RETRIES` defaults to 6 when `CI` is set, and 3 otherwise.** Setting it explicitly overrides either baseline, and the policy is now documented in the README. A CI job that previously gave up after about 7 seconds now waits up to about 63 seconds before failing — the reason this is a minor release rather than a patch.

- The retry diagnostic now prints a fractional delay, for example `retrying in 1.4s`.

## Also in This Release

- **Bundled dependencies refreshed** to current registry builds: pure-tls 1.15.1, serapeum, trivial-indent, cl-cancel, and drakma.

- **Evergreen Common Lisp build support.** `egcl --no-init --load setup.lisp` builds ocicl with EGCL alongside SBCL, using the forks pinned in `egcl-forks.txt`. See `docs/HACKING.md`.

## Upgrade Notes

Drop-in replacement for 2.20.0; no breaking changes.

On 2.20.0 and cannot upgrade yet? Set `OCICL_HTTP_RETRIES: 6` (and optionally `OCICL_DOWNLOAD_CONCURRENCY: 2`) in your CI job for the longer budget now — though without the jitter, retries stay synchronized.

## Installation

Download the package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.21.0).
