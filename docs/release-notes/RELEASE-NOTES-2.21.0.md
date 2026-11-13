# ocicl 2.21.0 Release Notes

**Release Date:** November 2026

## Summary

ocicl 2.21.0 is mostly about surviving a flaky registry, with a handful of install fixes alongside. The retry machinery was sound but its two numbers were wrong — concurrent downloads all slept the identical interval and came back in the same instant, and an unattended run took the retry budget meant for someone sitting at a terminal. Beyond that: git+https sources now fetch their submodules, `ocicl install` no longer hangs on a download whose connection stays open past the body, ocicl can now be built with Evergreen Common Lisp, and the bundled dependencies have been refreshed.

## Bug Fixes

- **git+https installs now fetch submodules.** `fetch-git-tree` cloned the upstream with `--no-checkout`, checked out the ref, then deleted `.git` and copied the tree — never initialising submodules. A system that vendors a dependency as a submodule (for example cl-webview, which pins webview/webview) installed with an empty submodule directory and failed at load. The install now runs `git submodule update --init --recursive` after the checkout and before `.git` is removed — a no-op when there is no `.gitmodules`, and skipped for a sparse subdirectory checkout whose submodules lie outside it. A submodule that cannot be fetched now fails the install loudly rather than leaving a silently empty directory, mirroring the registry build, which already inits submodules. (Reported as [#216](https://github.com/ocicl/ocicl/issues/216).)

- **`ocicl install` no longer hangs at its first download.** The copy from the HTTP response to disk read the stream until end of file. Drakma sets no length bound on a `:want-stream` response, so the copy only finished when the server closed the connection — and GitHub's package CDN answers `Connection: keep-alive` even when the request says `Connection: close`, so in some network setups the connection stayed open after the body and the final read waited forever. When the response declares a length, ocicl now reads exactly that many bytes and stops; without one it reads to end of file as before.

- **Concurrent downloads no longer retry in step.** A reset connection was retried after `(expt 2 attempt)` seconds, which is the same number for every caller. For a single download that is exactly right. For an install it was the bug: a non-interactive install fetches blobs at twice CPU-count concurrently, so a four-CPU runner opens eight downloads against one CDN edge, and when that edge resets several at once, every affected worker slept the identical interval and they all came back together — then again together, and again. The backoff was assembling the very herd it existed to break up. The signature is visible in the reported logs: `global-vars` and `bordeaux-threads` both announcing `retrying in 1s` at the same moment. Half the interval is now fixed, so a reset still costs a real pause, and half is randomized, so the workers fan out. (Reported as [#210](https://github.com/ocicl/ocicl/issues/210).)

- **An unattended run gets the patient retry budget by default.** The default of three retries — about seven seconds of total patience — suits a person at a prompt, who would rather hear that the registry is unreachable than watch a minute of backoff. In CI the arithmetic reverses: a reset connection costs the whole run, and nobody is watching the clock. ocicl's own CI has been setting `OCICL_HTTP_RETRIES=6` since 2.20.0 for exactly that reason, and nobody else knew to, because the knob was documented only in the 2.20.0 release notes — not the README, not the manpage. A default that every user has to discover and re-set is not a default. ocicl now reads `CI`, which GitHub Actions, GitLab, CircleCI, Travis and Azure all set, and takes six retries there: roughly sixty-three seconds of patience. `CI=false` and an empty `CI` mean what they say.

These changes do not stop a registry CDN from resetting connections; they let ocicl ride those resets out instead of converging on the instant that just failed, and give an unwatched build enough patience to get through them.

## Changed Defaults

- **`OCICL_HTTP_RETRIES` now defaults to 6 when `CI` is set**, and remains 3 otherwise. Setting the variable explicitly overrides either baseline, as before, and choosing the baseline before reading the variable makes the whole thing idempotent, which it was not previously. The retry policy is now documented in the README next to `OCICL_DOWNLOAD_CONCURRENCY`, so the next person hitting a flaky registry finds it without reading release notes for a version they are already past.

  This is why the release is minor rather than a patch. Nothing here breaks, but a CI job that previously gave up after about seven seconds will now spend up to a minute before it does, and a run that used to fail fast against a genuinely unreachable registry will take longer to say so. That is the intended trade — a build nobody is watching should prefer finishing to finishing early — but it is a change in observable behaviour, and it happens without anyone asking for it.

- The retry diagnostic prints a fractional delay (`retrying in 1.4s`) now that the interval is no longer a whole number. It still reports the number it actually sleeps.

## Also in This Release

- **Bundled dependencies refreshed** to their current registry builds: pure-tls 1.15.1, serapeum, trivial-indent, cl-cancel and drakma.

- **Builds with Evergreen Common Lisp.** `egcl --no-init --load setup.lisp` now builds ocicl with Evergreen Common Lisp alongside SBCL, using EGCL-specific forks of several vendored systems pinned in `egcl-forks.txt` (cloned into a gitignored `egcl-systems/` that shadows `systems/` for the EGCL build only). See `docs/HACKING.md` for the details and caveats.

## Upgrade Notes

Drop-in replacement for 2.20.0. No breaking changes.

If you are on 2.20.0 and cannot upgrade yet, setting `OCICL_HTTP_RETRIES: 6` as a job-level `env:` in your workflow gets you the patient budget today with no other change. `OCICL_DOWNLOAD_CONCURRENCY: 2` helps as well, by opening fewer simultaneous connections for the CDN to reset — though neither gets you the jitter, so retries will still be synchronized.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.21.0).
