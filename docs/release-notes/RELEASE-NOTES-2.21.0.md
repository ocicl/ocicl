# ocicl 2.21.0 Release Notes

**Release Date:** October 2026

## Summary

A CI build that hit a connection reset from the registry failed more often than it had any need to. The retry machinery was sound; its two numbers were wrong. Concurrent downloads all slept the identical interval and came back in the same instant, and an unattended run took the retry budget meant for someone sitting at a terminal.

## Bug Fixes

- **Concurrent downloads no longer retry in step.** A reset connection was retried after `(expt 2 attempt)` seconds, which is the same number for every caller. For a single download that is exactly right. For an install it was the bug: a non-interactive install fetches blobs at twice CPU-count concurrently, so a four-CPU runner opens eight downloads against one CDN edge, and when that edge resets several at once, every affected worker slept the identical interval and they all came back together — then again together, and again. The backoff was assembling the very herd it existed to break up. The signature is visible in the reported logs: `global-vars` and `bordeaux-threads` both announcing `retrying in 1s` at the same moment. Half the interval is now fixed, so a reset still costs a real pause, and half is randomized, so the workers fan out. (Reported as [#210](https://github.com/ocicl/ocicl/issues/210).)

- **An unattended run gets the patient retry budget by default.** The default of three retries — about seven seconds of total patience — suits a person at a prompt, who would rather hear that the registry is unreachable than watch a minute of backoff. In CI the arithmetic reverses: a reset connection costs the whole run, and nobody is watching the clock. ocicl's own CI has been setting `OCICL_HTTP_RETRIES=6` since 2.20.0 for exactly that reason, and nobody else knew to, because the knob was documented only in the 2.20.0 release notes — not the README, not the manpage. A default that every user has to discover and re-set is not a default. ocicl now reads `CI`, which GitHub Actions, GitLab, CircleCI, Travis and Azure all set, and takes six retries there: roughly sixty-three seconds of patience. `CI=false` and an empty `CI` mean what they say.

Worth being straight about what this does and does not do. It does not stop a registry CDN resetting connections. It makes ocicl ride those resets out instead of converging on the instant that just failed, and gives an unwatched build enough patience to get through them.

## Changed Defaults

- **`OCICL_HTTP_RETRIES` now defaults to 6 when `CI` is set**, and remains 3 otherwise. Setting the variable explicitly overrides either baseline, as before, and choosing the baseline before reading the variable makes the whole thing idempotent, which it was not previously. The retry policy is now documented in the README next to `OCICL_DOWNLOAD_CONCURRENCY`, so the next person hitting a flaky registry finds it without reading release notes for a version they are already past.

  This is why the release is minor rather than a patch. Nothing here breaks, but a CI job that previously gave up after about seven seconds will now spend up to a minute before it does, and a run that used to fail fast against a genuinely unreachable registry will take longer to say so. That is the intended trade — a build nobody is watching should prefer finishing to finishing early — but it is a change in observable behaviour, and it happens without anyone asking for it.

- The retry diagnostic prints a fractional delay (`retrying in 1.4s`) now that the interval is no longer a whole number. It still reports the number it actually sleeps.

## Upgrade Notes

Drop-in replacement for 2.20.0. No breaking changes.

If you are on 2.20.0 and cannot upgrade yet, setting `OCICL_HTTP_RETRIES: 6` as a job-level `env:` in your workflow gets you the patient budget today with no other change. `OCICL_DOWNLOAD_CONCURRENCY: 2` helps as well, by opening fewer simultaneous connections for the CDN to reset — though neither gets you the jitter, so retries will still be synchronized.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.21.0).
