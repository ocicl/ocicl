# ocicl 2.20.0 Release Notes

**Release Date:** September 2026

## Summary

`ocicl update` did not work in 2.19.1, on any platform. That is fixed, along with `git+` installs on Windows, and a failed download now fails the install instead of being reported and forgotten.

## Bug Fixes

- **`ocicl update` no longer fails with `No trusted root certificates available for verification`.** The failure was universal — every platform, every user — and it looked like a trust store problem on the user's machine, which is where the first reports pointed. It was not. cl+ssl carries the verification decision on the stream rather than on the context, so drakma builds its context with `+ssl-verify-none+` and asks the stream to verify; pure-tls auto-loaded system roots only when the *context* itself verified. The self-update path is the one request ocicl makes without naming a CA file, so it was the one request that arrived at the handshake with nothing to anchor to — which is why `ocicl list` worked while `ocicl update` failed in the same binary against the same trust store. Fixed in pure-tls 1.15.1, vendored here. (Reported as [#208](https://github.com/ocicl/ocicl/issues/208).)

- **Installing a system from a git source works on Windows.** Git marks the pack files it writes under `.git/objects/pack` read-only, and Windows refuses to delete a read-only file, so ocicl could not remove the temporary clone: `ocicl install git+...` died with `DELETE-FILE-ERROR ... Access is denied`. The attribute is now cleared before deletion — a no-op elsewhere, where deletability depends on the containing directory rather than the file's own mode. Cleanup of the staging directory is also best-effort now: a clone that cannot be removed is litter, not a reason to fail a fetch that already succeeded. (Reported as [#209](https://github.com/ocicl/ocicl/issues/209).)

- **`SSL_CERT_FILE` and `SSL_CERT_DIR` are honoured.** ocicl's own HTTP path read only `OCICL_CA_FILE` and `OCICL_CA_DIR`, while its self-update path read the OpenSSL variables, so one binary held two notions of where trust lives depending on the subcommand. They now rank below the `OCICL_` variables and above the built-in locations, which is how Guix and NixOS point tools at a trust store that lives outside `/etc`.

- **A file where the systems directory goes says so.** Building ocicl leaves its binary at `./ocicl`, which is exactly the name a project's `ocicl/` systems directory wants. The collision surfaced as `Can't create directory .../ocicl, a file with the same name already exists`, which names the path twice and the problem never.

- **Errors are reported rather than handed to the debugger.** Anything ocicl did not specifically expect used to reach SBCL's debugger, so a command line tool answered a mistake with a numbered list of restarts and an offer of `HELP`. It now prints the error and exits. Set `OCICL_DEBUG` to get the debugger back.

## Breaking Changes

- **A failed download now fails the install.** Download errors were printed and then dropped: `ocicl install` exited 0 having installed nothing, and whatever came next died somewhere unrelated — in CI, three steps later, as an ASDF "component not found" that pointed at the wrong thing entirely. Failures are now carried to the exit status. Scripts and pipelines that tolerated partial installs by accident will start seeing a non-zero exit where they previously saw success. That is the point, but it is a change in observable behaviour, and it is why this is a minor release rather than a patch.

## New Features

- **`OCICL_HTTP_RETRIES`** sets how many times a transient HTTP failure is retried, beyond the first attempt (default 3, maximum 16). Three suits someone waiting at a terminal, who would rather hear that the registry is unreachable than watch a minute of backoff. Somewhere unattended, where a reset connection costs a whole build, patience is worth more.

## Upgrade Notes

Drop-in replacement for 2.19.1, with the exit-status change above the one thing to check if you script `ocicl install`.

Users on 2.19.1 cannot reach this release with `ocicl update`, since that is the command this release fixes. Install it from the releases page or your package manager instead.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.20.0).
