# ocicl 2.18.0 Release Notes

**Release Date:** September 2026

## Summary

Feature and security release: `ocicl install` now accepts `git+` sources, installing systems directly from a git repository alongside registry-managed ones. This release also closes several security issues in how ocicl handles registry downloads and the systems CSV, fixes `ocicl changes` truncating project names, and fixes `ocicl update` failing on Linux with a TLS `UNKNOWN-CA` error.

## New Features

### Installing systems from git

`ocicl install` now accepts `git+` sources, so a system that isn't in a registry — a personal or private project, or one whose registry entry is temporarily broken — can be installed directly from git and used exactly like a registry-managed system, with no `.sbclrc` or ASDF source-registry changes:

```
ocicl install git+https://github.com/me/my-lib
ocicl install git+https://github.com/me/my-lib@main
ocicl install git+file:///home/me/hacking/my-lib
ocicl install git+https://github.com/me/mono#subdirectory=libs/my-lib
```

A source has the form `git+URL[@REF][#PARAMS]`, where `REF` is a branch, tag, or commit SHA (default: the remote's default branch), and `PARAMS` are `&`-separated `ref=` and `subdirectory=` values. ocicl clones the repository, pins the resolved commit in `ocicl.csv` (`git+URL@SHA#ref=REF`), and places the tree under `ocicl/` as a sibling of registry-managed systems. Commit `ocicl.csv` but not the fetched tree: on a fresh clone, `ocicl install` re-fetches every git-sourced system from its pinned commit.

Git-sourced systems then behave like any other ocicl system: `ocicl latest` advances them along their recorded ref (a commit-SHA pin stays fixed), `ocicl remove` removes them along with unused dependencies, and they appear in `ocicl collect-licenses` and SBOM output. See "Installing Systems from Git" in the README for details.

## Security

- **Registry manifests and blobs are now verified against their sha256 digests.** Previously the `@sha256:` pins recorded in `ocicl.csv` were not enforced on download — ocicl trusted whatever bytes the registry returned. A manifest requested by digest is now checked to hash to that digest, and every layer blob is verified against the layer digest from the verified manifest before extraction. A compromised or MITM'd registry can no longer substitute tarball contents for a pinned system.
- **`ocicl.csv` path columns can no longer escape the systems directory.** The guard protecting directory deletion accepted upward-relative paths, so a crafted `ocicl.csv` row with a `../` path could make `ocicl remove` delete the project directory rather than a system under `ocicl/`. The guard now rejects any path not strictly contained in the systems directory.
- **Server-supplied digests are validated before use.** Digest strings from registry responses are checked to be well-formed `sha256:` digests before they are used in URLs or written into `ocicl.csv`, closing a row-injection vector via a comma in the `Docker-Content-Digest` header.

## Bug Fixes

- **`ocicl changes` no longer truncates project names.** With no arguments, `ocicl changes` computed each project's key using a hard-coded prefix length left over from the old `systems/` directory, eating the first two characters of every name (e.g. `-change-case` instead of `cl-change-case`) and breaking the version lookup behind it.
- **`ocicl update` failed on Linux with `TLS verification error: No trusted root certificates available for verification (UNKNOWN-CA)`.** The self-update path connects to GitHub through drakma and the bundled pure-tls cl+ssl compatibility layer, whose default TLS context uses `+verify-peer+`. pure-tls only auto-loaded the system CA store for `+verify-required+` contexts, so the self-update context came up with an empty trust store and rejected GitHub's certificate. ocicl's own registry operations (`install`, `list`, `latest`, …) were unaffected because they verify with `+verify-required+`, and Windows/macOS were unaffected because their native verifiers supply the OS trust store. Fixed by the pure-tls update below, which auto-loads the system trust store whenever certificate verification is enabled.

## Dependency Updates

- **pure-tls 1.12.0 → 1.13.0.** Among other changes, pure-tls now auto-loads the system trust store for `+verify-peer+` contexts, not only `+verify-required+` (introduced in 1.12.1); this is the `ocicl update` fix above.
- **All bundled dependency systems refreshed** to their current registry versions.

## Breaking Changes

None for normal use. Note that installs which previously succeeded against a registry serving content that does not match its advertised digest will now fail, by design.

## Upgrade Notes

Drop-in replacement for 2.17.0. If you are on Linux and `ocicl update` failed under 2.17.0, install this release manually (for example via your distribution package or the release tarball below), then `ocicl update` will work again.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.18.0).
