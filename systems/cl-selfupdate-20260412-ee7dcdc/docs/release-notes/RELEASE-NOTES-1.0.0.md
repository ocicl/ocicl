# cl-selfupdate 1.0.0

**Release date:** 2026-03-31

This release addresses **5 security vulnerabilities** identified by the
[CL-SEC initiative](https://github.com/CL-SEC/CL-SEC).  All users
should upgrade immediately.

## Security Fixes

### CL-SEC-2026-0134 — Checksum validation optional and silently skipped (HIGH)

When no checksum file was present in the release assets, downloads
proceeded with zero integrity verification and no warning.

**Fix:** A prominent warning is now emitted when no checksum file is
found.  Asset file size is verified against the reported size as a
basic sanity check.

### CL-SEC-2026-0135 — Predictable temp directory enables symlink attacks (HIGH)

The temp directory used `get-universal-time` (predictable) in its name.
A local attacker could pre-create the directory as a symlink. Temp
directories were never cleaned up.

**Fix:** Temp directories are now created with `mkdtemp` on SBCL
(cryptographically random names) or a random 12-character suffix on
other implementations.  Temp directories are cleaned up on failure
via `unwind-protect`.

### CL-SEC-2026-0136 — No TLS certificate verification enforcement (HIGH)

Neither the dexador nor drakma HTTP backends explicitly configured
TLS certificate verification, leaving it to library defaults.

**Fix:** The drakma backend now passes `:verify :required` on all
requests.  The dexador backend documents that verification depends
on the underlying TLS library (pure-tls verifies by default).

### CL-SEC-2026-0137 — Archive path traversal (MEDIUM)

Archive entries with `..` in their names could escape the extraction
directory.  Executable name matching used substring `search`, allowing
overly permissive matches.

**Fix:** Archive entries containing `..` are rejected. Executable name
matching uses exact match instead of substring search.

### CL-SEC-2026-0138 — No version downgrade protection (MEDIUM)

`download-update` and `apply-update` would install any release
including older versions with known vulnerabilities.  GitHub API owner
and repo parameters were not validated.

**Fix:** `update-self` now refuses to downgrade.  GitHub API path
components are validated to reject injection characters.

## Other Changes

- Version bumped to 1.0.0 to reflect the security audit milestone.
- Added GitHub Actions release workflow.

## Acknowledgments

Security issues identified by the CLSEC (Common Lisp Security
Initiative) automated audit.
