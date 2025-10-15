# ocicl 2.7.9 Release Notes

I'm pleased to announce ocicl 2.7.9, a bug fix release addressing a Windows TLS verification issue.

## What's Changed

### Windows TLS Verification Fix

Fixed a critical issue on Windows where the platform-specific TLS verification default (disabled) was being incorrectly overridden by command-line parsing. The fix ensures that `*verify-tls*` is only modified when `-k/--insecure` or `OCICL_INSECURE` is explicitly provided, properly preserving platform-specific defaults.

Previously, the Windows-specific default for TLS verification would not be respected, causing certificate verification issues on Windows systems. This release corrects the command-line argument handling to maintain the appropriate platform-specific behavior.

**Fixes #147**

---

For more information, visit the [ocicl repository](https://github.com/ocicl/ocicl) or read the [documentation](https://github.com/ocicl/ocicl/blob/main/README.md).
