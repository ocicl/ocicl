# ocicl 2.17.0 Release Notes

**Release Date:** July 2026

## Summary

Feature release: every ocicl command now has its own `--help`, and the bundled pure-tls TLS stack is updated to 1.12.0, bringing a series of certificate- and handshake-hardening security fixes.

## New Features

### Command-specific `--help`

Every ocicl subcommand now responds to `--help` with its own usage and options, e.g. `ocicl install --help` or `ocicl lint --help`. The top-level help also advertises this: `Use 'ocicl COMMAND --help' for command-specific options.`

## Security

ocicl uses [pure-tls](https://github.com/ocicl/pure-tls) for all HTTPS connections to OCI registries. This release updates pure-tls from 1.11.1 to 1.12.0, which includes the following hardening fixes:

- **Hostname verification rejects unsafe DNS names.** Names containing an embedded NUL or any non-LDH byte are now rejected outright rather than reaching a silent unequal compare, closing the classic `www.bank.com\0.evil.com` truncation-confusion class. The check runs after IDNA normalization, so legitimate Unicode and wildcard SANs still validate.
- **Fail closed on an unusable explicit CA source.** Naming a `:ca-file` or `:ca-directory` that is unreadable, empty, or malformed now signals an error at context creation instead of silently producing a trust-nothing store that fails every certificate.
- **CL-SEC-2026-0207 — ExtendedKeyUsage now enforced during chain verification.** A leaf certificate restricted to a different purpose (e.g. `clientAuth` only) is no longer accepted as a TLS server certificate.
- **CL-SEC-2026-0206 — Out-of-bounds read parsing a hostile ECHConfig.** Malformed `ECHConfigList` input now signals a graceful decode error instead of an uncaught condition.
- **Stricter TLS 1.3 handshake extension validation.** Several handshake extensions (e.g. `server_name`, `certificate_authorities`) now reject trailing/invalid bytes rather than ignoring them.

## Bug Fixes

- **pure-tls TLS 1.3 session resumption interoperability.** Reconnections that offered a cached session ticket to servers without ML-KEM support (e.g. Java/JSSE-based servers such as JFrog Artifactory, and OpenSSL-based servers) previously failed with a fatal alert. Session resumption now works correctly against these implementations.

## Breaking Changes

None. This release is fully backward compatible with 2.16.14.

## Upgrade Notes

Drop-in replacement for 2.16.14.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.17.0):

**Linux:**
- **Fedora/RHEL/CentOS**: `sudo dnf install ocicl` (via repo) or download RPM
- **Debian/Ubuntu**: `sudo apt install ocicl` (via repo) or download DEB

**Windows:**
- **Installer**: `ocicl-2.17.0-setup.exe` (recommended)
- **MSI**: `ocicl-2.17.0.msi`
- **Chocolatey**: `choco install ocicl`
- **ZIP**: `ocicl-2.17.0-windows-amd64.zip`

**macOS:**
- **Homebrew**: `brew install ocicl`
- **ARM64**: `ocicl-2.17.0-macos-arm64.tar.gz`
- **x64**: `ocicl-2.17.0-macos-x64.tar.gz`
