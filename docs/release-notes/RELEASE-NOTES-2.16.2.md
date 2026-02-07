# ocicl 2.16.2 Release Notes

## Bug Fixes

### TLS Debug Output

Fixed `OCICL_TLS_DEBUG` environment variable showing "unknown" for TLS implementation:

**Problem:**
- Debug output always showed `TLS implementation: unknown` even when pure-tls was in use
- Used compile-time reader conditionals (`#+pure-tls`) but pure-tls doesn't set a feature flag

**Solution:**
- Changed to runtime package detection using `(find-package :pure-tls)`
- Now correctly identifies "pure-tls" or "cl+ssl" based on loaded packages

**Now shows:**
```
; TLS implementation: pure-tls
; TLS verify=T ca-file=/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem ca-dir=NIL
; pure-tls: using CA bundle: /etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem
; pure-tls: trust store loaded with 151 certificate(s)
```

This makes troubleshooting TLS issues much clearer for users.

## Breaking Changes

None. This release is fully backward compatible with 2.16.1.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.16.2):

**Linux:**
- **Fedora/RHEL/CentOS**: `ocicl-2.16.2-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.16.2-1_amd64.deb`

**Windows:**
- **Installer**: `ocicl-2.16.2-setup.exe` (recommended)
- **MSI**: `ocicl-2.16.2.msi`
- **Chocolatey**: `choco install ocicl` (after community repo publication)
- **ZIP**: `ocicl-2.16.2-windows-amd64.zip`

**macOS:**
- **Homebrew**: `brew install ocicl`
- **ARM64**: `ocicl-2.16.2-macos-arm64.tar.gz`
- **x64**: `ocicl-2.16.2-macos-x64.tar.gz`
