# ocicl 2.12.0 Release Notes

## What's New

### Pure Common Lisp TLS

ocicl now uses [pure-tls](https://github.com/atgreen/pure-tls) for HTTPS connections, eliminating the OpenSSL dependency.

**Benefits:**
- **No OpenSSL required** - No need to install or configure OpenSSL/LibreSSL
- **Pure Common Lisp** - TLS 1.3 implementation with no foreign library dependencies
- **Simpler deployment** - Single binary works everywhere without system library issues
- **Modern security** - TLS 1.3 only, with ChaCha20-Poly1305 and AES-GCM cipher suites

This change is transparent to users - ocicl continues to work exactly as before, but now works out of the box on systems without OpenSSL installed.

## Bug Fixes

None.

## Breaking Changes

- Servers that only support TLS 1.2 or older are no longer supported (TLS 1.3 is required)
- Most modern servers support TLS 1.3, so this should not affect typical usage

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.12.0):

**Linux:**
- **Fedora/RHEL/CentOS**: `ocicl-2.12.0-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.12.0-1_amd64.deb`

**Windows:**
- **Installer**: `ocicl-2.12.0-setup.exe` (recommended)
- **MSI**: `ocicl-2.12.0.msi`
- **Chocolatey**: `choco install ocicl`
- **ZIP**: `ocicl-2.12.0-windows-amd64.zip`

**macOS:**
- **ARM64 (Apple Silicon)**: `ocicl-2.12.0-macos-arm64.tar.gz`
- **x64 (Intel)**: `ocicl-2.12.0-macos-x64.tar.gz`
- **Homebrew**: `brew install ocicl`
