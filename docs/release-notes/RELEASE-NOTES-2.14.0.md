# ocicl 2.14.0 Release Notes

## What's New

### pure-tls 1.2.0 with macOS Keychain Integration

This release updates the bundled pure-tls library to version 1.2.0, which adds native macOS Keychain support for certificate chain verification. This matches the Windows CryptoAPI integration added in pure-tls 1.1.0.

Key improvements in pure-tls 1.2.0:
- **Native macOS trust store** - Uses Security.framework for certificate verification against system Keychain
- **No CA bundle needed on macOS** - Automatically uses macOS trusted root certificates
- **Enterprise PKI support** - Respects MDM-deployed certificates on macOS
- **Improved +verify-peer+ behavior** - Now verifies the full certificate chain, not just hostname

## Bug Fixes

None.

## Breaking Changes

None.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.14.0):

**Linux:**
- **Fedora/RHEL/CentOS**: `ocicl-2.14.0-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.14.0-1_amd64.deb`

**Windows:**
- **Installer**: `ocicl-2.14.0-setup.exe` (recommended)
- **MSI**: `ocicl-2.14.0.msi`
- **Chocolatey**: `choco install ocicl`
- **ZIP**: `ocicl-2.14.0-windows-amd64.zip`

**macOS:**
- **ARM64 (Apple Silicon)**: `ocicl-2.14.0-macos-arm64.tar.gz`
- **x64 (Intel)**: `ocicl-2.14.0-macos-x64.tar.gz`
- **Homebrew**: `brew install ocicl`
