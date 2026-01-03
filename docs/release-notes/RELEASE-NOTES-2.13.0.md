# ocicl 2.13.0 Release Notes

## What's New

### Windows TLS Certificate Verification Enabled

TLS certificate verification is now enabled by default on Windows, matching the behavior on Linux and macOS. This was previously disabled due to OpenSSL configuration issues on Windows, but with the switch to pure-tls in 2.12.0, these issues no longer apply.

### OpenSSL No Longer Required on Windows

The `openssl.dll` dependency has been removed. Windows users no longer need to install OpenSSL separately.

## Bug Fixes

None.

## Breaking Changes

None.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.13.0):

**Linux:**
- **Fedora/RHEL/CentOS**: `ocicl-2.13.0-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.13.0-1_amd64.deb`

**Windows:**
- **Installer**: `ocicl-2.13.0-setup.exe` (recommended)
- **MSI**: `ocicl-2.13.0.msi`
- **Chocolatey**: `choco install ocicl`
- **ZIP**: `ocicl-2.13.0-windows-amd64.zip`

**macOS:**
- **ARM64 (Apple Silicon)**: `ocicl-2.13.0-macos-arm64.tar.gz`
- **x64 (Intel)**: `ocicl-2.13.0-macos-x64.tar.gz`
- **Homebrew**: `brew install ocicl`
