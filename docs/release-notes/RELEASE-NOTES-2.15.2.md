# ocicl 2.15.2 Release Notes

## Bug Fixes

- Fixed `ocicl update` failing to download updates from GitHub releases
  - Resolved TLS connection issues with pure-tls cl+ssl compatibility layer
  - Fixed gzip decompression failing during archive extraction

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.15.2):

**Linux:**
- **Standalone**: `ocicl-2.15.2-linux-amd64.tar.gz`
- **Fedora/RHEL/CentOS**: `ocicl-2.15.2-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.15.2-1_amd64.deb`

**Windows:**
- **Installer**: `ocicl-2.15.2-setup.exe` (recommended)
- **MSI**: `ocicl-2.15.2.msi`
- **Chocolatey**: `choco install ocicl`
- **ZIP**: `ocicl-2.15.2-windows-amd64.zip`

**macOS:**
- **ARM64 (Apple Silicon)**: `ocicl-2.15.2-macos-arm64.tar.gz`
- **x64 (Intel)**: `ocicl-2.15.2-macos-x64.tar.gz`
- **Homebrew**: `brew install ocicl`
