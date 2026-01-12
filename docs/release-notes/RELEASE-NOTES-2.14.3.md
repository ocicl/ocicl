# ocicl 2.14.3 Release Notes

## Bug Fixes

- Fixed global systems lookup failing when a local CSV file exists (#183)
  - When `find-workdir` located a local CSV, `*systems-csv*` was set to an absolute path
  - This caused the global CSV path construction to incorrectly use the local CSV
  - Result: globally installed dependencies were re-downloaded locally
- Added verbose logging to `find-asdf-system-file` for debugging dependency resolution

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.14.3):

**Linux:**
- **Fedora/RHEL/CentOS**: `ocicl-2.14.3-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.14.3-1_amd64.deb`

**Windows:**
- **Installer**: `ocicl-2.14.3-setup.exe` (recommended)
- **MSI**: `ocicl-2.14.3.msi`
- **Chocolatey**: `choco install ocicl`
- **ZIP**: `ocicl-2.14.3-windows-amd64.zip`

**macOS:**
- **ARM64 (Apple Silicon)**: `ocicl-2.14.3-macos-arm64.tar.gz`
- **x64 (Intel)**: `ocicl-2.14.3-macos-x64.tar.gz`
- **Homebrew**: `brew install ocicl`
