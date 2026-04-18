# ocicl 2.16.12 Release Notes

**Release Date:** April 2026

## Summary

Packaging improvements: SPDX SBOM included in RPM and DEB packages, DEB apt warning fixed.

## New Features

### SBOM in Linux packages

RPM and DEB packages now include an SPDX SBOM (Software Bill of Materials)
installed at `/usr/share/sbom/ocicl-2.16.12.spdx.json`. This enables
system-level SBOM aggregation and vulnerability correlation tooling.

## Bug Fixes

- Fixed missing `Date` field in DEB repository Release file that caused
  `apt update` to emit a warning ([#194](https://github.com/ocicl/ocicl/issues/194)).

## Breaking Changes

None. This release is fully backward compatible with 2.16.11.

## Upgrade Notes

Drop-in replacement for 2.16.11.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.16.12):

**Linux:**
- **Fedora/RHEL/CentOS**: `sudo dnf install ocicl` (via repo) or download RPM
- **Debian/Ubuntu**: `sudo apt install ocicl` (via repo) or download DEB

**Windows:**
- **Installer**: `ocicl-2.16.12-setup.exe` (recommended)
- **MSI**: `ocicl-2.16.12.msi`
- **Chocolatey**: `choco install ocicl`
- **ZIP**: `ocicl-2.16.12-windows-amd64.zip`

**macOS:**
- **Homebrew**: `brew install ocicl`
- **ARM64**: `ocicl-2.16.12-macos-arm64.tar.gz`
- **x64**: `ocicl-2.16.12-macos-x64.tar.gz`
