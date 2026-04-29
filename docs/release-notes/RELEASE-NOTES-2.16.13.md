# ocicl 2.16.13 Release Notes

**Release Date:** April 2026

## Summary

Bug fix release: GPG signing keys for RPM and DEB package repositories are now correctly published.

## Bug Fixes

- **Fixed GPG key deployment for package repositories**: The RPM and DEB repository GPG public keys were not being uploaded during the release workflow, so `RPM-GPG-KEY-ocicl` and the DEB archive keyring were unavailable from the package repo URLs. Users configuring `dnf`/`apt` repos with GPG verification would fail to fetch the signing key.

## New Features

- **Full CI/release/packaging infrastructure in project templates**: The `cli` and `web1` templates created by `ocicl new` now include GitHub Actions workflows for CI testing, release builds (RPM, DEB, tarballs, Windows installers), SBOM generation, GPG signing, and GitHub Pages-hosted package repositories.

## Breaking Changes

None. This release is fully backward compatible with 2.16.12.

## Upgrade Notes

Drop-in replacement for 2.16.12.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.16.13):

**Linux:**
- **Fedora/RHEL/CentOS**: `sudo dnf install ocicl` (via repo) or download RPM
- **Debian/Ubuntu**: `sudo apt install ocicl` (via repo) or download DEB

**Windows:**
- **Installer**: `ocicl-2.16.13-setup.exe` (recommended)
- **MSI**: `ocicl-2.16.13.msi`
- **Chocolatey**: `choco install ocicl`
- **ZIP**: `ocicl-2.16.13-windows-amd64.zip`

**macOS:**
- **Homebrew**: `brew install ocicl`
- **ARM64**: `ocicl-2.16.13-macos-arm64.tar.gz`
- **x64**: `ocicl-2.16.13-macos-x64.tar.gz`
