# ocicl 2.16.11 Release Notes

**Release Date:** April 2026

## Summary

Security update: pure-tls 1.11.1 fixes three certificate verification and TLS post-handshake vulnerabilities.

## Security

### pure-tls 1.11.1 (updated from 1.11.0)

Three security issues fixed in certificate verification and post-handshake handling:

- **Trust-anchor forgery** — Trust-anchor matching now requires
  cryptographic signature verification, not just issuer-name equality,
  preventing forged intermediates from satisfying the anchor check.
- **TLS 1.3 message fragmentation** — Post-handshake messages now use
  the reassembly buffer to correctly handle TLS 1.3 message
  fragmentation and coalescing across records.
- **Wildcard hostname bypass** — Wildcard hostname validation now
  rejects known multi-label public suffixes (e.g., `*.co.uk`) that were
  previously accepted due to insufficient dot-counting.

## Updated Dependencies

- **drakma** updated (new build)
- **cl-selfupdate** updated to 20260412-ee7dcdc
- **40ants-doc** updated to 20260411-5b0d0ba

## Breaking Changes

None. This release is fully backward compatible with 2.16.10.

## Upgrade Notes

Drop-in replacement for 2.16.10.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.16.11):

**Linux:**
- **Fedora/RHEL/CentOS**: `sudo dnf install ocicl` (via repo) or download RPM
- **Debian/Ubuntu**: `sudo apt install ocicl` (via repo) or download DEB

**Windows:**
- **Installer**: `ocicl-2.16.11-setup.exe` (recommended)
- **MSI**: `ocicl-2.16.11.msi`
- **Chocolatey**: `choco install ocicl`
- **ZIP**: `ocicl-2.16.11-windows-amd64.zip`

**macOS:**
- **Homebrew**: `brew install ocicl`
- **ARM64**: `ocicl-2.16.11-macos-arm64.tar.gz`
- **x64**: `ocicl-2.16.11-macos-x64.tar.gz`
