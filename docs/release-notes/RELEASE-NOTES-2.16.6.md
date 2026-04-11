# ocicl 2.16.6 Release Notes

## Changes

### Updated dependencies

- Removed unused dependencies (atomics, babel, bordeaux-threads, cffi, chipz, chunga, cl-base64, cl-cancel, cl-plus-ssl, cl-ppcre, closer-mop, documentation-utils, flexi-streams, global-vars, idna, iolib, mgl-pax, named-readtables, precise-time, puri, split-sequence, swap-bytes, trivial-features, trivial-garbage, trivial-gray-streams, trivial-indent, usocket)
- Updated cl-selfupdate, drakma, pure-tls, and serapeum to latest versions

### Hardened GitHub Actions workflows

- Pinned all GHA actions to commit SHAs instead of mutable tags
- Added explicit permissions blocks to all workflows
- Fixed potential script injection vulnerabilities

### Documentation

- Added instructions on maintaining the ocicl repos

## Breaking Changes

None. This release is fully backward compatible with 2.16.5.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.16.6):

**Linux:**
- **Fedora/RHEL/CentOS**: `ocicl-2.16.6-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.16.6-1_amd64.deb`

**Windows:**
- **Installer**: `ocicl-2.16.6-setup.exe` (recommended)
- **MSI**: `ocicl-2.16.6.msi`
- **Chocolatey**: `choco install ocicl` (after community repo publication)
- **ZIP**: `ocicl-2.16.6-windows-amd64.zip`

**macOS:**
- **Homebrew**: `brew install ocicl`
- **ARM64**: `ocicl-2.16.6-macos-arm64.tar.gz`
- **x64**: `ocicl-2.16.6-macos-x64.tar.gz`
