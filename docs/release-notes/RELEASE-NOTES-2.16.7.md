# ocicl 2.16.7 Release Notes

## Changes

### Restored missing bundled systems

The v2.16.6 dependency update incorrectly removed transitive
dependencies still required for source builds (e.g., Homebrew).  This
release restores all missing systems: babel, bordeaux-threads, cffi,
chipz, chunga, cl-base64, cl-cancel, cl-ppcre, closer-mop,
documentation-utils, flexi-streams, global-vars, idna,
mgl-pax-bootstrap, named-readtables, precise-time, puri,
split-sequence, trivial-features, trivial-garbage, trivial-gray-streams,
trivial-indent, usocket, and atomics.

Fixes #193.

### Fixed Windows build failure

Guarded `sb-posix:mkdtemp` in cl-selfupdate with
`#+(and sbcl (not windows))` since this function is not available on
Windows SBCL.

## Breaking Changes

None. This release is fully backward compatible with 2.16.5.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.16.7):

**Linux:**
- **Fedora/RHEL/CentOS**: `ocicl-2.16.7-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.16.7-1_amd64.deb`

**Windows:**
- **Installer**: `ocicl-2.16.7-setup.exe` (recommended)
- **MSI**: `ocicl-2.16.7.msi`
- **Chocolatey**: `choco install ocicl` (after community repo publication)
- **ZIP**: `ocicl-2.16.7-windows-amd64.zip`

**macOS:**
- **Homebrew**: `brew install ocicl`
- **ARM64**: `ocicl-2.16.7-macos-arm64.tar.gz`
- **x64**: `ocicl-2.16.7-macos-x64.tar.gz`
