# ocicl 2.15.0 Release Notes

## What's New

### Self-Update Feature

ocicl can now update itself directly from GitHub releases using the new `update` command:

```bash
# Check for updates
ocicl update --check

# Update to latest version
ocicl update

# Include prereleases
ocicl update --prerelease
```

The update command downloads the appropriate binary for your platform, validates the checksum, and atomically replaces the current executable with automatic rollback on failure.

### Command-Specific Options

CLI options are now properly scoped to their respective commands. Options like `--depth` for `tree` and `--dry-run` for `update` are now validated per-command rather than globally, providing clearer error messages for invalid option combinations.

Each command with options now supports `--help` for command-specific help:

```bash
ocicl tree --help
ocicl update --help
ocicl lint --help
```

## Bug Fixes

- Fixed version display showing "Current version: NIL" during self-update checks
- Improved error handling for malformed version strings

## Breaking Changes

None.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.15.0):

**Linux:**
- **Fedora/RHEL/CentOS**: `ocicl-2.15.0-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.15.0-1_amd64.deb`

**Windows:**
- **Installer**: `ocicl-2.15.0-setup.exe` (recommended)
- **MSI**: `ocicl-2.15.0.msi`
- **Chocolatey**: `choco install ocicl`
- **ZIP**: `ocicl-2.15.0-windows-amd64.zip`

**macOS:**
- **ARM64 (Apple Silicon)**: `ocicl-2.15.0-macos-arm64.tar.gz`
- **x64 (Intel)**: `ocicl-2.15.0-macos-x64.tar.gz`
- **Homebrew**: `brew install ocicl`
