# ocicl 2.9.0 Release Notes

## What's New

### Windows and macOS Binary Releases

ocicl now provides pre-built binaries for Windows and macOS in addition to Linux:

**Windows packages:**
- `ocicl-2.9.0-windows-amd64.zip` - Standalone binary
- `ocicl-2.9.0-setup.exe` - NSIS installer with optional PATH configuration
- `ocicl-2.9.0.msi` - Windows Installer package
- `ocicl.2.9.0.nupkg` - Chocolatey package

**macOS packages:**
- `ocicl-2.9.0-macos-arm64.tar.gz` - Apple Silicon (M1/M2/M3)
- `ocicl-2.9.0-macos-x64.tar.gz` - Intel Macs

### Bug Fixes

- **Fixed format directive in remove command**: The `ocicl remove` command now correctly displays messages when systems don't exist or have dependents. Previously, the format directive could produce garbled output due to incorrect list flattening.

### Template Improvements

- Added package documentation strings to all project templates (basic, cli, web1)
- Added docstrings to `make-app` functions in cli and web1 templates
- Fixed unused variable warning in cli template handler
- Renamed `+static-dispatch-table+` to `*static-dispatch-table*` in web1 template to follow Lisp naming conventions for special variables

### CI/CD Improvements

- Windows is now included in the CI test matrix
- All tests pass on Linux, macOS, and Windows
- Added Test Package Building workflow for pre-release validation

## Breaking Changes

None. This release is fully backward compatible with 2.8.x.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.9.0):

**Linux:**
- **Fedora/RHEL/CentOS**: `ocicl-2.9.0-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.9.0-1_amd64.deb`

**Windows:**
- **Installer**: `ocicl-2.9.0-setup.exe` (recommended)
- **MSI**: `ocicl-2.9.0.msi`
- **Chocolatey**: `choco install ocicl` (after community repo publication)
- **ZIP**: `ocicl-2.9.0-windows-amd64.zip`

**macOS:**
- **Apple Silicon**: `ocicl-2.9.0-macos-arm64.tar.gz`
- **Intel**: `ocicl-2.9.0-macos-x64.tar.gz`
