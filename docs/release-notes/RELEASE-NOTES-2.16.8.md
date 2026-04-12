# ocicl 2.16.8 Release Notes

## Changes

### Support for authenticated/private OCI registries

ocicl now supports authenticated and private OCI registries, enabling
use with registries that require credentials (e.g., private GitHub
Container Registry, AWS ECR, Azure ACR).

### New OCICL_SYSTEMS_DIR environment variable

Added the `OCICL_SYSTEMS_DIR` environment variable to allow users to
customize where ocicl stores downloaded systems, instead of always
using the default location.

### Fixed missing ocicl.csv in web1 template

The `web1` project template was missing its `ocicl.csv` file, which
is now included.

### CI improvements

Added a 30-minute timeout to CI jobs to prevent runaway builds.

## Breaking Changes

None. This release is fully backward compatible with 2.16.7.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.16.8):

**Linux:**
- **Fedora/RHEL/CentOS**: `ocicl-2.16.8-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.16.8-1_amd64.deb`

**Windows:**
- **Installer**: `ocicl-2.16.8-setup.exe` (recommended)
- **MSI**: `ocicl-2.16.8.msi`
- **Chocolatey**: `choco install ocicl` (after community repo publication)
- **ZIP**: `ocicl-2.16.8-windows-amd64.zip`

**macOS:**
- **Homebrew**: `brew install ocicl`
- **ARM64**: `ocicl-2.16.8-macos-arm64.tar.gz`
- **x64**: `ocicl-2.16.8-macos-x64.tar.gz`
