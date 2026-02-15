# ocicl 2.16.5 Release Notes

## Bug Fixes

### Fix libyear regression reporting 0.00 for all systems

The `ocicl libyear` command was silently reporting 0.00 libyears for all systems due to two regressions:

**1. Wrong lookup key in project dedup hash**
- `do-libyear` stored the OCI image repository name (e.g., `ag-proto-cli`) extracted from the image reference, then used it for both the `*ocicl-systems*` lookup and the registry tag query
- Since `*ocicl-systems*` is keyed by system name (e.g., `ag-grpc`), the lookup failed silently whenever the repo name differed from the system name
- Fixed by storing the system name directly, matching how `do-changes` works

**2. Version fallback only extracted git hash, not full tag**
- When the `_00_OCICL_VERSION` file is absent (e.g., systems directory not present), `get-project-version` fell back to extracting the substring after the last `-` in the directory name
- For `ag-gRPC-20260214-dd7b561`, this gave `dd7b561` instead of `20260214-dd7b561`
- The truncated version never matched any registry tag, so `get-versions-since` always returned NIL
- The fallback now detects the `-YYYYMMDD` date pattern and extracts the full version (e.g., `20260214-dd7b561`)

### Lint: extend cond-vs-if fixer

- Extended `fix-cond-vs-if` to handle two-clause COND with `(t ...)` as the second clause, converting to IF with proper indentation
- Added `coerce-to-node-formatted` for indented IF/PROGN output in lint fixers

## Breaking Changes

None. This release is fully backward compatible with 2.16.4.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.16.5):

**Linux:**
- **Fedora/RHEL/CentOS**: `ocicl-2.16.5-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.16.5-1_amd64.deb`

**Windows:**
- **Installer**: `ocicl-2.16.5-setup.exe` (recommended)
- **MSI**: `ocicl-2.16.5.msi`
- **Chocolatey**: `choco install ocicl` (after community repo publication)
- **ZIP**: `ocicl-2.16.5-windows-amd64.zip`

**macOS:**
- **Homebrew**: `brew install ocicl`
- **ARM64**: `ocicl-2.16.5-macos-arm64.tar.gz`
- **x64**: `ocicl-2.16.5-macos-x64.tar.gz`
