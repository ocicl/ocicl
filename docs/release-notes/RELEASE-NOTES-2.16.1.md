# ocicl 2.16.1 Release Notes

## Bug Fixes

### TLS Connection Failures on Fedora/RHEL 9+ (Issue #190)

Fixed package installation failures on Fedora/RHEL 9+ systems where pure-tls couldn't find system CA certificates:

**Fixes:**
- Added Fedora/RHEL 9+ CA path (`/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem`)
- Improved error messages with actionable solutions for TLS failures
- Added diagnostic test script for debugging TLS issues
- Added CI tests to verify TLS connectivity

**New environment variable:**
- **`OCICL_TLS_DEBUG`** - Set to `1` to enable TLS debugging output showing:
  - Which TLS implementation is in use (pure-tls or cl+ssl)
  - Which CA certificate file/directory is being used
  - Number of CA certificates loaded

**Usage:**
```bash
export OCICL_TLS_DEBUG=1
ocicl install <package>
```

### SBOM Security and Code Quality (Issue #191)

Improved Software Bill of Materials (SBOM) generation with proper cryptographic hashing:

**Upgraded from MD5 to SHA-256:**
- SBOM checksums now use SHA-256 instead of MD5
- SHA-256 is the recommended algorithm per CycloneDX standards for supply chain security
- Uses ironclad library for cross-implementation compatibility
- MD5 is cryptographically broken and no longer suitable for integrity verification

**Code cleanup:**
- Removed unused `digest` variable
- Removed redundant "warm-up" hash calculation that wasted I/O
- Simplified function from ~12 lines to 4 lines
- Function now actually calculates SHA-256 (previously named calculate-sha256 but computed MD5)
- Removed obsolete `sb-md5` dependency (SBCL-only)

**Dependencies:**
- Added `:ironclad` for proper cryptographic hashing

## Breaking Changes

None. This release is fully backward compatible with 2.16.0.

However, SBOM output format changes:
- Hash algorithm field now shows "SHA-256" instead of "MD5"
- Hash values will be different (SHA-256 produces 256-bit vs MD5's 128-bit hashes)
- Tools consuming SBOM output should handle the algorithm field correctly

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.16.1):

**Linux:**
- **Fedora/RHEL/CentOS**: `ocicl-2.16.1-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.16.1-1_amd64.deb`

**Windows:**
- **Installer**: `ocicl-2.16.1-setup.exe` (recommended)
- **MSI**: `ocicl-2.16.1.msi`
- **Chocolatey**: `choco install ocicl` (after community repo publication)
- **ZIP**: `ocicl-2.16.1-windows-amd64.zip`

**macOS:**
- **Homebrew**: `brew install ocicl`
- **ARM64**: `ocicl-2.16.1-macos-arm64.tar.gz`
- **x64**: `ocicl-2.16.1-macos-x64.tar.gz`
