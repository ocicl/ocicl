# ocicl 2.16.0 Release Notes

## What's New

### Optional OpenSSL Support

ocicl now supports building with cl+ssl (OpenSSL) instead of pure-tls for TLS connections. This is useful for environments that require TLS 1.2 support or prefer OpenSSL.

To build with OpenSSL support:
```bash
USE_LEGACY_OPENSSL=1 sbcl --load setup.lisp
```

By default, ocicl continues to use pure-tls (pure Common Lisp TLS 1.3 implementation with no external dependencies).

### TLS Library Display

The `ocicl version` command now shows which TLS library is in use:

```
ocicl version:   v2.16.0
Lisp runtime:    SBCL 2.5.10 configured with 3GB memory
ASDF version:    3.3.7
TLS support:     pure-tls 1.9.0
```

### Updated Dependencies

- pure-tls updated to v1.9.0
- cl-selfupdate updated to latest version with USE_LEGACY_OPENSSL support

## Breaking Changes

None. This release is fully backward compatible with 2.15.x.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.16.0):

**Linux:**
- **Fedora/RHEL/CentOS**: `ocicl-2.16.0-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.16.0-1_amd64.deb`

**Windows:**
- **Installer**: `ocicl-2.16.0-setup.exe` (recommended)
- **MSI**: `ocicl-2.16.0.msi`
- **ZIP**: `ocicl-2.16.0-windows-amd64.zip`

**macOS:**
- **ARM64**: `ocicl-2.16.0-macos-arm64.tar.gz`
- **x64**: `ocicl-2.16.0-macos-x64.tar.gz`
- **Homebrew**: `brew install ocicl`
