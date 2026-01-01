# ocicl 2.11.0 Release Notes

## What's New

### Lowercase Symbol Output in Lint Fixes

The `ocicl lint --fix` command now outputs symbols in lowercase, matching Common Lisp conventions:

```lisp
;; Before: (= 0 x) was fixed to (ZEROP X)
;; Now:    (= 0 x) is fixed to (zerop x)
```

This applies to all auto-fix rules that generate new code, including:
- `use-zerop`: `(= x 0)` → `(zerop x)`
- `when-for-unless`, `unless-for-when`
- `incf`/`decf` conversions
- All other AST-based fixes

## Bug Fixes

- Fixed uppercase symbol output in lint auto-fixes (symbols now output as lowercase)

## Breaking Changes

None. This release is fully backward compatible with 2.10.x.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.11.0):

**Linux:**
- **Fedora/RHEL/CentOS**: `ocicl-2.11.0-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.11.0-1_amd64.deb`

**Windows:**
- **Installer**: `ocicl-2.11.0-setup.exe` (recommended)
- **MSI**: `ocicl-2.11.0.msi`
- **Chocolatey**: `choco install ocicl`
- **ZIP**: `ocicl-2.11.0-windows-amd64.zip`

**macOS:**
- **Homebrew**: `brew install ocicl`
