# ocicl 2.16.4 Release Notes

## Bug Fixes

### Linter Improvements

**1. Issue #159: Improved defvar-without-value message**
- Updated the `defvar-without-value` linter message to be more helpful
- Now suggests using `(DECLAIM (SPECIAL ...))` for forward declarations
- Provides clearer guidance on when DEFVAR without a value is appropriate

**Problem:**
The previous message "DEFVAR without initial value" didn't explain why this might be an issue or what the alternatives are.

**Solution:**
New message: "DEFVAR without initial value - consider (DECLAIM (SPECIAL ...)) for forward declarations"

**2. Issues #164 & #165: Disabled semantically incorrect rules**
- Disabled the `if-or` rule that suggested replacing `(if (or ...) t nil)` with `(or ...)`
- Disabled the `cond-or` rule that suggested replacing `(cond ((or ...) t))` with `(or ...)`
- Both transformations change semantics: T (always true) vs generalized boolean (implementation-dependent)

**Problem:**
```lisp
(if (or condition1 condition2) t nil)  ; Always returns T or NIL
(or condition1 condition2)             ; Returns the true value, not T
```

The transformation changes the return value from boolean T to a generalized boolean (the actual true value), which can break code that depends on canonical T.

**Solution:**
Both rules have been disabled to prevent incorrect transformations. While context-sensitive analysis could detect safe cases, the complexity doesn't justify the benefit.

## Breaking Changes

None. This release is fully backward compatible with 2.16.3.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.16.4):

**Linux:**
- **Fedora/RHEL/CentOS**: `ocicl-2.16.4-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.16.4-1_amd64.deb`

**Windows:**
- **Installer**: `ocicl-2.16.4-setup.exe` (recommended)
- **MSI**: `ocicl-2.16.4.msi`
- **Chocolatey**: `choco install ocicl` (after community repo publication)
- **ZIP**: `ocicl-2.16.4-windows-amd64.zip`

**macOS:**
- **Homebrew**: `brew install ocicl`
- **ARM64**: `ocicl-2.16.4-macos-arm64.tar.gz`
- **x64**: `ocicl-2.16.4-macos-x64.tar.gz`
