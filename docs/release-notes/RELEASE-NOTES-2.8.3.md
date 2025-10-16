# ocicl 2.8.3 Release Notes

## What's New

### Critical Bug Fixes

**Debian Package Binary Stripping Fixed:**
- Fixed critical bug where Debian packaging was stripping the ocicl binary
- Stripping corrupted the embedded SBCL core, causing "Can't find sbcl.core" errors at runtime
- Added `override_dh_strip` to debian/rules to prevent binary stripping
- DEB packages now work correctly out of the box

### License Collection Improvements

**Enhanced License Scanning:**
- Refactored license scanning code for improved maintainability and extensibility
- Added support for finding licenses in subdirectories
- Improved detection of license information in block comments
- Fixed .asd file selection by properly handling version suffixes in directory names
- Migrated from shell wildcards to UIOP functions for more reliable file discovery
- Improved .asd file selection logic and simplified license file pattern matching

**Better Edge Case Handling:**
- Enhanced license collection to handle various edge cases
- Added support for LICENSES directories (plural)
- Added encoding fallback for non-UTF-8 encoded LICENSE files

### Bug Fixes

- Fixed type error in diff command when using string arguments

## Breaking Changes

None. This release is fully backward compatible with 2.8.2.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.8.3):

- **Standalone binary**: `ocicl-linux-x86_64`
- **Fedora/RHEL/CentOS**: `ocicl-2.8.3-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.8.3-1_amd64.deb`
