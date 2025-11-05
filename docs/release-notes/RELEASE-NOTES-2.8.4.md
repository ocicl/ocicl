# ocicl 2.8.4 Release Notes

## What's New

### Linter Improvements

**Refined Conditional Context Rules:**
- Improved `not-null` rule to only trigger in conditional contexts (if, when, unless, and, or, cond)
- Improved `use-serapeum-nand` rule to only trigger in conditional contexts
- These rules now avoid false positives when `(not (null ...))` or `(not (and ...))` are used to explicitly convert values to boolean T/NIL
- Examples of where rules no longer trigger:
  - `(setf result (not (null x)))` - intentional boolean conversion
  - `(return-from fn (not (and x y)))` - explicit T/NIL return value

**Why These Changes Matter:**
Sometimes developers intentionally use these patterns to ensure a function returns exactly T or NIL rather than a truthy/falsy value. The refined rules respect this intent while still catching cases where simplification is appropriate.

### Dependency Updates

Updated system registry with several new dependencies:
- documentation-utils
- lambda-fiddle
- legit
- serapeum (updated to newer version)
- simple-inferiors
- trivial-indent

## Breaking Changes

None. This release is fully backward compatible with 2.8.3.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.8.4):

- **Standalone binary**: `ocicl-linux-x86_64`
- **Fedora/RHEL/CentOS**: `ocicl-2.8.4-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.8.4-1_amd64.deb`
