# ocicl 2.8.5 Release Notes

## What's New

### Linter Improvements

**Refined needless-and-t Rule:**
- The `needless-and-t` rule now only triggers in conditional contexts (if, when, unless, or, cond test clauses)
- Previously flagged all `(AND ... T)` patterns, but sometimes trailing T is intentional to return exactly T instead of the last truthy value
- Examples of where the rule no longer triggers:
  - `(setf result (and x y t))` - intentional T return value
  - `(return-from fn (and condition t))` - explicit boolean conversion

### Dependency Updates

- Updated named-readtables to fix compatibility with SBCL 2.5.11 ([#177](https://github.com/ocicl/ocicl/issues/177))

## Breaking Changes

None. This release is fully backward compatible with 2.8.4.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.8.5):

- **Standalone binary**: `ocicl-linux-x86_64`
- **Fedora/RHEL/CentOS**: `ocicl-2.8.5-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.8.5-1_amd64.deb`
