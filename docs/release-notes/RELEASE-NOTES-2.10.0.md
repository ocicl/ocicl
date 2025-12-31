# ocicl 2.10.0 Release Notes

## What's New

### Lint Auto-Fix Feature

The `ocicl lint` command now supports automatic code fixes:

```bash
# Automatically fix issues
ocicl lint --fix file.lisp

# Preview what would be fixed without modifying files
ocicl lint --dry-run file.lisp
```

**53 auto-fixable rules** covering:
- **Whitespace**: trailing whitespace, tabs, paren spacing, blank lines, final newline
- **Symbols**: `CAR`→`FIRST`, `CDR`→`REST`, `EQ`→`EQL`, `SETQ`→`SETF`
- **Quotes**: quoted nil/keywords/numbers/T
- **List operations**: cons patterns, append simplification, acons
- **Logic**: needless and/or, null checks, consp
- **Conditionals**: when/unless, if simplifications, cond→if, bare-progn-in-if
- **Arithmetic**: 1+/1-, zerop, incf/decf, add-zero
- **Functions**: identity, constantly, shiftf
- **ASDF**: system name strings
- **Bindings**: `LET*` with single binding → `LET`
- **Case clauses**: `T` → `OTHERWISE`

### RCS Backup for Fixes

When `--fix` modifies a file, ocicl automatically creates a backup:
- Uses RCS (Revision Control System) if `ci`/`co` are available
- Creates `RCS/` directory automatically if needed
- Falls back to `.bak` files otherwise

### AST-Based Lint Rules

The linter now uses `rewrite-cl` zippers for source-preserving AST manipulation:
- Fixes preserve original formatting, comments, and package prefixes
- More accurate detection of code patterns
- Better handling of edge cases

## Bug Fixes

- Fixed issue count when multiple issues at the same position are fixed
- Fixed plural handling in lint output messages
- Fixed closing-parens-same-line detection to ignore cases with comments between parens

## Breaking Changes

None. This release is fully backward compatible with 2.9.x.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.10.0):

**Linux:**
- **Fedora/RHEL/CentOS**: `ocicl-2.10.0-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.10.0-1_amd64.deb`

**Windows:**
- **Installer**: `ocicl-2.10.0-setup.exe` (recommended)
- **MSI**: `ocicl-2.10.0.msi`
- **Chocolatey**: `choco install ocicl`
- **ZIP**: `ocicl-2.10.0-windows-amd64.zip`

**macOS:**
- **Homebrew**: `brew install ocicl`
