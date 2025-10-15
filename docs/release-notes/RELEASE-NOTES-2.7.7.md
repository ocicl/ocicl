# ocicl 2.7.7 Release Notes

We're pleased to announce ocicl 2.7.7, a bug fix release addressing linter issues.

## What's Changed

### Linter Improvements

This release fixes two important issues with the ocicl linter:

- **Path Resolution Fix**: Fixed linter path resolution when running from a saved core image. Previously, the linter could fail to locate files correctly when ocicl was running from a compiled core image rather than source.

- **False Positive Fixes**: Eliminated false positives in linter warnings for:
  - Character literals (e.g., `#\Space`, `#\Newline`)
  - Lambda lists in function definitions

  These constructs were previously being incorrectly flagged by the linter, causing unnecessary noise in linting output.

## Installation

Install or upgrade ocicl via Homebrew:

```bash
brew install ocicl
# or
brew upgrade ocicl
```

Or from source:

```bash
git clone https://github.com/ocicl/ocicl.git
cd ocicl
git checkout v2.7.7
sbcl --load setup.lisp
```

## Breaking Changes

None. This release is fully backward compatible with 2.7.6.

## Contributors

Thanks to all contributors who helped make this release possible!

---

For more information, visit the [ocicl repository](https://github.com/ocicl/ocicl) or read the [documentation](https://github.com/ocicl/ocicl/blob/main/README.md).
