# ocicl 2.7.5 Release Notes

We're pleased to announce ocicl 2.7.5, a maintenance release focused on code quality improvements and library modernization.

## What's Changed

### Code Quality Improvements

This release represents a comprehensive effort to improve code quality across the entire ocicl codebase through systematic linting and refactoring:

- **Linting Infrastructure**: Added pre-commit hooks and GitHub Actions CI steps to automatically lint code, ensuring consistent quality standards going forward
- **Readability Enhancements**: Replaced legacy `CAR`/`CDR` calls with modern `FIRST`/`REST` equivalents for improved readability
- **Code Simplification**: Replaced complex `COND` statements with simpler `OR` expressions where appropriate
- **Best Practices**: Added proper default clauses to `COND` statements and optimized `MULTIPLE-VALUE-LIST` usage
- **Cleanup**: Fixed parenthesis counts, removed trailing whitespace, and addressed various linting issues throughout the codebase

### Library Modernization

Enhanced the linter with intelligent library suggestions to help developers write more idiomatic Common Lisp:

- **Alexandria Support**: Added suggestions for `-f` modify macros and other Alexandria utilities
- **Serapeum Integration**: Added high-priority Serapeum function suggestions
- **Smart Recommendations**: The linter now suggests modern library alternatives for common code patterns (opt-in via `.ocicl-lint.conf`)
- **Applied Improvements**: Applied library suggestions throughout the ocicl codebase, using utilities from Alexandria and Serapeum where appropriate
- **Selective Adoption**: Intentionally suppressed suggestions where adding dependencies wasn't justified

### Runtime and Setup Improvements

- **Boolean Checks**: Refactored runtime to use `uiop:file-exists-p` for cleaner boolean file existence checks
- **Setup Cleanup**: Cleaned up `setup.lisp` and enforced linting standards
- **Runtime Quality**: Comprehensive cleanup of `ocicl-runtime.lisp` with enforced linting

### Documentation

- **Contributor Guide**: Added `HACKING.md` with comprehensive guidelines for contributors
- **Consistency**: Standardized on lowercase 'ocicl' throughout documentation

### Dependency Updates

- Updated all dependency versions to their latest releases

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
git checkout v2.7.5
sbcl --load setup.lisp
```

## Breaking Changes

None. This release is fully backward compatible with 2.7.4.

## Contributors

Thanks to all contributors who helped make this release possible!

---

For more information, visit the [ocicl repository](https://github.com/ocicl/ocicl) or read the [documentation](https://github.com/ocicl/ocicl/blob/main/README.md).
