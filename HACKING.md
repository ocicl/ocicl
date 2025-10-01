# Contributing to ocicl

This guide covers how to set up your development environment and contribute to ocicl.

## Development Setup

### Prerequisites

- SBCL (Steel Bank Common Lisp)
- Git
- Basic familiarity with Common Lisp

### Getting Started

1. **Clone the repository**
   ```bash
   git clone https://github.com/ocicl/ocicl.git
   cd ocicl
   ```

2. **Set up Git hooks**
   ```bash
   git config core.hooksPath .githooks
   ```
   This enables the pre-commit hook that runs the linter on staged files.

3. **Build ocicl**
   ```bash
   sbcl --load setup.lisp
   ```
   This creates the `./ocicl` binary in your working directory.

## Development Workflow

### Making Changes

1. Make your changes to the source code
2. Test your changes by rebuilding:
   ```bash
   sbcl --load setup.lisp
   ```
3. Run the linter on your changes:
   ```bash
   ./ocicl lint <file-or-directory>
   ```

### Code Quality

ocicl uses a built-in linter to maintain code quality. All code must pass linting before being committed.

#### Running the Linter

```bash
# Lint a single file
./ocicl lint myfile.lisp

# Lint a directory
./ocicl lint lint/

# Lint the entire project
./ocicl lint ocicl.asd
```

#### Linting Rules

The linter checks for:
- Code style (whitespace, indentation, parentheses)
- Common Lisp best practices (FIRST/REST vs CAR/CDR, EQL vs EQ)
- License headers (SPDX identifiers)
- Package declarations
- Naming conventions
- Code smells and anti-patterns

#### Suppressing Lint Warnings

If you have a legitimate reason to violate a lint rule, you can suppress it with a comment:

```lisp
(car my-list)  ; lint:suppress use-first-rest
```

To suppress all rules on a line:
```lisp
(some-code)  ; lint:suppress
```

### Pre-commit Hook

The pre-commit hook automatically runs the linter on staged `.lisp` files before each commit. If linting fails, the commit is blocked.

To bypass the hook in exceptional cases:
```bash
git commit --no-verify
```

### Testing

After making changes:

1. Rebuild the binary
2. Run basic smoke tests:
   ```bash
   ./ocicl version
   ./ocicl help
   ```
3. Test relevant functionality for your changes

## Coding Style

- Follow Common Lisp naming conventions:
  - `*special-variables*` with asterisks
  - `+constants+` with plus signs (or use `defconstant`)
  - `function-names` with hyphens
- Keep lines under 120 characters (configurable)
- Use FIRST/REST instead of CAR/CDR for better readability
- Prefer EQL over EQ for comparing symbols, numbers, and characters
- Include SPDX license identifiers in file headers
- Use `(in-package #:package-name)` at the start of files

## Project Structure

- `ocicl.lisp` - Main CLI entry point
- `ocicl.asd` - ASDF system definition
- `lint/` - Linter implementation
  - `lint/rules/` - Individual linting rules
  - `lint/linter.lisp` - Core linting functionality
  - `lint/config.lisp` - Configuration handling
- `runtime/` - Runtime files embedded in the binary
- `templates/` - Project templates

## Submitting Changes

1. Make sure your changes pass all lint checks
2. Write clear, descriptive commit messages
3. Test your changes thoroughly
4. Submit a pull request with:
   - Description of what changed and why
   - Any relevant issue numbers
   - Test results

## Getting Help

- Report issues at: https://github.com/ocicl/ocicl/issues
- For questions about development, open a discussion or issue

## License

ocicl is distributed under the MIT License. All contributions must be compatible with this license.
