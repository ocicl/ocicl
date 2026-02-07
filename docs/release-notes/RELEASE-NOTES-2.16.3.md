# ocicl 2.16.3 Release Notes

## Bug Fixes

### Linter False Positives and Auto-Fixer Issues

Fixed five linter bugs reported in GitHub issues #185-189:

**1. Issue #185: malformed-let in backquoted macro definitions**
- Added `zip-in-backquote-p` to detect macro template contexts
- Updated `rule-let-validation` to skip validation inside backquoted forms
- Fixed false positive where `,db-var` in macro templates was incorrectly flagged as CONS

**Problem:**
```lisp
(defmacro with-db ((db-var) &body body)
  `(bt:with-lock-held (*db-lock*)
     (let ((,db-var (or *db* (open-db))))  ; <-- false positive here
       ...)))
```

**Solution:**
The linter now recognizes backquoted expressions as macro template code and doesn't parse commas as part of binding variables.

**2. Issue #186: redundant-block doesn't account for RETURN statements**
- Enhanced the `redundant-block` rule to walk the body and check for RETURN/RETURN-FROM statements
- A block is only flagged as redundant if it contains no return statements

**Problem:**
```lisp
(block nil
  (let ((x 42))
    (when (> x 10)
      (return))  ; <-- block is needed for this
    (do-something x)))
```

**Solution:**
The linter now checks for RETURN/RETURN-FROM statements before flagging a block as redundant.

**3. Issue #187: Auto-fixer generates invalid code for (progn ,@body)**
- Updated `fix-redundant-progn` to detect backquoted contexts and unquote-splicing forms
- Prevents attempting to fix `(progn ,@body)` in macro templates
- No longer generates invalid literal "unquote-splicing body" text

**Problem:**
The auto-fixer would transform `(progn ,@body)` to literal text `(unquote-splicing body)`, causing runtime errors.

**Solution:**
The fixer now skips `(progn ,@body)` patterns in backquoted contexts, recognizing they're needed in macros.

**4. Issue #188: redundant-progn in macros with &body**
- Updated the `redundant-progn` rule to detect unquote-splicing forms
- Correctly recognizes that `(progn ,@body)` is needed in macros since &body can splice multiple forms

**Problem:**
```lisp
(defmacro with-db ((db-var) &body body)
  `(unwind-protect
       (progn ,@body)  ; <-- not redundant, body can be multiple forms
     (cleanup)))
```

**Solution:**
The linter now skips the redundant-progn check for `(progn ,@body)` patterns.

**5. Issue #189: Auto-fixer generates broken COND indentation**
- Rewrote `fix-bare-progn-in-if` to use AST manipulation instead of manual string formatting
- Properly builds COND forms using rewrite-cl's AST nodes
- Eliminates indentation issues from manual string construction

**Problem:**
The fixer would generate COND forms with the `(t ...)` clause at incorrect indentation.

**Solution:**
Now uses proper AST building instead of string concatenation, producing correctly formatted code.

### Pre-commit Hook Update

- Updated the pre-commit hook to exclude the `systems/` directory
- Vendored third-party code is no longer linted during commits
- Only lints files in `lint/`, `src/`, and `test/` directories

## Breaking Changes

None. This release is fully backward compatible with 2.16.2.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.16.3):

**Linux:**
- **Fedora/RHEL/CentOS**: `ocicl-2.16.3-1.*.x86_64.rpm`
- **Debian/Ubuntu**: `ocicl_2.16.3-1_amd64.deb`

**Windows:**
- **Installer**: `ocicl-2.16.3-setup.exe` (recommended)
- **MSI**: `ocicl-2.16.3.msi`
- **Chocolatey**: `choco install ocicl` (after community repo publication)
- **ZIP**: `ocicl-2.16.3-windows-amd64.zip`

**macOS:**
- **Homebrew**: `brew install ocicl`
- **ARM64**: `ocicl-2.16.3-macos-arm64.tar.gz`
- **x64**: `ocicl-2.16.3-macos-x64.tar.gz`
