# Agent Notes for This Repository

Scope: This file applies to the entire repository.

General
- Prefer small, focused commits; multi-line messages encouraged (subject, then detailed bullets).
- Run `parlinter -l ocicl.lisp` before committing to catch paren/formatting issues.

Formatting & Strings
- Don’t use `#.#?` (read-time evaluation of interpolated strings) with runtime variables. It causes compile-time reader errors.
- Avoid dynamic format control strings. Use a constant control string and pass arguments, or pass a single prebuilt string as an argument, e.g. `(format t "~a~%" #?"…")`.
- Colorized output conventions: pass color strings as args with a constant control string, or use `#?` only for the argument value (not the control string).

Templates
- Always overwrite existing template files when installing/updating built-in templates. Do not add a preserve/force-only mode; the intended behavior is to refresh unconditionally.

TLS & Proxies
- TLS verification must be ON by default. Provide `-k/--insecure` and `OCICL_INSECURE` only for debugging.
- Respect `OCICL_CA_FILE` and `OCICL_CA_DIR` for custom trust roots.
- Proxy configuration is read from `HTTPS_PROXY`/`HTTP_PROXY` (`NO_PROXY` supported). Proxy Basic auth is derived from `user:pass@host` in the proxy URI. Configure Drakma proxy settings at startup.

OCI Layer Selection
- Our OCI images are tarballs of source. When resolving a tag:
  - If the manifest lists `:layers`, pick the digest from the first layer.
  - If it’s an index (`:manifests`), pick the first child manifest, fetch it, then pick its first layer’s digest.
  - No mediaType or platform preference is needed.

I/O & Files
- Write CSVs atomically: write to a temp file in the same directory, then `uiop:rename-file-overwriting-target` into place.
- Use `uiop:ensure-all-directories-exist` before writing.

HTTP Client
- Use Drakma via the `ocicl.http` shim; default to `:verify :required`.
- Pass `:proxy-basic-authorization` when credentials exist.
- Consider adding timeouts and retries for robustness; keep semantics idempotent for GETs.

---

Project Overview

OCICL is a modern alternative to Quicklisp for Common Lisp system distribution and management. It uses OCI-compliant artifacts distributed via container registries with secure TLS distribution and sigstore integrity verification.

Development Commands
- Build and install: `sbcl --load setup.lisp`
- Binary location: `~/.local/bin/ocicl` (after installation)
- Memory configuration: Uses 3072MB dynamic space by default
- Better error messages: `sbcl --eval "(asdf:load-system :ocicl)"` gives clearer compilation errors than setup.lisp

Testing
- CI tests run via GitHub Actions (`.github/workflows/ci.yaml`)
- Manual testing: Run individual test applications in `*-test/` directories
- Template testing: Each template in `templates/` has its own Makefile

Testing Self-Update
- The version string comes from `version-string` library which uses git tags if HEAD has one
- To test `ocicl update` with an older version, delete the local git tag:
  ```bash
  git tag -d v2.15.1        # delete local tag
  sbcl --load setup.lisp    # rebuild - version falls back to .asd + git hash
  ./ocicl version           # shows e.g. "2.15.0-g07df634+dirty"
  ./ocicl update --check    # now detects v2.15.1 as available
  ```
- After testing, restore the tag: `git fetch --tags`

Key OCICL Commands for Development Testing
- `ocicl install SYSTEM` - Install a system locally
- `ocicl setup` - Configure runtime environment
- `ocicl new APP-NAME [TEMPLATE]` - Create new project from template
- `ocicl list SYSTEM` - Show available system versions

Architecture

Core Components
- `ocicl.lisp` - Main application logic (currently v2.6.5)
- `runtime/ocicl-runtime.lisp` - Runtime system embedded in Lisp images
- `templates/` - Project templates (basic, cli, web1)
- `http.lisp` - HTTP utilities using Drakma

System Dependencies
Uses ASDF exclusively. Key dependencies include:
- `:with-user-abort`, `:unix-opts`, `:drakma`, `:cl-json` (core)
- `:tar`, `:copy-directory` (archives)
- `:diff`, `:cl-template`, `:version-string` (utilities)

Template System
- Templates use `cl-template` for variable substitution
- Syntax: `{{app-name}}` and `<%= @ author %>`
- Templates are embedded in the binary but can be overridden

Key Architectural Patterns

Package Distribution
- Uses CSV metadata files (`ocicl.csv`) for system information
- Parent directory inheritance for package resolution
- Local vs global system installation modes
- OCI artifact storage with GPG signature verification

Runtime Integration
- Runtime automatically discovers and loads systems
- ASDF integration with bundled ASDF version
- Memory-optimized for embedded use in applications

File Discovery
The `find-asd-files` function (in `ocicl.lisp`, around line ~600) excludes directories:
- Hidden directories (starting with `.`)
- `_build/`, `_darcs/`, `.git/`, `.svn/`, etc.
- Build artifact directories

Development Notes

Recent Changes
- Enhanced `.asd` file searching logic with better directory exclusion
- Improved path resolution for runtime and template files
- Version currently at 2.6.5

Build System
- No traditional Makefile in root directory
- Uses SBCL-specific compilation with core compression
- CI/CD runs on Ubuntu, Windows, and MacOS with proxy testing

Security
- GPG signature verification for all packages
- Sigstore transparency log integration
- TLS-only distribution via container registries
