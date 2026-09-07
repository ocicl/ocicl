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

<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:970c3bf2 -->
## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

**Architecture in one line:** issues live in a local Dolt DB; sync uses `refs/dolt/data` on your git remote; `.beads/issues.jsonl` is a passive export. See https://github.com/gastownhall/beads/blob/main/docs/SYNC_CONCEPTS.md for details and anti-patterns.

## Agent Context Profiles

The managed Beads block is task-tracking guidance, not permission to override repository, user, or orchestrator instructions.

- **Conservative (default)**: Use `bd` for task tracking. Do not run git commits, git pushes, or Dolt remote sync unless explicitly asked. At handoff, report changed files, validation, and suggested next commands.
- **Minimal**: Keep tool instruction files as pointers to `bd prime`; use the same conservative git policy unless active instructions say otherwise.
- **Team-maintainer**: Only when the repository explicitly opts in, agents may close beads, run quality gates, commit, and push as part of session close. A current "do not commit" or "do not push" instruction still wins.

## Session Completion

This protocol applies when ending a Beads implementation workflow. It is subordinate to explicit user, repository, and orchestrator instructions.

1. **File issues for remaining work** - Create beads for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **Handle git/sync by active profile**:
   ```bash
   # Conservative/minimal/default: report status and proposed commands; wait for approval.
   git status

   # Team-maintainer opt-in only, unless current instructions forbid it:
   git pull --rebase
   bd dolt push
   git push
   git status
   ```
5. **Hand off** - Summarize changes, validation, issue status, and any blocked sync/commit/push step

**Critical rules:**
- Explicit user or orchestrator instructions override this Beads block.
- Do not commit or push without clear authority from the active profile or the current user request.
- If a required sync or push is blocked, stop and report the exact command and error.
<!-- END BEADS INTEGRATION -->

<!-- BEGIN BEADS CODEX SETUP: generated by bd setup codex -->
## Beads Issue Tracker

Use Beads (`bd`) for durable task tracking in repositories that include it. Use the `beads` skill at `.agents/skills/beads/SKILL.md` (project install) or `~/.agents/skills/beads/SKILL.md` (global install) for Beads workflow guidance, then use the `bd` CLI for issue operations.

### Quick Reference

```bash
bd ready                # Find available work
bd show <id>            # View issue details
bd update <id> --claim  # Claim work
bd close <id>           # Complete work
bd prime                # Refresh Beads context
```

### Rules

- Use `bd` for all task tracking; do not create markdown TODO lists.
- Run `bd prime` when Beads context is missing or stale. Codex 0.129.0+ can load Beads context automatically through native hooks; use `/hooks` to inspect or toggle them.
- Keep persistent project memory in Beads via `bd remember`; do not create ad hoc memory files.

**Architecture in one line:** issues live in a local Dolt DB; sync uses `refs/dolt/data` on your git remote; `.beads/issues.jsonl` is a passive export. See https://github.com/gastownhall/beads/blob/main/docs/SYNC_CONCEPTS.md for details and anti-patterns.
<!-- END BEADS CODEX SETUP -->
