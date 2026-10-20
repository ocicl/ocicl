# Project Instructions for AI Agents

This file provides instructions and context for AI coding agents working on this project.

<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:6cd5cc61 -->
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
   git push
   git status
   ```
5. **Hand off** - Summarize changes, validation, issue status, and any blocked sync/commit/push step

**Critical rules:**
- Explicit user or orchestrator instructions override this Beads block.
- Do not commit or push without clear authority from the active profile or the current user request.
- If a required sync or push is blocked, stop and report the exact command and error.
<!-- END BEADS INTEGRATION -->


## Build & Test

See AGENTS.md ("Development Commands" and "Testing").

```bash
sbcl --load setup.lisp               # build with SBCL
egcl --no-init --load setup.lisp     # build with Evergreen Common Lisp (fetches egcl-forks.txt into egcl-systems/)
```

## Architecture Overview

_Add a brief overview of your project architecture_

## Conventions & Patterns

See AGENTS.md for the full project conventions. In particular:

- ocicl never uses the real cl+ssl (OpenSSL FFI). `ocicl.asd` loads `pure-tls/cl+ssl-compat` and registers `"cl+ssl"` as an immutable system, so dependencies that name `:cl+ssl` (Drakma) are satisfied by pure-tls. cl+ssl is deliberately not vendored. When loading Drakma or any part of ocicl's dependency tree outside `ocicl.asd` (REPL experiments, bisection scripts, other Lisps), do the same first: `(asdf:load-system :pure-tls/cl+ssl-compat) (asdf:register-immutable-system "cl+ssl")`. Otherwise ASDF signals MISSING-DEPENDENCY for cl+ssl, which is a test-setup error, not a real failure. `USE_LEGACY_OPENSSL=1` is the only path that uses real cl+ssl.
