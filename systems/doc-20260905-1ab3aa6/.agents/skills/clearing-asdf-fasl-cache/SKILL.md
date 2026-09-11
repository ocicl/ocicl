---
name: clearing-asdf-fasl-cache
description: Use when ASDF fasl cache is stale and build tools (build-docs, qlot exec sbcl, etc.) do not pick up source changes — typically after modifying docstrings, adding defgeneric forms, or renaming exported symbols. Apply when recompilation shows no effect or warnings persist after source fixes.
---

# Clearing ASDF Fasl Cache

## Overview

ASDF compiles `.lisp` sources to `.fasl` (Fast-Load) files and caches them in `~/.cache/common-lisp/`. When source files change but tools use stale fasls, changes appear to have no effect. The fix is to delete the cache for the project using the `clear-fasl-cache.sh` script.

## When to Use

- Modified docstrings but `build-docs` still reports them as undocumented
- Added `defgeneric` forms but "No source location found" warnings persist
- Renamed or re-exported symbols but old names still referenced
- Build tools use a different SBCL image (standalone ros binary vs `qlot exec sbcl`)
- ASDF does not detect source changes after git operations (checkout, rebase)

## Usage

Run the script from the project root:

```bash
~/.config/opencode/skills/clearing-asdf-fasl-cache/clear-fasl-cache.sh
```

Or pass an explicit path:

```bash
~/.config/opencode/skills/clearing-asdf-fasl-cache/clear-fasl-cache.sh /path/to/project
```

The script removes ALL fasl cache directories for that project (any SBCL version, any architecture) and reports what was deleted.

## Common Issues

| Situation | Why | Fix |
|-----------|-----|-----|
| Build-docs still shows old warnings after source fix | Standalone ros binary may have its own fasl cache | Run the script — it removes all caches for the project |
| ASDF claims sources are up to date | File timestamps not sufficient to detect changes after git operations | Run the script, then touch a source file (`touch src/vars.lisp`) and rebuild |
| Only one sub-package affected | Package-inferred systems create many small fasls | Run the script — it clears all cached fasls for the project |
