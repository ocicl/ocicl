---
name: writing-common-lisp
description: Use when creating or editing Common Lisp source files (.lisp/.asd) — setting up packages, declaring function types, choosing iteration/control-flow/condition constructs, or applying naming and formatting conventions in a Common Lisp project.
---

# Writing Common Lisp

## Overview
Conventions for new `.lisp`/`.asd` source in **package-inferred ASDF systems**: package definition, type declarations, control flow, conditions, and formatting. Apply across all CL projects in this workspace.

## When to Use
- Creating a new `.lisp` file or a `defpackage`/`uiop:define-package` form
- Adding a function, generic function, method, or condition to an existing file
- Reviewing CL code for style consistency

**When NOT to use:** a project's own `AGENTS.md`/`ai/cl-style.md` may override these — read it first. Never edit files under `.qlot/` (third-party dependencies).

## Package definition (package-inferred systems)
Each file is its own package, named after its full filesystem path:
`src/models/foo.lisp` under system `myapp` → package `myapp/src/models/foo`.

```lisp
(uiop:define-package #:myapp/src/models/foo
  (:use #:cl)
  (:import-from #:serapeum
                #->
                #:dict)
  (:export #:bar))
(in-package #:myapp/src/models/foo)
```

Rules:
- `uiop:define-package`, **not** `defpackage`.
- Package name **must match every path segment** including `src/` (most common baseline mistake: dropping it).
- `#:` prefix on every package and symbol name.
- `(:use #:cl)` only. For external libraries, **prefer `:local-nicknames` or targeted `:import-from`** — never `(:use #:some-lib)`, which floods your namespace and causes symbol clashes.
- Explicit `:export`; do not export internals.

`:local-nicknames` form:
```lisp
(:local-nicknames (#:ser #:serapeum))
```

## File layout
`;;;;` file comment → `(in-package ...)` as the **first executable form** → optional file-specific `declaim` → definitions. One blank line between top-level forms.

## Types
```lisp
(-> bar (number) (values number &optional))
(defun bar (x) ...)
```
Use `serapeum:->` for signatures. Use `(declare (ignore var))` for unused parameters.

## Control flow and iteration
- `loop` **without** keyword style — write `(loop for x in list while (...) do ...)`; do **not** use `:for`, `:do`, `:while`.
- `ecase` / `etypecase` for exhaustive dispatch; the error form uses `format`: `(ecase kind (... ...))`.

## Conditions and errors
```lisp
(define-condition my-error (error)
  ((reason :initarg :reason :reader my-error-reason))
  (:report (lambda (c s) (format s "My error: ~A" (my-error-reason c)))))
```
- `(error "format ~A" val)` with arguments — never a bare literal string.
- Prefer restarts over `signal`. **Never** runtime `eval`, `intern`, or `unintern`.

## Formatting
Google Common Lisp Style Guide: 2-space indent, ≤100 columns, no tabs, lower-case `lisp-case`. Predicates end in `-p`; specials are `*earmuffs*`; constants are `+plus+`. Docstrings are required on public APIs.

For exported variables and public extension points, the docstring should state the behavioral contract, not just the type. If a public hook list ignores non-funcallable elements, say so explicitly in the variable docstring.

## Common Mistakes
| Symptom | Fix |
|---|---|
| `defpackage` used | Switch to `uiop:define-package` |
| Package name misses a path segment (e.g. `myapp/models/foo` for file `src/models/foo.lisp`) | Include every segment: `myapp/src/models/foo` |
| `(:use #:serapeum)` or `(:use #:some-lib)` | Replace with `:local-nicknames` or `:import-from` of only the symbols used |
| Keyword-style `loop` (`:for`, `:do`, `:while`) | Drop the keywords: `(loop for x ... while ... do ...)` |
| `(error "boom")` bare string | `(error "boom: ~A" val)` with format args |
| Exported variable lacks a docstring or behavior contract | Add a docstring describing semantics, especially for hooks/configuration variables |

## Related skills
For CLOS classes and constructors: **defining-clos-classes**. For subtle language gotchas when writing new or large code: **avoiding-common-lisp-pitfalls**. For interactive reload after edits: **using-common-lisp-repl**.
