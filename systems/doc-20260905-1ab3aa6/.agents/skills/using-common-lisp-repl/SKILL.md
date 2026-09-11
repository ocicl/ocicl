---
name: using-common-lisp-repl
description: Use when developing Common Lisp interactively against a running image reachable through an MCP eval tool (eval_lisp_form / eval-lisp-form) and you need to evaluate forms safely, reload a file after editing it, capture REPL output, or locate a dependency's sources on disk.
---

# Using the Common Lisp REPL

## Overview
When the project has a **running Lisp image** reachable through an MCP eval tool (`eval_lisp_form` / `eval-lisp-form`), prefer evaluating forms **in that live image** over shelling out to a fresh `sbcl`. This keeps shared state intact: loaded systems, in-memory data, open connections.

## When to Use
- You need to evaluate a form, inspect runtime state, or call a system function
- You edited a file and must reload it into the live image
- A form might hang and you must not block the image forever
- You need the on-disk path of an installed dependency's sources

**When NOT to use:** for a one-shot answer with no shared state and no connected MCP eval tool, a plain `sbcl --eval` is fine.

## Evaluate with a deadline (never hang forever)
Wrap any form that could loop or block:
```lisp
(sb-sys:with-deadline (:seconds 10)
  <your form>)
```
`sb-sys:with-deadline` is deadline-based and **propagates into nested calls** — prefer it over `sb-ext:with-timeout` for REPL/MCP contexts. On expiry it signals `sb-sys:deadline-timeout`.

## Reload a file after editing
For a package-inferred file `src/session.lisp` of system `myapp` (package `myapp/src/session`):
```lisp
(asdf:load-system "myapp" :force '("myapp/src/session"))
```
`:force` with a **list** recompiles only that file — cheaper than `:force t`, which forces the whole system.

### Redefining `defstruct` → `defclass` (or any structural change)
`(setf (find-class 'session) nil)` is **not** enough: the old accessors, constructor, and copier remain `fboundp` and will clash or shadow the new class. Delete the package first, then force-reload:
```lisp
(delete-package :myapp/src/session)
(asdf:load-system "myapp" :force '("myapp/src/session"))
```
Existing in-memory instances of the old type become orphaned — recreate them.

## Register a local project explicitly
If ASDF in the live image does not know about the current checkout, add it to the central registry first:
```lisp
(pushnew #P"/path/to/project/"
         asdf:*central-registry*
         :test #'equal)
```
Do this before `asdf:load-system` or `asdf:test-system` when the image was started outside the project wrapper.

## Capture output that goes to `*standard-output*`
Some forms (notably test runners) print their report to stdout and return a different value. Capture it:
```lisp
(with-output-to-string (*standard-output*)
  (rove:run-test 'my-system/tests::my-test))
```
See also **testing-with-rove**.

## Locate a dependency's sources
```lisp
(ql:where-is-system :defmain)
;; => #p"/.../quicklisp/dists/.../software/defmain-..."
```
Useful before reading or stepping through a Quicklisp-installed library. Never edit files under `.qlot/` — they are third-party.

## Docs/debugging workflow from the REPL
The live image is also useful for reproducing docs and test problems when wrapper scripts are missing locally:
```lisp
(asdf:test-system "myapp")
(docs-builder:build "myapp-docs")
```
Treat this as a diagnostic tool. A local REPL failure caused by SSL/network access to external docs is an environment issue unless CI reproduces it.

## Strict compilation check
After changing CLOS slots or schema classes, force a complete rebuild and run
the suite under high safety and debug settings. Scope the declaration with
`locally`:
```lisp
(locally
  (declare (optimize (debug 3) (safety 3)))
  (asdf:load-system :myapp :force t)
  (asdf:test-system :myapp))
```
Inspect source files for a lower file-local optimization declaration as well;
it can weaken checks within that compilation unit.

## Quick Reference
| Task | Form |
|---|---|
| Guard against hangs | `(sb-sys:with-deadline (:seconds 10) ...)` |
| Register local checkout | `(pushnew #P"/path/to/project/" asdf:*central-registry* :test #'equal)` |
| Reload one file | `(asdf:load-system "myapp" :force '("myapp/src/foo"))` |
| Redefine struct → class | `(delete-package :myapp/src/foo)` then `load-system` with `:force '("file")` |
| Capture stdout | `(with-output-to-string (*standard-output*) ...)` |
| Find lib sources | `(ql:where-is-system :lib)` |

## Common Mistakes
| Symptom | Fix |
|---|---|
| Used `sb-ext:with-timeout` | Prefer `sb-sys:with-deadline` — deadline propagates through nested calls and is the workspace convention |
| `asdf:test-system` / `load-system` says the system is missing | Add the checkout to `asdf:*central-registry*` first |
| `defstruct` → `defclass` errors, or old accessors linger after reload | `(delete-package :pkg)` first, then `load-system :force '("file")` — not just `(setf (find-class) nil)` |
| `:force t` recompiles the whole system | Pass a list of files: `:force '("myapp/src/foo")` |
| Spawned a new `sbcl` from the shell | Use the live image via the MCP eval tool instead |

## Related skills
For running tests from the REPL: **testing-with-rove**. For the style of the code you are reloading: **writing-common-lisp**. If reload still shows stale code, the fasl cache is stale — see **clearing-asdf-fasl-cache**.
