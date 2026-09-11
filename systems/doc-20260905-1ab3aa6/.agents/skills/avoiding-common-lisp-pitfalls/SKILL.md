---
name: avoiding-common-lisp-pitfalls
description: Use when writing new or large Common Lisp code where subtle library or language gotchas are likely — event-emitter argument order, parenthesis imbalance in a big file, or a defmethod whose specializers may silently collide with one in a dependency. Also apply when wiring defmain subcommands.
---

# Avoiding Common Lisp Pitfalls

## Overview
Verified, non-obvious CL traps: the `event-emitter` library's inconsistent argument order, silent CLOS method replacement, parenthesis imbalance in large files, and `defmain` subcommand quirks.

## When to Use
- Using the `event-emitter` library (`on`, `emit`, `remove-listener`, …)
- About to add a `defmethod` whose specializers may match a method defined in a dependency
- Writing or debugging a large Lisp file (>100 lines) and suspecting a paren error
- Using `defmain` for a CLI with subcommands

## `event-emitter` has INCONSISTENT argument order
This is the most surprising trap — agents uniformly assume the order is consistent. It is not. Verified from `src/event-emitter.lisp`:

```lisp
;; EVENT first
(on   event object listener)
(once event object listener)
(emit event object &rest args)
;; OBJECT first
(add-listener         object event listener)
(remove-listener      object event listener &key (start 0))
(remove-all-listeners object &optional event)
(listeners            object event)
```

```lisp
(event-emitter:on :tool-call provider handler)              ;; good
(event-emitter:on provider :tool-call handler)              ;; BAD — NO-APPLICABLE-METHOD on the keyword
(event-emitter:emit :tool-call provider call-id tool args)  ;; good
(event-emitter:remove-listener provider :tool-call handler) ;; good (object first)
```

**Mnemonic:** `on` / `once` / `emit` are **event-first**; anything named `*-listener` or `listeners` is **object-first**.

## Silent CLOS method replacement
CLOS identifies a method by `(generic-function, qualifiers, specializer-list)`. Two `defmethod`s with the **same specializers and qualifiers** are the **same method** — the later load silently replaces the earlier one. No conflict is signaled, no merge happens.

Before adding a `defmethod` on a generic from a dependency, check whether the base already defines a method with identical specializers — otherwise yours silently overrides it and callers get your return type unexpectedly.

## `define-global-var` is not ordinary `let`-bindable state
Globals created with `global-vars:define-global-var` are a recurring trap in tests and temporary overrides. Treat them as mutable process-wide state, not as ordinary specials you can safely rebind with `let`.

For temporary overrides, prefer:
```lisp
(let ((old-value *some-global*))
  (unwind-protect
       (progn
         (setf *some-global* new-value)
         ...)
    (setf *some-global* old-value)))
```
If you try `(let ((*some-global* ...)) ...)`, compilation may fail or behavior may differ from ordinary special variables.

## Parenthesis balance in large files
Don't write functions over ~100 lines; split into helpers. Before `load-system` on a freshly edited file, check paren balance with the bundled tool:

```lisp
(load "check-paren-balance.lisp")
(check-paren-balance "src/big-file.lisp")
;; *** EXTRA `)` at line 47
;; Final depth: 0 (0 = balanced)
```

Negative depth at line N → extra `)` on that line. Non-zero final depth → missing `)`. Count closing-paren runs (`))))`) carefully — the single most common error.

## defmain subcommands (reference)
- A subcommand named after a CL symbol (`list`, `do`) trips `SYMBOL-PACKAGE-LOCKED-ERROR`. Fix with `(:shadow #:list)` in the `defpackage`, or just rename (`ls`, `serve`).
- The generated dispatch function is the symbol `DEFMAIN:SUBCOMMAND` (in the `defmain` package), not a local symbol — call `(defmain:subcommand)`, never a bare `(subcommand)`.
- Parent command arguments are passed as symbols from the **parent** package. Import them in the subcommand's package, or it sees unbound same-named symbols: `(:import-from #:myapp/cli/main #:main #:model #:output)`.

## Common Mistakes
| Symptom | Fix |
|---|---|
| `NO-APPLICABLE-METHOD-ERROR` on a keyword (`SILO`) from `event-emitter:on` / `emit` | Swap to event-first: `(on :event object handler)` |
| `event-emitter:remove-listener` not finding the handler | It is object-first: `(remove-listener object :event handler)` |
| Your `defmethod` silently shadows one from a dependency | Check for existing same-specializer methods first; rename or change specializers |
| A `define-global-var` global cannot be used in `LET` | Save/restore with `setf` + `unwind-protect` instead of rebinding |
| `SYMBOL-PACKAGE-LOCKED-ERROR` loading a `defmain` subcommand | `(:shadow #:list)` in `defpackage`, or pick a non-CL name |
| `(subcommand)` → `UNBOUND-VARIABLE` | Use `(defmain:subcommand)` |
| Subcommand sees unbound parent-arg symbols | `(:import-from #:parent/pkg #:model #:output ...)` in the subcommand package |
| Extra/missing `)` in a big file | Run `check-paren-balance` (see `check-paren-balance.lisp`) before `load-system` |

## Related skills
For the surrounding style: **writing-common-lisp**. For reloading a file after edits: **using-common-lisp-repl**.
