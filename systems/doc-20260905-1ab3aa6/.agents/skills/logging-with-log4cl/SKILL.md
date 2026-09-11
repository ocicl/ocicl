---
name: logging-with-log4cl
description: Use when adding or changing log calls in a Common Lisp project that uses log4cl (the LOG package), when compilation fails with "Package LOG does not exist" in an ASDF package-inferred system, or when a log:info call with a path or tilde crashes or garbles at runtime.
---

# Logging with log4cl

## Overview
log4cl exposes the `LOG` package (`log:info`, `log:error`, `log:debug`, …). These macros are `cl:format`-based: the **first argument is a format control string** and any further arguments are format args.

## When to Use
- Adding log output to a CL file that uses (or should use) log4cl
- Compile error `The package LOG does not exist` in a package-inferred system
- A `log:info` call crashes or mangles output because of a literal `~` (typically from a path)

## Prefer `log:info` over `format t`
`log:info` / `log:error` give structured, level-filterable, redirectable output. Do not use `(format t ...)` for diagnostics.

```lisp
(log:info "Processed ~A rows in ~A ms" count elapsed)   ;; good
(format t "[info] Processed ~A rows~%" count)            ;; bad
```

Write `log:info` with the `log:` prefix (aids grep). Do not `(:import-from #:log #:info)` just to drop the prefix unless the project convention says so.

## First argument is a format control string
`~` is the format directive character. Paths and home-directory strings contain `~` and will crash or mangle output if inlined into the control string.

```lisp
(log:info "Loading plugins from ~/.config/barista/plugins/")   ;; bad — ~ consumed as a directive
(log:info "Loading plugins from ~~/.config/...")               ;; works but brittle — breaks the moment this becomes dynamic
(log:info "Loading plugins from ~A" dir)                       ;; good — value passed as a ~A argument
```

**Rule of thumb:** the control string holds fixed structure; every `~`-bearing or variable value goes in as a `~A` argument. Paths are dynamic in practice — never inline them.

## `Package LOG does not exist` in package-inferred systems
ASDF infers a file's dependencies from its `uiop:define-package` clauses. If `log` only appears as a prefixed symbol in the body (`log:info`), ASDF does not see a dependency on `log4cl`, and compiling the file fails because the `LOG` package is not loaded yet.

Fix — register the package nickname in the **main `.asd`**, then files can `(:import-from #:log ...)` and ASDF inference resolves `log4cl`:

```lisp
;; in myapp.asd
(asdf:register-system-packages "log4cl" '("LOG"))
```

```lisp
;; in the file
(uiop:define-package #:myapp/src/workers/poller
  (:use #:cl)
  (:import-from #:log #:info)
  (:export #:run-poller))
```

## Common Mistakes
| Symptom | Fix |
|---|---|
| `log:info` crashes / garbles on a path containing `~` | Pass the path as a `~A` argument, not inline in the control string |
| `The package LOG does not exist` at compile time in a package-inferred file | `(asdf:register-system-packages "log4cl" '("LOG"))` in the main `.asd`, plus `(:import-from #:log #:info)` in the file |
| Using `format t` for diagnostics | Replace with `log:info` / `log:error` |
| `~~` used to escape an inlined path | Replace with `~A` + argument — paths are dynamic |

## Related skills
For the surrounding package and `uiop:define-package` conventions: **writing-common-lisp**.
