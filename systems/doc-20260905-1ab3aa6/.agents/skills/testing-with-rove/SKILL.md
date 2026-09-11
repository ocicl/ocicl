---
name: testing-with-rove
description: Use when writing or running tests with the Rove framework in a Common Lisp project, wiring an ASDF test-op, debugging an unexpected (signals ...) test result, or running a single test from the REPL.
---

# Testing with Rove

## Overview
Rove is the test framework used across these CL projects. Its `signals` assertion has a **counter-intuitive argument order** (form first, condition second) that is the single most common Rove bug.

## When to Use
- Writing a new test or `deftest`
- A `signals` test passes or fails for the wrong reason
- Wiring an ASDF `test-op`, or running a single test from the REPL

## The `signals` signature (read this first)
```lisp
(defmacro signals (form &optional (condition ''error)))
```
**The form under test is the FIRST argument; the condition type is the OPTIONAL SECOND (defaults to `'error`).**

```lisp
;; CORRECT — (do-thing) is evaluated, expected to signal my-error
(signals (do-thing) 'my-error)
(signals (do-thing))                  ; defaults to 'error

;; WRONG — 'my-error becomes the form (never signals); (do-thing) is read as the type
(signals 'my-error (do-thing))
```
The wrong form silently misbehaves: it evaluates `'my-error` (which does not signal) and treats `(do-thing)` as the condition type specifier. The test does not do what it looks like it does — every baseline agent writes it the wrong way first.

## Running tests
```lisp
(asdf:test-system :myapp-test)                                ; whole system
(rove:run :myapp-test/models/foo)                            ; one module
(rove:run-test 'myapp-test/models/foo::test-adding-foo)      ; one test
```

### Capturing per-assertion output from the REPL
`rove:run-test` prints its report to `*standard-output*` and returns a different value. Wrap it to see the per-assertion output:
```lisp
(with-output-to-string (*standard-output*)
  (rove:run-test 'myapp-test/models/foo::test-adding-foo))
```

## Test file conventions
- Tests live in `tests/` mirroring `src/`, named `*-test.lisp`.
- Structure: `(deftest <unit> ... (testing "<aspect>" ...))`. Name the `deftest` after the unit under test.
- Add brand-new test files to the test system's ASDF components when not auto-inferred.
- Write the failing test before the implementation.

## Resetting global state safely
If the code under test uses `global-vars:define-global-var`, do **not** try to rebind it with `let`. Save the old value, `setf` the test value, and restore it with `unwind-protect`:
```lisp
(let ((old-value *some-global*))
  (unwind-protect
       (progn
         (setf *some-global* :test-value)
         ...)
    (setf *some-global* old-value)))
```
This matters for logging/configuration code where tests need temporary global hooks or appenders.

## Reloading tests in a live image
After editing a test file in the REPL workflow, force just that test file instead of recompiling the whole system:
```lisp
(asdf:load-system "myapp-tests" :force '("myapp-tests/core"))
```

## Strict schema and CLOS regression check
For a changed CLOS class or schema class, add a test that constructs every
optional-slot combination relevant to the public constructor, especially the
case where the argument is omitted. Then run the complete suite after the
strict compilation check from **using-common-lisp-repl**; this catches a slot
whose `:initform` does not satisfy its declared CLOS type.

## Testing internal (`%`-prefixed) functions
Private functions named like `%helper` cannot be referenced from the test package. To test them:
```lisp
;; in the source package
(:export #:%helper)
;; in the test package
(:import-from #:myapp/src/foo #:%helper)
```
The `%` prefix already signals "internal"; exporting it for tests is accepted practice.

## Common Mistakes
| Symptom | Fix |
|---|---|
| `(signals 'my-error (do-thing))` — test passes/fails for the wrong reason | Swap to `(signals (do-thing) 'my-error` — form first, type second |
| `LET` on a `define-global-var` global fails or behaves strangely | Use `setf` + `unwind-protect`, not dynamic rebinding |
| `rove:run-test` output not visible | Wrap in `(with-output-to-string (*standard-output*) ...)` |
| Cannot reference `%helper` from tests | `(:export #:%helper)` in source, `(:import-from ...)` in test |
| New test file not picked up | Add it to the test system's ASDF components |

## Related skills
For running these from a live image: **using-common-lisp-repl**. For test-code style: **writing-common-lisp**.
