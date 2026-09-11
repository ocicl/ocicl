---
name: lint-common-lisp
description: Run static analysis on Common Lisp projects with the Mallet command-line linter. Use when reviewing, diagnosing, or fixing lint findings in `.lisp` or `.asd` files; when establishing a lint baseline; or when verifying that Common Lisp changes leave no actionable Mallet findings. Supports repositories using Qlot, ASDF, and Rove.
---

# Lint Common Lisp

Use Mallet to find likely mistakes, not to impose broad formatting changes.

## Workflow

1. Inspect repository instructions and the working tree. Preserve unrelated
   changes. Check whether `mallet` is available with `command -v mallet`.
2. Determine the scope before linting. Prefer tracked project files:

   ```sh
   git ls-files -z -- '*.lisp' '*.asd' | xargs -0 mallet --format line
   ```

   For an untracked project, use `rg --files -g '*.lisp' -g '*.asd'` and pass
   only those project paths to Mallet. Exclude vendored code, generated files,
   dependency directories, and build caches unless the user explicitly asks to
   lint them.
3. Record the baseline by file, rule, and severity. Read the surrounding code
   before changing it. Treat warnings about dynamic bindings, macros, ASDF
   names, or package imports as hypotheses; confirm their runtime role first.
4. Fix one logical issue at a time with the smallest semantics-preserving edit.
   Never use `--fix` without previewing its effect with `--fix-dry-run` and
   confirming that bulk edits are in scope.
5. After each changed file, run the repository's relevant test command before
   editing the next file. Prefer documented project commands. When available:

   ```sh
   qlot exec ros -L sbcl-bin run \
     --eval '(ql:quickload :<project>-tests)' \
     --eval '(asdf:test-system :<project>)' \
     --quit
   ```

   For a Rove test system, use its documented `asdf:test-system` command; do
   not substitute a partial compilation check for the full suite unless the
   user requests a narrower verification.
6. Re-run the same project-wide Mallet command. Resolve every actionable
   finding. If a rule is a verified false positive, preserve the working code,
   document why, and disable only that rule for the final command or project
   configuration. Do not silence unrelated rules or claim a clean default run.

## Safety checks

- Do not remove an import, special-variable binding, declaration, or ASDF
  system merely because it appears unused; verify indirect and runtime use.
- Replace runtime `eval`, `read-from-string`, or similar dynamic input paths
  only after tracing the input contract and adding or retaining adequate tests.
- Keep package-inferred ASDF system names consistent with their component
  namespaces; lint conventions must not break ASDF resolution.
- Report the exact lint command, any scoped suppression, and test evidence.

## Completion criteria

Finish only when the selected lint scope is clean, or when every remaining
finding has an explicit, narrow, evidence-backed suppression accepted by the
user. Include the final Mallet output and the test result in the handoff.
