# Repository Guidelines

## Project Structure & Module Organization

This is a Common Lisp documentation library split into ASDF package-inferred systems. Core APIs live in `src/`; optional documentation-generation features, renderers, themes, and locatives live in `full/`. The system definitions are `40ants-doc.asd`, `40ants-doc-full.asd`, and `40ants-doc-test.asd`. Tests are in `test/`, with expected rendered-output fixtures under `test/data/baseline/`. Browser assets are kept in `css/`, `js/`, and `static/`; Emacs helpers are in `elisp/`.

Keep a feature in the smallest appropriate subsystem. For example, add a core locative in `src/locatives/` and a rendering-specific locative in `full/locatives/`.

## Development Workflow

Use the `atomic-spec-orchestrator` skill for all project work. Follow its Atomic Spec process, including the required role transitions and gate validation, when applicable.

## Build, Test, and Development Commands

Install the locked dependencies once with:

```sh
qlot install
```

Run the complete test system from the repository root:

```sh
CL_SOURCE_REGISTRY="$(pwd)//" qlot exec ros run \
  --eval '(ql:quickload "40ants-doc-test") \
  --eval '(asdf:test-system "40ants-doc-test")' \
  --quit
```

CI also runs the project linter (install its wrapper first if needed):

```sh
qlot exec ros install 40ants-asdf-system 40ants-linter
qlot exec 40ants-linter --system "40ants-doc, 40ants-doc-full, 40ants-doc-test" --imports
```

## Coding Style & Naming Conventions

Use two-space indentation and the existing Lisp layout: `uiop:define-package`, imports grouped by package, then `in-package`. Use lowercase, hyphenated file names and slash-separated package/subsystem names, e.g. `40ants-doc-full/locatives/function`. Public special variables use earmuffs (`*discard-documentation-p*`); constants use plus signs. Add docstrings to exported definitions and keep comments focused on non-obvious design constraints.

## Testing Guidelines

Tests use Rove's `deftest`, `testing`, and assertions. Name tests descriptively with a `test-` prefix, such as `test-reference-collection`. Add focused coverage for changed behavior, including edge cases. Rendering changes may require intentional updates to `test/data/baseline/`; inspect those diffs carefully rather than accepting broad fixture changes.

## Commit & Pull Request Guidelines

Use short, imperative commit subjects with a capitalized verb (for example, `Fixed dependencies collection.` or `Added ...`). Keep documentation-only updates separate where practical. Pull requests should explain the behavior change, identify affected ASDF systems, include relevant test or baseline changes, and link the issue when one exists. Ensure tests and the linter pass before requesting review.
