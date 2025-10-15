# Release v2.7.8

## New Linting Rules

This release adds five new linting rules inspired by the [mallet](https://github.com/fukamachi/mallet) project:

### Error Level
- **`wrong-otherwise`** - Detects `otherwise` or `t` clauses in `ecase`/`etypecase` forms, which defeats their exhaustiveness checking guarantees

### Warning Level
- **`unused-local-function`** - Identifies unused functions in `flet` and `labels` forms
  - Correctly handles shadowing in nested bindings
  - Respects `declare ignore`/`ignorable` declarations
  - Supports underscore naming convention for intentionally unused functions
  - Distinguishes between `flet` (no mutual recursion) and `labels` (mutual recursion allowed)

- **`missing-otherwise`** - Warns when `case` or `typecase` forms lack an `otherwise` clause, or suggests using `otherwise` instead of `t` for better convention

### Convention Level
- **`asdf-component-strings`** - Enforces ASDF best practice of using strings instead of symbols for system names and dependencies in `.asd` files

- **`bare-progn-in-if`** - Suggests using `cond` when `if` has bare `progn` in then/else branches for improved readability

## Improvements

- Fixed ASDF system definition to use `"ocicl"` (string) instead of `#:ocicl` (symbol), following ASDF best practices
- Applied new linting rules to improve code quality in `ocicl.lisp` and `lint/parsing.lisp`

## Credits

Special thanks to the [mallet](https://github.com/fukamachi/mallet) project for rule inspiration and design patterns.
