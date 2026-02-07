#!/bin/bash
# Test runner for linter fixes (Issues 185-189)

set -e

echo "Loading ocicl.lint system and running tests..."
echo ""

sbcl --non-interactive \
  --eval "(require :asdf)" \
  --eval "(push (truename \"lint/\") asdf:*central-registry*)" \
  --eval "(asdf:load-system :ocicl.lint)" \
  --eval "(load \"test/lint-tests.lisp\")" \
  --eval "(in-package :ocicl-lint-tests)" \
  --eval "(run-all-tests)" \
  --eval "(quit :unix-status (if (= *test-failed* 0) 0 1))"

exit_code=$?

if [ $exit_code -eq 0 ]; then
  echo ""
  echo "✓ All tests passed!"
else
  echo ""
  echo "✗ Some tests failed"
fi

exit $exit_code
