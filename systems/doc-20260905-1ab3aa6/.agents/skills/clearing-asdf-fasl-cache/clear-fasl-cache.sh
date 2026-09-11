#!/bin/bash
#
# clear-fasl-cache.sh — Remove stale ASDF fasl caches for a Common Lisp project.
#
# Usage:
#   ./clear-fasl-cache.sh                          # uses current directory
#   ./clear-fasl-cache.sh /path/to/project          # explicit path
#
# The script removes all fasl cache directories under
# ~/.cache/common-lisp/sbcl-*/ that match the given project path.

set -euo pipefail

PROJECT_DIR="${1:-$(pwd)}"

# Resolve to absolute path
PROJECT_DIR="$(cd "$PROJECT_DIR" 2>/dev/null && pwd)" || {
    echo "ERROR: Cannot resolve path: $PROJECT_DIR" >&2
    exit 1
}

CACHE_BASE="$HOME/.cache/common-lisp"
MATCH_PATTERN="$CACHE_BASE/sbcl-*/$PROJECT_DIR"

# Check if anything matches
MATCHES=$(ls -d $MATCH_PATTERN 2>/dev/null || true)
if [ -z "$MATCHES" ]; then
    echo "No fasl cache found for: $PROJECT_DIR"
    echo "Searched: $MATCH_PATTERN"
    exit 0
fi

echo "Removing fasl caches for: $PROJECT_DIR"
echo "$MATCHES"
echo ""

for dir in $MATCHES; do
    rm -rf "$dir"
    echo "  Removed: $dir"
done

echo ""
echo "Done. Rebuild with your tool to recompile from source."
