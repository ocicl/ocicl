#!/bin/sh
# Re-pin egcl-forks.txt to the current tip of each fork's branch.
#
# Usage: releng/update-egcl-forks.sh [PIN-FILE]
set -eu

pins="${1:-$(dirname "$0")/../egcl-forks.txt}"
tmp="$pins.tmp"

while IFS= read -r line; do
    case "$line" in
        ''|'#'*) printf '%s\n' "$line"; continue ;;
    esac
    set -- $line
    name=$1 branch=$2 old=$3
    new=$(git ls-remote "https://github.com/atgreen/$name" "refs/heads/$branch" | cut -f1)
    if [ -z "$new" ]; then
        printf 'No branch %s in atgreen/%s\n' "$branch" "$name" >&2
        exit 1
    fi
    [ "$new" = "$old" ] || printf '%s: %s -> %s\n' "$name" "$old" "$new" >&2
    printf '%-23s %-15s %s\n' "$name" "$branch" "$new"
done < "$pins" > "$tmp"
mv "$tmp" "$pins"
