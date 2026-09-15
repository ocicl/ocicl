#!/bin/bash
# Tests for how tarball extraction handles link entries (ocicl-2w4).
#
# cl-tar's simple extractor turns a link into a plain file by copying the
# bytes of whatever it names, which fails outright on a link to a directory
# (Eclector ships documentation/presentation-slides -> a slides directory)
# and would happily copy a file from outside the staging tree.  ocicl
# extracts links itself instead; these tests pin that behaviour down.
#
# Run from the repository root.  Symlink fixtures need a POSIX filesystem.

set -e

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

fail() { echo "FAIL  $1"; exit 1; }
pass() { echo "PASS  $1"; }

echo "=== extraction link tests ==="

mkdir -p "$TMP/src/pkg/docs/slides"
echo "real contents" > "$TMP/src/pkg/file.txt"
echo "slide one"     > "$TMP/src/pkg/docs/slides/one.txt"
echo "secret"        > "$TMP/outside.txt"

ln    "$TMP/src/pkg/file.txt" "$TMP/src/pkg/hardlink.txt"
ln -s slides       "$TMP/src/pkg/docs/to-directory"
ln -s ../file.txt  "$TMP/src/pkg/docs/to-file"
ln -s to-file      "$TMP/src/pkg/docs/to-link"
ln -s /etc/hosts   "$TMP/src/pkg/escape-absolute"
ln -s ../../outside.txt "$TMP/src/pkg/escape-relative"
ln -s nowhere      "$TMP/src/pkg/broken"

(cd "$TMP/src" && tar cf "$TMP/pkg.tar" pkg)

OUT="$TMP/out"
mkdir "$OUT"
sbcl --non-interactive --no-userinit \
  --eval "(load \"runtime/asdf.lisp\")" \
  --eval "(asdf:initialize-source-registry (list :source-registry :inherit-configuration (list :tree (uiop:getcwd))))" \
  --eval "(asdf:load-system :ocicl)" \
  --eval "(with-open-file (in \"$TMP/pkg.tar\" :element-type '(unsigned-byte 8))
            (ocicl::extract-source-archive in \"$OUT/\"))" \
  > "$TMP/extract.log" 2>&1 \
  || { cat "$TMP/extract.log"; fail "extraction signaled an error"; }

[ -f "$OUT/pkg/file.txt" ] || fail "plain file extracted"
[ -f "$OUT/pkg/docs/slides/one.txt" ] || fail "nested file extracted"
pass "extracts plain files"

[ -z "$(find "$OUT" -type l)" ] || fail "extraction created a symlink"
pass "creates no symlinks"

[ -f "$OUT/pkg/docs/to-file" ] || fail "link to a file became a file"
[ "$(cat "$OUT/pkg/docs/to-file")" = "real contents" ] \
  || fail "link to a file carries the target's contents"
pass "dereferences a link to a file"

[ -f "$OUT/pkg/docs/to-link" ] || fail "link to a link became a file"
[ "$(cat "$OUT/pkg/docs/to-link")" = "real contents" ] \
  || fail "link to a link carries the target's contents"
pass "dereferences a link to a link"

[ -f "$OUT/pkg/hardlink.txt" ] || fail "hard link became a file"
[ "$(cat "$OUT/pkg/hardlink.txt")" = "real contents" ] \
  || fail "hard link carries the target's contents"
pass "dereferences a hard link"

[ ! -e "$OUT/pkg/docs/to-directory" ] || fail "link to a directory was materialized"
pass "drops a link to a directory"

[ ! -e "$OUT/pkg/broken" ] || fail "broken link was materialized"
pass "drops a broken link"

[ ! -e "$OUT/pkg/escape-absolute" ] || fail "link to an absolute path outside the tree"
[ ! -e "$OUT/pkg/escape-relative" ] || fail "link to a relative path outside the tree"
[ "$(cat "$TMP/outside.txt")" = "secret" ] || fail "file outside the tree was disturbed"
pass "drops links that escape the extraction directory"

echo ""
echo "All extraction tests passed."
