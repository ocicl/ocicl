#!/bin/bash
# Tests for git+ package sources: fullname parsing units plus an
# end-to-end exercise of install/latest/remove against local git repos.
# Run from the repository root; expects a freshly built ./ocicl binary.

set -e

echo "=== git+ source unit tests ==="
sbcl --non-interactive --no-userinit \
  --eval "(load \"runtime/asdf.lisp\")" \
  --eval "(asdf:initialize-source-registry (list :source-registry :inherit-configuration (list :tree (uiop:getcwd))))" \
  --eval "(asdf:load-system :ocicl)" \
  --eval "(load \"test/git-source-tests.lisp\")" \
  --eval "(uiop:quit (if (zerop (ocicl-git-source-tests:run-all-tests)) 0 1))"

OCICL="$(pwd)/ocicl"
if [ ! -x "$OCICL" ]; then
  echo "no ./ocicl binary; build it first (sbcl --load setup.lisp)"
  exit 1
fi

echo ""
echo "=== git+ source integration tests ==="

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
GIT="git -c user.email=test@test -c user.name=test -c commit.gpgsign=false"

fail() { echo "FAIL  $1"; exit 1; }
pass() { echo "PASS  $1"; }

# An upstream repository with a top-level system and a monorepo-style
# subdirectory system.
mkdir -p "$TMP/upstream/libs/inner"
cd "$TMP/upstream"
$GIT init -q -b main .
cat > testlib.asd <<'EOF'
(defsystem "testlib" :components ((:file "testlib")))
EOF
echo '(defpackage :testlib (:use :cl))' > testlib.lisp
cat > libs/inner/innerlib.asd <<'EOF'
(defsystem "innerlib" :components ((:file "innerlib")))
EOF
echo '(defpackage :innerlib (:use :cl))' > libs/inner/innerlib.lisp
$GIT add -A && $GIT commit -q -m "commit one"
SHA1=$(git rev-parse HEAD)

mkdir "$TMP/proj"
cd "$TMP/proj"
# Anchor ocicl's workdir discovery here, so it can't walk up and adopt
# an ocicl.csv that happens to exist above the temp directory.
touch ocicl.csv

# install git+URL@REF pins the resolved commit
$OCICL install "git+file://$TMP/upstream@main"
grep -q "testlib, git+file://$TMP/upstream@$SHA1#ref=main" ocicl.csv \
  || fail "install: pinned fullname in ocicl.csv"
[ -f "ocicl/upstream-${SHA1:0:7}/testlib.asd" ] \
  || fail "install: tree at <basename>-<shortsha>"
pass "install git+URL@REF"

# a fresh clone re-fetches from the pin
rm -rf ocicl/upstream-*
$OCICL install
[ -f "ocicl/upstream-${SHA1:0:7}/testlib.asd" ] \
  || fail "re-fetch: tree restored from pinned commit"
pass "install re-fetches a missing tree"

# subdirectory install lives in its own tree
$OCICL install "git+file://$TMP/upstream#subdirectory=libs/inner"
[ -f "ocicl/inner-${SHA1:0:7}/innerlib.asd" ] \
  || fail "subdirectory: tree named after the subdirectory"
[ -f "ocicl/upstream-${SHA1:0:7}/testlib.asd" ] \
  || fail "subdirectory: full-tree install left alone"
grep -q "innerlib, git+file://$TMP/upstream@$SHA1#subdirectory=libs/inner" ocicl.csv \
  || fail "subdirectory: fullname records the subdirectory"
pass "install git+URL#subdirectory=PATH"

# latest advances along the recorded ref
cd "$TMP/upstream"
echo ";; more" >> testlib.lisp
$GIT commit -qam "commit two"
SHA2=$(git rev-parse HEAD)
cd "$TMP/proj"
$OCICL latest
[ -f "ocicl/upstream-${SHA2:0:7}/testlib.asd" ] \
  || fail "latest: tree advanced to new commit"
[ ! -d "ocicl/upstream-${SHA1:0:7}" ] \
  || fail "latest: old tree removed"
grep -q "testlib, git+file://$TMP/upstream@$SHA2#ref=main" ocicl.csv \
  || fail "latest: pin advanced in ocicl.csv"
[ -f "ocicl/inner-${SHA2:0:7}/innerlib.asd" ] \
  || fail "latest: subdirectory tree advanced along default branch"
pass "latest advances git pins"

# a commit-SHA pin stays put
$OCICL remove innerlib
[ ! -d "ocicl/inner-${SHA2:0:7}" ] || fail "remove: tree deleted"
! grep -q "innerlib" ocicl.csv || fail "remove: rows deleted"
pass "remove deletes a git-sourced tree"

$OCICL install "git+file://$TMP/upstream@$SHA1#subdirectory=libs/inner"
$OCICL latest
grep -q "innerlib, git+file://$TMP/upstream@$SHA1#ref=$SHA1" ocicl.csv \
  || fail "sha pin: still pinned after latest"
[ -d "ocicl/inner-${SHA1:0:7}" ] || fail "sha pin: tree untouched"
pass "commit-SHA pins survive latest"

# Security: a malicious ocicl.csv path column must not let a delete escape
# the systems directory (ocicl-hih).
mkdir -p "$TMP/victim/ocicl"
cd "$TMP/victim"
touch canary.txt              # lives in the project root, one level above ocicl/
cat > ocicl.csv <<EOF
evil, ghcr.io/ocicl/evil@sha256:0000000000000000000000000000000000000000000000000000000000000000, ../evil.asd
EOF
# 'ocicl remove' on the crafted row would resolve to <project>/../ under the
# old guard; it must refuse and leave the project (and its parent) intact.
$OCICL remove evil >/dev/null 2>&1 || true
[ -f canary.txt ] || fail "remove: canary above ocicl/ was deleted (traversal!)"
[ -d "$TMP/victim" ] || fail "remove: project dir was deleted (traversal!)"
[ -d "$TMP" ] || fail "remove: temp root was deleted (traversal!)"
pass "remove refuses a '..' path in ocicl.csv"

# Same crafted row via the git+ refetch path ('ocicl install' with no args).
cd "$TMP/victim"
cat > ocicl.csv <<EOF
evil, git+file://$TMP/upstream@$SHA1, ../evil.asd
EOF
$OCICL install >/dev/null 2>&1 || true
[ -f canary.txt ] || fail "install: canary above ocicl/ was deleted (traversal!)"
[ -d "$TMP/victim" ] || fail "install: project dir was deleted (traversal!)"
pass "install refuses a '..' path in ocicl.csv"

# Security: dangerous transports / injection-shaped sources are refused
# by the CLI before git runs (ocicl-j83).
cd "$TMP/victim"
rm -f ocicl.csv; touch ocicl.csv
$OCICL install "git+ext::sh -c touch${IFS}/tmp/ocicl-pwned" >/dev/null 2>&1 || true
[ ! -e /tmp/ocicl-pwned ] || { rm -f /tmp/ocicl-pwned; fail "ext:: transport executed a command!"; }
if $OCICL install "git+ext::sh" >/dev/null 2>&1; then fail "ext:: source was accepted"; fi
pass "install refuses ext:: transport source"
if $OCICL install "git+-upload-pack=touch" >/dev/null 2>&1; then fail "'-'-prefixed URL was accepted"; fi
pass "install refuses '-'-prefixed URL"

echo ""
echo "All git+ source tests passed."
