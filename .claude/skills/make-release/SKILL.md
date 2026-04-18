---
name: make-release
description: Cut a new ocicl release. Bumps version in ocicl.asd, verifies the build and tests pass, generates release notes, commits, tags, and pushes. Use when asked to "release", "cut a release", "bump version", or "tag a new version".
argument-hint: "[version]"
disable-model-invocation: true
allowed-tools: Bash Read Edit Write Grep Glob
---

# Release ocicl

Follow these steps exactly, stopping on any failure.

## 1. Determine version

If the user provided a version (`$ARGUMENTS`), validate that it matches `MAJOR.MINOR.PATCH` where each component is a non-negative integer (e.g. `2.17.0`). If it doesn't, stop and tell the user.

If no version was provided, suggest one:

1. Read the current version from `:version` in `ocicl.asd`.
2. Review commits since the last tag:
   ```bash
   git log $(git describe --tags --abbrev=0)..HEAD --oneline
   ```
3. Apply semver rules:
   - **MAJOR** bump: commits contain breaking changes (removed features, changed APIs, incompatible behavior)
   - **MINOR** bump: commits add new features, new commands, new capabilities
   - **PATCH** bump: commits are only bug fixes, documentation, or internal improvements
4. Present the suggested version with a one-line rationale and ask the user to confirm or override.

Use the confirmed version as VERSION for all subsequent steps.

**Check the tag doesn't already exist:**
```bash
git tag -l "vVERSION"
```
If output is non-empty, stop -- this version has already been released.

**Ensure clean working tree:**
```bash
git status --porcelain
```
If there are staged or unstaged changes to **tracked** files, stop and ask the user to commit or stash first.

Then check untracked files. If any look like they don't belong in the repo (log files, binaries, build artifacts, temp files, editor backups, etc.), list them and ask the user whether to clean up, add to `.gitignore`, or proceed anyway. Benign untracked files (e.g. `.claude/`, local config) are fine to ignore silently.

**Verify git identity:**
```bash
git config user.email
```
If the result is not `green@moxielogic.com`, stop and tell the user their git email is misconfigured for this repository.

**Ensure we're on main and synced with remote:**
```bash
git branch --show-current
git fetch origin main
git rev-list HEAD..origin/main --count
```
If the branch is not `main`, or there are upstream commits not yet pulled, stop and tell the user.

**Check CI is green on HEAD:**
```bash
gh run list --branch main --limit 1 --json conclusion --jq '.[0].conclusion'
```
If the result is not `success`, stop and warn the user that CI is failing on main. Ask whether to proceed anyway.

## 2. Update version

Edit `ocicl.asd` and set `:version` to `"VERSION"`.

## 3. Build and test

```bash
sbcl --load setup.lisp 2>&1 | tail -10
```

If the build fails, stop and report the failure. Do NOT continue.

```bash
sbcl --non-interactive --eval '(asdf:test-system "ocicl")' 2>&1 | tail -20
```

If any tests fail, stop and report the failure. Do NOT continue.

## 4. Generate release notes

Create `docs/release-notes/RELEASE-NOTES-VERSION.md`.

To decide what goes in the notes, diff against the most recent tag:

```bash
git log $(git describe --tags --abbrev=0)..HEAD --oneline
```

**Check updated dependencies for noteworthy changes:**

```bash
git diff $(git describe --tags --abbrev=0)..HEAD -- systems.csv
```

For each dependency that changed, especially security-critical ones like `pure-tls`, check the upstream changelog or commit history for user-facing changes (security fixes, new features, breaking changes). Surface important upstream changes in the release notes — particularly security fixes, which should get their own ## Security section with advisory IDs and descriptions.

Include ONLY user-facing changes:
- Security fixes (from dependencies or this project)
- Bug fixes
- New features
- Breaking changes (if any)
- Notable dependency updates (with version info)

Do NOT include internal changes (refactors, lint fixes, doc updates, CI changes, directory reorganization). Those are visible in the git log for anyone who needs them.

Match the voice and format of previous release notes in `docs/release-notes/`. The standard format is:

```markdown
# ocicl VERSION Release Notes

**Release Date:** Month YYYY

## Summary

One-line summary of the release.

## New Features

### Feature Name

Description of the feature.

## Bug Fixes

- Description of fix.

## Breaking Changes

None (or list them).

## Upgrade Notes

Drop-in replacement for PREVIOUS_VERSION (or describe migration steps).

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/vVERSION):

**Linux:**
- **Fedora/RHEL/CentOS**: `sudo dnf install ocicl` (via repo) or download RPM
- **Debian/Ubuntu**: `sudo apt install ocicl` (via repo) or download DEB

**Windows:**
- **Installer**: `ocicl-VERSION-setup.exe` (recommended)
- **MSI**: `ocicl-VERSION.msi`
- **Chocolatey**: `choco install ocicl`
- **ZIP**: `ocicl-VERSION-windows-amd64.zip`

**macOS:**
- **Homebrew**: `brew install ocicl`
- **ARM64**: `ocicl-VERSION-macos-arm64.tar.gz`
- **x64**: `ocicl-VERSION-macos-x64.tar.gz`
```

## 5. Commit

```bash
git add ocicl.asd docs/release-notes/RELEASE-NOTES-VERSION.md
git commit -m "Boost version to VERSION"
```

## 6. Tag and push

Ask the user for confirmation before pushing, then:

```bash
git tag -s vVERSION -m "Release VERSION"
git push origin main
git push origin vVERSION
```

Pushing the tag triggers GitHub Actions which automatically builds all packages (RPM, DEB, Linux tarball, Windows ZIP/NSIS/MSI) and creates the GitHub release.
