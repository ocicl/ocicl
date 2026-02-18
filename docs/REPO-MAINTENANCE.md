# Maintaining the ocicl repos

Familiarize yourself with how ocicl works here: ../README.md

The ocicl repos are archives of tarballs maintained as OCI artifacts.
The current repos are hosted in ghcr.io and dockerhub.
Tarballs are built and published via github actions.
Repos containing metadata describing these repos are available in
git@github.com:/ocicl/PROJECT


## Overview

The ocicl package ecosystem consists of ~2,500 repos under the
[github.com/ocicl](https://github.com/ocicl) organization, providing
~2,600 ASDF systems. Each repo is a thin metadata wrapper around an
upstream Common Lisp project. A GitHub Actions workflow in each repo
periodically checks for upstream changes and, when found, builds a
tarball and publishes it as an OCI artifact to both `ghcr.io/ocicl`
and `docker.io/ocicl`.


## Key resources

| Resource | Location |
|----------|----------|
| ocicl-action (GHA code) | https://github.com/ocicl/ocicl-action |
| System additions requests | https://github.com/ocicl/request-system-additions-here |
| Master system list | https://raw.githubusercontent.com/ocicl/request-system-additions-here/main/all-ocicl-systems.txt |
| Quicklisp projects | https://github.com/quicklisp/quicklisp-projects |


## Repo structure

Every ocicl project repo under `github.com/ocicl/` has the same structure:

```
.github/
  workflows/
    main.yml        # GitHub Actions workflow (identical across repos)
.gitignore          # Contains: *~
LICENSE             # MIT (covers the repo metadata, not the upstream software)
README.org          # The key metadata file
```

The `README.org` is the only file that varies between repos. It
controls everything: what source to fetch, how to track updates, and
which ASDF systems the package provides.


## README.org format

The README.org is an org-mode file with a heading, description, and a
metadata table. There are two tracking modes.

### Mode 1: Git commit tracking

Used for projects where you track the latest commit on HEAD (most repos):

```org
* cl-str

A modern and consistent Common Lisp string manipulation library.

|---------+--------------------------------------------|
| source  | git:https://github.com/vindarel/cl-str.git |
| commit  | 69ef8b4                                    |
| systems | str                                        |
|---------+--------------------------------------------|
```

The `commit` field holds a 7-character git short hash. The
update-check action runs `git ls-remote HEAD` against the upstream
URL and compares the result.

### Mode 2: Version/release tracking

Used for projects that make formal releases (tags/tarballs):

```org
* bordeaux-threads

Bordeaux-Threads is a Common Lisp threading library.

|---------+-----------------------------------------------------------------------------------|
| source  | file:https://github.com/sionescu/bordeaux-threads/archive/refs/tags/v0.9.4.tar.gz |
| version | 0.9.4                                                                             |
| systems | bordeaux-threads                                                                  |
|---------+-----------------------------------------------------------------------------------|
```

The `version` field holds a version string. The update-check action
queries the GitHub/Codeberg releases API (or git tags) for the latest
version.

### Field reference

| Field | Prefix | Description |
|-------|--------|-------------|
| `source` | `git:` | A git repository URL (GitHub, GitLab, Codeberg, SourceHut, SourceForge) |
| `source` | `file:` | A tarball/archive URL (typically a GitHub release archive) |
| `commit` | *(none)* | 7-char short hash of the upstream commit being tracked |
| `version` | *(none)* | Version string from upstream release tags |
| `systems` | *(none)* | Space-separated list of ASDF system names provided by this package |

### Multiple systems

Many repos provide more than one ASDF system. List them all,
space-separated:

```org
| systems | cffi cffi-libffi cffi-toolchain cffi-grovel cffi-uffi-compat |
```

```org
| systems | clack clack-handler-hunchentoot clack-handler-wookie clack-socket clack-handler-toot |
```

Each listed system gets its own OCI artifact in the registry. For
example, the cffi repo pushes separate artifacts to
`ghcr.io/ocicl/cffi`, `ghcr.io/ocicl/cffi-libffi`, etc.

**Important:** Do not include test systems (e.g. `foo-test`,
`foo-tests`, `foo/tests`) in the `systems` list. Only list systems
that users would depend on for production use.

### Supported upstream sources

The update-check action supports these upstream hosts:

| Host | Example source URL |
|------|--------------------|
| GitHub | `git:https://github.com/vindarel/cl-str.git` |
| GitLab | `git:https://gitlab.common-lisp.net/alexandria/alexandria.git` |
| Codeberg | `git:https://codeberg.org/shinmera/trivial-indent.git` |
| SourceHut | `git:https://git.sr.ht/~user/project` |
| SourceForge | `git:git://git.code.sf.net/p/cl-tap-producer/code` |

For version tracking, the update-check action queries the
GitHub/Codeberg releases API first, then falls back to git tags.

Note: GitLab and SourceForge sources work for building but the
automated update-check only supports GitHub, Codeberg, and SourceHut
for detecting new commits/versions. GitLab-hosted repos will still
build whenever the README.org is manually updated.


## Admin working directory

`OCICL_ADMIN_HOME` is the root directory under which ocicl project
repos are checked out for maintenance. All maintenance commands in
this document use `$OCICL_ADMIN_HOME` to refer to this location.

```bash
export OCICL_ADMIN_HOME=~/ocicl-admin
```


## The `om` script

The `om` script automates creating a new ocicl project repo. It
creates the repo on GitHub under the `ocicl` organization, clones it,
and populates it with the standard workflow, LICENSE, and a skeleton
README.org. You then fill in the upstream details and push.

Here is the complete script:

```bash
#!/bin/sh

set -x

cd $OCICL_ADMIN_HOME

gh repo create ocicl/$1 --public --clone

# git clone git@github.com:/ocicl/$1

cd $1

echo *~ > .gitignore
mkdir -p .github/workflows
cat > .github/workflows/main.yml <<END
on:
  push:
  workflow_dispatch:
  schedule:
    # Check for updates every 6 hours
    - cron: '0 */6 * * *'

jobs:
  ocicl_job:
    permissions:
      issues: write
      packages: write
      contents: write
    runs-on: ubuntu-latest
    name: Test and publish package
    timeout-minutes: 40
    steps:
      - name: Checkout repository
        uses: actions/checkout@v4

      - name: Check for upstream updates
        id: update-check
        uses: ocicl/ocicl-action/update-check@main
        with:
          github-token: \${{ secrets.GITHUB_TOKEN }}

      - id: build-and-publish
        if: steps.update-check.outputs.updated == 'true'
        uses: ocicl/ocicl-action@main
        with:
          gpg_signing_key: \${{ secrets.GPG_SIGNING_KEY }}
          gpg_public_key: \${{ secrets.GPG_PUBLIC_KEY }}
          dockerhub_password: \${{ secrets.DOCKERHUB_PASSWORD }}
          llm_api_key: \${{ secrets.LLM_API_KEY }}
END

cat > LICENSE <<END
--------------------------------------------------------------------------------
This license pertains to the files within this repository, and does
not apply to software that is merely referenced by, but not
incorporated into, this repository.
--------------------------------------------------------------------------------

MIT License

Copyright (c) 2024 ocicl hackers

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
END

cat > README.org <<END
* $1



|---------+-------------------------------------------|
| source  | git: |
| commit  | |
| systems | $1 |
|---------+-------------------------------------------|

END
```

The script uses `$OCICL_ADMIN_HOME` (see "Admin working directory"
above).


## Creating a new repo

### Example: Adding a simple single-system package

```bash
# Create the repo (creates and clones github.com/ocicl/my-new-package)
$ om my-new-package
$ cd my-new-package
```

The skeleton README.org looks like:

```org
* my-new-package

|---------+-------------------------------------------|
| source  | git: |
| commit  | |
| systems | my-new-package |
|---------+-------------------------------------------|
```

Edit it to fill in the upstream details.

**Important:** Do NOT set the `commit` field to the current upstream
HEAD hash. If the hash already matches upstream, the update-check will
report "No update needed" and the build/publish stage will never run.
The artifact will never be published even though the workflow shows as
successful. Instead, use a dummy value like `0000000` so the
update-check sees a difference and triggers the initial build.

Complete the README.org:

```org
* my-new-package

A brief description of what my-new-package does.

|---------+--------------------------------------------------|
| source  | git:https://github.com/author/my-new-package.git |
| commit  | 0000000                                          |
| systems | my-new-package                                   |
|---------+--------------------------------------------------|
```

Then push:

```bash
$ git add -A && git commit -m "Add my-new-package" && git push
```

The push triggers the GitHub Actions workflow. Because the commit hash
(`0000000`) doesn't match upstream HEAD, the update-check will detect
a difference, update the hash, and trigger the build/publish stage.

### Example: Adding a package with multiple systems

```bash
$ om cffi
$ cd cffi
```

When a project provides multiple ASDF systems, list them all in the
`systems` field. Do **not** include test systems (like `cffi-tests`):

```org
* cffi

CFFI, the Common Foreign Function Interface, purports to be a portable
foreign function interface for Common Lisp.

|---------+--------------------------------------------------------------|
| source  | git:https://github.com/cffi/cffi.git                         |
| commit  | 71fdf5d                                                      |
| systems | cffi cffi-libffi cffi-toolchain cffi-grovel cffi-uffi-compat |
|---------+--------------------------------------------------------------|
```

```bash
$ git add -A && git commit -m "Add cffi" && git push
```

### Example: Adding a release-tracked package

Some projects make formal releases. Track them by version instead of
commit. Change `commit` to `version` and `git:` to `file:` with a
tarball URL:

```bash
$ om bordeaux-threads
$ cd bordeaux-threads
```

Find the latest release version:

```bash
$ gh api repos/sionescu/bordeaux-threads/releases/latest -q .tag_name
v0.9.4
```

```org
* bordeaux-threads

Bordeaux-Threads is a Common Lisp threading library.

|---------+-----------------------------------------------------------------------------------|
| source  | file:https://github.com/sionescu/bordeaux-threads/archive/refs/tags/v0.9.4.tar.gz |
| version | 0.9.4                                                                             |
| systems | bordeaux-threads                                                                  |
|---------+-----------------------------------------------------------------------------------|
```

```bash
$ git add -A && git commit -m "Add bordeaux-threads" && git push
```

### Example: Adding a package from GitLab

```org
* alexandria

Alexandria is a collection of portable public domain utilities.

|---------+--------------------------------------------------------------|
| source  | git:https://gitlab.common-lisp.net/alexandria/alexandria.git |
| commit  | 8514d8e                                                      |
| systems | alexandria                                                   |
|---------+--------------------------------------------------------------|
```

### Example: Adding a package from Codeberg

```org
* trivial-indent

A simple library to allow indentation hints for SWANK.

|---------+------------------------------------------------------|
| source  | git:https://codeberg.org/shinmera/trivial-indent.git |
| commit  | df07d50                                              |
| systems | trivial-indent                                       |
|---------+------------------------------------------------------|
```

### Example: Adding a package where repo name differs from system name

Sometimes the upstream repo name doesn't match the ASDF system name.
The ocicl repo name is your choice, but the `systems` field must list
the actual ASDF system names:

```org
* code

A TAP producer for Common Lisp.

|---------+---------------------------------------------------|
| source  | git:git://git.code.sf.net/p/cl-tap-producer/code |
| commit  | 893dbdc                                          |
| systems | mw-equiv                                         |
|---------+---------------------------------------------------|
```

### Choosing which systems to include

When looking at a project's `.asd` files, you will often see test
systems defined alongside the main systems. For example, a project
might define `foo`, `foo-utils`, and `foo-test`. Only include the
systems that users would depend on:

```org
# Good - only production systems:
| systems | foo foo-utils |

# Bad - don't include test systems:
| systems | foo foo-utils foo-test |
```


## Keeping all-ocicl-systems.txt current

The file `all-ocicl-systems.txt` in the
[request-system-additions-here](https://github.com/ocicl/request-system-additions-here)
repo is the master list of every ASDF system available through ocicl.
Users and tooling rely on this list, so it must be kept up to date
whenever systems are added or removed.

### When to update

**Only add systems to the list after they are actually published.**
Use `ocicl list <system>` to confirm the artifact is available before
updating the list. A successful workflow run does NOT mean the package
was published (see Troubleshooting).

Update `all-ocicl-systems.txt` whenever you:

- Add a new ocicl repo (after confirming `ocicl list` shows it)
- Add new systems to an existing repo's `systems` field
- Remove a system or archive a repo

### Generating the current list from the org

You can regenerate the list by scraping the `systems` fields from
every repo's README.org. This is slow (there are ~2,500 repos) but
authoritative:

```bash
# Fetch all system names from every repo's README.org
$ gh api orgs/ocicl/repos --paginate -q '.[].name' | while read repo; do
    systems=$(gh api "repos/ocicl/$repo/contents/README.org" -q .content 2>/dev/null \
      | base64 -d 2>/dev/null \
      | grep '^| systems' \
      | sed 's/^| systems  *| *//' \
      | sed 's/ *|$//')
    if [ -n "$systems" ]; then
      for s in $systems; do
        echo "$s"
      done
    fi
  done | sort -u > /tmp/all-systems-generated.txt
```

### Checking for drift

Compare the published list against what the org actually provides:

```bash
# Download the current published list
$ curl -sL https://raw.githubusercontent.com/ocicl/request-system-additions-here/main/all-ocicl-systems.txt \
    | sort > /tmp/published.txt

# Compare (assuming you generated /tmp/all-systems-generated.txt above)
$ diff /tmp/published.txt /tmp/all-systems-generated.txt
```

Lines prefixed with `>` are systems that exist in the org but are
missing from the published list. Lines prefixed with `<` are entries
in the published list that no longer exist.

### Updating the list

Clone the repo, update the file, and push:

```bash
$ gh repo clone ocicl/request-system-additions-here
$ cd request-system-additions-here
$ cp /tmp/all-systems-generated.txt all-ocicl-systems.txt
$ git add all-ocicl-systems.txt
$ git commit -m "New systems"
$ git push
```

Or for a quick addition of a single system, you can edit in place:

```bash
$ gh repo clone ocicl/request-system-additions-here
$ cd request-system-additions-here
# Add the new system name in sorted position
$ echo "my-new-system" >> all-ocicl-systems.txt
$ sort -o all-ocicl-systems.txt all-ocicl-systems.txt
$ git add all-ocicl-systems.txt
$ git commit -m "New systems"
$ git push
```


## How the build pipeline works

When a push is made to an ocicl project repo (including automated
pushes from the update-check), the GitHub Actions workflow runs in two
stages:

### Stage 1: Update check (`ocicl/ocicl-action/update-check@main`)

1. Parses `README.org` to extract `source`, `commit`/`version`, and `systems`
2. Contacts the upstream source:
   - For `commit` mode: runs `git ls-remote HEAD` and compares hashes
   - For `version` mode: queries the releases API or git tags
3. If a new version is detected:
   - Updates the `commit` or `version` field in `README.org`
   - For version-tracked repos, also updates version strings in the source URL
   - Commits and pushes the change
   - Sets `updated=true` output
4. If the upstream repo is missing/inaccessible, creates a GitHub issue

### Stage 2: Build and publish (`ocicl/ocicl-action@main`)

Only runs when `updated=true`. This stage:

1. Runs inside a container (`ghcr.io/ocicl/ocicl-sandbox:latest`)
   which clones the source, builds the tarball, and produces `NAME`,
   `VERSION`, `SYSTEMS`, and a tarball in `src/`
2. Pushes the tarball as an OCI artifact to both registries:
   ```
   ghcr.io/ocicl/<system-name>:<version>
   docker.io/ocicl/<system-name>:<version>
   ```
   Also tags each as `latest`.
3. Signs the tarball (GPG + sigstore rekor transparency log)
4. Generates an LLM-based change summary and pushes it as:
   ```
   ghcr.io/ocicl/<system-name>-changes.txt:<version>
   ```

### System name encoding

System names containing special characters are encoded:

| Character | Encoding |
|-----------|----------|
| Trailing `+` | `_plus` |
| Middle `+` | `_plus_` |
| `/` | `_slash_` |

For example, a system named `cl+ssl` becomes `cl_plus_ssl` in the
OCI registry.

### Scheduled checks

Every repo runs its workflow on a cron schedule:

```yaml
schedule:
  - cron: '0 */6 * * *'
```

This checks for upstream updates every 6 hours.


## Monitoring build failures

Builds can break for many reasons: upstream API changes, new
dependencies, renamed system definitions, transient network issues,
etc. Monitoring and fixing failures is an ongoing maintenance task.

### Checking for failures across the org

```bash
# List recent failed workflow runs across all ocicl repos
$ gh search code --owner ocicl --filename main.yml "" | head -50
# (For a more targeted approach, check individual repos)

# Check a specific repo's recent runs
$ gh run list -R ocicl/cl-str --limit 5
STATUS  TITLE              WORKFLOW  BRANCH  EVENT     ID          ELAPSED  AGE
✓       Update commit...   CI        main    push      12345678    2m30s    3h
✗       Update commit...   CI        main    schedule  12345677    5m12s    9h
...

# List only failed runs
$ gh run list -R ocicl/cl-str --status failure --limit 10
```

### Diagnosing a failure

```bash
# View the full log for a failed run
$ gh run view <run-id> -R ocicl/cl-str --log

# View just the failed job's log
$ gh run view <run-id> -R ocicl/cl-str --log-failed
```

### Common failure patterns and fixes

**ASDF system not found:**
The `systems` field in README.org lists a system name that doesn't
match what the upstream `.asd` file defines. Fix: check the upstream
`.asd` file and correct the `systems` list.

```bash
# Look at what .asd files the upstream actually defines
$ git clone --depth 1 https://github.com/author/project.git /tmp/project
$ find /tmp/project -name '*.asd' -exec grep -l 'defsystem' {} \;
$ grep 'defsystem' /tmp/project/*.asd
```

**Missing build dependency:**
The sandbox container may not have a library the upstream project
needs. This often shows up as an ASDF dependency error in the build
log. Fix: the missing dependency needs to exist as its own ocicl
package first. Create it, wait for it to build successfully, then
retry the failing package.

**Upstream repo disappeared or moved:**
The update-check will auto-create a GitHub issue when it can't reach
an upstream. Fix: find the new URL, update `README.org`, and push.

```bash
$ gh repo clone ocicl/<package>
$ cd <package>
# Update the source URL in README.org
$ git add README.org && git commit -m "Update source URL to new location" && git push
```

**Stale upstream mirror (missing symbols, package not found):**
A build fails with errors like "package X does not designate any
package" or "symbol Y not found in package Z" even though the
dependency exists in ocicl. This usually means the ocicl dependency
is tracking a stale mirror (e.g., a GitHub mirror) while the real
development has moved elsewhere (e.g., Codeberg). The published
version is too old and lacks symbols that newer packages need.

Fix: find the canonical upstream for the dependency and update its
ocicl repo to point there. Some authors (notably Shinmera/yukari
hafner) have migrated from GitHub to Codeberg; their `shinmera.com`
URLs redirect to Codeberg. Use the Codeberg URL directly so the
update-check can detect new commits:

```bash
# Check where a shinmera.com URL redirects to:
$ git ls-remote https://shinmera.com/projects/<name>.git HEAD
# Shows: warning: redirecting to https://codeberg.org/shinmera/<name>.git/

# Then update the ocicl repo to use the Codeberg URL
```

To find all ocicl repos still pointing to a stale mirror host:

```bash
$ gh search code "github.com/Shinmera" --owner ocicl --filename README.org
```

**Dependency needs to build first:**
When adding a new package that depends on another new package, the
dependency must be published before the dependent can build. The
dependent will fail on its first build attempt but should succeed on
the next 6-hour cron cycle (or manual re-trigger) once the dependency
is available. Plan additions in dependency order when possible.

**Transient network errors (OCI push failures, git clone timeouts):**
The pipeline has retry logic with exponential backoff, but sometimes
all retries fail. Fix: just re-run the workflow.

```bash
$ gh run rerun <run-id> -R ocicl/<package>
```

**Timeout (exceeds 40 minutes):**
Some large projects take a long time to build. The workflow has a
40-minute timeout. If a project consistently times out, it may need
investigation into why the build is slow (e.g., compiling large
C libraries via CFFI).

### Bulk-checking failures

To check for failures across many repos at once:

```bash
# Check the 20 most recently active repos for failures
$ for repo in $(gh api orgs/ocicl/repos --paginate -q '.[].name' | head -20); do
    status=$(gh run list -R ocicl/$repo --limit 1 -q '.[0].conclusion' 2>/dev/null)
    if [ "$status" = "failure" ]; then
      echo "FAILED: ocicl/$repo"
    fi
  done
```


## Monitoring quicklisp for new packages

The [quicklisp-projects](https://github.com/quicklisp/quicklisp-projects)
repo tracks all projects available in quicklisp. Periodically check it
for new packages that should be added to ocicl. There are two things
to monitor:

### Monitoring quicklisp-projects issues

People submit new packages to quicklisp by opening issues on the
[quicklisp-projects](https://github.com/quicklisp/quicklisp-projects/issues)
repo. These are a good source of new packages to add to ocicl as
well, often before they even land in quicklisp.

```bash
# List recent open issues (new package submissions)
$ gh issue list -R quicklisp/quicklisp-projects --state open --limit 20
```

Typical issue titles look like "Please add foo-bar" and include the
upstream repo URL. When you spot a new submission:

1. Check whether ocicl already has the system:
   ```bash
   $ ocicl list foo-bar
   ```
2. If not, grab the upstream URL from the issue and add it to ocicl
   (see "Creating a new repo" above)
3. Remember to update `all-ocicl-systems.txt` as well

### Comparing quicklisp projects to ocicl systems

```bash
# Get the list of quicklisp projects
$ git clone --depth 1 https://github.com/quicklisp/quicklisp-projects.git /tmp/ql-projects
$ ls /tmp/ql-projects/projects/ | sort > /tmp/ql-list.txt

# Get the list of ocicl systems
$ curl -sL https://raw.githubusercontent.com/ocicl/request-system-additions-here/main/all-ocicl-systems.txt \
    | sort > /tmp/ocicl-list.txt

# Find quicklisp projects not yet in ocicl
$ comm -23 /tmp/ql-list.txt /tmp/ocicl-list.txt
```

Note that the comparison is approximate since quicklisp uses project
names while ocicl uses ASDF system names, and these don't always
match. Use judgement when reviewing the diff.

### Examining a quicklisp project entry

Each quicklisp project has a directory under `projects/` with a
`source.txt` file describing where to fetch it:

```bash
$ cat /tmp/ql-projects/projects/str/source.txt
git https://github.com/vindarel/cl-str.git
```

The format is typically `git <url>` or `http <url>` (for tarballs).
This gives you the upstream URL you need for the ocicl README.org.

### Adding a quicklisp project to ocicl

Once you've identified a quicklisp project to add:

1. Check its `source.txt` for the upstream URL
2. Clone the upstream to find the ASDF system names (excluding test systems):
   ```bash
   $ git clone --depth 1 <upstream-url> /tmp/check-project
   $ grep -r 'defsystem' /tmp/check-project --include='*.asd' -h | head
   ```
3. Create the ocicl repo and push (see "Creating a new repo" above)
4. Verify the build succeeds:
   ```bash
   $ gh run list -R ocicl/<package> --limit 1
   ```


## Common maintenance tasks

### Viewing a repo's README.org

```bash
$ gh api repos/ocicl/cffi/contents/README.org -q .content | base64 -d
```

### Manually triggering a rebuild

You can trigger a workflow run via `workflow_dispatch`:

```bash
$ gh workflow run main.yml -R ocicl/cl-str
```

Or force a rebuild by updating the commit hash in README.org:

```bash
$ gh repo clone ocicl/cl-str
$ cd cl-str
# Edit README.org to update the commit hash
$ git add README.org && git commit -m "Force rebuild" && git push
```

### Checking what version is published

```bash
# Check the latest OCI tag
$ oras repo tags ghcr.io/ocicl/str

# Pull the latest artifact to inspect it
$ oras pull ghcr.io/ocicl/str:latest
```

### Listing all systems

The master list of all ocicl systems:

```bash
$ curl -sL https://raw.githubusercontent.com/ocicl/request-system-additions-here/main/all-ocicl-systems.txt | head -20
1am
3b-bmfont
3bgl-shader
3b-hdr
3bmd
...
```

Total count:

```bash
$ curl -sL https://raw.githubusercontent.com/ocicl/request-system-additions-here/main/all-ocicl-systems.txt | wc -l
```

### Finding which repo provides a system

If you know the system name, the repo might have a different name.
Search the org:

```bash
# List all repos in the org
$ gh api orgs/ocicl/repos --paginate -q '.[].name' | grep -i str
cl-str

# Or search README.org content for a system name
$ gh search code "systems | my-system" --owner ocicl
```

### Dealing with missing upstream repos

When the update-check detects a missing upstream repo, it
automatically creates a GitHub issue on the ocicl project repo.

To check whether an upstream repo still exists, use `gh api` rather
than `git ls-remote` — the latter prompts for credentials on missing
repos instead of giving a clean error:

```bash
# Clean 404 on missing repos:
$ gh api repos/author/project

# Avoid — hangs prompting for credentials on 404:
$ git ls-remote https://github.com/author/project.git HEAD
```

To find a new location for a missing upstream:

1. **Check quicklisp-projects** for the canonical source URL. Note
   that quicklisp may also point to the same dead URL, but it's worth
   checking as a starting point:
   ```bash
   $ gh api repos/quicklisp/quicklisp-projects/contents/projects/<name>/source.txt \
       -q .content | base64 -d
   ```
2. **Search for forks or mirrors on GitHub.** The `lisp-mirror`
   organization maintains copies of many Common Lisp projects:
   ```bash
   $ gh search repos "<project-name>" --owner lisp-mirror --json fullName,description
   $ gh search repos "<project-name>" --language "Common Lisp" --json fullName,description
   ```
3. **Check if the author reorganized.** Some authors delete old repos
   and restructure under a new naming convention. Search for other
   repos by the same author:
   ```bash
   $ gh search repos "" --owner <author> --language "Common Lisp" --json fullName,description
   ```
4. **Check the author's self-hosted git server** if the README.org
   description mentions one (e.g., "Mirror of https://git.example.net/...").
   These may also be down.

Once you find a new upstream, update the ocicl repo:

```bash
$ cd $OCICL_ADMIN_HOME
$ gh repo clone ocicl/some-package
$ cd some-package
# Update source URL and set commit to 0000000 to force a rebuild
$ git add README.org && git commit -m "Update source URL" && git push
```

If no replacement can be found and the project appears to be
permanently abandoned, consider archiving the ocicl repo and removing
the system from `all-ocicl-systems.txt`.

### Updating the GHA workflow across repos

The workflow file (`.github/workflows/main.yml`) is identical across
all repos. If the workflow needs updating, each repo must be updated
individually. The `om` script (above) shows the canonical workflow
content.

Note: The workflow references `ocicl/ocicl-action@main`, so changes
to the action code in the
[ocicl-action](https://github.com/ocicl/ocicl-action) repo take
effect across all repos automatically when pushed to the main branch.

### Adding organization secrets

The workflow requires these organization-level secrets:

| Secret | Purpose |
|--------|---------|
| `GPG_SIGNING_KEY` | Base64-encoded GPG private key for tarball signing |
| `GPG_PUBLIC_KEY` | Base64-encoded GPG public key |
| `DOCKERHUB_PASSWORD` | Password for the `ocicl` Docker Hub account |
| `LLM_API_KEY` | API key for LLM-based change summary generation |

These are set at the organization level and inherited by all repos.


## Handling system additions requests

Users request new systems at
https://github.com/ocicl/request-system-additions-here/issues. To
fulfill a request:

1. Note the upstream repo URL and ASDF system names from the issue
2. Create the ocicl repo:
   ```bash
   $ om <package-name>
   $ cd <package-name>
   ```
3. Edit README.org with the source, commit/version, and systems
   (excluding test systems)
4. Get the initial commit hash or version:
   ```bash
   # For git-tracked:
   $ git ls-remote https://github.com/author/project.git HEAD

   # For version-tracked:
   $ gh api repos/author/project/releases/latest -q .tag_name
   ```
5. Push:
   ```bash
   $ git add -A && git commit -m "Add <package-name>" && git push
   ```
6. Wait for the GHA workflow to complete successfully:
   ```bash
   $ gh run list -R ocicl/<package-name> --limit 1
   ```
7. Verify the package is actually available. A successful workflow run
   does not guarantee the artifact was published — the update-check
   stage may have passed with "No update needed" without triggering a
   build (see "Creating a new repo" above for why this happens).
   ```bash
   # Check that the OCI artifact exists
   $ gh api orgs/ocicl/packages/container/<system-name>

   # Verify the system is available to users
   $ ocicl list <system-name>
   ```
8. Only after confirming availability, close the issue with a comment
   noting the system is now available


## Verifying a published artifact

After a build, verify the published artifact:

```bash
# Pull the tarball
$ oras pull ghcr.io/ocicl/str:latest
Downloading 577fc7118b8a cl-str-20230511-b1c8380.tar.gz
Downloaded  577fc7118b8a cl-str-20230511-b1c8380.tar.gz
Pulled [registry] ghcr.io/ocicl/str:latest

# Pull the signature
$ oras pull ghcr.io/ocicl/str.sha256sum.sig:latest

# Verify the signature
$ sha256sum cl-str-20230511-b1c8380.tar.gz \
  | gpg --verify cl-str-20230511-b1c8380.tar.gz.sha256sum.sig -
gpg: Signature made Thu 11 May 2023 05:44:45 AM EDT
gpg:                using RSA key B96ACDBF35C5C1AB81596FB6D3AFE1884397BDC8
gpg: Good signature from "ocicl-tarball-signer" [ultimate]

# Check the rekor transparency log
$ rekor-cli search --sha $(sha256sum cl-str-20230511-b1c8380.tar.gz | sha256sum -)
```


## Troubleshooting

### Build fails in the sandbox container

Check the workflow run logs:

```bash
$ gh run list -R ocicl/<package> --limit 5
$ gh run view <run-id> -R ocicl/<package> --log-failed
```

Common causes:
- The upstream source has build dependencies not in the sandbox image
- The ASDF system names in `systems` don't match what the `.asd` file defines
- The upstream repo requires submodules or special build steps

### Workflow succeeds but package is not available

All workflow runs show as successful, but `ocicl list <system>` returns
nothing. This typically means the artifact was never published. The
update-check stage can pass with "No update needed" without ever
triggering the build/publish stage. This happens when the commit hash
in README.org already matches the upstream HEAD — common when a new
repo is created with the current hash.

Fix: set the commit hash to a dummy value to force the update-check
to detect a difference:

```bash
$ cd $OCICL_ADMIN_HOME
$ gh repo clone ocicl/<package>
$ cd <package>
# Change the commit hash to a dummy value
# (edit README.org: set commit to 0000000)
$ git add README.org && git commit -m "Force initial build" && git push
```

You can also verify whether an artifact exists in the registry:

```bash
$ gh api orgs/ocicl/packages/container/<system-name>
```

A 404 confirms the artifact was never published.

### Workflow disabled due to inactivity

GitHub automatically disables scheduled workflows after 60 days of
inactivity. When this happens, pushes to the repo will not trigger
the workflow, and the package will stop receiving updates silently.

Check if a workflow is disabled:

```bash
$ gh api repos/ocicl/<package>/actions/workflows \
    -q '.workflows[] | "\(.id) \(.state) \(.name)"'
```

Re-enable it:

```bash
$ gh api -X PUT repos/ocicl/<package>/actions/workflows/<workflow-id>/enable
```

After re-enabling, trigger a run manually since the push that was
missed won't be retried:

```bash
$ gh workflow run main.yml -R ocicl/<package>
```

### Update-check keeps reporting "No update needed"

Verify that the commit hash or version in README.org actually differs
from the upstream:

```bash
# For commit-tracked repos:
$ gh api repos/ocicl/<package>/contents/README.org -q .content | base64 -d | grep commit
$ git ls-remote <upstream-url> HEAD

# For version-tracked repos:
$ gh api repos/ocicl/<package>/contents/README.org -q .content | base64 -d | grep version
$ gh api repos/<upstream-owner>/<upstream-repo>/releases/latest -q .tag_name
```

### OCI push fails with permission errors

Ensure the workflow has the correct permissions:

```yaml
permissions:
  issues: write
  packages: write
  contents: write
```

And that the organization secrets are properly configured and
accessible to the repo.
