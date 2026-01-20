# cl-selfupdate

Self-update functionality for Common Lisp dumped image executables via GitHub and GitLab Releases.

Inspired by [go-github-selfupdate](https://github.com/rhysd/go-github-selfupdate).

## Features

- Multi-provider support: GitHub and GitLab
- Detect latest releases from Releases APIs
- Semantic version comparison
- Platform/architecture detection (Linux, macOS, Windows; amd64, arm64, etc.)
- Download and extract archives (.tar.gz, .zip, .gz)
- Atomic executable replacement with backup
- SHA256 checksum validation
- Token authentication (optional, for private repos or rate limits)
- Pluggable HTTP backends (dexador or drakma)
- Pure TLS via [pure-tls](https://github.com/atgreen/pure-tls) - no OpenSSL required

## Installation

```sh
ocicl install cl-selfupdate
```

## HTTP Backend Selection

cl-selfupdate supports multiple HTTP client libraries. Choose the one that fits your project:

```lisp
;; Use dexador (recommended, lighter weight)
(asdf:load-system :cl-selfupdate/dexador)

;; Or use drakma (if your project already uses it)
(asdf:load-system :cl-selfupdate/drakma)
```

If you load the core `cl-selfupdate` system without a backend, you'll get a `no-http-backend` error when making requests.

## Usage

### Basic Self-Update (GitHub)

```lisp
(require :cl-selfupdate/dexador)  ; or :cl-selfupdate/drakma

;; Set your current version
(setf cl-selfupdate:*current-version* "1.0.0")

;; Check for and apply updates
(cl-selfupdate:update-self "owner" "repo")
```

### Using GitLab

```lisp
;; Set default provider globally
(setf cl-selfupdate:*default-provider* cl-selfupdate:*gitlab*)

;; Or specify per-call with keyword
(cl-selfupdate:update-self "mygroup" "myproject"
                           :current-version "1.0.0"
                           :provider :gitlab)

;; For self-hosted GitLab, create a custom instance
(defvar *my-gitlab*
  (make-instance 'cl-selfupdate:gitlab-provider
                 :api-base "https://gitlab.mycompany.com/api/v4"))

(cl-selfupdate:update-self "mygroup" "myproject" :provider *my-gitlab*)
```

### Check If Update Available

```lisp
(multiple-value-bind (release newer-p)
    (cl-selfupdate:update-available-p "owner" "repo"
                                      :current-version "1.0.0")
  (when newer-p
    (format t "New version available: ~A~%"
            (cl-selfupdate:release-tag release))))
```

### Dry Run (Download Without Installing)

```lisp
(cl-selfupdate:update-self "owner" "repo"
                           :current-version "1.0.0"
                           :dry-run t)
```

### Release Notes

Release notes are automatically displayed during updates. You can also access them programmatically:

```lisp
;; update-self returns release notes as 4th value
(multiple-value-bind (updated-p new-version old-version notes)
    (cl-selfupdate:update-self "owner" "repo" :current-version "1.0.0")
  (when notes
    (format t "What's new:~%~A~%" notes)))

;; Or get notes from a release struct
(let ((release (cl-selfupdate:get-latest-release
                cl-selfupdate:*github* "owner" "repo")))
  (format t "~A~%" (cl-selfupdate:release-notes release)))

;; Suppress automatic display
(cl-selfupdate:update-self "owner" "repo"
                           :current-version "1.0.0"
                           :show-notes nil)
```

### Manual Download and Apply

```lisp
;; Download update
(let ((new-exe (cl-selfupdate:download-update "owner" "repo"
                                              :executable-name "myapp")))
  ;; Apply when ready
  (cl-selfupdate:apply-update new-exe))
```

### GitHub Authentication

For private repositories or to avoid rate limits:

```lisp
;; Via variable
(setf cl-selfupdate:*github-token* "ghp_xxxx")

;; Or via environment variable
;; export GITHUB_TOKEN=ghp_xxxx
```

### GitLab Authentication

For private repositories:

```lisp
;; Via variable
(setf cl-selfupdate:*gitlab-token* "glpat-xxxx")

;; Or via environment variable
;; export GITLAB_TOKEN=glpat-xxxx
```

### SHA256 Validation

Downloads are automatically validated against SHA256 checksums when available:

```lisp
;; Enabled by default
(setf cl-selfupdate:*validate-downloads* t)

;; Disable validation
(cl-selfupdate:update-self "owner" "repo"
                           :current-version "1.0.0"
                           :validate nil)
```

Supported checksum formats:
- Per-file checksums: `myapp_linux_amd64.tar.gz.sha256`
- Unified checksum files: `checksums.txt`, `SHA256SUMS`
- goreleaser-style checksum files

If validation fails, a `validation-error` condition is signaled:

```lisp
(handler-case
    (cl-selfupdate:update-self "owner" "repo" :current-version "1.0.0")
  (cl-selfupdate:validation-error (e)
    (format t "Checksum mismatch!~%Expected: ~A~%Got: ~A~%"
            (cl-selfupdate:validation-error-expected e)
            (cl-selfupdate:validation-error-actual e))))
```

## Release Naming Convention

Assets should be named with OS and architecture suffixes:

```
myapp_linux_amd64.tar.gz
myapp_darwin_arm64.tar.gz
myapp_windows_amd64.zip
myapp-linux-amd64
```

Supported patterns:
- Separators: `_` or `-`
- OS: `linux`, `darwin`, `windows`, `freebsd`, `openbsd`, `netbsd`
- Arch: `amd64`, `arm64`, `386`, `arm`, `ppc64`, `riscv64`
- Extensions: `.tar.gz`, `.tgz`, `.zip`, `.gz`, or none

## Example: Self-Updating Application

```lisp
(defpackage #:myapp
  (:use #:cl)
  (:export #:main))

(in-package #:myapp)

(defvar *version* "1.0.0")

(defun check-for-updates ()
  (handler-case
      (cl-selfupdate:update-self
       "myuser" "myapp"
       :current-version *version*
       :executable-name "myapp")
    (error (e)
      (format *error-output* "Update check failed: ~A~%" e))))

(defun main ()
  (when (member "--update" (uiop:command-line-arguments) :test #'string=)
    (check-for-updates)
    (uiop:quit))
  ;; ... rest of application
  )
```

## API Reference

### ASDF Systems

- `cl-selfupdate` - Core system (requires an HTTP backend)
- `cl-selfupdate/dexador` - Core + dexador HTTP backend
- `cl-selfupdate/drakma` - Core + drakma HTTP backend

### HTTP Backend

The library uses a pluggable HTTP backend system:

```lisp
;; Check which backend is loaded
cl-selfupdate:*http-backend*  ; => :dexador or :drakma

;; Custom backends can implement these generic functions:
(cl-selfupdate:http-request backend url &key method headers)
(cl-selfupdate:http-get backend url &key headers)
(cl-selfupdate:http-get-stream backend url &key headers)
```

### Provider Classes (CLOS)

The library uses CLOS for provider abstraction, making it easy to add new providers:

- `provider` - Base class for all providers
- `github-provider` - GitHub releases provider
- `gitlab-provider` - GitLab releases provider
- `*github*` - Default GitHub provider instance
- `*gitlab*` - Default GitLab provider instance
- `*default-provider*` - Default provider (initially `*github*`)

### Generic Functions (dispatch on provider class)

```lisp
;; These dispatch based on the provider's class
(list-releases provider owner repo &key per-page)
(get-latest-release provider owner repo)
(detect-latest provider owner repo &key include-prerelease)
(download-asset provider asset &key output-path)
```

### High-Level Functions

- `update-self` - Check for and apply updates
- `update-available-p` - Check if update is available
- `download-update` - Download latest release
- `apply-update` - Apply downloaded update

All high-level functions accept `:provider` which can be:
- A provider instance (`*github*`, `*gitlab*`)
- A keyword (`:github`, `:gitlab`)
- NIL (uses `*default-provider*`)

### Creating Custom Provider Instances

```lisp
;; For GitHub Enterprise
(defvar *my-github*
  (make-instance 'cl-selfupdate:github-provider
                 :api-base "https://github.mycompany.com/api/v3"
                 :token "my-token"))

;; For self-hosted GitLab
(defvar *my-gitlab*
  (make-instance 'cl-selfupdate:gitlab-provider
                 :api-base "https://gitlab.mycompany.com/api/v4"
                 :token "my-token"))

;; Use custom provider
(cl-selfupdate:update-self "owner" "repo" :provider *my-gitlab*)
```

## Dependencies

Core dependencies:
- [jsown](https://github.com/madnificent/jsown) - JSON parsing
- [cl-semver](https://github.com/cldm/cl-semver) - Semantic versioning
- [chipz](https://github.com/froydnj/chipz) - Decompression
- [archive](https://github.com/froydnj/archive) - Archive extraction
- [zip](https://github.com/froydnj/zip) - ZIP archive support
- [flexi-streams](https://github.com/edicl/flexi-streams) - Flexible streams
- [pure-tls](https://github.com/atgreen/pure-tls) - TLS 1.3 (no OpenSSL)
- [alexandria](https://gitlab.common-lisp.net/alexandria/alexandria) - Utilities
- [cl-ppcre](https://edicl.github.io/cl-ppcre/) - Regular expressions
- [ironclad](https://github.com/sharplispers/ironclad) - SHA256 checksums
- [quri](https://github.com/fukamachi/quri) - URI handling

HTTP backend (choose one):
- [dexador](https://github.com/fukamachi/dexador) - HTTP client (via cl-selfupdate/dexador)
- [drakma](https://github.com/edicl/drakma) - HTTP client (via cl-selfupdate/drakma)

## Author and License

`cl-selfupdate` was written by Anthony Green <green@moxielogic.com>
and is distributed under the terms of the MIT license.
