# ocicl
> A modern ASDF system distribution and management tool for Common Lisp

NOTE: To request additions to the ``ocicl`` repo, create an Issue
[here](https://github.com/ocicl/request-system-additions-here/issues/new?assignees=&labels=&projects=&template=request.yml&title=%5BSystem+Request%5D%3A+).

What is it?
------------
``ocicl`` is a modern alternative to quicklisp.  It is modern in the sense that:
* All software is [packaged as OCI-compliant artifacts](https://github.com/opencontainers/) and distributed from mirrored OCI-compliant registries (the GitHub and Docker Hub Container Registries).
* All software packages are securely distributed over TLS connections.
* All connections respect ``HTTPS_PROXY`` environment settings for authenticated proxy support.
* [sigstore](https://www.sigstore.dev/) is used to ensure the integrity and authenticity of all software packages.
* Software packages are project-local by default, simplifying the process of tying specific versions to your projects.
* All software packages are built and published transparently using hosted CI infrastructure ([github actions](https://github.com/ocicl/ocicl-action)).
* LLM-generated summaries of changes between versions are available for all packages.

``ocicl`` is pronounced like
"[ossicle](https://en.wikipedia.org/wiki/Ossicles)", a tiny bone
embedded in your middle ear.  Like the ossicles in your ear, the
``ocicl-runtime`` is a tiny library that is embedded in your lisp
image.  It is responsible for finding and loading
[ASDF](https://asdf.common-lisp.dev/) systems that you manage with the
``ocicl`` command line tool.

The main innovation behind ``ocicl`` is the idea of applying the
ecosystem of tooling and services from the world of application
container images to ordinary tarballs of Lisp code. In essence, OCI + CL = ``ocicl``.

``ocicl`` is under active development.  The ``ocicl-runtime`` is known
to work with the following Common Lisp implementations:
- [abcl](https://abcl.org)
- [ecl](https://gitlab.com/embeddable-common-lisp)
- [SBCL](https://www.sbcl.org/)
- [Allegro Common Lisp](https://franz.com/products/allegro-common-lisp/)

However, the ``ocicl`` command-line tool currently must be built with
SBCL on either Linux, Windows or MacOS.  Adapting to other systems and
platforms should not be difficult, and pull requests are welcome at
https://github.com/ocicl/ocicl/pulls.  Feedback is also welcome at
https://github.com/ocicl/ocicl/issues.

Quick Start
------------

You can install `ocicl` one of two ways: with
[homebrew](https://brew.sh) or from source.

For [homebrew](https://brew.sh) on Linux, Windows WSL, or macOS,
install and configure `ocicl` as follows:
```
green@fedora:~$ brew install ocicl
==> Downloading https://ghcr.io/v2/homebrew/core/ocicl/manifests/2.3.4
########################################################################################################################################## 100.0%
==> Fetching ocicl
==> Downloading https://ghcr.io/v2/homebrew/core/ocicl/blobs/sha256:fe9b2d51c012851588baef450ff39b453526a7fc2c5df38e9071fc253b136150
########################################################################################################################################## 100.0%
==> Pouring ocicl--2.3.4.x86_64_linux.bottle.tar.gz
🍺  /home/linuxbrew/.linuxbrew/Cellar/ocicl/2.3.4: 8 files, 36.4MB
==> Running `brew cleanup ocicl`...
green@fedora:~$ ocicl setup
;; Add the following to your lisp startup file
;; (~/.sbclrc, ~/.eclrc, ~/.abclrc or ~/.roswell/init.lisp):

#-ocicl
(when (probe-file #P"/home/green/.local/share/ocicl/ocicl-runtime.lisp")
  (load #P"/home/green/.local/share/ocicl/ocicl-runtime.lisp"))
(asdf:initialize-source-registry
  (list :source-registry (list :directory (uiop:getcwd)) :inherit-configuration))
```

To install from source, run ``sbcl --load setup.lisp`` in the source
directory.  This will build and install the ``ocicl`` binary in
``~/.local/bin`` on non-Windows systems, and
``%UserProfile%\AppData\Local\ocicl\`` on Windows.

The `setup.lisp` script will build an `ocicl` binary with 3072MB of
dynamic memory space.  If you need a different amount, run it like so:
```
sbcl --eval "(defconstant +dynamic-space-size+ 2048)" --load setup.lisp
```

Note that on Windows systems you will need `openssl.dll`.  For Windows
CI testing, we install `openssl.dll` using chocolatey, like so: `choco
install openssl.light`.

Now run ``ocicl setup``.  This is a mandatory step that installs the
``ocicl-runtime`` library, and suggests configurations for your
``${HOME}/.sbclrc`` file.

```
$ ocicl setup
;; Add the following to your lisp startup file
;; (~/.sbclrc, ~/.eclrc, ~/.abclrc or ~/.roswell/init.lisp):

#-ocicl
(when (probe-file #P"/home/green/.local/share/ocicl/ocicl-runtime.lisp")
  (load #P"/home/green/.local/share/ocicl/ocicl-runtime.lisp"))
(asdf:initialize-source-registry
  (list :source-registry (list :directory (uiop:getcwd)) :inherit-configuration))
```

The default behavior for the runtime is to invoke ``ocicl`` when ASDF
tries to load a system that it can't find.

If you are running behind a proxy, be sure to set your ``https_proxy``
environment variable appropriately.  For instance, the following could
be used for an authenticating proxy:
```
$ export https_proxy=https://username:password@myproxyhost:8080
```

Proxy Configuration
-------------------

`ocicl` and its HTTP client respect common proxy environment variables.

- Supported: `HTTPS_PROXY`, `https_proxy`, `HTTP_PROXY`, `http_proxy`
- Format options:
  - `http[s]://user:pass@host[:port]`
  - `host[:port]` (scheme assumed http)
  - `user:pass@host[:port]`
- Bypass list: `NO_PROXY`/`no_proxy` as a comma-separated list of hosts/domains
  - Example: `export NO_PROXY=localhost,127.0.0.1,.example.com`
- Authentication: sends HTTP Basic credentials via `Proxy-Authorization`.
- Precedence: first non-empty variable in the order above is used.

Security note: Environment variables can be visible to other processes/users on the
system. Prefer short-lived shells and avoid committing credentials into scripts.

TLS Verification
----------------

- Verification: Enabled by default (server certificates are verified).
- Insecure mode: Use `-k` or `--insecure` to skip certificate verification (similar to `curl -k`).
  - Alternatively, set `OCICL_INSECURE=1` in the environment.
- Custom CA paths:
  - `OCICL_CA_FILE`: path to a PEM bundle file
  - `OCICL_CA_DIR`: path to a directory containing CA files
- Connection timeout:
  - `OCICL_HTTP_TIMEOUT`: connection timeout in seconds for HTTP requests

Troubleshooting
- CA not trusted: set `OCICL_CA_FILE`/`OCICL_CA_DIR` to include your organization’s CA bundle (common on corporate proxies), or temporarily use `-k` for testing.
- Hostname mismatch: verify the registry host (`--registry`) and proxy settings (`HTTPS_PROXY`/`HTTP_PROXY`/`NO_PROXY`).
- Self-signed certificate: provide the signing CA via `OCICL_CA_FILE`/`OCICL_CA_DIR`, or use `-k` for testing only.
- Expired/not yet valid: check your system clock and the server’s certificate validity.
- Timeouts: check network/proxy reachability and consider increasing `OCICL_HTTP_TIMEOUT`.

Disabling verification reduces security and should be used only for testing on trusted networks.

Now try running this:
```
$ sbcl --eval "(asdf:load-system :str)"
```

Look at your current directory.  You should see a directory called
``ocicl`` and a file called ``ocicl.csv``.  The ``ocicl``
directory contains the code you just downloaded, and ``ocicl.csv``
contains a mapping of system names to OCI artifacts and ``.asd``
files.

```
str.test, ghcr.io/ocicl/str@sha256:0903b59c33d3026ac55a6f4b25a79094d08e3110758d8ae728bf4188db659313, cl-str-20230511-b1c8380/str.test.asd
str, ghcr.io/ocicl/str@sha256:0903b59c33d3026ac55a6f4b25a79094d08e3110758d8ae728bf4188db659313, cl-str-20230511-b1c8380/str.asd
cl-ppcre, ghcr.io/ocicl/cl-ppcre@sha256:5274824d397fa197d5c7790344ace27f2a30fc34c6cadb0a9fcce7d1e4052486, cl-ppcre-20230511-b4056c5a/cl-ppcre.asd
cl-ppcre-unicode, ghcr.io/ocicl/cl-ppcre@sha256:5274824d397fa197d5c7790344ace27f2a30fc34c6cadb0a9fcce7d1e4052486, cl-ppcre-20230511-b4056c5a/cl-ppcre-unicode.asd
cl-unicode, ghcr.io/ocicl/cl-unicode@sha256:b61ac07aed06c926720e6a4c155fd0c9411b01a05ee7ebba55fca7df491880e5, cl-unicode-20230511-2790a6b/cl-unicode.asd
flexi-streams, ghcr.io/ocicl/flexi-streams@sha256:091df0cda6006b19aa206b022bb6d06fd9d5e5787b6152b9f0ae6846926ac5e0, flexi-streams-20230511-74a1027/flexi-streams.asd
flexi-streams-test, ghcr.io/ocicl/flexi-streams@sha256:091df0cda6006b19aa206b022bb6d06fd9d5e5787b6152b9f0ae6846926ac5e0, flexi-streams-20230511-74a1027/flexi-streams-test.asd
trivial-gray-streams, ghcr.io/ocicl/trivial-gray-streams@sha256:e82a60fdccc33916f26b60a3af63ee110f0b364cc2af59eee4be86256e8ea2b6, trivial-gray-streams-20230511-2b3823e/trivial-gray-streams.asd
trivial-gray-streams-test, ghcr.io/ocicl/trivial-gray-streams@sha256:e82a60fdccc33916f26b60a3af63ee110f0b364cc2af59eee4be86256e8ea2b6, trivial-gray-streams-20230511-2b3823e/trivial-gray-streams-test.asd
cl-change-case, ghcr.io/ocicl/cl-change-case@sha256:61791ee49f0160adad694eedbe8804fe9bcebad54336b0fbb8ce1a82091e20fa, cl-change-case-0.2.0/cl-change-case.asd
```

The next time you try to load ``str``, ASDF will load the code that
you've already downloaded and compiled.

Now try deleting the ``ocicl`` directory, and loading ``str`` again
as above.  ``ocicl`` will download the exact version specified in the
``ocicl.csv`` file.  The idea here is that you would commit your
``ocicl.csv`` file to your project's source repo, but never the
``ocicl`` directory.  When you run your program, you will always be
using the library versions locked in your ``ocicl.csv`` file.

Now let's try the ``ocicl`` command line tool.

```
ocicl 2.7.0 - copyright (C) 2023-2025 Anthony Green <green@moxielogic.com>

Usage: ocicl [-v|--verbose] [-f|--force] [-g|--global] [-r|--registry REGISTRY]
             [-c|--color WHEN] [-d|--depth NUM] [-k|--insecure] command

Available options:
  -v, --verbose           produce verbose output
  -f, --force             force action
  -g, --global            operate on the global system collection
  -r, --registry REGISTRY use alternate oci registry [Default: ghcr.io/ocicl]
  -c, --color WHEN        color the output WHEN (auto, always, or never)
  -d, --depth NUM         maximum depth for the tree command (integer or "max", default 1)
  -k, --insecure          allow insecure TLS (skip certificate verification)

Choose from the following ocicl commands:

   help                                   Print this help text
   changes [SYSTEM[:VERSION]]...          Display changes
   clean                                  Remove system directories not listed in ocicl.csv
   diff SYSTEM                            Diff between the installed and latest versions
   diff SYSTEM VERSION                    Diff between the installed version and VERSION
   diff SYSTEM VERSION1 VERSION2          Diff between files in different system versions
   install [SYSTEM[:VERSION]]...          Install systems
   latest [SYSTEM]...                     Install latest version of systems
   libyear                                Calculate the libyear dependency freshness metric
   lint PATH...                           Lint Common Lisp files, directories, or .asd systems
   list SYSTEM...                         List available system versions
   new APP-NAME [TEMPLATE] [KEY=VALUE]... Create a new app
   remove [SYSTEM]...                     Remove systems
   setup [GLOBALDIR]                      Mandatory ocicl configuration
   templates [list]                       List available templates
   templates dirs                         Show template search path
   tree [SYSTEM]...                       Print tree of installed systems
   version                                Show the ocicl version information

Distributed under the terms of the MIT License
```

If we again delete the ``ocicl`` directory, running ``ocicl
install`` will download all of the systems specified in your
``ocicl.csv`` file.

```
$ ocicl install
; downloading ghcr.io/ocicl/str@sha256:0903b59c33d3026ac55a6f4b25a79094d08e3110758d8ae728bf4188db659313
; downloading ghcr.io/ocicl/cl-ppcre@sha256:5274824d397fa197d5c7790344ace27f2a30fc34c6cadb0a9fcce7d1e4052486
; downloading ghcr.io/ocicl/cl-unicode@sha256:b61ac07aed06c926720e6a4c155fd0c9411b01a05ee7ebba55fca7df491880e5
; downloading ghcr.io/ocicl/flexi-streams@sha256:091df0cda6006b19aa206b022bb6d06fd9d5e5787b6152b9f0ae6846926ac5e0
; downloading ghcr.io/ocicl/trivial-gray-streams@sha256:e82a60fdccc33916f26b60a3af63ee110f0b364cc2af59eee4be86256e8ea2b6
; downloading ghcr.io/ocicl/cl-change-case@sha256:61791ee49f0160adad694eedbe8804fe9bcebad54336b0fbb8ce1a82091e20fa
```

You can download additional systems like so:
```
$ ocicl install trivial-garbage
; downloaded ghcr.io/ocicl/trivial-garbage@sha256:a85dcf4110ad60ae9f6c70970acceb2bf12402ce5326891643d35506059afb1d
```

This downloads the latest version of trivial-garbage, which is the OCI
image with the ``latest`` tag, and is equivalent to ``ocicl install
trivial-garbage:latest``.

To see what other versions of a package are available, run ``ocicl list trivial-garbage``
```
$ ocicl list trivial-garbage
trivial-garbage:
 latest
 20230511-b3af9c0
```

Here we only have one version, 20230511-b3af9c0, which also has the
``latest`` tag.  Many lisp libraries are built from git sources
without release tags.  In this case, the version label represents the
build date and the git commit hash (b3af9c0).

To install any specific version of a system, just use the appropriate
version label in your ``ocicl install`` command.  For example:
``ocicl install trivial-garbage:20230511-b3af9c0`` or
``ocicl install str:latest``.

Running ``ocicl install SYSTEM`` with no version will do nothing
if any version of ``SYSTEM`` is already installed (unless
``--force`` is specified).  However, if a system version is specified
(including ``latest``) then ``ocicl install`` will always download and
install the system, even if it already exists on disk.

To compare differences between system versions, run `ocicl diff SYSTEM VERSION1
VERSION2`.

To update all systems in your ``ocicl.csv`` file to the latest
version, run ``ocicl latest``.

To remove an installed system, use ``ocicl remove``.  By default,
``ocicl`` will refuse to remove systems that are required to satisfy any
dependencies.  Use the ``ocicl --force remove`` option to ignore any
dependencies and always uninstall.

To use an alternate OCI registry for any operation, use the
``--registry`` option.  Using ``--registry`` with the ``setup``
command will persist this registry choice for all future ``ocicl``
invocations.  Subsequent uses of ``setup`` will preserve existing
registry choices unless the ``--force`` option used.

While the `ocicl` cli tool only supports setting a single alternate
registry, it's possible to use multiple registries by adding multiple
entries to the `ocicl-registry.cfg` file in your `${XDG_DATA_DIR}/ocicl`
directory.

In the examples so far, we see that all system downloads are recorded
in the current working directory.  This is the default behaviour.
However, when `ocicl.csv` appears in any parent directory, all
systems are downloaded and recorded in the `ocicl` sub-directory of
that parent.  The ocicl runtime mirrors this behaviour when it comes
to loading systems.  See the following for example usage.

```
green@fedora:~/hacking$ touch ocicl.csv
green@fedora:~/hacking$ mkdir project-1
green@fedora:~/hacking$ mkdir project-2
green@fedora:~/hacking$ cd project-1
green@fedora:~/hacking/project-1$ ocicl install str
; downloaded str@sha256:3771c466d33a923d4fd1b1aa71f7a60d0029405e7fb3bafae1a3f19b7ac9b121
; downloaded cl-ppcre@sha256:584907e0683621973579f397115945ef73e9d5b7afa77fae7cacecb3ad272f89
; downloaded cl-unicode@sha256:d98f12c1271fb3c1812a27d64dfbe95b0bc5bcfd545b71b8101874e61270b120
; downloaded cl-change-case@sha256:63b6a033f762d6dc5d365ce49f2a2c691677f2ec1375ebe4226d13b19a29dc7c
green@fedora:~/hacking/project-1$ cd ../project-2/
green@fedora:~/hacking/project-2$ ocicl install str
; str:1bcf26d already exists
green@fedora:~/hacking/project-2$ ls -l ../ocicl
total 0
drwxr-xr-x. 1 green green 112 Oct 17 23:05 cl-change-case-0.2.1
drwxr-xr-x. 1 green green 642 Oct 17 23:05 cl-ppcre-20240503-80fb19d
drwxr-xr-x. 1 green green 216 Oct 17 23:05 cl-str-20240708-1bcf26d
drwxr-xr-x. 1 green green 344 Oct 17 23:05 cl-unicode-20240503-07e7ff5
```

You may also choose to download systems "globally" for the current
user by using the ``--global`` option.  This is equivalent to
temporarily changing directory to a user-global directory before
performing any operation with the `ocicl` cli.  These "global" systems
are available at runtime using the following heuristic:
- If the system is available locally, then it is loaded from from the local `ocicl` directory.
- Else if the system is available in the global `ocicl` directory, it loaded from there.
- Otherwise, if `ocicl-runtime:*download*` is non-nil, then the system is downloaded either locally or globally:
    - if `ocicl-runtime:*force-global*` is non-nil, then the system is downloaded to the global `ocicl` directory.
    - else if `ocicl-runtime:*force-global*` is nil (default), then the system is downloaded locally.

To change the default user-global directory, provide the
optional ``GLOBALDIR`` argument when you invoke ``ocicl setup``.

You can change the default behaviour of downloading systems on demand
by setting ``ocicl-runtime:*download*`` to nil.

To get a list of all of the systems already downloaded (both locally
and globally), call `(ocicl-runtime:system-list)`.

AI-Generated Change Summaries
-----------------------------

The `ocicl` tool can provide summaries of changes between versions of
Lisp systems.  These summaries are produced by an LLM, and are
designed to describe key changes and user impacts for newer versions
of systems you depend on.

* `ocicl changes`: describes every change for every newer version of systems `ocicl` has installed
* `ocicl -v changes`: same, but provides verbose reporting on progress
* `ocicl changes cl-chat`: describes changes for every newer version of `cl-chat`
* `ocicl changes omg:20240427-5b316a0`: describes changes for every version of `omg` newer than `20240427-5b316a0`.

These summaries are pre-generated by the `ocicl` maintenance system by
feeding source code diffs to an LLM and uploading the results to the
OCI registry.

In some cases the description may be missing as they only started
being generated as of May 2024.

Code Linting
------------

`ocicl` includes an integrated Common Lisp linter that checks your code for style issues, common errors, and best practices. The linter supports linting individual files, entire directories, or ASDF system definitions.

### Usage

```bash
ocicl lint PATH...
```

Where `PATH` can be:
- **Individual files**: `ocicl lint myfile.lisp`
- **Directories**: `ocicl lint src/` (recursively lints all `.lisp` files)
- **ASDF systems**: `ocicl lint myproject.asd` (lints all components in the system definition)

### Features

- Comprehensive style checking based on Common Lisp best practices
- Detects common errors and anti-patterns
- Configurable rules via `.ocicl-lint.conf` file
- Colorized output (respects `--color` flag)
- Zero configuration required for basic usage

### Configuration

Create a `.ocicl-lint.conf` file in your project root to customize linting behavior:

```ini
# .ocicl-lint.conf
# Maximum line length (default: 120)
max-line-length = 100

# Rules to suppress globally (comma-separated)
suppress-rules = eval-usage, trailing-whitespace

# You can also use line-specific suppression with comments:
# ; lint:suppress rule-name
# ; lint:suppress  (suppress all rules on this line)
```

### Example Output

```bash
$ ocicl lint src/main.lisp
src/main.lisp:42:10: unused-parameter: Parameter FOO is unused
src/main.lisp:55:121: max-line-length: Line exceeds 120 characters
src/main.lisp:67:1: naming-underscore: Use hyphens instead of underscores in symbol names

Scanned 1 file(s), found 3 issue(s).
```

The linter returns exit code 0 if no issues are found, or 1 if issues are detected.

Dependency Freshness
--------------------

`ocicl` can compute the [libyear](https://libyear.com) dependency
freshness metric for the projects on which you depend.  It is a single
number telling you how up-to-date your dependencies are.  The libyear
value for a single project indicates the time between your version and
the most recent version.

```
$ ocicl libyear
OMGlib              0.02 libyears (6.01 days)
cl-opengl           0.02 libyears (7.01 days)
lqn                 0.01 libyears (1.00 days)
openapi-generator   0.01 libyears (1.00 days)
trivial-arguments   0.01 libyears (3.01 days)
graven-image        0.01 libyears (2.01 days)
cl-oju              0.02 libyears (4.01 days)
py4cl2-cffi         0.02 libyears (6.01 days)

TOTAL libyears: 0.09 (30.06 days)
```

Security
--------

All system tarballs have their sha256sum digest digitally signed with
the ocicl-tarball-signer key:
[`B96ACDBF35C5C1AB81596FB6D3AFE1884397BDC8`](https://raw.githubusercontent.com/ocicl/ocicl/refs/heads/main/ocicl-tarball-signer-public-key.asc).

You can download the unexpanded tarballs using [skopeo](https://github.com/containers/skopeo) or [oras](https://oras.land/) like so:
```
$ oras pull ghcr.io/ocicl/str:latest
Downloading 577fc7118b8a cl-str-20230511-b1c8380.tar.gz
Downloaded  577fc7118b8a cl-str-20230511-b1c8380.tar.gz
Pulled [registry] ghcr.io/ocicl/str:latest
Digest: sha256:0903b59c33d3026ac55a6f4b25a79094d08e3110758d8ae728bf4188db659313

$ ls -l
total 32
-rw-r--r--. 1 green green 24609 May 19 09:02 cl-str-20230511-b1c8380.tar.gz
```

Similarly, the signature is available by appending ``.sha256sum.sig`` to the system name.
```
$ oras pull ghcr.io/ocicl/str.sha256sum.sig:latest
Downloading 2a97da913ef7 cl-str-20230511-b1c8380.tar.gz.sha256sum.sig
Downloaded  2a97da913ef7 cl-str-20230511-b1c8380.tar.gz.sha256sum.sig
Pulled [registry] ghcr.io/ocicl/str.sig:latest
Digest: sha256:47903679d96504c5e83f08f7d6dfc4e613e7ab968e44dc46cb13b29f7917ddea
```

You can verify the signature like so:
```
$ sha256sum cl-str-20230511-b1c8380.tar.gz | gpg --verify cl-str-20230511-b1c8380.tar.gz.sha256sum.sig -
gpg: Signature made Thu 11 May 2023 05:44:45 AM EDT
gpg:                using RSA key B96ACDBF35C5C1AB81596FB6D3AFE1884397BDC8
gpg: Good signature from "ocicl-tarball-signer" [ultimate]
```

These signatures are also archived in the
[sigstore](https://www.sigstore.dev) [rekor transparency
log](https://docs.sigstore.dev/logging/overview/).  This gives you and
your auditors confidence that the code you are running is what it
claims to be.

You can search for these signatures based on the sha of the sha of the tarball
like so:
```
$ rekor-cli search --sha $(sha256sum cl-str-20230511-b1c8380.tar.gz | sha256sum -)
Found matching entries (listed by UUID):
24296fb24b8ad77a6594635675d0e6365b89ee0d5e3b1ce823adb19c28aa3602c2537163710638d9

$ rekor-cli get --uuid 24296fb24b8ad77a6594635675d0e6365b89ee0d5e3b1ce823adb19c28aa3602c2537163710638d9
LogID: c0d23d6ad406973f9559f3ba2d1ca01f84147d8ffc5b8445c224f98b9591801d
Index: 20300488
IntegratedTime: 2023-05-11T09:44:49Z
UUID: 24296fb24b8ad77a6594635675d0e6365b89ee0d5e3b1ce823adb19c28aa3602c2537163710638d9
Body: {
  "RekordObj": {
    "data": {
      "hash": {
        "algorithm": "sha256",
        "value": "577fc7118b8a21285ad871dd44e4fe25126fd05d2d4fad52a4015d5a01788d44"
      }
    },
    "signature": {
      "content": "LS0tLS1CRUdJTiBQR1AgU0lHTkFUVVJFLS0tLS0KCmlRR3pCQUFCQ2dBZEZpRUV1V3JOdnpYRndhdUJXVysyMDYvaGlFT1h2Y2dGQW1SY3VRMEFDZ2tRMDYvaGlFT1gKdmNqcGh3d0FsSUJ6N3IrcnhZSml5dHhHZlFXMDJ6Z0tzQ1BKcC9RRXI1NUdIZjBQN3U0QlBod0ZmRlFRbWhRWQpsYndoclpjMEcvRFRXdm5vdzBOa0RTRXFBbVhtUjIyMzJOWDFEMVVBSEVRYWUzc3lhbld0aTd4ZEhLdXI3TE90CmsyRmFFMFl0VGt0a1RscDBzSGlxazliWHkzVVpqUHBFazBWZzZCaTM2QVUzVVFCMHFpc1dKQ2o4RGVLZnhSN1EKdkgvblo0MnJSMUNsTkRhdzBXQWc5eFR0WmNCSTRydEM4UXFIbWIzQ2N6elJ2WVM3T3V2VFRaM1h4NkNPQVFjUgpjVHNKa25qSGI4MXFQNFlBNDFiQ3l1L28xWGVCUmxIM1ZXVURyWHBoWEhETm1FcFFaTVFpWVBOYUl2Q1dOQ09lCkRqSFhLazk3NnFBcFVzVHBxcFRIdmgxUGNxSFpFeFdPRWQwSkxpR3BzZW9vODN4M1k0bTBDaXRBTDhDK2xzTk4KTGxWeFNCbmZ6STJVZnpBK3lWMFVVT1pHMXhJY1QzMVNaRGRZV1VKdG9OZmVuSnA0RTNXdlZzZXA3UDhXMXZxOApHY0RVU2lxT010ajFIdHhqTmdMTldCUk01aDNaMHhyaHQ0SjFRejArZHVESGxCOWJxeU9OS240eGtLZnBrWlRBCkRUakJkZTJlCj1DdS9LCi0tLS0tRU5EIFBHUCBTSUdOQVRVUkUtLS0tLQo=",
      "format": "pgp",
      "publicKey": {
        "content": "LS0tLS1CRUdJTiBQR1AgUFVCTElDIEtFWSBCTE9DSy0tLS0tCgp4c0ROQkdSYnIrY0JEQUM5YUlXaElpc08zNjRCdnhYc093QjZwUklKWGQrbWJINHp1VXdRNHIrcmQvb3M0TG1SCkxaaVRDekVZbVMrcHhnUWQwb1haR2o5elZ3VzRYMndaVzI4K0RVRDljWnpHL1J6RkRHeExnZzc1d2dDYmp1czcKeVc0UmtQQkhYOXRsNUFuRHU0SjFKK01Ra3hyUEVHZFpFSVp2QVlKOVJFWDRDSVhHQUNjdXgyaGtaVUlTNVpxZQpvdjRwd1FReFhTNm5SOVpUOHpieWo1SkhQWmtRek02eU42c0F6aXFlUUZpK3pKdXJaTG9kdU9BbEs2Z3BjY1F1CkRnQ0JCM280S2cvSk5lZVB0VTlHYUxHSXB1ZGdxd0N2VHpSbUR0NUc5bjd0cVJaV0ZFMitxSmg5MVBFdzhyUjkKOG5NQWxZSFI1S3BJcUpwbldjOFRSME0yQ0tCOEd2UzNjS3E3NHYvQTZ5bUFPNzErbmVad3pxTENyTEsydllNeApHbjdOSUVKM0grRVFQcEplNGkrbWxoeGE5Rm1zMi9qSUdjemJMNGtSbGdqVE9wazFmOE4zaWZDbnp1YkpxQ3ZXCnZGbWMxM2I0eE9ic3BlWndjckk3Kzg2R092a2FaZk1ONWZweGt4U1lETStLSVEyZlNYcXp0ZGlHWDBVZEp3dG4KOEZhTlBnTUJvckFGMzAwQUVRRUFBYzBVYjJOcFkyd3RkR0Z5WW1Gc2JDMXphV2R1WlhMQ3dSRUVFd0VJQURzVwpJUVM1YXMyL05jWEJxNEZaYjdiVHIrR0lRNWU5eUFVQ1pGdXY1d0liQXdVTENRZ0hBZ0lpQWdZVkNna0lDd0lFCkZnSURBUUllQndJWGdBQUtDUkRUcitHSVE1ZTl5TEZ6Qy85TmJmVUIzYncyMUhHQStBTk03WHg5VVdpZVlNSzkKcWlFUUFyYVgvQm85cnl6c3ZtcnUxcDZYN2RsTmtoTEJzMWl1MFZ0eFB4SWhHdUdncHNPekV2N0Y3OFdvM0pwNQo4dVQ5cW5kelV2aXh2S2lFTTJsdzZHZWFrallCMjV4Zy9VSnpWNmxGYWhhcVZ6emhUSkovbkI5YU9jNDg1WjBpCnV1UnIxU3Nkb3VHbHdRTVZydktRbU1rMjlaTzlKcnNIUmxQR2hnY1p0K2J1bUJPdDNKZTQ0WGtkL0NtNDBxdXYKb2Mwb0FFNGN2c2JkSmE2d3EwODlQK3VPWFZNZlhpUDVBd3ZWSjYxQ2NEYWRJWk1kT29KYnc0b1k3V2dWS2NIWQpYd0tlS0NzOFpzRWs2OGlTRm1FQ3Q3THI4U2tENVcwRXY0QWV3clh4dEFXNElCYUtRaE1pSEZsQ2kvdzdLdjduCm1lM0t4Q1JEc0F4NEdld05iTXRaUjBnZHRwV21CWGVpRytROC8wZGdMUFRqRmFBWHFRQithUWNuMzFscnZCYVgKd0Jib3FtUlNtWDVwdk1uaHhiNSt0R0hsRU9QekMrZ0k0VU50cVZ4L1NIRCtSZHNNM0owd2huRjh3R09KWlRZUwo1WkxrZWR6Qjc3VEtzakNhci92Q2x5UTUvalIrck81VjlwSE93TTBFWkZ1djV3RU1BTUJkY1JYYUpCQ2V2dHV1CklnU3Nmc3hDb3NLRnJQN3JTdmN2bHlHT3pCVlBKT2JFbE5NbjVTOGNwZCtuMXcrQS9rdXFobUNHVmJjaHI2YkMKUERRbmJad2pwWUU3bGVISGtQT2tkT2JHY3VOWjZ1YVNsSktWcWg5aHpHeHZZUlhIMFhUSDNkb2NJTTdyRkdtOQpSMWdKcDhBNmtSbWg4ZVNSOWpMRWNhS2lRTUZPbEYzU3RFZms4VzFsZ0x2Z2VTVnJkc0s0Rnc3Ui9BRVlmek5xClpTZEdRYmx0WXkyekVKcG04M2QxN1BCZXN4eE5FdVNWYUlibCtqai9TLzVkL1h0OGVKdVJzbDVOeDdLSTRpenkKK1pFWkxOWkJZa3FzMTZsSnRva2oxclZOUitLWEtCN1pDenVacGxjYURJbExWMUN0YXFqVXpMazFlcVh0TXBZOQordTFocXd0em5vWnRsTTZTTmVYZnUzS3YzSFlaWUlJVGcrSDBLQVVnZko1S1F1YnNaN2dmSVpaMno0WkZHUHJPCllhWlRJQmNGMGlIbEhZWitPTVRONWpKR2VrU0NlTW0yVUZzZVBqSUtQdHhudTk0ajNDanhWbmlzZURzUkJja1oKcFRaY2VPNkZVa3pCYkltQ2FqNklZd2NmdFhBWVRFQjZXdzE0OUdFYnpONEVyV1pKaXdBUkFRQUJ3c0QyQkJnQgpDQUFnRmlFRXVXck52elhGd2F1QldXKzIwNi9oaUVPWHZjZ0ZBbVJicitjQ0d3d0FDZ2tRMDYvaGlFT1h2Y2lGCmxndi9URndrYXYzYWFFVnhvU0hDNDJzTGl0YmZGOC83YW53ZGZPZkhFdFVSejBmdk1vVEVsamdtS05jb3FQYkEKdkhJMmN1MXA3RTAvOXNZY0VTaXlPNzJVR29oWmZtaWFsTlhROE53TVcySzFnM1FZS0hKam55WWV2WnlkV3dlegpDbXovU0RpNXFBYmMzSVprTFZXRk5LOXdrenljVlhYcVB6ZnJzdDkyTXJ1ZktYSDc2eEExZlA2NXl6S21ZSW9WCnc3eHJkaGp2VzMvS0JHeU5iZHk0dmNucWdERUFCbWR5OUJxRkhKK0p4QnFIZXZKTjV6SXAySHFRK2x4YWVGYmgKQTllckJKYVMzNDU4eXVxd0FvTEJ4OURscHZYQzE2c3NXVVU3WGlJQ1pFb255aXVxQVN4QXMxZjk3SFNjVWx3UQpxWExLUURmZEFud25PeE9wcXFHS1E1M3FzN3UxdUJ0RzJrN0JQMmEwbE1EUW9Za1hoK2tPNTRVT0ZtS1lFNGdvCjE0SFdYSmc3S0Y5UjMxQ0ZrNXNGUU9yYmR1bCtoNC80VVRMSFhGTFBMVkkvTzYwZDNCNkd4cm91SlRSeWJDUHAKV0tvZjg0VGcyY1FiV2FmVko5bzlIbklwb2lPNGJCMVZrQndoM3E2TTE2L0kyWC9zNGhaVVNLODJYSWJ0TlFxRgpRZmxZCj1hbnlKCi0tLS0tRU5EIFBHUCBQVUJMSUMgS0VZIEJMT0NLLS0tLS0="
      }
    }
  }
}
```

Further explanation of the sigstore tooling and ecosystem is beyond
the scope of this document, but you can read about it at
[https://docs.sigstore.dev/](https://docs.sigstore.dev/).

Self-Hosting
------------

You may wish to self-host the ``ocicl`` registry.  This is easy, given
that the registry is just a regular OCI-compatible artifact registry.
You can use tools like
[``skopeo``](https://github.com/containers/skopeo) to copy artifacts
from the default registry, ``ghcr.io/ocicl``, into your own.  Or you
may choose to run a local registry that does pull-through caching,
like the [Zot registry](https://zotregistry.dev).

The ``ocicl`` source distribution includes a [sample shell
script](https://raw.githubusercontent.com/ocicl/ocicl/main/mirror-example.sh)
for mirroring systems from ``ghcr.io/ocicl`` into a locally-hosted OCI
registry.

Be sure to run ``setup`` with the ``--registry`` option to make this
new registry your default.

Systems
-------

Systems managed by ``ocicl`` are maintained in github, at
[https://github.com/ocicl](https://github.com/ocicl).  Each system has
its own source repo, with a ``README.org`` file containing everything
required to build and publish to the OCI registry via github actions.

The list of ocicl systems is always available at
[https://raw.githubusercontent.com/ocicl/request-system-additions-here/main/all-ocicl-systems.txt](https://raw.githubusercontent.com/ocicl/request-system-additions-here/main/all-ocicl-systems.txt).

Contributions are welcome and appreciated!  See
https://github.com/ocicl/request-system-additions-here for details.

## Project Templates

The `ocicl new` command creates a new application from a
template. Templates are a hierarchy of directories containing files
that a processed before being copied into your current working
directory.  They are processed using
[cl-template](https://github.com/alpha123/cl-template) and optional
key/value pairs that you pass into the templating engine. Templates
can be stored locally in your template directories or use the built-in
templates that come with `ocicl`.

### Usage

```bash
ocicl new APP-NAME [TEMPLATE] [KEY=VALUE]...
```

#### Arguments

| Argument | Description |
|-----------|------------|
| `APP-NAME` | Name of the directory to create for your new project. This value is also available inside templates as the `#:app-name` keyword. |
| `TEMPLATE` | *(Optional)* Template to use. Defaults to `basic`. If you have a template named `user`, it will override the `basic` default. |
| `KEY=VALUE` | *(Optional)* Additional template variables (e.g., `author=Alice`, `license=MIT`). Keys are automatically converted to keywords (`#:AUTHOR`, `#:LICENSE`, etc.). |

#### Example

```bash
# Create a basic project
ocicl new my-blog

# Create a project with a specific template and variables
ocicl new my-api web-service author="Jane Doe" license=GPL3
```

### Template Discovery

Templates are searched in the following order (first match wins):

1. **Command line option**: `--template-dir DIR` (can be used multiple times)
2. **Environment variable**: `$OCICL_TEMPLATE_PATH` (colon-separated list of directories)
3. **Configuration file**: `~/.local/share/ocicl/config/ocicl/ocicl-template-path.cfg` (one directory per line)
4. **Default location**: `~/.local/share/ocicl/templates/` (installed by `ocicl setup`)

#### Template Management Commands

- **List available templates**: `ocicl templates [list]`
- **Show template directories**: `ocicl templates dirs`

### How Templates Work

#### File Processing

**Text files** are processed as templates when they:
- End with the `.clt` extension, OR
- Contain at least one `<% ... %>` directive

These files use [cl-template](https://github.com/alpha123/cl-template) syntax for variable substitution and logic.

**Binary files** (containing NUL bytes) are copied without modification.

**Git directories** (`.git/`) are ignored during template processing.

#### File and Directory Naming

Use the `{{app-name}}` token (case-insensitive) in file or directory names to have it replaced with your `APP-NAME`.

Example: A template file named `{{app-name}}.asd` becomes `blog.asd` when you run `ocicl new blog`.

Tips and Troubleshooting
------------------------

* In the unlikely event that ``ghcr.io`` is unreachable, all packages
are also available at ``docker.io/ocicl``.  Switch registries by
running ``ocicl setup -r docker.io/ocicl``.

* You may find it convenient to tell ASDF to load from the current directory.
Do this by placing the following in your ``.sbclrc`` file:
```
(asdf:initialize-source-registry
  (list :source-registry (list :directory (uiop:getcwd)) :inherit-configuration))
```
* As an ``ocicl`` user, you may have had experience using quicklisp's
``local-projects`` mechanism, and are wondering how to do something
similar.  ASDF itself provides a simple mechanism for searching a
collection of subdirs for ``.asd`` files.  If, for instance, you had
a directory in which you cloned various lisp systems called
``/path/to/my/local-projects``, you would configure ASDF thusly:
```
(asdf:initialize-source-registry '(:source-registry :inherit-configuration (:tree #P"/path/to/my/local-projects/")))
```
* Setting ``ocicl-runtime:*verbose*`` to a stream (like ``t``, ``*standard-output*``, ``*error-output*``, etc) will output useful and
interesting log info.

* ``ocicl`` is bundled with ``asdf`` version 3.3.7.  You can delete it
(along with any fasl files) from the directory containing
``ocicl-runtime.lisp`` if you do not want this version to be loaded by
the runtime at startup.

* In addition to `ocicl.csv` with the `ocicl` systems directory, `ocicl`
  additionally supports `systems.csv` and `systems` for backward compatibility
  with earlier versions of ocicl.

Author and License
-------------------

``ocicl`` was written by [Anthony Green](https://github.com/atgreen),
and is distributed under the terms of the MIT license.

This software includes Lisp source code files written by Zachary
Beane, Mark Karpov, Ava Fox, and PMSF IT Consulting Pierre R. Mai.
Portions of the linter were borrowed from and inspired by Chris
Riesbeck's MIT licensed lisp-critic project.  See the ``ocicl`` source
files for details.
