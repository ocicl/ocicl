# OCICL TODO

Actionable improvements grouped by impact and area. Each item includes the key files/functions to touch and the intended behavior.

Next Up (Priority)
------------------
- [x] Enforce TLS verification by default (http.lisp `http-get`; add `:verify :required` and CA env vars; CLI `-k/--insecure` and `OCICL_INSECURE`)
- [ ] Clearer TLS error messages and troubleshooting hints
- [ ] Cache bearer tokens for registries with short TTL
- [ ] HTTP timeouts and limited retries with backoff for GETs
- [ ] Robust registry parsing via `puri` path segments
- [ ] Logging helper for consistent user/verbose output
- [ ] Align file discovery with documented exclusion policy

## High-Impact Fixes

- [x] HTTP proxy auth wiring
  - Implement in `http.lisp`:
    - `configure-drakma-proxy-from-env`: when credentials are present in proxy URI authority, compute and store proxy basic auth (user + pass).
    - `http-get`: pass `:proxy-basic-authorization` to `drakma:http-request` when applicable.
    - Call `configure-drakma-proxy-from-env` once at startup (already called in `ocicl:main`).
  - Verify with env vars `HTTPS_PROXY`, `HTTP_PROXY`, `NO_PROXY`.
  - Status: Implemented; docs added in README (Proxy Configuration).

- [ ] Enforce TLS verification by default
  - [x] In `http.lisp` `http-get`, pass `:verify :required` and plumb optional `:ca-file`/`:ca-directory` (read from `OCICL_CA_FILE`/`OCICL_CA_DIR`).
  - [x] CLI flag `-k`/`--insecure` and env `OCICL_INSECURE` to skip verification.
  - [ ] Ensure clear errors when trust roots are missing; document troubleshooting.
- [x] Robust layer selection in OCI manifest
  - Simplified: pick the first layer digest. If the tag is an index, pick the first child manifest and its first layer digest.

- [x] Binary diff correctness and performance
  - Compare only the bytes read using `mismatch :end1/:end2` and early exit on per-block size mismatch.
- [ ] Remove duplicate defvar
  - `ocicl.lisp` defines `*ocicl-systems*` twice; remove the second to avoid confusion/redefinition warnings.

- [ ] Robust layer selection in OCI manifest
  - In `get-blob` (ocicl.lisp), select the layer by `:mediaType` (`application/vnd.oci.image.layer.v1.tar+gzip`, etc.) instead of `(cadr (assoc :layers …))`.

- [ ] Binary diff correctness and performance
  - In `binary-files-differ-p`, compare only the bytes read using `mismatch :end1 bytes-read1 :end2 bytes-read2` instead of comparing whole fixed buffers with `equalp`.

## Robustness & Resilience

- [ ] Cache bearer tokens
  - Add a small cache keyed by `(registry, repository)` with TTL; reuse tokens in `get-bearer-token`/`system-version-list`/`get-manifest`.

- [ ] HTTP timeouts and retries
  - Plumb `:connection-timeout`/read/write timeouts (use Drakma capabilities) from env vars (e.g., `OCICL_HTTP_TIMEOUT`, `OCICL_HTTP_READ_TIMEOUT`, `OCICL_HTTP_WRITE_TIMEOUT`).
  - Implement limited retries with backoff for transient errors (429/5xx) in `http-get` call sites where safe (idempotent GETs).

- [x] Atomic CSV writes
  - `write-systems-csv` writes to a temp file in the same directory, then renames into place with `uiop:rename-file-overwriting-target`.

- [ ] Robust registry parsing
  - Replace `get-up-to-first-slash`/`get-repository-name` with `puri:parse-uri` + pathname segment handling to support multi-segment repositories.

- [ ] Specific error classification
  - Replace broad `(error (e) …)` with conditions that carry reason codes (auth, not-found, timeout). Emit concise user-facing messages and detailed diagnostics under `*verbose*`.

## Architecture & Code Quality

- [ ] Consolidate duplicate utilities
  - Create a `ocicl.util` package shared by `ocicl` and `runtime/ocicl-runtime.lisp` for: `split-on-delimiter`, `mangle`/`unmangle`, random base36, temp dir helpers, CSV parsing.
  - Ensure the runtime’s embedded strings are generated from shared sources.

- [ ] Align file discovery with documented policy
  - Update `find-asd-files` to exclude:
    - Hidden dirs (prefix `.`) and common VCS/build dirs: `.git`, `.svn`, `_build`, `_darcs`, `node_modules`, etc.
    - Keep the current exclusion of deeper `ocicl`/`systems` subtrees.
  - Make exclusion set configurable (env or config file).

- [ ] Input validation consistency
  - Validate/escape each path component for registry/repository/system/tag before interpolation into URLs.

- [ ] JSON package consistency
  - Standardize on `json:` (or `cl-json:`) alias throughout code for clarity; currently both are used.

- [ ] Logging facility
  - Wrap user-visible logging with a helper that normalizes color/no-color, verbosity levels, and output stream selection.

## Portability & Compatibility

- [ ] ASDF/system isolation for non-SBCL runtimes
  - Mirror the SBCL immutable system registration for other implementations where relevant (e.g., ABCL, CCL), guarding with features.

- [ ] CA bundle detection on Windows/macOS
  - Provide sane defaults or detection for CA bundle paths when TLS verification is required; document fallback and override.

## UX & CLI

- [ ] Color options polish
  - Add `--no-color` alias; ensure `NO_COLOR` env always disables color unless `--color=always` explicitly set.

- [ ] Improve user guidance
  - Enhance “already installed” and “not found” messages with hints (e.g., `--force`, `ocicl list <system>`).

- [ ] Version sorting in `list`
  - Offer semantic version sorting for semver-like tags; preserve reverse chronological for date/hash tags.

## Testing & CI

- [ ] Unit tests
  - `find-asd-files` exclusion matrix across platforms.
  - Proxy parsing and Drakma argument wiring (mock Drakma).
  - Token caching with expiry handling.
  - CSV read/write roundtrip and atomic replace.
  - `binary-files-differ-p` edge cases.

- [ ] Integration tests
  - Mock OCI registry to exercise manifest fetch, layer selection, and auth paths; dry-run downloads.

- [ ] CI coverage
  - Add TLS verification smoke test on Windows/macOS (against a known HTTPS endpoint) behind a fast path.

## Documentation

- [ ] Synchronize version numbers
  - `CLAUDE.md` advertises v2.6.5 while `ocicl.asd` is v2.6.6; update docs.

- [ ] Update file discovery docs
  - Reflect the actual directory exclusion rules implemented.

- [x] Proxy configuration guide
  - Documented proxy env vars, formats, precedence, `NO_PROXY`, and Basic auth in README.
- [x] TLS verification guide
  - Documented CA overrides, default verification behavior, and how to disable verification for debugging only (README).

## Example Patch Sketches (for guidance)

- Proxy/TLS in `http.lisp`:
  - Add parameters to `http-get` to pass `:verify`, `:ca-file`, `:ca-directory`, and `:proxy-basic-authorization` to `drakma:http-request`.
  - Auto-derive `proxy-basic-authorization` from env when available.



- Safer binary comparison:
  - Use `mismatch` limited by bytes read to avoid comparing uninitialized buffer tails.
