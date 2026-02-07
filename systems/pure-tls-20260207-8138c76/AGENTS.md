# Repository Guidelines

## Project Structure & Module Organization
- `src/` contains the TLS implementation, split into focused areas such as `crypto/`, `handshake/`, `record/`, and `x509/`. Core entry points and shared utilities live alongside (`package.lisp`, `streams.lisp`, `utils.lisp`).
- `compat/` provides the cl+ssl compatibility layer (`api.lisp`, `package.lisp`).
- `test/` hosts FiveAM test suites, helpers, and fixtures. Certificates live under `test/certs/`, and OpenSSL/BoringSSL fixtures live under `test/ssl-tests/`.
- `docs/` includes release notes and testing notes (`docs/testing/`).

## Build, Test, and Development Commands
- `make test` (default) runs the full test matrix.
- `make unit-tests` runs crypto/record/handshake/certificate tests only.
- `make network-tests` runs integration tests that require internet access.
- `make verify` runs certificate verification checks (`test-verify.lisp`).
- `make connect` runs a live TLS connection test against example.com.
- `make boringssl-tests` builds the shim and runs the BoringSSL TLS 1.3 suite.
- `make load` loads `:pure-tls` and `:pure-tls/test` to verify compilation.
- `make clean` removes compiled artifacts and the shim binary.
- `ocicl lint` runs periodic lint checks; use it regularly when changing dependencies or CI-facing code.

## Coding Style & Naming Conventions
- Follow existing Common Lisp formatting; use spaces (no tabs) and the project’s indentation style (2-space indent is typical in the current sources).
- Use lowercase, hyphenated names for functions and variables (e.g., `make-tls-client-stream`).
- Keep package forms and exports tidy in `src/package.lisp` and `test/package.lisp`.

## Testing Guidelines
- Tests use FiveAM (see `test/package.lisp` and `test/runner.lisp`).
- Name suites `*-tests` and place them in `test/*-tests.lisp`.
- Prefer unit tests for logic; keep network tests isolated and runnable via `make network-tests`.
- Dependency management for tests uses ocicl; ensure dependencies are installed/updated via ocicl before running test suites.

## BoringSSL Test Integration
The project includes a shim (`pure-tls-shim`) that allows running the BoringSSL TLS test suite against pure-tls.

### Building the Shim
```bash
make boringssl-shim  # Builds pure-tls-shim executable
```

### Running BoringSSL Tests
Requires either `BORINGSSL_DIR`/`BORINGSSL_RUNNER` pointing at a BoringSSL checkout, or `runner_test` on `PATH`:
```bash
# Run full test suite
make boringssl-tests

# Run specific tests using glob patterns (from the runner dir or with runner_test on PATH)
go test -v -shim-path=/path/to/pure-tls-shim -allow-unimplemented -test "*Basic*TLS13*"
go test -v -shim-path=/path/to/pure-tls-shim -allow-unimplemented -test "*ClientAuth*TLS13*"

# Capture TLS keys for Wireshark debugging
SSLKEYLOGFILE=/tmp/keylog.txt go test -v -shim-path=/path/to/pure-tls-shim ...
```

### Interpreting Test Output
Progress is shown as `failed/unimplemented/done/started/total`:
- `0/22/22/22/6533` means 0 failures, 22 skipped (unimplemented), 22 completed

### Shim Exit Codes
- `0` - Test passed
- `1` - Test failed
- `89` - Feature not implemented (test skipped)
- `90` - Expected failure

### Debugging Tips
- The shim writes debug output to stderr; look in test failure output
- Add debug statements to `src/crypto/aead.lisp`, `src/streams.lisp`, etc.
- Use `SSLKEYLOGFILE` to capture keys and verify with Wireshark
- Compare transcript hashes between client and server for key derivation issues

## TLS-Anvil Test Integration (Experimental)
TLS-Anvil is an RFC compliance test suite for TLS implementations. It runs via podman/docker.

**Current Status**: TLS-Anvil's TLS-Scanner uses a probing strategy that doesn't work well with TLS 1.3-only servers:
1. Scanner sends TLS 1.2 probes (without `supported_versions` extension) - pure-tls responds with `protocol_version` alert (code 70)
2. Scanner sends various cipher suite probes to enumerate server support - these also fail for TLS 1.2
3. Scanner may eventually send TLS 1.3 probes, but times out before completing feature extraction

The scanner needs to successfully complete handshakes to determine the server's capabilities. Since pure-tls only supports TLS 1.3, most probes fail immediately. Potential workarounds:
- Pre-configure a scan profile that skips TLS 1.2 probing
- Use TLS-Anvil's manual configuration to specify TLS 1.3-only mode
- Wait for TLS-Anvil updates to better handle TLS 1.3-only servers

### Running TLS-Anvil Tests
```bash
# Test pure-tls as a server (TLS-Anvil connects to us)
./test/tls-anvil/run-anvil-tests.sh server

# Use higher test strength for more thorough testing
STRENGTH=2 ./test/tls-anvil/run-anvil-tests.sh server
```

### Viewing Results
Results are saved to `test/tls-anvil/results/<timestamp>/`. Upload `results.zip` to the Anvil Web UI to view detailed test reports.

### Test Components
- `test/tls-anvil/anvil-server.lisp` - Simple TLS server for Anvil testing
- `test/tls-anvil/anvil-client.lisp` - Simple TLS client for Anvil testing
- `test/tls-anvil/run-anvil-tests.sh` - Main test runner script

## Commit & Pull Request Guidelines
- Commit subjects are short, imperative, and sentence case (e.g., "Fix ALPN negotiation mismatch handling").
- Include a clear description of behavior changes and test coverage in PRs.
- Link relevant issues and call out any required environment (e.g., internet access for network tests or BoringSSL availability).
