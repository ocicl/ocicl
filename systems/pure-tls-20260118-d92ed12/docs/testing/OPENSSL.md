# Adapting OpenSSL Tests for pure-tls

This document outlines the plan and process for adapting the comprehensive TLS test suite from the OpenSSL project to validate and improve the robustness of `pure-tls`.

## Goal

The primary goal is to enhance the `pure-tls` test suite by leveraging the extensive, configuration-driven tests from OpenSSL, particularly those in `openssl/test/ssl-tests`. This will provide a higher degree of confidence in our implementation's correctness and interoperability.

## Test Categories

Each adapted test should be categorized as:

- **PASS** - Test runs and must pass (RFC-required behavior)
- **SKIP** - Test uses unsupported features (with documented reason)
- **XFAIL** - Known failure due to intentional implementation difference
- **INTEROP** - Tests OpenSSL-specific quirks, not RFC conformance

This categorization ensures we distinguish between RFC compliance failures and intentional deviations from OpenSSL behavior.

## Methodology

The adaptation process will follow these steps:

1.  **Test Configuration Parsing:**
    A Lisp-based parser will be developed using the `iparse` library to read the OpenSSL `.cnf` test files. This parser will extract the parameters for each test case, including:
    - Protocol versions (e.g., TLSv1.3)
    - Ciphersuites and curves
    - Required certificates and keys
    - Client/Server verification modes
    - Expected outcomes (`Success`, `ServerFail`, `ClientFail`, specific alert types).

2.  **Test Execution Framework:**
    A new test runner will be created in `test/openssl-tests.lisp`. This runner will:
    - Iterate through the parsed test definitions.
    - Programmatically configure a `pure-tls` server and client based on each test's parameters.
    - Execute the TLS handshake.
    - Assert that the actual outcome matches the `ExpectedResult` from the `.cnf` file.
    - Validate expected alerts match RFC 8446 requirements, not just OpenSSL defaults.

3.  **Certificate Management:**
    The necessary test certificates and private keys will be copied from the OpenSSL source tree (`openssl/test/certs`) into the `pure-tls/test/certs` directory. This will ensure that the `pure-tls` test suite remains self-contained.

## Action Plan

- [x] **Step 1: Scaffolding:** Create `test/openssl-tests.lisp` and add it to the `pure-tls/test` ASDF system definition. Add `iparse` as a test dependency.
- [x] **Step 2: Parser Implementation:** Implement the `.cnf` file parser using `iparse`. Target a minimal subset that handles the structure of `01-simple.cnf`.
- [x] **Step 3: Certificate Integration:** Identify and copy the core certificates required for initial tests into `pure-tls/test/certs`.
- [x] **Step 4: Initial Test Case:** Start with `ssl-tests/01-simple.cnf` - implement and pass a single, fundamental test case (e.g., a successful TLS 1.3 handshake) to validate the end-to-end framework.
- [x] **Step 5: Expansion:** Incrementally add more test cases, focusing on:
    - TLS 1.3 Client and Server Authentication scenarios.
    - Cipher suite and curve negotiation.
    - Common and edge-case failure scenarios.
    - Alert code validation (ensure alerts match RFC 8446 requirements).

## Implementation Status (2026-01-05)

The OpenSSL test framework is **fully implemented** in `test/openssl-tests.lisp`:

### Parser (Complete)
- INI-style configuration parser using `iparse`
- Handles OpenSSL's nested section structure
- Resolves `${ENV::TEST_CERTS_DIR}` path templates

### Test Configuration (Complete)
- `openssl-test` structure captures all test parameters
- Automatic categorization: `:pass`, `:skip`, `:xfail`, `:interop`
- Skip reasons documented for unsupported features

### Test Execution (Complete)
- Live TLS client/server execution using pure-tls
- Proper result comparison (Success/ClientFail/ServerFail)
- FiveAM test suite integration

### Test Suites Integrated
| File | Tests | Description |
|------|-------|-------------|
| `01-simple.cnf` | 4 | Basic TLS 1.3 handshakes |
| `21-key-update.cnf` | 4 | KeyUpdate message handling |
| `09-alpn.cnf` | varies | ALPN negotiation |
| `14-curves.cnf` | varies | Elliptic curve selection |
| `13-fragmentation.cnf` | varies | Record fragmentation |
| `24-padding.cnf` | varies | TLS 1.3 record padding |
| `22-compression.cnf` | varies | Compression disabled tests |
| `05-sni.cnf` | varies | Server Name Indication |
| `03-custom_verify.cnf` | varies | Custom verification |
| `26-tls13_client_auth.cnf` | 14 | Client authentication |
| `12-ct.cnf` | varies | Certificate Transparency |
| `15-certstatus.cnf` | varies | OCSP stapling |
| `28-seclevel.cnf` | varies | Security levels |

### Supported Features
- Protocol version filtering (TLS 1.3 only)
- Certificate/key configuration
- ALPN protocol negotiation
- SNI callbacks (RejectMismatch, IgnoreMismatch)
- Verification modes (None, Peer, Require)
- Curve selection (X25519, P-256)
- Client certificates (mTLS) - Request and Require modes

### Automatically Skipped
- Session resumption tests (HandshakeMode=Resume)
- Server2 context switching (SNI virtual hosting)
- Custom verification callbacks
- Certificate Transparency (CT) validation
- OCSP stapling (CertStatus)
- Security levels (@SECLEVEL)
- Non-TLS-1.3 protocol versions
- Post-handshake client authentication (RequestPostHandshake/RequirePostHandshake)

Run tests with:
```bash
sbcl --eval '(asdf:test-system :pure-tls)' --quit
# Or specifically:
sbcl --eval '(asdf:load-system :pure-tls/test)' \
     --eval '(fiveam:run! '\''pure-tls/test::openssl-tests)' \
     --quit
```

## Risks and Constraints

- **Config surface area:** OpenSSL `.cnf` files include many options that are OpenSSL-specific or not implemented in `pure-tls`. Without strict scoping, the parser will become a time sink.
- **Test semantics mismatch:** Some tests assert OpenSSL behaviors that are not required by RFC 8446/5280. These should be tagged as `SKIP` or `UNSUPPORTED` rather than failing.
- **Certificate quirks:** OpenSSL’s test certs include intentionally non-standard or borderline cases. These are useful, but the expected outcome must be reviewed against RFC requirements, not OpenSSL defaults.
- **Interop vs. conformance:** Some tests are about interop quirks rather than strict conformance. These should be categorized separately to avoid conflating failures.

## Minimal Parser Subset (Phase 1)

Target only the fields needed to run a small TLS 1.3 core subset:

- Protocol version selection (TLS 1.3 only)
- Cipher suites
- Supported groups/curves
- Client/Server certificates and keys
- Verification modes (verify peer/required/none)
- ExpectedResult (Success/ClientFail/ServerFail) and optional alert

Anything else should default to `SKIP` with a clear reason until explicitly supported.

## Future Considerations

Areas to expand for comprehensive testing:

- **Session resumption / PSK** - OpenSSL tests for ticket-based resumption; our PSK support needs testing (currently auto-skipped)
- **0-RTT early data** - Currently unsupported in pure-tls (auto-skipped)
- **Post-handshake authentication** - Currently unsupported in pure-tls (auto-skipped)
- **Server2 context switching** - SNI-based virtual hosting with multiple certificates

### Implemented
- ✅ **Protocol version filtering** - Non-TLS-1.3 tests auto-skipped
- ✅ **Alert validation** - Tests validate RFC-required alert behavior
- ✅ **Key update** - `21-key-update.cnf` tests pass
- ✅ **mTLS (Mutual TLS)** - Client certificate support with Request/Require modes
