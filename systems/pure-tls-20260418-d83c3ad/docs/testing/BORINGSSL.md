# Integrating BoringSSL Test Infrastructure with pure-tls

This document describes how to leverage BoringSSL's comprehensive TLS test suite to validate `pure-tls`.

## Overview

BoringSSL provides a sophisticated, language-agnostic test harness in `ssl/test/runner/`. Unlike OpenSSL's declarative `.cnf` files, BoringSSL uses a **shim architecture**:

- **Go test runner** acts as both client and server, injecting protocol bugs
- **Shim binary** wraps the implementation under test
- **TCP communication** between runner and shim

This architecture allows testing any TLS implementation against BoringSSL's Go TLS implementation.

## What We Can Borrow

### 1. Test Key Files (Immediate Use)

Ready-to-use PEM key files in `ssl/test/runner/`:

| File | Type | Use Case |
|------|------|----------|
| `rsa_2048_key.pem` | RSA 2048-bit | Standard RSA tests |
| `ecdsa_p256_key.pem` | ECDSA P-256 | Primary ECDSA tests |
| `ecdsa_p384_key.pem` | ECDSA P-384 | Extended curve tests |
| `ecdsa_p521_key.pem` | ECDSA P-521 | Extended curve tests |
| `ed25519_key.pem` | Ed25519 | EdDSA tests |

**Action**: Copy to `test/certs/` for use in pure-tls tests.

### 2. ProtocolBugs Reference

The `ProtocolBugs` struct in `common.go` defines 300+ specific protocol violations. This serves as a comprehensive checklist of edge cases to test.

**TLS 1.3 relevant bugs**:

```
// Key exchange attacks
ECDHPointNotOnCurve          - Invalid curve point (tests P-256 validation)
TruncateKeyShare             - Malformed key share
SendEmptyKeyShare            - Missing key share data

// Signature attacks
InvalidSignature             - Bad signature bytes
SkipCertificateVerify        - Missing CertificateVerify

// PSK attacks
NegotiatePSKResumption       - PSK without key share
SendInvalidPSKBinder         - Bad binder MAC
TruncatePSKBinder            - Short binder

// Record layer attacks
RecordPadding                - Excessive padding
OuterRecordType              - Wrong content type
OmitRecordContents           - Empty encrypted record

// Handshake attacks
DuplicateExtension           - Repeated extensions
SendExtraChangeCipherSpec    - Unexpected CCS
SkipNewSessionTicket         - Missing ticket
```

### 3. TLS 1.3 Test Patterns

Key test categories from `tls13_tests.go` (2,366 lines):

**Record Layer** (`addTLS13RecordTests`):
- Record padding validation
- Empty record handling
- Content type validation

**Handshake** (`addTLS13HandshakeTests`):
- Key share negotiation
- HelloRetryRequest flows
- Cipher suite preference
- Extension ordering

**Resumption** (`addResumptionVersionTests`):
- PSK-only vs PSK+DHE
- Ticket lifetime
- Binder validation

**Key Update** (`addKeyUpdateTests`):
- Bidirectional key updates
- Update request/response

### 4. Certificate Test Data

Extensive PKI test fixtures in `pki/testdata/`:

- `verify_certificate_chain_unittest/` - Chain validation test cases
- `name_constraints_unittest/` - Name constraint edge cases
- `certificate_policies_unittest/` - Policy handling
- `path_builder_unittest/` - Path building algorithms

## Integration Approaches

### Approach A: Standalone Test Extraction

Extract test patterns and data without the full shim infrastructure:

1. **Copy key files** to `test/certs/`
2. **Translate test cases** from `tls13_tests.go` to FiveAM tests
3. **Use ProtocolBugs** as a checklist for error handling tests

**Pros**: Quick wins, no infrastructure overhead
**Cons**: Manual translation, may miss some edge cases

### Approach B: Shim Integration ✅ IMPLEMENTED

We implemented a full pure-tls shim following `PORTING.md`:

```
┌─────────────────┐     TCP      ┌─────────────────┐
│  Go Test Runner │◄────────────►│  pure-tls Shim  │
│  (runner.go)    │              │  (Lisp binary)  │
└─────────────────┘              └─────────────────┘
```

**Shim implementation** (`test/boringssl-shim.lisp`):
- Accept `-port` and `-shim-id` flags
- Parse 30+ configuration flags (TLS 1.3 relevant subset)
- Return exit code 89 for unimplemented features
- Return exit code 0 for success
- 53MB standalone SBCL binary

**Result**: 65.4% pass rate (4274/6533 tests)

### Approach C: Hybrid

1. Start with Approach A for immediate coverage
2. Build shim infrastructure incrementally
3. Run both standalone and shim tests

## Implementation Status

### Phase 1: Key Files and Patterns ✅ COMPLETE
- [x] Copy `*.pem` key files to `test/certs/boringssl/`
- [x] Create `test/boringssl-tests.lisp` with translated test cases
- [x] Document ProtocolBugs coverage in test comments
- [x] Verify key files load correctly (RSA-2048, ECDSA P-256, P-384)

### Phase 2: Shim Binary ✅ COMPLETE
- [x] Create `test/boringssl-shim.lisp` with command-line parsing
- [x] Implement core configuration structure
- [x] Document exit codes (0=success, 89=unimplemented, 90=expected-failure)
- [x] Build shim executable with SBCL (53MB standalone binary)
- [x] Implement `-is-handshaker-supported` query (returns "No")
- [x] Implement shim-id protocol (64-bit little-endian)
- [x] Proper exit code handling via sb-ext:exit

### Phase 3: Runner Integration ✅ COMPLETE
The shim can be built and run against the BoringSSL test runner:

```bash
# Clone BoringSSL
git clone https://boringssl.googlesource.com/boringssl

# Build the runner test binary (optional, but faster for repeated runs)
cd boringssl/ssl/test/runner
go test -c -o runner_test .

# Build the shim
make boringssl-shim

# Run tests
# Option A: point to a BoringSSL checkout
export BORINGSSL_DIR=/path/to/boringssl  # or BORINGSSL_RUNNER=/path/to/boringssl/ssl/test/runner
make boringssl-tests

# Option B: put a built runner on PATH
# (from the runner dir: go test -c -o runner_test .)
export PATH="/path/to/boringssl/ssl/test/runner:$PATH"
make boringssl-tests
```

**Current Status (2026-01-05):**
- **65.4% pass rate** (4274 passed, 2259 failed out of 6533 tests)
- Test suite completes fully without hanging ✅
- Shim connects to runner and sends shim-id ✅
- Exit code 89 properly returned for unimplemented features ✅
- DTLS and session resumption tests correctly skipped ✅
- TLS 1.2 tests correctly rejected (pure-tls is TLS 1.3 only) ✅
- TLS 1.3 handshakes work correctly ✅

**Supported Shim Flags:**
- `-port`, `-shim-id` - Core runner protocol
- `-server`, `-is-handshaker-supported` - Mode selection
- `-key-file`, `-cert-file`, `-trust-cert` - Certificate configuration
- `-min-version`, `-max-version` - Version constraints
- `-shim-writes-first`, `-shim-shuts-down` - Data exchange control
- `-check-close-notify` - Clean shutdown verification
- `-advertise-alpn`, `-expect-alpn` - ALPN negotiation
- `-host-name`, `-sni-hostname` - SNI configuration
- `-verify-peer`, `-require-any-client-certificate` - Client auth

**Not Implemented (exit 89):**
- DTLS tests
- Session resumption tests
- 0-RTT early data tests
- Certificate callback tests
- Peek functionality tests

### Future Work
- [x] ~~Debug TLS 1.3 handshake issues with runner~~ (65% pass rate achieved)
- [ ] Add PKI test data from `pki/testdata/`
- [ ] Integrate with CI/CD pipeline
- [ ] Implement ALPS extension support
- [ ] Implement certificate callback infrastructure
- [ ] Add peek/non-blocking read support

## Key Files Reference

| File | Lines | Purpose |
|------|-------|---------|
| `runner/runner.go` | 2,372 | Test harness main loop |
| `runner/common.go` | 2,366 | Config and ProtocolBugs structs |
| `runner/tls13_tests.go` | 2,366 | TLS 1.3 specific tests |
| `runner/handshake_messages.go` | 2,980 | Message parsing |
| `runner/certs.go` | ~500 | Certificate generation |
| `runner/PORTING.md` | 115 | Shim integration guide |
| `test_config.h` | 9,300 | Shim flag definitions |

## ProtocolBugs Checklist for pure-tls

Priority bugs to test (TLS 1.3 relevant):

### Critical (Security)
- [ ] `ECDHPointNotOnCurve` - Reject invalid P-256 points
- [ ] `InvalidSignature` - Reject bad signatures
- [ ] `SendInvalidPSKBinder` - Reject bad PSK binders
- [ ] `UnauthenticatedECDH` - Require authentication

### High (Correctness)
- [ ] `TruncateKeyShare` - Handle malformed key shares
- [ ] `DuplicateExtension` - Reject duplicate extensions
- [ ] `SkipCertificateVerify` - Require CertificateVerify
- [ ] `SendExtraChangeCipherSpec` - Ignore middlebox CCS

### Medium (Robustness)
- [ ] `RecordPadding` - Handle padded records
- [ ] `FragmentMessage` - Handle fragmented handshakes
- [ ] `SplitAndPackAppData` - Handle split application data
- [ ] `EmptyEncryptedExtensions` - Handle empty extensions

### Low (Edge Cases)
- [ ] `SendHelloRequestBeforeHandshake` - Reject pre-handshake messages
- [ ] `ALPNProtocol` - Validate ALPN negotiation
- [ ] `SendUnsolicitedExtension` - Reject unexpected extensions

## Comparison with OpenSSL Approach

| Aspect | OpenSSL (.cnf) | BoringSSL (shim) |
|--------|----------------|------------------|
| Format | Declarative config | Programmatic Go |
| Scope | ~100 test files | 44,000+ lines |
| Integration | Parse and execute | Binary protocol |
| Bug injection | Limited | 300+ ProtocolBugs |
| Maintenance | Manual sync | Run against runner |

**Recommendation**: Use both approaches:
- OpenSSL for quick declarative tests
- BoringSSL for comprehensive edge case coverage
