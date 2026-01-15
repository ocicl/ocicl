# Test Suite Status (2026-01-05)

## TLS-Anvil RFC Compliance Testing

### Setup
TLS-Anvil's scanner sends TLS 1.2 probes that timeout against TLS 1.3-only servers. We modified TLS-Anvil (in `~/git/TLS-Anvil`) to add a `-tls13Only` flag that bypasses the scanner.

**Modified files:**
- `TLS-Test-Framework/src/main/java/de/rub/nds/tlstest/framework/config/TlsAnvilConfig.java` - Added `-tls13Only` CLI parameter
- `TLS-Test-Framework/src/main/java/de/rub/nds/tlstest/framework/execution/TestPreparator.java` - Skip scanner, use static TLS 1.3 config

**Usage:**
```bash
cd ~/git/TLS-Anvil
mvn exec:java -pl TLS-Testsuite \
  -Dexec.args="-tls13Only -parallelHandshakes 1 server -connect localhost:4433"
```

### Initial Test Results
With `-tls13Only` flag, tests run immediately without scanner timeout:
- TLS 1.2 tests: Automatically skipped ("ProtocolVersion not supported by target")
- TLS 1.3 tests: Running against pure-tls

**Sample results from initial run:**
- `KeyUpdate.sendUnknownRequestMode`: 5/9 passed, 4/9 failed
  - Failures: "Expected fatal alert but received NEW_SESSION_TICKET" (pure-tls doesn't reject unknown KeyUpdate request modes with alert)

---

## Compliance Fixes Applied (2026-01-05)

The following RFC compliance issues were fixed:

### Fixed Issues

1. **KeyUpdate validation** (RFC 8446 §4.6.3) ✅
   - Now sends `illegal_parameter` alert for unknown `request_update` values
   - File: `src/streams.lisp`

2. **Warning alert rejection** (RFC 8446 §6) ✅
   - Now rejects warning-level alerts except `close_notify` and `user_canceled`
   - File: `src/record/record-layer.lisp`

3. **Compression method validation** (RFC 8446 §4.1.2) ✅
   - Server now validates `legacy_compression_methods` is exactly `[0]`
   - Sends `illegal_parameter` alert for non-compliant values
   - File: `src/handshake/server.lisp`

4. **Record size limit** (RFC 8446 §5.4) ✅
   - Fixed max encrypted record size from 16656 to 16640 bytes (2^14 + 256)
   - File: `src/constants.lisp`

5. **GREASE in NewSessionTicket** (RFC 8701 §4.1) ✅
   - Server now includes GREASE extension in NewSessionTicket
   - `GREASE-Server-TLS13` test now passes
   - File: `src/handshake/server.lisp`

6. **GREASE-Client-TLS13** (RFC 8701) ✅
   - Client GREASE test now passes
   - Client already sent GREASE values; test was passing

7. **MaxSendFragment support** ✅
   - Added configurable max send fragment size to record layer
   - `:max-send-fragment` parameter on `make-tls-client-stream` and `make-tls-server-stream`
   - Handshake and application data automatically fragment to respect limit
   - Files: `src/record/record-layer.lisp`, `src/streams.lisp`, `test/boringssl-shim.lisp`

8. **BadECDSA-TLS13** ✅
   - TLS 1.3 variants of BadECDSA tests pass (return `:BAD_SIGNATURE:`)
   - TLS 1.2 variants fail as expected (TLS 1.2 not supported)

9. **TooManyKeyUpdates** ✅
   - Added key update counter to prevent DoS via excessive rekeying
   - Limit: 32 KeyUpdate messages before connection termination
   - File: `src/streams.lisp`, `src/constants.lisp`

10. **SendEmptyRecords** ✅
    - Added empty record counter to prevent DoS via empty record flooding
    - Limit: 32 consecutive empty records before connection termination
    - File: `src/streams.lisp`, `src/constants.lisp`

### Remaining Gaps

1. **Bad record MAC**: Error mapping may differ from expected
2. **Bad Finished**: Verification failures may not produce expected alert code
3. **Certificate selection**: Issuer filtering and chain size handling

---

# BoringSSL Test Suite Status (2026-01-05)

## Latest Run
- **Pass rate: 66.0% (4311 passed, 2222 failed out of 6533 tests)**
- Command: `go test -shim-path=/home/green/git/pure-tls/pure-tls-shim -allow-unimplemented`
- Shim build: `make boringssl-shim`
- TLS 1.2 handling: shim skips any test whose version range permits < TLS 1.3 (returns exit 89 = unimplemented)
- Test suite completes fully without hanging

## Recent Fixes (2026-01-05)

### Alert Handling
- ✅ `SendBogusAlertType` - Detect invalid alert levels (not 1 or 2)
- ✅ `DoubleAlert` - Reject oversized alert records (> 2 bytes)
- ✅ `FragmentAlert` - Reject short/incomplete alert records
- ✅ `Alert` - Added `:TLSV1_ALERT_RECORD_OVERFLOW:` error code

### Record Layer
- ✅ `SendInvalidRecordType` - Added `:UNEXPECTED_RECORD:` error code
- ✅ `LargePlaintext-TLS13-Padded-*` - Fixed inner plaintext size validation
- ✅ SSLv2 ClientHello - Content type validation to reject invalid records

### Shim Improvements
- ✅ Fixed test suite hang at 6532/6533 (added `-shim-shuts-down` flag support)
- ✅ Added `-check-close-notify` flag support for bidirectional shutdown

## Remaining Failure Categories

### Not Implemented (Expected)
- TLS 1.2 tests (~35% of suite) - pure-tls is TLS 1.3 only
- ALPS (Application-Level Protocol Settings) - draft extension
- Certificate callbacks - FailCertCallback-* tests
- Peek functionality - Peek-* tests
- 0-RTT early data tests

### Recently Fixed
- ✅ `GREASE-Client-TLS13` - Client GREASE passes (was already working)
- ✅ `SendUserCanceledAlerts-TooMany-TLS13` - Passes (user_canceled handling correct)
- ✅ `MaxSendFragment-TLS13` - Now fragments records according to limit
- ✅ `BadECDSA-*-TLS13` - All TLS 1.3 variants pass (return `:BAD_SIGNATURE:`)
- ✅ `TooManyKeyUpdates` - Added key update counter with 32 limit
- ✅ `SendEmptyRecords` / `SendEmptyRecords-Async` - Added empty record counter with 32 limit
- ✅ `WrongMessageType-TLS13-*` - 9/10 TLS 1.3 tests now pass (added `:UNEXPECTED_MESSAGE:` error codes and alerts)
  - Client installs handshake write keys immediately after ServerHello for encrypted alerts
  - Files: `src/handshake/client.lisp`, `src/handshake/server.lisp`
- ✅ `TLS13-Client-ClientAuth-*` - Fixed client certificate handling when certificate is already an X509 object
  - File: `src/streams.lisp`
- ✅ `TLS13-Server-ClientAuth-*` - Server-side client auth tests pass
  - Fixed: skip chain verification when no trust store provided (still requires certificate)
  - Files: `src/handshake/server.lisp`, `src/streams.lisp`
- ✅ `ClientAuth-NoFallback-TLS13` - Added `:DECODE_ERROR:` for CertificateRequest parse errors
  - File: `src/handshake/client.lisp`

### Needs Investigation
- Certificate selection behaviors:
  - Several `CertificateSelection-*` cases (issuer filters, chain size)
- `WrongMessageType-TLS13-ClientCertificate-TLS` - Server-side test fails with "broken pipe"
  - TCP timing issue: connection closes before peer receives alert

## Notes
- TLS 1.2 is not required by RFC 8446. It is only a SHOULD if earlier versions are supported.
- The 65% pass rate reflects that ~35% of tests are TLS 1.2 specific
- Most remaining TLS 1.3 failures are in unimplemented features (ALPS, callbacks, etc.)
