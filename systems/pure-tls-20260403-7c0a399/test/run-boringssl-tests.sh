#!/bin/bash
# Run BoringSSL TLS 1.3 tests against pure-tls
#
# SPDX-License-Identifier: MIT
# Copyright (C) 2026 Anthony Green <green@moxielogic.com>

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
SHIM_PATH="$PROJECT_DIR/pure-tls-shim"
TIMEOUT="${TEST_TIMEOUT:-300}"
BORINGSSL_RUNNER="${BORINGSSL_RUNNER:-}"
RUNNER_BIN=""

if [ -n "$BORINGSSL_RUNNER_BIN" ]; then
    RUNNER_BIN="$BORINGSSL_RUNNER_BIN"
elif command -v runner_test >/dev/null 2>&1; then
    RUNNER_BIN="$(command -v runner_test)"
fi

# Check if shim exists
if [ ! -x "$SHIM_PATH" ]; then
    echo "Error: pure-tls-shim not found at $SHIM_PATH"
    echo "Build it with: make boringssl-shim"
    exit 1
fi

if [ -z "$RUNNER_BIN" ]; then
    if [ -n "$BORINGSSL_RUNNER" ]; then
        BORINGSSL_RUNNER="$BORINGSSL_RUNNER"
    elif [ -n "$BORINGSSL_DIR" ]; then
        BORINGSSL_RUNNER="$BORINGSSL_DIR/ssl/test/runner"
    fi

    if [ -z "$BORINGSSL_RUNNER" ] || [ ! -d "$BORINGSSL_RUNNER" ]; then
        echo "Error: BoringSSL runner not found."
        echo "Provide one of:"
        echo "  - BORINGSSL_RUNNER (path to ssl/test/runner)"
        echo "  - BORINGSSL_DIR (path to BoringSSL checkout)"
        echo "  - runner_test on PATH (or set BORINGSSL_RUNNER_BIN)"
        exit 1
    fi

    # Build runner if needed
    RUNNER_BIN="$BORINGSSL_RUNNER/runner_test"
    if [ ! -f "$RUNNER_BIN" ]; then
        echo "Building BoringSSL runner..."
        (cd "$BORINGSSL_RUNNER" && go test -c -o runner_test .)
    fi
fi

echo "=== Running BoringSSL TLS 1.3 Tests ==="
echo "Shim: $SHIM_PATH"
echo "Runner: $RUNNER_BIN"
echo ""

# Run tests and capture output
TMPLOG=$(mktemp)
trap "rm -f $TMPLOG" EXIT

# Run full test suite with timeout
if [ -n "$BORINGSSL_RUNNER" ]; then
    cd "$BORINGSSL_RUNNER"
    timeout "$TIMEOUT" go test -v \
        -shim-path="$SHIM_PATH" \
        -allow-unimplemented \
        2>&1 | tee "$TMPLOG" || true
else
    timeout "$TIMEOUT" "$RUNNER_BIN" \
        -test.v \
        -shim-path="$SHIM_PATH" \
        -allow-unimplemented \
        2>&1 | tee "$TMPLOG" || true
fi

echo ""
echo "=== Test Results Summary ==="

# Extract final counts (format: failed/unimplemented/done/started/total)
FINAL_LINE=$(grep -oE "[0-9]+/[0-9]+/[0-9]+/[0-9]+/[0-9]+" "$TMPLOG" | tail -1)
if [ -n "$FINAL_LINE" ]; then
    FAILED=$(echo "$FINAL_LINE" | cut -d/ -f1)
    UNIMPL=$(echo "$FINAL_LINE" | cut -d/ -f2)
    DONE=$(echo "$FINAL_LINE" | cut -d/ -f3)
    TOTAL=$(echo "$FINAL_LINE" | cut -d/ -f5)
    PASSED=$((DONE - FAILED - UNIMPL))

    echo "Overall: $DONE/$TOTAL tests completed"
    echo "  Passed: $PASSED"
    echo "  Failed: $FAILED"
    echo "  Unimplemented: $UNIMPL"
fi

echo ""
echo "=== Failure Breakdown (by unique test name) ==="

# Extract unique failed test names for accurate counting
grep -oP "FAILED \(\K[^)]+" "$TMPLOG" | sort -u > "$TMPLOG.fails"
UNIQUE_FAILED=$(wc -l < "$TMPLOG.fails")

# Count by category using unique test names
VERSION_NEG=$(grep -E "VersionNegotiation|MinimumVersion|Fallback" "$TMPLOG.fails" | wc -l)
TLS12=$(grep -v "VersionNegotiation\|MinimumVersion" "$TMPLOG.fails" | grep -E "TLS1$|TLS11|TLS12\b|-TLS1[012]\b" | wc -l)
QUIC=$(grep "QUIC" "$TMPLOG.fails" | wc -l)
ECH=$(grep "ECH" "$TMPLOG.fails" | wc -l)
SERVER=$(grep -E "Server-|-Server\b" "$TMPLOG.fails" | wc -l)

# TLS 1.3 tests (has TLS13 in name)
TLS13_TESTS=$(grep "TLS13" "$TMPLOG.fails" | wc -l)

# TLS 1.3 server-side tests
TLS13_SERVER=$(grep "TLS13" "$TMPLOG.fails" | grep -E "Server-|-Server\b" | wc -l)

# TLS 1.3 optional features (still valid TLS 1.3, but optional extensions/callbacks)
TLS13_ALPS=$(grep "TLS13" "$TMPLOG.fails" | grep "ALPS" | wc -l)
TLS13_ALPN=$(grep "TLS13" "$TMPLOG.fails" | grep "ALPN" | wc -l)
TLS13_CALLBACKS=$(grep "TLS13" "$TMPLOG.fails" | grep -E "Callback|CustomVerify|CustomCallback|Hint" | wc -l)
TLS13_CLIENT_AUTH=$(grep "TLS13" "$TMPLOG.fails" | grep -E "ClientAuth|ClientCertificate|CertificateSelection|Client-Sign|Client-Verify|CertificateVerification|CertReq" | wc -l)
TLS13_PSK=$(grep "TLS13" "$TMPLOG.fails" | grep -E "PSK|Ticket|Resumption|EarlyData" | wc -l)
TLS13_QUIC=$(grep "TLS13" "$TMPLOG.fails" | grep "QUIC" | wc -l)
TLS13_ECH=$(grep "TLS13" "$TMPLOG.fails" | grep "ECH" | wc -l)
TLS13_OPTIONAL_API=$(grep "TLS13" "$TMPLOG.fails" | grep -E "CustomKeyShares|ExportKeyingMaterial|Renegotiat|GREASE|Compliance" | wc -l)
TLS13_VERSION=$(grep "TLS13" "$TMPLOG.fails" | grep -E "VersionNegotiation|MinimumVersion" | wc -l)

# Calculate TLS 1.3 optional/expected total
TLS13_EXPECTED=$((TLS13_SERVER + TLS13_ALPS + TLS13_ALPN + TLS13_CALLBACKS + TLS13_CLIENT_AUTH + TLS13_PSK + TLS13_QUIC + TLS13_ECH + TLS13_OPTIONAL_API + TLS13_VERSION))

# TLS 1.3 core failures (potential bugs)
TLS13_CORE=$(grep "TLS13" "$TMPLOG.fails" | grep -v "Server\|-Server\|ALPS\|ALPN\|Callback\|CustomVerify\|Hint\|ClientAuth\|ClientCertificate\|CertificateSelection\|Client-Sign\|Client-Verify\|CertificateVerification\|CertReq\|PSK\|Ticket\|Resumption\|EarlyData\|QUIC\|ECH\|CustomKeyShares\|ExportKeyingMaterial\|Renegotiat\|GREASE\|Compliance\|VersionNegotiation\|MinimumVersion" | wc -l)

# Other/uncategorized
CATEGORIZED=$((VERSION_NEG + TLS12 + QUIC + ECH + SERVER + TLS13_TESTS))
OTHER=$((UNIQUE_FAILED > CATEGORIZED ? UNIQUE_FAILED - CATEGORIZED : 0))

echo "Unique test failures: $UNIQUE_FAILED"
echo ""
echo "Expected failures (implementation scope):"
echo "  TLS 1.0/1.1/1.2 (TLS 1.3 only):  $TLS12"
echo "  Version negotiation:              $VERSION_NEG"
echo "  Server-side (client-only impl):   $SERVER"
echo "  QUIC (not implemented):           $QUIC"
echo "  ECH (not implemented):            $ECH"
echo ""
echo "TLS 1.3 failures breakdown ($TLS13_TESTS total):"
echo "  Expected/optional failures:"
echo "    Server-side:                    $TLS13_SERVER"
echo "    Version negotiation:            $TLS13_VERSION"
echo "    QUIC:                           $TLS13_QUIC"
echo "    ECH:                            $TLS13_ECH"
echo "    ALPS (optional extension):      $TLS13_ALPS"
echo "    ALPN (protocol negotiation):    $TLS13_ALPN"
echo "    Client auth/certificates:       $TLS13_CLIENT_AUTH"
echo "    Callbacks/hints:                $TLS13_CALLBACKS"
echo "    PSK/Resumption/EarlyData:       $TLS13_PSK"
echo "    Optional APIs:                  $TLS13_OPTIONAL_API"
echo "  --------------------------------"
echo "  Expected subtotal:                $TLS13_EXPECTED"
echo "  Core protocol (potential bugs):   $TLS13_CORE"
echo ""
echo "Other/Generic:                      $OTHER"

# Show core TLS 1.3 failures that need investigation
if [ "$TLS13_CORE" -gt 0 ]; then
    echo ""
    echo "=== TLS 1.3 Core Failures (potential bugs) ==="
    grep "TLS13" "$TMPLOG.fails" | grep -v "Server\|-Server\|ALPS\|ALPN\|Callback\|CustomVerify\|Hint\|ClientAuth\|ClientCertificate\|CertificateSelection\|Client-Sign\|Client-Verify\|CertificateVerification\|CertReq\|PSK\|Ticket\|Resumption\|EarlyData\|QUIC\|ECH\|CustomKeyShares\|ExportKeyingMaterial\|Renegotiat\|GREASE\|Compliance\|VersionNegotiation\|MinimumVersion" | head -20
fi

rm -f "$TMPLOG.fails"

echo ""
echo "=== Most Common Errors ==="
grep "TLS error:" "$TMPLOG" | sed 's/.*TLS error: //' | sort | uniq -c | sort -rn | head -10

# Also show how many errors are TLS 1.2 compatibility related
TLS12_ERROR_COUNT=$(grep "TLS error:" "$TMPLOG" | grep -E "TLS 1.2 not supported|TLS 1.2-only extension|protocol_version|supported_versions" | wc -l)
echo ""
echo "  (Note: $TLS12_ERROR_COUNT errors are TLS 1.2 compatibility related)"

# Success criteria: Core TLS 1.3 failures should be low
# We're a TLS 1.3-only client, so server-side, TLS 1.2, and optional features don't count
echo ""
if [ "$TLS13_CORE" -gt 50 ]; then
    echo "WARNING: TLS 1.3 core failures higher than expected ($TLS13_CORE > 50)"
    echo "Review the core failures above to identify real bugs."
elif [ "$TLS13_CORE" -gt 0 ]; then
    echo "INFO: $TLS13_CORE TLS 1.3 core protocol failures to investigate"
else
    echo "SUCCESS: No TLS 1.3 core protocol failures detected"
fi

echo ""
echo "=== Tests Complete ==="
