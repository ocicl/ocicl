#!/bin/bash
# Track BoringSSL test regressions
#
# Usage:
#   ./test/track-regressions.sh --save-baseline  # Save current results as baseline
#   ./test/track-regressions.sh --compare        # Compare against baseline
#   ./test/track-regressions.sh                  # Run tests and compare if baseline exists
#
# SPDX-License-Identifier: MIT

# Note: Not using set -e as grep returns non-zero for no matches

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
BASELINE_FILE="$PROJECT_DIR/test/boringssl-baseline.txt"
SHIM_PATH="$PROJECT_DIR/pure-tls-shim"
TIMEOUT="${TEST_TIMEOUT:-300}"

# Find BoringSSL runner
if [ -n "$BORINGSSL_RUNNER_BIN" ]; then
    RUNNER_BIN="$BORINGSSL_RUNNER_BIN"
elif command -v runner_test >/dev/null 2>&1; then
    RUNNER_BIN="$(command -v runner_test)"
elif [ -n "$BORINGSSL_DIR" ]; then
    BORINGSSL_RUNNER="$BORINGSSL_DIR/ssl/test/runner"
    RUNNER_BIN="$BORINGSSL_RUNNER/runner_test"
    if [ ! -f "$RUNNER_BIN" ]; then
        echo "Building BoringSSL runner..."
        (cd "$BORINGSSL_RUNNER" && go test -c -o runner_test .)
    fi
elif [ -n "$BORINGSSL_RUNNER" ]; then
    RUNNER_BIN="$BORINGSSL_RUNNER/runner_test"
else
    echo "Error: BoringSSL runner not found."
    echo "Set BORINGSSL_DIR or BORINGSSL_RUNNER environment variable."
    exit 1
fi

# Check shim exists
if [ ! -x "$SHIM_PATH" ]; then
    echo "Error: pure-tls-shim not found at $SHIM_PATH"
    echo "Build it with: make boringssl-shim"
    exit 1
fi

run_tests() {
    local output_file="$1"
    local tmplog=$(mktemp)
    trap "rm -f $tmplog" RETURN

    echo "Running BoringSSL tests..." >&2

    # Run tests and capture output directly to file
    if [ -n "$BORINGSSL_RUNNER" ]; then
        cd "$BORINGSSL_RUNNER"
        timeout "$TIMEOUT" go test -v \
            -shim-path="$SHIM_PATH" \
            -allow-unimplemented \
            > "$tmplog" 2>&1 || true
    else
        timeout "$TIMEOUT" "$RUNNER_BIN" \
            -test.v \
            -shim-path="$SHIM_PATH" \
            -allow-unimplemented \
            > "$tmplog" 2>&1 || true
    fi
    # Display final test progress
    grep -oE "[0-9]+/[0-9]+/[0-9]+/[0-9]+/[0-9]+" "$tmplog" | tail -1 || true

    # Extract test results
    # Format: PASS or FAIL followed by test name
    # Failed tests show as "FAILED (TestName)"
    # Unimplemented tests show as "UNIMPLEMENTED (TestName)"
    # Passing tests don't have explicit markers, but we can infer from the pattern

    # Extract all test names and their status
    {
        # Get failed tests
        grep -oE "FAILED \([^)]+\)" "$tmplog" | sed 's/FAILED (\(.*\))/FAIL \1/' || true
        # Get unimplemented tests
        grep -oE "UNIMPLEMENTED \([^)]+\)" "$tmplog" | sed 's/UNIMPLEMENTED (\(.*\))/SKIP \1/' || true
    } | sort -k2 > "$output_file"

    # Count results
    local failed=0
    local skipped=0
    if [ -s "$output_file" ]; then
        failed=$(grep -c "^FAIL " "$output_file" 2>/dev/null || echo 0)
        skipped=$(grep -c "^SKIP " "$output_file" 2>/dev/null || echo 0)
    fi

    # Get total from the progress line
    local progress_line
    progress_line=$(grep -oE "[0-9]+/[0-9]+/[0-9]+/[0-9]+/[0-9]+" "$tmplog" | tail -1 | tr -d '\n\r') || true
    if [ -n "$progress_line" ]; then
        local total done_count passed
        total=$(echo "$progress_line" | cut -d/ -f5 | tr -d '[:space:]')
        done_count=$(echo "$progress_line" | cut -d/ -f3 | tr -d '[:space:]')
        failed=$(echo "$failed" | tr -d '[:space:]')
        skipped=$(echo "$skipped" | tr -d '[:space:]')
        passed=$((done_count - failed - skipped))
        echo "" >&2
        echo "=== Summary ===" >&2
        echo "Total: $done_count / $total" >&2
        echo "Passed: $passed" >&2
        echo "Failed: $failed" >&2
        echo "Skipped: $skipped" >&2
    fi
}

save_baseline() {
    local tmpresults=$(mktemp)
    trap "rm -f $tmpresults" RETURN

    run_tests "$tmpresults"

    cp "$tmpresults" "$BASELINE_FILE"
    echo ""
    echo "Baseline saved to $BASELINE_FILE"
    echo "Tests tracked: $(wc -l < "$BASELINE_FILE")"
}

compare_results() {
    if [ ! -f "$BASELINE_FILE" ]; then
        echo "No baseline found at $BASELINE_FILE"
        echo "Run with --save-baseline first"
        exit 1
    fi

    local tmpresults=$(mktemp)
    trap "rm -f $tmpresults" RETURN

    run_tests "$tmpresults"

    echo ""
    echo "=== Regression Analysis ==="

    # Find regressions (tests that were passing but now fail)
    # A regression is a test that:
    # - Was NOT in baseline (meaning it passed) but is now FAIL
    # - Was SKIP in baseline but is now FAIL

    local regressions=$(mktemp)
    local improvements=$(mktemp)
    local new_skips=$(mktemp)
    trap "rm -f $regressions $improvements $new_skips" RETURN

    # Get list of currently failing tests
    grep "^FAIL " "$tmpresults" | cut -d' ' -f2- | sort > "$regressions.current_fails" 2>/dev/null || touch "$regressions.current_fails"

    # Get list of previously failing tests
    grep "^FAIL " "$BASELINE_FILE" | cut -d' ' -f2- | sort > "$regressions.baseline_fails" 2>/dev/null || touch "$regressions.baseline_fails"

    # Get list of previously skipped tests
    grep "^SKIP " "$BASELINE_FILE" | cut -d' ' -f2- | sort > "$regressions.baseline_skips" 2>/dev/null || touch "$regressions.baseline_skips"

    # Regressions: in current fails but not in baseline fails (was passing or skipped, now fails)
    comm -23 "$regressions.current_fails" "$regressions.baseline_fails" > "$regressions"

    # Improvements: in baseline fails but not in current fails (was failing, now passes or skipped)
    comm -23 "$regressions.baseline_fails" "$regressions.current_fails" > "$improvements"

    local regression_count=$(wc -l < "$regressions" | tr -d ' ')
    local improvement_count=$(wc -l < "$improvements" | tr -d ' ')

    if [ "$regression_count" -gt 0 ]; then
        echo ""
        echo "!!! REGRESSIONS ($regression_count tests now failing that weren't before) !!!"
        echo "----------------------------------------------------------------"
        cat "$regressions" | head -50
        if [ "$regression_count" -gt 50 ]; then
            echo "... and $((regression_count - 50)) more"
        fi
    else
        echo "No regressions detected."
    fi

    if [ "$improvement_count" -gt 0 ]; then
        echo ""
        echo "+++ IMPROVEMENTS ($improvement_count tests now passing that were failing) +++"
        echo "----------------------------------------------------------------"
        cat "$improvements" | head -50
        if [ "$improvement_count" -gt 50 ]; then
            echo "... and $((improvement_count - 50)) more"
        fi
    fi

    # Cleanup temp files
    rm -f "$regressions.current_fails" "$regressions.baseline_fails" "$regressions.baseline_skips"

    echo ""
    if [ "$regression_count" -gt 0 ]; then
        echo "RESULT: $regression_count regressions, $improvement_count improvements"
        return 1
    else
        echo "RESULT: No regressions, $improvement_count improvements"
        return 0
    fi
}

# Main
case "${1:-}" in
    --save-baseline)
        save_baseline
        ;;
    --compare)
        compare_results
        ;;
    --help|-h)
        echo "Usage: $0 [--save-baseline | --compare | --help]"
        echo ""
        echo "Options:"
        echo "  --save-baseline  Run tests and save results as baseline"
        echo "  --compare        Run tests and compare against baseline"
        echo "  (no args)        Same as --compare if baseline exists, else --save-baseline"
        echo ""
        echo "Environment:"
        echo "  BORINGSSL_DIR    Path to BoringSSL checkout"
        echo "  BORINGSSL_RUNNER Path to ssl/test/runner directory"
        ;;
    *)
        if [ -f "$BASELINE_FILE" ]; then
            compare_results
        else
            echo "No baseline found. Creating initial baseline..."
            save_baseline
        fi
        ;;
esac
