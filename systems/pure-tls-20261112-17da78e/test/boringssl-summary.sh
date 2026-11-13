# shellcheck shell=bash
# boringssl-summary.sh --- shared result parsing for the BoringSSL test runner
#
# SPDX-License-Identifier: MIT
# Copyright (C) 2026 Anthony Green <green@moxielogic.com>
#
# Sourced by run-boringssl-tests.sh and track-regressions.sh.  Not executable
# on its own.
#
# The runner's output format changed upstream, so both formats are supported:
#
#   COUNTER format (BoringSSL up to ~2026-09, including the currently pinned
#   606d3a3).  The runner rewrites a progress counter in place using backspaces:
#
#       failed/unimplemented/done/started/total
#
#   and prints nothing for a passing or unimplemented test.  Because the
#   counter is rewritten with backspaces rather than newlines, FAILED text ends
#   up embedded mid-line, so "^FAILED (" never matches -- only an unanchored
#   "FAILED (" does.  Crucially there is no UNIMPLEMENTED text anywhere, so the
#   unimplemented count can ONLY come from the counter.  Deriving "passed" as
#   done-minus-failed here overstates it by the whole unimplemented population
#   (5699 instead of 605 on the pinned ref).
#
#   MARKER format (current upstream).  One clean line per test outcome --
#   "PASSED (Name)", "FAILED (Name)", "UNIMPLEMENTED (Name)", each at the start
#   of a line -- and NO progress counter at all.  Parsing only the counter
#   yields an empty summary on this format.
#
# boringssl_parse_counts <logfile>
#   On success sets BSSL_FAILED BSSL_UNIMPL BSSL_PASSED BSSL_DONE BSSL_TOTAL
#   and BSSL_FORMAT (counter|markers), and returns 0.
#   Returns 1 when the log matches neither format, which is how a truncated or
#   crashed run is distinguished from a completed one -- callers must not treat
#   a failure here as "zero failures".
boringssl_parse_counts() {
    local log="$1"
    BSSL_FAILED=""; BSSL_UNIMPL=""; BSSL_PASSED=""
    BSSL_DONE="";   BSSL_TOTAL="";  BSSL_FORMAT=""

    [ -r "$log" ] || return 1

    local counter
    counter=$(grep -oE "[0-9]+/[0-9]+/[0-9]+/[0-9]+/[0-9]+" "$log" 2>/dev/null | tail -1 || true)
    if [ -n "$counter" ]; then
        # shellcheck disable=SC2034  # consumed by callers, not in this file
        BSSL_FORMAT="counter"
        BSSL_FAILED=$(printf '%s' "$counter" | cut -d/ -f1)
        BSSL_UNIMPL=$(printf '%s' "$counter" | cut -d/ -f2)
        BSSL_DONE=$(printf   '%s' "$counter" | cut -d/ -f3)
        BSSL_TOTAL=$(printf  '%s' "$counter" | cut -d/ -f5)
        BSSL_PASSED=$((BSSL_DONE - BSSL_FAILED - BSSL_UNIMPL))
        return 0
    fi

    # NOTE: plain "|| true", never "|| echo 0".  grep -c ALREADY prints the
    # count and then exits 1 when that count is zero, so "|| echo 0" appends a
    # second zero and yields the literal string "0\n0" -- which is where the
    # old "Skipped: 00" output came from.
    local p f u
    p=$(grep -c "^PASSED ("        "$log" 2>/dev/null || true); p=${p:-0}
    f=$(grep -c "^FAILED ("        "$log" 2>/dev/null || true); f=${f:-0}
    u=$(grep -c "^UNIMPLEMENTED (" "$log" 2>/dev/null || true); u=${u:-0}

    if [ $((p + f + u)) -gt 0 ]; then
        # shellcheck disable=SC2034  # consumed by callers, not in this file
        BSSL_FORMAT="markers"
        BSSL_PASSED=$p; BSSL_FAILED=$f; BSSL_UNIMPL=$u
        BSSL_DONE=$((p + f + u)); BSSL_TOTAL=$BSSL_DONE
        return 0
    fi

    return 1
}

# boringssl_print_summary <logfile> [stream]
#   Print a Total/Passed/Failed/Unimplemented block, or a clear diagnostic when
#   the log cannot be parsed.  Returns non-zero in that case.
boringssl_print_summary() {
    local log="$1"
    if boringssl_parse_counts "$log"; then
        echo "Total: $BSSL_DONE / $BSSL_TOTAL"
        echo "Passed: $BSSL_PASSED"
        echo "Failed: $BSSL_FAILED"
        echo "Unimplemented: $BSSL_UNIMPL"
        return 0
    fi
    echo "WARNING: could not parse runner output ($log)."
    echo "         Neither the legacy progress counter nor PASSED/FAILED/"
    echo "         UNIMPLEMENTED markers were found. The run was probably"
    echo "         truncated (timeout) or the runner failed to start."
    return 1
}
