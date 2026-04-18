#!/bin/bash
# Run TLS-Anvil tests against pure-tls
#
# Usage:
#   ./run-anvil-tests.sh server   # Test pure-tls as a TLS server
#   ./run-anvil-tests.sh client   # Test pure-tls as a TLS client
#
# Prerequisites:
#   - podman installed
#   - SBCL with quicklisp/ocicl
#   - pure-tls dependencies installed (run 'make load' first)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PURE_TLS_DIR="$(dirname "$(dirname "$SCRIPT_DIR")")"
RESULTS_DIR="${PURE_TLS_DIR}/test/tls-anvil/results"
PORT=4433
STRENGTH="${STRENGTH:-1}"  # Test strength (1=quick, 2=default, higher=more thorough)

# Create results directory with timestamp
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
RESULTS_DIR="${RESULTS_DIR}/${TIMESTAMP}"
mkdir -p "$RESULTS_DIR"

cleanup() {
    if [ -n "$SERVER_PID" ]; then
        echo "Stopping pure-tls server (PID $SERVER_PID)..."
        kill "$SERVER_PID" 2>/dev/null || true
        wait "$SERVER_PID" 2>/dev/null || true
    fi
}
trap cleanup EXIT

case "${1:-server}" in
    server)
        echo "=== Testing pure-tls SERVER with TLS-Anvil ==="
        echo "Starting pure-tls server on port $PORT..."

        # Start pure-tls server in background
        cd "$PURE_TLS_DIR"
        sbcl --noinform --non-interactive \
            --eval '(require :asdf)' \
            --eval "(push #p\"$PURE_TLS_DIR/\" asdf:*central-registry*)" \
            --eval '(asdf:load-system :pure-tls)' \
            --load "$SCRIPT_DIR/anvil-server.lisp" &
        SERVER_PID=$!

        # Wait for server to start
        echo "Waiting for server to start..."
        sleep 5

        if ! kill -0 "$SERVER_PID" 2>/dev/null; then
            echo "ERROR: Server failed to start"
            exit 1
        fi

        echo "Running TLS-Anvil server tests (strength=$STRENGTH)..."
        podman run --rm \
            --network=host \
            -v "$RESULTS_DIR:/output:Z" \
            ghcr.io/tls-attacker/tlsanvil \
            -outputFolder /output \
            -parallelHandshakes 1 \
            -strength "$STRENGTH" \
            -zip \
            server \
            -connect "localhost:$PORT"
        ;;

    client)
        echo "=== Testing pure-tls CLIENT with TLS-Anvil ==="
        echo ""
        echo "NOTE: Client testing requires the trigger script to run on the host."
        echo "This is complex because TLS-Anvil runs in a container."
        echo ""
        echo "For now, you can manually test by:"
        echo "  1. Run TLS-Anvil server: podman run --rm --network=host ghcr.io/tls-attacker/tlsanvil client -port $PORT"
        echo "  2. In another terminal, run: sbcl --load $SCRIPT_DIR/anvil-client.lisp --eval '(anvil-client:connect \"localhost\" $PORT)'"
        exit 1
        ;;

    help|--help|-h)
        echo "Usage: $0 [server|client]"
        echo ""
        echo "Commands:"
        echo "  server  - Test pure-tls as a TLS server (TLS-Anvil connects to us)"
        echo "  client  - Test pure-tls as a TLS client (we connect to TLS-Anvil)"
        echo ""
        echo "Environment variables:"
        echo "  STRENGTH - Test strength (1=quick, 2=default, higher=more thorough)"
        echo "  PORT     - Port to use (default: 4433)"
        echo ""
        echo "Results are saved to: test/tls-anvil/results/<timestamp>/"
        echo "Upload results.zip to https://anvil-web.2.2.2.2.nip.io/ to view"
        exit 0
        ;;

    *)
        echo "Unknown command: $1"
        echo "Usage: $0 [server|client|help]"
        exit 1
        ;;
esac

echo ""
echo "Results saved to: $RESULTS_DIR"
echo ""
echo "To view results:"
echo "  1. Visit https://anvil-web.2.2.2.2.nip.io/"
echo "  2. Upload $RESULTS_DIR/results.zip"
