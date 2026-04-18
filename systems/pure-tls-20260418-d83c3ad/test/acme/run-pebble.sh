#!/bin/bash
# run-pebble.sh - Start Pebble ACME test server using podman
#
# Usage:
#   ./run-pebble.sh start   - Start Pebble in background
#   ./run-pebble.sh stop    - Stop Pebble
#   ./run-pebble.sh status  - Check if Pebble is running
#   ./run-pebble.sh logs    - Show Pebble logs

set -e

CONTAINER_NAME="pebble-acme-test"
# Pebble is published to GitHub Container Registry
PEBBLE_IMAGE="ghcr.io/letsencrypt/pebble:latest"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

start_pebble() {
    # Stop existing container if running
    podman rm -f "$CONTAINER_NAME" 2>/dev/null || true

    echo "Starting Pebble ACME test server..."

    # Run Pebble with host network so it can reach our validation server on localhost
    # Note: Pebble container uses /app as entrypoint, config at /test/config/pebble-config.json
    podman run -d \
        --name "$CONTAINER_NAME" \
        --network host \
        -e PEBBLE_VA_NOSLEEP=1 \
        -e PEBBLE_VA_ALWAYS_VALID=0 \
        -v "$SCRIPT_DIR/pebble-config.json:/test/config/pebble-config.json:ro,Z" \
        "$PEBBLE_IMAGE" \
        -config /test/config/pebble-config.json -strict

    echo "Waiting for Pebble to start..."
    sleep 3

    if podman ps | grep -q "$CONTAINER_NAME"; then
        echo "Pebble is running!"
        echo ""
        echo "ACME directory: https://localhost:14000/dir"
        echo "Management:     https://localhost:15000"
        echo ""
        echo "Pebble expects TLS-ALPN-01 validation on port 5001"
        echo ""
        echo "To trust Pebble's CA in your tests, download it from:"
        echo "  https://localhost:15000/roots/0"
    else
        echo "Failed to start Pebble"
        podman logs "$CONTAINER_NAME"
        exit 1
    fi
}

stop_pebble() {
    echo "Stopping Pebble..."
    podman rm -f "$CONTAINER_NAME" 2>/dev/null || true
    echo "Pebble stopped."
}

status_pebble() {
    if podman ps | grep -q "$CONTAINER_NAME"; then
        echo "Pebble is running"
        podman ps --filter "name=$CONTAINER_NAME"
    else
        echo "Pebble is not running"
    fi
}

logs_pebble() {
    podman logs "$CONTAINER_NAME"
}

case "${1:-}" in
    start)
        start_pebble
        ;;
    stop)
        stop_pebble
        ;;
    status)
        status_pebble
        ;;
    logs)
        logs_pebble
        ;;
    *)
        echo "Usage: $0 {start|stop|status|logs}"
        exit 1
        ;;
esac
