#!/bin/bash
# Test the Chrome interop server with openssl s_client

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

echo "Starting pure-tls server..."
sbcl --noinform --non-interactive --load chrome-server.lisp > /tmp/server.log 2>&1 &
SERVER_PID=$!

# Wait for server to start
sleep 4

echo "Connecting with OpenSSL..."
echo ""

# Connect and send HTTP request
printf "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n" | \
    openssl s_client -connect localhost:8443 -tls1_3 -quiet 2>/dev/null || true

echo ""
echo "=== Server Output ==="
cat /tmp/server.log

# Cleanup
kill $SERVER_PID 2>/dev/null || true
wait $SERVER_PID 2>/dev/null || true
