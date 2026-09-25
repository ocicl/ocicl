#!/bin/bash
# Trigger script for TLS-Anvil client testing
#
# This script is called by TLS-Anvil before each handshake test.
# It must trigger the pure-tls client to connect to TLS-Anvil's server.
#
# TLS-Anvil passes no arguments; we connect to localhost:4433 by default.

HOST="${ANVIL_HOST:-localhost}"
PORT="${ANVIL_PORT:-4433}"

# Run the pure-tls client
# Using sbcl to load and run a simple client connection
sbcl --noinform --non-interactive \
    --load "$(dirname "$0")/anvil-client.lisp" \
    --eval "(anvil-client:connect \"$HOST\" $PORT)" \
    --eval "(quit)"
