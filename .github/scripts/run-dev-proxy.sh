#!/usr/bin/env bash
set -euo pipefail

PROXY_PORT=8001
LOG_FILE=devproxy.log
CERT_FILE=devproxy-cert.pem

# ── 1. export the self-signed root CA ────────────────────────────────
devproxy cert export --output "$CERT_FILE"

# ── 2. start the proxy in the background, capture its console log ───
devproxy -p "$PROXY_PORT" --record --log-level Information \
  >  "$LOG_FILE" 2>&1 &
PROXY_PID=$!

# ── 3. expose settings to later steps in the same job ───────────────
{
  echo "HTTPS_PROXY=http://127.0.0.1:$PROXY_PORT"
  echo "DEVPROXY_CERT=$CERT_FILE"
  echo "DEVPROXY_LOG=$LOG_FILE"
} >> "$GITHUB_ENV"

# keep the script alive so that the job kills Dev Proxy automatically
wait "$PROXY_PID"
