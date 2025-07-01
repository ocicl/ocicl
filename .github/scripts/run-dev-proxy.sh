#!/usr/bin/env bash
set -eu

# pick an unused high port
PROXY_PORT=8001
LOG_FILE=devproxy.log
CERT_FILE=devproxy-cert.pem

# run in the background; redirect logs
devproxy --port $PROXY_PORT --log-file $LOG_FILE \
  --generate-ca-cert --save-ca-cert $CERT_FILE &
PROXY_PID=$!

# make values visible to later steps
echo "HTTPS_PROXY=http://127.0.0.1:$PROXY_PORT"    >> "$GITHUB_ENV"
echo "DEVPROXY_CERT=$CERT_FILE"                    >> "$GITHUB_ENV"
echo "DEVPROXY_LOG=$LOG_FILE"                      >> "$GITHUB_ENV"

# wait for the proxy to quit (it won’t, unless the job is cancelled)
wait $PROXY_PID
