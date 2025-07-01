#!/usr/bin/env bash
set -xeuo pipefail

PROXY_PORT=8001
LOG_FILE=devproxy.log

###############################################################################
# 1.  Generate *and trust* the self-signed root certificate
###############################################################################
#   - On Linux the command needs sudo to write into /usr/local/share/ca-certificates
#   - On Windows it imports into the LocalMachine\Root store automatically
###############################################################################
if [[ "$RUNNER_OS" == "Linux" ]]; then
  DEVPROXY_BIN=/home/linuxbrew/.linuxbrew/bin/devproxy
  sudo $DEVPROXY_BIN cert ensure
else
  DEVPROXY_BIN=devproxy
  $DEVPROXY_BIN cert ensure
fi

###############################################################################
# 2.  Start Dev Proxy in the background and capture its output
###############################################################################
$DEVPROXY_BIN -p "$PROXY_PORT" --record --log-level Information \
  > "$LOG_FILE" 2>&1 &
PROXY_PID=$!

###############################################################################
# 3.  Expose variables to the rest of this job
###############################################################################
{
  echo "HTTPS_PROXY=http://127.0.0.1:$PROXY_PORT"
  echo "DEVPROXY_LOG=$LOG_FILE"
} >> "$GITHUB_ENV"

###############################################################################
# 4.  Keep this script alive so that the proxy dies when the job ends
###############################################################################
wait "$PROXY_PID"
