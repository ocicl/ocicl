# ACME Integration Testing with Pebble

This directory contains tools for testing the pure-tls/acme system against
[Pebble](https://github.com/letsencrypt/pebble), Let's Encrypt's test ACME server.

## Prerequisites

1. **Podman** installed and working
2. **Test domain in /etc/hosts** - Add this line to `/etc/hosts`:
   ```
   127.0.0.1 acme-test.local
   ```
3. **Port 5001 available** for TLS-ALPN-01 validation

## Quick Start

```bash
# 1. Add test domain to /etc/hosts (requires sudo)
echo "127.0.0.1 acme-test.local" | sudo tee -a /etc/hosts

# 2. Start Pebble
./run-pebble.sh start

# 3. Run the integration test
sbcl --load pebble-integration-test.lisp

# 4. Stop Pebble when done
./run-pebble.sh stop
```

## Files

- `run-pebble.sh` - Shell script to start/stop Pebble via podman
- `pebble-config.json` - Pebble configuration
- `pebble-integration-test.lisp` - Full integration test

## Test Flow

The integration test performs:

1. **Prerequisites check** - Verifies Pebble is reachable
2. **Account registration** - Creates an ACME account
3. **Order creation** - Requests a certificate for the test domain
4. **Authorization** - Retrieves TLS-ALPN-01 challenge
5. **Validation** - Starts TLS-ALPN server and completes challenge
6. **Certificate issuance** - Finalizes order and downloads certificate

## Configuration

The test uses these defaults (configurable in the test file):

- **Test domain**: `acme-test.local`
- **Pebble URL**: `https://localhost:14000/dir`
- **Validation port**: 5001 (Pebble default for TLS-ALPN-01)

## Troubleshooting

### "Cannot connect to Pebble"
- Ensure Pebble is running: `./run-pebble.sh status`
- Check Pebble logs: `./run-pebble.sh logs`

### Validation fails
- Ensure `acme-test.local` resolves to `127.0.0.1`
- Ensure port 5001 is not in use
- Check that no firewall is blocking localhost connections

### TLS certificate errors
- The test sets `*skip-tls-verify*` to handle Pebble's self-signed cert

## Running Individual Tests

From a REPL, you can run parts of the test:

```lisp
(ql:quickload :pure-tls/acme)
(in-package :pure-tls/acme)

;; Configure for Pebble
(setf *skip-tls-verify* t)
(setf *directory-url* "https://localhost:14000/dir")

;; Initialize
(acme-init)

;; Register account
(acme-register-account "test@example.com")

;; Create order
(acme-new-order "acme-test.local")
```
