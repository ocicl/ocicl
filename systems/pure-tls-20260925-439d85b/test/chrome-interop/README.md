# Chrome Interoperability Test

Test X25519MLKEM768 (post-quantum hybrid) key exchange with Chrome.

## Prerequisites

- Chrome 124+ (has X25519MLKEM768 enabled by default)
- OpenSSL (for certificate generation)
- SBCL with usocket

## Quick Start

```bash
# 1. Generate self-signed localhost certificate (one time)
./generate-localhost-cert.sh

# 2. Start the test server
sbcl --load chrome-server.lisp

# 3. Open Chrome to https://localhost:8443/
#    Accept the self-signed certificate warning
```

## What to Expect

If post-quantum key exchange is working, you'll see:
- Server output: `Key Exchange: X25519MLKEM768 (hybrid post-quantum)`
- Webpage confirms: "Post-quantum hybrid key exchange is active!"

If Chrome falls back to classical:
- Server output: `Key Exchange: x25519`
- Check Chrome's `chrome://flags` and search for "post-quantum"

## Verifying in Chrome DevTools

1. Press F12 to open DevTools
2. Go to the "Security" tab
3. Look for "Key exchange" in the connection details
4. Should show "X25519MLKEM768" or similar

## Troubleshooting

**Certificate errors**: Re-run `./generate-localhost-cert.sh`

**Post-quantum not negotiating**:
- Check `chrome://flags` - search "Kyber" or "ML-KEM" or "post-quantum"
- Ensure Chrome 124 or later
- Try `chrome://settings/security` for post-quantum settings

**Connection refused**: Make sure port 8443 is available

## Testing with curl

If you have curl built with OpenSSL 3.5+:

```bash
curl -k -v --curves X25519MLKEM768 https://localhost:8443/
```

## Testing with openssl s_client

```bash
openssl s_client -connect localhost:8443 -groups X25519MLKEM768 -tls1_3
```
