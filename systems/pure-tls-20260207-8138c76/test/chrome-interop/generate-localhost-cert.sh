#!/bin/bash
#
# Generate self-signed certificates for localhost testing with Chrome
#
# This creates an ECDSA P-256 certificate valid for localhost and 127.0.0.1
# The certificate is valid for 365 days.
#
# Usage: ./generate-localhost-cert.sh
#

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

echo "Generating self-signed localhost certificate..."

# Generate ECDSA P-256 private key
openssl ecparam -name prime256v1 -genkey -noout -out localhost-key.pem

# Generate certificate signing request and self-signed certificate
# Include localhost and 127.0.0.1 in Subject Alternative Names
openssl req -new -x509 \
    -key localhost-key.pem \
    -out localhost-cert.pem \
    -days 365 \
    -subj "/CN=localhost/O=pure-tls Test/C=US" \
    -addext "subjectAltName=DNS:localhost,IP:127.0.0.1,IP:::1" \
    -addext "basicConstraints=CA:FALSE" \
    -addext "keyUsage=digitalSignature,keyEncipherment" \
    -addext "extendedKeyUsage=serverAuth"

echo ""
echo "Generated files:"
echo "  localhost-cert.pem - Server certificate"
echo "  localhost-key.pem  - Private key"
echo ""
echo "Certificate details:"
openssl x509 -in localhost-cert.pem -noout -subject -dates -ext subjectAltName

echo ""
echo "Done! Now run:"
echo "  sbcl --load chrome-server.lisp"
echo ""
echo "Then open Chrome to:"
echo "  https://localhost:8443/"
