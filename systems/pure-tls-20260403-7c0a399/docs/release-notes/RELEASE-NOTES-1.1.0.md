# Release Notes - v1.1.0

## Windows Native Certificate Verification

pure-tls now uses Windows CryptoAPI for certificate chain verification on Windows:

- **No CA bundle needed** - Uses Windows trusted root certificates automatically
- **Enterprise PKI support** - Respects Group Policy certificate deployments
- **Automatic updates** - Trust store is maintained by Windows Update
- **Authoritative** - CryptoAPI verdict is final; if it rejects a certificate, the connection fails

To disable and use pure Lisp verification instead:
```lisp
(setf pure-tls:*use-windows-certificate-store* nil)
```

## New Tests

- Windows-specific offline tests for bad certificate rejection
- Tests expired certificates, self-signed certificates, and known malware CAs (Superfish, eDellRoot)

## Changes

- Added `src/x509/windows-verify.lisp` with CFFI bindings to Windows CryptoAPI
- Added `*use-windows-certificate-store*` variable to control Windows native verification
- Certificate verification on Windows is now authoritative (no fallback to pure Lisp)
- Pure Lisp verification requires trusted roots when `+verify-required+` is used
- Added feature in README top-level features list
