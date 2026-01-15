# pure-tls 1.2.0 Release Notes

**Release Date:** January 2026

## Summary

This release adds native macOS Keychain support for certificate verification, matching the Windows CryptoAPI integration added in 1.1.0.

## New Features

### macOS Keychain Integration
- **Native trust store** - Uses macOS Security.framework for certificate chain verification
- **No CA bundle needed** - Automatically uses system Keychain trusted roots
- **Enterprise PKI support** - Respects MDM-deployed certificates
- **Configurable** - Set `*use-macos-keychain*` to NIL to use pure Lisp verification

### Improved +verify-peer+ Behavior
- `+verify-peer+` now verifies the certificate chain, not just the hostname
- Previously only hostname was checked; chain verification required `+verify-required+`
- Since servers always present certificates, this change ensures proper validation

## Bug Fixes

- Fixed potential memory leak in macOS certificate array creation on partial failure
- Improved CFError handling for actionable error messages on macOS

## API Changes

### New Exports
- `*use-macos-keychain*` - Control whether to use native macOS verification (default T on macOS)
- `verify-certificate-chain-macos` - Direct access to Security.framework verification (macOS only)

## Dependencies

- CFFI is now required on macOS (in addition to Windows) for native trust store bindings

## Installation

Using [ocicl](https://github.com/ocicl/ocicl):

```bash
ocicl install pure-tls
```
