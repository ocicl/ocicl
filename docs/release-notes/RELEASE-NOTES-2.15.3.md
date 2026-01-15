# ocicl 2.15.3 Release Notes

**Release Date:** January 2026

## Summary

Bugfix release with updated vendored systems.

## Changes

### Updated Systems

- **pure-tls** 1.8.0 - ML-DSA-65 post-quantum signatures, HRR extension validation fixes
- **drakma** 2.0.10
- **mgl-pax-bootstrap** (mgl-pax, dref, autoload)
- **named-readtables**

### Removed Systems

- **cl+ssl** - Removed in favor of pure-tls cl+ssl compatibility layer

## Installation

```bash
# Self-update existing installation
ocicl update

# Or download from releases
```

## Upgrade Notes

Applications using cl+ssl should switch to pure-tls with its cl+ssl compatibility layer:

```lisp
;; In your .sbclrc or project setup
(asdf:load-system :pure-tls/compat)
```

This provides API-compatible replacements for common cl+ssl functions.
