# pure-tls 1.10.0 Release Notes

**Release Date:** February 2026

## Summary

This release adds integrated timeout and cancellation support via the [`cl-context`](https://github.com/atgreen/cl-context) library. TLS operations can now be bounded with deadlines, cancelled cooperatively, and benefit from automatic context propagation through dynamic scoping.

## New Features

### Timeout and Cancellation Support

Integration with the `cl-context` library provides cooperative timeout and cancellation for TLS operations:

- **Automatic context propagation** - Uses `cl-context:*current-context*` dynamic variable, no explicit parameter passing required
- **Deadline enforcement** - Check timeouts at I/O boundaries (before each TLS record read, between handshake states)
- **Cooperative cancellation** - Cancel in-flight operations at next check point
- **Composable timeouts** - Parent deadlines automatically propagate to nested operations
- **Close-on-cancel watcher** - Optional background thread for immediate socket closure on cancellation

### New API Parameters

#### `make-tls-client-stream` and `make-tls-server-stream`

New optional `:request-context` parameter:

```lisp
(make-tls-client-stream socket
  :hostname "example.com"
  :request-context ctx)  ; Optional cl-context for timeout/cancellation
```

**Note:** Explicit context passing is rarely needed. The context automatically propagates via `*current-context*` when using `cl-context:with-timeout-context` or `cl-context:with-cancel`.

### New Conditions

Two new error conditions exported in the `pure-tls` package:

- **`tls-context-cancelled`** - Signaled when operation is cancelled via request context
- **`tls-deadline-exceeded`** - Signaled when operation exceeds its deadline

Both inherit from `tls-error` and can be caught as part of normal error handling.

### Usage Examples

**Basic timeout:**

```lisp
;; Timeout entire TLS operation (handshake + I/O) after 30 seconds
(cl-context:with-timeout-context (_ 30)
  (let ((socket (usocket:socket-connect "slow-server.com" 443
                                         :element-type '(unsigned-byte 8))))
    (pure-tls:with-tls-client-stream (tls (usocket:socket-stream socket)
                                          :hostname "slow-server.com")
      (read-line tls))))
```

**User cancellation:**

```lisp
;; Cooperative cancellation - checked at I/O boundaries
(multiple-value-bind (cancel-ctx cancel-fn)
    (cl-context:with-cancel (cl-context:background))
  (bt2:make-thread
    (lambda ()
      (let ((cl-context:*current-context* cancel-ctx))
        (pure-tls:make-tls-client-stream socket :hostname "example.com"))))
  ;; Later, when user clicks "Cancel":
  (funcall cancel-fn))
```

**Composable deadlines:**

```lisp
;; Parent deadline automatically propagates to all operations
(cl-context:with-timeout-context (_ 60)
  (pure-tls:with-tls-client-stream (tls socket :hostname "example.com")
    (write-http-request tls)
    (read-http-response tls)))  ; All I/O shares same 60s budget
```

## Implementation Details

### Timeout Behavior (Cooperative Checking)

Timeouts are checked cooperatively at safe points:

- **Checks occur before** each blocking operation, not during
- Existing blocking reads complete before timeout is detected
- Effective for slow servers (long waits between messages)
- Not effective for slow reads (partial data trickling in)

### When Timeout Checks Occur

- Before each TLS record read
- Between handshake state transitions
- Before stream read operations (`stream-read-byte`, `stream-read-sequence`)
- Currently **NOT** implemented for CRL fetching

### Close-on-Cancel Watcher Thread

When a context is provided, an optional watcher thread monitors for cancellation:

- Polls context state every 100ms
- Closes underlying socket when context is cancelled/deadline exceeded
- Enables immediate interruption of blocking I/O operations
- Thread automatically exits when context is done

## Breaking Changes

None. This release is fully backward compatible.

## Bug Fixes

- Fixed iparse API compatibility (now handles `IPARSE/UTIL:METAOBJECT` structs)
- Removed unsupported `:timeout` parameter from `usocket:socket-accept`
- Fixed unused context variable warnings in tests and examples

## Installation

Using [ocicl](https://github.com/ocicl/ocicl):

```bash
ocicl install pure-tls
```

## Upgrade Notes

This release is fully backwards compatible. The new `:request-context` parameter is optional and defaults to `nil` (no timeout/cancellation).

### Migration from Explicit Context Passing

If you were using explicit context passing patterns, you can simplify to use automatic propagation:

**Before (verbose):**
```lisp
(cl-context:with-timeout-context (ctx 30)
  (make-tls-client-stream socket
                          :hostname "example.com"
                          :request-context ctx))  ; Explicit passing
```

**After (idiomatic):**
```lisp
(cl-context:with-timeout-context (_ 30)
  (make-tls-client-stream socket
                          :hostname "example.com"))  ; Uses *current-context*
```

### Benefits

- **Bounded operations** - Timeouts checked at I/O boundaries
- **Responsive UIs** - Cancel long-running connections between operations
- **DoS protection** - Enforce per-connection time limits
- **Better testing** - Deterministic timeout behavior without sleep/polling

## Dependencies

New dependency added:
- [`cl-context`](https://github.com/atgreen/cl-context) - Cooperative cancellation and deadline propagation

Updated threading library:
- `bordeaux-threads` → `bordeaux-threads-2` for improved portability and atomic operations

## Documentation Updates

- README updated with comprehensive timeout/cancellation examples
- New Features section highlights cl-context integration
- API documentation updated for new parameters and conditions

## Credits

Context integration design and implementation based on [`cl-context`](https://github.com/atgreen/cl-context) by Anthony Green.
