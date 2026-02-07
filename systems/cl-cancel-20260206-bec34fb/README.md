# cl-cancel

A focused cancellation propagation library for Common Lisp, providing deadlines, timeouts, and hierarchical cancellation.

## Overview

`cl-cancel` provides hierarchical cancellation with deadlines and timeouts for Common Lisp applications. When a parent operation is cancelled, all child operations are automatically cancelled too.

## Key Features

- **Hierarchical Cancellation**: Parent cancellation propagates to all children
- **Deadlines & Timeouts**: Scheduled automatic cancellation with nanosecond precision
- **Efficient Waiting**: Semaphore-based blocking (no polling)
- **Stream Integration**: Automatic stream closure on cancellation for immediate I/O abort
- **Thread-Safe**: Lock-free operations where possible, careful lock ordering elsewhere
- **Scalable**: Single timer thread manages thousands of concurrent deadlines via min-heap

## Why cl-cancel?

Common Lisp lacks native support for:
1. **Cancellation propagation** - automatically cancelling child operations when parent is cancelled
2. **Deadline management** - treating time limits as cancellation triggers
3. **Stream cancellation** - aborting blocked I/O immediately when cancelled

`cl-cancel` provides these essential concurrency primitives.

## Installation

```lisp
;; Load the system
(asdf:load-system :cl-cancel)

;; Dependencies: bordeaux-threads, atomics, precise-time
```

## Basic Usage

### Simple Cancellation

```lisp
(use-package :cl-cancel)

;; Create a cancellable with a cancel function
(multiple-value-bind (ctx cancel)
    (with-cancel (background))

  ;; In another thread
  (bt:make-thread
   (lambda ()
     (loop
       (check-cancellation ctx)
       (do-work)))
   :name "worker")

  ;; Cancel from main thread
  (sleep 5)
  (funcall cancel))
```

### Timeout-Based Cancellation

```lisp
;; Timeout with automatic cleanup
(with-timeout-context (ctx 5.0)  ; 5 second timeout
  (fetch-data-from-slow-api))

;; Manual timeout management
(multiple-value-bind (ctx cancel)
    (with-timeout (background) 5.0)
  (unwind-protect
       (fetch-data-from-slow-api)
    (funcall cancel)))  ; Always clean up
```

### Deadline-Based Cancellation

```lisp
;; Absolute deadline (e.g., end of business hours)
(let ((deadline (+ (get-current-time) (* 60 60))))  ; 1 hour from now
  (with-deadline-context (ctx deadline)
    (process-batch-job)))
```

### Hierarchical Cancellation

```lisp
;; Parent cancellation propagates to children
(with-timeout-context (parent 10.0)
  (with-timeout-context (child1 5.0 parent)
    (task-1))  ; Times out after 5s OR when parent times out

  (with-timeout-context (child2 8.0 parent)
    (task-2))) ; Times out after 8s OR when parent times out
```

## Using Dynamic Variables for Request-Scoped Data

Instead of storing values in contexts, use Lisp's dynamic variables:

```lisp
;; Define request-scoped specials
(defvar *request-id* nil)
(defvar *user-id* nil)
(defvar *trace-id* nil)

;; Bind them at request boundary
(defun handle-request (request)
  (let ((*request-id* (generate-request-id))
        (*user-id* (extract-user-id request))
        (*trace-id* (extract-trace-id request)))
    (with-timeout-context (ctx 30.0)
      (process-request request))))

;; Access anywhere in the call stack
(defun log-message (msg)
  (format t "[~A] [~A] ~A~%"
          *request-id*
          *trace-id*
          msg))
```

Benefits of using dynamic variables:
1. Standard Lisp feature (no learning curve)
2. Better IDE support (navigation, completion)
3. Type declarations work (`(declaim (type string *request-id*))`)
4. Compiler optimization opportunities
5. Direct variable access (no lookup overhead)

## Stream Cancellation for Immediate I/O Abort

Automatically close streams when cancelled, interrupting blocked I/O:

```lisp
(defun fetch-http (url)
  (with-timeout-context (ctx 5.0)
    (let* ((socket (connect-to-host url))
           (cancel-monitor (close-stream-on-cancel socket)))
      (unwind-protect
           (progn
             (write-http-request socket url)
             (read-http-response socket))
        (funcall cancel-monitor)))))

;; Database query with cancellation
(defun query-db (query)
  (with-deadline-context (ctx (+ (get-current-time) 30))
    (let* ((conn (db-connect))
           (cancel-monitor (close-stream-on-cancel conn)))
      (unwind-protect
           (db-execute conn query)
        (funcall cancel-monitor)))))
```

When the context times out or is cancelled:
1. `close-stream-on-cancel` immediately closes the stream with `:abort t`
2. Blocked `read`/`write` operations are interrupted
3. The operation returns (often with an error)
4. Your cleanup code runs

## Complete Example: HTTP Client with Retry

```lisp
(defvar *request-id* nil
  "Current request ID")

(defun http-get-with-retry (url max-retries timeout)
  "Fetch URL with retries, timeout per attempt, and request tracking"
  (let ((*request-id* (make-uuid)))
    (dotimes (attempt max-retries)
      (handler-case
          (with-timeout-context (ctx timeout)
            (let* ((socket (connect-to-server url))
                   (cancel-monitor (close-stream-on-cancel socket)))
              (unwind-protect
                   (progn
                     (format t "[~A] Attempt ~D~%" *request-id* (1+ attempt))
                     (send-request socket url)
                     (return-from http-get-with-retry
                       (read-response socket)))
                (funcall cancel-monitor))))
        (deadline-exceeded (e)
          (format t "[~A] Timeout on attempt ~D~%" *request-id* (1+ attempt))
          (when (= attempt (1- max-retries))
            (error e)))
        (cancelled (e)
          (format t "[~A] Cancelled on attempt ~D~%" *request-id* (1+ attempt))
          (error e))))))
```

## API Reference

### Core Types

- `cancellable` - Base class for all cancellables
- `cancellable-context` - A cancellable that can be explicitly cancelled
- `background-context` - A cancellable that is never cancelled

### Creating Cancellables

- `(background)` → Returns a never-cancelled cancellable
- `(with-cancel parent)` → Create a cancellable with manual cancellation
- `(with-timeout parent seconds)` → Create a cancellable with timeout
- `(with-deadline parent absolute-time)` → Create a cancellable with deadline

### Cancellation Protocol

- `(done-p cancellable)` → T if cancelled or deadline exceeded
- `(cancelled-p cancellable)` → T if cancelled
- `(deadline cancellable)` → Returns deadline or NIL
- `(err cancellable)` → Returns error condition or NIL
- `(cancel cancellable &optional error)` → Cancel cancellable and children

### Utilities

- `(check-cancellation &optional cancellable)` → Signal error if done
- `(wait-until-done cancellable &optional timeout)` → Block until done
- `(close-stream-on-cancel stream &optional cancellable)` → Monitor stream closure
- `(get-current-time)` → Current time with nanosecond precision

### Dynamic Variables

- `*current-cancel-context*` - The current cancellable (implicit parameter passing)

### Convenience Macros

- `(with-cancel-context (var &optional parent) &body body)` → Auto-cleanup
- `(with-timeout-context (var seconds &optional parent) &body body)` → Auto-cleanup
- `(with-deadline-context (var deadline &optional parent) &body body)` → Auto-cleanup
- `(with-cancellable (var cancellable) &body body)` → Bind to `*current-cancel-context*`

### Conditions

- `cancellation-error` - Base class for all errors
- `cancelled` - Signaled when explicitly cancelled
- `deadline-exceeded` - Signaled when deadline/timeout expires

## Design Philosophy

1. **Cancellation is about control flow, not data flow**
   - Use cancellables for propagating cancellation
   - Use dynamic variables for propagating data

2. **Deadlines are scheduled cancellations**
   - A timeout is just "cancel after N seconds"
   - A deadline is "cancel at time T"
   - Both use the same propagation mechanism

3. **Explicit cleanup is better than implicit**
   - Always call the cancel function (use `unwind-protect`)
   - Use convenience macros when appropriate
   - Resource leaks are worse than verbose code

4. **Immediate cancellation over polling**
   - Use `close-stream-on-cancel` to abort I/O
   - Use `wait-until-done` to block efficiently
   - Avoid tight loops checking `done-p`

## Performance Characteristics

- **Cancellable creation**: O(1) with parent registration
- **Cancellation check**: O(1) lock-free read + parent chain walk
- **Cancellation propagation**: O(children) - all children cancelled recursively
- **Deadline management**: O(log n) via min-heap, single timer thread for all deadlines
- **Stream monitoring**: One thread per monitored stream (lightweight, semaphore-blocked)

## Thread Safety

All operations are thread-safe:
- Lock-free reads where possible
- Careful lock ordering prevents deadlocks
- No parent method calls under child locks
- Atomic operations for initialization

## Testing

```lisp
(asdf:test-system :cl-cancel)
```

## Related Work

- **Go's context package**: The inspiration for this library's cancellation semantics
- **bordeaux-threads**: Provides cross-implementation threading primitives

## License

MIT License - see LICENSE file for details

## Contributing

Contributions welcome! Please ensure:
1. All tests pass
2. New features include tests
3. Documentation is updated
4. Thread safety is maintained

## Author

Anthony Green <green@moxielogic.com>
