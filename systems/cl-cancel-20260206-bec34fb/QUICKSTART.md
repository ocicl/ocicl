# cl-cancel Quick Start Guide

Get started with cl-cancel in 5 minutes.

## Installation

```lisp
(asdf:load-system :cl-cancel)

;; Or add to your .asd file
:depends-on (#:cl-cancel ...)
```

## Basic Usage

### 1. Simple Timeout

```lisp
(use-package :cl-cancel)

;; Automatically cancel after 5 seconds
(handler-case
    (with-timeout-context (ctx 5.0)
      (fetch-data-from-api))
  (deadline-exceeded ()
    (format t "Request timed out!~%")
    :timeout))
```

### 2. Manual Cancellation

```lisp
;; Create cancellable with explicit cancel function
(multiple-value-bind (ctx cancel)
    (with-cancel (background))

  ;; Start background work
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

### 3. Parent-Child Cancellation

```lisp
;; Parent timeout propagates to children
(with-timeout-context (parent 10.0)

  ;; Child 1 - gets cancelled after 5s OR when parent times out
  (with-timeout-context (child1 5.0 parent)
    (task-1))

  ;; Child 2 - gets cancelled after 8s OR when parent times out
  (with-timeout-context (child2 8.0 parent)
    (task-2)))
```

### 4. Request-Scoped Data

Use dynamic variables for request-scoped data:

```lisp
;; Define your request-scoped variables
(defvar *request-id* nil)
(defvar *user-id* nil)

;; Bind at request boundary
(defun handle-request (request)
  (let ((*request-id* (generate-id))
        (*user-id* (extract-user request)))
    (with-timeout-context (ctx 30.0)
      (process-request request))))

;; Access anywhere in call stack
(defun log-message (msg)
  (format t "[~A] [~A] ~A~%"
          *request-id*
          *user-id*
          msg))
```

### 5. Stream Cancellation (for I/O)

```lisp
;; Automatically close stream when cancelled
(with-timeout-context (ctx 5.0)
  (let* ((socket (connect-to-server))
         (cancel-monitor (close-stream-on-cancel socket)))
    (unwind-protect
         (read-response socket)
      (funcall cancel-monitor))))
```

## Common Patterns

### HTTP Client with Retry

```lisp
(defun fetch-with-retry (url max-retries)
  (dotimes (attempt max-retries)
    (handler-case
        (with-timeout-context (ctx 5.0)
          (return-from fetch-with-retry
            (http-get url)))
      (deadline-exceeded ()
        (format t "Attempt ~D timed out~%" (1+ attempt))
        (when (= attempt (1- max-retries))
          (error "All retries exhausted"))))))
```

### Database Query

```lisp
(defun query-with-timeout (query timeout)
  (handler-case
      (with-timeout-context (ctx timeout)
        (let* ((conn (db-connect))
               (cancel-monitor (close-stream-on-cancel conn)))
          (unwind-protect
               (db-execute conn query)
            (funcall cancel-monitor))))
    (deadline-exceeded ()
      (format t "Query timed out~%")
      nil)))
```

### Worker Pool

```lisp
(defun run-worker-pool (tasks timeout)
  (with-timeout-context (pool-ctx timeout)
    (let ((threads '()))
      ;; Start workers
      (dolist (task tasks)
        (push (bt:make-thread
               (lambda ()
                 (handler-case
                     (loop
                       (check-cancellation pool-ctx)
                       (process-task task))
                   (cancelled ()
                     (cleanup-task task))))
               :name "worker")
              threads))

      ;; Wait for completion or timeout
      (dolist (thread threads)
        (bt:join-thread thread :timeout 1.0)))))
```

## API Cheatsheet

### Creating Cancellables

```lisp
(background)                      ; Never cancelled
(with-cancel parent)              ; Manual cancellation
(with-timeout parent seconds)     ; Cancel after N seconds
(with-deadline parent abs-time)   ; Cancel at absolute time
```

### Checking Status

```lisp
(done-p ctx)                      ; Is cancelled or past deadline?
(cancelled-p ctx)                 ; Was explicitly cancelled?
(deadline ctx)                    ; Get deadline (or NIL)
(err ctx)                         ; Get cancellation error
```

### Cancellation

```lisp
(cancel ctx)                      ; Cancel with default error
(cancel ctx custom-error)         ; Cancel with custom error
(check-cancellation ctx)          ; Signal error if done
```

### Utilities

```lisp
(wait-until-done ctx)             ; Block until cancelled
(wait-until-done ctx timeout)     ; Block with timeout
(close-stream-on-cancel stream)   ; Monitor stream closure
```

### Macros

```lisp
(with-cancel-context (ctx parent) body)
(with-timeout-context (ctx seconds parent) body)
(with-deadline-context (ctx abs-time parent) body)
(with-cancellable (var ctx) body) ; Bind *current-cancel-context*
```

## Error Handling

```lisp
(handler-case
    (with-timeout-context (ctx 5.0)
      (do-work))

  (cancelled (e)
    ;; Explicit cancellation
    (format t "Cancelled: ~A~%" e))

  (deadline-exceeded (e)
    ;; Timeout or deadline
    (format t "Deadline exceeded: ~A~%" e))

  (cancellation-error (e)
    ;; Catch all cancellation errors
    (format t "Cancellation error: ~A~%" e)))
```

## Best Practices

1. **Always clean up**
   ```lisp
   (multiple-value-bind (ctx cancel)
       (with-cancel (background))
     (unwind-protect
          (do-work)
       (funcall cancel)))  ; Always call cancel
   ```

2. **Use macros for auto-cleanup**
   ```lisp
   (with-timeout-context (ctx 5.0)
     (do-work))  ; Automatically cancelled on exit
   ```

3. **Check cancellation in loops**
   ```lisp
   (loop for item in items do
     (check-cancellation)
     (process-item item))
   ```

4. **Use dynamic variables for data**
   ```lisp
   ;; Good - standard Lisp
   (defvar *request-id* nil)
   (let ((*request-id* "REQ-123"))
     (do-work))
   ```

5. **Monitor I/O with streams**
   ```lisp
   (let ((cancel-monitor (close-stream-on-cancel socket)))
     (unwind-protect
          (do-io socket)
       (funcall cancel-monitor)))
   ```

## Next Steps

- Read the [full README](README.md) for detailed documentation
- Check [examples.lisp](examples.lisp) for more examples
- See [MIGRATION.md](MIGRATION.md) if coming from cl-context
- Run tests: `(asdf:test-system :cl-cancel)`

## Getting Help

- Check the [README](README.md) for comprehensive docs
- Look at [examples.lisp](examples.lisp) for patterns
- Open an issue on GitHub for bugs or questions

## Summary

cl-cancel provides:
- **Hierarchical cancellation** - parent→child propagation
- **Deadlines & timeouts** - time-based cancellation
- **Stream integration** - immediate I/O abort

Happy cancelling!
