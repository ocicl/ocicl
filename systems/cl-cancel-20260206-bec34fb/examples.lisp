;;;; examples.lisp
;;;;
;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Examples demonstrating cl-cancel usage

(defpackage #:cl-cancel-examples
  (:use #:cl #:cl-cancel)
  (:export
   #:example-basic-timeout
   #:example-hierarchical-cancellation
   #:example-http-client
   #:example-database-query
   #:example-worker-pool))

(in-package #:cl-cancel-examples)

;;; Example 1: Basic timeout

(defun example-basic-timeout ()
  "Demonstrate basic timeout cancellation"
  (format t "Starting task with 2 second timeout...~%")
  (handler-case
      (with-timeout-context (ctx 2.0)
        (loop for i from 1 to 10 do
          (check-cancellation ctx)
          (format t "Working... iteration ~D~%" i)
          (sleep 1)))
    (deadline-exceeded ()
      (format t "Task timed out!~%"))))

;;; Example 2: Hierarchical cancellation

(defun example-hierarchical-cancellation ()
  "Demonstrate parent-child cancellation propagation"
  (format t "Starting parent task with 5 second timeout...~%")
  (handler-case
      (with-timeout-context (parent 5.0)
        (format t "Parent started~%")

        ;; Child 1 - short timeout
        (handler-case
            (with-timeout-context (child1 2.0 parent)
              (format t "Child 1 started~%")
              (loop for i from 1 to 5 do
                (check-cancellation child1)
                (format t "Child 1 working... ~D~%" i)
                (sleep 1)))
          (deadline-exceeded ()
            (format t "Child 1 timed out~%")))

        ;; Child 2 - longer timeout, but will be cancelled when parent times out
        (handler-case
            (with-timeout-context (child2 10.0 parent)
              (format t "Child 2 started~%")
              (loop for i from 1 to 15 do
                (check-cancellation child2)
                (format t "Child 2 working... ~D~%" i)
                (sleep 1)))
          (deadline-exceeded ()
            (format t "Child 2 cancelled by parent timeout~%"))))
    (deadline-exceeded ()
      (format t "Parent timed out!~%"))))

;;; Example 3: HTTP client with retry

(defvar *request-id* nil
  "Current request ID for logging")

(defun simulate-http-request (url timeout)
  "Simulate an HTTP request with timeout"
  (with-timeout-context (ctx timeout)
    (format t "[~A] Fetching ~A~%" *request-id* url)
    (sleep (random 3.0))  ; Simulate network delay
    (check-cancellation ctx)
    (format t "[~A] Response received~%" *request-id*)
    :success))

(defun example-http-client ()
  "Demonstrate HTTP client with retry and request tracking"
  (let ((*request-id* (format nil "REQ-~D" (random 10000))))
    (format t "Starting HTTP request with retry~%")
    (dotimes (attempt 3)
      (handler-case
          (return-from example-http-client
            (simulate-http-request "https://api.example.com/data" 2.0))
        (deadline-exceeded ()
          (format t "[~A] Timeout on attempt ~D~%" *request-id* (1+ attempt))
          (when (= attempt 2)
            (format t "[~A] All retries exhausted~%" *request-id*)
            (return-from example-http-client :failed)))))))

;;; Example 4: Database query with timeout

(defvar *database-connection* nil
  "Simulated database connection")

(defun simulate-db-query (query timeout)
  "Simulate a database query with timeout and stream cancellation"
  (with-timeout-context (ctx timeout)
    ;; In a real implementation, you'd use close-stream-on-cancel here
    ;; (let ((cancel-monitor (close-stream-on-cancel *database-connection*)))
    ;;   (unwind-protect ...))
    (format t "Executing query: ~A~%" query)
    (sleep (random 4.0))  ; Simulate query execution
    (check-cancellation ctx)
    (format t "Query completed~%")
    '((id 1 name "Alice") (id 2 name "Bob"))))

(defun example-database-query ()
  "Demonstrate database query with timeout"
  (handler-case
      (let ((results (simulate-db-query "SELECT * FROM users" 3.0)))
        (format t "Got ~D results~%" (length results))
        results)
    (deadline-exceeded ()
      (format t "Database query timed out~%")
      nil)))

;;; Example 5: Worker pool with cancellation

(defvar *active-workers* 0
  "Count of active workers")

(defvar *workers-lock* (bt:make-lock "workers-lock")
  "Lock for worker count")

(defun worker-task (worker-id ctx work-items)
  "Worker function that processes items until cancelled"
  (bt:with-lock-held (*workers-lock*)
    (incf *active-workers*))
  (unwind-protect
       (loop for item in work-items do
         (check-cancellation ctx)
         (format t "Worker ~D processing: ~A~%" worker-id item)
         (sleep (random 1.0)))
    (bt:with-lock-held (*workers-lock*)
      (decf *active-workers*))
    (format t "Worker ~D shutting down~%" worker-id)))

(defun example-worker-pool ()
  "Demonstrate worker pool with coordinated cancellation"
  (format t "Starting worker pool with 3 workers~%")
  (with-timeout-context (ctx 5.0)
    (let ((threads '())
          (work-items '(task-1 task-2 task-3 task-4 task-5 task-6)))
      ;; Start workers
      (dotimes (i 3)
        (push (bt:make-thread
               (lambda ()
                 (handler-case
                     (worker-task (1+ i) ctx work-items)
                   (cancelled ()
                     (format t "Worker ~D cancelled~%" (1+ i)))
                   (deadline-exceeded ()
                     (format t "Worker ~D timed out~%" (1+ i)))))
               :name (format nil "worker-~D" (1+ i)))
              threads))

      ;; Wait for completion or timeout
      (handler-case
          (loop while (plusp *active-workers*) do
            (check-cancellation ctx)
            (sleep 0.5))
        (deadline-exceeded ()
          (format t "~%Worker pool timed out with ~D active workers~%" *active-workers*)))

      ;; Wait for threads to finish
      (dolist (thread threads)
        (bt:join-thread thread :timeout 1.0))))

  (format t "~%Worker pool shut down~%"))

;;; Example 6: Using dynamic variables for request scope

(defvar *user-id* nil
  "Current user ID")

(defvar *trace-id* nil
  "Distributed trace ID")

(defun log-with-context (level message)
  "Log message with request context"
  (format t "[~A] [~A] [~A] ~A~%"
          level
          (or *request-id* "NO-REQ")
          (or *user-id* "anonymous")
          message))

(defun process-user-request (user-id)
  "Process a request with user context and cancellation"
  (let ((*request-id* (format nil "REQ-~D" (random 10000)))
        (*user-id* user-id)
        (*trace-id* (format nil "TRACE-~D" (random 100000))))
    (log-with-context "INFO" "Request started")
    (handler-case
        (with-timeout-context (ctx 3.0)
          (log-with-context "INFO" "Fetching user data")
          (sleep 1)
          (check-cancellation ctx)
          (log-with-context "INFO" "Processing business logic")
          (sleep 1)
          (check-cancellation ctx)
          (log-with-context "INFO" "Request completed")
          :success)
      (deadline-exceeded ()
        (log-with-context "ERROR" "Request timed out")
        :timeout))))

(defun example-request-context ()
  "Demonstrate using dynamic variables for request-scoped data"
  (format t "Processing requests for multiple users~%~%")
  (dolist (user-id '("alice" "bob" "charlie"))
    (process-user-request user-id)
    (terpri)))

;;; Run all examples

(defun run-all-examples ()
  "Run all examples"
  (format t "~%=== Example 1: Basic Timeout ===~%")
  (example-basic-timeout)

  (format t "~%~%=== Example 2: Hierarchical Cancellation ===~%")
  (example-hierarchical-cancellation)

  (format t "~%~%=== Example 3: HTTP Client with Retry ===~%")
  (example-http-client)

  (format t "~%~%=== Example 4: Database Query ===~%")
  (example-database-query)

  (format t "~%~%=== Example 5: Worker Pool ===~%")
  (example-worker-pool)

  (format t "~%~%=== Example 6: Request Context with Dynamic Variables ===~%")
  (example-request-context)

  (format t "~%~%All examples completed!~%"))
