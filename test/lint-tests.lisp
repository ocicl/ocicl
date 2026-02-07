;;;; Linter test suite for issues 185-189

(defpackage #:ocicl-lint-tests
  (:use #:cl)
  (:import-from #:ocicl.lint
                #:lint-file
                #:apply-fixes-to-file
                #:issue-rule
                #:issue-line
                #:issue-column
                #:issue-message)
  (:export #:run-all-tests
           #:test-issue-185
           #:test-issue-186
           #:test-issue-187
           #:test-issue-188
           #:test-issue-189))

(in-package #:ocicl-lint-tests)

(defvar *test-count* 0)
(defvar *test-passed* 0)
(defvar *test-failed* 0)

(defmacro deftest (name &body body)
  `(defun ,name ()
     (format t "~%Running test: ~A~%" ',name)
     (incf *test-count*)
     (handler-case
         (progn
           ,@body
           (incf *test-passed*)
           (format t "  PASSED~%")
           t)
       (error (e)
         (incf *test-failed*)
         (format t "  FAILED: ~A~%" e)
         nil))))

(defun assert-true (condition message)
  (unless condition
    (error "Assertion failed: ~A" message)))

(defun assert-false (condition message)
  (when condition
    (error "Assertion failed: ~A" message)))

(defun assert-equal (expected actual message)
  (unless (equal expected actual)
    (error "Assertion failed: ~A~%  Expected: ~A~%  Actual: ~A" message expected actual)))

(defun has-issue-p (issues rule-name)
  "Check if any issue has the given rule name."
  (some (lambda (issue) (string= (issue-rule issue) rule-name)) issues))

(defun count-issues (issues rule-name)
  "Count issues with the given rule name."
  (count-if (lambda (issue) (string= (issue-rule issue) rule-name)) issues))

(defun write-test-file (content)
  "Write content to a temporary test file and return the path."
  (let ((path (format nil "/tmp/ocicl-lint-test-~A.lisp" (get-universal-time))))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string content out))
    path))

(defun cleanup-test-file (path)
  "Delete temporary test file."
  (when (probe-file path)
    (delete-file path)))

;;; Test for Issue 185: malformed-let in backquoted macro definitions

(deftest test-issue-185-no-false-positive
  "Backquoted macro with unquoted binding variable should not trigger malformed-let."
  (let* ((code "(defmacro with-db ((db-var) &body body)
  `(bt:with-lock-held (*db-lock*)
     (let ((,db-var (or *db* (open-db))))
       (unwind-protect
            ,@body
         (unless *db*
           (close-db ,db-var))))))
")
         (path (write-test-file code)))
    (unwind-protect
         (let ((issues (lint-file path)))
           (assert-false (has-issue-p issues "malformed-let")
                        "Should not flag ,db-var as malformed-let in backquoted macro"))
      (cleanup-test-file path))))

(deftest test-issue-185-actual-malformed
  "Actual malformed LET should still be caught."
  (let* ((code "(defun foo ()
  (let ((42 'x))
    x))
")
         (path (write-test-file code)))
    (unwind-protect
         (let ((issues (lint-file path)))
           (assert-true (has-issue-p issues "malformed-let")
                       "Should flag actual malformed LET with non-symbol binding"))
      (cleanup-test-file path))))

;;; Test for Issue 186: redundant-block with RETURN statements

(deftest test-issue-186-block-with-return
  "BLOCK with RETURN statement should not be flagged as redundant."
  (let* ((code "(defun foo ()
  (block nil
    (let ((x 42))
      (when (> x 10)
        (return))
      (do-something x))))
")
         (path (write-test-file code)))
    (unwind-protect
         (let ((issues (lint-file path)))
           (assert-false (has-issue-p issues "redundant-block")
                        "Should not flag BLOCK as redundant when it contains RETURN"))
      (cleanup-test-file path))))

(deftest test-issue-186-block-with-return-from
  "BLOCK with RETURN-FROM statement should not be flagged as redundant."
  (let* ((code "(defun foo ()
  (block myblock
    (let ((x 42))
      (when (> x 10)
        (return-from myblock x))
      (do-something x))))
")
         (path (write-test-file code)))
    (unwind-protect
         (let ((issues (lint-file path)))
           (assert-false (has-issue-p issues "redundant-block")
                        "Should not flag BLOCK as redundant when it contains RETURN-FROM"))
      (cleanup-test-file path))))

(deftest test-issue-186-truly-redundant-block
  "BLOCK without RETURN should still be flagged as redundant."
  (let* ((code "(defun foo ()
  (block nil
    (+ 1 2)))
")
         (path (write-test-file code)))
    (unwind-protect
         (let ((issues (lint-file path)))
           (assert-true (has-issue-p issues "redundant-block")
                       "Should flag BLOCK as redundant when it has no RETURN"))
      (cleanup-test-file path))))

;;; Test for Issue 187: Auto-fixer generating invalid code

(deftest test-issue-187-no-fix-progn-splice
  "Auto-fixer should not fix (progn ,@body) in macros."
  (let* ((code "(defmacro with-db ((db-var) &body body)
  `(bt:with-lock-held (*db-lock*)
     (let ((,db-var (or *db* (open-db))))
       (unwind-protect
            (progn ,@body)
         (unless *db*
           (close-db ,db-var))))))
")
         (path (write-test-file code))
         (issues (lint-file path)))
    (unwind-protect
         (progn
           ;; Should not have redundant-progn issue for (progn ,@body)
           (assert-false (has-issue-p issues "redundant-progn")
                        "Should not flag (progn ,@body) as redundant")
           ;; If there were an issue, applying fixes should not break the code
           (when (has-issue-p issues "redundant-progn")
             (let ((fixed-content (apply-fixes-to-file path issues)))
               (assert-false (search "unquote-splicing" fixed-content)
                           "Fixed code should not contain 'unquote-splicing'"))))
      (cleanup-test-file path))))

;;; Test for Issue 188: redundant-progn false positive with &body

(deftest test-issue-188-progn-with-splice
  "PROGN with ,@body should not be flagged as redundant."
  (let* ((code "(defmacro with-resource (&body body)
  `(unwind-protect
       (progn ,@body)
     (cleanup)))
")
         (path (write-test-file code)))
    (unwind-protect
         (let ((issues (lint-file path)))
           (assert-false (has-issue-p issues "redundant-progn")
                        "Should not flag (progn ,@body) as redundant"))
      (cleanup-test-file path))))

(deftest test-issue-188-truly-redundant-progn
  "PROGN with single non-spliced form should still be flagged."
  (let* ((code "(defun foo ()
  (progn (+ 1 2)))
")
         (path (write-test-file code)))
    (unwind-protect
         (let ((issues (lint-file path)))
           (assert-true (has-issue-p issues "redundant-progn")
                       "Should flag truly redundant PROGN"))
      (cleanup-test-file path))))

;;; Test for Issue 189: COND indentation in auto-fixer

(deftest test-issue-189-cond-indentation
  "Auto-fixer should generate properly indented COND forms."
  (let* ((code "(defun foo (msg)
  (if msg
      (handle-sync-client-message msg)
      (progn
        (llog:info \"Sync stream ended\")
        (return))))
")
         (path (write-test-file code))
         (issues (lint-file path)))
    (unwind-protect
         (when (has-issue-p issues "bare-progn-in-if")
           (let ((fixed-content (apply-fixes-to-file path issues)))
             ;; Check that (t is properly indented after (cond
             (assert-false (search "
(t" fixed-content)
                          "The (t clause should not be at column 0")
             ;; Should have proper indentation
             (assert-true (or (search "(cond (msg" fixed-content)
                             (search "(COND (msg" fixed-content))
                         "Should contain COND form")
             (assert-true (or (search "      (t" fixed-content)
                             (search "      (T" fixed-content))
                         "The (t clause should be indented properly")))
      (cleanup-test-file path))))

;;; Test runner

(defun run-all-tests ()
  "Run all linter tests and report results."
  (setf *test-count* 0
        *test-passed* 0
        *test-failed* 0)
  (format t "~%~%=== Running Linter Tests (Issues 185-189) ===~%")

  ;; Issue 185 tests
  (format t "~%--- Issue 185: malformed-let in backquoted macros ---")
  (test-issue-185-no-false-positive)
  (test-issue-185-actual-malformed)

  ;; Issue 186 tests
  (format t "~%--- Issue 186: redundant-block with RETURN ---")
  (test-issue-186-block-with-return)
  (test-issue-186-block-with-return-from)
  (test-issue-186-truly-redundant-block)

  ;; Issue 187 tests
  (format t "~%--- Issue 187: Auto-fixer invalid code ---")
  (test-issue-187-no-fix-progn-splice)

  ;; Issue 188 tests
  (format t "~%--- Issue 188: redundant-progn with &body ---")
  (test-issue-188-progn-with-splice)
  (test-issue-188-truly-redundant-progn)

  ;; Issue 189 tests
  (format t "~%--- Issue 189: COND indentation ---")
  (test-issue-189-cond-indentation)

  (format t "~%~%=== Test Results ===~%")
  (format t "Total:  ~A~%" *test-count*)
  (format t "Passed: ~A~%" *test-passed*)
  (format t "Failed: ~A~%" *test-failed*)
  (format t "~%")

  (= *test-failed* 0))
