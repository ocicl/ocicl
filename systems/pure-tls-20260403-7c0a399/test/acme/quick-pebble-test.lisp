;;; quick-pebble-test.lisp --- Quick test of ACME client with Pebble
;;;
;;; Tests connection and account registration only (no validation)

(require :asdf)
(setf *compile-verbose* nil *compile-print* nil *load-verbose* nil *load-print* nil)
(push #p"/home/green/git/pure-tls/" asdf:*central-registry*)
(handler-bind ((warning (function muffle-warning)))
  (asdf:load-system :pure-tls/acme))

(in-package :pure-tls/acme)

(format t "~%=== Quick ACME/Pebble Connection Test ===~%~%")

;; Configure for Pebble
(setf *skip-tls-verify* t)
(setf *directory-url* "https://localhost:14000/dir")

;; Reset state
(setf *directory* nil
      *account-key* nil
      *account-url* nil
      *nonce* nil)

;; Test 1: Initialize
(format t "1. Initializing ACME client...~%")
(handler-case
    (progn
      (acme-init)
      (format t "   Directory loaded successfully~%")
      (format t "   newAccount: ~A~%" (cdr (assoc :new-account *directory*)))
      (format t "   newOrder: ~A~%" (cdr (assoc :new-order *directory*))))
  (error (e)
    (format t "   ERROR: ~A~%" e)
    (sb-ext:exit :code 1)))

;; Test 2: Account registration
(format t "~%2. Registering account...~%")
(handler-case
    (let ((url (acme-register-account "test@example.com")))
      (if url
          (format t "   Account registered: ~A~%" url)
          (format t "   ERROR: No account URL returned~%")))
  (error (e)
    (format t "   ERROR: ~A~%" e)
    (sb-ext:exit :code 1)))

;; Test 3: Create order
(format t "~%3. Creating certificate order...~%")
(handler-case
    (multiple-value-bind (order url)
        (acme-new-order "test.example.com")  ; Domain doesn't need to exist
      (if order
          (progn
            (format t "   Order created: ~A~%" url)
            (format t "   Status: ~A~%" (cdr (assoc :status order)))
            (format t "   Authorizations: ~A~%"
                    (length (cdr (assoc :authorizations order)))))
          (format t "   ERROR: No order returned~%")))
  (error (e)
    (format t "   ERROR: ~A~%" e)))

(format t "~%=== Quick test completed ===~%")
(sb-ext:exit :code 0)
