;;; pebble-integration-test.lisp --- ACME integration test using Pebble
;;;
;;; Usage:
;;;   1. Start Pebble: ./run-pebble.sh start
;;;   2. Run test: sbcl --load pebble-integration-test.lisp
;;;   3. Stop Pebble: ./run-pebble.sh stop
;;;
;;; Requirements:
;;;   - Pebble running on localhost:14000 (use run-pebble.sh)
;;;   - Port 5001 available for TLS-ALPN-01 validation server
;;;   - Add to /etc/hosts: 127.0.0.1 acme-test.local

(require :asdf)
(setf *compile-verbose* nil *compile-print* nil *load-verbose* nil *load-print* nil)
(push #p"/home/green/git/pure-tls/" asdf:*central-registry*)
(handler-bind ((warning (function muffle-warning)))
  (asdf:load-system :pure-tls/acme))

(in-package :pure-tls/acme)

(defparameter *test-domain* "acme-test.local"
  "Domain to use for testing. Must resolve to 127.0.0.1 in /etc/hosts.")

(defparameter *test-email* "test@example.com"
  "Email for ACME account registration.")

(defparameter *pebble-url* "https://localhost:14000/dir"
  "Pebble ACME directory URL.")

(defparameter *validation-port* 5001
  "Port for TLS-ALPN-01 validation (Pebble default).")

(defun check-prerequisites ()
  "Check that prerequisites for the test are met."
  (format t "~%=== Checking Prerequisites ===~%")

  ;; Check Pebble is running
  (format t "~%1. Checking Pebble availability...~%")
  (handler-case
      (progn
        (setf *skip-tls-verify* t)
        (setf *directory-url* *pebble-url*)
        (acme-init)
        (format t "   Pebble is running at ~A~%" *pebble-url*)
        t)
    (error (e)
      (format t "   ERROR: Cannot connect to Pebble: ~A~%" e)
      (format t "   Make sure Pebble is running: ./run-pebble.sh start~%")
      nil)))

(defun run-account-test ()
  "Test account registration."
  (format t "~%=== Account Registration Test ===~%")
  (handler-case
      (let ((account-url (acme-register-account *test-email*)))
        (if account-url
            (progn
              (format t "   Account registered: ~A~%" account-url)
              t)
            (progn
              (format t "   ERROR: Account registration failed~%")
              nil)))
    (error (e)
      (format t "   ERROR: ~A~%" e)
      nil)))

(defun run-order-test ()
  "Test order creation."
  (format t "~%=== Order Creation Test ===~%")
  (handler-case
      (multiple-value-bind (order order-url)
          (acme-new-order *test-domain*)
        (if order
            (progn
              (format t "   Order created: ~A~%" order-url)
              (format t "   Status: ~A~%" (cdr (assoc :status order)))
              (format t "   Authorizations: ~A~%"
                      (length (cdr (assoc :authorizations order))))
              (values t order order-url))
            (progn
              (format t "   ERROR: Order creation failed~%")
              nil)))
    (error (e)
      (format t "   ERROR: ~A~%" e)
      nil)))

(defun run-authorization-test (order)
  "Test getting authorization and challenge info."
  (format t "~%=== Authorization Test ===~%")
  (handler-case
      (let* ((auth-urls (cdr (assoc :authorizations order)))
             (auth-url (first auth-urls))
             (auth (acme-get-authorization auth-url)))
        (format t "   Authorization URL: ~A~%" auth-url)
        (format t "   Identifier: ~A~%" (cdr (assoc :identifier auth)))
        (let ((challenge (get-tls-alpn-challenge auth)))
          (if challenge
              (progn
                (format t "   TLS-ALPN-01 challenge found!~%")
                (format t "   Token: ~A~%" (cdr (assoc :token challenge)))
                (format t "   URL: ~A~%" (cdr (assoc :url challenge)))
                (values t auth challenge auth-url))
              (progn
                (format t "   ERROR: TLS-ALPN-01 challenge not available~%")
                (format t "   Available challenges:~%")
                (dolist (c (cdr (assoc :challenges auth)))
                  (format t "     - ~A~%" (cdr (assoc :type c))))
                nil))))
    (error (e)
      (format t "   ERROR: ~A~%" e)
      nil)))

(defun run-validation-test (challenge auth-url)
  "Test TLS-ALPN-01 validation."
  (format t "~%=== TLS-ALPN-01 Validation Test ===~%")
  (let* ((token (cdr (assoc :token challenge)))
         (challenge-url (cdr (assoc :url challenge)))
         (key-auth (compute-key-authorization token)))
    (format t "   Key authorization: ~A~%" key-auth)

    ;; Start validation server
    (format t "   Starting TLS-ALPN-01 server on port ~A...~%" *validation-port*)
    (handler-case
        (progn
          (start-tls-alpn-server *test-domain* key-auth *validation-port*)
          (format t "   Validation server started~%")
          (sleep 1)

          ;; Tell ACME to verify
          (format t "   Responding to challenge...~%")
          (acme-respond-challenge challenge-url)

          ;; Poll for validation
          (format t "   Polling for validation (this may take a few seconds)...~%")
          (multiple-value-bind (result status)
              (acme-poll-status auth-url :max-attempts 20 :delay 1)
            (declare (ignore result))
            (stop-tls-alpn-server)
            (case status
              (:valid
               (format t "   Validation SUCCESSFUL!~%")
               t)
              (:invalid
               (format t "   Validation FAILED (invalid)~%")
               nil)
              (:timeout
               (format t "   Validation TIMEOUT~%")
               nil)
              (t
               (format t "   Unexpected status: ~A~%" status)
               nil))))
      (error (e)
        (stop-tls-alpn-server)
        (format t "   ERROR: ~A~%" e)
        nil))))

(defun run-certificate-test (order order-url)
  "Test certificate finalization and download."
  (format t "~%=== Certificate Issuance Test ===~%")
  (handler-case
      (let ((finalize-url (cdr (assoc :finalize order))))
        ;; Generate CSR
        (format t "   Generating CSR...~%")
        (multiple-value-bind (private-key public-key)
            (generate-domain-key)
          (let ((csr (generate-csr private-key public-key *test-domain*)))
            (format t "   CSR generated (~A bytes)~%" (length csr))

            ;; Finalize
            (format t "   Finalizing order...~%")
            (acme-finalize-order finalize-url csr)

            ;; Poll for certificate
            (format t "   Waiting for certificate...~%")
            (multiple-value-bind (final-order status)
                (acme-poll-status order-url :max-attempts 20 :delay 1)
              (case status
                ((:valid :ready)
                 (let ((cert-url (cdr (assoc :certificate final-order))))
                   (if cert-url
                       (progn
                         (format t "   Certificate URL: ~A~%" cert-url)
                         ;; Download certificate
                         (let ((cert-pem (acme-download-certificate cert-url)))
                           (if cert-pem
                               (progn
                                 (format t "   Certificate downloaded!~%")
                                 (format t "   Certificate begins with:~%")
                                 (format t "   ~A...~%"
                                         (subseq cert-pem 0 (min 100 (length cert-pem))))
                                 ;; Save to test directory
                                 (let ((cert-path (merge-pathnames
                                                   (format nil "~A-test.pem" *test-domain*)
                                                   *cert-directory*)))
                                   (ensure-directories-exist cert-path)
                                   (with-open-file (out cert-path :direction :output
                                                                  :if-exists :supersede)
                                     (write-string cert-pem out))
                                   (format t "   Saved to: ~A~%" cert-path))
                                 t)
                               (progn
                                 (format t "   ERROR: Failed to download certificate~%")
                                 nil))))
                       (progn
                         (format t "   ERROR: No certificate URL in response~%")
                         nil))))
                (t
                 (format t "   ERROR: Order not ready, status: ~A~%" status)
                 nil))))))
    (error (e)
      (format t "   ERROR: ~A~%" e)
      nil)))

(defun run-full-integration-test ()
  "Run the complete ACME integration test with Pebble."
  (format t "~%")
  (format t "========================================~%")
  (format t "  ACME Integration Test with Pebble~%")
  (format t "========================================~%")
  (format t "~%Test domain: ~A~%" *test-domain*)
  (format t "Pebble URL: ~A~%" *pebble-url*)
  (format t "Validation port: ~A~%" *validation-port*)

  ;; Reset state
  (setf *directory* nil
        *account-key* nil
        *account-url* nil
        *nonce* nil)

  ;; Run tests
  (let ((results '()))
    ;; Prerequisites
    (push (cons "Prerequisites" (check-prerequisites)) results)
    (unless (cdar results)
      (format t "~%!!! Prerequisites not met, aborting !!!~%")
      (sb-ext:exit :code 1))

    ;; Account
    (push (cons "Account Registration" (run-account-test)) results)
    (unless (cdar results)
      (format t "~%!!! Account registration failed, aborting !!!~%")
      (sb-ext:exit :code 1))

    ;; Order
    (multiple-value-bind (success order order-url)
        (run-order-test)
      (push (cons "Order Creation" success) results)
      (unless success
        (format t "~%!!! Order creation failed, aborting !!!~%")
        (sb-ext:exit :code 1))

      ;; Authorization
      (multiple-value-bind (success auth challenge auth-url)
          (run-authorization-test order)
        (push (cons "Authorization" success) results)
        (unless success
          (format t "~%!!! Authorization failed, aborting !!!~%")
          (sb-ext:exit :code 1))

        ;; Validation
        (let ((validation-success (run-validation-test challenge auth-url)))
          (push (cons "TLS-ALPN-01 Validation" validation-success) results)
          (unless validation-success
            (format t "~%!!! Validation failed, aborting !!!~%")
            (sb-ext:exit :code 1))

          ;; Certificate
          (let ((cert-success (run-certificate-test order order-url)))
            (push (cons "Certificate Issuance" cert-success) results)))))

    ;; Summary
    (format t "~%")
    (format t "========================================~%")
    (format t "  Test Results Summary~%")
    (format t "========================================~%")
    (dolist (result (reverse results))
      (format t "  ~A: ~A~%"
              (car result)
              (if (cdr result) "PASSED" "FAILED")))

    (let ((all-passed (every #'cdr results)))
      (format t "~%")
      (if all-passed
          (format t "=== ALL TESTS PASSED ===~%")
          (format t "=== SOME TESTS FAILED ===~%"))
      (sb-ext:exit :code (if all-passed 0 1)))))

;; Run the test
(run-full-integration-test)
