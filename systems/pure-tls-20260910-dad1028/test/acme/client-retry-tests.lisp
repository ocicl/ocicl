;;; test/acme/client-retry-tests.lisp --- ACME client retry/restart tests
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Exercises the condition/restart retry layer of the ACME client with a
;;; stubbed HTTP transport and a no-op sleep, so recoverable ACME responses
;;; (badNonce, 202/not-ready, 429/rateLimited) are driven without a network or
;;; real delays.

(in-package #:cl-user)

(defpackage #:pure-tls/acme/test
  (:use #:cl #:fiveam)
  (:export #:run-acme-retry-tests
           #:acme-retry-tests))

(in-package #:pure-tls/acme/test)

(def-suite acme-retry-tests
  :description "Retry/restart handling for recoverable ACME HTTP responses.")

(in-suite acme-retry-tests)

;;;; ---------------------------------------------------------------------------
;;;; Stub transport
;;;; ---------------------------------------------------------------------------

(defvar *stub-requests*)   ; list of (:url u :method m :content c), in call order
(defvar *stub-responses*)  ; queue of (body status headers)
(defvar *stub-sleeps*)     ; list of requested sleep durations, in order

(defun stub-request (url &rest args &key method content &allow-other-keys)
  "Stand-in for *http-request-function*: records the call and pops the next
   queued (body status headers) triple."
  (declare (ignore args))
  (setf *stub-requests*
        (append *stub-requests*
                (list (list :url url :method method :content content))))
  (let ((response (pop *stub-responses*)))
    (unless response
      (error "stub-request: response queue exhausted for ~A" url))
    (values (first response) (second response) (third response))))

(defun stub-sleep (seconds)
  "Stand-in for *sleep-function*: records the duration instead of sleeping."
  (setf *stub-sleeps* (append *stub-sleeps* (list seconds))))

(defmacro with-acme-stub ((responses) &body body)
  "Run BODY with the ACME HTTP and sleep seams stubbed. RESPONSES is a list of
   (body status headers) triples returned in order."
  `(let ((*stub-requests* nil)
         (*stub-responses* (copy-list ,responses))
         (*stub-sleeps* nil)
         (acme::*http-request-function* #'stub-request)
         (acme::*sleep-function* #'stub-sleep))
     ,@body))

(defun make-test-client ()
  "Build an ACME client wired for stubbed transport: a fresh account key, an
   account URL, and a directory with the endpoints the tests need. No disk
   store is touched."
  (let ((client (acme::%make-acme-client
                 :directory-url "https://example.test/directory")))
    (setf (acme::acme-client-account-key client)
          (ironclad:generate-key-pair :secp256r1))
    (setf (acme::acme-client-account-url client) "https://example.test/acct/1")
    (setf (acme::acme-client-directory client)
          '((:new-nonce . "https://example.test/new-nonce")
            (:new-account . "https://example.test/new-account")
            (:new-order . "https://example.test/new-order")))
    client))

(defun request-nonce (request)
  "Decode the base64url JWS in REQUEST's :content and return its protected
   header nonce."
  (let* ((jws (cl-json:decode-json-from-string (getf request :content)))
         (protected64 (rest (assoc :protected jws)))
         (protected-json (flexi-streams:octets-to-string
                          (acme::base64url-decode protected64)
                          :external-format :utf-8))
         (protected (cl-json:decode-json-from-string protected-json)))
    (rest (assoc :nonce protected))))

;;;; ---------------------------------------------------------------------------
;;;; Tests
;;;; ---------------------------------------------------------------------------

(test bad-nonce-refreshes-and-retries
  "A badNonce response is retried once, re-signed with the fresh nonce the
   server returned, and the success body is returned."
  (let ((client (make-test-client)))
    (setf (acme::acme-client-nonce client) "nonce-1")
    (with-acme-stub
        ((list
          (list "{\"type\":\"urn:ietf:params:acme:error:badNonce\",\"detail\":\"bad nonce\"}"
                400 '((:replay-nonce . "nonce-fresh")))
          (list "{\"status\":\"valid\"}"
                200 '((:replay-nonce . "nonce-3")))))
      (multiple-value-bind (response status)
          (acme::client-post client "https://example.test/order" nil :use-kid t)
        (is (= 2 (length *stub-requests*))
            "badNonce should produce exactly 2 requests (1 retry)")
        (is (= 200 status))
        (is (string= "valid" (rest (assoc :status response))))
        (is (string= "nonce-1" (request-nonce (first *stub-requests*)))
            "first request signs with the original nonce")
        (is (string= "nonce-fresh" (request-nonce (second *stub-requests*)))
            "retry must be re-signed with the fresh nonce from the badNonce response")))))

(test download-retries-not-ready-until-pem
  "A 202/not-ready download waits per Retry-After, retries, and returns the PEM
   chain (the real-world 'cert issued but download returned no PEM' fix)."
  (let ((client (make-test-client)))
    (setf (acme::acme-client-nonce client) "nonce-1")
    (with-acme-stub
        ((list
          (list "" 202 '((:retry-after . "1") (:replay-nonce . "nonce-2")))
          (list (format nil "-----BEGIN CERTIFICATE-----~%MIIBfake~%-----END CERTIFICATE-----~%")
                200 '((:replay-nonce . "nonce-3")))))
      (let ((pem (acme:client-download-certificate client "https://example.test/cert")))
        (is (= 2 (length *stub-requests*))
            "download should retry the not-ready response once (2 requests)")
        (is (not (null pem))
            "download must return the PEM after the bounded 202 wait, not NIL")
        (is (and (stringp pem) (search "BEGIN CERTIFICATE" pem))
            "returned body must be the PEM certificate chain")
        (is (equal '(1) *stub-sleeps*)
            "should wait exactly once, per Retry-After, via the sleep seam")))))

(test bad-nonce-on-download-returns-raw-pem
  "The real 'cert issued but download returned no PEM' failure: the download
   POST-as-GET is rejected with HTTP 400 badNonce. The download must refresh the
   nonce, retry, and return the RAW PEM body on 200 -- never NIL, and never a
   JSON-decoded alist (the 200 success body is raw PEM, only the 400 error body
   is JSON)."
  (let ((client (make-test-client)))
    (setf (acme::acme-client-nonce client) "nonce-1")
    (with-acme-stub
        ((list
          (list "{\"type\":\"urn:ietf:params:acme:error:badNonce\",\"status\":400}"
                400 '((:replay-nonce . "nonce-download-fresh")))
          (list (format nil "-----BEGIN CERTIFICATE-----~%MIIBfake~%-----END CERTIFICATE-----~%")
                200 nil)))
      (let ((pem (acme:client-download-certificate client "https://example.test/cert")))
        (is (= 2 (length *stub-requests*))
            "badNonce on download must retry once (2 requests)")
        (is (string= "nonce-download-fresh"
                     (request-nonce (second *stub-requests*)))
            "the retry must be re-signed with the fresh nonce from the 400 badNonce")
        (is (not (null pem))
            "download must return the PEM after the badNonce retry, not NIL")
        (is (stringp pem)
            "the 200 body is raw PEM: it must be returned as a string, never JSON-decoded")
        (is (search "BEGIN CERTIFICATE" pem)
            "returned body must be the raw PEM certificate chain")))))

(test rate-limited-waits-and-retries
  "A 429/rateLimited response waits per Retry-After, retries, and succeeds."
  (let ((client (make-test-client)))
    (setf (acme::acme-client-nonce client) "nonce-1")
    (with-acme-stub
        ((list
          (list "{\"type\":\"urn:ietf:params:acme:error:rateLimited\",\"detail\":\"slow down\"}"
                429 '((:retry-after . "2") (:replay-nonce . "nonce-2")))
          (list "{\"status\":\"ready\"}"
                200 '((:replay-nonce . "nonce-3")))))
      (multiple-value-bind (response status)
          (acme::client-post client "https://example.test/order" nil :use-kid t)
        (is (= 2 (length *stub-requests*))
            "rate limit should be retried once (2 requests)")
        (is (= 200 status))
        (is (string= "ready" (rest (assoc :status response))))
        (is (equal '(2) *stub-sleeps*)
            "should wait per Retry-After via the sleep seam")))))

(test persistent-bad-nonce-is-bounded
  "A server that always returns badNonce is retried up to 3 times, then the
   condition surfaces cleanly (no infinite loop, no crash). Under an
   established handler the acme-bad-nonce condition propagates; with no handler
   at all SIGNAL simply returns and the 400 response is surfaced to the caller
   as before v1.12.0. Either way recovery is bounded to 4 total requests."
  (let ((client (make-test-client)))
    (setf (acme::acme-client-nonce client) "nonce-1")
    (with-acme-stub
        ((loop repeat 8
               collect (list "{\"type\":\"urn:ietf:params:acme:error:badNonce\",\"detail\":\"bad nonce\"}"
                             400 '((:replay-nonce . "nonce-x")))))
      (signals acme:acme-bad-nonce
        (acme::client-post client "https://example.test/order" nil :use-kid t))
      (is (= 4 (length *stub-requests*))
          "badNonce retries are bounded to 3 (4 total requests)"))))

(test poll-status-tolerates-numeric-status
  "A poll response whose :status is numeric (a problem document's status) is
   treated as non-terminal rather than compared as a string; polling recovers
   to :valid on a later response."
  (let ((client (make-test-client)))
    (setf (acme::acme-client-nonce client) "nonce-1")
    (with-acme-stub
        ((list
          (list "{\"status\":202}" 200 '((:replay-nonce . "nonce-2")))
          (list "{\"status\":\"valid\"}" 200 '((:replay-nonce . "nonce-3")))))
      (multiple-value-bind (response state)
          (acme:client-poll-status client "https://example.test/order"
                                    :max-attempts 5 :delay 1)
        (is (eq :valid state)
            "a numeric :status must not crash; poll recovers to :valid")
        (is (string= "valid" (rest (assoc :status response))))
        (is (= 2 (length *stub-requests*)))
        (is (equal '(1) *stub-sleeps*)
            "poll waits its own single interval between attempts, never nesting")))))

(test terminal-error-is-not-retried
  "A non-recoverable 4xx is not retried; the caller raises as before v1.12.0."
  (let ((client (make-test-client)))
    (setf (acme::acme-client-nonce client) "nonce-1")
    (with-acme-stub
        ((list
          (list "{\"type\":\"urn:ietf:params:acme:error:malformed\",\"detail\":\"bad request\"}"
                400 '((:replay-nonce . "nonce-2")))
          (list "{\"status\":\"valid\"}" 200 nil)))
      (signals acme:acme-order-error
        (acme:client-new-order client "example.test"))
      (is (= 1 (length *stub-requests*))
          "a non-recoverable 4xx must not be retried"))))

(test register-account-log-omits-account-email
  "The pre-POST account-registration log must never carry the account email.
   Install a capturing logger, drive registration over the stubbed transport
   (no network), and assert the registration log line is emitted verbatim with
   no '@' and no address text anywhere in what was logged."
  (let ((client (make-test-client))
        (captured '()))
    (setf (acme::acme-client-logger client)
          (lambda (level format-string &rest args)
            (declare (ignore level))
            (push (apply #'format nil format-string args) captured)))
    (setf (acme::acme-client-nonce client) "nonce-1")
    (with-acme-stub
        ((list
          (list "{\"status\":\"valid\"}" 201
                '((:location . "https://example.test/acct/1")
                  (:replay-nonce . "nonce-2")))))
      (acme::client-register-account client "ops@example.test"))
    (let ((messages (reverse captured)))
      (is (member "Registering ACME account" messages :test #'string=)
          "the registration log line must be emitted verbatim, address-free")
      (is (notany (lambda (m) (find #\@ m)) messages)
          "no emitted log line may contain an @ (the account email must not leak)")
      (is (notany (lambda (m) (search "ops@example.test" m)) messages)
          "the account email must never appear in any emitted log line"))))

;;;; ---------------------------------------------------------------------------
;;;; Runner
;;;; ---------------------------------------------------------------------------

(defun run-acme-retry-tests ()
  "Run the ACME client retry test suite. Returns T if all tests pass."
  (format t "~&=== Running ACME Client Retry Tests ===~%~%")
  (run! 'acme-retry-tests))
