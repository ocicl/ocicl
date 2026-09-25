;;; test/security-regression-tests.lisp --- Security regression tests
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Regression tests for security findings surfaced by a SAST triage of the
;;; pure-Lisp verification and handshake-parsing paths.
;;;
;;; Each test asserts the SECURE behaviour for a fixed finding and guards
;;; against regression:
;;;   * CL-SEC-2026-0206 -- out-of-bounds read parsing a hostile ECHConfig
;;;   * CL-SEC-2026-0207 -- ExtendedKeyUsage not enforced during chain verify
;;;
;;; Fixtures (cert-only, no private keys) live in test/certs/ and were produced
;;; with OpenSSL; see the comments on each test for how to regenerate them.

(in-package #:pure-tls/test)

(def-suite security-regression-tests
  :description "Regression tests for SAST security findings (expected-failing until fixed)")

(in-suite security-regression-tests)

;;;; Note: hex-to-bytes is defined in crypto-tests.lisp; test-cert-path and
;;;; *test-certs-dir* are defined in certificate-tests.lisp.  Both files load
;;;; before this one (see pure-tls.asd :serial t component order).

;;;; ---------------------------------------------------------------------------
;;;; Finding: ECH config parsing crashes with a raw, non-TLS error on a
;;;; malformed length field (remote DoS from a single peer message).
;;;;
;;;; src/handshake/ech.lisp parse-ech-config-contents reads attacker-controlled
;;;; length fields (pk_len, pn_len, ext_len) and slices with AREF/SUBSEQ BEFORE
;;;; the only bounds check ((<= pos end), ech.lisp:92).  An oversized length
;;;; makes SUBSEQ raise SB-KERNEL:BOUNDING-INDICES-BAD-ERROR -- an ordinary CL
;;;; error, NOT a subtype of PURE-TLS:TLS-ERROR.  The EncryptedExtensions
;;;; parse path (extensions.lisp ~590) reaches this unconditionally, and the
;;;; handshake error handlers only catch TLS-* conditions, so a malicious peer
;;;; aborts the handshake with an uncaught Lisp error.
;;;;
;;;; Secure behaviour: malformed peer ECH bytes MUST surface as a graceful
;;;; PURE-TLS:TLS-ERROR (e.g. tls-decode-error / tls-handshake-error), never a
;;;; raw bounds error.  This test will pass once the ECH parser validates each
;;;; length against the remaining buffer (or routes through the bounds-checked
;;;; tls-buffer readers).
;;;; ---------------------------------------------------------------------------

(test ech-config-malformed-length-is-graceful
  "Malformed ECHConfigList length must raise a TLS-ERROR, not a raw Lisp crash."
  ;; ECHConfigList:
  ;;   total_len = 0x0009
  ;;   ECHConfig { version = 0xfe0d, length = 0x0005,
  ;;               contents = { config_id=0x00, kem_id=0x0020, pk_len=0xffff } }
  ;; pk_len (0xffff) runs far past the 11-byte buffer.
  (let ((bytes (hex-to-bytes "00 09 fe 0d 00 05 00 00 20 ff ff")))
    ;; Currently raises SB-KERNEL:BOUNDING-INDICES-BAD-ERROR (not a tls-error),
    ;; so this SIGNALS assertion fails until the parser is hardened.
    (signals pure-tls:tls-error
      (pure-tls::parse-ech-config-list bytes))))

;;;; ---------------------------------------------------------------------------
;;;; Finding: ExtendedKeyUsage (EKU) is recognised but never enforced.
;;;;
;;;; The pure-Lisp chain verifier accepts a leaf whose EKU does NOT include
;;;; serverAuth as a valid server certificate.  src/x509/verify.lisp
;;;; verify-certificate-chain checks dates, names, BasicConstraints, keyCertSign,
;;;; path length, and signatures, but contains no EKU enforcement; EKU is even
;;;; listed as a "known critical" extension (certificate.lisp), so a critical
;;;; clientAuth-only EKU passes silently.
;;;;
;;;; Secure behaviour: a leaf valid only for clientAuth must NOT be accepted for
;;;; TLS server authentication.
;;;;
;;;; DESIGN NOTE: verify-certificate-chain is also used for mTLS client-cert
;;;; validation, where a clientAuth leaf is correct.  The fix adds a :purpose
;;;; keyword (the TLS client path requests :server-auth, the server path
;;;; requests :client-auth); a leaf whose EKU is present but lists neither the
;;;; requested purpose nor anyExtendedKeyUsage is rejected.  This test requests
;;;; :server-auth explicitly, mirroring the client handshake path.
;;;;
;;;; Fixtures (regenerate with):
;;;;   openssl req -x509 -newkey rsa:2048 -nodes -keyout root.key \
;;;;     -out security-regression-root-ca.pem -subj "/CN=Test Root CA" \
;;;;     -days 36500 -sha256 \
;;;;     -addext "basicConstraints=critical,CA:TRUE" \
;;;;     -addext "keyUsage=critical,keyCertSign,cRLSign"
;;;;   openssl req -newkey rsa:2048 -nodes -keyout leaf.key -out leaf.csr \
;;;;     -subj "/CN=victim.example" -sha256
;;;;   printf "basicConstraints=critical,CA:FALSE\nkeyUsage=critical,digitalSignature\nextendedKeyUsage=critical,clientAuth\nsubjectAltName=DNS:victim.example\n" > ext.cnf
;;;;   openssl x509 -req -in leaf.csr -CA security-regression-root-ca.pem \
;;;;     -CAkey root.key -CAcreateserial \
;;;;     -out security-regression-clientauth-leaf.pem -days 36500 -sha256 \
;;;;     -extfile ext.cnf
;;;; ---------------------------------------------------------------------------

(test clientauth-only-leaf-rejected-for-server-auth
  "A clientAuth-only leaf must not validate as a server certificate."
  ;; Force the pure-Lisp verification path (not the OS native verifiers).
  (let ((pure-tls:*use-windows-certificate-store* nil)
        (pure-tls:*use-macos-keychain* nil))
    (let* ((root (pure-tls:parse-certificate-from-file
                  (test-cert-path "security-regression-root-ca.pem")))
           (leaf (pure-tls:parse-certificate-from-file
                  (test-cert-path "security-regression-clientauth-leaf.pem"))))
      ;; Sanity: the fixture really is EKU clientAuth-only with a critical EKU
      ;; extension that the verifier currently treats as "known".
      (is (member :extended-key-usage
                  (pure-tls::certificate-critical-extensions leaf))
          "Fixture leaf should carry a critical ExtendedKeyUsage extension")
      ;; With :purpose :server-auth, a clientAuth-only leaf must be rejected.
      (signals pure-tls:tls-certificate-error
        (pure-tls::verify-certificate-chain (list leaf) (list root)
                                            :now (get-universal-time)
                                            :purpose :server-auth)))))

;;;; ---------------------------------------------------------------------------
;;;; Finding: resumption must carry forward the original handshake's
;;;; authentication (RFC 8446 Sections 2.2 and 4.2.11).
;;;;
;;;; On a first verify-required handshake the client verifies the certificate
;;;; chain and hostname, then caches a NewSessionTicket keyed by hostname.  A
;;;; later PSK resumption legitimately skips Certificate/CertificateVerify -- the
;;;; resumed session inherits the authentication of the handshake that minted the
;;;; ticket -- so demanding a fresh certificate breaks valid resumption.
;;;;
;;;; The fix records, on each ticket, the hostname the minting handshake
;;;; certificate-verified under verify-required, and accepts a certificate-less
;;;; resumed Finished ONLY when the accepted PSK's ticket proves verification of
;;;; the SAME host.  Otherwise it fails closed, exactly as before.
;;;;
;;;; These proofs drive a real pure-tls loopback (pure-tls server + pure-tls
;;;; client over 127.0.0.1) so the handshakes are genuine, and they keep the
;;;; process-global ticket cache WARM across connections (no per-connection
;;;; reset) so resumption exercises the real cache.
;;;;
;;;; Fixtures generated with (long-dated CA + leaf, SAN=resumption.test):
;;;;   openssl req -x509 -newkey rsa:2048 -nodes -keyout resumption-ca.key \
;;;;     -out resumption-ca.pem -subj "/CN=pure-tls Resumption Test CA" \
;;;;     -days 36500 -sha256 \
;;;;     -addext "basicConstraints=critical,CA:TRUE" \
;;;;     -addext "keyUsage=critical,keyCertSign,cRLSign"
;;;;   openssl req -newkey rsa:2048 -nodes -keyout resumption-leaf.key \
;;;;     -out resumption-leaf.csr -subj "/CN=resumption.test" -sha256
;;;;   printf "basicConstraints=critical,CA:FALSE\nkeyUsage=critical,digitalSignature,keyEncipherment\nextendedKeyUsage=serverAuth\nsubjectAltName=DNS:resumption.test\n" > ext.cnf
;;;;   openssl x509 -req -in resumption-leaf.csr -CA resumption-ca.pem \
;;;;     -CAkey resumption-ca.key -CAcreateserial \
;;;;     -out resumption-leaf.pem -days 36500 -sha256 -extfile ext.cnf
;;;; (resumption-ca.pem, resumption-leaf.pem, resumption-leaf.key live in
;;;; test/certs/; the transient CA key and CSR are not kept.)
;;;; ---------------------------------------------------------------------------

(defun %resumption-server-loop (port n-conns ready-flag ready-lock ready-cv server-info)
  "Accept N-CONNS sequential pure-tls connections on PORT using the resumption
   test leaf certificate.  Each accepted connection completes the handshake
   (which sends a NewSessionTicket), then a single app-data byte is pushed so
   the client's read loop consumes the post-handshake ticket, then the
   connection closes.  Each connection's server-side psk-accepted flag is pushed
   onto (car SERVER-INFO), newest first."
  (let ((listen-sock nil))
    (unwind-protect
         (handler-case
             (progn
               (setf listen-sock (usocket:socket-listen "127.0.0.1" port
                                                        :reuse-address t
                                                        :element-type '(unsigned-byte 8)))
               (bt:with-lock-held (ready-lock)
                 (setf (car ready-flag) t)
                 (bt:condition-notify ready-cv))
               (dotimes (i n-conns)
                 (let ((client-sock nil)
                       (tls nil))
                   (unwind-protect
                        (handler-case
                            (progn
                              (setf client-sock
                                    (usocket:socket-accept listen-sock
                                                           :element-type '(unsigned-byte 8)))
                              (setf tls (pure-tls:make-tls-server-stream
                                         (usocket:socket-stream client-sock)
                                         :certificate (test-cert-path "resumption-leaf.pem")
                                         :key (test-cert-path "resumption-leaf.key")))
                              (push (pure-tls::server-handshake-psk-accepted
                                     (pure-tls::tls-stream-handshake tls))
                                    (car server-info))
                              ;; App-data byte drives the client's fill-buffer so
                              ;; it consumes the post-handshake NewSessionTicket.
                              (write-byte 42 tls)
                              (force-output tls)
                              ;; Wait for the client's acknowledgement byte (or EOF
                              ;; if the client aborted, e.g. a fail-closed case).
                              (read-byte tls nil nil))
                          (error () nil))
                     (ignore-errors (when tls (close tls)))
                     (ignore-errors (when client-sock
                                      (usocket:socket-close client-sock)))))))
           (error () nil))
      (ignore-errors (when listen-sock (usocket:socket-close listen-sock))))))

(defun %spawn-resumption-server (port n-conns)
  "Spawn the resumption loopback server for N-CONNS connections on PORT.
   Blocks until the server is listening.  Returns (values thread server-info),
   where (car SERVER-INFO) accumulates each connection's server-side
   psk-accepted flag (newest first)."
  (let ((ready-lock (bt:make-lock "resumption-server-ready"))
        (ready-cv (bt:make-condition-variable :name "resumption-server-ready-cv"))
        (ready-flag (list nil))
        (server-info (list nil)))
    (let ((thread (bt:make-thread
                   (lambda ()
                     (%resumption-server-loop port n-conns ready-flag ready-lock
                                              ready-cv server-info))
                   :name "resumption-test-server")))
      (bt:with-lock-held (ready-lock)
        (loop until (car ready-flag)
              do (unless (bt:condition-wait ready-cv ready-lock :timeout 5)
                   (return))))
      ;; Small delay to ensure the listening socket is fully ready.
      (sleep 0.05)
      (values thread server-info))))

(defun %resumption-client (port hostname verify ca-file)
  "Open one pure-tls client connection to PORT for HOSTNAME under VERIFY,
   trusting only CA-FILE.  On a successful handshake, reads the app-data byte
   (consuming the server's NewSessionTicket into the process-global cache),
   sends an acknowledgement byte, and returns the client handshake object so
   callers can inspect psk-accepted / peer-certificate.  Handshake failures
   (e.g. a fail-closed resumption) propagate to the caller."
  (let ((sock nil)
        (tls nil))
    (unwind-protect
         (progn
           (setf sock (usocket:socket-connect "127.0.0.1" port
                                              :element-type '(unsigned-byte 8)))
           (let ((ctx (pure-tls:make-tls-context :verify-mode verify
                                                 :ca-file ca-file
                                                 :auto-load-system-ca nil)))
             (setf tls (pure-tls:make-tls-client-stream
                        (usocket:socket-stream sock)
                        :hostname hostname
                        :verify verify
                        :context ctx))
             (let ((hs (pure-tls::tls-stream-handshake tls)))
               ;; Consume the post-handshake NewSessionTicket, then acknowledge.
               (read-byte tls)
               (write-byte 43 tls)
               (force-output tls)
               hs)))
      (ignore-errors (when tls (close tls)))
      (ignore-errors (when sock (usocket:socket-close sock))))))

(test resumed-psk-carries-forward-verification
  "A verify-required full handshake mints a certificate-verified ticket; later
   connections to the same host resume via PSK (server skips its Certificate)
   and the client accepts them as authenticated -- with the real process-global
   ticket cache warm across all connections.  RFC 8446 Sections 2.2 / 4.2.11."
  (let ((pure-tls:*use-windows-certificate-store* nil)
        (pure-tls:*use-macos-keychain* nil)
        (port (allocate-test-port))
        (ca (test-cert-path "resumption-ca.pem"))
        (host "resumption.test"))
    ;; Start from a clean slate for this host; the cache then stays warm across
    ;; every connection below (never reset between handshakes).
    (pure-tls::session-ticket-cache-clear host)
    (multiple-value-bind (thread server-info)
        (%spawn-resumption-server port 3)
      (unwind-protect
           (let (hs1 hs2 hs3)
             ;; 1st: full handshake with real certificate verification.
             (setf hs1 (%resumption-client port host pure-tls:+verify-required+ ca))
             (is (not (pure-tls::client-handshake-psk-accepted hs1))
                 "First handshake must be a full (non-resumed) handshake")
             (is (pure-tls::client-handshake-peer-certificate hs1)
                 "First handshake must present a server certificate")
             ;; The ticket minted by connection 1 carries proven provenance.
             (let ((tk (pure-tls::session-ticket-cache-get host)))
               (is (and tk (equal (pure-tls::session-ticket-verified-hostname tk) host))
                   "Cached ticket must record the verified hostname"))
             ;; 2nd: resume via PSK; server skips Certificate; accepted as authenticated.
             (setf hs2 (%resumption-client port host pure-tls:+verify-required+ ca))
             (is (pure-tls::client-handshake-psk-accepted hs2)
                 "Second connection must resume via PSK, not full-handshake")
             (is (not (pure-tls::client-handshake-peer-certificate hs2))
                 "Resumed connection must receive no server certificate")
             ;; 3rd: carry-forward keeps repeated warm-cache resumptions working.
             (setf hs3 (%resumption-client port host pure-tls:+verify-required+ ca))
             (is (pure-tls::client-handshake-psk-accepted hs3)
                 "Third connection must also resume via PSK")
             (is (not (pure-tls::client-handshake-peer-certificate hs3))
                 "Third resumed connection must receive no server certificate")
             ;; Server side agrees: one full handshake, then two resumptions.
             (is (equal (reverse (car server-info)) '(nil t t))
                 "Server must full-handshake once then resume twice"))
        (pure-tls::session-ticket-cache-clear host)
        (ignore-errors (bt:join-thread thread))))))

(test resumption-nil-provenance-fails-closed
  "A ticket minted by a non-verified (+verify-none+) origin proves no
   authentication; offering it on a +verify-required+ resumption to that host
   must fail closed with a catchable tls-certificate-error."
  (let ((pure-tls:*use-windows-certificate-store* nil)
        (pure-tls:*use-macos-keychain* nil)
        (port (allocate-test-port))
        (ca (test-cert-path "resumption-ca.pem"))
        (host "resumption.test"))
    (pure-tls::session-ticket-cache-clear host)
    (multiple-value-bind (thread server-info)
        (%spawn-resumption-server port 2)
      (declare (ignore server-info))
      (unwind-protect
           (progn
             ;; 1st: full handshake under +verify-none+ -> NIL-provenance ticket.
             (%resumption-client port host pure-tls:+verify-none+ ca)
             (let ((tk (pure-tls::session-ticket-cache-get host)))
               (is (and tk (null (pure-tls::session-ticket-verified-hostname tk)))
                   "A verify-none origin must cache a NIL-provenance ticket"))
             ;; 2nd: resume under +verify-required+ -> must fail closed.
             (signals pure-tls:tls-certificate-error
               (%resumption-client port host pure-tls:+verify-required+ ca)))
        (pure-tls::session-ticket-cache-clear host)
        (ignore-errors (bt:join-thread thread))))))

(test resumption-cross-hostname-fails-closed
  "A ticket whose proven hostname differs from the host being connected to must
   fail closed on resumption, even though the server accepts the PSK -- exercising
   the hostname-equality guard on the resumed certificate-less Finished."
  (let ((pure-tls:*use-windows-certificate-store* nil)
        (pure-tls:*use-macos-keychain* nil)
        (port (allocate-test-port))
        (ca (test-cert-path "resumption-ca.pem"))
        (host "resumption.test"))
    (pure-tls::session-ticket-cache-clear host)
    (multiple-value-bind (thread server-info)
        (%spawn-resumption-server port 2)
      (declare (ignore server-info))
      (unwind-protect
           (progn
             ;; 1st: full verify-required handshake mints a ticket for HOST.
             (%resumption-client port host pure-tls:+verify-required+ ca)
             ;; Rewrite the cached ticket's proven hostname to a different host
             ;; while leaving it keyed under HOST, so the next resumption offers a
             ;; genuine PSK whose provenance is for the wrong identity.
             (let ((tk (pure-tls::session-ticket-cache-get host)))
               (is (and tk (equal (pure-tls::session-ticket-verified-hostname tk) host))
                   "Sanity: minted ticket is provenance-stamped for HOST")
               (setf (pure-tls::session-ticket-verified-hostname tk) "other-identity.test"))
             ;; 2nd: server resumes the PSK, but its provenance is cross-hostname.
             (signals pure-tls:tls-certificate-error
               (%resumption-client port host pure-tls:+verify-required+ ca)))
        (pure-tls::session-ticket-cache-clear host)
        (ignore-errors (bt:join-thread thread))))))

;;;; ---------------------------------------------------------------------------
;;;; Finding: an unusable explicit :ca-file crashes the image instead of
;;;; signalling a catchable condition.
;;;;
;;;; make-tls-context's explicit-CA branch loaded the trust store through
;;;; read-file-bytes, which opens with-open-file with no :if-does-not-exist,
;;;; so a missing/unreadable file raised a raw FILE-ERROR.  FILE-ERROR is not
;;;; a subtype of PURE-TLS:TLS-ERROR, so a non-interactive consumer's
;;;; fail-closed handler (which catches only the tls-error family) could not
;;;; catch it and the image died.  A garbage or empty file was worse: the
;;;; parser swallowed the decode error and returned an empty trust store, so
;;;; the context silently trusted nothing.
;;;;
;;;; Secure behaviour: an explicitly-named CA source that cannot be read or
;;;; that yields zero trust anchors is a misconfiguration -- make-tls-context
;;;; must fail closed with a catchable PURE-TLS:TLS-CERTIFICATE-ERROR and the
;;;; image must survive.  Each case passes :auto-load-system-ca nil so the bad
;;;; file is the only trust source (no accidental system-store fallback).
;;;; ---------------------------------------------------------------------------

(test explicit-ca-source-fails-closed
  "An unusable explicit :ca-file must signal a catchable tls-error-family
   condition, never crash the image with a raw file-error."
  (let* ((dir (uiop:temporary-directory))
         (empty (merge-pathnames "pure-tls-fail-closed-empty.pem" dir))
         (garbage (merge-pathnames "pure-tls-fail-closed-garbage.pem" dir))
         (missing (merge-pathnames "pure-tls-fail-closed-does-not-exist.pem" dir)))
    (unwind-protect
         (progn
           ;; Empty file: zero certificates parse -> fail closed on empty store.
           (with-open-file (s empty :direction :output :if-exists :supersede
                                    :if-does-not-exist :create
                                    :element-type '(unsigned-byte 8)))
           ;; Garbage non-PEM bytes (invalid UTF-8 lead bytes): decode/parse
           ;; failure -> resignalled as a certificate error.
           (with-open-file (s garbage :direction :output :if-exists :supersede
                                      :if-does-not-exist :create
                                      :element-type '(unsigned-byte 8))
             (write-sequence #(255 254 0 1 2 3 128 200 66 66 7 7) s))
           ;; Make sure the "missing" path really is absent.
           (ignore-errors (delete-file missing))
           ;; Missing path (guaranteed absent).
           (signals pure-tls:tls-certificate-error
             (pure-tls:make-tls-context :ca-file (namestring missing)
                                        :auto-load-system-ca nil))
           ;; Empty file (zero usable anchors).
           (signals pure-tls:tls-certificate-error
             (pure-tls:make-tls-context :ca-file (namestring empty)
                                        :auto-load-system-ca nil))
           ;; Garbage non-PEM file.
           (signals pure-tls:tls-certificate-error
             (pure-tls:make-tls-context :ca-file (namestring garbage)
                                        :auto-load-system-ca nil))
           ;; Not-a-regular-file: pass the temp directory itself.  Opening a
           ;; directory as a file signals an error, which is portable AND
           ;; root-safe -- chmod 000 is bypassed when the suite runs as root,
           ;; so we deliberately use a directory path rather than an unreadable
           ;; regular file.
           (signals pure-tls:tls-certificate-error
             (pure-tls:make-tls-context :ca-file (namestring dir)
                                        :auto-load-system-ca nil)))
      (ignore-errors (delete-file empty))
      (ignore-errors (delete-file garbage)))))

;;;; Test Runner

;;;; ---------------------------------------------------------------------------
;;;; Finding: Adversarial certificate-chain validation (Georgiev et al.).
;;;;
;;;; verify-certificate-chain must fail closed on every class of forged chain:
;;;; a non-CA issuer, a violated path-length budget, a corrupted signature, and
;;;; an out-of-window validity date.  These tests drive the real pure-Lisp
;;;; verifier (OS native store disabled, :trust-anchor-mode :replace with an
;;;; explicit root list) so the decision is made by our own code, not the OS.
;;;;
;;;; The CA / date proofs use certificates constructed in-image: those checks
;;;; fire before signature verification, so no valid signatures are needed.  The
;;;; path-length and tampered-signature proofs need a chain whose earlier checks
;;;; genuinely pass, so they load a real OpenSSL-signed leaf+intermediate chain
;;;; (goodcn2-chain.pem) anchored at root-cert.pem and corrupt exactly one input.
;;;; ---------------------------------------------------------------------------

(defun %pem-chain (path)
  "Parse every CERTIFICATE block in a PEM file, in file order (leaf first).
   parse-certificate-from-file only decodes the first block, so multi-cert
   chain fixtures need this."
  (let ((text (pure-tls::octets-to-string (pure-tls::read-file-bytes path)))
        (certs nil)
        (pos 0)
        (begin "-----BEGIN CERTIFICATE-----")
        (end "-----END CERTIFICATE-----"))
    (loop for b = (search begin text :start2 pos)
          while b
          for e = (search end text :start2 b)
          while e
          do (push (pure-tls::parse-certificate
                    (pure-tls::base64-decode
                     (remove-if (lambda (c) (member c '(#\Newline #\Return #\Space)))
                                (subseq text (+ b (length begin)) e))))
                   certs)
             (setf pos (+ e (length end))))
    (nreverse certs)))

(defun %chain-cert (subject-cn issuer-cn
                    &key (basic-constraints :ca-true) path-length
                         (key-usage '(:key-cert-sign :crl-sign))
                         (not-before 0) (not-after most-positive-fixnum))
  "Construct an in-image X.509 certificate for chain-verification proofs.
   BASIC-CONSTRAINTS is :ca-true, :ca-false, or :absent.  The default validity
   window is always-valid; NOT-BEFORE / NOT-AFTER override it for date proofs.
   Names are single-CN so certificate-issued-by-p links a leaf to its issuer by
   equal CN."
  (pure-tls::make-x509-certificate
   :subject (pure-tls::make-x509-name :rdns (list (cons :common-name subject-cn)))
   :issuer (pure-tls::make-x509-name :rdns (list (cons :common-name issuer-cn)))
   :validity-not-before not-before
   :validity-not-after not-after
   :extensions
   (append
    (ecase basic-constraints
      (:ca-true (list (pure-tls::make-x509-extension
                       :oid :basic-constraints :critical t
                       :value (if path-length
                                  (list :ca t :path-length-constraint path-length)
                                  (list :ca t)))))
      (:ca-false (list (pure-tls::make-x509-extension
                        :oid :basic-constraints :critical t
                        :value (list :ca nil))))
      (:absent nil))
    (when key-usage
      (list (pure-tls::make-x509-extension
             :oid :key-usage :critical t :value key-usage))))))

(test chain-rejects-ca-false-intermediate
  "An issuer with BasicConstraints cA=FALSE (or absent) must not be accepted as
   a signing CA."
  (let ((pure-tls:*use-windows-certificate-store* nil)
        (pure-tls:*use-macos-keychain* nil)
        (now (get-universal-time)))
    ;; Intermediate explicitly asserts cA=FALSE.
    (let ((leaf (%chain-cert "leaf.example" "Intermediate CA"
                             :basic-constraints :absent))
          (inter (%chain-cert "Intermediate CA" "Root CA"
                              :basic-constraints :ca-false)))
      (signals pure-tls:tls-certificate-error
        (pure-tls::verify-certificate-chain (list leaf inter) (list inter)
                                            :now now :trust-anchor-mode :replace)))
    ;; Intermediate carries no BasicConstraints extension at all.
    (let ((leaf (%chain-cert "leaf.example" "Intermediate CA"
                             :basic-constraints :absent))
          (inter (%chain-cert "Intermediate CA" "Root CA"
                              :basic-constraints :absent)))
      (signals pure-tls:tls-certificate-error
        (pure-tls::verify-certificate-chain (list leaf inter) (list inter)
                                            :now now :trust-anchor-mode :replace)))))

(test chain-rejects-pathlen-violation
  "A CA asserting pathLenConstraint=0 with an intermediate CA below it in the
   chain must be rejected."
  (let ((pure-tls:*use-windows-certificate-store* nil)
        (pure-tls:*use-macos-keychain* nil)
        (now (get-universal-time)))
    (destructuring-bind (leaf inter)
        (%pem-chain (test-cert-path "openssl/goodcn2-chain.pem"))
      (let ((root (pure-tls:parse-certificate-from-file
                   (test-cert-path "openssl/root-cert.pem"))))
        ;; Baseline: the untampered chain verifies, so the rejection below is
        ;; attributable solely to the path-length constraint.
        (is (pure-tls::verify-certificate-chain (list leaf inter root) (list root)
                                                :now now :trust-anchor-mode :replace)
            "Untampered goodcn2 chain should verify")
        ;; Assert pathLenConstraint=0 on the trusted root: it may issue end
        ;; entities but no intermediate CA -- and the chain has exactly one.
        (let ((bc (find :basic-constraints
                        (pure-tls::x509-certificate-extensions root)
                        :key #'pure-tls::x509-extension-oid)))
          (setf (pure-tls::x509-extension-value bc)
                (list :ca t :path-length-constraint 0)))
        (signals pure-tls:tls-certificate-error
          (pure-tls::verify-certificate-chain (list leaf inter root) (list root)
                                              :now now :trust-anchor-mode :replace))))))

(test chain-rejects-tampered-signature
  "A chain that passes name / CA / pathLen / date checks but whose leaf
   signature is corrupted must be rejected at signature verification."
  (let ((pure-tls:*use-windows-certificate-store* nil)
        (pure-tls:*use-macos-keychain* nil)
        (now (get-universal-time)))
    (destructuring-bind (leaf inter)
        (%pem-chain (test-cert-path "openssl/goodcn2-chain.pem"))
      (let ((root (pure-tls:parse-certificate-from-file
                   (test-cert-path "openssl/root-cert.pem"))))
        ;; Baseline: the untampered chain verifies.
        (is (pure-tls::verify-certificate-chain (list leaf inter root) (list root)
                                                :now now :trust-anchor-mode :replace)
            "Untampered goodcn2 chain should verify")
        ;; Flip one byte of the leaf signature.  Every earlier check still
        ;; passes, so a rejection can only come from signature verification.
        (let ((sig (copy-seq (pure-tls::x509-certificate-signature leaf))))
          (setf (aref sig 20) (logxor #xff (aref sig 20)))
          (setf (pure-tls::x509-certificate-signature leaf) sig))
        (signals pure-tls:tls-certificate-error
          (pure-tls::verify-certificate-chain (list leaf inter root) (list root)
                                              :now now :trust-anchor-mode :replace))))))

(test chain-rejects-expired-leaf
  "A leaf whose notAfter is in the past must be rejected."
  (let ((pure-tls:*use-windows-certificate-store* nil)
        (pure-tls:*use-macos-keychain* nil)
        (now (get-universal-time)))
    (let ((root (%chain-cert "Root CA" "Root CA" :basic-constraints :ca-true))
          (leaf (%chain-cert "leaf.example" "Root CA"
                             :basic-constraints :absent
                             :not-after (- now 100000))))
      ;; tls-certificate-expired is internal to pure-tls (double colon).
      (signals pure-tls::tls-certificate-expired
        (pure-tls::verify-certificate-chain (list leaf root) (list root)
                                            :now now :trust-anchor-mode :replace)))))

(test chain-rejects-not-yet-valid-leaf
  "A leaf whose notBefore is in the future must be rejected."
  (let ((pure-tls:*use-windows-certificate-store* nil)
        (pure-tls:*use-macos-keychain* nil)
        (now (get-universal-time)))
    (let ((root (%chain-cert "Root CA" "Root CA" :basic-constraints :ca-true))
          (leaf (%chain-cert "leaf.example" "Root CA"
                             :basic-constraints :absent
                             :not-before (+ now 100000000))))
      ;; tls-certificate-not-yet-valid is internal to pure-tls (double colon).
      (signals pure-tls::tls-certificate-not-yet-valid
        (pure-tls::verify-certificate-chain (list leaf root) (list root)
                                            :now now :trust-anchor-mode :replace)))))

;;;; ---------------------------------------------------------------------------
;;;; Finding: RFC 5280 4.2.1.3 -- an issuer whose KeyUsage extension is present
;;;; but omits keyCertSign must not be accepted as a signing CA, even when its
;;;; BasicConstraints assert cA=TRUE.  keyCertSign is the specific bit that
;;;; authorizes certificate signing; a CA scoped to (say) CRL signing only must
;;;; not be able to mint end-entity certificates.
;;;;
;;;; The fixture is arranged so that keyCertSign is the OPERATIVE rejection:
;;;; the issuer is cA=TRUE (CA check passes), the leaf's issuer name matches
;;;; (issued-by passes), and both certs are always-valid (date checks pass), so
;;;; the only failing check is the missing keyCertSign key usage.
;;;; ---------------------------------------------------------------------------

(test chain-rejects-keycertsign-absent-issuer
  "An issuer with KeyUsage present but lacking keyCertSign must be rejected as a
   signing CA, even with BasicConstraints cA=TRUE."
  (let ((pure-tls:*use-windows-certificate-store* nil)
        (pure-tls:*use-macos-keychain* nil)
        (now (get-universal-time)))
    (let ((leaf (%chain-cert "leaf.example" "Issuing CA"
                             :basic-constraints :absent))
          ;; cA=TRUE, but KeyUsage is present WITHOUT :key-cert-sign.
          (issuer (%chain-cert "Issuing CA" "Root CA"
                               :basic-constraints :ca-true
                               :key-usage '(:crl-sign))))
      (signals pure-tls:tls-certificate-error
        (pure-tls::verify-certificate-chain (list leaf issuer) (list issuer)
                                            :now now :trust-anchor-mode :replace)))))

;;;; ---------------------------------------------------------------------------
;;;; DNS name-safety in hostname verification.
;;;;
;;;; verify-hostname must reject a syntactically unsafe DNS name (embedded NUL,
;;;; non-LDH bytes) outright rather than letting it reach a silent
;;;; unequal-compare.  These drive the real validator with certificate objects
;;;; constructed in-image (no private keys / OpenSSL fixtures needed for the
;;;; identity decision).
;;;; ---------------------------------------------------------------------------

(defun %san-cert (&rest dns-names)
  "Build a certificate whose only identity is the given SAN dNSName(s)."
  (pure-tls::make-x509-certificate
   :extensions (list (pure-tls::make-x509-extension
                      :oid :subject-alt-name
                      :value (mapcar (lambda (d) (list :dns d)) dns-names)))))

(defun %cn-only-cert (common-name)
  "Build a certificate with a Subject Common Name and NO subjectAltName."
  (pure-tls::make-x509-certificate
   :subject (pure-tls::make-x509-name
             :rdns (list (cons :common-name common-name)))))

(defun %nul-name ()
  "The classic embedded-NUL truncation-confusion SAN: www.bank.com<NUL>.evil.com."
  (concatenate 'string "www.bank.com" (string (code-char 0)) ".evil.com"))

(test verify-hostname-embedded-nul-san-is-rejected
  "A SAN dNSName carrying an embedded NUL must never be the basis of a match."
  (let ((evil-name (%nul-name)))
    ;; (a) The malicious name reaching the validator as the SAN, with the
    ;;     truncated benign identity requested, must not match.
    (signals pure-tls:tls-verification-error
      (pure-tls:verify-hostname (%san-cert evil-name) "www.bank.com"))
    ;; (b) The malicious name reaching the validator as the requested identity
    ;;     is rejected outright as an invalid DNS name.
    (signals pure-tls:tls-verification-error
      (pure-tls:verify-hostname (%san-cert "www.bank.com") evil-name))))

(test verify-hostname-u-label-still-verifies
  "The DNS name-safety check runs after IDNA normalization, so a Unicode
   (U-label) requested identity still verifies against its punycode A-label
   SAN rather than being rejected as non-LDH."
  (let ((u-label (format nil "m~Cnchen.example.com" (code-char 252)))) ; münchen
    (is-true (pure-tls:verify-hostname
              (%san-cert "xn--mnchen-3ya.example.com") u-label))))

;;;; ---------------------------------------------------------------------------
;;;; Hostname-verification policy: two orthogonal RFC 6125 knobs on the TLS
;;;; context, both defaulting to the permissive value so the default profile is
;;;; byte-for-byte the general-purpose behaviour.  ALLOW-WILDCARDS gates whether
;;;; wildcard SANs match; ALLOW-CN-FALLBACK gates Common Name fallback for a
;;;; no-SAN certificate.
;;;; ---------------------------------------------------------------------------

(test verify-hostname-default-honors-wildcard-san
  "The default policy honors an RFC 6125 wildcard SAN: *.example.com must
   authenticate www.example.com when no explicit policy is supplied."
  (is (pure-tls:verify-hostname (%san-cert "*.example.com") "www.example.com")
      "A wildcard SAN must authenticate a single-label host under the default policy"))

(test verify-hostname-allow-wildcards-nil-excludes-wildcard-san
  "With ALLOW-WILDCARDS disabled, a wildcard SAN is excluded from matching even
   where the general matcher would cover it, while an exact SAN still matches."
  (let ((policy (pure-tls:make-hostname-policy :allow-wildcards nil)))
    (signals pure-tls:tls-verification-error
      (pure-tls:verify-hostname (%san-cert "*.example.com") "foo.example.com"
                                :policy policy))
    (is (pure-tls:verify-hostname (%san-cert "dns.google") "dns.google"
                                  :policy policy)
        "An exact SAN must still match when wildcards are disabled")))

(test verify-hostname-default-permits-cn-fallback
  "The default policy falls back to the Subject Common Name for a no-SAN
   certificate (deprecated but still deployed)."
  (is (pure-tls:verify-hostname (%cn-only-cert "www.example.com")
                                "www.example.com")
      "CN fallback must authenticate a matching no-SAN certificate by default"))

(test verify-hostname-allow-cn-fallback-nil-rejects-no-san
  "With ALLOW-CN-FALLBACK disabled the Common Name is never consulted, so a
   no-SAN certificate is rejected even when its CN matches exactly."
  (let ((policy (pure-tls:make-hostname-policy :allow-cn-fallback nil)))
    (signals pure-tls:tls-verification-error
      (pure-tls:verify-hostname (%cn-only-cert "www.example.com")
                                "www.example.com"
                                :policy policy))))

(test hostname-policy-general-matcher-unchanged
  "The general RFC 6125 wildcard matcher is untouched by the policy seam: a
   single-label wildcard still matches structurally, *.com does not, and a
   wildcard over a multi-label public suffix does not."
  (is (pure-tls::hostname-matches-p "*.example.com" "foo.example.com"))
  (is (not (pure-tls::hostname-matches-p "*.com" "foo.com")))
  (is (not (pure-tls::hostname-matches-p "*.co.uk" "foo.co.uk"))))

;;;; ---------------------------------------------------------------------------
;;;; Handshake reassembly hardening: a peer controls the uint24 length in the
;;;; handshake message header and could previously force pure-tls to buffer up
;;;; to 16 MiB per message via many record-sized fragments, with O(n^2) copy
;;;; work from repeated full-buffer concatenation.  The fix (a) rejects an
;;;; excessive advertised length as soon as the 4-byte header is visible,
;;;; before further fragments are buffered, and (b) reassembles into a
;;;; growable buffer so each fragment is copied O(1) times.
;;;; ---------------------------------------------------------------------------

(defun %hs-header (msg-type body-length &optional (body-bytes 0))
  "Build a handshake message prefix: 4-byte header advertising BODY-LENGTH,
   followed by BODY-BYTES zero bytes of (possibly partial) body."
  (pure-tls::concat-octet-vectors
   (pure-tls::octet-vector msg-type
                           (ldb (byte 8 16) body-length)
                           (ldb (byte 8 8) body-length)
                           (ldb (byte 8 0) body-length))
   (pure-tls::make-octet-vector body-bytes)))

(test handshake-buffer-rejects-excessive-non-certificate-length
  "A non-certificate handshake header advertising one byte over
   *max-handshake-message-size* is rejected before any further append."
  (let ((buffer (%hs-header pure-tls::+handshake-finished+
                            (1+ pure-tls::*max-handshake-message-size*))))
    (signals pure-tls:tls-handshake-error
      (pure-tls::check-handshake-buffer-size buffer nil))))

(test handshake-buffer-accepts-length-at-cap
  "A handshake header advertising exactly the configured maximum passes the
   size check (and an incomplete header is never checked)."
  (is-false (pure-tls::check-handshake-buffer-size
             (%hs-header pure-tls::+handshake-finished+
                         pure-tls::*max-handshake-message-size*)
             nil))
  ;; Header incomplete: no length to validate yet
  (is-false (pure-tls::check-handshake-buffer-size
             (pure-tls::octet-vector pure-tls::+handshake-finished+ 0) nil)))

(test handshake-buffer-certificate-honors-configured-cap
  "A Certificate message is bounded by *max-certificate-list-size* (plus
   framing overhead) when that limit is set, and rejected above it."
  (let ((pure-tls:*max-certificate-list-size* 1000))
    ;; Within cap + framing overhead: accepted
    (is-false (pure-tls::check-handshake-buffer-size
               (%hs-header pure-tls::+handshake-certificate+ 1259) nil))
    ;; One byte over: rejected
    (signals pure-tls:tls-handshake-error
      (pure-tls::check-handshake-buffer-size
       (%hs-header pure-tls::+handshake-certificate+ 1260) nil))))

(test handshake-buffer-certificate-unlimited-allows-protocol-max
  "With *max-certificate-list-size* = 0 (unlimited), a Certificate message may
   advertise up to the uint24 protocol maximum, preserving existing behavior."
  (let ((pure-tls:*max-certificate-list-size* 0))
    (is-false (pure-tls::check-handshake-buffer-size
               (%hs-header pure-tls::+handshake-certificate+ #xFFFFFF) nil))
    ;; But a non-certificate type claiming the same length is still rejected
    (signals pure-tls:tls-handshake-error
      (pure-tls::check-handshake-buffer-size
       (%hs-header pure-tls::+handshake-encrypted-extensions+ #xFFFFFF) nil))))

(test handshake-buffer-fragmented-message-reassembles
  "A message fragmented across several appends still reassembles and extracts
   correctly through the growable reassembly buffer."
  (let* ((body-length 100)
         (fragment-1 (%hs-header pure-tls::+handshake-finished+ body-length 40))
         (fragment-2 (pure-tls::make-octet-vector 40 :initial-element 1))
         (fragment-3 (pure-tls::make-octet-vector 20 :initial-element 2))
         (buffer nil))
    (setf buffer (pure-tls::handshake-buffer-append buffer fragment-1))
    (is-false (pure-tls::handshake-buffer-has-complete-message-p buffer))
    (is-false (pure-tls::check-handshake-buffer-size buffer nil))
    (setf buffer (pure-tls::handshake-buffer-append buffer fragment-2))
    (is-false (pure-tls::handshake-buffer-has-complete-message-p buffer))
    (setf buffer (pure-tls::handshake-buffer-append buffer fragment-3))
    (is-true (pure-tls::handshake-buffer-has-complete-message-p buffer))
    (multiple-value-bind (message-bytes remaining)
        (pure-tls::handshake-buffer-extract-message buffer)
      (is (null remaining))
      (is (equalp (pure-tls::concat-octet-vectors fragment-1 fragment-2 fragment-3)
                  message-bytes)))))

(test handshake-buffer-append-grows-in-place
  "handshake-buffer-append reuses the growable buffer across appends instead
   of allocating and re-copying the full accumulated buffer per fragment."
  (let* ((first (pure-tls::make-octet-vector 512 :initial-element 3))
         (second (pure-tls::make-octet-vector 512 :initial-element 4))
         (buffer (pure-tls::handshake-buffer-append nil first)))
    (is (eq buffer (pure-tls::handshake-buffer-append buffer second)))
    (is (= 1024 (length buffer)))
    (is (equalp (pure-tls::concat-octet-vectors first second)
                (coerce buffer '(vector (unsigned-byte 8)))))))

;;;; ---------------------------------------------------------------------------
;;;; Finding: a NOW that is not a real is not type-checked where it is used, so
;;;; the fault gets misattributed to the certificate.
;;;;
;;;; A non-real NOW flows into the notBefore / notAfter comparisons and fails
;;;; there, as a bare TYPE-ERROR naming a comparison rather than the argument
;;;; that was wrong.  A caller reading that condition sees a problem with the
;;;; chain it passed in.  On Windows and macOS the native dispatches never read
;;;; NOW at all, so the same bad value produces a different outcome again.
;;;;
;;;; Secure behaviour: a NOW that is not a real is rejected outright, ahead of
;;;; the native dispatches, so the call fails in one place on every platform
;;;; with a condition that names the argument responsible.
;;;; ---------------------------------------------------------------------------

(test non-real-verification-time-is-rejected
  "A NOW that is not a real must be rejected, and a call supplying a real one
   must be unaffected."
  (let ((pure-tls:*use-windows-certificate-store* nil)
        (pure-tls:*use-macos-keychain* nil)
        (now (get-universal-time)))
    (destructuring-bind (leaf inter)
        (%pem-chain (test-cert-path "openssl/goodcn2-chain.pem"))
      (let ((root (pure-tls:parse-certificate-from-file
                   (test-cert-path "openssl/root-cert.pem"))))
        ;; Baseline: with a real NOW the chain verifies, so the rejection below
        ;; is attributable solely to the value of NOW.
        (is (pure-tls::verify-certificate-chain (list leaf inter root) (list root)
                                                :now now :trust-anchor-mode :replace)
            "Untampered goodcn2 chain should verify")
        ;; A NOW that is not a real must signal a certificate error naming the
        ;; argument, rather than reaching the date comparisons and failing there
        ;; with a condition that reads as a fault in the certificate.
        (signals pure-tls:tls-certificate-error
          (pure-tls::verify-certificate-chain (list leaf inter root) (list root)
                                              :now :not-a-time))))))

;;;; ---------------------------------------------------------------------------
;;;; Finding: a negative NOW is a real, so it passes a type check and still
;;;; reaches the date comparisons, where it is misread as a fact about the
;;;; certificate.
;;;;
;;;; A negative universal time is below every notBefore, so the chain is
;;;; refused with TLS-CERTIFICATE-NOT-YET-VALID.  The caller is told their
;;;; certificate is not yet valid when the certificate was never at fault, and
;;;; nothing in that condition points at the argument that caused it.  This is
;;;; the half a bare REALP check does not cover.
;;;;
;;;; Secure behaviour: NOW is required to be non-negative as well as real, so
;;;; the bad argument is named at entry instead of being reported as a property
;;;; of the chain.
;;;; ---------------------------------------------------------------------------

(test negative-verification-time-is-rejected
  "A negative NOW must be refused as a bad argument, not passed through to the
   date comparisons where it reads as a fault in the certificate."
  (let ((pure-tls:*use-windows-certificate-store* nil)
        (pure-tls:*use-macos-keychain* nil))
    (destructuring-bind (leaf inter)
        (%pem-chain (test-cert-path "openssl/goodcn2-chain.pem"))
      (let* ((root (pure-tls:parse-certificate-from-file
                    (test-cert-path "openssl/root-cert.pem")))
             (condition (handler-case
                            (progn (pure-tls::verify-certificate-chain
                                    (list leaf inter root) (list root) :now -1)
                                   nil)
                          (pure-tls:tls-certificate-error (e) e))))
        ;; A negative NOW is below every notBefore, so without the non-negative
        ;; half of the guard it reaches the date comparisons and signals
        ;; TLS-CERTIFICATE-NOT-YET-VALID, blaming the certificate for a fault in
        ;; the argument.  Asserting the condition type is what separates the two.
        (is (typep condition 'pure-tls:tls-certificate-error)
            "A negative NOW should signal")
        (is (not (typep condition 'pure-tls::tls-certificate-not-yet-valid))
            "A negative NOW should name the argument, not the certificate")))))

;;;; ---------------------------------------------------------------------------
;;;; Finding: a HOSTNAME that is neither a string nor NIL is read by the two
;;;; native dispatches and ignored on the pure-Lisp path, and it is the path
;;;; that ignores it where the call succeeds.
;;;;
;;;; HOSTNAME is read only inside the Windows and macOS native dispatches, which
;;;; hand it to a foreign string conversion (windows-verify.lisp
;;;; cffi:with-foreign-string, macos-verify.lisp %cf-string-create, whose
;;;; argument is declared :string).  A non-string is a CFFI error there.  On the
;;;; pure-Lisp path nothing ever looks at it: the chain verifies to T with no
;;;; hostname checked, and nothing in the return value distinguishes that from a
;;;; chain whose hostname really was verified.
;;;;
;;;; Secure behaviour: a HOSTNAME that is neither NIL nor a string is rejected
;;;; alongside the time, so every path agrees and the silent success becomes a
;;;; condition.
;;;; ---------------------------------------------------------------------------

(test non-string-hostname-is-rejected
  "A HOSTNAME that is neither a string nor NIL must be rejected, and a call
   supplying NIL must be unaffected."
  (let ((pure-tls:*use-windows-certificate-store* nil)
        (pure-tls:*use-macos-keychain* nil)
        (now (get-universal-time)))
    (destructuring-bind (leaf inter)
        (%pem-chain (test-cert-path "openssl/goodcn2-chain.pem"))
      (let ((root (pure-tls:parse-certificate-from-file
                   (test-cert-path "openssl/root-cert.pem"))))
        ;; Baseline: with HOSTNAME left at its NIL default the chain verifies,
        ;; so the rejection below is attributable solely to HOSTNAME.
        (is (pure-tls::verify-certificate-chain (list leaf inter root) (list root)
                                                :now now :trust-anchor-mode :replace)
            "Untampered goodcn2 chain should verify")
        ;; The pure-Lisp path never reads HOSTNAME, so without the guard this
        ;; call returns T with no hostname checked at all.
        (signals pure-tls:tls-certificate-error
          (pure-tls::verify-certificate-chain (list leaf inter root) (list root)
                                              :now now :hostname t))))))


;;;; ---------------------------------------------------------------------------
;;;; Finding: unbounded recursion in the DER parser (CWE-674).
;;;;
;;;; PARSE-DER-NODE (src/x509/asn1.lisp) recurses into PARSE-DER-CONTENTS for
;;;; every constructed node, which recurses back into PARSE-DER-NODE.  Nothing
;;;; bounded the nesting depth, so a certificate consisting of nested SEQUENCEs
;;;; drove the recursion as deep as its byte budget allowed.
;;;;
;;;; That budget was effectively unlimited: MAX-HANDSHAKE-MESSAGE-BODY-SIZE
;;;; exempts Certificate messages from *MAX-HANDSHAKE-MESSAGE-SIZE* and falls
;;;; back to the uint24 protocol maximum when *MAX-CERTIFICATE-LIST-SIZE* is 0.
;;;; Roughly 133KB of nested DER exhausted a 1MB control stack, and on SBCL that
;;;; abort is FATAL and uncatchable -- it kills the image, not the connection,
;;;; so neither PROCESS-CERTIFICATE's HANDLER-CASE nor a SERIOUS-CONDITION
;;;; handler could contain it.  Parsing happens before any signature or chain
;;;; verification, so no authentication was required to reach it.
;;;;
;;;; Secure behaviour: nesting past +ASN1-MAX-DEPTH+ must raise a graceful
;;;; TLS-DECODE-ERROR.
;;;; ---------------------------------------------------------------------------

(defun %nested-der-sequences (depth)
  "Build DER for DEPTH nested SEQUENCEs wrapped around a NULL."
  (labels ((len-bytes (n)
             (if (< n 128)
                 (list n)
                 (let ((bytes nil) (v n))
                   (loop while (plusp v)
                         do (push (ldb (byte 8 0) v) bytes)
                            (setf v (ash v -8)))
                   (cons (logior #x80 (length bytes)) bytes)))))
    (let ((payload (list #x05 #x00)))       ; innermost: NULL
      (dotimes (i depth)
        (setf payload (append (list #x30) (len-bytes (length payload)) payload)))
      (make-array (length payload)
                  :element-type '(unsigned-byte 8)
                  :initial-contents payload))))

(test der-nesting-depth-is-bounded
  "Deeply nested DER must raise TLS-DECODE-ERROR, never exhaust the stack."
  ;; Well inside the limit: still parses.
  (is (pure-tls::parse-der (%nested-der-sequences 8))
      "A shallowly nested structure must still parse")
  ;; Past the limit: refused gracefully.  Before the fix this recursed until
  ;; SBCL died with a fatal 'Control stack exhausted', taking the image with it.
  (signals pure-tls:tls-decode-error
    (pure-tls::parse-der (%nested-der-sequences (+ pure-tls::+asn1-max-depth+ 1))))
  ;; The payload that used to be lethal (~133KB, depth 30000) is now just an
  ;; error.  Reaching this form at all proves the process survived.
  (signals pure-tls:tls-decode-error
    (pure-tls::parse-der (%nested-der-sequences 30000))))

(test certificate-nesting-depth-is-bounded
  "A hostile certificate of nested SEQUENCEs must not kill the process."
  ;; PARSE-CERTIFICATE is the exported entry point a peer reaches through
  ;; PROCESS-CERTIFICATE, and is itself public API for untrusted cert files.
  (signals pure-tls:tls-error
    (pure-tls:parse-certificate (%nested-der-sequences 30000))))


;;;; ---------------------------------------------------------------------------
;;;; Finding: unbounded recursion on post-handshake records (CWE-674).
;;;;
;;;; TLS-STREAM-FILL-BUFFER (src/streams.lisp) re-entered itself once per record
;;;; for empty records, ignorable alerts and post-handshake handshake messages.
;;;; Two of those branches were metered (+MAX-EMPTY-RECORDS+, +MAX-WARNING-ALERTS+)
;;;; but the handshake branch had no counter, and the self-call sat inside a
;;;; HANDLER-CASE so SBCL could not tail-call it.  CHECK-HANDSHAKE-BUFFER-SIZE
;;;; caps only the ADVERTISED length, so a peer announced a 16384-byte body of an
;;;; unknown handshake type and dribbled it one byte per record; ~11250 records
;;;; (~259KB) exhausted a 1MB control stack, fatally and uncatchably.
;;;;
;;;; Secure behaviour: the read path is iterative, and a peer making no progress
;;;; is cut off with a TLS-ERROR after +MAX-CONSECUTIVE-HANDSHAKE-RECORDS+.
;;;; ---------------------------------------------------------------------------

(defun %plaintext-handshake-records (n &optional (payload-byte #xAA))
  "N one-byte plaintext handshake records, as a peer would put them on the wire.
The first four bytes form a handshake header announcing an unknown message type
(0x63) with a 16384-byte body, so the message never completes and every record
is pure no-progress."
  (let ((out (flexi-streams:make-in-memory-output-stream)))
    (dotimes (i n)
      (let ((byte (case i (0 #x63) (1 #x00) (2 #x40) (3 #x00) (t payload-byte))))
        ;; content_type(22) || legacy_version(0303) || length(0001) || byte
        (write-sequence (vector 22 3 3 0 1 byte) out)))
    (flexi-streams:get-output-stream-sequence out)))

(defun %stream-over-records (bytes)
  "A TLS-CLIENT-STREAM whose record layer reads BYTES (no cipher installed, so
records are consumed as plaintext).  The transport is bidirectional because the
rejection paths send a fatal alert before signalling."
  (let* ((io (make-two-way-stream
              (flexi-streams:make-in-memory-input-stream bytes)
              (flexi-streams:make-in-memory-output-stream)))
         (stream (make-instance 'pure-tls::tls-client-stream :stream io)))
    (setf (pure-tls::tls-stream-record-layer stream)
          (pure-tls::make-record-layer io))
    stream))

(test post-handshake-record-dribble-is-bounded
  "A peer dribbling a handshake message one byte per record must be cut off."
  ;; Well past the cap, and past the ~11250 records that used to be fatal.
  (let ((stream (%stream-over-records
                 (%plaintext-handshake-records 20000))))
    ;; Reaching this assertion at all proves the read path no longer grows the
    ;; control stack per record: before the fix SBCL aborted the whole image
    ;; after ~11250 records.  The flood is now consumed iteratively and
    ;; terminates on the unknown message type once the message completes.
    ;;
    ;; Note there is deliberately NO per-record cap: one-byte handshake records
    ;; are legitimate (BoringSSL's SplitHandshakeRecords tests produce exactly
    ;; that), and an earlier version of this fix rejected them.  The bound comes
    ;; from check-handshake-buffer-size capping the advertised body instead.
    (signals pure-tls:tls-error
      (pure-tls::tls-stream-fill-buffer stream))))

(test post-handshake-unknown-message-is-rejected
  "RFC 8446 s4.6: an unexpected post-handshake message must not be ignored."
  ;; A COMPLETE handshake message of an unknown type (0x63, zero-length body),
  ;; delivered in one record.  This makes progress, so the no-progress cap is
  ;; not what rejects it -- the message type is.
  (let ((stream (%stream-over-records
                 (vector 22 3 3 0 4 #x63 #x00 #x00 #x00))))
    (signals pure-tls:tls-error
      (pure-tls::tls-stream-fill-buffer stream))))


;;;; ---------------------------------------------------------------------------
;;;; Finding: *MAX-CERTIFICATE-LIST-SIZE* defaulted to 0 (unlimited).
;;;;
;;;; MAX-HANDSHAKE-MESSAGE-BODY-SIZE exempts Certificate messages from
;;;; *MAX-HANDSHAKE-MESSAGE-SIZE* and falls back to the uint24 protocol maximum
;;;; when the certificate cap is 0, so any peer could make us buffer a 16MB
;;;; Certificate message before a single signature was checked.  That budget is
;;;; also what let nested DER recurse deep enough to kill the image, and what
;;;; turned one connection into ~315 CPU-seconds of ML-DSA-65 verification.
;;;;
;;;; Secure behaviour: the shipped default bounds the message.
;;;; ---------------------------------------------------------------------------

(test certificate-list-size-has-a-bounded-default
  "The shipped default must bound a Certificate message, not allow 16MB."
  (is (plusp pure-tls:*max-certificate-list-size*)
      "*max-certificate-list-size* must not ship as 0 (unlimited)")
  ;; A Certificate message claiming the uint24 protocol maximum is refused
  ;; under the shipped default.
  (signals pure-tls:tls-handshake-error
    (pure-tls::check-handshake-buffer-size
     (%hs-header pure-tls::+handshake-certificate+ #xFFFFFF) nil))
  ;; A realistically-sized chain still passes.
  (is-false (pure-tls::check-handshake-buffer-size
             (%hs-header pure-tls::+handshake-certificate+ 65536) nil)
            "A 64KB certificate chain must still be accepted"))


;;;; ---------------------------------------------------------------------------
;;;; Finding: RSA-PSS certificate/CRL signatures could never verify.
;;;;
;;;; Two independent defects, both on the same path, which is why nobody noticed
;;;; that RSA-PSS was entirely non-functional:
;;;;
;;;;  1. src/x509/verify.lisp and src/x509/crl.lisp passed
;;;;     (ironclad:digest-sequence hash-algo tbs) to IRONCLAD:VERIFY-SIGNATURE
;;;;     with :PSS.  Ironclad's PSS-VERIFY hashes its MESSAGE argument itself
;;;;     (pkcs1.lisp: m-hash = (digest-sequence digest-name message)), so this
;;;;     verified against H(H(tbs)).  The CertificateVerify path in
;;;;     handshake/client.lisp always passed the raw content and was correct.
;;;;  2. The bare digest OIDs (2.16.840.1.101.3.4.2.x) were absent from
;;;;     *WELL-KNOWN-OIDS*, so PARSE-RSA-PSS-PARAMS could not resolve
;;;;     hashAlgorithm and handed a raw OID list to ironclad, which rejected it
;;;;     as "not a supported digest".
;;;;
;;;; Fail-closed (PSS chains were rejected, never forged), so this is a
;;;; correctness regression guard rather than an exploit guard.
;;;;
;;;; Fixtures regenerated with:
;;;;   openssl req -x509 -newkey rsa:2048 -sigopt rsa_padding_mode:pss \
;;;;     -sigopt rsa_pss_saltlen:32 -sha256 -nodes -days 36500 \
;;;;     -subj /CN=rsapss.test -keyout /dev/null -out rsa-pss-selfsigned.pem
;;;; ---------------------------------------------------------------------------

(test rsa-pss-certificate-signature-verifies
  "An RSA-PSS self-signed certificate must verify against its own key."
  (dolist (fixture '("rsa-pss-selfsigned.pem" "rsa-pss-sha384-selfsigned.pem"))
    (let ((cert (pure-tls:parse-certificate-from-file (test-cert-path fixture))))
      (is (eql :rsassa-pss (pure-tls::x509-certificate-signature-algorithm cert))
          "~A should parse as RSASSA-PSS" fixture)
      ;; hashAlgorithm must resolve to a keyword, not a raw OID list.
      (is (keywordp (getf (pure-tls::x509-certificate-signature-algorithm-params cert)
                          :hash))
          "~A hashAlgorithm must resolve to a digest keyword" fixture)
      (is-true (pure-tls::verify-certificate-signature cert cert)
               "~A self-signature must verify" fixture))))

(test rsa-pss-certificate-signature-rejects-tampering
  "The RSA-PSS path must still REJECT a bad signature (not blanket-accept)."
  (let* ((cert (pure-tls:parse-certificate-from-file
                (test-cert-path "rsa-pss-selfsigned.pem")))
         (tbs (copy-seq (pure-tls::x509-certificate-tbs-raw cert))))
    ;; Flip a byte in the signed data; the signature must no longer match.
    (setf (aref tbs (floor (length tbs) 2))
          (logxor #xFF (aref tbs (floor (length tbs) 2))))
    (setf (pure-tls::x509-certificate-tbs-raw cert) tbs)
    (is-false (pure-tls::verify-certificate-signature cert cert)
              "A tampered tbsCertificate must fail RSA-PSS verification")))


;;;; ---------------------------------------------------------------------------
;;;; Finding: an EMPTY client trust store did not fail closed under mTLS.
;;;;
;;;; PROCESS-CLIENT-CERTIFICATE-VERIFY guarded only on (unless trust-store ...),
;;;; so an empty-but-non-NIL store passed the check and reached
;;;; VERIFY-CERTIFICATE-CHAIN with NIL roots.  On macOS/Windows the native
;;;; dispatch runs BEFORE the pure-Lisp "no trusted roots" guard and treats NIL
;;;; roots as "use the OS trust store", so a server meaning to accept clients
;;;; under one private CA would instead accept any client certificate chaining
;;;; to any publicly-trusted root.
;;;;
;;;; MAKE-TRUST-STORE-FROM-DIRECTORY returns an empty store without complaint
;;;; (it only WARNs on unreadable files), so this is reachable by ordinary
;;;; misconfiguration.
;;;;
;;;; Secure behaviour: an empty trust store under +verify-required+ is a
;;;; misconfiguration and must be rejected, on every platform.
;;;; ---------------------------------------------------------------------------

(test empty-trust-store-is-not-a-usable-anchor-set
  "An empty trust store must be distinguishable from a populated one."
  ;; The guard's predicate: it is (trust-store-certificates store) that decides,
  ;; not the mere existence of the store object.
  (let ((empty (pure-tls::make-trust-store))
        (populated (pure-tls::make-trust-store
                    :certificates (list (pure-tls:parse-certificate-from-file
                                         (test-cert-path "rsa-pss-selfsigned.pem"))))))
    (is-false (pure-tls::trust-store-certificates empty)
              "A default trust store must be empty")
    (is-true (pure-tls::trust-store-certificates populated)
             "A populated trust store must report its certificates")
    ;; make-trust-store-from-directory yields an empty store rather than
    ;; erroring, which is how an empty store reaches a server in practice.
    (let ((from-empty-dir (pure-tls::make-trust-store-from-directory
                           (merge-pathnames "no-such-ca-dir/"
                                            (uiop:temporary-directory)))))
      (is-false (pure-tls::trust-store-certificates from-empty-dir)
                "A directory with no PEMs yields an empty store, not an error"))))


;;;; ---------------------------------------------------------------------------
;;;; Finding: the SNI callback never ran when a default certificate was set.
;;;;
;;;; PROCESS-CLIENT-HELLO gated the sni-callback on
;;;; (null (server-handshake-certificate-chain hs)).  That slot is populated at
;;;; construction from MAKE-TLS-SERVER-STREAM's :certificate argument, so any
;;;; server configured with BOTH a default certificate and an :sni-callback --
;;;; the ordinary virtual-hosting setup -- never invoked the callback and served
;;;; the default certificate to every vhost.  The callback's :reject return
;;;; (which sends unrecognized_name) never fired either.  The intended test was
;;;; "did the certificate-provider answer during THIS handshake".
;;;;
;;;; Not a vulnerability (certificates are public, and a client's own hostname
;;;; verification rejects the wrong one) but a silent failure of a configured
;;;; dispatch mechanism.
;;;; ---------------------------------------------------------------------------

(test sni-callback-runs-when-default-certificate-configured
  "An :sni-callback must be consulted even when :certificate supplies a default."
  (let* ((cert-path (test-cert-path "openssl/server-ed25519-cert.pem"))
         (key-path (test-cert-path "openssl/server-ed25519-key.pem"))
         (port (+ 21000 (random 1000)))
         (callback-hostname (list :never-called))
         (server-result (list nil))
         (ready-lock (bt:make-lock "sni-ready"))
         (ready-cv (bt:make-condition-variable :name "sni-ready-cv"))
         (ready-flag (list nil)))
    (bt:make-thread
     (lambda ()
       (let ((server-socket nil) (client-socket nil))
         (unwind-protect
              (handler-case
                  (progn
                    (setf server-socket
                          (usocket:socket-listen "127.0.0.1" port
                                                 :reuse-address t
                                                 :element-type '(unsigned-byte 8)))
                    (bt:with-lock-held (ready-lock)
                      (setf (first ready-flag) t)
                      (bt:condition-notify ready-cv))
                    (setf client-socket
                          (usocket:socket-accept server-socket
                                                 :element-type '(unsigned-byte 8)))
                    (let ((tls-stream
                            (pure-tls:make-tls-server-stream
                             (usocket:socket-stream client-socket)
                             ;; BOTH a default certificate AND an sni-callback.
                             :certificate (namestring cert-path)
                             :key (namestring key-path)
                             :sni-callback
                             (lambda (hostname)
                               (setf (first callback-hostname) hostname)
                               ;; Returning NIL means "use the default", so the
                               ;; handshake still completes and the test is
                               ;; about the callback being REACHED.
                               nil))))
                      (close tls-stream)
                      (setf (first server-result) :success)))
                (error (e) (setf (first server-result) (format nil "~A" e))))
           (when client-socket (ignore-errors (usocket:socket-close client-socket)))
           (when server-socket (ignore-errors (usocket:socket-close server-socket))))))
     :name "sni-callback-test-server")
    (bt:with-lock-held (ready-lock)
      (loop until (first ready-flag)
            do (bt:condition-wait ready-cv ready-lock :timeout 5)))
    (sleep 0.05)
    (let ((client-socket nil))
      (unwind-protect
           (handler-case
               (progn
                 (setf client-socket
                       (usocket:socket-connect "127.0.0.1" port
                                               :element-type '(unsigned-byte 8)))
                 (let ((tls-stream (pure-tls:make-tls-client-stream
                                    (usocket:socket-stream client-socket)
                                    :sni-hostname "vhost.example"
                                    :verify pure-tls:+verify-none+)))
                   (close tls-stream)
                   (sleep 0.2)
                   (is (equal "vhost.example" (first callback-hostname))
                       "sni-callback must be called with the client's SNI, got ~S"
                       (first callback-hostname))))
             (error (e) (fail "Client connection failed: ~A" e)))
        (when client-socket (ignore-errors (usocket:socket-close client-socket)))))))


;;;; ---------------------------------------------------------------------------
;;;; Finding: VERIFY-DEPTH was accepted, stored, and never enforced.
;;;;
;;;; MAKE-TLS-CONTEXT took :verify-depth and stored it in the context struct
;;;; (and cl+ssl:make-context forwarded it), but VERIFY-CERTIFICATE-CHAIN never
;;;; read it and no chain-length limit existed anywhere.  An operator setting
;;;; :verify-depth 3 believed something was enforcing it.
;;;;
;;;; Bounding chain LENGTH matters independently of the byte-size cap: every
;;;; additional link costs one signature verification, and those run BEFORE the
;;;; trust-anchor check, so a long chain of self-crafted certificates is
;;;; attacker-directed CPU work regardless of whether it can ever anchor.
;;;; ---------------------------------------------------------------------------

(test verify-depth-limits-chain-length
  "A chain longer than the configured depth must be rejected."
  (let ((cert (pure-tls:parse-certificate-from-file
               (test-cert-path "rsa-pss-selfsigned.pem")))
        (root (pure-tls:parse-certificate-from-file
               (test-cert-path "rsa-pss-selfsigned.pem"))))
    ;; The chain contents do not matter: the length check runs before any
    ;; signature verification, which is the whole point.
    (let ((long-chain (make-list 12 :initial-element cert)))
      (signals pure-tls:tls-certificate-error
        (pure-tls::verify-certificate-chain long-chain (list root)
                                            :max-depth 5))
      ;; Within the limit it gets past the length check and fails later, on
      ;; the actual chain contents -- a different error path.
      (handler-case
          (progn (pure-tls::verify-certificate-chain (list cert) (list root)
                                                     :max-depth 5)
                 (pass "short chain reached normal verification"))
        (pure-tls:tls-certificate-error (e)
          (is (not (search "chain too long" (princ-to-string e)))
              "A chain within the limit must not be rejected for length"))
        (pure-tls:tls-error () (pass "short chain reached normal verification"))))
    ;; The default comes from *default-verify-depth*, which the stream
    ;; constructors bind from the context's verify-depth.
    (is (plusp pure-tls::*default-verify-depth*)
        "*default-verify-depth* must have a positive default")
    (let ((pure-tls::*default-verify-depth* 2))
      (signals pure-tls:tls-certificate-error
        (pure-tls::verify-certificate-chain
         (make-list 3 :initial-element cert) (list root))))))


;;;; ---------------------------------------------------------------------------
;;;; DER strictness fixes (adjudicated secscan leads, 2026-09-17).
;;;; ---------------------------------------------------------------------------

(test der-child-may-not-overrun-parent
  "A nested DER element must stay inside its parent's declared length."
  ;; Outer SEQUENCE declares 2 bytes of content, but the child NULL is 2 bytes
  ;; of header plus nothing -- fine.  Then declare 2 but supply a 4-byte child.
  (let ((ok (make-array 4 :element-type '(unsigned-byte 8)
                          :initial-contents '(#x30 #x02 #x05 #x00)))
        (overrun (make-array 6 :element-type '(unsigned-byte 8)
                               ;; SEQUENCE len 2, but contains OCTET STRING len 2
                               ;; (4 bytes total) -- the child runs past the parent
                               :initial-contents '(#x30 #x02 #x04 #x02 #xAA #xBB))))
    (is (pure-tls::parse-der ok) "A well-formed nested structure must parse")
    (signals pure-tls:tls-decode-error (pure-tls::parse-der overrun))))

(test der-oid-decoding-is-strict-and-complete
  "OID decoding must handle multi-byte first subidentifiers and reject padding."
  ;; 2.5.29.19 (basicConstraints) = 55 1D 13
  (is (equal '(2 5 29 19) (pure-tls::decode-der-oid
                           (make-array 3 :element-type '(unsigned-byte 8)
                                         :initial-contents '(#x55 #x1D #x13))))
      "canonical OID must decode")
  ;; A non-minimal subidentifier (leading 0x80) is invalid DER and used to
  ;; decode to the same OID, giving one OID several encodings.
  (signals pure-tls:tls-decode-error
    (pure-tls::decode-der-oid (make-array 4 :element-type '(unsigned-byte 8)
                                            :initial-contents '(#x55 #x80 #x1D #x13))))
  ;; A trailing continuation byte leaves a subidentifier unterminated.
  (signals pure-tls:tls-decode-error
    (pure-tls::decode-der-oid (make-array 2 :element-type '(unsigned-byte 8)
                                            :initial-contents '(#x55 #x81))))
  ;; Multi-byte FIRST subidentifier: 2.100.3 encodes as 81 34 03
  ;; (first subid = 40*2 + 100 = 180 = 0x81 0x34).  Reading only bytes[0]
  ;; mis-decoded every OID of this shape.
  (is (equal '(2 100 3) (pure-tls::decode-der-oid
                         (make-array 3 :element-type '(unsigned-byte 8)
                                       :initial-contents '(#x81 #x34 #x03))))
      "multi-byte first subidentifier must decode correctly"))

(test uint16-extension-lists-reject-odd-length
  "An odd-length uint16 list must raise a TLS error, not a raw bounds error."
  ;; supported_groups: 2-byte list length, then an odd number of bytes.
  (signals pure-tls:tls-decode-error
    (pure-tls::decode-uint16-list
     (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(0 29 0))
     "supported_groups"))
  (is (equal '(29 23) (pure-tls::decode-uint16-list
                       (make-array 4 :element-type '(unsigned-byte 8)
                                     :initial-contents '(0 29 0 23))
                       "supported_groups"))
      "an even-length list must still decode"))

(defun run-security-regression-tests ()
  "Run the security regression suite.  Returns T if all tests pass."
  (format t "~&=== Running pure-tls Security Regression Tests ===~%~%")
  (run! 'security-regression-tests))
