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
      ;; (now and hostname are positional &optional args before the &key.)
      (signals pure-tls:tls-certificate-error
        (pure-tls::verify-certificate-chain (list leaf) (list root)
                                            (get-universal-time) nil
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
                                            now nil :trust-anchor-mode :replace)))
    ;; Intermediate carries no BasicConstraints extension at all.
    (let ((leaf (%chain-cert "leaf.example" "Intermediate CA"
                             :basic-constraints :absent))
          (inter (%chain-cert "Intermediate CA" "Root CA"
                              :basic-constraints :absent)))
      (signals pure-tls:tls-certificate-error
        (pure-tls::verify-certificate-chain (list leaf inter) (list inter)
                                            now nil :trust-anchor-mode :replace)))))

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
                                                now nil :trust-anchor-mode :replace)
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
                                              now nil :trust-anchor-mode :replace))))))

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
                                                now nil :trust-anchor-mode :replace)
            "Untampered goodcn2 chain should verify")
        ;; Flip one byte of the leaf signature.  Every earlier check still
        ;; passes, so a rejection can only come from signature verification.
        (let ((sig (copy-seq (pure-tls::x509-certificate-signature leaf))))
          (setf (aref sig 20) (logxor #xff (aref sig 20)))
          (setf (pure-tls::x509-certificate-signature leaf) sig))
        (signals pure-tls:tls-certificate-error
          (pure-tls::verify-certificate-chain (list leaf inter root) (list root)
                                              now nil :trust-anchor-mode :replace))))))

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
                                            now nil :trust-anchor-mode :replace)))))

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
                                            now nil :trust-anchor-mode :replace)))))

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
                                            now nil :trust-anchor-mode :replace)))))

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

(defun run-security-regression-tests ()
  "Run the security regression suite.  Returns T if all tests pass."
  (format t "~&=== Running pure-tls Security Regression Tests ===~%~%")
  (run! 'security-regression-tests))
