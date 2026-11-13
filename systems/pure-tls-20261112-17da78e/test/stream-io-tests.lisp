;;; test/stream-io-tests.lisp --- Application-data stream I/O tests
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; In-process (loopback) tests for TLS application-data read behaviour.
;;; Regression coverage for GitHub #32: STREAM-READ-SEQUENCE used to return a
;;; short read as soon as the underlying socket was momentarily drained, so a
;;; dexador Content-Length body read into a fixed buffer came back with a
;;; zero-filled tail.  The method must instead block until the range is full
;;; or end of file, per the gray-streams contract.

(in-package #:pure-tls/test)

(def-suite stream-io-tests
  :description "TLS application-data stream read/write behaviour")

(in-suite stream-io-tests)

(defun stream-io-test-pattern (n)
  "A vector of N bytes, each in 1..254 so a zero anywhere marks an unwritten
(NUL-padded) byte."
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n v)
      (setf (aref v i) (1+ (mod i 254))))))

(defun run-dribble-read (&key (total 48000) (chunk 1600) (pause 0.01)
                              (request total))
  "Stand up a loopback TLS server that writes TOTAL bytes in CHUNK-sized
pieces, pausing PAUSE seconds between them so the client's socket drains, and
read them back with a single READ-SEQUENCE into a REQUEST-byte buffer.

Returns (values N RBUF PAYLOAD SERVER-RESULT): N is READ-SEQUENCE's return
value, RBUF the client's buffer, PAYLOAD the TOTAL bytes the server sent, and
SERVER-RESULT :SUCCESS or an error string."
  (let* ((cert-path (merge-pathnames "test/certs/openssl/server-ed25519-cert.pem"
                                     (asdf:system-source-directory :pure-tls)))
         (key-path (merge-pathnames "test/certs/openssl/server-ed25519-key.pem"
                                    (asdf:system-source-directory :pure-tls)))
         (payload (stream-io-test-pattern total))
         (port (+ 23000 (random 4000)))
         (server-result (list nil))
         (ready-lock (bt:make-lock "dribble-server-ready"))
         (ready-cv (bt:make-condition-variable :name "dribble-server-ready-cv"))
         (ready-flag (list nil))
         (server-thread nil))
    (setf server-thread
          (bt:make-thread
           (lambda ()
             (let ((server-socket nil)
                   (client-socket nil))
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
                          (let ((tls (pure-tls:make-tls-server-stream
                                      (usocket:socket-stream client-socket)
                                      :certificate cert-path
                                      :key key-path)))
                            ;; Dribble the body out, one flushed record per
                            ;; chunk with a gap between, then close cleanly.
                            (loop for off from 0 below total by chunk
                                  do (write-sequence payload tls
                                                     :start off
                                                     :end (min total (+ off chunk)))
                                     (force-output tls)
                                     (sleep pause))
                            (close tls)
                            (setf (first server-result) :success)))
                      (error (e)
                        (setf (first server-result) (format nil "~A" e))))
                 (when client-socket (ignore-errors (usocket:socket-close client-socket)))
                 (when server-socket (ignore-errors (usocket:socket-close server-socket))))))
           :name "dribble-test-server"))
    (bt:with-lock-held (ready-lock)
      (loop until (first ready-flag)
            do (bt:condition-wait ready-cv ready-lock :timeout 5)))
    (sleep 0.05)
    (let ((client-socket nil))
      (unwind-protect
           (progn
             (setf client-socket
                   (usocket:socket-connect "127.0.0.1" port
                                           :element-type '(unsigned-byte 8)))
             (let ((tls (pure-tls:make-tls-client-stream
                         (usocket:socket-stream client-socket)
                         :sni-hostname "dribble-test"
                         :verify pure-tls:+verify-none+))
                   (rbuf (make-array request :element-type '(unsigned-byte 8)
                                             :initial-element 0)))
               (let ((n (read-sequence rbuf tls)))
                 (ignore-errors (close tls))
                 (when server-thread
                   (ignore-errors (bt:join-thread server-thread)))
                 (values n rbuf payload (first server-result)))))
        (when client-socket (ignore-errors (usocket:socket-close client-socket)))))))

(test read-sequence-fills-dribbled-body
  "READ-SEQUENCE returns the whole Content-Length-style body, with no short
read and no NUL-padded tail, even when it arrives in paused pieces (GitHub #32)."
  (multiple-value-bind (n rbuf payload server-result)
      (run-dribble-read :total 48000)
    (is (eql :success server-result)
        "server failed to send the body: ~A" server-result)
    (is (= n 48000)
        "READ-SEQUENCE returned ~D of 48000 bytes (short read before EOF)" n)
    (is (zerop (count 0 rbuf))
        "~D of 48000 bytes came back NUL (unwritten tail)" (count 0 rbuf))
    (is (equalp rbuf payload)
        "the bytes read do not match the bytes sent")))

(test read-sequence-short-only-at-eof
  "A short READ-SEQUENCE return means end of file and nothing less: asking for
more than the server sends returns exactly what was sent, intact."
  (multiple-value-bind (n rbuf payload server-result)
      (run-dribble-read :total 48000 :request (+ 48000 4096))
    (is (eql :success server-result)
        "server failed to send the body: ~A" server-result)
    (is (= n 48000)
        "READ-SEQUENCE should stop at EOF after 48000 bytes, got ~D" n)
    (is (equalp (subseq rbuf 0 48000) payload)
        "the bytes read up to EOF do not match the bytes sent")))
