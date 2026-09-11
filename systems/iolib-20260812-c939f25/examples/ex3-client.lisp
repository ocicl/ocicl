(in-package :iolib.examples)

;;;; This file was originally written by Peter Keller (psilord@cs.wisc.edu)
;;;; and this code is released under the same license as IOLib.

;;;; This example is similar to ex2-client.lisp, except we've added in
;;;; catching of various conditions which may be signaled during the
;;;; network communication.

;; ex-0b
(defun run-ex3-client-helper (host port)

  ;; Create a internet TCP socket under IPV4
  (with-open-socket
      (socket :connect :active
              :address-family :internet
              :type :stream
              :external-format '(:utf-8 :eol-style :crlf)
              :ipv6 nil)

    ;; do a blocking connect to the daytime server on the port.
    (connect socket (lookup-hostname host) :port port :wait t)
    (format t "Connected to server ~A:~A from my local connection at ~A:~A!~%"
            (remote-name socket) (remote-port socket)
            (local-name socket) (local-port socket))

    (handler-case
        ;; read the one line of information I need from the daytime
        ;; server.  I can use read-line here because this is a TCP
        ;; socket. It will block until the whole line is read.
        (let ((line (read-line socket)))
          (format t "~A" line)
          t)

      ;; However, let's notice the signaled condition if the server
      ;; went away prematurely...
      (end-of-file ()
        (format t "Got end-of-file. Server closed connection!")))))
;; ex-0e

;; ex-1b
;; The main entry point into ex3-client
(defun run-ex3-client (&key (host *host*) (port *port*))
  (handler-case

      (run-ex3-client-helper host port)

    ;; handle a commonly signaled error...
    (socket-connection-refused-error ()
      (format t "Connection refused to ~A:~A. Maybe the server isn't running?~%"
              (lookup-hostname host) port))))
;; ex-1e
