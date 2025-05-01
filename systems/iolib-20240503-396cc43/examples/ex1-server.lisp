(in-package :iolib.examples)

;;;; This file was originally written by Peter Keller (psilord@cs.wisc.edu)
;;;; and this code is released under the same license as IOLib.

;;;; This example implements an IPV4 TCP blocking i/o iterative date
;;;; server which handles one connection and then exits.

;;;; This example is written in a non-conventional style resembling
;;;; that of C. Later programs will deviate from this style and move
;;;; more towards the Common Lisp style.

;;;; This server as it stands has more than a few problems especially
;;;; due to not cleaning up the server socket if the code exits
;;;; poorly, suppose returning to the toplevel when you break into the
;;;; debugger while the server is running.

;; ex-0b
(defun run-ex1-server (&key (port *port*))
  ;; Create a passive (server) TCP socket under IPV4 Sockets meant to
  ;; be listened upon *must* be created passively. This is a minor
  ;; deviation from the Berkeley socket interface.
  (let ((socket
         (make-socket
          :connect :passive
          :address-family :internet
          :type :stream
          :external-format '(:utf-8 :eol-style :crlf)
          :ipv6 nil)))
    (format t "Created socket: ~A[fd=~A]~%" socket (socket-os-fd socket))
    ;; ex-0e

    ;; ex-1b
    ;; Bind the socket to all interfaces with specified port.
    (bind-address socket
                  +ipv4-unspecified+ ; which means INADDR_ANY or 0.0.0.0
                  :port port
                  :reuse-addr t)
    (format t "Bound socket: ~A~%" socket)
    ;; ex-1e

    ;; ex-2b
    ;; Convert the sockxet to a listening socket
    (listen-on socket :backlog 5)
    (format t "Listening on socket bound to: ~A:~A~%"
            (local-host socket) (local-port socket))
    ;; ex-2e

    ;; ex-3b
    ;; Block on accepting a connection
    (format t "Waiting to accept a connection...~%")
    (let ((client (accept-connection socket :wait t)))
      (when client
        ;; When we get a new connection, show who it is from.
        (multiple-value-bind (who rport)
            (remote-name client)
          (format t "Got a connection from ~A:~A!~%" who rport))
        ;; ex-3e

        ;; ex-4b
        ;; Since we're using a internet TCP stream, we can use format
        ;; with it. However, we should be sure to call finish-output on
        ;; the socket in order that all the data is sent. Also, this is
        ;; a blocking write.
        (multiple-value-bind (s m h d mon y)
            (get-decoded-time)
          (format t "Sending the time...")
          (format client "~A/~A/~A ~A:~A:~A~%" mon d y h m s)
          (finish-output client))
        ;; ex-4e

        ;; ex-5b
        ;; We're done talking to the client.
        (close client)
        (format t "Sent!~%"))
      ;; ex-5e

      ;; ex-6b
      ;; We're done with the server socket too.
      (close socket)
      (finish-output)
      t)))
;; ex-6e

