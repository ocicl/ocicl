(in-package :iolib.examples)

;;;; This file was originally written by Peter Keller (psilord@cs.wisc.edu)
;;;; and this code is released under the same license as IOLib.

;;;; This next example is a more common-lisp-like style, and when
;;;; appropriate it will be used for the rest of the examples.

;;;; It implements an IPV4 blocking i/o iterative server which serves
;;;; clients sequentially forever. There is no error handling of
;;;; client boundary conditions such as a client connection but then
;;;; immediately closing the connection.  Handling errors will be in
;;;; later examples.

(defun run-ex2-server (&key (port *port*))

  ;; This is an appropriate use of with-open-socket since we are
  ;; synchronously and iteratively handling client connections.
  (with-open-socket
      (server :connect :passive
              :address-family :internet
              :type :stream
              :ipv6 nil
              :external-format '(:utf-8 :eol-style :crlf))
    (format t "Created socket: ~A[fd=~A]~%" server (socket-os-fd server))

    ;; Bind the socket to all interfaces with specified port.
    (bind-address server +ipv4-unspecified+ :port port :reuse-addr t)
    (format t "Bound socket: ~A~%" server)

    ;; Start listening on the server socket
    (listen-on server :backlog 5)
    (format t "Listening on socket bound to: ~A:~A~%"
            (local-host server)
            (local-port server))

    ;; ex-0b
    ;; Keep accepting connections forever.
    (loop
       (format t "Waiting to accept a connection...~%")

       ;; Using with-accept-connection, when this form returns it will
       ;; automatically close the client connection.
       (with-accept-connection (client server :wait t)
         ;; When we get a new connection, show who it is from.
         (multiple-value-bind (who rport)
             (remote-name client)
           (format t "Got a connnection from ~A:~A!~%" who rport))

         ;; Since we're using a internet TCP stream, we can use format
         ;; with it. However, we should be sure to finish-output in
         ;; order that all the data is sent.
         (multiple-value-bind (s m h d mon y)
             (get-decoded-time)
           (format t "Sending the time...")
           (format client "~A/~A/~A ~A:~A:~A~%" mon d y h m s)
           (finish-output client)
           (format t "Sent!~%")
           (finish-output)
           t)))))
;; ex-0e

