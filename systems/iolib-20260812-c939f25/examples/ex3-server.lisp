(in-package :iolib.examples)

;;;; This file was originally written by Peter Keller (psilord@cs.wisc.edu)
;;;; and this code is released under the same license as IOLib.

;;;; This is a more common-lisp-like style of ex2-server, and to be
;;;; used for the rest of the examples as appropriate. We introduce
;;;; with-open-socket, which does a lot of cleanup on the created
;;;; socket and ensures it is closed. This is usually the recommended
;;;; idiom for simple clients.

;;;; Also in this example we start to handle some of the more common
;;;; conditions which can be signaled by IOLib.

;; ex-0b
(defun run-ex3-server-helper (port)
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

    ;; start listening on the server socket
    (listen-on server :backlog 5)
    (format t "Listening on socket bound to: ~A:~A~%"
            (local-host server)
            (local-port server))
    ;; ex-0e

    ;; ex-1b
    ;; keep accepting connections forever.
    (loop
       (format t "Waiting to accept a connection...~%")

       ;; Here we see with-accept-connection which simplifies closing
       ;; the client socket when are done with it.
       (with-accept-connection (client server :wait t)
         ;; When we get a new connection, show who it
         ;; is from.
         (multiple-value-bind (who rport)
             (remote-name client)
           (format t "Got a connnection from ~A:~A!~%" who rport))

         ;; Since we're using an internet TCP stream, we can use format
         ;; with it. However, we should be sure to finish-output in
         ;; order that all the data is sent.
         (multiple-value-bind (s m h d mon y)
             (get-decoded-time)
           (format t "Sending the time...")

           ;; Catch the condition of the client closing the connection.
           ;; Since we exist inside a with-accept-connection, the
           ;; socket will be automatically closed.
           (handler-case
               (progn
                 (format client "~A/~A/~A ~A:~A:~A~%" mon d y h m s)
                 (finish-output client))

             (socket-connection-reset-error ()
               (format t "Client reset connection!~%"))

             (hangup ()
               (format t "Client closed conection!~%")))

           (format t "Sent!~%"))))
    ;; ex-1e

    ;; ex-2b
    t))
;; ex-2e

;; ex-3b
;; This is the main entry point into the example 3 server.
(defun run-ex3-server (&key (port *port*))
  (handler-case

      (run-ex3-server-helper port)

    (socket-address-in-use-error ()
      ;; Here we catch a condition which represents trying to bind to
      ;; the same port before the first one has been released by the
      ;; kernel.  Generally this means you forgot to put ':reuse-addr
      ;; t' as an argument to bind address.
      (format t "Bind: Address already in use, forget :reuse-addr t?")))

  (finish-output))
;; ex-3e