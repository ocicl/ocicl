(in-package :iolib.examples)

;;;; This file was originally written by Peter Keller (psilord@cs.wisc.edu)
;;;; and this code is released under the same license as IOLib.

;;;; This is a concurrent daytime server made with threads instead of
;;;; forked processes.  The special variable *ex4-tlss-client* is a
;;;; global variable which is rebound to be the client socket in the
;;;; context of the newly created thread.

;; ex-0b
;; This variable is the means by which we transmit the client socket from
;; the initial thread to the particular thread which will handle that client.
(defvar *ex4-tls-client* nil)
;; ex-0e

;; ex-1b
(defun run-ex4-server-helper (port)
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
    ;; ex-1e

    ;; ex-2b
    ;; Here we introduce unwind-protect to ensure we properly clean up
    ;; any leftover threads when the server exits for whatever reason.
    ;; keep accepting connections forever, but if this exits for
    ;; whatever reason ensure to destroy any remaining running
    ;; threads.
    (unwind-protect
         (loop                         ; keep accepting connections...
            (format t "Waiting to accept a connection...~%")
            (finish-output)
            (let* ((client (accept-connection server :wait t))
                   ;; set up the special variable according to the
                   ;; needs of the Bordeaux Threads package to pass in
                   ;; the client socket we accepted to the about to be
                   ;; created thread.  *default-special-bindings* must
                   ;; not be modified, so here we just push a new scope
                   ;; onto it.
                   (*default-special-bindings*
                    (acons '*ex4-tls-client* client
                           *default-special-bindings*)))

              ;; ...and handle the connection!
              (when client
                (make-thread #'process-ex4-client-thread
                             :name 'process-ex4-client-thread))))

      ;; Clean up form for uw-p.
      ;; Clean up all of the client threads when done.
      ;; This code is here for the benefit of the REPL because it is
      ;; intended that this tutorial be worked interactively. In a real
      ;; threaded server, the server would just exit--destroying the
      ;; server process, and causing all threads to exit which then notifies
      ;; the clients.
      (format t "Destroying any active client threads....~%")
      (mapc #'(lambda (thr)
                (when (and (thread-alive-p thr)
                           (string-equal "process-ex4-client-thread"
                                         (thread-name thr)))
                  (format t "Destroying: ~A~%" thr)
                  ;; Ignore any conditions which might arise if a
                  ;; thread happened to finish in the race between
                  ;; liveness testing and destroying.
                  (ignore-errors
                    (destroy-thread thr))))
            (all-threads)))))
;; ex-2e

;; ex-3b
;;; The thread which handles the client connection.
(defun process-ex4-client-thread ()
  ;; This variable is set outside of the context of this thread.
  (declare (ignorable *ex4-tls-client*))
  ;; ex-3e
  ;; ex-4b
  ;; We ensure the client socket is always closed!
  (unwind-protect
       (multiple-value-bind (who port)
           (remote-name *ex4-tls-client*)
         (format t "A thread is handling the connection from ~A:~A!~%"
                 who port)

         ;; Prepare the time and send it to the client.
         (multiple-value-bind (s m h d mon y)
             (get-decoded-time)
           (handler-case
               (progn
                 (format t "Sending the time to ~A:~A..." who port)
                 (format *ex4-tls-client*
                         "~A/~A/~A ~A:~A:~A~%"
                         mon d y h m s)
                 (finish-output *ex4-tls-client*)
                 (format t "Sent!~%"))

             (socket-connection-reset-error ()
               (format t "Client ~A:~A reset the connection!~%" who port))

             (hangup ()
               (format t "Client ~A:~A closed connection.~%" who port)))))

    ;; Cleanup form for uw-p.
    (format t "Closing connection to ~A:~A!~%"
            (remote-host *ex4-tls-client*) (remote-port *ex4-tls-client*))
    (close *ex4-tls-client*)))
;; ex-4e


;; ex-5b
;; The entry point into this example.
(defun run-ex4-server (&key (port *port*))
  (handler-case

      (run-ex4-server-helper port)

    ;; handle some common signals
    (socket-address-in-use-error ()
      (format t "Bind: Address already in use, forget :reuse-addr t?")))

  (finish-output))
;; ex-5e


