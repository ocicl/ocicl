(in-package :iolib.examples)

;;;; This file was originally written by Peter Keller (psilord@cs.wisc.edu)
;;;; and this code is released under the same license as IOLib.

;;;; Building on ex4-server.lisp, this follows a similar design except
;;;; it is a threaded line echo server. Each thread handles one client and
;;;; simply echos the lines the client send back. Each thread performs
;;;; blocking i/o, but multiple threads can function at the same time.

;; ex-0b
;; The special variable used to hold the client socket for the thread
;; managing it.
(defvar *ex5-tls-client* nil)
;; ex-0e

;; Set up a server and handle the connections.
;; ex-1b
(defun run-ex5-server-helper (port)
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
    ;; keep accepting connections forever, but if this exits for whatever
    ;; reason ensure to destroy any remaining running threads.
    (unwind-protect
         (loop
            (format t "Waiting to accept a connection...~%")
            (finish-output)
            (let* ((client (accept-connection server :wait t))
                   ;; set up the special variable to store the client
                   ;; we accepted...
                   (*default-special-bindings*
                    (acons '*ex5-tls-client* client
                           *default-special-bindings*)))

              ;; ...and handle the connection!
              (when client
                (make-thread #'process-ex5-client-thread
                             :name 'process-ex5-client-thread))))
      ;; ex-2e

      ;; ex-3b
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
                           (string-equal "process-ex5-client-thread"
                                         (thread-name thr)))
                  (format t "Destroying: ~A~%" thr)
                  ;; Ignore any conditions which might arise if a
                  ;; thread happened to finish in the race between
                  ;; liveness testing and destroying.
                  (ignore-errors
                    (destroy-thread thr))))
            (all-threads)))))
;; ex-3e

;; ex-4b
;; The thread which handles the client connection.
(defun process-ex5-client-thread ()
  ;; declared ignorable because this dynamic variable is bound outside
  ;; of the context of this function.
  (declare (ignorable *ex5-tls-client*))
  ;; no matter how we get out of the client processing loop, we always
  ;; close the connection.
  (unwind-protect
       (multiple-value-bind (who port)
           (remote-name *ex5-tls-client*)
         (format t "A thread is handling the connection from ~A:~A!~%"
                 who port)

         (handler-case
             ;;  perform the actual echoing algorithm
             (str-ex5-echo *ex5-tls-client* who port)

           (socket-connection-reset-error ()
             (format t "Client ~A:~A: connection reset by peer.~%"
                     who port))

           (end-of-file ()
             (format t "Client ~A:~A closed connection for a read.~%"
                     who port)
             t)

           (hangup ()
             (format t "Client ~A:~A closed connection for a write.~%"
                     who port)
             t)))

    ;; cleanup form of the unwind-protect
    ;; We always close the connection to the client, even if this
    ;; thread gets destroyed (at least in SBCL this cleanup form gets
    ;; run when this thread is destroyed).
    (format t "Closing connection to ~A:~A!~%"
            (remote-host *ex5-tls-client*) (remote-port *ex5-tls-client*))
    (close *ex5-tls-client*)
    t))
;; ex-4e

;; ex-5b
;; The actual function which speaks to the client.
(defun str-ex5-echo (client who port)
  ;; here we let signaled conditions on the boundary conditions of the
  ;; client (meaning it closes its connection to us on either a read or
  ;; a write) bail us out of the infinite loop
  (let ((done nil))
    (loop until done
       do
       (let ((line (read-line client)))
         (format t "Read line from ~A:~A: ~A~%" who port line)
         (format client "~A~%" line)
         (finish-output client)
         (format t "Wrote line to ~A:~A: ~A~%" who port line)

         ;; Exit the thread when the user requests it with 'quit'.
         ;; This forces a close to the client socket.
         (when (string= line "quit")
           (setf done t))
         t))))
;; ex-5e

;; ex-6b
;; This just checks for some error conditions so we can print out a nice
;; message about it.
(defun run-ex5-server (&key (port *port*))
  (handler-case

      (run-ex5-server-helper port)

    ;; handle some common conditions
    (socket-address-in-use-error ()
      (format t "Bind: Address already in use, forget :reuse-addr t?")))

  (finish-output))
;; ex-6e


