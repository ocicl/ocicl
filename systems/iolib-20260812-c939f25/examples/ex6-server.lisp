(in-package :iolib.examples)

;;;; This file was originally written by Peter Keller (psilord@cs.wisc.edu)
;;;; and this code is released under the same license as IOLib.

;;;; This program uses the multiplexer to select on a listening socket
;;;; which when it becomes ready in the read bit, will do a blocking
;;;; accept, and then register a callback then when it is ready will
;;;; do a blocking read and then immediately a blocking write back to
;;;; the same socket.  This is not multithreaded and in fact you
;;;; probably don't want to mix threading and the multiplexor together
;;;; with respect to reading/writing to the network sockets.

;; ex-0b
;; This variable represents the multiplexer state.
(defvar *ex6-server-event-base*)
;; ex-0e

;; ex-1b
;; This holds any open connections to clients as keys in the table. The values
;; is a list containing the host and port of the connection. We use this to
;; close all connections to the clients, if any, when the server exits.  This
;; allows all clients to notice the server had gone away.
(defvar *ex6-server-open-connections*)
;; ex-1e

;; ex-2b
;; Set up the server and server clients with the multiplexer
(defun run-ex6-server-helper (port)

  ;; We don't use with-open-socket here since we may need to have a
  ;; finer control over when we close the server socket.
  (let ((server (make-socket :connect :passive
                             :address-family :internet
                             :type :stream
                             :ipv6 nil
                             :external-format '(:utf-8 :eol-style :crlf))))
    (unwind-protect
         (progn
           (format t "Created socket: ~A[fd=~A]~%" server (socket-os-fd server))
           ;; Bind the socket to all interfaces with specified port.
           (bind-address server +ipv4-unspecified+ :port port :reuse-addr t)
           (format t "Bound socket: ~A~%" server)

           ;; start listening on the server socket
           (listen-on server :backlog 5)
           (format t "Listening on socket bound to: ~A:~A~%"
                   (local-host server)
                   (local-port server))
           ;; ex-2e

           ;; ex-3b
           ;; Set up the initial listener handler for any incoming clients
           (set-io-handler *ex6-server-event-base*
                           (socket-os-fd server)
                           :read
                           (make-ex6-server-listener-handler server))

           ;; keep accepting connections forever.
           (handler-case
               (event-dispatch *ex6-server-event-base*)

             ;; Just in case any handler misses these conditions, we
             ;; catch them here.
             (socket-connection-reset-error ()
               (format t "~A~A~%"
                       "Caught unexpected reset by peer! "
                       "Client connection reset by peer!"))
             (hangup ()
               (format t "~A~A~%"
                       "Caught unexpected hangup! "
                       "Client closed connection on write!"))
             (end-of-file ()
               (format t "~A~A~%"
                       "Caught unexpected end-of-file! "
                       "Client closed connection on read!"))))
      ;; ex-3e

      ;; ex-4b
      ;; Cleanup expression for uw-p.
      ;; Ensure the server socket is closed, regardless of how we left
      ;; the server.
      (close server))))
;; ex-4e

;; ex-5b
;; When the multiplexer states the server socket is ready for reading
;; it means that we have a client ready to accept. So we accept it and
;; then register the accepted client socket back into the multiplexer
;; with the appropritate echo protocol function.
(defun make-ex6-server-listener-handler (socket)
  (lambda (fd event exception)

    ;; do a blocking accept, returning nil if no socket
    (let* ((client (accept-connection socket :wait t)))
      (when client
        (multiple-value-bind (who port)
            (remote-name client)
          (format t "Accepted a client from ~A:~A~%" who port)

          ;; save the client connection in case we need to close it later
          ;; when the server exits.
          (setf (gethash `(,who ,port) *ex6-server-open-connections*) client)

          ;; set up an line echo function for the client socket.
          (set-io-handler *ex6-server-event-base*
                          (socket-os-fd client)
                          :read
                          (make-ex6-server-line-echoer
                           client
                           who
                           port
                           (make-ex6-server-disconnector client))))))))
;; ex-5e

;; ex-6b
;; This function returns a function that reads a line, then
;; echoes it right back onto the socket it came from. This is blocking
;; i/o.  This code can suffer denial of service attacks like on page
;; 167 of "Unix Network Programming 2nd Edition: Sockets and XTI", by
;; Richard Stevens.
(defun make-ex6-server-line-echoer (socket who port disconnector)
  (format t "Creating line-echoer for ~A:~A~%" who port)
  (lambda (fd event exception)
    (handler-case
        (let ((line (read-line socket))) ;; read a line from the client
          (format t "Read ~A:~A: ~A~%" who port line)
          (format socket "~A~%" line) ;; write it the client
          (finish-output socket)
          (format t "Wrote ~A:~A: ~A~%" who port line)

          ;; close the connection to the client if it asked to quit
          (when (string= line "quit")
            (format t "Client requested quit!~%")
            (funcall disconnector who port)))

      (socket-connection-reset-error ()
        ;; Handle the usual and common conditions we'll see while
        ;; talking to a client
        (format t "Client's connection was reset by peer.~%")
        (funcall disconnector who port))

      (hangup ()
        (format t "Client went away on a write.~%")
        (funcall disconnector who port))

      (end-of-file ()
        (format t "Client went away on a read.~%")
        (funcall disconnector who port)))))
;; ex-6e

;; ex-7b
;; If we decide we need to disconnect ourselves from the client, this will
;; remove all the handlers and remove the record of our connection from
;; *ex6-server-open-connections*.
(defun make-ex6-server-disconnector (socket)
  (lambda (who port)
    (format t "Closing connection to ~A:~A~%" who port)
    (remove-fd-handlers *ex6-server-event-base* (socket-os-fd socket))
    (close socket)
    (remhash `(,who ,port) *ex6-server-open-connections*)))
;; ex-7e

;; ex-8b
;; This is the entrance function into this example.
(defun run-ex6-server (&key (port *port*))
  (let ((*ex6-server-open-connections* nil)
        (*ex6-server-event-base* nil))
    (unwind-protect
         (handler-case
             (progn
               ;; Clear the open connection table and init the event base
               (setf *ex6-server-open-connections*
                     (make-hash-table :test #'equalp)

                     *ex6-server-event-base*
                     (make-instance 'event-base))

               (run-ex6-server-helper port))

           ;; handle a common signal
           (socket-address-in-use-error ()
             (format t "Bind: Address already in use, forget :reuse-addr t?")))
      ;; ex-8e

      ;; ex-9b
      ;; Cleanup form for uw-p
      ;; Close all open connections to the clients, if any. We do this
      ;; because when the server goes away we want the clients to know
      ;; immediately. Sockets are not memory, and can't just be garbage
      ;; collected whenever. They have to be eagerly closed.
      (maphash
       #'(lambda (k v)
           (format t "Closing a client connection to ~A~%" k)
           ;; We don't want to signal any conditions on the close...
           (close v :abort t))
       *ex6-server-open-connections*)

      ;; and clean up the multiplexer too!
      (when *ex6-server-event-base*
        (close *ex6-server-event-base*))
      (format t "Server Exited~%")
      (finish-output))))
;; ex-9e
