(in-package :iolib.examples)

;;;; This file was originally written by Peter Keller (psilord@cs.wisc.edu)
;;;; and this code is released under the same license as IOLib.

;;;; This programs differs from ex6-server in that an explicit
;;;; io-buffer is created and used to separate the read handler from
;;;; the write handler.  Also this server is aware if the client
;;;; closesd the write end of its socket to the server, but still
;;;; expects to be reading inflight data in the server In ex6-server,
;;;; if the read was ready to the client, but not the write, we would
;;;; block. In this server, the read would happen, and we could move
;;;; onto another client of the first one would have blocked while
;;;; writing the response. This client is slightly more "ready for
;;;; I/O" friendly, but all i/o in in fact blocking.

;; This holds any open connections to clients as keys in the
;; table. The values is a list containing the host and port of the
;; connection. We use this to close all connections to the clients, if
;; any, when the server exits.  This allows all clients to notice the
;; server had gone away.
(defvar *ex7-open-connections*)

;; This program uses the multiplexer to select on a listening socket
;; which when it becomes ready in the read bit, will do a blocking
;; accept, and then create a closure which acts as a buffer between
;; reading and writing the client data back and forth to the
;; client. The reads use send-to and recv-from reading and writing
;; only what is necessary to write.

;; This makes a closure which contains a buffer filled by recv-from
;; and sent be send-to. When the buffer is empty, the write handler is
;; removed.

(defun run-ex7-server-helper (port)
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

    ;; Set up the initial listener handler for any incoming clients
    ;; What kind of error checking do I need to do here?
    (set-io-handler *ex7-event-base*
                    (socket-os-fd server)
                    :read
                    (make-ex7-server-listener-handler server))

    ;; keep accepting connections forever.
    (handler-case
        (event-dispatch *ex7-event-base*)

      (socket-connection-reset-error ()
        (format t "Caught unexpected connection reset by peer!~%"))

      (hangup ()
        (format t "Caught unexpected hangup! Cilent closed connection on write!"))
      (end-of-file ()
        (format t "Caught unexpected end-of-file! Client closed connection on read!")))))

;; ex-0b
;; Create the listener closure which accepts the client and registers the
;; buffer functions with it.
(defun make-ex7-server-listener-handler (socket)
  (lambda (fd event exception)
    ;; do a blocking accept, returning nil if no socket
    (let* ((client (accept-connection socket :wait t)))
      (when client
        (multiple-value-bind (who port)
            (remote-name client)
          (format t "Accepted a client from ~A:~A~%" who port)

          ;; save the client connection in case we need to close it later.
          (setf (gethash `(,who ,port) *ex7-open-connections*) client)

          ;; We make an io-buffer, which takes care of reading from the
          ;; socket and echoing the information it read back onto the
          ;; socket.  The buffer takes care of this with two internal
          ;; handlers, a read handler and a write handler.
          (let ((io-buffer
                 (make-ex7-io-buffer client who port
                                     (make-ex7-server-disconnector client))))

            ;; set up an line echo function for the client socket.  The
            ;; internals of the buffer will perform the appropriate
            ;; registration/unregistration of the required handlers at
            ;; the right time depending upon data availability.

            (set-io-handler *ex7-event-base*
                            (socket-os-fd client)
                            :read
                            (funcall io-buffer :read-a-line))

            (set-io-handler *ex7-event-base*
                            (socket-os-fd client)
                            :write
                            (funcall io-buffer :write-a-line))))))))
;; ex-0e

;; ex-1b
(defun make-ex7-server-disconnector (socket)
  ;; When this function is called, it can be told which callback to remove, if
  ;; no callbacks are specified, all of them are removed! The socket can be
  ;; additionally told to be closed.
  (lambda (who port &rest events)
    (let ((fd (socket-os-fd socket)))
      (if (not (intersection '(:read :write :error) events))
          (remove-fd-handlers *ex7-event-base* fd :read t :write t :error t)
          (progn
            (when (member :read events)
              (remove-fd-handlers *ex7-event-base* fd :read t))
            (when (member :write events)
              (remove-fd-handlers *ex7-event-base* fd :write t))
            (when (member :error events)
              (remove-fd-handlers *ex7-event-base* fd :error t)))))
    ;; and finally if were asked to close the socket, we do so here
    (when (member :close events)
      (format t "Closing connection to ~A:~A~%" who port)
      (finish-output)
      (close socket)
      (remhash `(,who ,port) *ex7-open-connections*))))
;; ex-1e

;; If for whatever reason the server has to stop running, we ensure that all
;; open connections are closed.
(defun run-ex7-server (&key (port *port*))
  (let ((*ex7-open-connections* nil)
        (*ex7-event-base* nil))
    (unwind-protect
         (handler-case
             (progn
               (setf *ex7-open-connections* (make-hash-table :test #'equalp)
                     *ex7-event-base* (make-instance 'event-base))

               (run-ex7-server-helper port))

           ;; handle some common signals
           (socket-address-in-use-error ()
             (format t "Bind: Address already in use, forget :reuse-addr t?")))

      ;; Cleanup form for uw-p
      ;; Close all open connections to the clients, if any. We do this because
      ;; when the server goes away we want the clients to know immediately. Sockets
      ;; are not memory, and can't just be garbage collected whenever. They have
      ;; to be eagerly closed.
      (maphash
       #'(lambda (k v)
           (format t "Force closing a client connection to ~A~%" k)
           (close v :abort t))
       *ex7-open-connections*)

      ;; and clean up the event-base too!
      (when *ex7-event-base*
        (close *ex7-event-base*))
      (format t "Server Exited.~%")
      (finish-output))))
