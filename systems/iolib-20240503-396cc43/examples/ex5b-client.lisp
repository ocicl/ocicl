(in-package :iolib.examples)

;;;; This file was originally written by Peter Keller (psilord@cs.wisc.edu)
;;;; and this code is released under the same license as IOLib.

(defvar *ex5b-event-base*)

;; This program differs from ex5a-client in the fact that when the end-of-file
;; is reached for the input, only the write end of the socket is shutdown, this
;; allows inflight data to reach the server.

(defun run-ex5b-client-helper (host port)

  ;; Create a internet TCP socket under IPV4
  (let ((socket (make-socket :connect :active
                             :address-family :internet
                             :type :stream
                             :external-format '(:utf-8 :eol-style :crlf)
                             :ipv6 nil)))

    ;; I don't use with-open-socket here since it is the responsibility of the
    ;; io handlers to close the active socket. If the socket were to be closed
    ;; in the io handler, then with-open-socket also closed it, another
    ;; condition will be signaled when with-open-socket tries to finish the
    ;; output on the socket.
    (unwind-protect
         (progn
           ;; do a blocking connect to the echo server on the port.
           (connect socket (lookup-hostname host) :port port :wait t)

           (format t "Connected to server ~A:~A from my local connection at ~A:~A!~%"
                   (remote-name socket) (remote-port socket)
                   (local-name socket) (local-port socket))

           ;; set up the handlers for read and write
           (set-io-handler *ex5b-event-base*
                           (socket-os-fd socket)
                           :read
                           (make-ex5b-str-cli-read
                            socket
                            (make-ex5b-client-disconnector socket)))

           (set-io-handler *ex5b-event-base*
                           (socket-os-fd socket)
                           :write
                           (make-ex5b-str-cli-write
                            socket
                            (make-ex5b-client-disconnector socket)))

           (handler-case
               ;; keep processing input and output on the fd by calling
               ;; the relevant handlers as the socket becomes ready.
               (event-dispatch *ex5b-event-base*)

             ;; We'll notify the user of the client here something happened,
             ;; and let the always run expression for the uw-p form take
             ;; care of closing the socket in case it was left open.
             (hangup ()
               (format t
                       "Uncaught hangup. Server closed connection on write!!~%"))
             (end-of-file ()
               (format t "Uncaught end-of-file. Server closed connection on read!!~%"))))

      ;; The always run expression from uw-p.
      ;; Try to clean up if the client aborted badly and left the socket open.
      ;; It is safe to close an already closed socket
      (format t "Client safely closing open socket to server.~%")
      (close socket :abort t))))

;; ex-0b
(defun make-ex5b-str-cli-write (socket disconnector)
  ;; When this next function gets called it is because the event dispatcher
  ;; knows the socket to the server is writable.
  (lambda (fd event exception)
    ;; Get a line from stdin, and send it to the server
    (handler-case
        (let ((line (read-line)))
          (format socket "~A~%" line)
          (finish-output socket))

      (end-of-file ()
        (format t
                "make-ex5b-str-cli-write: User performed end-of-file!~%")
        ;; Shutdown the write end of my pipe to give the inflight data the
        ;; ability to reach the server!
        (format t
                "make-ex5b-str-cli-write: Shutting down write end of socket!~%")
        (shutdown socket :write t)
        ;; since we've shut down the write end of the pipe, remove this handler
        ;; so we can't read more data from *standard-input* and try to write it
        ;; it to the server.
        (funcall disconnector :write))

      (hangup ()
        (format t
                "make-ex5b-str-cli-write: server closed connection on write!~%")
        (funcall disconnector :close)))))
;; ex-0e

(defun make-ex5b-str-cli-read (socket disconnector)
  ;; When this next function gets called it is because the event dispatcher
  ;; knows the socket from the server is readable.
  (lambda (fd event exception)
    ;; get a line from the server, and send it to *standard-output*
    (handler-case
        ;; If we send "quit" to the server, it will close its connection to
        ;; us and we'll notice that with an end-of-file.
        (let ((line (read-line socket)))
          (format t "~A~%" line))

      (end-of-file ()
        (format t "make-ex5b-str-cli-read: server closed connection on read!~%")
        ;; If the server closed the connection I'm reading from, then we
        ;; definitely can't write any more to it either, so remove all
        ;; handlers for this socket and close everything.
        (funcall disconnector :close)))))

(defun make-ex5b-client-disconnector (socket)
  ;; When this function is called, it can be told which callback to remove, if
  ;; no callbacks are specified, all of them are removed! The socket can be
  ;; additionally told to be closed.
  (lambda (&rest events)
    (format t "Disconnecting socket: ~A~%" socket)
    (let ((fd (socket-os-fd socket)))
      (if (not (intersection '(:read :write :error) events))
          (remove-fd-handlers *ex5b-event-base* fd :read t :write t :error t)
          (progn
            (when (member :read events)
              (remove-fd-handlers *ex5b-event-base* fd :read t))
            (when (member :write events)
              (remove-fd-handlers *ex5b-event-base* fd :write t))
            (when (member :error events)
              (remove-fd-handlers *ex5b-event-base* fd :error t)))))
    ;; and finally if were asked to close the socket, we do so here
    (when (member :close events)
      (close socket))))

(defun run-ex5b-client (&key (host *host*) (port *port*))
  (let ((*ex5b-event-base* nil))
    (unwind-protect
         (progn
           ;; When the connection gets closed, either intentionally in the client
           ;; or because the server went away, we want to leave the multiplexer
           ;; event loop. So, when making the event-base, we explicitly state
           ;; that we'd like that behavior.
           (setf *ex5b-event-base*
                 (make-instance 'iomux:event-base :exit-when-empty t))

           (handler-case

               (run-ex5b-client-helper host port)

             ;; handle a commonly signaled error...
             (socket-connection-refused-error ()
               (format t
                       "Connection refused to ~A:~A. Maybe the server isn't running?~%"
                       (lookup-hostname "localhost") port))))

      ;; ensure we clean up the event base regardless of how we left the client
      ;; algorithm
      (when *ex5b-event-base*
        (close *ex5b-event-base*))
      (format t "Client Exited.~%"))))




