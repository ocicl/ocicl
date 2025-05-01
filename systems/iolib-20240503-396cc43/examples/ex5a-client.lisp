(in-package :iolib.examples)

;;;; This file was originally written by Peter Keller (psilord@cs.wisc.edu)
;;;; and this code is released under the same license as IOLib.

;;;; This example uses the event-dispatcher to know when there is data
;;;; ready for reading and writing to the server. However, the actual
;;;; reads and writes are nonblocking i/o. We don't use
;;;; with-open-socket here since the disconnector function associated
;;;; with each handler may close the socket and we wouldn't want to
;;;; close it again in a manner which might signal yet another
;;;; condition--which is what the cleanup form of with-open-socket
;;;; will do.

;; ex-0b
;; This will be an instance of the multiplexer.
(defvar *ex5a-event-base*)
;; ex-0e

;; in batch mode, this will cut the connection to the server when end of input
;; is found, which kills the inflight data to the server. Problematic.

;; ex-1b
(defun run-ex5a-client-helper (host port)
  ;; Create a internet TCP socket under IPV4
  ;; We specifically do not use with-open-socket here since that form is
  ;; more suited for synchronous i/o on one socket. Since we do not use that
  ;; form, it is up to the handlers to decide to remove and close the socket
  ;; when the connection to the server should be closed.
  (let ((socket (make-socket :connect :active
                             :address-family :internet
                             :type :stream
                             :external-format '(:utf-8 :eol-style :crlf)
                             :ipv6 nil)))
    ;; ex-1e

    ;; Don't use with-open-socket here since it is the responsibility
    ;; of the io handlers to close the active socket. If the socket
    ;; were to be closed in the io handler, then with-open-socket also
    ;; closed it, another condition will be signaled when
    ;; with-open-socket tries to finish the output on the socket.
    ;; ex-2b
    (unwind-protect
         (progn
           ;; do a blocking connect to the echo server on the port.
           (connect socket (lookup-hostname host) :port port :wait t)

           (format t "Connected to server ~A:~A from my local connection at ~A:~A!~%"
                   (remote-host socket) (remote-port socket)
                   (local-host socket) (local-port socket))

           ;; set up the handlers for read and write
           (set-io-handler *ex5a-event-base*
                           (socket-os-fd socket)
                           :read
                           (make-ex5a-str-cli-read
                            socket
                            (make-ex5a-client-disconnector socket)))

           (set-io-handler *ex5a-event-base*
                           (socket-os-fd socket)
                           :write
                           (make-ex5a-str-cli-write
                            socket
                            (make-ex5a-client-disconnector socket)))

           (handler-case
               ;; keep processing input and output on the fd by
               ;; calling the relevant handlers as the socket becomes
               ;; ready. The relevant handlers will take care of
               ;; closing the socket at appropriate times.
               (event-dispatch *ex5a-event-base*)

             ;; We'll notify the user of the client if a handler missed
             ;; catching common conditions.
             (hangup ()
               (format t "Uncaught hangup. Server closed connection on write!%"))
             (end-of-file ()
               (format t "Uncaught end-of-file. Server closed connection on read!%"))))
      ;; ex-2e

      ;; ex-3b

      ;; Cleanup expression for uw-p.
      ;; Try to clean up if the client aborted badly and left the socket open.
      ;; It is safe to call close mutiple times on a socket.
      ;; However, we don't want to finish-output on the socket since that
      ;; might signal another condition since the io handler already closed
      ;; the socket.
      (format t "Client safely closing open socket to server.~%")
      (close socket :abort t))))
;; ex-3e

;; ex-4b
(defun make-ex5a-str-cli-write (socket disconnector)
  ;; When this next function gets called it is because the event dispatcher
  ;; knows the socket to the server is writable.
  (lambda (fd event exception)
    ;; Get a line from stdin, and send it to the server
    (handler-case
        (let ((line (read-line)))
          (format socket "~A~%" line)
          (finish-output socket))

      (end-of-file ()
        (format t "make-ex5a-str-cli-write: User performed end-of-file!~%")
        (funcall disconnector :close))

      (hangup ()
        (format t
                "make-ex5a-str-cli-write: server closed connection on write!~%")
        (funcall disconnector :close)))))
;; ex-4e

;; ex-5b
(defun make-ex5a-str-cli-read (socket disconnector)
  ;; When this next function gets called it is because the event dispatcher
  ;; knows the socket from the server is readable.
  (lambda (fd event exception)
    ;; get a line from the server, and send it to *standard-output*
    (handler-case
        ;; If we send "quit" to the server, it will close its connection to
        ;; us and we'll notice that with an end-of-file.
        (let ((line (read-line socket)))
          (format t "~A~%" line)
          (finish-output))

      (end-of-file ()
        (format t "make-ex5a-str-cli-read: server closed connection on read!~%")
        (funcall disconnector :close)))))
;; ex-5e

;; ex-6b
(defun make-ex5a-client-disconnector (socket)
  ;; When this function is called, it can be told which callback to remove, if
  ;; no callbacks are specified, all of them are removed! The socket can be
  ;; additionally told to be closed.
  (lambda (&rest events)
    (format t "Disconnecting socket: ~A~%" socket)
    (let ((fd (socket-os-fd socket)))
      (if (not (intersection '(:read :write :error) events))
          (remove-fd-handlers *ex5a-event-base* fd :read t :write t :error t)
          (progn
            (when (member :read events)
              (remove-fd-handlers *ex5a-event-base* fd :read t))
            (when (member :write events)
              (remove-fd-handlers *ex5a-event-base* fd :write t))
            (when (member :error events)
              (remove-fd-handlers *ex5a-event-base* fd :error t)))))
    ;; and finally if were asked to close the socket, we do so here
    (when (member :close events)
      (close socket :abort t))))
;; ex-6e

;; ex-7b
;; This is the entry point for this example.
(defun run-ex5a-client (&key (host *host*) (port *port*))
  (let ((*ex5a-event-base* nil))
    (unwind-protect
         (progn
           ;; When the connection gets closed, either intentionally in the client
           ;; or because the server went away, we want to leave the multiplexer
           ;; event loop. So, when making the event-base, we explicitly state
           ;; that we'd like that behavior.
           (setf *ex5a-event-base*
                 (make-instance 'iomux:event-base :exit-when-empty t))
           (handler-case
               (run-ex5a-client-helper host port)

             ;; handle a commonly signaled error...
             (socket-connection-refused-error ()
               (format t "Connection refused to ~A:~A. Maybe the server isn't running?~%"
                       (lookup-hostname host) port))))
      ;; ex-7e

      ;; ex-8b
      ;; Cleanup form for uw-p
      ;; ensure we clean up the event base regardless of how we left the client
      ;; algorithm
      (when *ex5a-event-base*
        (close *ex5a-event-base*))
      (format t "Client Exited.~%")
      (finish-output))))
;; ex-8e



