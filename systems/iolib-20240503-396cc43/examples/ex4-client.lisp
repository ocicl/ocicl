(in-package :iolib.examples)

;;;; This file was originally written by Peter Keller (psilord@cs.wisc.edu)
;;;; and this code is released under the same license as IOLib.

;;;; This program is a very simple echo client. After connecting to
;;;; the server it reads a line from the console, echos it to the
;;;; server, reads the response back, then echos it to
;;;; *standard-output*. We handle common conditions. Type "quit" on a
;;;; line by itself to exit the client.

;; ex-0b
(defun run-ex4-client-helper (host port)

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
            (remote-host socket) (remote-port socket)
            (local-host socket) (local-port socket))

    (handler-case
        (ex4-str-cli socket)

      (socket-connection-reset-error ()
        (format t "Got connection reset. Server went away!"))

      (hangup ()
        (format t "Got hangup. Server closed connection on write!~%"))

      (end-of-file ()
        (format t "Got end-of-file. Server closed connection on read!~%")))))
;; ex-0e

;; ex-1b
;; read a line from stdin, write it to the server, read the response, write
;; it to stdout. If we read 'quit' then echo it to the server which will
;; echo it back to us and then close its connection to us.
(defun ex4-str-cli (socket)
  (loop
     (let ((line (read-line)))
       ;; send it to the server, get the response.
       (format socket "~A~%" line)
       (finish-output socket)
       (format t "~A~%" (read-line socket)))))
;; ex-1e

;; ex-2b
;; This is the entry point into this example
(defun run-ex4-client (&key (host *host*) (port *port*))
  (unwind-protect
       (handler-case

           (run-ex4-client-helper host port)

         ;; handle a commonly signaled error...
         (socket-connection-refused-error ()
           (format t "Connection refused to ~A:~A. Maybe the server isn't running?~%"
                   (lookup-hostname host) port)))

    ;; Cleanup form
    (format t "Client Exited.~%")))
;; ex-2e