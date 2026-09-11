(in-package :iolib.examples)

;;;; This file was originally written by Peter Keller (psilord@cs.wisc.edu)
;;;; and this code is released under the same license as IOLib.

;;;; The entry call to this example is: (run-ex1-client) It can take
;;;; two keyword arguements of :host STRING and :port INTEGER.

;;;; This example implements a very simple IPV4 TCP blocking i/o
;;;; client which talks to a date server. After connecting to the date
;;;; server, a single line is sent from the server to the cilent and
;;;; then the client disconnects.

;;;; We don't handle many errors and this code is written from a
;;;; C-style perspective that we will avoid where possible in future
;;;; examples.

;; ex-0b
(defun run-ex1-client (&key (host *host*) (port *port*))
  ;; ex-0e

  ;; ex-1b
  ;; Create a internet TCP socket under IPV4
  (let ((socket
         (make-socket
          :connect :active
          :address-family :internet
          :type :stream
          :external-format '(:utf-8 :eol-style :crlf)
          :ipv6 nil)))
    ;; ex-1e

    ;; ex-2b
    ;; do a blocking connect to the daytime server on the port.
    (connect socket (lookup-hostname host) :port port :wait t)
    (format t "Connected to server ~A:~A via my local connection at ~A:~A!~%"
            (remote-host socket) (remote-port socket)
            (local-host socket) (local-port socket))
    ;; ex-2e

    ;; ex-3b
    ;; read the one line of information I need from the daytime
    ;; server.  I can use read-line here because this is a TCP socket.
    (let ((line (read-line socket)))
      (format t "~A" line))
    ;; ex-3e

    ;; ex-4b
    ;; all done
    (close socket)
    t))
;; ex-4e
