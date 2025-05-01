(in-package :iolib.examples)

;;;; This file was originally written by Peter Keller (psilord@cs.wisc.edu)
;;;; and this code is released under the same license as IOLib.

;;;; This example is almost the same as ex1-client.lisp, except we move it
;;;; closer to a Common Lisp style.

;; ex-0b
(defun run-ex2-client (&key (host *host*) (port *port*))

  ;; We introduce with-open-socket here as a means to easily wrap
  ;; usually synchronous and blocking communication with a form that
  ;; ensures the socket is closed no matter how we exit it.
  (with-open-socket
      (socket :connect :active
              :address-family :internet
              :type :stream
              :external-format '(:utf-8 :eol-style :crlf)
              :ipv6 nil)

    ;; Do a blocking connect to the daytime server on the port.  We
    ;; also introduce lookup-hostname, which converts a hostname to an
    ;; 4 values, but in our case we only want the first, which is an
    ;; address.
    (connect socket (lookup-hostname host) :port port :wait t)
    (format t "Connected to server ~A:~A from my local connection at ~A:~A!~%"
            (remote-name socket) (remote-port socket)
            (local-name socket) (local-port socket))

    ;; read the one line of information I need from the daytime
    ;; server.  I can use read-line here because this is a TCP
    ;; socket. It will block until the whole line is read.
    (let ((line (read-line socket)))
      (format t "~A" line)
      t)))
;; ex-0e
