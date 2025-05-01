(in-package :iolib.examples)

;;;; This file was originally written by Peter Keller (psilord@cs.wisc.edu)
;;;; and this code is released under the same license as IOLib.

;;;; This buffer generator creates a closure which holds an array that can
;;;; hold up to a maximum number of bytes read from the socket. The bytes are
;;;; not inspected and simply stored in an array. If there are bytes to write
;;;; out from the array the write handler is registered and they are written.
;;;; In this manner does the written data "follow" the read data. When the
;;;; end of the buffer is reach via a write, we start over at the beginning
;;;; again.

;;;; This io-buffer reads and writes in a nonblocking fashion on the
;;;; socket and stores partial reads and writes in an internal buffer.

;; The event dispatcher
(defvar *ex8-event-base*)

;; ex-0b
(defun make-ex8-io-buffer (socket who port disconnector &key (max-bytes 16384))
  (let ((echo-buf (make-array max-bytes :element-type 'unsigned-byte))
        (read-index 0)
        (write-index 0)
        (read-handler-registered nil)
        (write-handler-registered nil)
        (eof-seen nil))
    ;; ex-0e
    ;; ex-1b
    (labels
        ;; This is the function responsible for reading bytes from the client.
        ((read-some-bytes (fd event exception)
           (handler-case
               (progn
                 ;; Read however much we are able.
                 (multiple-value-bind (buf bytes-read)
                     (receive-from socket
                                   :buffer echo-buf
                                   :start read-index
                                   :end max-bytes)

                   ;; Unlike read-ing from a stream, receive-from
                   ;; returns zero on an end-of-file read, so we turn
                   ;; around and signal that condition so our
                   ;; handler-case can deal with it properly like our
                   ;; other examples.
                   (when (zerop bytes-read)
                     (error 'end-of-file))

                   (format t "Read ~A bytes from ~A:~A~%" bytes-read who port)
                   (incf read-index bytes-read))

                 ;; Register the write handler if there is data to
                 ;; write.
                 ;;
                 ;; Then, try to write some data to the socket right
                 ;; away even though it might not be ready simply to
                 ;; avoid another go around. The write-some-bytes
                 ;; function must be able to catch econnreset because
                 ;; this connection may be closed at the time of this
                 ;; call. Normally, if the multiplexer has told me I
                 ;; could write then it'd be ok, but since this write
                 ;; is outside of the multiplexer and an optimization,
                 ;; it needs to check.
                 (when (/= write-index read-index)
                   (unless write-handler-registered
                     (set-io-handler *ex8-event-base*
                                     (socket-os-fd socket)
                                     :write
                                     #'write-some-bytes)
                     (setf write-handler-registered t))

                   ;; See if I can write it right away!
                   (write-some-bytes fd :write nil))

                 ;; If I'm out of room to store more data then remove
                 ;; myself from the io handler. When the write handler
                 ;; notices that it has finished writing everything,
                 ;; all indicies get set back to zero and the write
                 ;; handler removes itself.  If write-some-bytes in
                 ;; the call above worked, then read-index might not
                 ;; equal max-bytes when this line of code gets
                 ;; executed.
                 (when (= read-index max-bytes)
                   (funcall disconnector who port :read)
                   (setf read-handler-registered nil)))

             (socket-connection-reset-error ()
               ;; Handle the client sending a reset.
               (format t "Client ~A:~A: connection reset by peer.~%" who port)
               (funcall disconnector who port :close))

             (end-of-file ()
               ;; When we get an end of file, that doesn't necessarily
               ;; mean the client went away, it could just mean that
               ;; the client performed a shutdown on the write end of
               ;; its socket and it is expecting the data stored in
               ;; the server to be written to it.  However, if there
               ;; is nothing left to write and our read end is closed,
               ;; we shall consider it that the client went away and
               ;; close the connection.
               (format t "Client ~A:~A produced end-of-file on a read.~%"
                       who port)
               (if (= read-index write-index)
                   (funcall disconnector who port :close)
                   (progn
                     (funcall disconnector who port :read)
                     (setf read-handler-registered nil)
                     (setf eof-seen t))))))
         ;; ex-1e

         ;; ex-2b
         ;; This is the function responsible for writing bytes to the client.
         (write-some-bytes (fd event exception)
           (handler-case
               (progn
                 ;; If there is data to be written, write it.  NOTE:
                 ;; There is often no indication of failure to write
                 ;; with send-to. If I'm writing to a closed (by the
                 ;; client) socket, it could be that send-to tells me
                 ;; nothing is wrong and returns the number of bytes
                 ;; wrotten. In this case, nothing was written but we
                 ;; have no way of knowing. Usually in this case, the
                 ;; read handler will get a 0 bytes read on the socket
                 ;; and we can know the connection is broken.
                 (when (> read-index write-index)
                   (let ((wrote-bytes (send-to socket echo-buf
                                               :start write-index
                                               :end read-index)))
                     (format t "Wrote ~A bytes to ~A:~A~%" wrote-bytes who port)
                     (incf write-index wrote-bytes)))

                 ;; If we see we're out of data to write and we saw an eof,
                 ;; then close the connection, we're done. If we didn't see an
                 ;; eof, then unregister the write handler and reregister the
                 ;; read handler to get more data. If the buffer indices
                 ;; are at the very end, reset them to the beginning.
                 (when (= read-index write-index)
                   (if eof-seen
                       (funcall disconnector who port :close)
                       (progn

                         ;; nothing more to write, so unregister writer
                         (funcall disconnector who port :write)
                         (setf write-handler-registered nil)

                         ;; If we're at the end of the buffer, move to the
                         ;; beginning so there is more room for data.
                         (when (= read-index write-index max-bytes)
                           (setf read-index 0
                                 write-index 0))

                         ;; Reregister the read handler to get more data
                         (unless read-handler-registered
                           (set-io-handler *ex8-event-base*
                                           (socket-os-fd socket)
                                           :read
                                           #'read-some-bytes)
                           (setf read-handler-registered t))))))

             (socket-connection-reset-error ()
               ;; If for somer eaon the client reset the network connection,
               ;; we'll get this signal.
               (format t "Client ~A:~A: connection reset by peer.~%" who port)
               (funcall disconnector who port :close))

             (isys:ewouldblock ()
               ;; Sometimes this happens on a write even though it
               ;; might have been marked as ready. Also we might have
               ;; asked to write on an unknown status socket. Ignore
               ;; it and we will try again later.
               (format t "write-some-bytes: ewouldblock~%")
               nil)

             (isys:epipe ()
               ;; In this server, if the client doesn't accept data,
               ;; it also means it will never send us data again. So
               ;; close the connection for good.
               (format t "Client ~A:~A got hangup on write.~%" who port)
               (funcall disconnector who port :close)))))
      ;; ex-2e

      ;; ex-3b
      ;; This is the function returned from make-ex8-io-buffer which
      ;; allows us access to the read/writer in the scope of the
      ;; closure.  We will ask for the correct functions when setting
      ;; up the io handlers.  NOTE: By simply asking for the handler,
      ;; I've assumed it is to be immediately put into an iolib event
      ;; handler. This is why they are considered registered at this
      ;; point.
      (lambda (msg)
        (cond
          ((equalp msg :read-some-bytes)
           (setf read-handler-registered t)
           #'read-some-bytes)
          ((equalp msg :write-some-bytes)
           (setf write-handler-registered t)
           #'write-some-bytes)
          (t
           (error "make-ex8-buffer: Please supply :read-some-bytes or :write-some-bytes~%")))))))

;; ex-3e

