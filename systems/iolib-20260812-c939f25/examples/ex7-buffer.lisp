(in-package :iolib.examples)

;;;; This file was originally written by Peter Keller (psilord@cs.wisc.edu)
;;;; and this code is released under the same license as IOLib.

;;;; This function returns a closure from which I can ask for a
;;;; reader/writer function that the event-dispatcher can use.  When
;;;; constructing the buffer, which consists of a queue of lines, I
;;;; specify the socket upon which the buffer is responsible and how
;;;; bytes I'm willing to buffer. Obviously, if a line is very much
;;;; bigger than the max-bytes I have, I have to read the whole thing
;;;; due to read-line and the blocking i/o requirement.

;; The event dispatcher
(defvar *ex7-event-base*)

;; ex-0b
(defun make-ex7-io-buffer (socket who port disconnector &key (max-bytes 4096))
  (let ((line-queue (make-queue))
        (bytes-left-to-write 0)
        (read-handler-registered nil)
        (write-handler-registered nil)
        (eof-seen nil))
    ;; ex-0e
    ;; ex-1b
    (labels
        ;; If this function notices that there is data to write, it will
        ;; set the io-handler on the socket for the write handler.
        ;; If the function notices it has read >= than the max-bytes
        ;; it will remove itself from the handler *after* ensuring the
        ;; write handler is set up properly.
        ((read-a-line (fd event exception)
           (handler-case
               (let ((line (format nil "~A~%" (read-line socket)))) ; add a \n
                 (format t "Read from ~A:~A: ~A" who port line)
                 (enqueue line line-queue)
                 (incf bytes-left-to-write (length line))

                 (when (> bytes-left-to-write 0)
                   ;; If the write handler isn't registered, then do
                   ;; it now since I have data to write.
                   (unless write-handler-registered
                     (set-io-handler *ex7-event-base*
                                     (socket-os-fd socket)
                                     :write
                                     #'write-a-line)
                     (setf write-handler-registered t)))

                 ;; Now, if there is more data than I should be
                 ;; reading, remove myself from the io handler. When
                 ;; the write handler notices that, after writing some
                 ;; data, more of it can be read, it will reregister
                 ;; the io handler for the read socket.
                 (when (>= bytes-left-to-write max-bytes)
                   (funcall disconnector who port :read)
                   (setf read-handler-registered nil)))

             (socket-connection-reset-error ()
               ;; If the client resets its connection, we close
               ;; everything down.
               (format t "Client ~A:~A: Connection reset by peer~%" who port)
               (funcall disconnector who port :close))

             (end-of-file ()
               ;; When we get an end of file, that doesn't necessarily
               ;; mean the client went away, it could just mean that
               ;; the client performed a shutdown on the write end of
               ;; its socket and it is expecting the data stored in
               ;; the server to be written to it.  However, if there
               ;; is nothing left to write and our read end is close,
               ;; we shall consider it that the client went away and
               ;; close the connection.
               (format t "Client ~A:~A produced end-of-file on a read.~%"
                       who port)
               (if (zerop bytes-left-to-write)
                   (funcall disconnector who port :close)
                   (progn
                     (funcall disconnector who port :read)
                     (setf read-handler-registered nil)
                     (setf eof-seen t))))))
         ;; ex-1e

         ;; ex-2b
         ;; This function will notice that if it has written enough bytes to
         ;; bring the bytes-left-to-write under max-bytes, it will re-register
         ;; the reader io handler. If there is no data to write, it will,
         ;; after ensuring the read handler is registered, unregister itself
         ;; as to not be called constantly on a write ready socket with no
         ;; data to write.
         (write-a-line (fd event exception)
           (handler-case
               (progn
                 ;; If we have something to write to the client, do so.
                 (when (> bytes-left-to-write 0)
                   (let ((line (dequeue line-queue)))
                     (format socket "~A" line) ;; newline is in the string.
                     (finish-output socket)
                     (format t "Wrote to ~A:~A: ~A" who port line)
                     (decf bytes-left-to-write (length line))))

                 ;; If we see we've fallen below the max-bytes mark,
                 ;; re-register the read handler to get more data for
                 ;; us. However, don't reregister the read handler if
                 ;; we've seen that the client closed our read end of
                 ;; our socket.
                 (when (< bytes-left-to-write max-bytes)
                   (unless (or eof-seen read-handler-registered)
                     (set-io-handler *ex7-event-base*
                                     (socket-os-fd socket)
                                     :read
                                     #'read-a-line)
                     (setf read-handler-registered t)))

                 ;; If we notice that we don't have any data to write
                 ;; AND have seen the end of file from the client,
                 ;; then we close the connection to the client since
                 ;; it will never speak to us again and we're done
                 ;; speaking to it.
                 ;;
                 ;; If notice we've written all of our data and there
                 ;; might be more to do later, then unregister the
                 ;; write handler so we don't get called
                 ;; unnecesarily. This might mean that sometimes we'll
                 ;; have to make an extra trip through the
                 ;; event-dispatcher to perform the write if we read
                 ;; more from the client and it reregisters us.
                 (when (zerop bytes-left-to-write)
                   (if eof-seen
                       (funcall disconnector who port :close)
                       (progn
                         (funcall disconnector who port :write)
                         (setf write-handler-registered nil)))))

             (socket-connection-reset-error ()
               ;; If I happen to get a reset, make sure the connection
               ;; is closed.  I shouldn't get this here, but if you
               ;; tinker with the flow of this example, it is a good
               ;; guard to have.
               (format t "Client ~A:~A: connection reset by peer.~%" who port)
               (funcall disconnector who port :close))

             (hangup ()
               ;; In this server, if the client doesn't accept data,
               ;; it also means it will never send us data again. So
               ;; close the connection for good.
               (format t "Client ~A:~A got hangup on write.~%" who port)
               (funcall disconnector who port :close)))))
      ;; ex-2e

      ;; ex-3b
      ;; This is the actual function returned from make-ex7-io-buffer
      ;; which allows us access to the read/writer in the scope of the
      ;; closure.  We will ask for the correct functions when setting
      ;; up the io handlers.  NOTE: By simply asking for the handler,
      ;; I've assumed it is to be immediately put into an iolib event
      ;; handler. This is why they are considered registered at this point.
      (lambda (msg)
        (cond
          ((equalp msg :read-a-line)
           (setf read-handler-registered t)
           #'read-a-line)
          ((equalp msg :write-a-line)
           (setf write-handler-registered t)
           #'write-a-line)
          (t
           (error "make-ex7-buffer: Please supply :read-a-line or :write-a-line~%")))))))
;; ex-3e


