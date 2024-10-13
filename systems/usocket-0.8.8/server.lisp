;;;; See LICENSE for licensing information.

(in-package :usocket)

(defvar *server*)

(defun socket-server (host port function &optional arguments
                      &key in-new-thread (protocol :stream)
                           ;; for udp
                           (timeout 1) (max-buffer-size +max-datagram-packet-size+)
                           ;; for tcp
                           element-type (reuse-address t) multi-threading
                           name)
  "Create a simple TCP or UDP socket server.

`host' and `port' name the local interface and port the server will listen on.
`function' is the handler function. It must take at least one argument: a stream
for a TCP server, or a buffer for a UDP server. For the UDP server, the handler
must also return a buffer.
`arguments' is a list of additional arguments to pass to the handler function,
after the first.
If `in-new-thread' is true, the server will be spawned on a new thread and this
function will return immediately. Otherwise, this function will run
indefinitely. `name' specifies the name of the thread.
`protocol' can be :stream for a TCP server (the default) or :datagram for a UDP server.
For UDP servers:
`timeout' specifies a read timeout, 1 second by default.
`max-buffer-size' specifies the maximum size a UDP packet can take up, `+max-datagram-packet-size+' by default.
For TCP servers:
`element-type' specifies the stream's element type.
If `reuse-address' is true, wildcard hosts and more specific hosts can share a port.
If `multi-threading' is true, each connection will be launched in a new thread,
allowing the server to handle multiple connections in parallel."
  (let* ((real-host (or host *wildcard-host*))
         (socket (ecase protocol
                   (:stream
                    (apply #'socket-listen
                           `(,real-host ,port
                             ,@(when element-type `(:element-type ,element-type))
                             ,@(when reuse-address `(:reuse-address ,reuse-address)))))
                   (:datagram
                    (socket-connect nil nil :protocol :datagram
                                    :local-host real-host
                                    :local-port port)))))
    (labels ((real-call ()
               (ecase protocol
                 (:stream
                  (tcp-event-loop socket function arguments
                                  :element-type element-type
                                  :multi-threading multi-threading))
                 (:datagram
                  (udp-event-loop socket function arguments
                                  :timeout timeout
                                  :max-buffer-size max-buffer-size)))))
      (if in-new-thread
          (values (bt2:make-thread #'real-call :name (or name "USOCKET Server")) socket)
          (progn
            (setq *server* socket)
            (real-call))))))

(defvar *remote-host*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*remote-host* 'variable)
        "The remote host of a TCP or UDP event. This variable is dynamically bound~
 in the context of a `socket-server', specifically the handler function."))

(defvar *remote-port*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*remote-port* 'variable)
        "The remote port of a TCP or UDP event. This variable is dynamically bound~
 in the context of a `socket-server', specifically the handler function."))

(defun default-udp-handler (buffer) ; echo
  "Example handler for a UDP socket-server. Returns datagrams to sender."
  (declare (type (simple-array (unsigned-byte 8) *) buffer))
  buffer)

(defun udp-event-loop (socket function &optional arguments
                       &key timeout max-buffer-size)
  (let ((buffer (make-array max-buffer-size :element-type '(unsigned-byte 8) :initial-element 0))
        (sockets (list socket)))
    (unwind-protect
        (loop do
          (multiple-value-bind (return-sockets real-time)
              (wait-for-input sockets :timeout timeout)
            (declare (ignore return-sockets))
            (when real-time
              (multiple-value-bind (recv n *remote-host* *remote-port*)
                  (socket-receive socket buffer max-buffer-size)
                (declare (ignore recv))
                (if (<= 0 n)
                    (progn
                      (let ((reply
                             (apply function (subseq buffer 0 n) arguments)))
                        (when reply
                          (replace buffer reply)
                          (let ((n (socket-send socket buffer (length reply)
                                                :host *remote-host*
                                                :port *remote-port*)))
                            (when (minusp n)
                              (error "send error: ~A~%" n))))))
                  (error "receive error: ~A" n))))
            #+scl (when thread:*quitting-lisp* (return))
            #+(and cmu mp) (mp:process-yield)))
      (socket-close socket)
      (values))))

(defun default-tcp-handler (stream) ; null
  "Example handler for a TCP socket-server. Sends client 'Hello, world!' then closes the stream."
  (declare (type stream stream))
  (format stream "Hello world!~%"))

(defun echo-tcp-handler (stream)
  "Example handler for a TCP socket-server, being an echo server."
  (loop
     (when (listen stream)
       (let ((line (read-line stream nil)))
     (write-line line stream)
     (force-output stream)))))

(defun tcp-event-loop (socket function &optional arguments
                       &key element-type multi-threading)
  (let ((real-function #'(lambda (client-socket &rest arguments)
                           (unwind-protect
                               (multiple-value-bind (*remote-host* *remote-port*) (get-peer-name client-socket)
                                 (apply function (socket-stream client-socket) arguments))
                             (close (socket-stream client-socket))
                             (socket-close client-socket)
                             nil))))
    (unwind-protect
         (loop do
           (block continue
             (let* ((client-socket (apply #'socket-accept
                                          socket
                                          (when element-type
                                            (list :element-type element-type))))
                    (client-stream (socket-stream client-socket)))
                    (if multi-threading
                        (bt2:make-thread
                         (lambda ()
                           (handler-case (apply real-function client-socket arguments)
                             #+sbcl
                             (sb-bsd-sockets:invalid-argument-error ())))
                         :name "USOCKET Client")
                        (unwind-protect
                             (handler-case (apply real-function client-socket arguments)
                               #+sbcl
                               (sb-bsd-sockets:invalid-argument-error ()
                                 (return-from continue)))
                          (close client-stream)
                          (socket-close client-socket)))
                    #+scl (when thread:*quitting-lisp* (return))
                    #+(and cmu mp) (mp:process-yield))))
      (socket-close socket)
      (values))))
