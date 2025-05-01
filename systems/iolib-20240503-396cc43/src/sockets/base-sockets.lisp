;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Base socket classes.
;;;

(in-package :iolib/sockets)

;;;; Sockets

(defclass socket (dual-channel-fd-mixin)
  ((address-family :initarg :address-family :accessor socket-address-family)
   (protocol :initarg :protocol :accessor socket-protocol)
   (bound    :initform nil      :reader   socket-bound-p :type boolean))
  (:documentation "Base class for sockets."))
(unset-method-docstring #'socket-address-family () '(socket))
(set-function-docstring 'socket-address-family "Return the address family of a socket.")
(unset-method-docstring #'socket-protocol () '(socket))
(set-function-docstring 'socket-protocol "Return the protocol of a socket.")

(defgeneric make-socket (&key address-family type connect ipv6 external-format
                              &allow-other-keys)
  (:documentation "Create an instance of a subclass of SOCKET. ADDRESS-FAMILY, TYPE, CONNECT and IPV6
are used to specify the kind of socket to create.

* ADDRESS-FAMILY - :INTERNET, :LOCAL (or :FILE as synonim), or :NETLINK(on Linux)
* TYPE - :STREAM, :DATAGRAM or :RAW
* CONNECT - :ACTIVE or :PASSIVE
* IPV6 - if NIL, create an IPv4 socket, otherwise an IPv6 socket.

To initialize the socket, the following keyword arguments can be used depending on ADDRESS-FAMILY, TYPE and CONNECT:
* :local-host - a hostname designator or NIL. If non-null the socket will be bound to this address
* :local-port - a port designator or NIL. If LOCAL-HOST is non-null, bind the socket to this port. If NIL, choose a random port
* :remote-host - a hostname designator or NIL. If non-null the socket will be connected to this address
* :remote-port - a port designator. If REMOTE-HOST is non-null, connect the socket to this port
* :local-filename - a string or NIL. If non-null the socket will be bound to this file
* :remote-filename - a string or NIL. If non-null the socket will be connected to this file
* :backlog - a positive integer or NIL. Specifies the length of the incomming connection queue and can't be larger than +MAX-BACKLOG-SIZE+. If NIL, default is *DEFAULT-BACKLOG-SIZE*
* :reuse-address: a boolean(default T). set option SO_REUSEADDR if LOCAL-HOST is non-null
* :keepalive - a boolean. set option SO_KEEPALIVE
* :nodelay - a boolean. set option SO_NODELAY
* :interface - a string. set option SO_BINDTODEVICE to this interface
* :input-buffer-size - a positive integer. Create the stream input buffer of this size
* :output-buffer-size - a positive integer. Create the stream output buffer of this size
* :multicast-groups - a non-negative integer. Accept messages sent to those multicast groups

Glossary:
* hostname designator: an instance of INET-ADDRESS or any object accepted by LOOKUP-HOST. IPV6 is passed to LOOKUP-HOST as is
* port designator: any object accepted by LOOKUP-SERVICE

:address-family :INTERNET :type :STREAM :connect :ACTIVE
* Valid keyword args: :LOCAL-HOST, :LOCAL-PORT, :REMOTE-HOST, :REMOTE-PORT, :REUSE-ADDRESS, :KEEPALIVE, :NODELAY, :INPUT-BUFFER-SIZE and :OUTPUT-BUFFER-SIZE

:address-family :INTERNET :type :STREAM :connect :PASSIVE
* Valid keyword args: :LOCAL-HOST, :LOCAL-PORT, :BACKLOG, :REUSE-ADDRESS, :INTERFACE and :NODELAY

:address-family :INTERNET :type :STREAM :connect :ACTIVE
* Valid keyword args: :LOCAL-FILENAME, :REMOTE-FILENAME, :INPUT-BUFFER-SIZE and :OUTPUT-BUFFER-SIZE

:address-family :INTERNET :type :STREAM :connect :PASSIVE
* Valid keyword args: :LOCAL-FILENAME, :REMOTE-FILENAME, :BACKLOG and :REUSE-ADDRESS

:address-family :INTERNET :type :DATAGRAM
* Valid keyword args: :LOCAL-HOST, :LOCAL-PORT, :REMOTE-HOST, :REMOTE-PORT, :REUSE-ADDRESS, :INTERFACE and :BROADCAST

:address-family :LOCAL :type :DATAGRAM
* Valid keyword args: :LOCAL-FILENAME and :REMOTE-FILENAME

:address-family :INTERNET :type :RAW
* Valid keyword args: :INCLUDE-HEADERS

:address-family :NETLINK :type :RAW
* Valid keyword args: :LOCAL-PORT and :MULTICAST-GROUPS"))

(defgeneric make-socket-from-fd (fd &key dup connect external-format
                                    input-buffer-size output-buffer-size)
  (:documentation "Create a socket instance of the appropriate subclass of SOCKET using FD.
The connection type of the socket must be specified - :ACTIVE or :PASSIVE.
The address family and type of the socket are automatically discovered using OS functions. Buffer sizes
for the new socket can also be specified using INPUT-BUFFER-SIZE and OUTPUT-BUFFER-SIZE."))

(defgeneric make-socket-pair (&key type protocol external-format
                                   input-buffer-size output-buffer-size)
  (:documentation "Create a pair of sockets connected to each other.
The socket type must be either :STREAM or :DATAGRAM. Currently OSes can only create :LOCAL sockets this way.
Buffer sizes for the new sockets can also be specified using INPUT-BUFFER-SIZE and OUTPUT-BUFFER-SIZE."))

(defgeneric socket-os-fd (socket)
  (:documentation "Returns the OS file descriptor of SOCKET."))

(defgeneric socket-type (socket)
  (:documentation "Returns the socket type of SOCKET - :STREAM or :DATAGRAM."))

(defgeneric socket-open-p (socket)
  (:documentation "Returns a boolean indicating whether or not the file descriptor of SOCKET is open."))

(defgeneric local-name (socket)
  (:documentation "For INTERNET sockets, returns two values: the local host and the local port.
For LOCAL sockets, returns the local filename."))

(defgeneric local-host (socket)
  (:documentation "Returns the local host of SOCKET.
Works only on INTERNET sockets."))

(defgeneric local-port (socket)
  (:documentation "Returns the local port of SOCKET - an (UNSIGNED-BYTE 16).
Works only on INTERNET sockets."))

(defgeneric local-filename (socket)
  (:documentation "Returns the local filename of SOCKET.
Works only on LOCAL sockets."))

(defgeneric remote-name (socket)
  (:documentation "For INTERNET sockets, returns two values: the remote host and the remote port.
For REMOTE sockets, returns the remote filename."))

(defgeneric remote-host (socket)
  (:documentation "Returns the remote host of SOCKET.
Works only on INTERNET sockets."))

(defgeneric remote-port (socket)
  (:documentation "Returns the remote port of SOCKET - an (UNSIGNED-BYTE 16).
Works only on INTERNET sockets."))

(defgeneric remote-filename (socket)
  (:documentation "Returns the remote filename of SOCKET.
Works only on LOCAL sockets."))

(defgeneric socket-option (socket option-name)
  (:documentation "Returns the value(s) of OS option OPTION-NAME on SOCKET.
For a complete list of supported options see src/sockets/socket-options.lisp."))

(defclass stream-socket (socket) ()
  (:default-initargs :type :stream)
  (:documentation "Mixin for sockets of type SOCK_STREAM."))

(defclass datagram-socket (socket) ()
  (:default-initargs :type :datagram)
  (:documentation "Mixin for sockets of type SOCK_DGRAM."))

(defclass raw-socket (socket) ()
  (:default-initargs :type :raw)
  (:documentation "Mixin for sockets of type SOCK_RAW."))

(defgeneric disconnect (socket)
  (:documentation "Disassociates SOCKET from any remote address.
Works only on DATAGRAM sockets."))

(define-symbol-macro +default-inet-address-family+
  (if *ipv6* :ipv6 :ipv4))

(defclass internet-socket (socket) ()
  (:default-initargs :address-family +default-inet-address-family+)
  (:documentation "Mixin for sockets of domain AF_INET or AF_INET6."))

(defclass local-socket (socket) ()
  (:default-initargs :address-family :local)
  (:documentation "Mixin for sockets of domain AF_LOCAL."))

#+linux
(defclass netlink-socket (socket) ()
  (:default-initargs :address-family :netlink)
  (:documentation "Mixin for sockets of domain AF_NETLINK."))

(defgeneric send-file-descriptor (socket file-descriptor)
  (:documentation "Send FILE-DESCRIPTOR through SOCKET.
The receiving process must use RECEIVE-FILE-DESCRIPTOR to receive the
file descriptor in order for it to be valid in the receiving process."))

(defgeneric receive-file-descriptor (socket)
  (:documentation "Receive a file descriptor as ancillary data through SOCKET."))

(defun socket-read-fn (fd buffer nbytes)
  (debug-only
    (assert buffer)
    (assert fd))
  (%recvfrom fd buffer nbytes 0 (null-pointer) (null-pointer)))

(defun socket-write-fn (fd buffer nbytes)
  (debug-only
    (assert buffer)
    (assert fd))
  (%sendto fd buffer nbytes 0 (null-pointer) 0))

(defclass active-socket (socket dual-channel-gray-stream) ()
  (:default-initargs :read-fn #'socket-read-fn
                     :write-fn #'socket-write-fn)
  (:documentation "Mixin class for active(client) sockets."))

(defgeneric connect (socket address &key &allow-other-keys)
  (:documentation "Connects SOCKET to ADDRESS. For INTERNET sockets you can specify
the port to connect to using keyword argument PORT. The default value of PORT is 0,
which usually means letting the OS choose a random port to connect to.
WAIT specifies how long to wait for a connection: NIL means \"return immediately\", a non-negative real
specifies a timeout in seconds and T means \"wait forever\"."))

(defgeneric socket-connected-p (socket)
  (:documentation "Returns a boolean specifying whether or not SOCKET is connected."))

(defgeneric shutdown (socket &key read write)
  (:documentation "Shut down all or part of a connection. If READ it non-NIL, further receptions are
disallowed; if WRITE is non-NIL, further transmissions are disallowed. CLOSE must still be called on
SOCKET in order to release OS resources."))

(defgeneric receive-from (socket &rest args &key &allow-other-keys)
  (:documentation "Receives data from SOCKET. If BUFFER is specified
START and END are used as bounding index. In that case BUFFER must be
an array and its ARRAY-ELEMENT-TYPE be either (UNSIGNED-BYTE 8) or T.
If BUFFER is not specified an (UNSIGNED-BYTE 8) buffer of size SIZE
will be allocated.

Some flags can also be passed to recvfrom(2):
* :OUT-OF-BAND for receiving out-of-band data - only for STREAM sockets
* :PEEK for keeping the returned data in the kernel buffers
* :WAIT-ALL for waiting until the entire buffer can be filled
* :DONT-WAIT for making only the current call non-blocking

The first two values returned are the buffer and the number of elements that have been copied into the buffer.
For INTERNET DATAGRAM sockets, two additional values are returned: the host and port of the remote peer
from which the data was received.
For LOCAL DATAGRAM sockets, one additional values is returned: the filename of the remote peer
from which the data was received."))

(defgeneric send-to (socket buffer &rest args &key &allow-other-keys)
  (:documentation "Send the contents of BUFFER to SOCKET.
BUFFER must be a vector that can be coerced to a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).
START and END are used a bounding index on BUFFER.
For disconnected datagram sockets, REMOTE-HOST and REMOTE-PORT or REMOTE-FILENAME are used
as destination for the data.

Some flags can also be passed to sendto(2):
* :OUT-OF-BAND for receiving out-of-band data - only for stream sockets
* :DONT-WAIT for making only the current call non-blocking
* :DONT-ROUTE for sending only to hosts on directly connected networks, not using gateways
* :CONFIRM for signalling progress on the link layer - only available on Linux and only with DATAGRAM sockets
* :MORE for telling the kernel that there is more data to send - only available on Linux

Returns the number of bytes sent."))

(defclass passive-socket (socket)
  ((listening :initform nil :reader socket-listening-p :type boolean)
   (external-format :initarg :external-format :reader external-format-of)
   (active-class :initarg :active-class :reader active-class
                 :type symbol :allocation :class))
  (:default-initargs :external-format :default)
  (:documentation "Mixin class for PASSIVE(server) sockets."))

(defgeneric bind-address (socket address &key &allow-other-keys)
  (:documentation "Sets the local address of SOCKET to ADDRESS(and PORT for INTERNET sockets).
REUSE-ADDRESS sets the SO_REUSEADDR socket option on SOCKET."))

(defgeneric listen-on (socket &key &allow-other-keys)
  (:documentation "Start allowing incoming connections on SOCKET.
BACKLOG specifies the maximum length of the queue of pending connections."))

(defgeneric accept-connection (passive-socket &key &allow-other-keys)
  (:documentation "Extracts the first connection from the queue of pending connections on SOCKET.
WAIT specifies how long to wait for a connection: NIL means \"return immediately\", a non-negative real
specifies a timeout in seconds and T means \"wait forever\".
EXTERNAL-FORMAT optionally specifies the external format of the new socket - the default being
that of SOCKET. Buffer sizes for the new socket can also be specified using INPUT-BUFFER-SIZE
and OUTPUT-BUFFER-SIZE.
If a connection is received, returns two or three values: the newly created socket, the remote peer
address and the remote port if applicable."))

(defclass socket-stream-internet-active
    (active-socket stream-socket internet-socket) ()
  (:documentation "Class representing active sockets of type SOCK_STREAM and domain AF_INET or AF_INET6."))

(defclass socket-stream-internet-passive
    (passive-socket stream-socket internet-socket) ()
  (:default-initargs :active-class 'socket-stream-internet-active)
  (:documentation "Class representing passive sockets of type SOCK_STREAM and domain AF_INET or AF_INET6."))

(defclass socket-stream-local-active
    (active-socket stream-socket local-socket) ()
  (:documentation "Class representing active sockets of type SOCK_STREAM and domain AF_LOCAL."))

(defclass socket-stream-local-passive
    (passive-socket stream-socket local-socket) ()
  (:default-initargs :active-class 'socket-stream-local-active)
  (:documentation "Class representing passive sockets of type SOCK_STREAM and domain AF_LOCAL."))

(defclass socket-datagram-internet
    (datagram-socket internet-socket) ()
  (:documentation "Class representing active sockets of type SOCK_DGRAM and domain AF_INET or AF_INET6."))

(defclass socket-datagram-local
    (datagram-socket local-socket) ()
  (:documentation "Class representing active sockets of type SOCK_DGRAM and domain AF_LOCAL."))

(defclass socket-raw-internet
    (internet-socket raw-socket) ()
  (:default-initargs :type :raw)
  (:documentation "Class representing active sockets of type SOCK_RAW and domain AF_LOCAL."))

#+linux
(defclass socket-raw-netlink
    (netlink-socket raw-socket) ()
  (:default-initargs :type :raw)
  (:documentation "Class representing active sockets of type SOCK_RAW and domain AF_NETLINK."))
