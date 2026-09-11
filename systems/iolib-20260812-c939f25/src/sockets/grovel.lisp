;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Grovelling for socket constants and types.
;;;
;;; Copyright (C) 2005-2006, Emily Backes  <lucca@accela.net>
;;; Copyright (C) 2005-2006, Dan Knapp  <dankna@accela.net>
;;; Copyright (C) 2007, Stelian Ionescu  <sionescu@cddr.org>
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;; This file contains a lot of unused types and constants that should
;;; be cleaned up or at least commented out.

(include "sys/socket.h" "sys/un.h" "netdb.h" "errno.h" "time.h"
         "net/if.h" "netinet/in.h" "netinet/tcp.h" "netinet/ip.h"
         #+linux "linux/errqueue.h" #+linux "linux/icmp.h"
         #+linux "linux/netlink.h"
         "arpa/inet.h")

(in-package :iolib/sockets)

(constantenum socket-error-values
  ((:eprotonosupport "EPROTONOSUPPORT"))
  ((:esocktnosupport "ESOCKTNOSUPPORT"))
  ((:enotsock "ENOTSOCK"))
  ((:edestaddrreq "EDESTADDRREQ"))
  ((:emsgsize "EMSGSIZE"))
  ((:eprototype "EPROTOTYPE"))
  ((:enoprotoopt "ENOPROTOOPT"))
  ((:eremote "EREMOTE"))
  ((:enolink "ENOLINK") :optional t)
  ((:epfnosupport "EPFNOSUPPORT"))
  ((:eafnosupport "EAFNOSUPPORT"))
  ((:eaddrinuse "EADDRINUSE"))
  ((:eaddrnotavail "EADDRNOTAVAIL"))
  ((:enetdown "ENETDOWN"))
  ((:enetunreach "ENETUNREACH"))
  ((:enetreset "ENETRESET"))
  ((:econnaborted "ECONNABORTED"))
  ((:econnreset "ECONNRESET"))
  ((:eisconn "EISCONN"))
  ((:enotconn "ENOTCONN"))
  ((:eshutdown "ESHUTDOWN"))
  ((:etoomanyrefs "ETOOMANYREFS"))
  ((:etimedout "ETIMEDOUT"))
  ((:econnrefused "ECONNREFUSED"))
  ((:ehostdown "EHOSTDOWN"))
  ((:ehostunreach "EHOSTUNREACH"))
  ((:enonet "ENONET") :optional t)
  ((:enobufs "ENOBUFS"))
  ((:eopnotsupp "EOPNOTSUPP"))
  ((:ebadf "EBADF"))
  ((:einval "EINVAL"))
  ((:enomem "ENOMEM"))
  ((:eacces "EACCES"))
  ((:efault "EFAULT"))
  ((:emfile "EMFILE"))
  ((:einprogress "EINPROGRESS"))
  ((:ealready "EALREADY"))
  ((:eloop "ELOOP"))
  ((:enametoolong "ENAMETOOLONG"))
  ((:enotempty "ENOTEMPTY"))
  ((:eusers "EUSERS"))
  ((:edquot "EDQUOT"))
  ((:estale "ESTALE")))

;;;; sys/socket.h

(ctype socklen-t "socklen_t")
(ctype sa-family-t "sa_family_t")

;;; socket() - socket address family
(constant (af-unspec "AF_UNSPEC" "PF_UNSPEC"))
(constant (af-inet "AF_INET" "PF_INET")
  :documentation "IPv4 Protocol family")
(constant (af-inet6 "AF_INET6" "PF_INET6")
  :documentation "IPv6 Protocol family")
(constant (af-local "AF_UNIX" "AF_LOCAL" "PF_UNIX" "PF_LOCAL")
  :documentation "File domain sockets")
(constant (af-packet "AF_PACKET" "PF_PACKET")
  :documentation "Raw packet access"
  :optional t)
(constant (af-route "AF_ROUTE" "PF_ROUTE")
  :documentation "Routing sockets"
  :optional t)
(constant (af-key "AF_KEY" "PF_KEY"))
(constant (af-netlink "AF_NETLINK" "PF_NETLINK")
  :documentation "Linux Netlink sockets" :optional t)

;;; socket() - socket type
(constant (sock-stream "SOCK_STREAM")
  :documentation "TCP")
(constant (sock-dgram "SOCK_DGRAM")
  :documentation "UDP")
(constant (sock-seqpacket "SOCK_SEQPACKET")
  :documentation "Reliable Sequenced Datagram Protocol"
  :optional t)
(constant (sock-raw "SOCK_RAW")
  :documentation "Raw protocol access"
  :optional t)
(constant (sock-rdm "SOCK_RDM")
  :documentation "Reliable Unordered Datagram Protocol"
  :optional t)

;;; socket() - socket protocol
(constant (ipproto-ip "IPPROTO_IP"))
(constant (ipproto-ipv6 "IPPROTO_IPV6"))
(constant (ipproto-icmp "IPPROTO_ICMP"))
(constant (ipproto-icmpv6 "IPPROTO_ICMPV6"))
(constant (ipproto-raw "IPPROTO_RAW"))
(constant (ipproto-tcp "IPPROTO_TCP"))
(constant (ipproto-udp "IPPROTO_UDP"))
(constant (ipproto-sctp "IPPROTO_SCTP") :optional t)

(cstruct sockaddr "struct sockaddr"
  (family "sa_family" :type sa-family-t))

(cstruct sockaddr-storage "struct sockaddr_storage"
  (family "ss_family" :type sa-family-t))

(constant (somaxconn "SOMAXCONN")
  :documentation "Maximum listen() queue length")

(constant (sol-socket "SOL_SOCKET")
  :documentation "get/setsockopt socket level constant.")

(constant (sol-tcp "SOL_TCP")
  :documentation "get/setsockopt TCP level constant."
  :optional t)
(constant (sol-ip "SOL_IP")
  :documentation "get/setsockopt IP level constant."
  :optional t)
(constant (sol-raw "SOL_RAW")
  :documentation "get/setsockopt raw level constant."
  :optional t)

;;; getsockopt/setsockopt()
(constant (so-acceptconn "SO_ACCEPTCONN"))
(constant (so-acceptfilter "SO_ACCEPTFILTER") :optional t)    ; freebsd
(constant (so-bindtodevice "SO_BINDTODEVICE") :optional t)    ; linux
(constant (so-bintime "SO_BINTIME") :optional t)              ; freebsd
(constant (so-broadcast "SO_BROADCAST"))
(constant (so-debug "SO_DEBUG"))
(constant (so-dontroute "SO_DONTROUTE"))
(constant (so-error "SO_ERROR"))
(constant (so-keepalive "SO_KEEPALIVE"))
(constant (so-label "SO_LABEL") :optional t)                  ; freebsd
(constant (so-linger "SO_LINGER"))
(constant (so-listenincqlen "SO_LISTENINCQLEN") :optional t)  ; freebsd
(constant (so-listenqlen "SO_LISTENQLEN") :optional t)        ; freebsd
(constant (so-listenqlimit "SO_LISTENQLIMIT") :optional t)    ; freebsd
(constant (so-nosigpipe "SO_NOSIGPIPE") :optional t)          ; freebsd
(constant (so-oobinline "SO_OOBINLINE"))
(constant (so-passcred "SO_PASSCRED") :optional t) ; linux
(constant (so-peercred "SO_PEERCRED") :optional t) ; linux
(constant (so-peerlabel "SO_PEERLABEL") :optional t)          ; freebsd
(constant (so-priority "SO_PRIORITY") :optional t) ; linux
(constant (so-rcvbuf "SO_RCVBUF"))
(constant (so-rcvlowat "SO_RCVLOWAT"))
(constant (so-rcvtimeo "SO_RCVTIMEO"))
(constant (so-reuseaddr "SO_REUSEADDR"))
(constant (so-reuseport "SO_REUSEPORT") :optional t)          ; freebsd
(constant (so-sndbuf "SO_SNDBUF"))
(constant (so-sndlowat "SO_SNDLOWAT"))
(constant (so-sndtimeo "SO_SNDTIMEO"))
(constant (so-timestamp "SO_TIMESTAMP"))
(constant (so-type "SO_TYPE"))
(constant (so-useloopback "SO_USELOOPBACK") :optional t)      ; freebsd

;; TCP options
(constant (tcp-cork "TCP_CORK") :optional t)                  ; linux
(constant (tcp-defer-accept "TCP_DEFER_ACCEPT") :optional t)  ; linux
(constant (tcp-info "TCP_INFO") :optional t)                  ; linux
(constant (tcp-keepcnt "TCP_KEEPCNT") :optional t)            ; linux
(constant (tcp-keepidle "TCP_KEEPIDLE") :optional t)          ; linux
(constant (tcp-keepintvl "TCP_KEEPINTVL") :optional t)        ; linux
(constant (tcp-linger2 "TCP_LINGER2") :optional t)            ; linux
(constant (tcp-maxseg "TCP_MAXSEG") :optional t)              ; linux, freebsd
(constant (tcp-nodelay "TCP_NODELAY") :optional t)            ; linux, freebsd
(constant (tcp-noopt "TCP_NOOPT") :optional t)                ; freebsd
(constant (tcp-nopush "TCP_NOPUSH") :optional t)              ; freebsd
(constant (tcp-quickack "TCP_QUICKACK") :optional t)          ; linux
(constant (tcp-syncnt "TCP_SYNCNT") :optional t)              ; linux
(constant (tcp-window-clamp "TCP_WINDOW_CLAMP") :optional t)  ; linux

;; IP options
(constant (ip-hdrincl "IP_HDRINCL"))
(constant (ip-recverr "IP_RECVERR") :optional t)

#+linux
(progn
  (cenum extended-error-origin
    ((:none         "SO_EE_ORIGIN_NONE"))
    ((:local        "SO_EE_ORIGIN_LOCAL"))
    ((:icmp         "SO_EE_ORIGIN_ICMP"))
    ((:icmp6        "SO_EE_ORIGIN_ICMP6"))
    ((:timestamping "SO_EE_ORIGIN_TIMESTAMPING")))

  (cstruct sock-extended-err "struct sock_extended_err"
    (errno  "ee_errno"  :type :uint32)
    (origin "ee_origin" :type extended-error-origin)
    (type   "ee_type"   :type :uint8)
    (code   "ee_code"   :type :uint8)
    (info   "ee_info"   :type :uint8)
    (data   "ee_data"   :type :uint8)))

;; RAW options
#+linux
(progn
  (constant (icmp-filter "ICMP_FILTER"))

  (constantenum (icmp-types :define-constants t)
    ((:icmp-echo-request "ICMP_ECHO"))
    ((:icmp-echo-reply "ICMP_ECHOREPLY"))
    ((:icmp-dest-unreach "ICMP_DEST_UNREACH"))
    ((:icmp-source-quench "ICMP_SOURCE_QUENCH"))
    ((:icmp-redirect "ICMP_REDIRECT"))
    ((:icmp-time-exceeded "ICMP_TIME_EXCEEDED"))
    ((:icmp-parameter-prob "ICMP_PARAMETERPROB"))
    ((:icmp-timestamp-request "ICMP_TIMESTAMP"))
    ((:icmp-timestamp-reply "ICMP_TIMESTAMPREPLY"))
    ((:icmp-info-request "ICMP_INFO_REQUEST"))
    ((:icmp-info-reply "ICMP_INFO_REPLY"))
    ((:icmp-address-request "ICMP_ADDRESS"))
    ((:icmp-address-reply "ICMP_ADDRESSREPLY")))

  (constantenum (icmp-unreach :define-constants t)
    ((:icmp-net-unreach "ICMP_NET_UNREACH"))
    ((:icmp-host-unreach "ICMP_HOST_UNREACH"))
    ((:icmp-prot-unreach "ICMP_PROT_UNREACH"))
    ((:icmp-port-unreach "ICMP_PORT_UNREACH"))
    ((:icmp-frag-needed "ICMP_FRAG_NEEDED"))
    ((:icmp-sr-failed "ICMP_SR_FAILED"))
    ((:icmp-net-unknown "ICMP_NET_UNKNOWN"))
    ((:icmp-host-unknown "ICMP_HOST_UNKNOWN"))
    ((:icmp-host-isolated "ICMP_HOST_ISOLATED"))
    ((:icmp-net-ano "ICMP_NET_ANO"))
    ((:icmp-host-ano "ICMP_HOST_ANO"))
    ((:icmp-net-unr-tos "ICMP_NET_UNR_TOS"))
    ((:icmp-host-unr-tos "ICMP_HOST_UNR_TOS"))
    ((:icmp-pkt-filtered "ICMP_PKT_FILTERED"))
    ((:icmp-prec-violation "ICMP_PREC_VIOLATION"))
    ((:icmp-prec-cutoff "ICMP_PREC_CUTOFF")))

  (constantenum (icmp-redirect :define-constants t)
    ((:icmp-redir-net "ICMP_REDIR_NET"))
    ((:icmp-redir-host "ICMP_REDIR_HOST"))
    ((:icmp-redir-nettos "ICMP_REDIR_NETTOS"))
    ((:icmp-redir-hosttos "ICMP_REDIR_HOSTTOS")))

  (constantenum (icmp-time-exceeded :define-constants t)
    ((:icmp-exc-ttl "ICMP_EXC_TTL"))
    ((:icmp-exc-fragtime "ICMP_EXC_FRAGTIME"))))

;;; shutdown()
(constant (shut-rd "SHUT_RD" "SD_RECEIVE"))
(constant (shut-wr "SHUT_WR" "SD_SEND"))
(constant (shut-rdwr "SHUT_RDWR" "SD_BOTH"))

;;; recvmsg/sendmsg()
(constant (msg-dontroute "MSG_DONTROUTE"))            ;         sendmsg
(constant (msg-oob "MSG_OOB"))                        ; recvmsg sendmsg
(constant (msg-peek "MSG_PEEK"))                      ; recvmsg
(constant (msg-errqueue "MSG_ERRQUEUE") :optional t)  ; recvmsg
(constant (msg-more "MSG_MORE") :optional t)          ;         sendmsg
(constant (msg-confirm "MSG_CONFIRM") :optional t)    ; sendmsg sendmsg
(constant (msg-proxy "MSG_PROXY") :optional t)        ;
(constant (msg-fin "MSG_FIN") :optional t)            ;
(constant (msg-syn "MSG_SYN") :optional t)            ;
(constant (msg-eof "MSG_EOF") :optional t)            ;
(constant (msg-nbio "MSG_NBIO") :optional t)          ;
(constant (msg-compat "MSG_COMPAT") :optional t)      ;
(constant (msg-trunc "MSG_TRUNC"))                    ; recvmsg
(constant (msg-waitall "MSG_WAITALL"))                ; recvmsg
(constant (msg-dontwait "MSG_DONTWAIT"))              ; recvmsg sendmsg
(constant (msg-nosignal "MSG_NOSIGNAL") :optional t)  ;         sendmsg
(constant (msg-eor "MSG_EOR"))                        ; recvmsg sendmsg
(constant (msg-ctrunc "MSG_CTRUNC"))                  ; recvmsg

(cstruct msghdr "struct msghdr"
  (name       "msg_name"       :type :pointer)
  (namelen    "msg_namelen"    :type socklen-t)
  (iov        "msg_iov"        :type :pointer)
  (iovlen     "msg_iovlen"     :type size-t)
  (control    "msg_control"    :type :pointer)
  (controllen "msg_controllen" :type socklen-t)
  (flags      "msg_flags"      :type :int))

(cstruct cmsghdr "struct cmsghdr"
  (len   "cmsg_len"   :type socklen-t)
  (level "cmsg_level" :type :int)
  (type  "cmsg_type"  :type :int))

#-(and) ; unused
(constant (cmgroup-max "CMGROUP_MAX") :optional t)

#+(or (or) freebsd) ; unused
(cstruct cmsgcred "struct cmsgcred"
  (pid     "cmcred_pid"     :type pid-t)
  (uid     "cmcred_uid"     :type uid-t)
  (euid    "cmcred_euid"    :type uid-t)
  (gid     "cmcred_gid"     :type gid-t)
  (ngroups "cmcred_ngroups" :type :short)
  (groups  "cmcred_groups"  :type gid-t :count :auto))

(constant (scm-rights "SCM_RIGHTS"))
(constant (scm-credentials "SCM_CREDENTIALS") :optional t)

#+(and nil linux)
(cstruct ucred "struct ucred"
  "Socket credential messages."
  (pid "pid" :type pid-t)
  (uid "uid" :type uid-t)
  (gid "gid" :type gid-t))

#+(and nil freebsd)
(cstruct sockcred "struct sockcred"
  (uid     "sc_uid"     :type uid-t)
  (euid    "sc_euid"    :type uid-t)
  (gid     "sc_gid"     :type gid-t)
  (egid    "sc_egid"    :type gid-t)
  (ngroups "sc_ngroups" :type :int)
  (groups  "sc_groups"  :type gid-t :count :auto))

(cstruct linger "struct linger"
  "SO_LINGER manipulation record."
  (onoff  "l_onoff"  :type :int)
  (linger "l_linger" :type :int))

#+(or freebsd dragonfly)
(cstruct accept-filter-arg "struct accept_filter_arg"
  (name "af_name" :type :uint8 :count :auto)
  (arg  "af_arg"  :type :uint8 :count :auto))

;;;; from sys/un.h

(cstruct sockaddr-un "struct sockaddr_un"
  "A UNIX-domain socket address."
  (family "sun_family" :type sa-family-t)
  (path   "sun_path"   :type :uint8 :count :auto))

#+dragonfly
(constant (local-peercred "LOCAL_PEERCRED"))

#+freebsd
(progn
  (constant (local-peercred "LOCAL_PEERCRED"))
  (constant (local-creds "LOCAL_CREDS"))
  (constant (local-connwait "LOCAL_CONNWAIT")))

;;;; from netinet/in.h

(ctype in-port-t "in_port_t")
(ctype in-addr-t "in_addr_t")

(cstruct sockaddr-in "struct sockaddr_in"
  "An IPv4 socket address."
  (family "sin_family" :type sa-family-t)
  (port   "sin_port"   :type in-port-t)
  (addr   "sin_addr"   :type in-addr-t))

(cstruct in-addr-struct "struct in_addr"
  (addr "s_addr" :type :uint32))

(cunion in6-addr "struct in6_addr"
  "An IPv6 address."
  (addr8  "s6_addr"   :type :uint8  :count :auto))

(cstruct sockaddr-in6 "struct sockaddr_in6"
  "An IPv6 socket address."
  (family   "sin6_family"   :type sa-family-t)
  (port     "sin6_port"     :type in-port-t)
  (flowinfo "sin6_flowinfo" :type :uint32)
  (addr     "sin6_addr"     :type (:union in6-addr))
  (scope-id "sin6_scope_id" :type :uint32))

#-(and) ; unused
(progn
  (constant (inaddr-any "INADDR_ANY"))
  (constant (inaddr-broadcast "INADDR_BROADCAST"))
  (constant (inaddr-none "INADDR_NONE"))
  (constant (in-loopbacknet "IN_LOOPBACKNET"))
  (constant (inaddr-loopback "INADDR_LOOPBACK"))
  (constant (inaddr-unspec-group "INADDR_UNSPEC_GROUP"))
  (constant (inaddr-allhosts-group "INADDR_ALLHOSTS_GROUP"))
  (constant (inaddr-allrtrs-group "INADDR_ALLRTRS_GROUP"))
  (constant (inaddr-max-local-group "INADDR_MAX_LOCAL_GROUP")))

(constant (inet-addrstrlen "INET_ADDRSTRLEN"))
(constant (inet6-addrstrlen "INET6_ADDRSTRLEN"))

(constant (ipv6-join-group "IPV6_JOIN_GROUP"))
(constant (ipv6-leave-group "IPV6_LEAVE_GROUP"))
(constant (ipv6-multicast-hops "IPV6_MULTICAST_HOPS"))
(constant (ipv6-multicast-if "IPV6_MULTICAST_IF"))
(constant (ipv6-multicast-loop "IPV6_MULTICAST_LOOP"))
(constant (ipv6-unicast-hops "IPV6_UNICAST_HOPS"))
(constant (ipv6-v6only "IPV6_V6ONLY"))

;;;; from netinet/tcp.h

#+linux
(cenum connstates
  ((:tcp-established "TCP_ESTABLISHED"))
  ((:tcp-syn-sent "TCP_SYN_SENT"))
  ((:tcp-syn-recv "TCP_SYN_RECV"))
  ((:tcp-fin-wait1 "TCP_FIN_WAIT1"))
  ((:tcp-fin-wait2 "TCP_FIN_WAIT2"))
  ((:tcp-time-wait "TCP_TIME_WAIT"))
  ((:tcp-close "TCP_CLOSE"))
  ((:tcp-close-wait "TCP_CLOSE_WAIT"))
  ((:tcp-last-ack "TCP_LAST_ACK"))
  ((:tcp-listen "TCP_LISTEN"))
  ((:tcp-closing "TCP_CLOSING")))

;;;; from net/if.h

(cstruct if-nameindex "struct if_nameindex"
  (index "if_index" :type :unsigned-int)
  (name  "if_name"  :type :string))

(constant (ifnamesize "IF_NAMESIZE"))
(constant (ifnamsiz "IFNAMSIZ"))

(cstruct ifreq "struct ifreq"
  (name "ifr_name" :type :char :count :auto))

;;;; from linux/netlink.h

#+linux
(cstruct sockaddr-nl "struct sockaddr_nl"
  "A Netlink socket address."
  (family "nl_family" :type sa-family-t)
  (port   "nl_pid"    :type :uint32)
  (groups "nl_groups" :type :uint32))
