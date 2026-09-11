(in-package :iolib.sockets)

(defcstruct ip-header
  (ver-ihl  :uint8)
  (tos      :uint8)
  (length   :uint16)
  (id       :uint16)
  (offset   :uint16)
  (ttl      :uint8)
  (protocol :uint8)
  (checksum :uint16)
  (saddr    :uint32)
  (daddr    :uint32))

(defcstruct icmp-header
  (type     :uint8)
  (code     :uint8)
  (checksum :uint16)
  (quench   :uint32))

(defun write-ip-header (ip-header total-length target-ip)
  (with-foreign-slots ((ver-ihl length id offset ttl protocol daddr)
                       ip-header (:struct ip-header))
    (setf ver-ihl  #x45       ; Version 4, header length 5 words(20 bytes)
          length   total-length
          offset   #b01000000 ; Don't fragment
          ttl      64
          protocol ipproto-icmp
          daddr    (htonl target-ip))))

(defun compute-icmp-checksum (icmp-header packet-size)
  (let* ((sum1
           (loop :for offset :from 0 :below (/ packet-size 2)
                 :sum (mem-aref icmp-header :uint16 offset)))
         (sum2 (+ (ash sum1 -16)
                  (logand sum1 #xFFFF))))
    (logand #xFFFF (lognot (+ sum2 (ash sum2 -16))))))

(defun write-icmp-header (icmp-header packet-size id seqno)
  (with-foreign-slots ((type quench checksum)
                       icmp-header (:struct icmp-header))
    (let ((new-quench
            (+ (ash id 16) seqno)))
      (setf type     icmp-echo-request
            quench   (htonl new-quench))
      (setf checksum (compute-icmp-checksum icmp-header packet-size)))))

(defun ping (target &key (id #xFF) (seqno 1))
  (with-open-socket (socket :address-family :ipv4 :type :raw :protocol ipproto-icmp
                            :include-headers t)
    (let* ((payload-size 4)
           (icmp-packet-size (+ (isys:sizeof '(:struct icmp-header)) payload-size))
           (frame-size (+ (isys:sizeof '(:struct ip-header)) icmp-packet-size)))
      (with-foreign-object (frame :uint8 frame-size)
        (isys:bzero frame frame-size)
        (let* ((ip-header frame)
               (icmp-header (cffi:inc-pointer ip-header (isys:sizeof '(:struct ip-header))))
               (payload (cffi:inc-pointer icmp-header (isys:sizeof '(:struct icmp-header)))))
          (write-ip-header ip-header frame-size (dotted-to-integer target))
          (setf (mem-ref payload :uint32) (htonl #x1A2B3C4D))
          (write-icmp-header icmp-header icmp-packet-size id seqno)
          (send-to socket frame :end frame-size :remote-host target)
          (iolib/multiplex:wait-until-fd-ready (socket-os-fd socket) :input)
          (receive-from socket :size (* 64 1024)))))))
