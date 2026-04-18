;;; tls-anvil-client.lisp --- TLS-Anvil trigger script for client testing
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; This script is used by TLS-Anvil to trigger a pure-tls client connection.
;;; Usage: sbcl --script tls-anvil-client.lisp <host> <port>

(require :asdf)

(handler-case
    (progn
      (let ((args (uiop:command-line-arguments)))
        (unless (>= (length args) 2)
          (format *error-output* "Usage: sbcl --script tls-anvil-client.lisp <host> <port>~%")
          (sb-ext:exit :code 1))

        (let ((host (first args))
              (port (parse-integer (second args))))

          ;; Load pure-tls
          (asdf:load-system :pure-tls :verbose nil)
          (asdf:load-system :usocket :verbose nil)

          ;; Connect to TLS-Anvil server
          (let ((socket (usocket:socket-connect host port :element-type '(unsigned-byte 8))))
            (unwind-protect
                 (let ((tls-stream
                         (funcall (find-symbol "MAKE-TLS-CLIENT-STREAM" :pure-tls)
                                  (usocket:socket-stream socket)
                                  :hostname host
                                  :verify (symbol-value (find-symbol "+VERIFY-NONE+" :pure-tls)))))
                   ;; Do a simple read/write
                   (let ((buf (make-array 1024 :element-type '(unsigned-byte 8))))
                     ;; Try to read - TLS-Anvil may send data
                     (handler-case
                         (read-sequence buf tls-stream)
                       (end-of-file () nil))
                     ;; Close cleanly
                     (close tls-stream)))
              (usocket:socket-close socket))))))
  (error (e)
    (format *error-output* "TLS error: ~A~%" e)
    (sb-ext:exit :code 1)))
