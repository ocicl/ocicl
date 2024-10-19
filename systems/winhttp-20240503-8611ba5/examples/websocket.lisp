
(defpackage winhttp-websocket-test
  (:use #:cl))

(in-package #:winhttp-websocket-test)

(defparameter *hostname* nil)
(defparameter *port* nil)

(defun client ()
  (winhttp:with-websocket (hwebsocket *hostname* *port*)
    (let ((seq (make-array 64 :element-type '(unsigned-byte 8))))
      (winhttp:websocket-send hwebsocket (babel:string-to-octets "hello"))
      (multiple-value-bind (count type) (winhttp:websocket-receive hwebsocket seq)
	(format t "Type ~S Msg ~S~%" type (subseq seq 0 count))))))



