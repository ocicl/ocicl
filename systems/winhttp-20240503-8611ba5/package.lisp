;;;; Copyright (c) Frank James 2017 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:winhttp
  (:use #:cl #:cffi)
  (:export #:http-request
	   #:with-http
	   #:with-connect 
	   #:with-request
	   #:crack-url
	   #:http-open
	   #:close-handle
	   #:add-request-headers
	   #:http-connect
	   #:http-open-request
	   #:query-headers
	   #:query-status-code
	   #:read-data
	   #:receive-response
	   #:send-request
	   #:query-data-available
	   #:set-credentials
	   #:define-status-callback
	   #:set-status-callback

	   ;; websocket
	   #:websocket-close
	   #:websocket-complete-upgrade
	   #:websocket-receive
	   #:websocket-send
	   #:websocket-shutdown
	   #:upgrade-to-websocket
	   #:with-websocket
	   #:websocket-query-close-status
	   ))


