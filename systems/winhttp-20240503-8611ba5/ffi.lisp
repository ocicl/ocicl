;;;; Copyright (c) Frank James 2017 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:winhttp)

(define-foreign-library winhttp
  (:windows "WinHttp.dll"))

(use-foreign-library winhttp)

(defcfun (%format-message "FormatMessageA" :convention :stdcall)
    :uint32
  (flags :uint32)
  (source :pointer)
  (msg-id :uint32)
  (lang-id :uint32)
  (buffer :pointer)
  (size :uint32)
  (args :pointer))

(defparameter *winhttp-errors* 
  '((12001 "Out of Handles")
    (12002 "Timeout")
    (12004 "Internal Error")
    (12005 "Invalid URL")
    (12006 "Unrecognised scheme")
    (12007 "Name not resolved")
    (12009 "Invalid Option")
    (12011 "Option not settable")
    (12012 "Shutdown")
    (12015 "Login failure")
    (12017 "Operation cancelled")
    (12018 "Incorrect handle type")
    (12019 "Incorrect handle state")
    (12029 "Cannot connect")
    (12030 "Connection error")
    (12032 "Resend request")
    (12044 "Client auth cert needed")
    (12100 "Cannot call before open")
    (12101 "Cannot call before send")
    (12102 "Cannot call after send")
    (12103 "Cannot call after open")
    (12150 "Header not found")
    (12152 "Invalid server response")
    (12153 "Invalid header")
    (12154 "Invalid query request")
    (12155 "Header already exists")
    (12156 "Redirect failed")
    (12178 "Auto proxy service error")
    (12166 "Bad auto proxy script")
    (12167 "Unable to download script")
    (12176 "Unhandled script type")
    (12177 "Script execution error")
    (12172 "Not initialized")
    (12175 "Secure failure")
    (12037 "Certificate date invalid")
    (12038 "Certificate CN invalid")
    (12045 "Certificate Invalid CA")
    (12057 "Certificate Rev failed")
    (12157 "Secure channel error")
    (12169 "Invalid certificate")
    (12170 "Certificate revoked")
    (12179 "Certificate wrong usage")
    (12180 "Autodecection failed")
    (12181 "Header count execeeded")
    (12182 "Header size overflow")
    (12183 "Chunked encoding header size overflow")
    (12184 "Response drain overflow")
    (12185 "Client certificate no private key")
    (12186 "Client certificate no access private key")))
(defun winhttp-error-string (code)
  (cadr (assoc code *winhttp-errors*)))

(defun format-message (code)
  "Use FormatMessage to convert the error code into a system-defined string."
  (let ((msg (winhttp-error-string code)))
    (when msg (return-from format-message msg)))

  (with-foreign-object (buffer :char 1024)
    (let ((n (%format-message #x00001000
			      (null-pointer)
			      code
			      0
			      buffer
			      1024
			      (null-pointer))))
      (if (= n 0)
	  (let ((ec (%get-last-error)))
	    (format nil "Failed to format message ~A: ~A"
		    ec
		    (format-message ec)))
	  (foreign-string-to-lisp buffer :count (- n 2))))))

(define-condition win-error (error)
  ((code :initform 0 :initarg :code :reader win-error-code))
  (:report (lambda (condition stream)
	     (format stream "ERROR ~A: ~A" 
		     (win-error-code condition)
		     (format-message (win-error-code condition))))))

(defcfun (%get-last-error "GetLastError" :convention :stdcall) :long)

(defun get-last-error ()
  (let ((code (%get-last-error)))
    (unless (zerop code)
      (error 'win-error :code code))))

(defmacro with-wide-string ((var string) &body body)
  `(with-foreign-string (,var ,string :encoding :ucs-2)
     ,@body))

(defmacro with-buffers (bindings &body body)
  `(with-foreign-objects (,@(mapcar (lambda (binding)
				      (destructuring-bind (var len) binding
					  `(,var :uint8 ,len)))
				    bindings))
     ,@body))

(defun memset (p type &optional (val 0))
  (dotimes (i (foreign-type-size type))
    (setf (mem-aref p :uint8 i) val)))


;; struct URL_COMPONENTS {
;;   DWORD         dwStructSize;
;;   LPWSTR        lpszScheme;
;;   DWORD         dwSchemeLength;
;;   int           nScheme;
;;   LPWSTR        lpszHostName;
;;   DWORD         dwHostNameLength;
;;   INTERNET_PORT nPort;
;;   LPWSTR        lpszUserName;
;;   DWORD         dwUserNameLength;
;;   LPWSTR        lpszPassword;
;;   DWORD         dwPasswordLength;
;;   LPWSTR        lpszUrlPath;
;;   DWORD         dwUrlPathLength;
;;   LPWSTR        lpszExtraInfo;
;;   DWORD         dwExtraInfoLength;
;; };
(defcstruct url-components
  (size :uint32)
  (scheme :pointer)
  (scheme-len :uint32)
  (nscheme :int32) ;; 1==http 2==https 3==ftp?
  (hostname :pointer)
  (hostname-len :uint32)
  (port :uint16)
  (username :pointer)
  (username-len :uint32)
  (password :pointer)
  (password-len :uint32)
  (url :pointer)
  (url-len :uint32)
  (extra :pointer)
  (extra-len :uint32))


;; BOOL WINAPI WinHttpCrackUrl(
;;   _In_    LPCWSTR          pwszUrl,
;;   _In_    DWORD            dwUrlLength,
;;   _In_    DWORD            dwFlags,
;;   _Inout_ LPURL_COMPONENTS lpUrlComponents
;; );
(defconstant +icu-escape+ #x80000000)
(defconstant +icu-decode+ #x10000000)

(defcfun (%crack-url "WinHttpCrackUrl" :convention :stdcall) :boolean
  (url :pointer)
  (url-len :uint32)
  (flags :uint32)
  (components :pointer))
(defun crack-url (url)
  "Parse a URL into its components."
  (declare (type string url))
  (with-foreign-objects ((comp '(:struct url-components)))
    (with-wide-string (ustr url)
      (with-buffers ((hostname 512)
		     (scheme 64)
		     (url 512)
		     (extra 512)
		     (user 512)
		     (pass 512))
	(memset comp '(:struct url-components))
	(setf (foreign-slot-value comp '(:struct url-components) 'size)
	      (foreign-type-size '(:struct url-components))
	      (foreign-slot-value comp '(:struct url-components) 'hostname)
	      hostname
	      (foreign-slot-value comp '(:struct url-components) 'hostname-len)
	      256 
	      (foreign-slot-value comp '(:struct url-components) 'scheme)
	      scheme
	      (foreign-slot-value comp '(:struct url-components) 'scheme-len)
	      32	      
	      (foreign-slot-value comp '(:struct url-components) 'url)
	      url
	      (foreign-slot-value comp '(:struct url-components) 'url-len)
	      256 
	      (foreign-slot-value comp '(:struct url-components) 'username)
	      user
	      (foreign-slot-value comp '(:struct url-components) 'username-len)
	      256 
	      (foreign-slot-value comp '(:struct url-components) 'password)
	      pass
	      (foreign-slot-value comp '(:struct url-components) 'password-len)
	      256 
	      (foreign-slot-value comp '(:struct url-components) 'extra)
	      extra
	      (foreign-slot-value comp '(:struct url-components) 'extra-len)
	      256)
	(let ((res (%crack-url ustr 0 +icu-escape+ comp)))
	  (unless res (get-last-error))
	  (list :scheme (let ((s (foreign-slot-value comp '(:struct url-components) 'nscheme)))
			  (ecase s
			    (1 :http)
			    (2 :https)
			    (3 :ftp)))
		:url (foreign-string-to-lisp url :encoding :ucs-2le)
		:hostname (foreign-string-to-lisp hostname :encoding :ucs-2le)
		:username (let ((u (foreign-string-to-lisp user :encoding :ucs-2le)))
			    (unless (string= u "") u))
		:password (let ((p (foreign-string-to-lisp pass :encoding :ucs-2le)))
			    (unless (string= p "") p))
		:extra (foreign-string-to-lisp extra :encoding :ucs-2le)
		:port (let ((p (foreign-slot-value comp '(:struct url-components) 'port)))
			(cond
			  ((zerop p)
			   (let ((s (foreign-slot-value comp '(:struct url-components) 'nscheme)))
			     (cond
			       ((= s 1) 80)
			       ((= s 2) 443)
			       ((= s 3) 21)
			       (t 80))))
			  (t p)))))))))

		
		
(defconstant +access-no-proxy+ 1)
(defconstant +access-named-proxy+ 3)

;; HINTERNET WINAPI WinHttpOpen(
;;   _In_opt_ LPCWSTR pwszUserAgent,
;;   _In_     DWORD   dwAccessType,
;;   _In_     LPCWSTR pwszProxyName,
;;   _In_     LPCWSTR pwszProxyBypass,
;;   _In_     DWORD   dwFlags
;; );	    
(defcfun (%http-open "WinHttpOpen" :convention :stdcall) :pointer
  (user :pointer)
  (access-type :uint32)
  (proxy :pointer)
  (bypass :pointer)
  (flags :uint32))
(defun http-open (&optional user-agent proxy)
  (with-wide-string (u (or user-agent "winhttp"))
    (with-wide-string (p (or proxy ""))
      (let ((h (%http-open u
                           (if proxy +access-named-proxy+ +access-no-proxy+)
			   (if proxy p (null-pointer))
			   (null-pointer)
			   0)))
        (when (null-pointer-p h) (get-last-error))
        h))))

;; BOOL WINAPI WinHttpCloseHandle(
;;   _In_ HINTERNET hInternet
;; );
(defcfun (%close-handle "WinHttpCloseHandle" :convention :stdcall) :bool
  (h :pointer))
(defun close-handle (h)
  (let ((res (%close-handle h)))
    (unless res (get-last-error))))

(defconstant +addreq-add+ #x20000000)
(defconstant +addreq-replace+ #x80000000)

;; BOOL WINAPI WinHttpAddRequestHeaders(
;;   _In_ HINTERNET hRequest,
;;   _In_ LPCWSTR   pwszHeaders,
;;   _In_ DWORD     dwHeadersLength,
;;   _In_ DWORD     dwModifiers
;; );  
(defcfun (%add-request-headers "WinHttpAddRequestHeaders" :convention :stdcall) :boolean
  (hreq :pointer)
  (headers :pointer)
  (len :uint32)
  (modifiers :uint32))
(defun add-request-headers (hreq header)
  (with-wide-string (h header)
    (let ((res (%add-request-headers hreq
				     h
				     #xffffffff
				     (logior +addreq-add+ +addreq-replace+))))
      (unless res (get-last-error)))))

;; HINTERNET WINAPI WinHttpConnect(
;;   _In_       HINTERNET     hSession,
;;   _In_       LPCWSTR       pswzServerName,
;;   _In_       INTERNET_PORT nServerPort,
;;   _Reserved_ DWORD         dwReserved
;; );
(defcfun (%http-connect "WinHttpConnect" :convention :stdcall) :pointer
  (hsession :pointer)
  (server :pointer)
  (port :uint16)
  (reserved :uint32))
(defun http-connect (hsession hostname &optional port)
  (with-wide-string (s hostname)
    (let ((res (%http-connect hsession s (or port 80) 0)))
      (when (null-pointer-p res) (get-last-error))
      res)))

(defconstant +flag-secure+ #x00800000)
;; HINTERNET WINAPI WinHttpOpenRequest(
;;   _In_ HINTERNET hConnect,
;;   _In_ LPCWSTR   pwszVerb,
;;   _In_ LPCWSTR   pwszObjectName,
;;   _In_ LPCWSTR   pwszVersion,
;;   _In_ LPCWSTR   pwszReferrer,
;;   _In_ LPCWSTR   *ppwszAcceptTypes,
;;   _In_ DWORD     dwFlags
;; );
(defcfun (%win-http-open-request "WinHttpOpenRequest" :convention :stdcall) :pointer
  (hconnect :pointer)
  (verb :pointer)
  (object :pointer)
  (version :pointer)
  (referrer :pointer)
  (accept-types :pointer)
  (flags :uint32))
(defun http-open-request (hconnect &key verb url https-p)
  (with-wide-string (ustr (or url ""))
    (with-wide-string (vstr (if verb
				(string-upcase (string verb))
				"GET"))
      (let ((hreq (%win-http-open-request hconnect
					  vstr
					  ustr
					  (null-pointer)
					  (null-pointer)
					  (null-pointer)
					  (if https-p +flag-secure+ 0))))
	(when (null-pointer-p hreq) (get-last-error))
	hreq))))

(defparameter *info-levels*
  '((:mime-version 0)
    (:content-type 1)
    (:content-transfer-encoding 2)
    (:content-id 3)
    (:content-description 4)
    (:content-length 5)
    (:content-language 6)
    (:allow 7)
    (:public 8)
    (:date 9)
    (:expires 10)
    (:last-modified 11)
    (:message-id 12)
    (:uri 13)
    (:derived-from 14)
    (:cost 15)
    (:link 16)
    (:pragma 17)
    (:version 18)
    (:status-code 19)
    (:status-text 20)
    (:raw-headers 21)
    (:raw-headers-crlf 22)
    (:connection 23)
    (:accept 24)
    (:accept-charset 25)
    (:accept-encoding 26)
    (:accept-language 27)
    (:authorization 28)
    (:content-encoding 29)
    (:forwarded 30)
    (:from 31)
    (:if-modified-since 32)
    (:location 33)
    (:orig-uri 34)
    (:referer 35)
    (:retry-after 36)
    (:server 37)
    (:title 38)
    (:user-agent 39)
    (:www-authenticate 40)
    (:proxy-authenticate 41)
    (:accept-ranges 42)
    (:set-cookie 43)
    (:cookie 44)
    (:request-method 45)
    (:refresh 46)
    (:content-disposition 47)
    (:age 48)
    (:cache-control 49)
    (:content-base 50)
    (:content-location 51)
    (:content-md5 52)
    (:content-range 53)
    (:etag 54)
    (:host 55)
    (:if-match 56)
    (:if-none-match 57)
    (:if-range 58)
    (:if-unmodified-since 59)
    (:max-forwards 60)
    (:proxy-authorization 61)
    (:range 62)
    (:transfer-encoding 63)
    (:upgrade 64)
    (:vary 65)
    (:via 66)
    (:warning 67)
    (:expect 68)
    (:proxy-connection 69)
    (:unless-modified-since 70)
    (:proxy-support 75)
    (:authentication-info 76)
    (:passport-urls 77)
    (:passport-config 78)))
(defun query-info-level (name)
  (cadr (assoc name *info-levels*)))

;; BOOL WINAPI WinHttpQueryHeaders(
;;   _In_     HINTERNET hRequest,
;;   _In_     DWORD     dwInfoLevel,
;;   _In_opt_ LPCWSTR   pwszName,
;;   _Out_    LPVOID    lpBuffer,
;;   _Inout_  LPDWORD   lpdwBufferLength,
;;   _Inout_  LPDWORD   lpdwIndex
;; );
(defcfun (%http-query-headers "WinHttpQueryHeaders" :convention :stdcall) :boolean
  (hreq :pointer)
  (info :uint32)
  (name :pointer)
  (buf :pointer)
  (len :pointer)
  (idx :pointer))

(defun query-headers (hreq)
  (with-foreign-objects ((buf :uint8 (* 1024 32))
			 (len :uint32))
    (setf (mem-aref len :uint32) (* 1024 32))
    (let ((res (%http-query-headers hreq
				    (query-info-level :raw-headers-crlf)
				    (null-pointer)
				    buf
				    len
				    (null-pointer))))
      (unless res (get-last-error)))
    (let ((hstr (foreign-string-to-lisp buf :encoding :ucs-2le :count (mem-aref len :uint32))))
      (with-input-from-string (s hstr)
	(do ((l (read-line s nil nil) (read-line s nil nil))
	     (hlist nil))
	    ((null l) (reverse hlist))
	  (setf l (remove #\return l :test #'char=))
	  (let ((pos (position #\: l :test #'char=)))
	    (when pos
	      (push (list (subseq l 0 pos)
			  (subseq l (+ pos 2)))
		    hlist))))))))

(defun query-status-code (hreq)
  (with-foreign-objects ((buf :uint8 64)
			 (len :uint32))
    (setf (mem-aref len :uint32) 64)
    (let ((res (%http-query-headers hreq
				    (query-info-level :status-code)
				    (null-pointer)
				    buf
				    len
				    (null-pointer))))
      (unless res (get-last-error)))
    (parse-integer (foreign-string-to-lisp buf :encoding :ucs-2le))))

;; BOOL WINAPI WinHttpReadData(
;;   _In_  HINTERNET hRequest,
;;   _Out_ LPVOID    lpBuffer,
;;   _In_  DWORD     dwNumberOfBytesToRead,
;;   _Out_ LPDWORD   lpdwNumberOfBytesRead
;; );
(defcfun (%read-data "WinHttpReadData" :convention :stdcall) :boolean
  (hreq :pointer)
  (buf :pointer)
  (len :uint32)
  (nbytes :pointer))
(defun read-data (hreq seq &key (start 0) end)
  (let ((count (- (or end (length seq)) start)))
    (with-foreign-object (buf :uint8 count)
      (with-foreign-object (nbytes :uint32)
	(let ((res (%read-data hreq buf count nbytes)))
	  (unless res (get-last-error))
	  (dotimes (i (mem-aref nbytes :uint32))
	    (setf (aref seq (+ i start)) (mem-aref buf :uint8 i)))
	  (mem-aref nbytes :uint32))))))


;; BOOL WINAPI WinHttpReceiveResponse(
;;   _In_       HINTERNET hRequest,
;;   _Reserved_ LPVOID    lpReserved
;; );
(defcfun (%receive-response "WinHttpReceiveResponse" :convention :stdcall) :boolean
  (hreq :pointer)
  (reserved :pointer))
(defun receive-response (hreq)
  (let ((res (%receive-response hreq (null-pointer))))
    (unless res (get-last-error))
    nil))

;; BOOL WINAPI WinHttpSendRequest(
;;   _In_     HINTERNET hRequest,
;;   _In_opt_ LPCWSTR   pwszHeaders,
;;   _In_     DWORD     dwHeadersLength,
;;   _In_opt_ LPVOID    lpOptional,
;;   _In_     DWORD     dwOptionalLength,
;;   _In_     DWORD     dwTotalLength,
;;   _In_     DWORD_PTR dwContext
;; );
(defcfun (%send-request "WinHttpSendRequest" :convention :stdcall) :boolean
  (hreq :pointer)
  (headers :pointer)
  (hlen :uint32)
  (optional :pointer)
  (len :uint32)
  (total :uint32)
  (context :pointer))
(defun send-request (hreq seq &key (start 0) end)
  (let ((count (- (or end (length seq)) start)))
    (with-foreign-object (buf :uint8 count)
      (dotimes (i count)
	(setf (mem-aref buf :uint8 i) (aref seq (+ start i))))	     
      (let ((res (%send-request hreq
				(null-pointer)
				0
				buf
				count
				count
				(null-pointer))))
	(unless res (get-last-error))
	nil))))


;; BOOL WINAPI WinHttpQueryDataAvailable(
;;   _In_  HINTERNET hRequest,
;;   _Out_ LPDWORD   lpdwNumberOfBytesAvailable
;; );
(defcfun (%query-data-available "WinHttpQueryDataAvailable" :convention :stdcall) :boolean
  (hreq :pointer)
  (nbytes :pointer))
(defun query-data-available (hreq)
  (with-foreign-object (nbytes :uint32)
    (let ((res (%query-data-available hreq nbytes)))
      (unless res (get-last-error))
      (mem-aref nbytes :uint32))))

;; BOOL WINAPI WinHttpSetCredentials(
;;   _In_       HINTERNET hRequest,
;;   _In_       DWORD     AuthTargets,
;;   _In_       DWORD     AuthScheme,
;;   _In_       LPCWSTR   pwszUserName,
;;   _In_       LPCWSTR   pwszPassword,
;;   _Reserved_ LPVOID    pAuthParams
;; );
(defcfun (%set-credentials "WinHttpSetCredentials" :convention :stdcall) :boolean
  (hreq :pointer)
  (targets :uint32)
  (scheme :uint32)
  (username :pointer)
  (password :pointer)
  (params :pointer))
(defun set-credentials (hreq username password &optional (scheme :basic) (target :server))
  (with-wide-string (ustr username)
    (with-wide-string (pstr password)
      (let ((res (%set-credentials hreq
				   (ecase target
				     (:server 0)
				     (:proxy 1))
				   (ecase scheme
				     (:basic 1)
				     (:ntlm 2)
				     (:passport 4)
				     (:digest 8)
				     (:negotiate 16))
				   (if (eq scheme :negotiate) (null-pointer) ustr)
				   (if (eq scheme :negotiate) (null-pointer) pstr)
				   (null-pointer))))
	(unless res (get-last-error))
	nil))))
			      


;; BOOL WINAPI WinHttpSetOption(
;;   _In_ HINTERNET hInternet,
;;   _In_ DWORD     dwOption,
;;   _In_ LPVOID    lpBuffer,
;;   _In_ DWORD     dwBufferLength
;; );
(defcfun (%set-option "WinHttpSetOption" :convention :stdcall) :boolean
  (hreq :pointer)
  (option :uint32)
  (buf :pointer)
  (len :uint32))

(defun set-ignore-certificates (hreq)
  (with-foreign-object (buf :uint32)
    (setf (mem-aref buf :uint32)
	  (logior #x0100 ;; ignore-unknown-ca 
		  #x2000 ;; ignore-data-invalid 
		  #x1000 ;; ignore-cert-cn-invalid 
		  #x0200 ;; ignore-wrong-usage
;;          #x00010000 ;; SECURITY_FLAG_IGNORE_WEAK_SIGNATURE 
;;          #x10000000 ;; SECURITY_FLAG_STRENGTH_WEAK
;;          #x20000000 ;; SECURITY_FLAG_STRENGTH_STRONG
;;          #x40000000 ;; SECURITY_FLAG_STRENTH_MEDIUM 
		  0))
    (let ((ret (%set-option hreq
                            31 ;; WINHTTP_OPTION_SECURITY_FLAGS
                            buf
                            4)))
      (unless ret
        (get-last-error))))
  nil)


(defun set-secure-protocols (hinternet &key ssl2 ssl3 tls1 tls1-1 tls1-2)
  "Set the TLS protocols that can be used for a session.
HINTERNET ::= session handle."
  (let ((prots
         (logior (if ssl2 #x0008 0)
                 (if ssl3 #x0020 0)
                 (if tls1 #x0080 0)
                 (if tls1-1 #x0200 0)
                 (if tls1-2 #x0800 0))))
    (with-foreign-object (buf :uint32)
      (setf (mem-aref buf :uint32) prots)
      (let ((ret (%set-option hinternet
                              84 ;; WINHTTP_OPTION_SECURE_PROTOCOLS 
                              buf 4)))
        (unless ret
          (get-last-error))))))

;; BOOL WINAPI WinHttpQueryAuthSchemes(
;;   _In_  HINTERNET hRequest,
;;   _Out_ LPDWORD   lpdwSupportedSchemes,
;;   _Out_ LPDWORD   lpdwFirstScheme,
;;   _Out_ LPDWORD   pdwAuthTarget
;; );

;; BOOL WINAPI WinHttpSetTimeouts(
;;   _In_ HINTERNET hInternet,
;;   _In_ int       dwResolveTimeout,
;;   _In_ int       dwConnectTimeout,
;;   _In_ int       dwSendTimeout,
;;   _In_ int       dwReceiveTimeout
;; );
(defcfun (%set-timeouts "WinHttpSetTimeouts" :convention :stdcall) :boolean
  (hreq :pointer)
  (resolve :int32)
  (connect :int32)
  (send :int32)
  (recv :int32))
(defun set-timeouts (hreq &key resolve connect send recv)
  (let ((res (%set-timeouts hreq 
			    (or resolve 0)
			    (or connect 0)
			    (or send 0)
			    (or recv 0))))
    (unless res (get-last-error))
    nil))

;; WINHTTPAPI WINHTTP_STATUS_CALLBACK WinHttpSetStatusCallback(
;;   IN HINTERNET               hInternet,
;;   IN WINHTTP_STATUS_CALLBACK lpfnInternetCallback,
;;   IN DWORD                   dwNotificationFlags,
;;   IN DWORD_PTR               dwReserved
;; );
(defcfun (%set-status-callback "WinHttpSetStatusCallback" :convention :stdcall)
    :pointer
  (hinternet :pointer)
  (callback :pointer)
  (flags :uint32)
  (reserved :pointer))

(defun set-status-callback (hinternet callback)
  "Set a callback to receive status updates. This can be used to track
progress of an HTTP request. 
HINTERNET ::= request handle
CALLBACK ::= foreign callback address.
"
  (%set-status-callback hinternet callback #xffffffff (null-pointer)))


;; WINHTTPAPI DWORD WinHttpWebSocketClose(
;;   HINTERNET hWebSocket,
;;   USHORT    usStatus,
;;   PVOID     pvReason,
;;   DWORD     dwReasonLength
;; );
(defcfun (%websocket-close "WinHttpWebSocketClose" :convention :stdcall) :uint32
  (hwebsocket :pointer)
  (status :uint16)
  (reason :pointer)
  (creason :uint32))
(defun websocket-close (hwebsocket &optional status)
  "Close the websocket handle" 
  (%websocket-close hwebsocket
		    (or status 0)
		    (null-pointer)
		    0))

;; typedef enum _WINHTTP_WEB_SOCKET_CLOSE_STATUS {
;;   WINHTTP_WEB_SOCKET_SUCCESS_CLOSE_STATUS,
;;   WINHTTP_WEB_SOCKET_ENDPOINT_TERMINATED_CLOSE_STATUS,
;;   WINHTTP_WEB_SOCKET_PROTOCOL_ERROR_CLOSE_STATUS,
;;   WINHTTP_WEB_SOCKET_INVALID_DATA_TYPE_CLOSE_STATUS,
;;   WINHTTP_WEB_SOCKET_EMPTY_CLOSE_STATUS,
;;   WINHTTP_WEB_SOCKET_ABORTED_CLOSE_STATUS,
;;   WINHTTP_WEB_SOCKET_INVALID_PAYLOAD_CLOSE_STATUS,
;;   WINHTTP_WEB_SOCKET_POLICY_VIOLATION_CLOSE_STATUS,
;;   WINHTTP_WEB_SOCKET_MESSAGE_TOO_BIG_CLOSE_STATUS,
;;   WINHTTP_WEB_SOCKET_UNSUPPORTED_EXTENSIONS_CLOSE_STATUS,
;;   WINHTTP_WEB_SOCKET_SERVER_ERROR_CLOSE_STATUS,
;;   WINHTTP_WEB_SOCKET_SECURE_HANDSHAKE_ERROR_CLOSE_STATUS
;; } WINHTTP_WEB_SOCKET_CLOSE_STATUS;


;; WINHTTPAPI HINTERNET WinHttpWebSocketCompleteUpgrade(
;;   HINTERNET hRequest,
;;   DWORD_PTR pContext
;; );
(defcfun (%websocket-complete-upgrade "WinHttpWebSocketCompleteUpgrade" :convention :stdcall) :pointer
  (hreq :pointer)
  (pcxt :pointer))
(defun websocket-complete-upgrade (hrequest)
  "Complete the upgrade to the websocket. returns the websocket handle. The request handle can now be closed."
  (let ((res (%websocket-complete-upgrade hrequest (null-pointer))))
    (if (null-pointer-p res)
	(get-last-error)
	res)))

;; WINHTTPAPI DWORD WinHttpWebSocketQueryCloseStatus(
;;   HINTERNET hWebSocket,
;;   USHORT    *pusStatus,
;;   PVOID     pvReason,
;;   DWORD     dwReasonLength,
;;   DWORD     *pdwReasonLengthConsumed
;; );
(defcfun (%websocket-query-close-status "WinHttpWebSocketQueryCloseStatus" :convention :stdcall) :uint32
  (hwebsocket :pointer)
  (status :pointer)
  (reason :pointer)
  (creason :uint32)
  (reasonlength :pointer))
(defun websocket-query-close-status (hwebsocket)
  "Get the websocket close status." 
  (with-foreign-objects ((status :uint16)
			 (reason :uint8 128)
			 (count :uint32))
    (%websocket-query-close-status hwebsocket
				   status
				   reason
				   128
				   count)
    (values (mem-aref status :uint16)
	    (foreign-string-to-lisp reason :count (mem-aref count :uint32)))))

;; WINHTTPAPI DWORD WinHttpWebSocketReceive(
;;   HINTERNET                      hWebSocket,
;;   PVOID                          pvBuffer,
;;   DWORD                          dwBufferLength,
;;   DWORD                          *pdwBytesRead,
;;   WINHTTP_WEB_SOCKET_BUFFER_TYPE *peBufferType
;; );
(defcfun (%websocket-recv "WinHttpWebSocketReceive" :convention :stdcall) :uint32 
  (hwebsocket :pointer)
  (buffer :pointer)
  (cbuffer :uint32)
  (nbytes :pointer)
  (buffertype :pointer))

(defun websocket-receive (hwebsocket seq &key (start 0) end)
  "Receive some data. 
HWEBSOCKET ::= the websocket handke
SEQ ::= buffer that receives the data
START, END ::= buffer region to fill in
returns values count buffer-type, where
COUNT ::= number of bytes received
BUFFER-TYPE ::= type of message received
" 
  (let ((count (- (or end (length seq)) start)))
    (with-foreign-objects ((buf :uint8 count)
			   (nbytes :uint32)
			   (btype :uint32))
      (let ((sts (%websocket-recv hwebsocket
				  buf
				  count
				  nbytes
				  btype)))
	(unless (zerop sts)
	  (error "Failed ~S" sts)))
      (dotimes (i (mem-aref nbytes :uint32))
	(setf (aref seq (+ start i)) (mem-aref buf :uint8 i)))
      (values (mem-aref nbytes :uint32) (get-buffer-type (mem-aref btype :uint32))))))

;; WINHTTPAPI DWORD WinHttpWebSocketSend(
;;   HINTERNET                      hWebSocket,
;;   WINHTTP_WEB_SOCKET_BUFFER_TYPE eBufferType,
;;   PVOID                          pvBuffer,
;;   DWORD                          dwBufferLength
;; );
(defcfun (%websocket-send "WinHttpWebSocketSend" :convention :stdcall) :uint32
  (hwebsocket :pointer)
  (btype :uint32)
  (buf :pointer)
  (cbuf :uint32))

(defun websocket-send (hwebsocket seq &key (start 0) end buffer-type)
  "Send some data on a websocket.
HWEBSOCKET ::= websocket handle
SEQ ::= buffer
START,END ::= buffer region to send
BUFFER-TYPE ::= type of message"
  (let ((count (- (or end (length seq)) start)))
    (with-foreign-objects ((buf :uint8 count))
      (let ((sts (%websocket-send hwebsocket
				  (if buffer-type
				      (get-buffer-type buffer-type)
				      0)
				  buf
				  count)))
	(unless (zerop sts)
	  (error "Failed ~S" sts)))))
  nil)

;; WINHTTPAPI DWORD WinHttpWebSocketShutdown(
;;   HINTERNET hWebSocket,
;;   USHORT    usStatus,
;;   PVOID     pvReason,
;;   DWORD     dwReasonLength
;; );
(defcfun (%websocket-shutdown "WinHttpWebSocketShutdown" :convention :stdcall) :uint32
  (hwebsocket :pointer)
  (status :uint16)
  (reason :pointer)
  (creason :uint32))

(defun websocket-shutdown (hwebsocket &optional status)
  "Shutdown a websocket"
  (%websocket-shutdown hwebsocket
		       (or status 0)
		       (null-pointer)
		       0))


;; typedef enum _WINHTTP_WEB_SOCKET_BUFFER_TYPE {
;;   WINHTTP_WEB_SOCKET_BINARY_MESSAGE_BUFFER_TYPE,
;;   WINHTTP_WEB_SOCKET_BINARY_FRAGMENT_BUFFER_TYPE,
;;   WINHTTP_WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE,
;;   WINHTTP_WEB_SOCKET_UTF8_FRAGMENT_BUFFER_TYPE,
;;   WINHTTP_WEB_SOCKET_CLOSE_BUFFER_TYPE
;; } WINHTTP_WEB_SOCKET_BUFFER_TYPE;
(defconstant +websocket-binary-message+ 0)
(defconstant +websocket-binary-fragment+ 1)
(defconstant +websocket-utf8-message+ 2)
(defconstant +websocket-utf8-fragment+ 3)
(defconstant +websocket-close+ 4)
(defun get-buffer-type (buffertype)
  (ecase buffertype
    (0 :binary)
    (1 :binary-fragment)
    (2 :utf8)
    (3 :utf8-fragment)
    (4 :close)
    (:binary 0)
    (:binary-fragment 1)
    (:utf8 2)
    (:utf8-fragment 3)
    (:close 4)))


(defconstant +WINHTTP-OPTION-UPGRADE-TO-WEB-SOCKET+ 114)

(defun upgrade-to-websocket (hrequest)
  "Call this before the initial SEND-REQUEST call." 
  (%set-option hrequest 
               +WINHTTP-OPTION-UPGRADE-TO-WEB-SOCKET+
	       (null-pointer)
	       0))

(defmacro with-websocket ((var hostname port &key url https-p) &body body)
  "Bind VAR to a websocket connected to hostname:port/url." 
  (let ((ghttp (gensym))
	(ghconnect (gensym))
	(ghreq (gensym)))
    `(with-http (,ghttp)
       (with-connect (,ghconnect ,ghttp ,hostname ,port)
	 (with-request (,ghreq ,ghconnect :url ,url :https-p ,https-p)
	   (upgrade-to-websocket ,ghreq)
	   (send-request ,ghreq nil :end 0)
	   (receive-response ,ghreq)
	   (let ((,var (websocket-complete-upgrade ,ghreq)))
	     (unwind-protect (progn ,@body)
	       (websocket-close ,var))))))))

