;;;; Copyright (c) Frank James 2017 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:winhttp)

(defmacro with-http ((var &optional user-agent proxy) &body body)
  "Evaluate body with VAR bound to a session handle."
  `(let ((,var (http-open ,user-agent ,proxy)))
     (unwind-protect (progn ,@body)
       (close-handle ,var))))

(defmacro with-connect ((var http hostname port) &body body)
  "Evaluate body with VAR bound to a connection handle."
  `(let ((,var (http-connect ,http ,hostname ,port)))
     (unwind-protect (progn ,@body)
       (close-handle ,var))))

(defmacro with-request ((var hconn &key verb url https-p) &body body)
  "Evaludate body with VAR bound to a request handle."
  `(let ((,var (http-open-request ,hconn
				  :verb ,verb
				  :url ,url
				  :https-p ,https-p)))
     (unwind-protect (progn ,@body)
       (close-handle ,var))))
		     
(defun query-content-length (headers)
  "Returns content length as specified in header."
  (dolist (h headers)
    (destructuring-bind (hname hval) h
      (when (string-equal hname "Content-Length")
	(return-from query-content-length (parse-integer hval)))))
  nil)

(defparameter *status-cb-types*
  '((:RESOLVING-NAME          #x00000001)
    (:NAME-RESOLVED           #x00000002)
    (:CONNECTING-TO-SERVER    #x00000004)
    (:CONNECTED-TO-SERVER     #x00000008)
    (:SENDING-REQUEST         #x00000010)
    (:REQUEST-SENT            #x00000020)
    (:RECEIVING-RESPONSE      #x00000040)
    (:RESPONSE-RECEIVED       #x00000080)
    (:CLOSING-CONNECTION      #x00000100)
    (:CONNECTION-CLOSED       #x00000200)
    (:HANDLE-CREATED          #x00000400)
    (:HANDLE-CLOSING          #x00000800)
    (:DETECTING-PROXY         #x00001000)
    (:REDIRECT                #x00004000)
    (:INTERMEDIATE-RESPONSE   #x00008000)
    (:SECURE-FAILURE          #x00010000)
    (:HEADERS-AVAILABLE       #x00020000)
    (:DATA-AVAILABLE          #x00040000)
    (:READ-COMPLETE           #x00080000)
    (:WRITE-COMPLETE          #x00100000)
    (:REQUEST-ERROR           #x00200000)
    (:SENDREQUEST-COMPLETE    #x00400000)
    (:GETPROXYFORURL-COMPLETE #x01000000)
    (:CLOSE-COMPLETE          #x02000000)
    (:SHUTDOWN-COMPLETE       #x04000000)))

(defmacro define-status-callback (name (hinternet context status infop infolen) &body body)
  "Define a foreign callback function which can be used to receive http request status updates.
See MSDN for information on WinHttpSetStatusCallback function. Use SET-STATUS-CALLBACK to register 
this with a request handle.

HINTERNET ::= handle 
CONTEXT ::= pointer to DWORD context 
STATUS ::= symbol naming status update type 
INFOP ::= pointer to info buffer
INFOLEN ::= length of info buffer
"
  (let ((gstatus (gensym)))
    `(defcallback ,name :void
         ((,hinternet :pointer)
          (,context :pointer)
          (,gstatus :uint32)
          (,infop :pointer)
          (,infolen :uint32))
       (let ((,status (first (find ,gstatus *status-cb-types* :key #'second))))
         ,@body))))

;; I got this list by doing a (maphash (lambda (k v) (list k (string k))) babel::*string-vector-mappings*)
;; We can support multiple string mappings to the same keyword identifier
;; by adding more strings to the end of the list.
;; e.g. both "UCS-2" and "UCS2" both map to :UCS-2 
(defparameter *charsets*
  '((:US-ASCII "US-ASCII")
    (:ASCII "ASCII")
    (:IBM-037 "IBM-037")
    (:EBCDIC-US "EBCDIC-US")
    (:EBCDIC-INTERNATIONAL "EBCDIC-INTERNATIONAL")
    (:LATIN1 "LATIN1")
    (:LATIN-1 "LATIN-1")
    (:ISO-8859-1 "ISO-8859-1")
    (:LATIN2 "LATIN2")
    (:LATIN-2 "LATIN-2")
    (:ISO-8859-2 "ISO-8859-2")
    (:LATIN3 "LATIN3")
    (:LATIN-3 "LATIN-3")
    (:ISO-8859-3 "ISO-8859-3")
    (:LATIN4 "LATIN4")
    (:LATIN-4 "LATIN-4")
    (:ISO-8859-4 "ISO-8859-4")
    (:CYRILLIC "CYRILLIC")
    (:ISO-8859-5 "ISO-8859-5")
    (:ARABIC "ARABIC")
    (:ISO-8859-6 "ISO-8859-6")
    (:GREEK "GREEK")
    (:ISO-8859-7 "ISO-8859-7")
    (:HEBREW "HEBREW")
    (:ISO-8859-8 "ISO-8859-8")
    (:LATIN5 "LATIN5")
    (:LATIN-5 "LATIN-5")
    (:ISO-8859-9 "ISO-8859-9")
    (:LATIN6 "LATIN6")
    (:LATIN-6 "LATIN-6")
    (:ISO-8859-10 "ISO-8859-10")
    (:ISO-8859-11 "ISO-8859-11")
    (:ISO-8859-13 "ISO-8859-13")
    (:LATIN8 "LATIN8")
    (:LATIN-8 "LATIN-8")
    (:ISO-8859-14 "ISO-8859-14")
    (:LATIN9 "LATIN9")
    (:LATIN-9 "LATIN-9")
    (:ISO-8859-15 "ISO-8859-15")
    (:LATIN10 "LATIN10")
    (:LATIN-10 "LATIN-10")
    (:ISO-8859-16 "ISO-8859-16")
    (:UTF-8 "UTF-8")
    (:UTF-8B "UTF-8B")
    (:UTF-16 "UTF-16")
    (:UTF-16/LE "UTF-16/LE")
    (:UTF-16LE "UTF-16LE")
    (:UTF-16/BE "UTF-16/BE")
    (:UTF-16BE "UTF-16BE")
    (:UCS-4 "UCS-4")
    (:UTF-32 "UTF-32")
    (:UCS-4/LE "UCS-4/LE")
    (:UCS-4LE "UCS-4LE")
    (:UTF-32/LE "UTF-32/LE")
    (:UTF-32LE "UTF-32LE")
    (:UCS-4/BE "UCS-4/BE")
    (:UCS-4BE "UCS-4BE")
    (:UTF-32/BE "UTF-32/BE")
    (:UTF-32BE "UTF-32BE")
    (:UCS-2 "UCS-2" "UCS2")
    (:UCS-2/LE "UCS-2/LE")
    (:UCS-2LE "UCS-2LE")
    (:UCS-2/BE "UCS-2/BE")
    (:UCS-2BE "UCS-2BE")
    (:WINDOWS-1251 "WINDOWS-1251")
    (:CP1251 "CP1251")
    (:WINDOWS-1252 "WINDOWS-1252")
    (:CP1252 "CP1252")
    (:EUCJP "EUCJP")
    (:CP932 "CP932")
    (:GBK "GBK")
    (:KOI8-RU "KOI8-RU")
    (:KOI8-R "KOI8-R")
    (:KOI8-U "KOI8-U")))
(defun charset-by-name (str)
  "Lookup a charset identifier by name. We only support the charsets 
supported by babel. We do a lookup by name because we don't want to be 
interning strings received from the web server."
  (car (find-if (lambda (cs)
		  (some (lambda (s)
			  (string-equal s str))
			(cdr cs)))
		*charsets*)))

(defun get-content-charset (headers)
  "Try and get the content charset from the headers. We need this otherwise 
babel may not be able to decode the text."
  (let ((str (second (find "Content-Type" headers :key #'car :test #'string-equal))))
    (when str
      (let ((pos (search "charset=" str :test #'string-equal)))
	(when pos
	  (incf pos 8)
	  (let ((epos (position #\; str :start pos :test #'char=)))
	    (charset-by-name (string-trim " " (subseq str pos epos)))))))))
		   
(defun http-request (url &key (method :get) 
                           post-data (post-start 0) post-end 
                           rawp headers timeout ignore-certificates-p
                           statuscb recv-buf (recv-start 0) recv-end proxy)
  "Send HTTP request to server. 
URL ::= string in format [http|https://][username:password@]hostname[:port][/url]
METHOD ::= HTTP verb e.g. :GET, :POST etc 
POST-DATA ::= if specified, is an octet vector sent as post data. Uses region bounded 
by POST-START and POST-END.
RAWP ::= if true returns octets otherwise return data is parsed as text.
HEADERS ::= list of (header &optional value)* extra headers to add.
TIMEOUT ::= integer milliseconds to wait for connection and receive.
IGNORE-CERTIFICATES-P ::= if true will set option flags to ignore certificate errors.
STATUSCB ::= if non-nil, is a symbol naming a callback defined using define-status-callback.
This will be invoked to inform various status messages. 
RECV-BUF ::= if provided, is an octet vector that receives the reply body. 
If not supplied a buffer is allocated. Uses region bounded by RECV-START and RECV-END.
PROXY ::= NAME:PORT of proxy server to use ie 127.0.0.1:8080 or localhost:8080
Returns values return-data status-code headers content-length.
"

  (let ((comp (crack-url url)))
    (with-http (hsession nil proxy)
      (when (eq (getf comp :scheme) :https)
        (set-secure-protocols hsession :tls1 t :tls1-1 t :tls1-2 t))
      
      (with-connect (hconn hsession (getf comp :hostname) (getf comp :port))
        (with-request (hreq hconn
                            :verb method
                            :url (getf comp :url)
                            :https-p (eq (getf comp :scheme) :https))
          
          (when statuscb
            (set-status-callback hreq (get-callback statuscb)))
          
          (let ((user (getf comp :username))
                (pass (getf comp :password)))
            (when (and user pass)
              (set-credentials hreq user pass)))
          
          (dolist (h headers)
            (add-request-headers hreq (format nil "~A: ~A"
                                              (first h)
                                              (or (second h) ""))))
          
          (when (eq (getf comp :scheme) :https)
            (when ignore-certificates-p
              (set-ignore-certificates hreq)))
          
          (when timeout (set-timeouts hreq :connect timeout :recv timeout))
          
          (send-request hreq 
                        (if (stringp post-data)
                            (babel:string-to-octets post-data)
                            post-data)
                        :start post-start :end post-end)
          (receive-response hreq)
          (let* ((headers (query-headers hreq))
                 (status (query-status-code hreq))
                 (len (if (integerp recv-buf) recv-buf (* 32 1024)))
                 (count 0)
                 (resp (if (vectorp recv-buf)
                           recv-buf
                           (make-array len :element-type '(unsigned-byte 8) :adjustable t)))
		 (resp-start recv-start)
		 (resp-end recv-end))
            (when (> len 0)
              (do ((done nil))
                  (done)

                ;; if we have allocated a response buffer, check it is large enough
                ;; if there isn't enough space, use adjust-array to increase the buffer
                (let ((nda (query-data-available hreq)))
                  (when (and (not (vectorp recv-buf))
                             (> nda (- (length resp) count)))
                    (adjust-array resp (+ count nda))))

                ;; receive response data 
                (let ((n (read-data hreq resp :start (+ resp-start count) :end resp-end)))
                  (when (zerop n)
                    (setf done t))
                  (incf count n)

		  (when (and resp-end (> (- resp-end resp-start) count))
		    (setf done t)))))
            (values
             (if rawp
                 resp
                 (let ((encoding (or (get-content-charset headers) :utf-8)))
		   (babel:octets-to-string resp :end count :encoding encoding)))
             status
             headers
             count))))))) 
