;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DRAKMA; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/drakma/request.lisp,v 1.58 2008/05/30 11:30:45 edi Exp $

;;; Copyright (c) 2006-2012, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :drakma)

(defun determine-body-format (headers external-format-in)
  "The default function used by Drakma to determine how the content
body is to be read.  See the docstring of *BODY-FORMAT-FUNCTION* for
more info."
  (handler-case
      (multiple-value-bind (type subtype params)
          (get-content-type headers)
        (when (text-content-type-p type subtype)
          (let* ((charset (parameter-value "charset" params))
                 (name (cond (charset (as-keyword charset))
                             (t external-format-in))))
            (make-external-format name :eol-style :lf))))
    (error (condition)
      (drakma-warn "Problems determining charset \(falling back to binary):~%~A"
                   condition))))

(defun send-content (content stream &optional external-format-out)
  "Sends CONTENT to the stream STREAM as part of the request body
depending on the type of CONTENT."
  (when content
    (cond ((stringp content)
           (setf (flexi-stream-external-format stream) external-format-out)
           (write-string content stream)
           (setf (flexi-stream-external-format stream) +latin-1+))
          ((or (arrayp content) (listp content))
           (write-sequence content stream))
          ((and (streamp content)
                (input-stream-p content)
                (open-stream-p content)
                (subtypep (stream-element-type content) 'octet))
           (let ((buf (make-array +buffer-size+ :element-type 'octet)))
             (loop
              (let ((pos (read-sequence buf content)))
                (when (zerop pos) (return))
                (write-sequence buf stream :end pos)))))
          ((pathnamep content)
           (with-open-file (from content :element-type 'octet)
             ;; calls itself with a stream now
             (send-content from stream)))
          ((or (functionp content)
               (and (symbolp content)
                    (fboundp content)))
           (funcall content stream))
          (t (parameter-error "Don't know how to send content ~S to server." content)))))

(defun make-form-data-function (parameters boundary external-format-out)
  "Creates and returns a closure which can be used as an argument for
SEND-CONTENT to send PARAMETERS as a `multipart/form-data' request
body using the boundary BOUNDARY."
  (lambda (stream)
    (flet ((crlf ()
             "Sends carriage return and linefeed to STREAM."
             (write-char #\Return stream)
             (write-char #\Linefeed stream)))
      (dolist (name/value parameters)
        (destructuring-bind (name . value)
            name/value
          (when (or (pathnamep value)
                    (streamp value)
                    (functionp value))
            (setq value (list value)))
          (format stream "--~A" boundary)
          (crlf)
          (format stream "Content-Disposition: form-data; name=\"~A\"" name)
          (cond ((stringp value)
                 (crlf)
                 (format stream "Content-Type: text/plain; charset=~a" external-format-out)
                 (crlf) (crlf)
                 (setf (flexi-stream-external-format stream) external-format-out)
                 (format stream "~A" value)
                 (setf (flexi-stream-external-format stream) +latin-1+))
                ((null value)
                 (crlf))
                ((and (listp value)
                      (first value)
                      (not (stringp (first value))))
                 (let* ((file-source (first value))
                        (filename (or (getf (rest value) :filename)
                                      (etypecase file-source
                                        (function "user-closure")
                                        (file-stream (or (file-namestring file-source)
                                                         "user-stream"))
                                        (stream "user-stream")
                                        (pathname (file-namestring file-source)))))
                        (content-type (or (getf (rest value) :content-type)
                                          "application/octet-stream")))
                   (format stream "; filename=\"~A\"" filename)
                   (crlf)
                   (format stream "Content-Type: ~A" content-type)
                   (crlf) (crlf)
                   ;; use SEND-CONTENT to send file as binary data
                   (send-content file-source stream)))
                (t (parameter-error
                    "Don't know what to do with name/value pair (~S . ~S) in multipart/form-data body."
                    name value)))
          (crlf)))
      (format stream "--~A--" boundary)
      (crlf))))

(defun %read-body (stream element-type)
  ;; On ABCL, a flexi-stream is not a normal stream. This is caused by
  ;; a bug in ABCL which is supposedly quite difficult to fix. More
  ;; details here: http://abcl.org/trac/ticket/377
  #-abcl
  (declare (stream stream))
  "Helper function to read from stream into a buffer of element-type, which is returned."
  (let ((buffer (make-array +buffer-size+ :element-type element-type))
        (result (make-array +buffer-size+ :element-type element-type :adjustable t))
        (result-length +buffer-size+))
    (loop for index = 0 then size
          for pos = (read-sequence buffer stream)
          for size = (+ index pos)
          do
          (when (>= size result-length)
            (adjust-array result (setf result-length (* result-length 2))))
          (replace result buffer :start1 index :end2 pos)
          while (= pos +buffer-size+)
          finally (adjust-array result size))
    result))

(defun read-body (stream headers textp &key (decode-content t))
  "Reads the message body from the HTTP stream STREAM using the
information contained in HEADERS \(as produced by HTTP-REQUEST).  If
TEXTP is true, the body is assumed to be of content type `text' and
will be returned as a string.  Otherwise an array of octets \(or NIL
for an empty body) is returned.  Returns the optional `trailer' HTTP
headers of the chunked stream \(if any) as a second value."
  (let ((content-length (when-let (value (and (not (header-value :transfer-encoding headers)) ;; see RFC 2616, section 4.4, 3.
                                              (header-value :content-length headers)))
                          (parse-integer value)))
        (element-type (if textp
                        #+:lispworks7.1 'lw:simple-char #-:lispworks7.1 'character
                        'octet)))
    (values (cond ((eql content-length 0) nil)
                  (content-length
                   (setf (flexi-stream-element-type stream) 'octet)
                   (let ((result (make-array content-length :element-type 'octet)))
                     #+:clisp
                     (setf (flexi-stream-element-type stream) 'octet)
                     (read-sequence result stream)
                     (when (and decode-content (header-value :content-encoding headers))
                       (setq result (with-input-from-sequence (s result)
                                      (%read-body (decode-response-stream headers s) 'octet))))
                     (when textp
                       (setf result
                             (octets-to-string result :external-format (flexi-stream-external-format stream))
                             #+:clisp (flexi-stream-element-type stream)
                             #+:clisp element-type))
                     result))
                  (t
                   ;; no content length, read until EOF (or end of chunking)
                   #+:clisp
                   (setf (flexi-stream-element-type stream) element-type)
                   ;; Help the streams on top of chunked streams
                   ;; detect EOF at the end of chunking without trying
                   ;; to reading extra data.
                   (setf (chunked-input-stream-eof-after-last-chunk (flexi-stream-stream stream)) t)
                   (unwind-protect
                        (%read-body (decode-flexi-stream headers stream
                                                         :decode-content decode-content)
                                    element-type)
                     (setf (chunked-input-stream-eof-after-last-chunk (flexi-stream-stream stream)) nil))))
            (chunked-input-stream-trailers (flexi-stream-stream stream)))))

(defun trivial-uri-path (uri-string)
  "If the PRESERVE-URI argument is used, the URI needs to be passed to
  the server in unmodified form.  This function returns just the path
  component of the URI with no URL encoding or other modifications done."
  (cl-ppcre:regex-replace "[^/]+://[^/]*/?" uri-string "/"))

(defun http-request (uri &rest args
                         &key (protocol :http/1.1)
                              (method :get)
                              force-ssl
                              certificate
                              key
                              certificate-password
                              verify
                              (max-depth 10)
                              ca-file
                              ca-directory
                              parameters
                              (url-encoder #'url-encode)
                              content
                              (content-type "application/x-www-form-urlencoded")
                              (content-length nil content-length-provided-p)
                              form-data
                              cookie-jar
                              basic-authorization
                              (user-agent :drakma)
                              (accept "*/*")
                              range
                              (proxy *default-http-proxy*)
                              (no-proxy-domains *no-proxy-domains*)
                              proxy-basic-authorization
                              real-host
                              additional-headers
                              (redirect 5)
                              auto-referer
                              keep-alive
                              (close t)
                              (external-format-out *drakma-default-external-format*)
                              (external-format-in *drakma-default-external-format*)
                              force-binary
                              want-stream
                              stream
                              preserve-uri
                              decode-content ; default to nil for backwards compatibility
                              #+(or abcl clisp lispworks mcl openmcl sbcl)
                              (connection-timeout 20)
                              #+:lispworks7.1 (read-timeout 20)
                              #+(and :lispworks7.1 (not :lw-does-not-have-write-timeout))
                              (write-timeout 20 write-timeout-provided-p)
                              #+:openmcl
                              deadline
                              &aux (unparsed-uri (if (stringp uri) (copy-seq uri) (puri:copy-uri uri))))
  "Sends a HTTP request to a web server and returns its reply.  URI
is where the request is sent to, and it is either a string denoting a
uniform resource identifier or a PURI:URI object.  The scheme of URI
must be `http' or `https'.  The function returns SEVEN values - the
body of the reply \(but see below), the status code as an integer, an
alist of the headers sent by the server where for each element the car
\(the name of the header) is a keyword and the cdr \(the value of the
header) is a string, the URI the reply comes from \(which might be
different from the URI the request was sent to in case of redirects),
the stream the reply was read from, a generalized boolean which
denotes whether the stream should be closed \(and which you can
usually ignore), and finally the reason phrase from the status line as
a string.

PROTOCOL is the HTTP protocol which is going to be used in the
request line, it must be one of the keywords :HTTP/1.0 or
:HTTP/1.1.  METHOD is the method used in the request line, a
keyword \(like :GET or :HEAD) denoting a valid HTTP/1.1 or WebDAV
request method, or :REPORT, as described in the Versioning
Extensions to WebDAV.  Additionally, you can also use the pseudo
method :OPTIONS* which is like :OPTIONS but means that an
\"OPTIONS *\" request line will be sent, i.e. the URI's path and
query parts will be ignored.

If FORCE-SSL is true, SSL will be attached to the socket stream
which connects Drakma with the web server.  Usually, you don't
have to provide this argument, as SSL will be attached anyway if
the scheme of URI is `https'.

CERTIFICATE is the file name of the PEM encoded client certificate to
present to the server when making a SSL connection.  KEY specifies the
file name of the PEM encoded private key matching the certificate.
CERTIFICATE-PASSWORD specifies the pass phrase to use to decrypt the
private key.

VERIFY can be specified to force verification of the certificate that
is presented by the server in an SSL connection.  It can be specified
either as NIL if no check should be performed, :OPTIONAL to verify the
server's certificate if it presented one or :REQUIRED to verify the
server's certificate and fail if an invalid or no certificate was
presented.

MAX-DEPTH can be specified to change the maximum allowed certificate
signing depth that is accepted.  The default is 10.

CA-FILE and CA-DIRECTORY can be specified to set the certificate
authority bundle file or directory to use for certificate validation.

The CERTIFICATE, KEY, CERTIFICATE-PASSWORD, VERIFY, MAX-DEPTH, CA-FILE
and CA-DIRECTORY parameters are ignored for non-SSL requests.  They
are also ignored on LispWorks.

PARAMETERS is an alist of name/value pairs \(the car and the cdr each
being a string) which denotes the parameters which are added to the
query part of the URL or \(in the case of a POST request) comprise the
body of the request.  (But see CONTENT below.)  The values can also be
NIL in which case only the name \(without an equal sign) is used in
the query string.  The name/value pairs are URL-encoded using the
FLEXI-STREAMS external format EXTERNAL-FORMAT-OUT before they are sent
to the server unless FORM-DATA is true in which case the POST request
body is sent as `multipart/form-data' using EXTERNAL-FORMAT-OUT.  The
values of the PARAMETERS alist can also be pathnames, open binary
input streams, unary functions, or lists where the first element is of
one of the former types.  These values denote files which should be
sent as part of the request body.  If files are present in PARAMETERS,
the content type of the request is always `multipart/form-data'.  If
the value is a list, the part of the list behind the first element is
treated as a plist which can be used to specify a content type and/or
a filename for the file, i.e. such a value could look like, e.g.,
\(#p\"/tmp/my_file.doc\" :content-type \"application/msword\"
:filename \"upload.doc\").

URL-ENCODER specifies a custom URL encoder function which will be used
by drakma to URL-encode parameter names and values.  It needs to be a
function of one argument.  The argument is the string to encode, the
return value must be the URL-encoded string.  This can be used if
specific encoding rules are required.

CONTENT, if not NIL, is used as the request body - PARAMETERS is
ignored in this case.  CONTENT can be a string, a sequence of
octets, a pathname, an open binary input stream, or a function
designator.  If CONTENT is a sequence, it will be directly sent
to the server \(using EXTERNAL-FORMAT-OUT in the case of
strings).  If CONTENT is a pathname, the binary contents of the
corresponding file will be sent to the server.  If CONTENT is a
stream, everything that can be read from the stream until EOF
will be sent to the server.  If CONTENT is a function designator,
the corresponding function will be called with one argument, the
stream to the server, to which it should send data.

Finally, CONTENT can also be the keyword :CONTINUATION in which case
HTTP-REQUEST returns only one value - a `continuation' function.  This
function has one required argument and one optional argument.  The
first argument will be interpreted like CONTENT above \(but it cannot
be a keyword), i.e. it will be sent to the server according to its
type.  If the second argument is true, the continuation function can
be called again to send more content, if it is NIL the continuation
function returns what HTTP-REQUEST would have returned.

If CONTENT is a sequence, Drakma will use LENGTH to determine its
length and will use the result for the `Content-Length' header sent to
the server.  You can overwrite this with the CONTENT-LENGTH parameter
\(a non-negative integer) which you can also use for the cases where
Drakma can't or won't determine the content length itself.  You can
also explicitly provide a CONTENT-LENGTH argument of NIL which will
imply that no `Content-Length' header will be sent in any case.  If no
`Content-Length' header is sent, Drakma will use chunked encoding to
send the content body.  Note that this will not work with older web
servers.

Providing a true CONTENT-LENGTH argument which is not a non-negative
integer means that Drakma /must/ build the request body in RAM and
compute the content length even if it would have otherwise used
chunked encoding, for example in the case of file uploads.

CONTENT-TYPE is the corresponding `Content-Type' header to be sent and
will be ignored unless CONTENT is provided as well.

Note that a query already contained in URI will always be sent with
the request line anyway in addition to other parameters sent by
Drakma.

COOKIE-JAR is a cookie jar containing cookies which will
potentially be sent to the server \(if the domain matches, if
they haven't expired, etc.) - this cookie jar will be modified
according to the `Set-Cookie' header\(s) sent back by the server.

BASIC-AUTHORIZATION, if not NIL, should be a list of two strings
\(username and password) which will be sent to the server for
basic authorization.  USER-AGENT, if not NIL, denotes which
`User-Agent' header will be sent with the request.  It can be one
of the keywords :DRAKMA, :FIREFOX, :EXPLORER, :OPERA, or :SAFARI
which denote the current version of Drakma or, in the latter four
cases, a fixed string corresponding to a more or less recent \(as
of August 2006) version of the corresponding browser.  Or it can
be a string which is used directly.

ACCEPT, if not NIL, specifies the contents of the `Accept' header
sent.

RANGE optionally specifies a subrange of the resource to be requested.
It must be specified as a list of two integers which indicate the
start and \(inclusive) end offset of the requested range, in bytes
\(i.e. octets).

If PROXY is not NIL, it should be a string denoting a proxy
server through which the request should be sent.  Or it can be a
list of two values - a string denoting the proxy server and an
integer denoting the port to use \(which will default to 80
otherwise).  Defaults to *default-http-proxy*.
PROXY-BASIC-AUTHORIZATION is used like
BASIC-AUTHORIZATION, but for the proxy, and only if PROXY is
true. If the host portion of the uri is present in the
*no-proxy-domains* or the NO-PROXY-DOMAINS list then the proxy
setting will be ignored for this request.

If NO-PROXY-DOMAINS is set then it will supersede the
*no-proxy-domains* variable. Inserting domains into this list will
allow them to ignore the proxy setting.

If REAL-HOST is not NIL, request is sent to the denoted host instead
of the URI host.  When specified, REAL-HOST supersedes PROXY.

ADDITIONAL-HEADERS is a name/value alist of additional HTTP headers
which should be sent with the request.  Unlike in PARAMETERS, the cdrs
can not only be strings but also designators for unary functions
\(which should in turn return a string) in which case the function is
called each time the header is written.

If REDIRECT is not NIL, it must be a non-negative integer or T.
If REDIRECT is true, Drakma will follow redirects \(return codes
301, 302, 303, or 307) unless REDIRECT is 0.  If REDIRECT is an
integer, it will be decreased by 1 with each redirect.
Furthermore, if AUTO-REFERER is true when following redirects,
Drakma will populate the `Referer' header with the URI that
triggered the redirection, overwriting an existing `Referer'
header \(in ADDITIONAL-HEADERS) if necessary.

If KEEP-ALIVE is T, the server will be asked to keep the
connection alive, i.e. not to close it after the reply has been
sent.  \(Note that this not necessary if both the client and the
server use HTTP 1.1.)  If CLOSE is T, the server is explicitly
asked to close the connection after the reply has been sent.
KEEP-ALIVE and CLOSE are obviously mutually exclusive.

If the message body sent by the server has a text content type, Drakma
will try to return it as a Lisp string.  It'll first check if the
`Content-Type' header denotes an encoding to be used, or otherwise it
will use the EXTERNAL-FORMAT-IN argument.  The body is decoded using
FLEXI-STREAMS.  If FLEXI-STREAMS doesn't know the external format, the
body is returned as an array of octets.  If the body is empty, Drakma
will return NIL.

If the message body doesn't have a text content type or if
FORCE-BINARY is true, the body is always returned as an array of
octets.

If WANT-STREAM is true, the message body is NOT read and instead the
\(open) socket stream is returned as the first return value.  If the
sixth value of HTTP-REQUEST is true, the stream should be closed \(and
not be re-used) after the body has been read.  The stream returned is
a flexi stream \(see http://weitz.de/flexi-streams/) with a chunked
stream \(see http://weitz.de/chunga/) as its underlying stream.  If
you want to read binary data from this stream, read from the
underlying stream which you can get with FLEXI-STREAM-STREAM.

Drakma will usually create a new socket connection for each HTTP
request.  However, you can use the STREAM argument to provide an
open socket stream which should be re-used.  STREAM MUST be a
stream returned by a previous invocation of HTTP-REQUEST where
the sixth return value wasn't true.  Obviously, it must also be
connected to the correct server and at the right position
\(i.e. the message body, if any, must have been read).  Drakma
will NEVER attach SSL to a stream provided as the STREAM
argument.

CONNECTION-TIMEOUT is the time \(in seconds) Drakma will wait until it
considers an attempt to connect to a server as a failure. It is
supported only on some platforms \(currently abcl, clisp, LispWorks,
mcl, openmcl and sbcl). READ-TIMEOUT and WRITE-TIMEOUT are the read
and write timeouts \(in seconds) for the socket stream to the server.
All three timeout arguments can also be NIL \(meaning no timeout), and
they don't apply if an existing stream is re-used.  READ-TIMEOUT
argument is only available for LispWorks, WRITE-TIMEOUT is only
available for LispWorks 5.0 or higher.

DEADLINE, a time in the future, specifies the time until which the
request should be finished.  The deadline is specified in internal
time units.  If the server fails to respond until that time, a
COMMUNICATION-DEADLINE-EXPIRED condition is signalled.  DEADLINE is
only available on CCL 1.2 and later.

If PRESERVE-URI is not NIL, the given URI will not be processed. This
means that the URI will be sent as-is to the remote server and it is
the responsibility of the client to make sure that all parameters are
encoded properly. Note that if this parameter is given, and the
request is not a POST with a content-type of `multipart/form-data',
PARAMETERS will not be used.

If DECODE-CONTENT is not NIL, then the content will automatically be
decoded according to any encodings specified in the Content-Encoding
header. The actual decoding is done by the DECODE-STREAM generic function,
and you can implement new methods to support additional encodings.
Any encodings in Transfer-Encoding, such as chunking, are always performed."
  #+lispworks7.1
  (declare (ignore certificate key certificate-password verify max-depth ca-file ca-directory))
  (unless (member protocol '(:http/1.0 :http/1.1) :test #'eq)
    (parameter-error "Don't know how to handle protocol ~S." protocol))
  (setq uri (cond ((puri:uri-p uri) (puri:copy-uri uri))
                  (t (puri:parse-uri uri))))
  (unless (member method +known-methods+ :test #'eq)
    (parameter-error "Don't know how to handle method ~S." method))
  (unless (member (puri:uri-scheme uri) '(:http :https) :test #'eq)
    (parameter-error "Don't know how to handle scheme ~S." (puri:uri-scheme uri)))
  (when (and close keep-alive)
    (parameter-error "CLOSE and KEEP-ALIVE must not be both true."))
  (when (and form-data (not (member method '(:post :put :report) :test #'eq)))
    (parameter-error "FORM-DATA only makes sense with POST requests."))
  (when range
    (unless (and (listp range)
                 (integerp (first range))
                 (integerp (second range))
                 (<= (first range) (second range)))
      (parameter-error "RANGE parameter must be specified as list of two integers, with the second larger or equal to the first")))
  ;; supersede PROXY with REAL-HOST
  (when real-host
    (setq proxy real-host))
  ;; convert PROXY argument to canonical form
  (when proxy
    (when (atom proxy)
      (setq proxy (list proxy 80))))
  ;; Ignore the proxy for whitelisted hosts.
  (when (member (puri:uri-host uri) no-proxy-domains :test #'string=)
    (setq proxy '()))
  ;; make sure we don't get :CRLF on Windows
  (let ((*default-eol-style* :lf)
        (file-parameters-p (find-if-not (lambda (thing)
                                          (or (stringp thing)
                                              (null thing)))
                                        parameters :key #'cdr))
        parameters-used-p)
    (when (and file-parameters-p (not (or (eq method :post)
                                          (eq method :put))))
      (parameter-error "Don't know how to handle parameters in ~S, as this is not a POST or PUT request."
                       parameters))
    (when (or (eq method :post)
              (eq method :put) ; make Drakma more flexible towards wrongly implemented endpoints
              )
      ;; create content body for POST unless it was provided
      (unless content
        ;; mark PARAMETERS argument as used up, so we don't use it
        ;; again below
        (setq parameters-used-p t)
        (cond ((or form-data file-parameters-p)
               (let ((boundary (format nil "----------~A" (make-random-string))))
                 (setq content (make-form-data-function parameters boundary external-format-out)
                       content-type (format nil "multipart/form-data; boundary=~A" boundary)))
               (unless (or file-parameters-p content-length-provided-p)
                 (setq content-length (or content-length t))))
              (t
               (setq content (alist-to-url-encoded-string parameters external-format-out url-encoder)
                     content-type "application/x-www-form-urlencoded")))))
    (let ((proxying-https-p (and proxy (not stream) (not real-host)
                                 (or force-ssl
                                     (eq :https (puri:uri-scheme uri)))))
           http-stream raw-http-stream must-close done)
      (unwind-protect
          (progn
            (let ((host (or (and proxy (first proxy))
                            (puri:uri-host uri)))
                  (port (cond ((and proxy (not real-host)) (second proxy))
                              ((puri:uri-port uri))
                              (t (default-port uri))))
                  (use-ssl (and (not proxying-https-p)
                                (or force-ssl
                                    (eq (puri:uri-scheme uri) :https)))))
              #+(and :lispworks5.0 :mswindows
                     (not :lw-does-not-have-write-timeout))
              (when use-ssl
                (when (and write-timeout write-timeout-provided-p)
                  (drakma-warn "Disabling WRITE-TIMEOUT because it doesn't mix well with SSL."))
                (setq write-timeout nil))
              (setq http-stream (or stream
                                    #+:lispworks7.1
                                    (comm:open-tcp-stream host port
                                                          :element-type 'octet
                                                          :timeout connection-timeout
                                                          :read-timeout read-timeout
                                                          #-:lw-does-not-have-write-timeout
                                                          :write-timeout
                                                          #-:lw-does-not-have-write-timeout
                                                          write-timeout
                                                          :errorp t)
                                    #-:lispworks7.1
                                    (usocket:socket-stream
                                     (usocket:socket-connect host port
                                                             :element-type 'octet
                                                             #+:openmcl :deadline
                                                             #+:openmcl deadline
                                                             #+(or abcl clisp lispworks mcl openmcl sbcl)
                                                             :timeout
                                                             #+(or abcl clisp lispworks mcl openmcl sbcl)
                                                             connection-timeout
                                                             :nodelay :if-supported)))
                    raw-http-stream http-stream)
              #+:openmcl
              (when deadline
                ;; it is correct to set the deadline here even though
                ;; it may have been initialized by SOCKET-CONNECT
                ;; already - the stream may have been passed in by the
                ;; user and the user may want to adjust the deadline
                ;; for every request
                (setf (ccl:stream-deadline http-stream) deadline))
            (labels ((write-http-line (fmt &rest args)
                       (when *header-stream*
                         (format *header-stream* "~?~%" fmt args))
                       (format http-stream "~?~C~C" fmt args #\Return #\Linefeed))
                     (write-header (name value-fmt &rest value-args)
                       (write-http-line "~A: ~?" name value-fmt value-args))
                     (wrap-stream (http-stream)
                       (make-flexi-stream (make-chunked-stream http-stream)
                                          :external-format +latin-1+)))
              (when (and use-ssl
                         ;; don't attach SSL to existing streams
                         (not stream))
                #+:lispworks7.1
                (comm:attach-ssl http-stream
                                 :ssl-side :client
                                 #-(or lispworks4 lispworks5 lispworks6)
                                 :tlsext-host-name
                                 #-(or lispworks4 lispworks5 lispworks6)
                                 (puri:uri-host uri))
                #-:lispworks7.1
                (setq http-stream (make-ssl-stream http-stream
                                                   :hostname (puri:uri-host uri)
                                                   :certificate certificate
                                                   :key key
                                                   :certificate-password certificate-password
                                                   :verify verify
                                                   :max-depth max-depth
                                                   :ca-file ca-file
                                                   :ca-directory ca-directory)))
              (cond (stream
                     (setf (flexi-stream-element-type http-stream)
                           #+:lispworks6 'lw:simple-char #-:lispworks6 'character
                           (flexi-stream-external-format http-stream) +latin-1+))
                    (t
                     (setq http-stream (wrap-stream http-stream))))
              (when proxying-https-p
                ;; set up a tunnel through the proxy server to the
                ;; final destination
                (write-http-line "CONNECT ~A:~:[443~;~:*~A~] HTTP/1.1"
                                 (puri:uri-host uri) (puri:uri-port uri))
                (write-http-line "Host: ~@[[~*~]~A~@[]~*~]:~:[443~;~:*~A~]"
                                 (puri:uri-is-ip6 uri)
                                 (puri:uri-host uri)
                                 (puri:uri-is-ip6 uri)
                                 (puri:uri-port uri))
                (write-http-line "")
                (force-output http-stream)
                ;; check we get a 200 response before proceeding
                (unless (eql (second (read-status-line http-stream *header-stream*)) 200)
                  (error "Unable to establish HTTPS tunnel through proxy."))
                ;; got a connection; we have to read a blank line,
                ;; turn on SSL, and then we can transmit
                (read-line* http-stream)
                #+:lispworks7.1
                (comm:attach-ssl raw-http-stream
                                 :ssl-side :client
                                 #-(or lispworks4 lispworks5 lispworks6)
                                 :tlsext-host-name
                                 #-(or lispworks4 lispworks5 lispworks6)
                                 (puri:uri-host uri))
                #-:lispworks7.1
                (setq http-stream (wrap-stream
                                   (make-ssl-stream raw-http-stream
                                                    :hostname (puri:uri-host uri)
                                                    :certificate certificate
                                                    :key key
                                                    :certificate-password certificate-password
                                                    :verify verify
                                                    :max-depth max-depth
                                                    :ca-file ca-file
                                                    :ca-directory ca-directory))))
              (when-let (all-get-parameters
                         (and (not preserve-uri)
                              (append (dissect-query (puri:uri-query uri))
                                      (and (not parameters-used-p) parameters))))
                (setf (puri:uri-query uri)
                      (alist-to-url-encoded-string all-get-parameters external-format-out url-encoder)))
              (when (eq method :options*)
                ;; special pseudo-method
                (setf method :options
                      (puri:uri-path uri) "*"
                      (puri:uri-query uri) nil))
              (write-http-line "~A ~A ~A"
                               (string-upcase method)
                               (if (and preserve-uri
                                        (stringp unparsed-uri))
                                   (trivial-uri-path unparsed-uri)
                                   (puri:render-uri (if (and proxy
                                                             (null stream)
                                                             (not proxying-https-p)
                                                             (not real-host))
                                                        uri
                                                        (make-instance 'puri:uri
                                                                       :path (puri:uri-path uri)
                                                                       :parsed-path (puri:uri-parsed-path uri)
                                                                       :query (puri:uri-query uri)
                                                                       :escaped t))
                                                    nil))
                               (string-upcase protocol))
              (when (not (assoc "Host" additional-headers :test #'string-equal))
                (write-header "Host" "~@[[~*~]~A~@[]~*~]~@[:~A~]"
                              (puri:uri-is-ip6 uri)
                              (puri:uri-host uri)
                              (puri:uri-is-ip6 uri)
                              (non-default-port uri)))
              (when user-agent
                (write-header "User-Agent" "~A" (user-agent-string user-agent)))
              (when basic-authorization
                (write-header "Authorization" "Basic ~A"
                              (base64:string-to-base64-string
                               (format nil "~A:~A"
                                       (first basic-authorization)
                                       (second basic-authorization)))))
              (when (and proxy proxy-basic-authorization)
                (write-header "Proxy-Authorization" "Basic ~A"
                              (base64:string-to-base64-string
                               (format nil "~A:~A"
                                       (first proxy-basic-authorization)
                                       (second proxy-basic-authorization)))))
              (when accept
                (write-header "Accept" "~A" accept))
              (when range
                (write-header "Range" "bytes=~A-~A" (first range) (second range)))
              (when cookie-jar
                ;; write all cookies in one fell swoop, so even Sun's
                ;; web server has a chance to get it
                (when-let (cookies (loop for cookie in (cookie-jar-cookies cookie-jar)
                                         when (send-cookie-p cookie uri force-ssl)
                                         collect (cookie-name cookie) and
                                         collect (cookie-value cookie)))
                  (write-header "Cookie" "~{~A=~A~^; ~}" cookies)))
              (when keep-alive
                (write-header "Connection" "Keep-Alive"))
              (when close
                (setq must-close close)
                (write-header "Connection" "close"))
              (loop for (name . value) in additional-headers
                    do (write-header name "~A"
                                     (cond ((or (functionp value)
                                                (and (symbolp value)
                                                     (fboundp value)))
                                            (funcall value))
                                           (t value))))
              (when content
                (when content-type
                  (write-header "Content-Type" "~A" content-type))
                (when (or (and (not content-length-provided-p)
                               (stringp content))
                          (and content-length
                               (not (or (and (integerp content-length)
                                             (not (minusp content-length)))
                                        (typep content '(or (vector octet) list))
                                        (eq content :continuation)))))
                  ;; CONTENT-LENGTH forces us to compute request body
                  ;; in RAM
                  (setq content
                        (with-output-to-sequence (bin-out)
                          (let ((out (make-flexi-stream bin-out :external-format +latin-1+)))
                            (send-content content out external-format-out)))))
                (when (and (or (not content-length-provided-p)
                               (eq content-length t))
                           (typep content '(or (vector octet) list)))
                  (setq content-length (length content)))
                (cond (content-length
                       (write-header "Content-Length" "~D" content-length))
                      (t
                       (write-header "Transfer-Encoding" "chunked"))))
              ;; end of request headers
              (when *header-stream*
                (terpri *header-stream*))
              (format http-stream "~C~C" #\Return #\Linefeed)
              (force-output http-stream)
              (when (and content (null content-length))
                (setf (chunked-stream-output-chunking-p
                       (flexi-stream-stream http-stream)) t))
              (labels ((finish-request (content &optional continuep)
                         (send-content content http-stream external-format-out)
                         (when continuep
                           (force-output http-stream)
                           (return-from finish-request))
                         (setf (chunked-stream-output-chunking-p
                                (flexi-stream-stream http-stream)) nil)
                         (finish-output http-stream)
                         (with-character-stream-semantics
                           (multiple-value-bind (server-protocol status-code status-text)
                               ;; loop until status is NOT 100
                               (loop for (server-protocol status-code status-text)
                                     = (read-status-line http-stream *header-stream*)
                                     when (= status-code 100)
                                     ;; ignore headers sent until non-100 status is seen
                                     do (read-http-headers http-stream *header-stream*)
                                     until (/= status-code 100)
                                     finally (return (values server-protocol status-code status-text)))
                             (let ((headers (read-http-headers http-stream *header-stream*))
                                   body external-format-body)
                               (let ((connections (header-value :connection headers)))
                                 (when connections
                                   (setq connections (split-tokens connections)))
                                 (when (or (member "close" connections :test #'string-equal)
                                           (not (or (and (eq protocol :http/1.1)
                                                         (eq server-protocol :http/1.1))
                                                    (member "Keep-Alive" connections
                                                            :test #'string-equal))))
                                   (setq must-close t)))
                               (when cookie-jar
                                 (update-cookies (get-cookies headers uri) cookie-jar))
                               (when (and redirect
                                          (member status-code +redirect-codes+)
                                          (header-value :location headers))
                                 (unless (or (eq redirect t)
                                             (and (integerp redirect)
                                                  (plusp redirect)))
                                   (cerror "Continue anyway."
                                           'drakma-simple-error
                                           :format-control "Status code was ~A, but ~
~:[REDIRECT is ~S~;redirection limit has been exceeded~]."
                                           :format-arguments (list status-code (integerp redirect) redirect)))
                                 (when auto-referer
                                   (setq additional-headers (set-referer uri additional-headers)))
                                 (let* ((location (header-value :location headers))
                                        (new-uri (let (puri:*strict-parse*)
                                                   (puri:merge-uris location uri)))
                                        ;; can we re-use the stream?
                                        (old-server-p (and (string= (puri:uri-host new-uri)
                                                                    (puri:uri-host uri))
                                                           (eql (puri:uri-port new-uri)
                                                                (puri:uri-port uri))
                                                           (eq (puri:uri-scheme new-uri)
                                                               (puri:uri-scheme uri)))))
                                   (unless old-server-p
                                     (setq must-close t
                                           want-stream nil))
                                   ;; try to re-use the stream, but only
                                   ;; if the user hasn't opted for a
                                   ;; connection which is always secure
                                   (let ((re-use-stream (and old-server-p
                                                             (not must-close)
                                                             (not force-ssl))))
                                     ;; close stream if we can't re-use it
                                     (unless re-use-stream
                                       (ignore-errors (close http-stream)))
                                     (setq done t)
                                     (return-from http-request
                                       (let ((method (if (and (member status-code +redirect-to-get-codes+)
                                                              (member method +redirect-to-get-methods+))
                                                         :get
                                                         method)))
                                         (apply #'http-request new-uri
                                                :method method
                                                :redirect (cond ((integerp redirect) (1- redirect))
                                                                (t redirect))
                                                :stream (and re-use-stream http-stream)
                                                :additional-headers (remove "Authorization" additional-headers
                                                                            :test 'string-equal :key 'car)
                                                :parameters parameters
                                                :preserve-uri t
                                                :form-data (if (eq method :get)
                                                               nil
                                                               form-data)
                                                args))))))
                               (let ((transfer-encodings (header-value :transfer-encoding headers)))
                                 (when transfer-encodings
                                   (setq transfer-encodings (split-tokens transfer-encodings)))
                                 (when (member "chunked" transfer-encodings :test #'equalp)
                                   (setf (chunked-stream-input-chunking-p
                                          (flexi-stream-stream http-stream)) t)))
                               (when (setq external-format-body
                                           (and (not force-binary)
                                                (funcall *body-format-function*
                                                         headers external-format-in)))
                                 (setf (flexi-stream-external-format http-stream)
                                       external-format-body))
                               (when force-binary
                                 (setf (flexi-stream-element-type http-stream) 'octet))
                               (unless (or want-stream
                                           (eq method :head)
                                           (= status-code 204))
                                 (let (trailers)
                                   (multiple-value-setq (body trailers)
                                     (read-body http-stream headers external-format-body
                                                :decode-content decode-content))
                                   (when trailers
                                     (drakma-warn "Adding trailers from chunked encoding to HTTP headers.")
                                     (setq headers (nconc headers trailers)))))
                               (setq done t)
                               (values (if want-stream
                                           (decode-flexi-stream headers http-stream
                                                                :decode-content decode-content)
                                           body)
                                       status-code
                                       headers
                                       uri
                                       http-stream
                                       must-close
                                       status-text))))))
                (when (eq content :continuation)
                  (return-from http-request #'finish-request))
                (finish-request content)))))
        ;; the cleanup form of the UNWIND-PROTECT above
        (when (and http-stream
                   (or (not done)
                       (and must-close
                            (not want-stream)))
                   (not (eq content :continuation)))
          (ignore-errors (close http-stream)))))))
