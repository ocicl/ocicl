;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CHUNGA; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/chunga/known-words.lisp,v 1.3 2008/05/29 22:21:09 edi Exp $

;;; Copyright (c) 2006-2010, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :chunga)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +known-words+
    '(;; headers including WebDAV, WebSockets and some de facto standard headers
      "Accept"
      "Accept-CH"
      "Accept-Charset"
      "Accept-Encoding"
      "Accept-Language"
      "Accept-Patch"
      "Accept-Post"
      "Accept-Ranges"
      "Access-Control-Allow-Credentials"
      "Access-Control-Allow-Headers"
      "Access-Control-Allow-Methods"
      "Access-Control-Allow-Origin"
      "Access-Control-Expose-Headers"
      "Access-Control-Max-Age"
      "Access-Control-Request-Headers"
      "Access-Control-Request-Method"
      "Age"
      "Allow"
      "Alt-Svc"
      "Alt-Used"
      "Attribution-Reporting-Eligible"
      "Attribution-Reporting-Register-Source"
      "Attribution-Reporting-Register-Trigger"
      "Authorization"
      "Available-Dictionary"
      "Cache-Control"
      "Clear-Site-Data"
      "Connection"
      "Content-DPR"
      "Content-Digest"
      "Content-Disposition"
      "Content-Encoding"
      "Content-Language"
      "Content-Length"
      "Content-Location"
      "Content-MD5"
      "Content-Range"
      "Content-Security-Policy"
      "Content-Security-Policy-Report-Only"
      "Content-Type"
      "Cookie"
      "Critical-CH"
      "Cross-Origin-Embedder-Policy"
      "Cross-Origin-Opener-Policy"
      "Cross-Origin-Resource-Policy"
      "DAV"
      "DNT"
      "DPR"
      "Date"
      "Depth"
      "Destination"
      "Device-Memory"
      "Dictionary-ID"
      "Downlink"
      "ECT"
      "ETag"
      "Early-Data"
      "Expect"
      "Expect-CT"
      "Expires"
      "Forwarded"
      "From"
      "Host"
      "If"
      "If-Match"
      "If-Modified-Since"
      "If-None-Match"
      "If-Range"
      "If-Unmodified-Since"
      "Integrity-Policy"
      "Integrity-Policy-Report-Only"
      "Keep-Alive"
      "Last-Modified"
      "Link"
      "Location"
      "Lock-Token"
      "Max-Forwards"
      "NEL"
      "No-Vary-Search"
      "Observe-Browsing-Topics"
      "Origin"
      "Origin-Agent-Cluster"
      "Overwrite"
      "Permissions-Policy"
      "Pragma"
      "Prefer"
      "Preference-Applied"
      "Priority"
      "Proxy-Authenticate"
      "Proxy-Authorization"
      "RTT"
      "Range"
      "Referer"
      "Referrer-Policy"
      "Refresh"
      "Report-To"
      "Reporting-Endpoints"
      "Repr-Digest"
      "Retry-After"
      "Save-Data"
      "Sec-Browsing-Topics"
      "Sec-CH-Prefers-Color-Scheme"
      "Sec-CH-Prefers-Reduced-Motion"
      "Sec-CH-Prefers-Reduced-Transparency"
      "Sec-CH-UA"
      "Sec-CH-UA-Arch"
      "Sec-CH-UA-Bitness"
      "Sec-CH-UA-Form-Factors"
      "Sec-CH-UA-Full-Version"
      "Sec-CH-UA-Full-Version-List"
      "Sec-CH-UA-Mobile"
      "Sec-CH-UA-Model"
      "Sec-CH-UA-Platform"
      "Sec-CH-UA-Platform-Version"
      "Sec-CH-UA-WoW64"
      "Sec-Fetch-Dest"
      "Sec-Fetch-Mode"
      "Sec-Fetch-Site"
      "Sec-Fetch-User"
      "Sec-GPC"
      "Sec-Purpose"
      "Sec-Speculation-Tags"
      "Sec-WebSocket-Accept"
      "Sec-WebSocket-Extensions"
      "Sec-WebSocket-Key"
      "Sec-WebSocket-Protocol"
      "Sec-WebSocket-Version"
      "Server"
      "Server-Timing"
      "Service-Worker"
      "Service-Worker-Allowed"
      "Service-Worker-Navigation-Preload"
      "Set-Cookie"
      "Set-Login"
      "SourceMap"
      "Speculation-Rules"
      "Strict-Transport-Security"
      "Supports-Loading-Mode"
      "TE"
      "TimeOut"
      "Timing-Allow-Origin"
      "Tk"
      "Trailer"
      "Transfer-Encoding"
      "Upgrade"
      "Upgrade-Insecure-Requests"
      "Use-As-Dictionary"
      "User-Agent"
      "Vary"
      "Via"
      "Viewport-Width"
      "WWW-Authenticate"
      "Want-Content-Digest"
      "Want-Repr-Digest"
      "Warning"
      "Width"
      "X-Content-Type-Options"
      "X-DNS-Prefetch-Control"
      "X-Forwarded-For"
      "X-Forwarded-Host"
      "X-Forwarded-Proto"
      "X-Frame-Options"
      "X-Permitted-Cross-Domain-Policies"
      "X-Powered-By"
      "X-Robots-Tag"
      "X-XSS-Protection"
      ;; methods including WebDAV
      "CONNECT"
      "COPY"
      "DELETE"
      "GET"
      "HEAD"
      "LOCK"
      "MKCOL"
      "MOVE"
      "OPTIONS"
      "PATCH"
      "POST"
      "PROPFIND"
      "PROPPATCH"
      "PUT"
      "TRACE"
      "UNLOCK"
      ;; protocols
      "HTTP/1.1"
      "HTTP/1.0"
      ;; only a few and only the "preferred MIME names" - see
      ;; <http://www.iana.org/assignments/character-sets> for a
      ;; complete list
      "US-ASCII"
      "ISO-8859-1"
      "UTF-8"
      "UTF-16"
      "UTF-32BE"
      "UTF-32LE")
    "A list of words \(headers, methods, protocols, character sets)
that are typically seen in HTTP communication.  Mostly from RFC 2616,
but includes WebDAV stuff and other things as well."))

(define-constant +string-to-keyword-hash+
  (let ((hash (make-hash-table :test 'equal :size (length +known-words+))))
    (loop for word in +known-words+
          do (setf (gethash word hash) (make-keyword word nil)))
    hash)
  "A hash table which case-insensitively maps the strings from
+KNOWN-WORDS+ to keywords.")

(define-constant +keyword-to-string-hash+
  (let ((hash (make-hash-table :test 'eq :size (length +known-words+))))
    (loop for word in +known-words+
          do (setf (gethash (make-keyword word nil) hash)
                   (string-capitalize word)))
    hash)
  "A hash table which maps keywords derived from +KNOWN-WORDS+ to
capitalized strings.")

(defun as-keyword-if-found (string &key (destructivep t))
  "Checks if the string STRING is found as a keyword and if it is, returns the keyword, otherwise it returns the input string."
  (or (find-symbol (string-upcase string) (find-package "KEYWORD"))
      string))

(defun as-keyword (string &key (destructivep t))
  "Converts the string STRING to a keyword where all characters are
uppercase or lowercase, taking into account the current readtable
case.  Might destructively modify STRING if DESTRUCTIVEP is true which
is the default.  \"Knows\" several HTTP header names and methods and
is optimized to not call INTERN for these."
  (or (gethash string +string-to-keyword-hash+)
      (make-keyword string destructivep)))

(defun as-capitalized-string (keyword)
  "Kind of the inverse of AS-KEYWORD.  Has essentially the same effect
as STRING-CAPITALIZE but is optimized for \"known\" keywords like
:CONTENT-LENGTH or :GET."
  (or (gethash keyword +keyword-to-string-hash+)
      (string-capitalize keyword)))
