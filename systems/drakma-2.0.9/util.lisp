;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DRAKMA; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/drakma/util.lisp,v 1.36 2008/05/30 11:30:45 edi Exp $

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

#+:lispworks
(require "comm")

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'lw:when-let))

#-:lispworks
(defmacro when-let ((var expr) &body body)
  "Evaluates EXPR, binds it to VAR, and executes BODY if VAR has
a true value."
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'lw:with-unique-names))

#-:lispworks
(defmacro with-unique-names ((&rest bindings) &body body)
  "Syntax: WITH-UNIQUE-NAMES ( { var | (var x) }* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  `(let ,(mapcar #'(lambda (binding)
                     (check-type binding (or cons symbol))
                     (if (consp binding)
                       (destructuring-bind (var x) binding
                         (check-type var symbol)
                         `(,var (gensym ,(etypecase x
                                          (symbol (symbol-name x))
                                          (character (string x))
                                          (string x)))))
                       `(,binding (gensym ,(symbol-name binding)))))
                 bindings)
         ,@body))

(defun ends-with-p (seq suffix &key (test #'char-equal))
  "Returns true if the sequence SEQ ends with the sequence
SUFFIX.  Individual elements are compared with TEST."
  (let ((mismatch (mismatch seq suffix :from-end t :test test)))
    (or (null mismatch)
        (= mismatch (- (length seq) (length suffix))))))

(defun starts-with-p (seq prefix &key (test #'char-equal))
  "Returns true if the sequence SEQ starts with the sequence
PREFIX whereby the elements are compared using TEST."
  (let ((mismatch (mismatch seq prefix :test test)))
    (or (null mismatch)
        (= mismatch (length prefix)))))

(defun url-encode (string external-format)
  "Returns a URL-encoded version of the string STRING using the
external format EXTERNAL-FORMAT."
  (with-output-to-string (out)
    (loop for octet across (string-to-octets (or string "")
                                             :external-format external-format)
          for char = (code-char octet)
          do (cond ((or (char<= #\0 char #\9)
                        (char<= #\a char #\z)
                        (char<= #\A char #\Z)
                        (find char "$-_.!*'()," :test #'char=))
                     (write-char char out))
                   ((char= char #\Space)
                     (write-char #\+ out))
                   (t (format out "%~2,'0x" (char-code char)))))))

(defun alist-to-url-encoded-string (alist external-format url-encoder)
  "ALIST is supposed to be an alist of name/value pairs where both
names and values are strings \(or, for values, NIL).  This function
returns a string where this list is represented as for the content
type `application/x-www-form-urlencoded', i.e. the values are
URL-encoded using the external format EXTERNAL-FORMAT, the pairs are
joined with a #\\& character, and each name is separated from its
value with a #\\= character.  If the value is NIL, no #\\= is used."
  (with-output-to-string (out)
    (loop for first = t then nil
          for (name . value) in alist
          unless first do (write-char #\& out)
          do (format out "~A~:[~;=~A~]"
                      (funcall url-encoder name external-format)
                      value
                      (funcall url-encoder value external-format)))))

(defun default-port (uri)
  "Returns the default port number for the \(PURI) URI URI.
Works only with the http and https schemes."
  (ecase (puri:uri-scheme uri)
    (:http 80)
    (:https 443)))

(defun non-default-port (uri)
  "If the \(PURI) URI specifies an explicit port number which is
different from the default port its scheme, this port number is
returned, otherwise NIL."
  (when-let (port (puri:uri-port uri))
    (when (/= port (default-port uri))
      port)))

(defun user-agent-string (token)
  "Returns a corresponding user agent string if TOKEN is one of
the keywords :DRAKMA, :FIREFOX, :EXPLORER, :OPERA, or :SAFARI.
Returns TOKEN itself otherwise."
  (case token
    (:drakma
     (format nil "Drakma/~A (~A~@[ ~A~]; ~A;~@[ ~A;~] http://weitz.de/drakma/)"
             *drakma-version*
             (or (lisp-implementation-type) "Common Lisp")
             (or (lisp-implementation-version) "")
             (or #-:clisp (software-type)
                 #+(or :win32 :mswindows) "Windows"
                 #-(or :win32 :mswindows) "Unix")
             (or #-:clisp (software-version))))
    (:firefox
     "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.0.6) Gecko/20060728 Firefox/1.5.0.6")
    (:explorer
     "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; .NET CLR 1.1.4322; .NET CLR 2.0.50727)")
    (:opera
     "Opera/9.01 (Windows NT 5.1; U; en)")
    (:safari
     "Mozilla/5.0 (Macintosh; U; Intel Mac OS X; en) AppleWebKit/418.8 (KHTML, like Gecko) Safari/419.3")
    (otherwise token)))

(defun header-value (name headers)
  "If HEADERS is an alist of headers as returned by HTTP-REQUEST
and NAME is a keyword naming a header, this function returns the
corresponding value of this header \(or NIL if it's not in
HEADERS)."
  (cdr (assoc name headers :test #'eq)))

(defun parameter-present-p (name parameters)
  "If PARAMETERS is an alist of parameters as returned by, for
example, READ-TOKENS-AND-PARAMETERS and NAME is a string naming a
parameter, this function returns the full parameter \(name and
value) - or NIL if it's not in PARAMETERS."
  (assoc name parameters :test #'string-equal))

(defun parameter-value (name parameters)
  "If PARAMETERS is an alist of parameters as returned by, for
example, READ-TOKENS-AND-PARAMETERS and NAME is a string naming a
parameter, this function returns the value of this parameter - or
NIL if it's not in PARAMETERS."
  (cdr (parameter-present-p name parameters)))

(defun make-random-string (&optional (length 50))
  "Generates and returns a random string length LENGTH.  The
string will consist solely of decimal digits and ASCII letters."
  (with-output-to-string (s)
    (dotimes (i length)
      (write-char (ecase (random 5)
                    ((0 1) (code-char (+ #.(char-code #\a) (random 26))))
                    ((2 3) (code-char (+ #.(char-code #\A) (random 26))))
                    ((4) (code-char (+ #.(char-code #\0) (random 10)))))
                  s))))

(defun safe-parse-integer (string)
  "Like PARSE-INTEGER, but returns NIL instead of signalling an error."
  (ignore-errors (parse-integer string)))

(defun interpret-as-month (string)
  "Tries to interpret STRING as a string denoting a month and returns
the corresponding number of the month.  Accepts three-letter
abbreviations like \"Feb\" and full month names likes \"February\".
Finally, the function also accepts strings representing integers from
one to twelve."
  (or (when-let (pos (position (subseq string 0 (min 3 (length string)))
                               '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                       "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                               :test #'string=))
        (1+ pos))
      (when-let (num (safe-parse-integer string))
        (when (<= 1 num 12)
          num))))

(defun interpret-as-time-zone (string)
  "Tries to interpret STRING as a time zone abbreviation which can
either be something like \"PST\" or \"GMT\" with an offset like
\"GMT-02:00\"."
  (or (cdr (assoc string *time-zone-map* :test #'string=))
      (cl-ppcre:register-groups-bind (sign hours minutes) ("(?:GMT|)\\s*([+-]?)(\\d\\d):?(\\d\\d)" string)
        (* (if (equal sign "-") 1 -1)
           (+ (parse-integer hours) (/ (parse-integer minutes) 60))))
      (cookie-date-parse-error "Can't interpret ~S as a time zone." string)))

(defun set-referer (referer-uri &optional alist)
  "Returns a fresh copy of the HTTP header list ALIST with the
`Referer' header set to REFERER-URI.  If REFERER-URI is NIL, the
result will be a list of headers without a `Referer' header."
  (let ((alist-sans-referer (remove "Referer" alist :key #'car :test #'string=)))
    (cond (referer-uri (acons "Referer" referer-uri alist-sans-referer))
          (t alist-sans-referer))))

(defun text-content-type-p (type subtype)
  "Returns a true value iff the combination of TYPE and SUBTYPE
matches an entry of *TEXT-CONTENT-TYPES*.  See docstring of
*TEXT-CONTENT-TYPES* for more info."
  (loop for (candidate-type . candidate-subtype) in *text-content-types*
        thereis (and (or (null candidate-type)
                         (string-equal type candidate-type))
                     (or (null candidate-subtype)
                         (string-equal subtype candidate-subtype)))))

(defmacro with-sequence-from-string ((stream string) &body body)
  "Kludge to make Chunga tokenizing functionality usable.  Works like
  WITH-INPUT-FROM-STRING, but creates a sequence of octets that works
  with CHUNGA::PEEK-CHAR* and friends."
  `(flex:with-input-from-sequence (,stream (map 'list #'char-code ,string))
     ,@body))

(defun split-set-cookie-string (string)
  "Splits the string STRING which is assumed to be the value of a
`Set-Cookie' into parts corresponding to individual cookies and
returns a list of these parts \(substrings).

The string /should/ be split at commas, but heuristical approach is
used instead which doesn't split at commas which are followed by what
cannot be recognized as the start of the next cookie.  This is
necessary because servers send headers containing unquoted commas
which are not meant as separators."
  ;; this would of course be a lot easier with CL-PPCRE's SPLIT
  (let ((cookie-start 0)
        (string-length (length string))
        search-start
        result)
    (tagbody
     ;; at this point we know that COOKIE-START is the start of a new
     ;; cookie (at the start of the string or behind a comma)
     next-cookie
     (setq search-start cookie-start)
     ;; we reach this point if the last comma didn't separate two
     ;; cookies or if there was no previous comma
     skip-comma
     (unless (< search-start string-length)
       (return-from split-set-cookie-string (nreverse result)))
     ;; look is there's a comma
     (let* ((comma-pos (position #\, string :start search-start))
            ;; and if so, look for a #\= behind the comma
            (equals-pos (and comma-pos (position #\= string :start comma-pos)))
            ;; check that (except for whitespace) there's only a token
            ;; (the name of the next cookie) between #\, and #\=
            (new-cookie-start-p (and equals-pos
                                     (every 'token-char-p
                                            (trim-whitespace string
                                                             :start (1+ comma-pos)
                                                             :end equals-pos)))))
       (when (and comma-pos (not new-cookie-start-p))
         (setq search-start (1+ comma-pos))
         (go skip-comma))
       (let ((end-pos (or comma-pos string-length)))
         (push (trim-whitespace (subseq string cookie-start end-pos)) result)
         (setq cookie-start (1+ end-pos))
         (go next-cookie))))))

#-:lispworks7.1
(defun make-ssl-stream (http-stream &key certificate key certificate-password verify (max-depth 10) ca-file ca-directory
                                         hostname)
  "Attaches SSL to the stream HTTP-STREAM and returns the SSL stream
\(which will not be equal to HTTP-STREAM)."
  (declare (ignorable http-stream certificate-password max-depth ca-directory hostname))
  (check-type verify (member nil :optional :required))
  (when (and certificate
             (not (probe-file certificate)))
    (error "certificate file ~A not found" certificate))
  (when (and key
             (not (probe-file key)))
    (error "key file ~A not found" key))
  (when (and ca-file
             (not (probe-file ca-file)))
    (error "ca file ~A not found" ca-file))
  #+(and :allegro (not :allegro-cl-express) (not :drakma-no-ssl))
  (socket:make-ssl-client-stream http-stream
                                 :certificate certificate
                                 :key key
                                 :certificate-password certificate-password
                                 :verify verify
                                 :max-depth max-depth
                                 :ca-file ca-file
                                 :ca-directory ca-directory)
  #+(and :mocl-ssl (not :drakma-no-ssl))
  (progn
    (when (or ca-file ca-directory)
      (warn ":max-depth, :ca-file and :ca-directory arguments not available on this platform"))
    (rt:start-ssl http-stream :verify verify))
  #+(and (or :allegro-cl-express (not :allegro)) (not :mocl-ssl) (not :drakma-no-ssl))
  (let ((s http-stream)
        (ctx (cl+ssl:make-context :verify-depth max-depth
                                  :verify-mode cl+ssl:+ssl-verify-none+
                                  :verify-callback nil
                                  :verify-location (or (and ca-file ca-directory
                                                            (list ca-file ca-directory))
                                                       ca-file ca-directory
                                                       :default))))
    (cl+ssl:with-global-context (ctx :auto-free-p t)
      (cl+ssl:make-ssl-client-stream
       (cl+ssl:stream-fd s)
       :verify verify
       :hostname hostname
       :close-callback (lambda () (close s))
       :certificate certificate
       :key key
       :password certificate-password)))
  #+:drakma-no-ssl
  (error "SSL not supported. Remove :drakma-no-ssl from *features* to enable SSL"))

(defun dissect-query (query-string)
  "Accepts a query string as in PURI:URI-QUERY and returns a
corresponding alist of name/value pairs."
  (when query-string
    (loop for parameter-pair in (cl-ppcre:split "&" query-string)
          for (name value) = (cl-ppcre:split "=" parameter-pair :limit 2)
          collect (cons name value))))
