;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DRAKMA; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/drakma/read.lisp,v 1.17 2008/05/25 11:35:20 edi Exp $

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

(defun read-status-line (stream &optional log-stream)
  "Reads one line from STREAM \(using Chunga's READ-LINE*) and
interprets it as a HTTP status line.  Returns a list of two or
three values - the protocol \(HTTP version) as a keyword, the
status code as an integer, and optionally the reason phrase."
  (let* ((*current-error-message* "While reading status line:")
         (line (or (read-line* stream log-stream)
                   (error 'drakma-simple-error
                          :format-control "No status line - probably network error.")))
         (first-space-pos (or (position #\Space line :test #'char=)
                              (syntax-error "No space in status line ~S." line)))
         (second-space-pos (position #\Space line
                                     :test #'char=
                                     :start (1+ first-space-pos))))
    (list (cond ((string-equal line "HTTP/1.0" :end1 first-space-pos) :http/1.0)
                ((string-equal line "HTTP/1.1" :end1 first-space-pos) :http/1.1)
                (t (syntax-error "Unknown protocol in ~S." line)))
          (or (ignore-errors (parse-integer line
                                            :start (1+ first-space-pos)
                                            :end second-space-pos))
              (syntax-error "Status code in ~S is not an integer." line))
          (and second-space-pos (subseq line (1+ second-space-pos))))))

(defun get-content-type (headers)
  "Reads and parses a `Content-Type' header and returns it as
three values - the type, the subtype, and an alist \(possibly
empty) of name/value pairs for the optional parameters.  HEADERS
is supposed to be an alist of headers as returned by
HTTP-REQUEST.  Returns NIL if there is no such header amongst
HEADERS."
  (when-let (content-type (header-value :content-type headers))
    (with-sequence-from-string (stream content-type)
      (let* ((*current-error-message* "Corrupted Content-Type header:")
             (type (read-token stream))
             (subtype (and (assert-char stream #\/)
                           (read-token stream)))
             (parameters (read-name-value-pairs stream)))
        (values type subtype parameters)))))

(defun read-token-and-parameters (stream)
  "Reads and returns \(as a two-element list) from STREAM a token
and an optional list of parameters \(attribute/value pairs)
following the token."
  (skip-whitespace stream)
  (list (read-token stream)
        (read-name-value-pairs stream)))

(defun skip-more-commas (stream)
  "Reads and consumes from STREAM any number of commas and
whitespace.  Returns the following character or NIL in case of
END-OF-FILE."
  (loop while (eql (peek-char* stream nil) #\,)
        do (read-char* stream) (skip-whitespace stream))
  (skip-whitespace stream))

(defun read-tokens-and-parameters (string &key (value-required-p t))
  "Reads a comma-separated list of tokens from the string STRING.
Each token can be followed by an optional, semicolon-separated
list of attribute/value pairs where the attributes are tokens
followed by a #\\= character and a token or a quoted string.
Returned is a list where each element is either a string \(for a
simple token) or a cons of a string \(the token) and an alist
\(the attribute/value pairs).  If VALUE-REQUIRED-P is NIL, the
value part \(including the #\\= character) of each attribute/value
pair is optional."
  (with-sequence-from-string (stream string)
    (loop with *current-error-message* = (format nil "While parsing ~S:" string)
          for first = t then nil
          for next = (and (skip-whitespace stream)
                          (or first (assert-char stream #\,))
                          (skip-whitespace stream)
                          (skip-more-commas stream))
          for token = (and next (read-token stream))
          for parameters = (and token
                                (read-name-value-pairs stream
                                                       :value-required-p value-required-p))
          while token
          collect (if parameters (cons token parameters) token))))

(defun split-tokens (string)
  "Splits the string STRING into a list of substrings separated
by commas and optional whitespace.  Empty substrings are
ignored."
  (loop for old-position = -1 then position
        for position = (and old-position
                            (position #\, string :test #'char= :start (1+ old-position)))
        for substring = (and old-position
                             (trim-whitespace (subseq string (1+ old-position) position)))
        while old-position
        when (plusp (length substring))
        collect substring))
