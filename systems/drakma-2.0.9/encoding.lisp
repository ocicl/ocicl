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

(defgeneric decode-stream (encoding-type stream)
  (:documentation "Generic function to decode a stream.
This is a generic function which decodes the stream based on the encoding-type.
If a response contains one or more transfer or content encodings, then decode-stream
is called for each encoding type in the correct order to properly decode the stream to its
original content.

ENCODING-TYPE will be a keyword created by upcasing and interning the encoding type from the header.
STREAM will be the stream that needs to be decoded. decode-stream returns a new stream from
which you can read the decoded data."))

(defmethod decode-stream ((encoding-type t) stream)
  "Default handler, just return the stream."
  stream)

#-:drakma-no-chipz
(defmethod decode-stream ((encoding-type (eql :gzip)) stream)
  "Decode stream using gzip compression."
  (chipz:make-decompressing-stream 'chipz:gzip stream))

#-:drakma-no-chipz
(defmethod decode-stream ((encoding-type (eql :deflate)) stream)
  "Decode stream using deflate compression in zlib container."
  (chipz:make-decompressing-stream 'chipz:zlib stream))

(defmethod decode-stream ((encoding-type (eql :chunked)) (stream chunked-input-stream))
  "Decode a chunked stream.
Special method for chunked-input-stream that just turns chunking on."
  (setf (chunked-stream-input-chunking-p stream) t)
  stream)

(defmethod decode-stream ((encoding-type (eql :chunked)) stream)
  "General decode method for chunked stream.
Creates new chunked-stream."
  (let ((chunk-stream (make-chunked-stream stream)))
    (decode-stream :chunked chunk-stream)))

(defun decode-response-stream (headers stream &key (decode-content t))
  "Perform all necessary decodings on stream, from the Transfer-Encoding and
Content-Encoding headers.

If DECODE-CONTENT is nil, only the Transfer-Encoding headers will be used."
  (let ((transfer-encodings (header-value :transfer-encoding headers))
        (content-encodings (and decode-content
                                (header-value :content-encoding headers))))
    (when transfer-encodings
      (setq transfer-encodings (split-tokens transfer-encodings)))
    (when content-encodings
      (setq content-encodings (split-tokens content-encodings)))
    ;; Reverse, because we need to run decodings in the opposite order
    ;; they were applied.
    (let ((encodings (nreverse (nconc content-encodings transfer-encodings))))
      (loop for s = stream then (decode-stream encoding s)
            for encoding-str in encodings
            for encoding = (intern (string-upcase encoding-str) 'keyword)
            finally (return s)))))

(defun decode-flexi-stream (headers stream &key (decode-content t))
  (declare (flexi-input-stream stream))
  "Perform all necessary decodings on the internal stream of a flexi-stream.
Wrapper around decode-response-stream which preserverves the external format of the
flexi-stream.

If DECODE-CONTENT is nil, the Content-Encoding header will not be used to
determine which decoding mechanisms to use. Most servers use Content-Encoding
to identify compression."
  (let* ((raw-stream (flexi-stream-stream stream))
         (result (decode-response-stream headers raw-stream
                                         :decode-content decode-content)))
    (if (eq raw-stream result)
        stream
        (make-flexi-stream result
                           :external-format
                           (flexi-stream-external-format stream)
                           :element-type
                           (flexi-stream-element-type stream)))))
