;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :iolib/internal/conduits)

(macrolet
    ((define-gray-streams-package ()
       (let ((#1=gray-streams-package
              #+abcl             :gray-streams
              #+allegro          :excl
              #+(or cmu scl)     :ext
              #+(or clisp ecl)   :gray
              #+(or ccl openmcl) :ccl
              #+lispworks        :stream
              #+sbcl             :sb-gray
              #-(or abcl allegro cmu scl clisp ecl ccl openmcl lispworks sbcl)
              (cl:error "Your CL implementation isn't supported."))
             (#2=gray-streams-symbols
              '(#:fundamental-stream #:fundamental-input-stream
                #:fundamental-output-stream #:fundamental-character-stream
                #:fundamental-binary-stream #:fundamental-character-input-stream
                #:fundamental-character-output-stream
                #:fundamental-binary-input-stream
                #:fundamental-binary-output-stream #:stream-read-char
                #:stream-unread-char #:stream-read-char-no-hang
                #:stream-peek-char #:stream-listen #:stream-read-line
                #:stream-clear-input #:stream-write-char #:stream-line-column
                #:stream-start-line-p #:stream-write-string #:stream-terpri
                #:stream-fresh-line #:stream-finish-output #:stream-force-output
                #:stream-clear-output #:stream-advance-to-column
                #:stream-read-byte #:stream-write-byte)))
         `(defpackage :iolib/common-lisp
            (:nicknames :iolib.cl :iolib.common-lisp)
            (:extend/excluding :common-lisp
                               #:export #:unexport #:defpackage 
                               #:delete-package #:rename-package
                               #:defconstant
                               #:boolean)
            (:export #:defconstant #:boolean)
            (:extend/excluding :iolib/internal/conduits
                               #:recompute-conduits)
            (:import-from ,#1# ,@#2#)
            (:export #:trivial-gray-stream-mixin
                     #:stream-read-sequence
                     #:stream-write-sequence
                     #:stream-file-position
                     ,@#2#)))))
  (define-gray-streams-package))

(defpackage :iolib/common-lisp-user
  (:nicknames :iolib/cl-user :iolib.cl-user)
  (:use :iolib/common-lisp))
