;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-INTERPOL; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-interpol/util.lisp,v 1.12 2008/07/23 14:41:37 edi Exp $

;;; Copyright (c) 2003-2008, Dr. Edmund Weitz. All rights reserved.

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

(in-package :cl-interpol)

(define-condition simple-reader-error (simple-condition reader-error)
  ()
  (:documentation "A reader error which can be signalled by ERROR."))

(defmacro signal-reader-error (format-control &rest format-arguments)
  "Like ERROR but signals a SIMPLE-READER-ERROR for the stream
*STREAM*."
  `(error 'simple-reader-error
          :stream *stream*
          :format-control ,format-control
          :format-arguments (list ,@format-arguments)))

(defun string-list-to-string (string-list)
  "Concatenates a list of strings to one string."
  ;; this function was originally provided by JP Massar for CL-PPCRE;
  ;; note that we can't use APPLY with CONCATENATE here because of
  ;; CALL-ARGUMENTS-LIMIT
  (let ((total-size 0))
    (dolist (string string-list)
      (incf total-size (length string)))
    (let ((result-string (make-array total-size :element-type 'character))
          (curr-pos 0))
      (dolist (string string-list)
        (replace result-string string :start1 curr-pos)
        (incf curr-pos (length string)))
      result-string)))

(defun get-end-delimiter (start-delimiter delimiters &key errorp)
  "Find the closing delimiter corresponding to the opening delimiter
START-DELIMITER in a list DELIMITERS which is formatted like
*OUTER-DELIMITERS*. If ERRORP is true, signal an error if none was
found, otherwise return NIL."
  (loop for element in delimiters
        if (eql start-delimiter element)
        do (return-from get-end-delimiter start-delimiter)
        else if (and (consp element)
                     (char= start-delimiter (car element)))
        do (return-from get-end-delimiter (cdr element)))
  (when errorp
    (signal-reader-error "~S not allowed as a delimiter here" start-delimiter)))

(declaim (inline make-collector))
(defun make-collector ()
  "Create an empty string which can be extended by
VECTOR-PUSH-EXTEND."
  (make-array 0
              :element-type 'character
              :fill-pointer t
              :adjustable t))

(declaim (inline make-char-from-code))
(defun make-char-from-code (number)
  "Create character from char-code NUMBER. NUMBER can be NIL which is
interpreted as 0."
  ;; Only look at rightmost eight bits in compliance with Perl
  (let ((code (logand #o377 (or number 0))))
    (or (and (< code char-code-limit)
             (code-char code))
        (signal-reader-error "No character for char-code #x~X"
                             number))))

(declaim (inline lower-case-p*))
(defun lower-case-p* (char)
  "Whether CHAR is a character which has case and is lowercase."
  (or (not (both-case-p char))
      (lower-case-p char)))

(defmacro read-char* ()
  "Convenience macro because we always read from the same string with
the same arguments."
  `(read-char *stream* t nil t))

(defmacro peek-char* ()
  "Convenience macro because we always peek at the same string with
the same arguments."
  `(peek-char nil *stream* t nil t))

(declaim (inline copy-readtable*))
(defun copy-readtable* ()
  "Returns a copy of the readtable which was current when
INTERPOL-READER was invoked. Memoizes its result."
  (or *readtable-copy*
      (setq *readtable-copy* (copy-readtable))))

(declaim (inline nsubvec))
(defun nsubvec (sequence start &optional (end (length sequence)))
  "Return a subvector by pointing to location in original vector."
  (make-array (- end start)
              :element-type (array-element-type sequence)
              :displaced-to sequence
              :displaced-index-offset start))
