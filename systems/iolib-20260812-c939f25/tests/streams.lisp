;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- iolib/streams test suite.
;;;
;;; Copyright (c) 2006-2007, Dr. Edmund Weitz.  All rights reserved.
;;; Copyright (c) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
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

(in-package :iolib/tests)

(in-suite :iolib/streams)

(defclass my-file-stream (dual-channel-gray-stream)
  ((path :initarg :path :reader file-stream-path)))

;;; Very ad-hoc: doesn't do :DIRECTION :PROBE, or handle errors,
;;; :IF-DOES-NOT-EXIST, among many other things.  This kind of thing
;;; should be moved into OSICAT, btw.
;;;
;;; FIXME: implement single-channel stream
(defun make-file-stream (path &key
                         (direction :input)
                         (if-exists :error)
                         (if-does-not-exist (ecase direction
                                              (:input :error)
                                              ((:io :output) :create)))
                         (external-format :default))
  (declare (ignore if-does-not-exist))
  ;; move OPEN to INITIALIZE-INSTANCE
  (let ((fd (isys:open (namestring path)
                       (logior (ecase direction
                                 (:input isys:o-rdonly)
                                 (:output (logior isys:o-creat isys:o-wronly))
                                 (:io (logior isys:o-creat isys:o-rdwr)))
                               (ecase if-exists
                                 (:error isys:o-excl)
                                 (:supersede isys:o-trunc)
                                 (:append isys:o-append)
                                 (:overwrite 0)))
                       (logior isys:s-irusr isys:s-iwusr))))
    (setf (isys:fd-nonblock-p fd) t)
    (make-instance 'my-file-stream
                   :path path
                   :fd fd
                   :external-format external-format)))

(defmacro with-open-file-stream ((var path &rest options) &body body)
  (with-gensyms (stream)
    `(let ((,stream (make-file-stream ,path ,@options)))
       (with-open-stream (,var ,stream)
         ,@body))))

(defvar *data-dir*
  (asdf:system-relative-pathname "iolib" "tests/data/"))

(defvar *test-dir*
  (asdf:apply-output-translations
   (asdf:system-relative-pathname "iolib" "tests/test-dir/")))

;;; A list of test files where each entry consists of the name
;;; prefix and a list of encodings.
(defvar *test-files*
  '(("kafka" (#-8bit-chars :utf-8 :latin-1 #|:cp1252|#))
    ("tilton" (#-8bit-chars :utf-8 :ascii))
    ("hebrew" (#-8bit-chars :utf-8 #|:latin-8|#))
    ("russian" (#-8bit-chars :utf-8 #|:koi8r|#))
    ("unicode_demo" (#-8bit-chars :utf-8 #|:utf-16 :utf-32|#))))

;;; For a name suffix FILE-NAME and a symbol SYMBOL denoting an
;;; encoding returns a list of pairs where the car is a full file name
;;; and the cdr is the corresponding external format.  This list
;;; contains all possible line-end conversions.
(defun create-file-variants (file-name symbol)
  (loop :for eol-style :in '(:lf :cr :crlf) :collect
        (cons (format nil "~A_~(~A~)_~(~A~).txt"
                      file-name symbol eol-style)
              (babel:make-external-format symbol :eol-style eol-style))))

;;; For a name suffix FILE-NAME and a list of symbols SYMBOLS denoting
;;; different encodings of the corresponding file returns a list of
;;; lists which can be used as arglists for COMPARE-FILES.
(defun create-test-combinations (file-name symbols)
  (let ((file-variants (loop :for symbol :in symbols
                             :nconc (create-file-variants file-name symbol))))
    (loop :for (name-in . external-format-in) :in file-variants
          :nconc (loop :for (name-out . external-format-out) :in file-variants
                       :collect (list name-in external-format-in
                                      name-out external-format-out)))))

;;; Returns a true value iff FILE1 and FILE2 have the same contents
;;; (viewed as binary files).
(defun file-equal (file1 file2)
  (with-open-file (stream1 file1 :element-type '(unsigned-byte 8))
    (with-open-file (stream2 file2 :element-type '(unsigned-byte 8))
      (and (= (file-length stream1) (file-length stream2))
           (loop :for byte1 := (read-byte stream1 nil nil)
                 :for byte2 := (read-byte stream2 nil nil)
                 :while (and byte1 byte2)
                 :always (= byte1 byte2))))))

;;; Copies the contents of the file denoted by the pathname PATH-IN to
;;; the file denoted by the pathname PATH-OUT using flexi streams -
;;; STREAM-IN is read with the external format EXTERNAL-FORMAT-IN and
;;; STREAM-OUT is written with EXTERNAL-FORMAT-OUT.  The input file is
;;; opened with the :DIRECTION keyword argument DIRECTION-IN, the
;;; output file is opened with the :DIRECTION keyword argument
;;; DIRECTION-OUT.
(defun %copy-file (path-in external-format-in path-out external-format-out
                   direction-out direction-in)
  (with-open-file-stream (in path-in
                             :direction direction-in
                             :if-does-not-exist :error
                             :if-exists :overwrite
                             :external-format external-format-in)
    (with-open-file-stream (out path-out
                                :direction direction-out
                                :if-does-not-exist :create
                                :if-exists :supersede
                                :external-format external-format-out)
      (loop :for line := (read-line in nil nil)
            :while line :do (write-line line out)))))

(defun ef-name (ef)
  (format nil "~A ~A"
          (babel-encodings:enc-name (babel:external-format-encoding ef))
          (babel:external-format-eol-style ef)))

;;; Copies the contents of the file (in the 'test') denoted by the
;;; relative pathname PATH-IN to the file (in a temporary directory)
;;; denoted by the relative pathname PATH-OUT using flexi streams -
;;; STREAM-IN is read with the external format EXTERNAL-FORMAT-IN and
;;; STREAM-OUT is written with EXTERNAL-FORMAT-OUT.  The resulting
;;; file is compared with an existing file in the 'test' directory to
;;; check if the outcome is as expected.  Uses various variants of the
;;; :DIRECTION keyword when opening the files."
(defun compare-files (path-in external-format-in path-out external-format-out)
  (ensure-directories-exist *test-dir*)
  (let ((full-path-in (merge-pathnames path-in *data-dir*))
        (full-path-out (merge-pathnames path-out *test-dir*))
        (full-path-orig (merge-pathnames path-out *data-dir*)))
    (dolist (direction-out '(:output :io) t)
      (dolist (direction-in '(:input :io))
        (let ((description (format nil "Test ~S ~A [~A] --> ~A [~A]"
                                   path-in (ef-name external-format-in)
                                   direction-in (ef-name external-format-out)
                                   direction-out)))
          (format *error-output* "~&;; ~A.~%" description)
          (%copy-file full-path-in external-format-in
                      full-path-out external-format-out
                      direction-out direction-in)
          (unless (file-equal full-path-out full-path-orig)
            (format *error-output* "~&;;   Test failed!!!~%")
            (return* nil)))))))

(test (big-stream-comparision-test :compile-at :definition-time)
  (is-false
   (let ((args-list (loop :for (file-name symbols) :in *test-files*
                          :nconc (create-test-combinations file-name symbols))))
     (loop :for args :in args-list
           :unless (apply #'compare-files args)
           :collect args))))
