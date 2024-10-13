;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/build/dump.lisp,v 1.39 2012-05-04 21:17:45 edi Exp $

;;; Copyright (c) 2008-2012, Dr. Edmund Weitz. All rights reserved.

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

(in-package :cl-unicode)

(defun build-range-list (reader test)
  "Loops through all non-NIL elements of *CHAR-DATABASE*, looks at
each element using the reader READER and builds an alist of the form
\((X1 . A1) (X2 . A2)) where the Xi are code points and the Ai are the
corresponding attributes as returned by READER.  The interesting part
is that the Xi can also be code point ranges like \(Y1 . Y2) meaning
that all code points from Y1 to \(including) Y2 have the same
attribute Ai and the function tries to make the alist as short as
possible, i.e. to partition it into as few ranges as possible.
Whether two attributes are the same is determined by the test function
TEST.  The resulting list is sorted."
  (let (range-list
        (last-attribute (funcall reader (aref *char-database* 0)))
        (last-code-point 0)
        (code-point 0))
    (flet ((add ()
             "Adds the range from LAST-CODE-POINT to \(excluding)
CODE-POINT with the attribute LAST-ATTRIBUTE to the result."
             (push (cons (cons last-code-point (1- code-point))
                         last-attribute)
                   range-list)))
      (loop
       (incf code-point)
       (when (= code-point #.(1- +code-point-limit+))
         (add)
         (return))
       (let* ((char-info (aref *char-database* code-point))
              (attribute (and char-info (funcall reader char-info))))
         (unless (funcall test attribute last-attribute)
           (add)
           (setq last-attribute attribute
                 last-code-point code-point)))))
    (nreverse range-list)))

(defun split-range-list (range-list)
  "Recursively splits a range list as returned by BUILD-RANGE-LIST in
the middle and thus converts it into a binary search tree which can be
used by the TREE-LOOKUP function."
  (let ((length (length range-list)))
    (cond ((zerop length) nil)
          (t (let ((middle (round (1- length) 2)))
               (list (nth middle range-list)
                     (split-range-list (subseq range-list 0 middle))
                     (split-range-list (subseq range-list (1+ middle)))))))))

(defun build-tree (reader &optional (test #'eq))
  "Uses BUILD-RANGE-LIST and SPLIT-RANGE-LIST to build a binary search
tree for READER which is one of the readers of the CHAR-INFO class.
Attributes are compared with TEST."
  (split-range-list (build-range-list reader test)))

(defun dump-method (name reader stream &optional (test #'eq test-provided-p))
  "Writes a method definition for a unary method called NAME
specialized for integer \(code points) to the stream STREAM which
returns a value equivalent to

  \(APPLY READER \(AREF *CHAR-DATABASE* <code-point>))

but uses compact binary search trees instead of the *CHAR-DATABASE*
array.  TEST is used by BUILD-TREE to decide whether two adjacent
characters have the same attribute.  If TEST isn't provided, it is
assumed that the attribute is a property symbol and the method will
return two values - the symbol and the canonical name of the symbol."
  (let ((definition (if test-provided-p
                      `(defmethod ,name ((code-point integer))
                         (tree-lookup code-point ',(build-tree reader test)))
                      `(defmethod ,name ((code-point integer))
                         (let ((symbol (tree-lookup code-point ',(build-tree reader))))
                           (values (property-name symbol) symbol))))))
    (print definition stream)))

(defmacro with-output-to-source-file ((stream relative-path &key no-header-p) &body body)
  "Executes BODY with STREAM bound to an output file stream which
writes to the file denoted by RELATIVE-PATH - a path relative to the
location of this source file.  Writes a Lisp header to the files
unless NO-HEADER-P is true."
  `(let ((pathname (merge-pathnames ,relative-path
                                    (make-pathname :name nil
                                                   :type nil
                                                   :version nil
                                                   :defaults *this-file*))))
     (when *compile-verbose*
       (format t "~&;;; Writing source file ~A" (file-namestring pathname))
       (force-output))
     (with-open-file (,stream pathname
                              :direction :output
                              :if-exists :supersede)
       (with-standard-io-syntax ()
         (let ((*package* (find-package :cl-unicode))
               #+clisp
               (*print-readably* nil)) ;; clisp produces rubbish otherwise
           ,@(unless no-header-p
               '((format out ";;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE; Base: 10 -*-~%")))
           (format out ";;; This file was auto-generated by the BUILD-CL-UNICODE system~2%")
           ,@(unless no-header-p
               '((print '(in-package :cl-unicode) out)))
           ,@body)))))

(defun dump-methods ()
  "Dumps several methods to the CL-UNICODE source file methods.lisp
using DUMP-METHOD."
  (with-output-to-source-file (out "../methods.lisp")
    (dump-method 'script 'script* out)
    (dump-method 'code-block 'code-block* out)
    (dump-method 'word-break 'word-break* out)
    (dump-method 'age 'age* out #'equal)
    (dump-method 'general-category 'general-category* out)
    (dump-method 'bidi-class 'bidi-class* out)
    (dump-method 'numeric-type 'numeric-type* out)
    (dump-method 'numeric-value 'numeric-value* out #'eql)
    (dump-method 'combining-class 'combining-class* out #'eql)
    (dump-method 'bidi-mirroring-glyph% 'bidi-mirroring-glyph* out #'eql)
    (dump-method 'binary-props 'binary-props* out #'equal)
    (dump-method 'idna-mapping 'idna-mapping* out #'equal)
    (dump-method 'decomposition-mapping 'decomposition-mapping* out #'equal)
    (dump-method 'case-folding-mapping 'case-folding-mapping* out #'equal)))

(defun dump-hash-table (hash-table-name stream)
  "Writes code to the STREAM which reinitializes the hash table
contained in the global special variable named HASH-TABLE-NAME to its
current state.  It is assumed that all keys and values of the hash
table can be printed readably."
  (print `(clrhash ,hash-table-name) stream)
  (let ((key-value-alist
         (loop for key being the hash-keys of (symbol-value hash-table-name)
               using (hash-value value)
               when value
               collect (cons key value))))
    (print `(loop for (key . value) in ',key-value-alist
                  do (setf (gethash key ,hash-table-name) value))
           stream)))

(defun dump-hash-tables ()
  "Dumps several hash tables to the CL-UNICODE source file
hash-tables.lisp using DUMP-HASH-TABLE."
  (with-output-to-source-file (out "../hash-tables.lisp")
    (dump-hash-table '*canonical-names* out)
    (dump-hash-table '*names-to-code-points* out)
    (dump-hash-table '*code-points-to-names* out)
    (dump-hash-table '*unicode1-names-to-code-points* out)
    (dump-hash-table '*code-points-to-unicode1-names* out)
    (dump-hash-table '*case-mappings* out)
    (dump-hash-table '*special-case-mappings* out)
    (dump-hash-table '*jamo-short-names* out)
    (dump-hash-table '*property-aliases* out)
    (dump-hash-table '*composition-mappings* out)
    ;; finally add code which adds the computed Hangul syllable names
    ;; at load time
    (print '(add-hangul-names) out)))

(defun dump-list (list-name stream)
  "Writes code to the STREAM which reinitializes the list contained in
the global special variable named LIST-NAME to its current state.  It
is assumed that all elements of the list can be printed readably."
  (print `(setq ,list-name ',(symbol-value list-name)) stream))

(defun dump-lists ()
  "Dumps several list to the CL-UNICODE source file lists.lisp using
DUMP-LIST."
  (with-output-to-source-file (out "../lists.lisp")
    (dump-list '*general-categories* out)
    (dump-list '*compatibility-formatting-tags* out)
    (dump-list '*scripts* out)
    (dump-list '*code-blocks* out)
    (dump-list '*binary-properties* out)
    (dump-list '*bidi-classes* out)))

(defun dump-derived-tests ()
  "Parses the Unicode data file \"DerivedCoreProperties.txt\" \(which
is not used in read.lisp) and uses it to create a file
\"derived-properties\" which will be used by CL-UNICODE-TEST."
  (with-output-to-source-file (out (make-pathname :name "derived-properties"
                                                  :type nil
                                                  :directory '(:relative :up "test"))
                               :no-header-p t)
    (let (last-test)
      (labels ((really-add-test (test)
                 "Writes the test designator from ADD-TEST in a
\"delayed\" fashion to make sure that the previous test \(which hasn't
been written to disk yet) doesn't contradict the current test.  This
is necessary because the file we're parsing contains several adjacent
ranges."
                 (when last-test
                   (unless (= (first last-test) (first test))
                     (print last-test out)))
                 (setq last-test test))
               (add-test (code-point property &optional (successp t))
                 "Writes a test designator \(used by the function
CL-UNICODE-TEST::PROPERTY-TESTS) for a check whether CODE-POINT has
the property PROPERTY to the stream OUT, but only does this if
CODE-POINT is below +CODE-POINT-LIMIT+.  Tests for the inverse if
SUCCESSP is NIL.  The test designator isn't actually written to the
stream, though, but handed over to REALLY-ADD-TEST."
                 (when (< code-point +code-point-limit+)
                   (really-add-test `(,code-point ,property ,successp)))))
        (with-unicode-codepoint-file ((code-point-range property) "DerivedCoreProperties.txt")
          (cond ((atom code-point-range)
                 (add-test code-point-range property)
                 (add-test (1+ code-point-range) property nil))
                (t
                 (add-test (car code-point-range) property)
                 (add-test (cdr code-point-range) property)
                 (add-test (1+ (cdr code-point-range)) property nil))))
        (print last-test out)))))

(defun dump-normalization-tests ()
  "Parses the Unicode data file \"NormalizationTest.txt\" \(which
is not used in read.lisp) and uses it to create a file
\"normalization-forms\" which will be used by CL-UNICODE-TEST."
  (with-output-to-source-file (out (make-pathname :name "normalization-forms"
                                                  :type nil
                                                  :directory '(:relative :up "test"))
                               :no-header-p t)
    (with-unicode-file ("NormalizationTest.txt" contents)
      (unless (char= #\@ (char (car  contents) 0))
        (destructuring-bind (source nfc nfd nfkc nfkd &rest extra)
            (mapcar #'(lambda (x) (parse-value x 'hex-list nil)) contents)
          (declare (ignore extra))
          (let ((*print-radix* t)
                (*print-base* 16))
            (print (list source nfc nfd nfkc nfkd) out)))))))

(defun dump-data-structures ()
  "Dumps all the information contained in *CHAR-DATABASE* and the
related hash tables and lists to the corresponding Lisp and test
source files."
  (dump-methods)
  (dump-hash-tables)
  (dump-lists)
  (dump-derived-tests)
  (dump-normalization-tests)
  (setq *char-database* nil))

(defun create-source-files ()
  "Combines BUILD-DATA-STRUCTURES and DUMP-DATA-STRUCTURES to create
the \"missing\" CL-UNICODE source files."
  (build-data-structures)
  (dump-data-structures)
  (setq *char-database* nil))

