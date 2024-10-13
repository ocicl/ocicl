;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/build/read.lisp,v 1.34 2012-05-04 21:17:45 edi Exp $

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

(defmacro with-unicode-file ((file-name contents-name) &body body)
  "Utility macro to parse a file which is formatted as described in
<http://unicode.org/Public/UNIDATA/UCD.html#UCD_File_Format>.  The
file named FILE-NAME is searched for in the directory \"data/\"
relative to this source file.  The code then iterates through the file
and executes BODY for each non-comment line binding contents to the
list of fields in the line"
  `(let ((pathname (merge-pathnames ,file-name (merge-pathnames "data/" *this-file*))))
       (when *compile-verbose*
         (format t "~&;;; Parsing Unicode file ~A" (file-namestring pathname))
         (force-output))
       (with-open-file (binary-in pathname :element-type 'flex:octet)
         ;; Unicode data files must be read as UTF-8
         (let ((in (flex:make-flexi-stream binary-in :external-format '(:utf-8 :eol-style :lf))))
           (loop
            (flet ((get-line-contents ()
                     (let ((line (or (read-line in nil) (return))))
                       (and (not (ppcre:scan "^\\s*(?:#|$)" line))
                            (ppcre:split "\\s*(#.*)?$|\\s*;\\s*" line :limit most-positive-fixnum)))))
              (let ((,contents-name (get-line-contents)))
                (when ,contents-name
                  ,@body))))))))

(defmacro with-unicode-codepoint-file (((&rest bindings) file-name &optional two-line-ranges) &body body)
  "Utility macro to parse a file which is formatted as described in
<http://unicode.org/Public/UNIDATA/UCD.html#UCD_File_Format>.  The
file named FILE-NAME is searched for in the directory \"data/\"
relative to this source file.  The code then iterates through the file
and executes BODY for each non-comment line binding the variables in
BINDINGS to the parsed fields of the line.  For the details of
BINDINGS see the EXTRACT-FOO functions in util.lisp or the usage of
this macro below.  If TWO-LINE-RANGES is true, then the macro expects
a file like \"UnicodeData.txt\" where ranges aren't denoted as usual
but rather using <..., First> and <..., Last>."
  (let ((variables (extract-variables bindings))
        (types (extract-types bindings)))
    `(with-unicode-file (,file-name contents)
      (destructuring-bind ,variables
          (parse-one-line contents ',types (list ,@(extract-defaults bindings)))
        ,@(when two-line-ranges
            `((when (ppcre:scan "^<.*, First>$" ,(second variables))
                (let ((range-end (first (parse-one-line (list (first (get-line-contents)))))))
                  (setq ,(first variables) (cons ,(first variables) range-end))))))
        ,@body))))

(defmacro with-code-point-range ((var range) &body body)
  "Utility macro which executes BODY with VAR bound to each code point
in RANGE in turn.  VAR can either be an integer \(for one code point)
or a cons of two integers \(for an inclusive range)."
  (with-rebinding (range)
    `(flet ((thunk (,var) ,@body))
       (cond ((atom ,range) (thunk ,range))
             (t (loop for point from (car ,range) to (cdr ,range)
                      do (thunk point)))))))

(defun read-character-data ()
  "Parses the file \"UnicodeData.txt\" and generates one CHAR-INFO
entry per code point which is stored in *CHAR-DATABASE*."
  ;; by definition, we'll never see this property in the file, so we
  ;; have to add it to *GENERAL-CATEGORIES* explicitly
  (setq *general-categories* (list '#.(property-symbol "Cn")))
  (with-unicode-codepoint-file ((code-point-range
                                 name
                                 (general-category symbol)
                                 (combining-class integer)
                                 (bidi-class symbol)
                                 (decomposition-mapping tagged-hex-list nil)
                                 (decimal-digit integer nil)
                                 (digit integer nil)
                                 (numeric rational nil)
                                 (bidi-mirrored boolean)
                                 (unicode1-name string nil)
                                 ;; ISO comment, ignored
                                 _
                                 (uppercase-mapping hex nil)
                                 (lowercase-mapping hex nil)
                                 (titlecase-mapping hex nil))
                                "UnicodeData.txt" t)
    (when (and (listp decomposition-mapping)
               (car decomposition-mapping)
               (symbolp (car decomposition-mapping)))
      (pushnew (car decomposition-mapping) *compatibility-formatting-tags* :test #'eq))
    (pushnew general-category *general-categories* :test #'eq)
    (pushnew bidi-class *bidi-classes* :test #'eq)
    ;; if the name starts with #\<, it's not really a name but denotes
    ;; a range - some of these names (CJK unified ideographs and
    ;; Hangul syllables) will be computed later, the others are NIL
    (when (char= (char name 0) #\<)
      (setq name nil))
    (with-code-point-range (code-point code-point-range)
      (setf (aref *char-database* code-point)
            (make-instance 'char-info
                           :code-point code-point
                           :name name
                           :general-category general-category
                           :combining-class combining-class
                           :bidi-class bidi-class
                           :decomposition-mapping decomposition-mapping
                           :numeric-type (cond (decimal-digit '#.(property-symbol "Decimal"))
                                               (digit '#.(property-symbol "Digit"))
                                               (numeric '#.(property-symbol "Numeric")))
                           :numeric-value numeric
                           :binary-props (and bidi-mirrored
                                              (list '#.(property-symbol "BidiMirrored")))
                           :unicode1-name unicode1-name
                           :uppercase-mapping uppercase-mapping
                           :lowercase-mapping lowercase-mapping
                           :titlecase-mapping titlecase-mapping)))))

(defun read-scripts ()
  "Parses the file \"Scripts.txt\" and adds the information about the
script to the corresponding entries in *CHAR-DATABASE*."
  (with-unicode-codepoint-file ((code-point-range (script symbol)) "Scripts.txt")
    (pushnew script *scripts* :test #'eq)
    (with-code-point-range (code-point code-point-range)
      (let ((char-info (aref *char-database* code-point)))
        (when char-info
          (setf (script* char-info) script))))))

(defun read-code-blocks ()
  "Parses the file \"Blocks.txt\" and adds the information about the
code block to the corresponding entries in *CHAR-DATABASE*."
  (with-unicode-codepoint-file ((code-point-range (code-block symbol)) "Blocks.txt")
    (pushnew code-block *code-blocks* :test #'eq)
    (with-code-point-range (code-point code-point-range)
      (let ((char-info (aref *char-database* code-point)))
        (when char-info
          (setf (code-block* char-info) code-block))))))

(defun read-word-breaks ()
  "Parses the file \"Scripts.txt\" and adds the information about the
script to the corresponding entries in *CHAR-DATABASE*."
  (with-unicode-codepoint-file ((code-point-range (word-break symbol)) "auxiliary/WordBreakProperty.txt")
    ;;(pushnew word-break *word-breaks* :test #'eq)
    (with-code-point-range (code-point code-point-range)
      (let ((char-info (aref *char-database* code-point)))
        (when char-info
          (setf (word-break* char-info) word-break))))))


(defun read-idna-mapping ()
  "Parses the file \"Scripts.txt\" and adds the information about the
script to the corresponding entries in *CHAR-DATABASE*."
  (with-unicode-codepoint-file ((code-point-range
                                 (mapping-type symbol)
                                 (mapped-to hex-list nil)
                                 (scope symbol nil))
                                "idna/IdnaMappingTable.txt")
    (with-code-point-range (code-point code-point-range)
      (unless (eq mapping-type '#.(property-symbol "disallowed"))
        (let ((char-info (aref *char-database* code-point))
              (mapping (list mapping-type mapped-to scope)))
          (if char-info
              (setf (idna-mapping* char-info) mapping)
              ;; this file actually contains some information for
              ;; unassigned (but reserved) code points, like e.g. #xfff0
              (setf char-info (make-instance 'char-info :code-point code-point
                                                        :idna-mapping mapping)
                    (aref *char-database* code-point) char-info)))))))

(defun add-hangul-decomposition ()
  "Computes the canonical decomposition for all Hangul syllables."
  (declare #.*standard-optimize-settings*)
  (when *compile-verbose*
    (format t "~&;;; Computing Hangul syllable decomposition")
    (force-output))
  (loop for code-point from +first-hangul-syllable+ to +last-hangul-syllable+
        for mapping = (compute-hangul-decomposition code-point)
        for char-info = (aref *char-database* code-point)
        when char-info
          do (setf (decomposition-mapping* char-info) mapping)))

(defun read-binary-properties ()
  "Parses the file \"PropList.txt\" and adds information about binary
properties to the corresponding entries in *CHAR-DATABASE*."
  ;; this property was derived from UnicodeData.txt already
  (setq *binary-properties* (list '#.(property-symbol "BidiMirrored")))
  (with-unicode-codepoint-file ((code-point-range (property symbol)) "PropList.txt")
    ;; we don't need this information as we derive it from a code
    ;; point not being mentioned in UnicodeData.txt - see also the
    ;; initform for GENERAL-CATEGORY in the definition of CHAR-INFO
    (unless (eq property '#.(property-symbol "NoncharacterCodePoint"))
      (pushnew property *binary-properties* :test #'eq)
      (with-code-point-range (code-point code-point-range)
        (let ((char-info (aref *char-database* code-point)))
          (unless char-info
            ;; this file actually contains some information for
            ;; unassigned (but reserved) code points, like e.g. #xfff0
            (setf char-info (make-instance 'char-info :code-point code-point)
                  (aref *char-database* code-point) char-info))
          (push property (binary-props* char-info)))))))

(defun read-derived-age ()
  "Parses the file \"DerivedAge.txt\" and adds information about the
\"age\" to the corresponding entries in *CHAR-DATABASE*."
  (with-unicode-codepoint-file ((code-point-range (age age)) "DerivedAge.txt")
    (with-code-point-range (code-point code-point-range)
      (let ((char-info (aref *char-database* code-point)))
        (when char-info
          (setf (age* char-info) age))))))

(defun read-mirroring-glyphs ()
  "Parses the file \"BidiMirroring.txt\" and adds information about
mirroring glyphs to the corresponding entries in *CHAR-DATABASE*."
  (with-unicode-codepoint-file ((code-point-range (mirroring-glyph hex)) "BidiMirroring.txt")
    (with-code-point-range (code-point code-point-range)
      (let ((char-info (aref *char-database* code-point)))
        (when char-info
          (setf (bidi-mirroring-glyph* char-info) mirroring-glyph))))))

(defun read-jamo ()
  "Parses the file \"Jamo.txt\" and stores information about Jamo
short names in the *JAMO-SHORT-NAMES* hash table.  This information is
later used to compute Hangul syllable names."
  (clrhash *jamo-short-names*)
  (with-unicode-codepoint-file ((code-point-range (short-name string "")) "Jamo.txt")
    (with-code-point-range (code-point code-point-range)
      (setf (gethash code-point *jamo-short-names*) short-name))))

(defun read-property-aliases ()
  "Parses the file \"PropertyAliases.txt\" and stores information about
aliases for properties in the *PROPERTY-ALIASES* hash table.  This
information is used to get properties by any alias."
  (clrhash *property-aliases*)
  (with-unicode-file ("PropertyAliases.txt" contents)
    (destructuring-bind (short-name long-name &rest additional-names) contents
      (let ((symb (property-symbol long-name))
            (long-name (string-upcase (canonicalize-name long-name)))
            (short-name (string-upcase (canonicalize-name short-name))))
        (setf (gethash long-name *property-aliases*) symb
              (gethash short-name *property-aliases*) symb)
        (loop for alt-name in additional-names
              :do (setf (gethash (string-upcase (canonicalize-name alt-name)) *property-aliases*) symb))))))

(defun read-special-casing ()
  "Parses the file \"SpecialCasing.txt\" and stores information about special casing rules."
  (clrhash *special-case-mappings*)
  (with-unicode-file ("SpecialCasing.txt" contents)
    (destructuring-bind (code-str lower title upper &rest conditions) contents
      (let ((code-point (parse-hex code-str))
            (rules (list (remove-if #'(lambda (x) (or (null x) (string= "" x))) conditions)
                         (parse-value lower 'hex-list nil)
                         (parse-value upper 'hex-list nil)
                         (parse-value title 'hex-list nil))))
        (pushnew rules (gethash code-point *special-case-mappings*))))))

(defun read-case-folding-mapping ()
  "Parses the file \"CaseFolding.txt\" and adds the information about the
case folding rules to the corresponding entries in *CHAR-DATABASE*."
  (with-unicode-codepoint-file ((code-point-range
                                 (status symbol)
                                 (mapped-to hex-list nil))
                                "CaseFolding.txt")
    (with-code-point-range (code-point code-point-range)
      (let ((char-info (aref *char-database* code-point))
            (mapping (list status mapped-to)))
        (if char-info
            (pushnew mapping (case-folding-mapping* char-info))
            ;; this file actually contains some information for
            ;; unassigned (but reserved) code points, like e.g. #xfff0
            (setf char-info (make-instance 'char-info :code-point code-point
                                                      :case-folding-mapping (list mapping))
                  (aref *char-database* code-point) char-info))))))

(defun default-bidi-class (char-info)
  "Returns the default Bidi class for the character described by the
CHAR-INFO object CHAR-INFO.  The default is computed as explained in
<http://unicode.org/Public/UNIDATA/extracted/DerivedBidiClass.txt>."
  (let ((code-point (code-point char-info)))
    (cond ((and (or (<= #x0600 code-point #x07BF)
                    (<= #xFB50 code-point #xFDFF)
                    (<= #xFE70 code-point #xFEFF))
                (not (find '#.(property-symbol "NoncharacterCodePoint")
                           (binary-props* char-info))))
           '#.(property-symbol "AL"))
          ((or (<= #x0590 code-point #x05FF)
               (<= #x07C0 code-point #x08ff)
               (<= #xFB1D code-point #xFB4F)
               (<= #x10800 code-point #x10FFF))
           '#.(property-symbol "R"))
          (t '#.(property-symbol "L")))))

(defun set-default-bidi-classes ()
  "Loops through all assigned characters in *CHAR-DATABASE* and
defaults their Bidi class if it wasn't set already."
  (loop for char-info across *char-database*
        when (and char-info (not (bidi-class* char-info)))
        do (let ((default-bidi-class (default-bidi-class char-info)))
             (pushnew default-bidi-class *bidi-classes* :test #'eq)
             (setf (bidi-class* char-info) default-bidi-class))))

(defun fill-database ()
  "Initializes all relevant datastructures and parses all Unicode data
files in the \"data/\" directory to build up enough information in
memory \(specifically the *CHAR-DATABASE* array) to write the missing
source code files for CL-UNICODE."
  (setq *char-database* (make-empty-char-database)
        *general-categories* nil
        *compatibility-formatting-tags* nil
        *scripts* nil
        *code-blocks* nil
        *binary-properties* nil
        *bidi-classes* nil)
  (initialize-property-symbols)
  (read-character-data)
  (read-scripts)
  (read-code-blocks)
  (read-word-breaks)
  (read-binary-properties)
  (read-derived-age)
  (read-mirroring-glyphs)
  (read-jamo)
  (read-property-aliases)
  (read-idna-mapping)
  (read-special-casing)
  (read-case-folding-mapping)
  (set-default-bidi-classes)
  (add-hangul-decomposition))

(defun build-name-mappings ()
  "Initializes and fills the hash tables which map code points to
\(Unicode 1.0) names and vice versa using the information in
*CHAR-DATABASE*."
  (clrhash *names-to-code-points*)
  (clrhash *unicode1-names-to-code-points*)
  (clrhash *code-points-to-names*)
  (clrhash *code-points-to-unicode1-names*)
  (loop for char-info across *char-database*
        for name = (and char-info (name char-info))
        for unicode1-name = (and char-info (unicode1-name* char-info))
        for code-point = (and char-info (code-point char-info))
        when name
        do (setf (gethash code-point *code-points-to-names*) name
                 (gethash (canonicalize-name name) *names-to-code-points*) code-point)
        when unicode1-name
        do (setf (gethash code-point *code-points-to-unicode1-names*) unicode1-name
                 (gethash (canonicalize-name unicode1-name) *unicode1-names-to-code-points*) code-point)))

(defun build-case-mapping ()
  "Initializes and filles the *CASE-MAPPINGS* hash table from
*CHAR-DATABASE*."
  (clrhash *case-mappings*)
  (loop for char-info across *char-database*
        for mappings = (and char-info
                            (list (lowercase-mapping* char-info)
                                  (uppercase-mapping* char-info)
                                  (titlecase-mapping* char-info)))
        when (and mappings (some #'identity mappings))
          do (setf (gethash (code-point char-info) *case-mappings*) mappings)))

(defun build-composition-mappings ()
  "Computes composition mapping as an inverse of canonical decomposition"
  (declare #.*standard-optimize-settings*)
  (when *compile-verbose*
    (format t "~&;;; Computing canonical composition mappings")
    (force-output))
  (clrhash *composition-mappings*)
  ;; read exclusion list
  (let ((exclusion-map (make-hash-table)))
    (with-unicode-codepoint-file ((code-point-range)
                                  "CompositionExclusions.txt")
      (with-code-point-range (code-point code-point-range)
        (setf (gethash code-point exclusion-map) t)))
    (flet ((non-starter-p (char-info)
             (let ((mapping (decomposition-mapping* char-info))
                   (ccc (combining-class* char-info)))
               (declare (fixnum ccc))
               (or (/= 0 ccc) (and (integerp (car mapping))
                                   (let ((first (aref *char-database* (car mapping))))
                                     (or (null first)
                                         (/= 0 (combining-class* first)))))))))
      (loop for char-info across *char-database*
            for mapping = (if (null char-info)
                              nil
                              (decomposition-mapping* char-info))
            unless (or (null mapping)
                       (symbolp (car mapping))
                       (/= 2 (length mapping)) ; ignore singletons
                       (non-starter-p char-info)
                       (gethash (code-point char-info) exclusion-map))
              do (pushnew (cons (nth 1 mapping) (code-point char-info))
                          (gethash (car mapping) *composition-mappings*))))))

(defun build-data-structures ()
  "One function to combine the complete process of parsing all Unicode
data files and building the corresponding Lisp datastructures in
memory."
  (fill-database)
  (when *compile-verbose*
    (format t "~&;;; Building hash tables")
    (force-output))
  (build-name-mappings)
  (build-case-mapping)
  (build-composition-mappings))
