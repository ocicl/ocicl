;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/api.lisp,v 1.32 2012-05-04 21:17:44 edi Exp $

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

(defun try-abbreviations (name scripts-to-try)
  "Helper function called by CHARACTER-NAMED when the
:TRY-ABBREVIATIONS-P keyword argument is true.  Tries to interpret
NAME as an abbreviation for a longer Unicode name and returns the
corresponding code point if it succeeds."
  (flet ((size-word (string)
           (if (ppcre:scan "[A-Z]" string) "CAPITAL" "SMALL"))
         (try (script size-word short-name)
           (or (character-named (format nil "~A ~A letter ~A"
                                        script size-word short-name)
                                :want-code-point-p t)
               (character-named (format nil "~A letter ~A"
                                        script short-name)
                                :want-code-point-p t)
               (character-named (format nil "~A ~A"
                                        script short-name)
                                :want-code-point-p t))))
    (ppcre:register-groups-bind (script short-name)
        ("^([^:]+):([^:]+)$" name)
      (let ((size-word (size-word short-name)))
        (return-from try-abbreviations
          (try script size-word short-name))))
    (loop with size-word = (size-word name)
          for script in scripts-to-try
          thereis (try script size-word name))))  

(defun unicode-name-reader (stream char arg)
  "The reader function used when the alternative character syntax is
enabled."
  (declare (ignore char arg))
  (let ((name (with-output-to-string (out)
                (write-char (read-char stream t nil t) out)
                (loop for next-char = (read-char stream t nil t)
                      while (find next-char "abcdefghijklmnopqrstuvwxyz0123456789_-+:"
                                  :test 'char-equal)
                      do (write-char next-char out)
                      finally (unread-char next-char stream)))))
    (or (character-named name)
        (error 'character-not-found :name name))))

(defun %enable-alternative-character-syntax ()
  "Internal function used to enable alternative character syntax and
store current readtable on stack."
  (push *readtable* *previous-readtables*)
  (setq *readtable* (copy-readtable))
  (set-dispatch-macro-character #\# #\\ 'unicode-name-reader)
  (values))

(defun %disable-alternative-character-syntax ()
  "Internal function used to restore previous readtable." 
  (setq *readtable*
        (if *previous-readtables*
          (pop *previous-readtables*)
          (copy-readtable nil)))
  (values))

(defgeneric unicode-name (c)
  (:documentation "Returns the Unicode name of a character as a string
or NIL if there is no name for that particular character.  C can be
the character's code point \(a positive integer) or a \(Lisp)
character assuming its character code is also its Unicode code
point.")
  (:method ((char character))
   (unicode-name (char-code char)))
  (:method ((code-point integer))
   (or (gethash code-point *code-points-to-names*)
       (maybe-compute-hangul-syllable-name code-point)
       (maybe-compute-cjk-name code-point))))

(defgeneric unicode1-name (c)
  (:documentation "Returns the Unicode 1.0 name of a character as a
string or NIL if there is no name for that particular character.  This
name is only non-NIL if it is significantly different from the Unicode
name (see UNICODE-NAME).  For control characters, sometimes the ISO
6429 name is returned instead.

C can be the character's code point \(a positive integer) or a \(Lisp)
character assuming its character code is also its Unicode code
point.")
  (:method ((char character))
   (unicode1-name (char-code char)))
  (:method ((code-point integer))
   (values (gethash code-point *code-points-to-unicode1-names*))))

(defun character-named (name &key
                             want-code-point-p
                             (try-unicode1-names-p *try-unicode1-names-p*)
                             (try-abbreviations-p *try-abbreviations-p*)
                             (scripts-to-try *scripts-to-try*)
                             (try-hex-notation-p *try-hex-notation-p*)
                             (try-lisp-names-p *try-lisp-names-p*))
  "Returns the character which has the name NAME \(a string) by
looking up the Unicode name \(see UNICODE-NAME).

If TRY-UNICODE1-NAMES is true, the Unicode 1.0 name \(see
UNICODE1-NAME) will be used as a fallback.

If TRY-ABBREVIATIONS-P is true, NAME is treated as an abbreviation as
follows: If NAME contains a colon, it is interpreted as
\"<script>:<short-name>\" and the function tries to look up, in turn,
the characters named \"<script> <size> LETTER <short-name>\",
\"<script> LETTER <short-name>\", and \"<script> <short-name>\" where
<size> is \"SMALL\" if none of the characters in <short-name> is
uppercase, \"CAPITAL\" otherwise.  If NAME does not contain a colon,
the same algorithm as above is tried with NAME instead of <short-name>
and each element of the list of strings SCRIPTS-TO-TRY as <string>.
\(SCRIPTS-TO-TRY can also be a single string which is interpreted as a
one-element list.)

If TRY-HEX-NOTATION-P is true, NAME can be of the form \"U+<x>\" where
<x> is a hexadecimal number with four to six digits with the obvious
meaning.

If TRY-LISP-NAMES-P is true, the function returns the character with
the character name NAME \(if there is one) or, if NAME is exactly one
character, it returns this character.

All the keyword-governed alternatives are tried in the order they're
described here.

See also *TRY-UNICODE1-NAMES-P*, *TRY-ABBREVIATIONS-P*,
*SCRIPTS-TO-TRY*, *TRY-HEX-NOTATION-P*, and *TRY-LISP-NAMES-P*.

Returns the code point instead of the character if WANT-CODE-POINT-P
is true.  This can be especially useful for Lisp implementations where
CHAR-CODE-LIMIT is smaller than +CODE-POINT-LIMIT+."
  (when (stringp scripts-to-try)
    (setq scripts-to-try (list scripts-to-try)))
  (let* ((canonicalized-name (canonicalize-name name))
         (code-point (or (gethash canonicalized-name *names-to-code-points*)
                         (maybe-find-hangul-syllable-code-point canonicalized-name)
                         (maybe-find-cjk-code-point canonicalized-name)
                         (and try-unicode1-names-p
                              (gethash canonicalized-name *unicode1-names-to-code-points*))
                         (and try-abbreviations-p
                              (let ((*try-unicode1-names-p* try-unicode1-names-p)
                                    (*try-abbreviations-p* nil))
                                (try-abbreviations name scripts-to-try)))
                         (and try-hex-notation-p
                              (ppcre:register-groups-bind ((#'parse-hex code-point))
                                  ("^U\\+([a-zA-Z0-9]{4,6})$" name)
                                (and (< code-point +code-point-limit+)
                                     code-point)))
                         (and try-lisp-names-p
                              (case (length name)
                                (1 (char-code (char name 0)))
                                (otherwise (let ((char (name-char name)))
                                             (and char (char-code char)))))))))
    (if want-code-point-p
      code-point
      (and code-point (code-char code-point)))))

(defgeneric script (c)
  (:documentation "Returns the script of a character as a string or
NIL if there is no script for that particular character.  C can be the
character's code point \(a positive integer) or a \(Lisp) character
assuming its character code is also its Unicode code point.  The
second return value \(if there is one) is the property symbol of the
script.

See also SCRIPTS.")
  (:method ((char character))
   (script (char-code char))))

(defgeneric code-block (c)
  (:documentation "Returns the block of a character as a string or NIL
if there is no block for that particular character.  C can be the
character's code point \(a positive integer) or a \(Lisp) character
assuming its character code is also its Unicode code point.  The
second return value \(if there is one) is the property symbol of the
block.

See also CODE-BLOCKS.")
  (:method ((char character))
   (code-block (char-code char))))

(defgeneric word-break (c)
  (:documentation "Returns the Word_Break property of a character as a
string.  C can be the character's code point \(a positive integer) or a
\(Lisp) character assuming its character code is also its Unicode code
point.  The second return value \(if there is one) is the property symbol of the
word break.")
  (:method ((char character))
   (word-break (char-code char))))

(defgeneric age (c)
  (:documentation "Returns the \"age\" of a character or NIL if there
is no age entry for that particular character.  The age of a character
is a list of two integers denoting the major and minor number of the
Unicode version where the character first appeared.  C can be the
character's code point \(a positive integer) or a \(Lisp) character
assuming its character code is also its Unicode code point.")
  (:method ((char character))
   (age (char-code char))))

(defgeneric general-category (c)
  (:documentation "Returns the general category of a character as a
string.  C can be the character's code point \(a positive integer) or
a \(Lisp) character assuming its character code is also its Unicode
code point.  The second return value is the property symbol of the
category.

See also GENERAL-CATEGORIES.")
  (:method :around (c)
   (multiple-value-bind (name symbol)
       (call-next-method)
     (cond (name (values name symbol))
           (t (values "Cn" '#.(property-symbol "Cn"))))))
  (:method ((char character))
   (general-category (char-code char))))

(defgeneric bidi-class (c)
  (:documentation "Returns the bidirectional \(\"Bidi\") class of a
character as a string or NIL if there is no bidirectional class for
that particular character.  C can be the character's code point \(a
positive integer) or a \(Lisp) character assuming its character code
is also its Unicode code point.  The second return value \(if there is
one) is the property symbol of the class.

See also BIDI-CLASSES")
  (:method ((char character))
   (bidi-class (char-code char))))

(defun bidi-mirroring-glyph (c &key want-code-point-p)
  "Returns the Bidi mirroring glyph for a character if the character
has the \"BidiMirrored\" property and an appropriate mirroring glyph
is defined.  C can be the character's code point \(a positive integer)
or a \(Lisp) character assuming its character code is also its Unicode
code point.

Returns the code point instead of the character if WANT-CODE-POINT-P
is true.  This can be especially useful for Lisp implementations where
CHAR-CODE-LIMIT is smaller than +CODE-POINT-LIMIT+."
  (let ((code-point (bidi-mirroring-glyph% (ensure-code-point c))))
    (cond ((and code-point (not want-code-point-p))
           (code-char code-point))
          (t code-point))))

(defgeneric numeric-type (c)
  (:documentation "Returns the numeric type of a character as a string
or NIL if that particular character has no numeric type.  C can be the
character's code point \(a positive integer) or a \(Lisp) character
assuming its character code is also its Unicode code point.  The
second return value \(if there is one) is the property symbol of the
numeric type.")
  (:method ((char character))
   (numeric-type (char-code char))))

(defgeneric numeric-value (c)
  (:documentation "Returns the numeric value of a character as a Lisp
rational or NIL \(for NaN).  C can be the character's code point \(a
positive integer) or a \(Lisp) character assuming its character code
is also its Unicode code point.")
  (:method ((char character))
   (numeric-value (char-code char))))

(defgeneric combining-class (c)
  (:documentation "Returns the combining class of a character as a
non-negative integer.  C can be the character's code point \(a
positive integer) or a \(Lisp) character assuming its character code
is also its Unicode code point.")
  (:method :around (c)
   (or (call-next-method) 0))
  (:method ((char character))
   (combining-class (char-code char))))

(defgeneric has-binary-property (c property)
  (:documentation "Checks whether a character has the binary property
PROPERTY.  C can be the character's code point \(a positive integer)
or a \(Lisp) character assuming its character code is also its Unicode
code point.  PROPERTY can be a string naming the property or the
corresponding property symbol.  If a true value is returned, it is the
property symbol.

See also BINARY-PROPERTIES.")
  (:method ((char character) property)
   (has-binary-property (char-code char) property))
  (:method (char (property-name string))
   (has-binary-property char (lookup-property-alias property-name)))
  (:method ((code-point integer) (property-symbol symbol))
   (find property-symbol (binary-props code-point) :test #'eq)))

(defun uppercase-mapping (c &key want-code-point-p want-special-p context)
  "Returns the simple uppercase mapping of a character.  C can be the
character's code point \(a positive integer) or a \(Lisp) character
assuming its character code is also its Unicode code point.  Returns
the character itself if no such mapping is explicitly defined.  Note
that case mapping only makes sense for characters with the \"LC\"
property.

Returns the code point instead of the character if WANT-CODE-POINT-P
is true. Returns a list of code points if WANT-SPECIAL-P is true.
This can be especially useful for Lisp implementations where
CHAR-CODE-LIMIT is smaller than +CODE-POINT-LIMIT+."
  (if want-special-p
      (special-mapping c 1 context)
      (mapping c 1 want-code-point-p)))

(defun lowercase-mapping (c &key want-code-point-p want-special-p context)
  "Returns the lowercase mapping of a character.  C can be the
character's code point \(a positive integer) or a \(Lisp) character
assuming its character code is also its Unicode code point.  Returns
the character itself if no such mapping is explicitly defined.  Note
that case mapping only makes sense for characters with the \"LC\"
property.

Returns the code point instead of the character if WANT-CODE-POINT-P
is true. Returns a list of code points if WANT-SPECIAL-P is true.
This can be especially useful for Lisp implementations where
CHAR-CODE-LIMIT is smaller than +CODE-POINT-LIMIT+."
  (if want-special-p
      (special-mapping c 0 context)
      (mapping c 0 want-code-point-p)))

(defun titlecase-mapping (c &key want-code-point-p want-special-p context)
  "Returns the titlecase mapping of a character.  C can be the
character's code point \(a positive integer) or a \(Lisp) character
assuming its character code is also its Unicode code point.  Returns
the character itself if no such mapping is explicitly defined.  Note
that case mapping only makes sense for characters with the \"LC\"
property.

Returns the code point instead of the character if WANT-CODE-POINT-P
is true. Returns a list of code points if WANT-SPECIAL-P is true.
This can be especially useful for Lisp implementations where
CHAR-CODE-LIMIT is smaller than +CODE-POINT-LIMIT+."
  (if want-special-p
      (special-mapping c 2 context)
      (mapping c 2 want-code-point-p)))

(defun case-fold-mapping (c &key want-code-point-p)
  (case-folding c want-code-point-p))

(defun general-categories ()
  "Returns a sorted list of all general categories known to
CL-UNICODE.  These are the possible return values of
GENERAL-CATEGORY."
  (sort (mapcar 'property-name *general-categories*) 'string-lessp))

(defun scripts ()
  "Returns a sorted list of all scripts known to CL-UNICODE.  These
are the possible return values of SCRIPT."
  (sort (mapcar 'property-name *scripts*) 'string-lessp))

(defun code-blocks ()
  "Returns a sorted list of all blocks known to CL-UNICODE.  These are
the possible return values of CODE-BLOCK."
  (sort (mapcar 'property-name *code-blocks*) 'string-lessp))

(defgeneric canonical-decomposition (c)
  (:documentation "Decomposes input according to Unicode Canonical Decomposition rules.")
  (:method ((char character))
    (canonical-decomposition (char-code char)))
  (:method ((code-point integer))
    (let ((mapping (decomposition-mapping code-point)))
      (if (or (null mapping) (symbolp (car mapping)))
          (list code-point)
          (loop for c in mapping
                nconc (canonical-decomposition c))))))

(defgeneric compatibility-decomposition (c)
  (:documentation "Decomposes input according to Unicode Compatibility Decomposition rules.")
  (:method ((char character))
    (compatibility-decomposition (char-code char)))
  (:method ((code-point integer))
    (let ((mapping (decomposition-mapping code-point)))
      (if (null mapping)
          (list code-point)
          (loop for c in (if (symbolp (car mapping))
                             (rest mapping)
                             mapping)
                nconc (compatibility-decomposition c))))))

(defgeneric normalization-form-d (s)
  (:documentation "NFD decomposition - per character canonical decomposition followed by canonical sort. Returns list of code points.")
  (:method ((char character))
    (canonical-decomposition char))
  (:method ((code-point integer))
    (canonical-decomposition code-point))
  (:method ((chars list))
    (canonical-sort (loop for c in chars
                          nconc (normalization-form-d c))))
  (:method ((chars string))
    (canonical-sort (loop for c across chars
                          nconc (canonical-decomposition c)))))

(defgeneric normalization-form-k-d (s)
  (:documentation "NFKD decomposition - per character compatibility decomposition followed by canonical sort. Returns list of code points.")
  (:method ((char character))
    (compatibility-decomposition char))
  (:method ((code-point integer))
    (compatibility-decomposition code-point))
  (:method ((chars list))
    (canonical-sort (loop for c in chars
                          nconc (normalization-form-k-d c))))
  (:method ((chars string))
    (canonical-sort (loop for c across chars
                          nconc (compatibility-decomposition c)))))

(defgeneric normalization-form-c (s)
  (:documentation "NFC normalization - per character canonical decomposition followed by canonical sort and canonical composition. Returns list of code points.")
  (:method ((char character))
    (normalization-form-c (char-code char)))
  (:method ((code-point integer))
    (normalization-form-c (list code-point)))
  (:method (chars)
    (canonical-composition (normalization-form-d chars))))

(defgeneric normalization-form-k-c (s)
  (:documentation "NFKC normalization - per character compatibility decomposition followed by canonical sort and canonical composition. Returns list of code points.")
  (:method ((char character))
    (normalization-form-k-c (char-code char)))
  (:method ((code-point integer))
    (normalization-form-k-c (list code-point)))
  (:method (chars)
    (canonical-composition (normalization-form-k-d chars))))

(defun binary-properties ()
  "Returns a sorted list of all binary properties known to CL-UNICODE.
These are the allowed second arguments \(modulo canonicalization) to
HAS-BINARY-PROPERTY."
  (sort (mapcar 'property-name *binary-properties*) 'string-lessp))

(defun bidi-classes ()
  "Returns a sorted list of all Bidi classes known to CL-UNICODE.
These are the possible return values of BIDI-CLASS."
  (sort (mapcar 'property-name *bidi-classes*) 'string-lessp))

(defun recognized-properties (&optional all)
  "Returns a list of all property names known to CL-UNICODE.  These
are the allowed second arguments \(modulo canonicalization) to
HAS-PROPERTY.  If ALL is true, known aliases \(like \"Letter\" for
\"L\") are also included."
  (sort (cond (all (loop for key being the hash-keys of *property-map*
                         collect key))
              (t (loop for key being the hash-keys of *property-tests*
                       collect (property-name key))))
        'string-lessp))

(defgeneric property-test (property &key errorp)
  (:documentation "Returns a unary function which can test code points
or Lisp characters for the property PROPERTY.  PROPERTY is interpreted
as in HAS-PROPERTY and PROPERTY-TEST is actually used internally by
HAS-PROPERTY but might come in handy if you need a faster way to test
for PROPERTY \(as you're saving the time to look up the property).

Returns NIL if no property named PROPERTY was found or signals an
error if ERRORP is true.")
  (:method ((property-name string) &key errorp)
   (property-test (or (gethash (canonicalize-name property-name) *property-map*)
                      (and errorp (signal-unicode-error "There is no property called ~S."
                                                        property-name)))))
  (:method ((property-symbol symbol) &key errorp)
   (or (gethash property-symbol *property-tests*)
       (and errorp (signal-unicode-error "There is no property called ~S." property-symbol)))))

(defun has-property (c property)
  "Checks whether a character has the named property PROPERTY.
PROPERTY can be a string naming a property \(which will be used for
look-up after canonicalization) or it can be a property symbol \(see
PROPERTY-SYMBOL).  C can be the character's code point \(a positive
integer) or a \(Lisp) character assuming its character code is also
its Unicode code point.

\"Properties\" in the sense of CL-UNICODE can be names of general
categories, scripts, blocks, binary properties, or Bidi classes,
amongst other things.  If there are a block and a script with the same
name \(like, say, \"Cyrillic\"), the bare name denotes the script.
Prepend \"Block:\" to the name to refer to the block.  \(You can also
prepend \"Script:\" to refer to the script unambiguously.)  Names of
Bidi classes must be prepended with \"BidiClass:\" if there's a
potential for ambiguity.

This function also recognizes several aliases for properties \(like
\"Symbol\" for \"S\") and you can, as in Perl, prepend block names
with \"In\" instead of \"Block:\" and most other properties with
\"Is\".  See RECOGNIZED-PROPERTIES.

See also PROPERTY-TEST."
  (funcall (property-test property :errorp t) c))

(defun list-all-characters (property &key want-code-point-p)
  "Lists all character \(ordered by code point) which have the
property PROPERTY where PROPERTY is interpreted as in HAS-PROPERTY.
If WANT-CODE-POINT-P is true, a list of code points instead of a list
of characters is returned.  \(If CHAR-CODE-LIMIT is smaller than
+CODE-POINT-LIMIT+ in your Lisp implementation, the list of code
points can actually be longer than the list of characters.)."
  (loop with test-function = (property-test property :errorp t)
        for code-point below (if want-code-point-p +code-point-limit+ char-code-limit)
        for thing = (if want-code-point-p code-point (code-char code-point))
        when (and thing (funcall (the function test-function) code-point))
        collect thing))

(defmacro enable-alternative-character-syntax ()
  "Enables an alternative Lisp character syntax which /replaces/ the
usual syntax: After a sharpsign and a backslash have been read, at
least one more character is read.  Reading then continues as long as
ASCII letters, digits, underlines, hyphens, colons, or plus signs are
read.  The resulting string is then used as input to CHARACTER-NAMED
to produce a character.

This macro expands into an EVAL-WHEN so that if you use it as a
top-level form in a file to be loaded and/or compiled it'll do what
you expect.  Technically, this'll push the current readtable on a
stack so that matching calls of this macro and
DISABLE-ALTERNATIVE-CHARACTER-SYNTAX can be nested.

Note that by default the alternative character syntax is not enabled
after loading CL-UNICODE."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-alternative-character-syntax)))

(defmacro disable-alternative-character-syntax ()
  "Restores the readtable which was active before the last call to
ENABLE-ALTERNATIVE-CHARACTER-SYNTAX.  If there was no such call, the
standard readtable is used.

This macro expands into an EVAL-WHEN so that if you use it as a
top-level form in a file to be loaded and/or compiled it'll do what
you expect.  Technically, this'll pop a readtable from the stack
described in ENABLE-ALTERNATIVE-CHARACTER-SYNTAX so that matching
calls of these macros can be nested."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%disable-alternative-character-syntax)))
