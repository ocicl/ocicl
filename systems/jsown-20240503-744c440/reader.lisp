(in-package :jsown)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 0) (debug 0))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +compile-unescape-json-strings+ t
    "Compiles support for unescaping json strings.
 If you set this to nil upon compilation time strings and keywords aren't escaped.  This makes the library incompliant with json, but it does make it a few % faster.
 Could be handy when used in a mapreduce situation where you don't mind debugging and speed is of utmost importance.")
  (defconstant +assume-fixnums+ nil
  "Compiles under the expectation that numbers (being integers and the float and non-float part of floats are fixnums.  By default this is turned off.  The performance hit seems to be around 2% to 8% in the mixed reader speed test."))

(defparameter *parsed-true-value* t
  "value to emit when parsing json's 'true'")
(defparameter *parsed-false-value* nil
  "value to emit when parsing json's 'false'")
(defparameter *parsed-null-value* nil
  "value to emit when parsing json's 'null'")
(defparameter *parsed-empty-list-value* nil
  "value to emit when parsing a json empty list '[]'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; character-tree support
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun build-character-tree (&rest strings)
  "Builds a character tree from a set of strings"
  (build-tree (loop for string in strings collect
                   (loop for char across (the simple-string string) collect char))))

(define-compiler-macro build-character-tree (&whole form &rest strings)
  (if (loop for string in strings unless (stringp string) return t)
      form
      `(quote ,(apply #'build-character-tree strings))))

(defun find-first-elts (lists)
  (remove-duplicates (loop for list in lists
                        when (first list)
                        collect (first list))
                     :test #'eql))

(defun build-tree (lists)
  "Builds a tree from a range of lists and a function to compare its elements by"
  (when lists
    (loop for first-elt in (find-first-elts lists)
       collect (let ((matching-lists (loop for list in lists when (and (first list) (eql (the character first-elt) (the character (first list))))
                                        collect (rest list))))
                 (list first-elt
                       (loop for list in matching-lists unless list return t) ;; t shows that this is an end-result
                       (build-tree matching-lists))))))

(defun iterate-tree (tree char)
  "Iterates a character-tree with the given character
 Returns two values, being the new tree and whether or not this is an end-point."
  (declare (type (or cons nil) tree)
           (type character char))
  (let ((solution (rest (find char tree :key #'first :test #'eql))))
    (when solution
      (values (second solution) (first solution)))))

;;;;;;;;;;;;;;;;;;;
;;;; buffer support
;;;;;;;;;;;;;;;;;;;

(defstruct buffer
  "A string-buffer which is used to operate on the strings
 The use of a string-buffer allows us to read the data in bulk, and to operate on it, by using simple index manipulations.
 Reading the string up front removes the hassle of having a fixed-size maximal input"
  (string ""
          :type simple-string) ; This contains the content of the buffer
  (index 0 :type fixnum) ; This is the current index of the buffer
  (mark 0 :type fixnum)) ; This contains a single number to indicate the start of a region.  The user must ensure that this does not get overwritten himself

(defun build-buffer (string)
  "Makes a new buffer and ensures the string is of the correct type"
  (make-buffer :string (if (typep string 'simple-string)
                           string
                           (coerce string 'simple-string))))

(declaim (inline next-char next-char/ next-char/i decr-char current-char peek-behind-char fetch-char subseq-buffer-mark mark-buffer mark-length skip-to skip-to/ skip-until skip-until/ skip-until* subseq-until subseq-until/ subseq-tree char-in-arr subseq-until/unescape unescape-string/count high-surrogate-p))

(defun high-surrogate-p (code-value)
  "character numbers between U+D800 and U+DFFF (inclusive) are
   reserved for use with the UTF-16 encoding form (as surrogate pairs)
   and do not directly represent characters. "
  (declare (type (integer 0 #.char-code-limit) code-value))
  (and (<= #xD800 code-value) (>= #xDFFF code-value)))

(defun next-char (buffer)
  (declare (type buffer buffer))
  "Sets the pointer to the next char in the buffer"
  (incf (buffer-index buffer)))
(defun next-char/ (buffer)
  (declare (type buffer buffer))
  "Sets the pointer to the next char in the buffer, ignores escaped characters (they start with a \\) through"
  (next-char buffer)
  (loop until (char/= (current-char buffer) #\\)
     if (progn (next-char buffer)
           (char= (current-char buffer) #\u))
     do
     ;; UTF-16 escapes are \uAAAA wide
       (let ((code-value (parse-integer (subseq (buffer-string buffer)
                                                    (+ (buffer-index buffer) 1)
                                                    (+ (buffer-index buffer) 5))
                                            :radix 16)))
         (declare (type (integer 0 #.char-code-limit) code-value))
         (if (high-surrogate-p code-value)
             (incf (buffer-index buffer) 11) ; 11 to skip the next escape
             (incf (buffer-index buffer) 5)))
     else
     do
       (incf (buffer-index buffer))))
(defun next-char/i (buffer)
  (declare (type buffer buffer))
  "Does what next-char/ does, but returns nil if no char was skipped or t if a char was skipped."
  (next-char buffer)
  (let ((skipped-characters 0))
    (loop until (char/= (current-char buffer) #\\)
       if (progn (next-char buffer)
             (char= (current-char buffer) #\u))
       do
         (let ((code-value (parse-integer (subseq (buffer-string buffer)
                                                    (+ (buffer-index buffer) 1)
                                                    (+ (buffer-index buffer) 5))
                                              :radix 16)))
           (declare (type (integer 0 #.char-code-limit) code-value))
           (cond ((high-surrogate-p code-value)
                  (incf (the fixnum skipped-characters) 11) ; 11 to skip the next escape
                  (incf (buffer-index buffer) 11))

                 (t 
                  (incf (the fixnum skipped-characters) 5)
                  (incf (buffer-index buffer) 5))))
       else
       do
         (incf (the fixnum skipped-characters) 1)
         (incf (buffer-index buffer)))
    skipped-characters))
(defun decr-char (buffer)
  (declare (type buffer buffer))
  "Sets the pointer to the previous char in the buffer"
  (decf (buffer-index buffer)))
(defun current-char (buffer)
  (declare (type buffer buffer))
  "Returns the current character the buffer is pointing to"
  (elt (buffer-string buffer) (buffer-index buffer)))
(defun peek-behind-char (buffer)
  (declare (type buffer buffer))
  (elt (buffer-string buffer) (1- (buffer-index buffer))))
(defun fetch-char (buffer)
  (declare (type buffer buffer))
  "Reads a character from the buffer and increases the index"
  (next-char buffer)
  (peek-behind-char buffer))
(defun subseq-buffer-mark (buffer)
  (declare (type buffer buffer))
  "Returns the content between index and mark for the current buffer
 result: (subseq buffer-string mark index))"
  (subseq (buffer-string buffer) (buffer-mark buffer) (buffer-index buffer)))
(defun mark-buffer (buffer)
  "Sets the mark of the buffer to the current character"
  (setf (buffer-mark buffer) (buffer-index buffer)))
(defun mark-length (buffer)
  (declare (type buffer buffer))
  "Returns the current amount of characters in the marked piece of the buffer"
  (the fixnum (- (buffer-index buffer) (buffer-mark buffer))))

(defun skip-to (buffer last-char)
  "Skips characters until <char> has been found.  <char> is the last char which is skipped
 see: skip-until"
  (declare (type buffer buffer)
           (type character last-char))
  (skip-until buffer last-char)
  (next-char buffer))
(defun skip-to/ (buffer last-char)
  "What skip-to does, but with the ignoring of \\"
  (declare (type buffer buffer)
           (type character last-char))
  (skip-until/ buffer last-char)
  (next-char/ buffer))
(defun skip-until (buffer last-char)
  "Skips characters until <char> has been found.  <char> is NOT skipped
 See: skip-to"
  (declare (type buffer buffer)
           (type character last-char))
  (loop until (eql (current-char buffer) last-char)
     do (next-char buffer))
  (values))
(defun skip-until/ (buffer last-char)
  "What skip-until does, but with \\ escaping"
  (declare (type buffer buffer)
           (type character last-char))
  (decr-char buffer)
  (loop do (next-char/ buffer)
     until (eql (current-char buffer) last-char)))

(defun char-in-arr (char char-arr)
  "Returns t if <char> is found in <char-arr>, returns nil otherwise"
  (declare (type simple-string char-arr)
           (type character char))
  (loop for c across char-arr
     when (eql char (the character c))
     do (return-from char-in-arr t))
  nil)

(defun skip-until* (buffer char-arr)
  "Skips characters until one of the characters in <char-arr> has been found.  The character which was found is not read from the buffer."
  (declare (type simple-string char-arr)
           (type buffer buffer))
  (loop until (char-in-arr (current-char buffer) char-arr)
     do (next-char buffer)))

(defun subseq-until (buffer char-arr)
  "Returns a subsequence of stream, reading everything before a character belonging to char-arr is found.  The character which was found is not read from the buffer"
  (declare (type buffer buffer)
           (type simple-string char-arr))
  (mark-buffer buffer)
  (loop until (char-in-arr (current-char buffer) char-arr)
     do (next-char buffer))
  (subseq-buffer-mark buffer))

(defun subseq-until/ (buffer last-char)
  "Does what subseq-until does, but does escaping too"
  (declare (type buffer buffer)
           (type character last-char))
  (mark-buffer buffer)
  (decr-char buffer)
  (loop do (next-char/ buffer)
     until (eql (current-char buffer) last-char))
  (subseq-buffer-mark buffer))

(defun unescape-string/count (buffer count)
  "Unescapes the given string based on JSOWN's spec"
  (declare (type buffer buffer)
           (type fixnum count))
  (let ((result (make-array (- (buffer-index buffer) (buffer-mark buffer) count)
                            :element-type 'character
                            :adjustable nil)))
    (let ((escaped-p nil)
          (target-string-index 0))
      (loop for buffer-index from (buffer-mark buffer) below (buffer-index buffer)
         for c = (elt (buffer-string buffer) buffer-index)
         do
           (if escaped-p
               (progn (setf escaped-p nil)
                      (setf (elt result target-string-index)
                            (case c
                              (#\b #\Backspace)
                              (#\f #\Linefeed)
                              (#\n #\Linefeed)
                              (#\r #\Return)
                              (#\t #\Tab)
                              (#\u (let ((high-surrogate
                                          (parse-integer
                                           (subseq (buffer-string buffer)
                                                   (+ buffer-index 1) ; after 'u'
                                                   (+ buffer-index 5)) ; 5 places
                                           :radix 16)))
                                     (declare (type (integer 0 #.char-code-limit) high-surrogate))
                                     (if (high-surrogate-p high-surrogate)
                                         (let ((low-surrogate (parse-integer
                                                               (subseq (buffer-string buffer)
                                                                       (+ buffer-index 7) ; after second 'u'
                                                                       (+ buffer-index 11)) ; 5 places
                                                               :radix 16)))
                                           (declare (type (integer 0 #.char-code-limit) low-surrogate))
                                           (prog1 (code-char (+ #x10000 (- low-surrogate #xDC00)
                                                                (* #x400  (- high-surrogate #xD800))))
                                             (incf buffer-index 10)))
                                         (prog1 (code-char high-surrogate) (incf buffer-index 4)))))
                              (t c)))
                      (incf target-string-index))
               (progn (if (eql c #\\)
                          (setf escaped-p t)
                          (progn (setf (elt result target-string-index) c)
                                 (incf target-string-index)))))))
    result))

(defun subseq-until/unescape (buffer last-char)
  "Does what subseq-until/ does, but unescapes the returned string"
  (declare (type buffer buffer)
           (type character last-char))
  (mark-buffer buffer)
  (decr-char buffer)
  (let ((unescape-count 0))
    ;; Work around ECL bug. See https://gitlab.com/embeddable-common-lisp/ecl/issues/401
    (loop do (incf #-ecl (the fixnum unescape-count)
                   #+ecl unescape-count
                   (the fixnum (next-char/i buffer)))
       until (eql (current-char buffer) last-char))
    (if (> unescape-count 0)
        (unescape-string/count buffer unescape-count)
        (subseq-buffer-mark buffer))))

(defun subseq-tree (buffer end-char tree)
  "Returns a sequence of the buffer, reading everything that matches with the given tree before end-char is found.  end-char is not read from the buffer
 Returns nil if no sequence matching the tree could be found.  It then stops iterating at the failed position
 Skips #\\"
  (declare (type buffer buffer)
           (type character end-char))
  (next-char buffer)
  (mark-buffer buffer)
  (decr-char buffer)
  (let ((accepted-p nil))
    (loop
       while (progn (next-char/ buffer)
                (and tree (char/= (current-char buffer) end-char)))
       do (multiple-value-setq (tree accepted-p) (iterate-tree tree (current-char buffer))))
    (values accepted-p
            (if accepted-p (subseq-buffer-mark buffer) ""))))

;;;;;;;;;;;;;;;;;;;;;;;
;;;; Parsing of json
;;;;;;;;;;;;;;;;;;;;;;;
(defun read-object (buffer)
  "reads an object, starting with { and ending with } into a in internal jsown object"
  (declare (type buffer buffer))
  (skip-until* buffer "{")
  (cons :obj
        (loop until (progn (skip-until* buffer "\"}") ; a string or the end of the objects are our only interests
                    (when (eql (current-char buffer) #\})
                      (next-char buffer) t))
           collect (cons (read-key buffer) ; we know that the first character is the " of the key
                         (progn (skip-to buffer #\:)
                            (read-value buffer))))))

(defun read-partial-object (buffer tree)
  "Reads an object from the buffer, but only when the key matches a key in the tree"
  (declare (type buffer buffer)
           (type (or cons nil) tree))
  (skip-until* buffer "{")
  (cons :obj
        (loop until (progn (skip-until* buffer "\"}")
                    (when (eql (current-char buffer) #\})
                      (next-char buffer) t))
           append (multiple-value-bind (found-p key)
                      (read-partial-key buffer tree)
                    (progn (skip-to buffer #\:)
                       (if found-p
                           (list (cons key (read-value buffer)))
                           (progn (skip-value buffer) nil)))))))

(defun skip-object (buffer)
  "Skips an object from the buffer
  PRE: Assumes the buffer's index is at the starting { of the object
  POST: The buffer's index is right after the ending } of the object"
  (declare (type buffer buffer))
  (loop until (progn (skip-until* buffer "\"}")
              (when (eql (current-char buffer) #\})
                (next-char buffer) t))
     do (progn (skip-key buffer)
               (skip-value buffer))))

(defun read-partial-key (buffer tree)
  "reads a key from the buffer. 
  PRE: Assumes the buffer's index is at the starting \" of the key
  POST: Returns (values key t) if the key was found as a valid key in the tree, or (values nil nil) if it was not
  POST: The buffer's index is right after the ending \" of the key"
  (declare (type buffer buffer)
           (type (or cons nil) tree))
  (multiple-value-bind (accepted-p solution)
      (subseq-tree buffer #\" tree)
    (declare (type (or nil t) accepted-p)
             (type simple-string solution))
    (skip-to/ buffer #\") ;; skip everything we needn't know
    (values accepted-p solution)))

(defun read-key (buffer)
  "Reads a key from the key-value list.
  PRE: Assumes the buffer's index is at the starting \" of the key
  POST: The buffer's index is right after the ending \" of the key"
  (declare (type buffer buffer))
  (parse-string buffer))

(defun skip-key (buffer)
  "Skips a key from the key-value list.
  PRE: Assumes the buffer's index is at the starting \" of the key
  POST: The buffer's index is right after the ending \" of the key"
  (declare (type buffer buffer))
  (skip-string buffer))

(defun read-value (buffer)
  "Reads a value from the stream.
 This searches for the first meaningful character, and delegates to the right function for that character"
  (declare (type buffer buffer))
  (skip-until* buffer "\"{[tfn0123456789-")
  (case (current-char buffer)
    (#\" (parse-string buffer))
    (#\{ (read-object buffer))
    (#\[ (read-array buffer))
    (#\t (incf (buffer-index buffer) 4)
         *parsed-true-value*)
    (#\f (incf (buffer-index buffer) 5)
         *parsed-false-value*)
    (#\n (incf (buffer-index buffer) 4)
         *parsed-null-value*)
    (t (read-number buffer))))

(defun skip-value (buffer)
  "Skips a value from the stream.
 This searches for the first meaningful character, and delegates to the right function for skipping that"
  (declare (type buffer buffer))
  (skip-until* buffer "\"{[tfn0123456789-")
  (case (current-char buffer)
    (#\" (skip-string buffer))
    (#\{ (skip-object buffer))
    (#\[ (skip-array buffer))
    (#\t (incf (buffer-index buffer) 4))
    (#\f (incf (buffer-index buffer) 5))
    (#\n (incf (buffer-index buffer) 4))
    (t (skip-number buffer)))
  (values))

(defun skip-string (buffer)
  (declare (type buffer buffer))
  "Skips the contents of an input string from the buffer.
  PRE: assumes the buffer's index is at the starting \"
  POST: the buffer's index is right after the ending \" "
  (next-char buffer)
  (skip-to/ buffer #\"))

(defun parse-string (buffer)
  "Reads a JSON string from the stream 
  PRE: assumes the buffer's index is at the starting \"
  POST: returns the matching string without converting escaped characters to their internal representation
  POST: the buffer's index is right after the ending \" "
  (declare (type buffer buffer))
  (next-char buffer)
  (let ((result (if +compile-unescape-json-strings+
                    (subseq-until/unescape buffer #\")
                    (subseq-until/ buffer #\"))))
    (next-char buffer)
    result))

(defun skip-array (buffer)
  (declare (type buffer buffer))
  "Skips the contents of an array from the buffer
  PRE: assumes the buffer's index is at the starting [
  POST: the buffer's index is right after the ending ]"
  (next-char buffer)
  (skip-until* buffer "]\"{[tfn0123456789-")
  (if (eql (current-char buffer) #\])
      (next-char buffer)
      (loop
         collect (skip-value buffer)
         until (progn (skip-until* buffer ",][")
                  (eql (fetch-char buffer) #\]))))) ; fetch-char reads the character from the stream, thus forwarding us to the correct position for skip-value and dropping the last #\] from the line.

(defun read-array (buffer)
  "Reads a JSON array from the stream
  PRE: assumes the buffer's index is at the starting [
  POST: returns a list containing all read objects
  POST: the buffer's index is right after the ending ]"
  (declare (type buffer buffer))
  (next-char buffer)
  (skip-until* buffer "]\"{[tfn0123456789-") ; the first intering object is the start of any new object, or the immediate end of this array
  (if (eql (current-char buffer) #\])
      (progn (next-char buffer) *parsed-empty-list-value*)
      (loop 
         collect (read-value buffer)
         until (progn (skip-until* buffer ",][")
                  (eql (fetch-char buffer) #\])))))

(eval-when (:compile-toplevel)
  (defun create-parse-number-code (&key exponent-p float-p)
    "Creates the code to parse a number.
  It is assumed that the whole number is placed in an accessible variable whole-number, available at runtime. 
  If exponent-p is non-nil it is assumed that the exponent is placed in the variable exp, available at runtime.
  If float-p is non-nil it is assumed that the float is placed in the variable float, available at runtime.
  If float-p is non-nil it is assumed that the amount of numbers in the float is placed in the variable float-digits, available at runtime."
    (cond ((and exponent-p float-p)
           `(* negate-number
               (+ whole-number
                  (/ float (expt 10 float-digits)))
               (expt 10 (* negate-exp exp))))
          (exponent-p
           `(* negate-number
               whole-number
               (expt 10 (* negate-exp exp))))
          (float-p
           `(* negate-number
               (+ whole-number
                  (/ float (expt 10 float-digits)))))
          (t
           `(* negate-number
               whole-number)))))

(defmacro set-read-number-part (currently-reading buffer &body body)
  (case currently-reading
    (:whole `(let ((whole-number number))
               (declare (type ,(if +assume-fixnums+ 'fixnum 'integer) whole-number))
               ,@body))
    (:float `(let ((float number)
                   (float-digits (mark-length ,buffer)))
               (declare (type ,(if +assume-fixnums+ 'fixnum 'integer) float float-digits))
               ,@body))
    (:exponent `(let ((exp number))
                  (declare (type fixnum exp))
                  ,@body))))

(defmacro read-number* (buffer &key (currently-reading :whole) (exponent-p t) (float-p t) (float-delimiters ".") (exp-delimiters "eE") (number-delimiters ",]} 
"))
  "This macro should be compared to inlined functions with respect to speed.  The macro creates a tree of spaghetti code that can read jsown numbers to lisp numbers."
  (labels ((delimiters-for (exponent-p float-p)
             (concatenate 'string (if float-p float-delimiters "") (if exponent-p exp-delimiters "") number-delimiters)))
    (let ((delimiters (delimiters-for exponent-p float-p)))
      `(progn
         (case (current-char ,buffer)
           (#\- (next-char ,buffer)
                ,(if (eq currently-reading :exponent)
                     `(setf negate-exp -1)
                     `(setf negate-number -1)))
           (#\+ (next-char ,buffer)))
         (let ((number (parse-integer (subseq-until ,buffer ,delimiters))))
           (declare (type ,(if +assume-fixnums+ 'fixnum 'integer) number))
           (cond ,@(concatenate 
                    'list
                    (when float-p
                      `(((char-in-arr (current-char ,buffer) ,float-delimiters)
                         (set-read-number-part ,currently-reading ,buffer
                           (next-char ,buffer) ; we can skip the matching character in float-delimiters after the variables have been set
                           (read-number* ,buffer :currently-reading :float :exponent-p ,exponent-p :float-p nil :float-delimiters ,float-delimiters :exp-delimiters ,exp-delimiters :number-delimiters ,number-delimiters)))))
                    (when exponent-p
                      `(((char-in-arr (current-char ,buffer) ,exp-delimiters)
                         (set-read-number-part ,currently-reading ,buffer
                           (next-char ,buffer) ; we can skip the matching character in exp-delimiters after the variables have been set
                           (read-number* ,buffer :currently-reading :exponent :exponent-p nil :float-p ,float-p :float-delimiters ,float-delimiters :exp-delimiters ,exp-delimiters :number-delimiters ,number-delimiters)))))
                    `((t
                       (set-read-number-part ,currently-reading ,buffer
                         ,(create-parse-number-code :exponent-p (not exponent-p) :float-p (not float-p))))))))))))

(defun read-number (buffer)
  "Reads a number from the buffer.
  PRE: assumes the index is pointing to the first character representing the number
  POST: the value of the character is returned
  POST: the buffer's index is at the position right after the last character representing the number"
  (declare (type buffer buffer))
  (let ((negate-exp 1)
        (negate-number 1))
    (read-number* buffer)))

(defun skip-number (buffer)
  "Skips a number from the buffer
  PRE: assumes the index is pointing to the first character representing the number.
  POST: the buffer's index is at the position right after the last character representing the number, possibly skipping spaces after that position"
  (declare (type buffer buffer))
  (skip-until* buffer ",]}")) ; a number can only occur within an array or the value part of an object.  As such we can skip all characters until we see one of .]} which will become the character under the index of the buffer.

;;;;;;;;;;;;;;;;;;;
;;;; User interface
(defun build-key-container (&rest keywords-to-read)
  "Builds an internal structure to speed up the keywords which you can read.  This should be used when the keywords needed are not known at compiletime, but you still want to parse those keywords of a lot of documents.
 If the keywords you are interested in are known at compiletime, the use of #'parse will automatically expand the kewords at compiletime.
 parse-with-container takes the result of this function and will return the keywords which have been inserted here."
  (apply #'build-character-tree keywords-to-read))
(define-compiler-macro build-key-container (&rest keywords-to-read)
  `(build-character-tree ,@keywords-to-read))

(defun parse-with-container (json-string container)
  "Parses the keywords which have been specified in the container from the  json string json-string.
 For most cases you can just use the parse function without a special key container.  This is only here to support some cases where the building of the key container takes too much time.  
 See #'parse for the normal variant.
 See #'build-key-container for a way to build new keyword containers."
  (let ((buffer (build-buffer json-string)))
    (read-partial-object buffer container)))

(defun parse (string &rest keywords-to-read)
  "Reads a json object from the given string, with the given keywords being the keywords which are fetched from the object.
 All parse functions assume <string> is not an empty json object.  (string/= string \"{}\")"
  (let ((buffer (build-buffer string)))
    (if keywords-to-read
        (read-partial-object buffer (apply #'build-character-tree keywords-to-read))
        (read-value buffer))))
(define-compiler-macro parse (&whole whole string &rest keywords-to-read) ; this allows the character tree to be precompiled
  (if keywords-to-read
      `(let ((buffer (build-buffer ,string)))
         (read-partial-object buffer (build-character-tree ,@keywords-to-read)))
      whole))

(defmacro with-injective-reader (&body body)
  "Rebinds *parsed-*-value* so that reading json documents is injective and converting them back to json yields roughly the same document as the original.
Rebinds:
- *parsed-true-value* => :true
- *parsed-false-value* => :false
- *parsed-null-value* => :null"
  `(let ((*parsed-true-value* :true)
         (*parsed-false-value* :false)
         (*parsed-null-value* :null)
         ;; (*parsed-empty-list-value* :empty-list)
         )
     ,@body))

(defun make-jsown-filter (value first-spec &rest other-specs)
  "Fancy filtering for jsown-parsed objects, functional implementation.  look at jsown-filter for a working version."
  (case first-spec
    (cl:map (let ((tmpvar (gensym "mapped-obj")))
              `(mapcar (lambda (,tmpvar) ,(apply #'make-jsown-filter tmpvar other-specs)) ,value)))
    (otherwise (let ((intermediate-computation `(jsown:val ,value ,first-spec)))
                 (if other-specs
                     (apply #'make-jsown-filter intermediate-computation other-specs)
                     intermediate-computation)))))

(defmacro filter (value &rest specs)
  "Fancy filtering for jsown-parsed objects.
spec can be one of the following:
[object] key to find.  will transform into (jsown:val value key)
[cl:map] use this modifier with an [object] modifier after it, to filter all elements in the list."
  (apply #'make-jsown-filter value specs))

(defun test-reader-speed (iterations)
  (let ((cur-time (get-internal-run-time)))
    (loop for x from 0 below iterations
       do (jsown:parse "{\"foo\":\"bar\",\"baz\":1000,\"bang\":100.10,\"bingo\":[\"aa\",10,1.1],\"bonzo\":{\"foo\":\"bar\",\"baz\":1000,\"bang\":100.10}}"))
    (/ (* iterations internal-time-units-per-second) (- (get-internal-run-time) cur-time))))
