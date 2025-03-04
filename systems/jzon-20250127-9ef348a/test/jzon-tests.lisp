(defpackage #:com.inuoe.jzon-tests
  (:use #:cl)
  (:import-from
   #:alexandria
   #:hash-table-keys
   #:plist-hash-table)
  (:import-from
   #:fiveam
   #:def-suite
   #:finishes
   #:in-suite
   #:is
   #:is-every
   #:signals
   #:test)
  (:import-from #:flexi-streams)
  (:import-from
   #:com.inuoe.jzon
   #:*writer*
   #:begin-array
   #:begin-array*
   #:begin-object
   #:coerced-fields
   #:close-parser
   #:close-writer
   #:end-array
   #:end-array*
   #:json-eof-error
   #:json-error
   #:json-parse-error
   #:json-parse-limit-error
   #:json-recursive-write-error
   #:json-write-error
   #:json-write-limit-error
   #:make-writer
   #:parse
   #:parse-next
   #:parse-next-element
   #:span
   #:stringify
   #:with-array
   #:with-array*
   #:with-parser
   #:with-object
   #:with-object*
   #:with-writer
   #:with-writer*
   #:write-array
   #:write-array*
   #:write-key
   #:write-key*
   #:write-object
   #:write-object*
   #:write-properties
   #:write-properties*
   #:write-property
   #:write-property*
   #:write-value
   #:write-value*
   #:write-values
   #:write-values*)
  (:import-from #:uiop)
  (:export
   #:jzon
   #:run
   #:main))

(in-package #:com.inuoe.jzon-tests)

(def-suite jzon
  :description "Tests for the jzon library.")

(defun run ()
  (fiveam:run! 'jzon))

(defun main (&rest args)
  (declare (ignore args))
  (let ((result (run)))
    (if result 0 -1)))

(in-suite jzon)

(def-suite parsing :in jzon)

(in-suite parsing)

(defun ph (&rest plist)
  "Shorthand for plist-hash-table."
  (plist-hash-table plist :test 'equal))

(defun utf-8 (string)
  (flexi-streams:string-to-octets string :external-format :utf-8))

(defun not-simple (vector)
  (make-array (length vector) :element-type (array-element-type vector) :fill-pointer t :initial-contents vector))

(defmacro bits-double-float (x)
  #-ecl
  `(org.shirakumo.float-features:bits-double-float ,x)
  #+ecl
  (if (find-symbol (string '#:bits-double-float) '#:si)
    `(,(intern (string '#:bits-double-float) '#:si) ,x)
    (let ((tmp (gensym (string 'tmp))))
      `(ffi:with-foreign-object (,tmp :double)
        (setf (ffi:deref-pointer ,tmp :uint64-t) ,x)
        (ffi:deref-pointer ,tmp :double)))))

(test parses-atoms
  (is (eq 'null (parse "null")))
  (is (eq 'null (parse "  null")))
  (is (eq 't    (parse "true")))
  (is (eq 't    (parse "  true")))
  (is (eq 'nil  (parse "false")))
  (is (eq 'nil  (parse "  false")))
  (is (eq 'null (parse (utf-8 "null"))))
  (is (eq 'null (parse (utf-8 "  null"))))
  (is (eq 't    (parse (utf-8 "true"))))
  (is (eq 't    (parse (utf-8 "  true"))))
  (is (eq 'nil  (parse (utf-8 "false"))))
  (is (eq 'nil  (parse (utf-8 "  false")))))

(test parses-atoms-error-on-incomplete
  (signals (json-parse-error) (parse "nul   "))
  (signals (json-parse-error) (parse "nu    "))
  (signals (json-parse-error) (parse "n     "))
  (signals (json-parse-error) (parse "nul"))
  (signals (json-parse-error) (parse "nu"))
  (signals (json-parse-error) (parse "n"))
  (signals (json-parse-error) (parse "tru    "))
  (signals (json-parse-error) (parse "tr     "))
  (signals (json-parse-error) (parse "t      "))
  (signals (json-parse-error) (parse "tru"))
  (signals (json-parse-error) (parse "tr"))
  (signals (json-parse-error) (parse "t"))
  (signals (json-parse-error) (parse "fals   "))
  (signals (json-parse-error) (parse "fal    "))
  (signals (json-parse-error) (parse "fa     "))
  (signals (json-parse-error) (parse "f      "))
  (signals (json-parse-error) (parse "fals"))
  (signals (json-parse-error) (parse "fal"))
  (signals (json-parse-error) (parse "fa"))
  (signals (json-parse-error) (parse "f"))

  (signals (json-parse-error) (parse (utf-8 "nul   ")))
  (signals (json-parse-error) (parse (utf-8 "nu    ")))
  (signals (json-parse-error) (parse (utf-8 "n     ")))
  (signals (json-parse-error) (parse (utf-8 "nul")))
  (signals (json-parse-error) (parse (utf-8 "nu")))
  (signals (json-parse-error) (parse (utf-8 "n")))
  (signals (json-parse-error) (parse (utf-8 "tru    ")))
  (signals (json-parse-error) (parse (utf-8 "tr     ")))
  (signals (json-parse-error) (parse (utf-8 "t      ")))
  (signals (json-parse-error) (parse (utf-8 "tru")))
  (signals (json-parse-error) (parse (utf-8 "tr")))
  (signals (json-parse-error) (parse (utf-8 "t")))
  (signals (json-parse-error) (parse (utf-8 "fals   ")))
  (signals (json-parse-error) (parse (utf-8 "fal    ")))
  (signals (json-parse-error) (parse (utf-8 "fa     ")))
  (signals (json-parse-error) (parse (utf-8 "f      ")))
  (signals (json-parse-error) (parse (utf-8 "fals")))
  (signals (json-parse-error) (parse (utf-8 "fal")))
  (signals (json-parse-error) (parse (utf-8 "fa")))
  (signals (json-parse-error) (parse (utf-8 "f"))))

(test parses-integers
  (is (integerp (parse "42")))
  (is (= 42 (parse "42"))))

(test parses-decimals
  (is (typep (parse "42.0") 'double-float))
  (is (= 42.0d0 (parse "42.0"))))

(test parses-exponent
  (is (typep (parse "42e1") 'double-float))
  (is (= 420.0d0 (parse "42e1"))))

(test parses-decimals-with-exponent
  (is (= 42.0d0 (parse "0.42e2"))))

(test parse-negative-decimal
  (is (= -0.1d0 (parse "-0.1"))))

(test disallows-leading-zeros
  (signals (json-parse-error) (parse "01"))
  (signals (json-parse-error) (parse "01.0"))
  (signals (json-parse-error) (parse "01e10")))

(test disallows-trailing-decimal-point
  (signals (json-parse-error) (parse "1."))
  (signals (json-parse-error) (parse "1.e10")))

(test disallows-trailing-exponent-marker
  (signals (json-parse-error) (parse "1e"))
  (signals (json-parse-error) (parse "1.0e"))
  (signals (json-parse-error) (parse "0e")))

(test parses-zero
  (is (eql 0 (parse "0"))))

(test parses-negative-zero.0
  (is (= (bits-double-float #x8000000000000000) (parse "-0.0"))))

(test parses-negative-zero
  (is (= (bits-double-float #x8000000000000000) (parse "-0"))))

(test parse-1.31300000121E8
  (is (= (bits-double-float #x419F4DEA807BE76D) (parse "1.31300000121E8"))))

(test parse--1.31300000121E8
  (is (= (bits-double-float #xC19F4DEA807BE76D) (parse "-1.31300000121E8"))))

(test parse-23456789012E66
  (is (= (bits-double-float #x4FC9EE093A64B854) (parse "23456789012E66"))))

(test parse-0.000000000000000000000034567890120102012
  (is (= (bits-double-float #x3B44E51F35466432) (parse "0.000000000000000000000034567890120102012"))))

(test parse-97924.49742786969
  (is (= (bits-double-float #x40F7E847F576ED07) (parse "97924.49742786969"))))

(test parse-22057.311791265754
  (is (= (bits-double-float #x40D58A53F4635A66) (parse "22057.311791265754"))))

(test parse-5e-324
  (is (= (bits-double-float #x0000000000000001) (parse "5e-324"))))

(test parse-4.9E-324
  (is (= (bits-double-float #x0000000000000001) (parse "4.9E-324"))))

(test parse-4.8E-324
  (is (= (bits-double-float #x0000000000000001) (parse "4.8E-324"))))

(test parse-0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005
  (is (= (bits-double-float #x0000000000000001) (parse "0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005"))))

(test parse-g-clef
  (is (string= #.(string (code-char #x1D11E)) (parse "\"\\uD834\\uDD1E\""))))

(test parses-arrays
  (is (equalp #() (parse "[]")))
  (is (equalp #(1 2 3) (parse "[1, 2, 3]"))))

(test empty-array-inside-array
  (is (equalp #(#()) (parse "[[]]"))))

(test empty-object-inside-array
  (is (equalp (vector (ph)) (parse "[{}]"))))

(test parses-arrays-signals-eof
  (signals (json-eof-error) (parse "[1, 2, 3")))

(test parses-arrays-disallows-trailing-comma
  (signals (json-parse-error) (parse "[1, 2, 3,]")))

(test parses-arrays-allows-trailing-comma-when-asked
  (is (equalp #(1 2 3) (parse "[1, 2, 3,]" :allow-trailing-comma t))))

(test parses-arrays-allows-trailing-comma-on-vector-input
  (is (equalp #(1 2 3) (parse (utf-8 "[1,2,3,]") :allow-trailing-comma t))))

(test parses-arrays-disallows-several-trailing-commas
  (signals (json-parse-error) (parse "[1, 2, 3,,]"))
  (signals (json-parse-error) (parse "[1, 2, 3,,]" :allow-trailing-comma t)))

(test parses-arrays-disallows-empty-with-comma
  (signals (json-parse-error) (parse "[,]"))
  (signals (json-parse-error) (parse "[,]" :allow-trailing-comma t)))

(test parses-arrays-disallows-trailing-comma-with-eof
  (signals (json-eof-error) (parse "[1, 2, 3,"))
  (signals (json-eof-error) (parse "[1, 2, 3," :allow-trailing-comma t)))

(test parses-objects
  (is (equalp (ph) (parse "{}")))
  (is (equalp (ph "x" 1 "y" 2) (parse "{\"x\": 1, \"y\": 2}"))))

(test parses-objects-eof
  (signals (json-eof-error) (parse "{"))
  (signals (json-eof-error) (parse "{\"x\": 1, \"y\": 2")))

(test parses-objects-disallows-trailing-comma
  (signals (json-parse-error) (parse "{\"x\": 1, \"y\": 2,}")))

(test parses-objects-allows-trailing-comma-when-asked
  (is (equalp (ph "x" 1 "y" 2) (parse "{\"x\": 1, \"y\": 2,}" :allow-trailing-comma t))))

(test parses-objects-disallows-several-trailing-commas
  (signals (json-parse-error) (parse "{\"x\": 1, \"y\": 2,,}"))
  (signals (json-parse-error) (parse "{\"x\": 1, \"y\": 2,,}" :allow-trailing-comma t)))

(test parses-object-disallows-trailing-comma-with-eof
  (signals (json-eof-error) (parse "{\"x\": 1, \"y\": 2,"))
  (signals (json-eof-error) (parse "{\"x\": 1, \"y\": 2," :allow-trailing-comma t)))

(test parses-object-disallows-empty-with-comma
  (signals (json-parse-error) (parse "{,}"))
  (signals (json-parse-error) (parse "{,}" :allow-trailing-comma t)))

(test parse-singular
  (is (equalp (ph "foo" "bar")
              (parse "{\"foo\":\"bar\"}"))
      "Matching of a single simple string")
  (is (equalp (ph "bar" 1000)
              (parse "{\"bar\":1000}"))
      "Matching of a single number")
  (is (equalp (ph "bar" 10.1d0)
              (parse "{\"bar\":10.1}"))
      "Matching of a single decimal number")
  (is (equalp (ph "bar" #("foo" 10 101.1d0))
              (parse "{\"bar\":[\"foo\",10,101.10]}"))
      "Matching of an array with various types of elements"))

(test parse-multiple
  (is (equalp (ph "foo" "bar" "baz" "bang" "bing" 100 "bingo" 1.1d0 "bazo" #(1 2 "foo"))
              (parse "{\"foo\":\"bar\",\"baz\":\"bang\",\"bing\":100,\"bingo\":1.1,\"bazo\":[1,2,\"foo\"]}"))
      "Parsing of multiple items of all kinds"))

(test parse-nested
  (is (equalp (ph "foo" (ph "bar" "baz"))
              (parse "{\"foo\":{\"bar\":\"baz\"}}"))
      "One object in one object")
  (is (equalp (ph "foo" "bar" "bie" (ph "bar" "baz" "bang" 1000) "bing" "bingo")
              (parse "{\"foo\":\"bar\",\"bie\":{\"bar\":\"baz\",\"bang\":1000},\"bing\":\"bingo\"}")))
  (is (equalp (ph "foo" (vector (ph "foo" "bar" "baz" 1000)))
              (parse "{\"foo\":[{\"foo\":\"bar\",\"baz\":1000}]}"))
      "Object in an array")
  (is (equalp (ph  "foo" "bar" "baz" (ph "boo" 100.1d0))
              (parse "{\"foo\":\"bar\",\"baz\":{\"boo\":100.10}}"))
      "Decimal number in inner object"))

(test unicode-chars
  (is (equalp (ph "λlambda" "💩poop")
              (parse "{\"\\u03BBlambda\":\"\\ud83d\\udca9poop\"}")))

  (is (equalp (ph "lambdaλ" "poop💩")
              (parse "{\"lambda\\u03BB\":\"poop\\ud83d\\udca9\"}")))

  (is (equalp (ph "lambdaλlambda" "poop💩poop")
              (parse "{\"lambda\\u03BBlambda\":\"poop\\ud83d\\udca9poop\"}"))))

(test signals-eof-in-unicode-escape
  (signals json-eof-error (parse "\"\\ud83d\\udca"))
  (signals json-eof-error (parse "\"\\ud83d\\udc"))
  (signals json-eof-error (parse "\"\\ud83d\\ud"))
  (signals json-eof-error (parse "\"\\ud83d\\u"))
  (signals json-eof-error (parse "\"\\ud83d\\"))
  (signals json-eof-error (parse "\"\\ud83d"))
  (signals json-eof-error (parse "\"\\ud83"))
  (signals json-eof-error (parse "\"\\ud8"))
  (signals json-eof-error (parse "\"\\ud"))
  (signals json-eof-error (parse "\"\\u"))
  (signals json-eof-error (parse "\"\\")))

(test parse-pools-keys
  (let* ((objects (parse "[{\"x\": 5}, {\"x\": 10}, {\"x\": 15}]"))
         (keys (map 'list #'hash-table-keys objects))
         (xs (mapcar #'first keys))
         (x1 (first xs))
         (x2 (second xs))
         (x3 (third xs)))
    (is (eq x1 x2))
    (is (eq x2 x3))))

(test parse-pools-keys-on-t
  (let* ((objects (parse "[{\"x\": 5}, {\"x\": 10}, {\"x\": 15}]" :key-fn t))
         (keys (map 'list #'hash-table-keys objects))
         (xs (mapcar #'first keys))
         (x1 (first xs))
         (x2 (second xs))
         (x3 (third xs)))
    (is (eq x1 x2))
    (is (eq x2 x3))))

(test parse-no-pools-keys-on-nil
  (let* ((objects (parse "[{\"x\": 5}, {\"x\": 10}, {\"x\": 15}]" :key-fn nil))
         (keys (map 'list #'hash-table-keys objects))
         (xs (mapcar #'first keys))
         (x1 (first xs))
         (x2 (second xs))
         (x3 (third xs)))
    (is (not (eq x1 x2)))
    (is (not (eq x2 x3)))))

(test parse-uses-custom-key-fn
  (let ((keys ()))
    (parse "[{\"x\": 5}, {\"x\": 10}, {\"x\": 15}]" :key-fn (lambda (key) (push key keys) key))
    (is (equalp '("x" "x" "x") keys))))

(test parse-ignores-pre-post-whitespace
  (is-every equalp
    (nil               (parse "  false "))
    (t                 (parse " true  "))
    ('null             (parse "   null "))
    (42                (parse "      42 "))
    (42.0d0            (parse "  42e0  "))
    ("Hello, world!"   (parse "   \"Hello, world!\"  "))
    (#(1 2 3)          (parse " [1,2,3]  "))
    ((ph "x" 10 "y" 0) (parse "   { \"x\": 10, \"y\": 0}   "))))

(test parse-accepts-stream
  (is-every equalp
    (nil               (parse (make-string-input-stream "false")))
    (t                 (parse (make-string-input-stream "true")))
    ('null             (parse (make-string-input-stream "null")))
    (42                (parse (make-string-input-stream "42")))
    (42.0d0            (parse (make-string-input-stream "42e0")))
    ("Hello, world!"   (parse (make-string-input-stream "\"Hello, world!\"")))
    (#(1 2 3)          (parse (make-string-input-stream "[1,2,3]")))
    ((ph "x" 10 "y" 0) (parse (make-string-input-stream "{ \"x\": 10, \"y\": 0}")))))

(test parse-accepts-pathname
  (flet ((parse (str)
           (uiop:with-temporary-file (:stream stream :pathname p :external-format :utf-8)
             (write-string str stream)
             (finish-output stream)
             (close stream)
             (parse p))))
    (is-every equalp
      (nil               (parse "false"))
      (t                 (parse "true"))
      ('null             (parse "null"))
      (42                (parse "42"))
      (42.0d0            (parse "42e0"))
      ("Hello, world!"   (parse "\"Hello, world!\""))
      (#(1 2 3)          (parse "[1,2,3]"))
      ((ph "x" 10 "y" 0) (parse "{ \"x\": 10, \"y\": 0}")))))

(test parse-accepts-non-simple-string
  (flet ((parse (str)
           (parse (make-array (length str) :element-type 'character :fill-pointer t :initial-contents str))))
    (is-every equalp
      (nil               (parse "false"))
      (t                 (parse "true"))
      ('null             (parse "null"))
      (42                (parse "42"))
      (42.0d0            (parse "42e0"))
      ("Hello, world!"   (parse "\"Hello, world!\""))
      (#(1 2 3)          (parse "[1,2,3]"))
      ((ph "x" 10 "y" 0) (parse "{ \"x\": 10, \"y\": 0}")))))

(test (parse-returns-base-strings :depends-on
                                  ;; Skip test on ECL - producing incorrectly upgraded array type https://github.com/Zulu-Inuoe/jzon/issues/59
                                  #+ecl (or)
                                  #-ecl (and))
  (is (eq '#.(upgraded-array-element-type 'base-char) (array-element-type (parse "\"COMMON-LISP\""))))
  (is (eq '#.(upgraded-array-element-type 'base-char) (array-element-type (parse "\"\\u0043\\u004F\\u004D\\u004D\\u004F\\u004E\\u002D\\u004C\\u0049\\u0053\\u0050\"")))))

(test parse-accepts-octet-vector
  (is-every equalp
    (nil               (parse (utf-8 "false")))
    (t                 (parse (utf-8 "true")))
    ('null             (parse (utf-8 "null")))
    (42                (parse (utf-8 "42")))
    (42.0d0            (parse (utf-8 "42e0")))
    ("Hello, world!"   (parse (utf-8 "\"Hello, world!\"")))
    (#(1 2 3)          (parse (utf-8 "[1,2,3]")))
    ((ph "x" 10 "y" 0) (parse (utf-8 "{ \"x\": 10, \"y\": 0}")))))

(test octet-vector-decodes-utf-8
  (is (equalp #("λlambda" "💩poop") (parse (utf-8 "[\"λlambda\",  \"💩poop\"]")))))

(test parse-accepts-binary-stream
  (is-every equalp
    (nil               (parse (flexi-streams:make-in-memory-input-stream (utf-8 "false"))))
    (t                 (parse (flexi-streams:make-in-memory-input-stream (utf-8 "true"))))
    ('null             (parse (flexi-streams:make-in-memory-input-stream (utf-8 "null"))))
    (42                (parse (flexi-streams:make-in-memory-input-stream (utf-8 "42"))))
    (42.0d0            (parse (flexi-streams:make-in-memory-input-stream (utf-8 "42e0"))))
    ("Hello, world!"   (parse (flexi-streams:make-in-memory-input-stream (utf-8 "\"Hello, world!\""))))
    (#(1 2 3)          (parse (flexi-streams:make-in-memory-input-stream (utf-8 "[1,2,3]"))))
    ((ph "x" 10 "y" 0) (parse (flexi-streams:make-in-memory-input-stream (utf-8 "{ \"x\": 10, \"y\": 0}"))))))

(test parse-accepts-simple-string-span
  (is (= 42 (parse (span "garbage42" :start 7))))
  (is (= 42 (parse (span "42moregarbage" :end 2))))
  (is (= 42 (parse (span "garbage42moregarbage" :start 7 :end 9))))
  (is (string= "hello" (parse (span "garbage\"hello\"moregarbage" :start 7 :end 14))))
  (is (string= "Poop:💩" (parse (span "garbage\"Poop:\\uD83D\\uDCA9\"moregarbage" :start 7 :end 26)))))

(test parse-accepts-string-span
  (is (= 42 (parse (span (not-simple "garbage42") :start 7))))
  (is (= 42 (parse (span (not-simple "42moregarbage") :end 2))))
  (is (= 42 (parse (span (not-simple "garbage42moregarbage") :start 7 :end 9))))
  (is (string= "hello" (parse (span (not-simple "garbage\"hello\"moregarbage") :start 7 :end 14))))
  (is (string= "Poop:💩" (parse (span (not-simple "garbage\"Poop:\\uD83D\\uDCA9\"moregarbage") :start 7 :end 26)))))

(test parse-accepts-simple-octet-vector-span
  (is (= 42 (parse (span (flexi-streams:string-to-octets "garbage42" :external-format :utf-8) :start 7))))
  (is (= 42 (parse (span (flexi-streams:string-to-octets "42moregarbage" :external-format :utf-8) :end 2))))
  (is (= 42 (parse (span (flexi-streams:string-to-octets "garbage42moregarbage" :external-format :utf-8) :start 7 :end 9)))))

(test parse-accepts-octet-vector-span
  (is (= 42 (parse (span (not-simple (flexi-streams:string-to-octets "garbage42" :external-format :utf-8)) :start 7))))
  (is (= 42 (parse (span (not-simple (flexi-streams:string-to-octets "42moregarbage" :external-format :utf-8)) :end 2))))
  (is (= 42 (parse (span (not-simple (flexi-streams:string-to-octets "garbage42moregarbage" :external-format :utf-8)) :start 7 :end 9)))))

(test spans-error-on-bad-ranges
  (signals (error) (span "hello" :start 50))
  (signals (type-error) (span "hello" :start nil))
  (signals (error) (span "hello" :end 50))
  (finishes (span "hello" :end nil))

  (signals (error) (span (utf-8 "hello") :start 50))
  (signals (type-error) (span (utf-8 "hello") :start nil))
  (signals (error) (span (utf-8 "hello") :end 50))
  (finishes (span (utf-8 "hello") :end nil)))

(test spans-error-on-bad-ranges-floats
  (signals (type-error) (span "hello" :start 1.0))
  (signals (type-error) (span "hello" :end 2.0))
  (signals (type-error) (span "hello" :start 1.0 :end 2.0))

  (signals (type-error) (span (utf-8 "hello") :start 1.0))
  (signals (type-error) (span (utf-8 "hello") :end 2.0))
  (signals (type-error) (span (utf-8 "hello") :start 1.0 :end 2.0)))

(test parse-allows-strings-below-max-string-length
  (finishes (parse "\"This is a string that is not long\"" :max-string-length 45)))

(test parse-allows-strings-at-max-string-length
  (finishes (parse "\"This is a string that is exactly not too long\"" :max-string-length 45)))

(test parse-limits-max-string-length
  (signals (json-parse-limit-error) (parse "\"This is a string that is too long\"" :max-string-length 5)))

(test parse-limits-max-string-length-on-vector-inputs
  (signals (json-parse-limit-error) (parse (utf-8 "\"This is a string that is too long\"") :max-string-length 5)))

(test parse-limits-max-string-length-with-escape-codes
  (signals (json-parse-limit-error) (parse "\"This is a string that is too long\bwith some special codes \\u00f8\"" :max-string-length 5)))

(test parse-reports-correct-position-when-encountering-control-char-in-string
  (handler-case (parse (concatenate 'string "\"null:" (string (code-char 0)) "\""))
    (json-parse-error (e) (is (= 7 (com.inuoe.jzon::%json-parse-error-column e))))))

(test parse-reports-correct-position-when-encountering-eof-in-string
  (handler-case (parse "\"null:")
    (json-eof-error (e)
      (is (= 6 (com.inuoe.jzon::%json-parse-error-column e))))))

(test parse-reports-correct-position-when-encountering-max-string-length-in-string
  (handler-case (parse "\"null:" :max-string-length 2)
    (json-parse-error (e)
      (is (= 4 (com.inuoe.jzon::%json-parse-error-column e))))))

;; TODO - pull this hardcode into a constant we can expose from jzon
(test parse-max-string-length-accepts-nil-for-no-limit
  (let ((big-chungus (make-array #x500000 :element-type 'character :initial-element #\space)))
    (setf (aref big-chungus 0) #.(char "\"" 0))
    (setf (aref big-chungus (1- (length big-chungus))) #.(char "\"" 0))
    (is (= (- #x500000 2) (count #\Space (parse big-chungus :max-string-length nil))))))

(test parse-max-string-length-accepts-nil-for-default-limit
  (let ((big-chungus (make-array #x500000 :element-type 'character :initial-element #\space)))
    (setf (aref big-chungus 0) #.(char "\"" 0))
    (setf (aref big-chungus (1- (length big-chungus))) #.(char "\"" 0))
    (signals (json-parse-limit-error) (parse big-chungus :max-string-length t))))

(test parse-max-string-length-accepts-array-dimesion-limit-1
  (let ((big-chungus (make-array #x500000 :element-type 'character :initial-element #\space)))
    (setf (aref big-chungus 0) #.(char "\"" 0))
    (setf (aref big-chungus (1- (length big-chungus))) #.(char "\"" 0))
    (is (= (- #x500000 2) (count #\Space (parse big-chungus :max-string-length (1- array-dimension-limit)))))))

(test parse-max-string-length-respects-escape-codes-1
  (finishes (parse "\"\\u00f8\"" :max-string-length 1)))

(test parse-max-string-length-respects-escape-codes-2
  (finishes (parse "\"\\n\"" :max-string-length 1)))

(test parse-errors-on-too-large-string-length
  (signals (type-error) (parse "\"Doesn't matter\"" :max-string-length (* array-dimension-limit 2))))

(test parse-disallows-comments
  (signals (json-parse-error) (parse "//Line comment
    123")))

(test parse-allows-comments-when-asked
  (is (= 123 (parse "//Line comment
  123" :allow-comments t))))

(test parse-line-comments-do-not-end-on-cr-only-lf
  (is (= 123 (parse (format nil "//Comment~C123~C 123" #\Return #\Linefeed) :allow-comments t)))
  (signals (json-eof-error) (parse (format nil "//Comment~C123 123" #\Return) :allow-comments t)))

(test parse-comments-delimit-atoms
  (is (= 123 (parse "123//Line comment" :allow-comments t)))
  (is (eq t (parse "true//Line comment" :allow-comments t)))
  (is (eq nil (parse "false//Line comment" :allow-comments t)))
  (is (eq 'null (parse "null//Line comment" :allow-comments t)))
  (is (string= "123" (parse "\"123\"//Line comment" :allow-comments t))))

(test parse-disallows-block-comments
  (signals (json-parse-error) (parse "/*Block comment*/ 123")))

(test parse-allows-block-comments-when-asked
  (is (= 123 (parse "/*Block comment*/ 123" :allow-comments t))))

(test parse-does-not-nest-block-comments
  (signals (json-parse-error) (parse "/*Block comment /*Nested Block Comment */ */ 123" :allow-comments t)))

(test unterminated-block-comment-errors
  (signals (json-eof-error) (parse "/* Some stuff" :allow-comments t))
  (signals (json-eof-error) (parse "/* Some stuff ** // " :allow-comments t)))

(test miscellaneous-block-comment-tests
  (is (= 123 (parse "123/*comment*/" :allow-comments t)))
  (is (eq t (parse "true/*comment*/" :allow-comments t)))
  (is (eq nil (parse "false/*comment*/" :allow-comments t)))
  (is (eq 'null (parse "null/*comment*/" :allow-comments t)))
  (is (string= "123" (parse "/*comment before */\"123\"/*comment*/" :allow-comments t)))
  (is (= 123 (parse "/*comment before */123/*comment*/" :allow-comments t)))
  (is (eq t (parse "/*comment before */true/*comment*/" :allow-comments t)))
  (is (eq nil (parse "/*comment before */false/*comment*/" :allow-comments t)))
  (is (eq 'null (parse "/*comment before */null/*comment*/" :allow-comments t)))
  (is (string= "123" (parse "/*comment before */\"123\"/*comment*/" :allow-comments t)))
  (is (= 123 (parse "/*comment before //line comment ignored inside block */123/*comment*/" :allow-comments t))))

(test parse-max-depth-disabled-when-nil
  (is (vectorp (parse (concatenate 'string (make-string 130 :initial-element #\[) (make-string 130 :initial-element #\])) :max-depth nil))))

(test parse-max-depth-defaults-when-t
  (signals (json-parse-limit-error) (parse (make-string 130 :initial-element #\[) :max-depth t)))

(test parse-errors-on-multiple-content
  (signals (json-parse-error) (parse "1 2")))

(test parse-no-error-on-multiple-content-when-asked
  (is (= 1 (parse "1 2" :allow-multiple-content t))))

(test parse-doesnt-overread-on-multiple-content-null
  (with-input-from-string (s "null  ")
    (parse s :allow-multiple-content t)
    (is (= 5 (file-position s)))))

(test parse-doesnt-overread-on-multiple-content-false
  (with-input-from-string (s "false  ")
    (parse s :allow-multiple-content t)
    (is (= 6 (file-position s)))))

(test parse-doesnt-overread-on-multiple-content-true
  (with-input-from-string (s "true  ")
    (parse s :allow-multiple-content t)
    (is (= 5 (file-position s)))))

(test parse-doesnt-overread-on-multiple-content-1234
  (with-input-from-string (s "1234  ")
    (parse s :allow-multiple-content t)
    (is (= 5 (file-position s)))))

(test parse-doesnt-overread-on-multiple-content-string
  (with-input-from-string (s "\"hello\"  ")
    (parse s :allow-multiple-content t)
    (is (= 7 (file-position s)))))

(test parse-doesnt-overread-on-multiple-content-array
  (with-input-from-string (s "[\"hello\"]  ")
    (parse s :allow-multiple-content t)
    (is (= 9 (file-position s))))
  (with-input-from-string (s "[1,2]  ")
    (parse s :allow-multiple-content t)
    (is (= 5 (file-position s)))))

(test parse-doesnt-overread-on-multiple-content-object
  (with-input-from-string (s "{\"x\":2}  ")
    (parse s :allow-multiple-content t)
    (is (= 7 (file-position s)))))

(test parse-needs-whitespace-for-bare-tokens-nullnull
  (signals (json-parse-error) (parse "nullnull" :allow-multiple-content t)))

(test parse-needs-whitespace-for-bare-tokens-nulllbrace
  (signals (json-parse-error) (parse "null[" :allow-multiple-content t))
  (signals (json-parse-error) (parse "null{" :allow-multiple-content t)))

(test parse-needs-whitespace-for-bare-tokens-nullrbrace
  (signals (json-parse-error) (parse "null]" :allow-multiple-content t))
  (signals (json-parse-error) (parse "null}" :allow-multiple-content t)))

(test parse-needs-whitespace-for-bare-tokens-falsequote
  (signals (json-parse-error) (parse "false\"" :allow-multiple-content t)))

(test parse-needs-whitespace-for-bare-tokens-falsefalse
  (signals (json-parse-error) (parse "falsefalse" :allow-multiple-content t)))

(test parse-needs-whitespace-for-bare-tokens-falselbrace
  (signals (json-parse-error) (parse "false[" :allow-multiple-content t))
  (signals (json-parse-error) (parse "false{" :allow-multiple-content t)))

(test parse-needs-whitespace-for-bare-tokens-falserbrace
  (signals (json-parse-error) (parse "false]" :allow-multiple-content t))
  (signals (json-parse-error) (parse "false}" :allow-multiple-content t)))

(test parse-needs-whitespace-for-bare-tokens-falsequote
  (signals (json-parse-error) (parse "false\"" :allow-multiple-content t)))

(test parse-needs-whitespace-for-bare-tokens-truetrue
  (signals (json-parse-error) (parse "truetrue" :allow-multiple-content t)))

(test parse-needs-whitespace-for-bare-tokens-truelbrace
  (signals (json-parse-error) (parse "true[" :allow-multiple-content t))
  (signals (json-parse-error) (parse "true{" :allow-multiple-content t)))

(test parse-needs-whitespace-for-bare-tokens-truerbrace
  (signals (json-parse-error) (parse "true]" :allow-multiple-content t))
  (signals (json-parse-error) (parse "true}" :allow-multiple-content t)))

(test parse-needs-whitespace-for-bare-tokens-truequote
  (signals (json-parse-error) (parse "true\"" :allow-multiple-content t)))

(test parse-needs-whitespace-for-bare-tokens-1234null
  (signals (json-parse-error) (parse "1234null" :allow-multiple-content t)))

(test parse-needs-whitespace-for-bare-tokens-1234lbrace
  (signals (json-parse-error) (parse "1234[" :allow-multiple-content t))
  (signals (json-parse-error) (parse "1234{" :allow-multiple-content t)))

(test parse-needs-whitespace-for-bare-tokens-1234rbrace
  (signals (json-parse-error) (parse "1234]" :allow-multiple-content t))
  (signals (json-parse-error) (parse "1234}" :allow-multiple-content t)))

(test parse-needs-whitespace-for-bare-tokens-1234quote
  (signals (json-parse-error) (parse "1234\"" :allow-multiple-content t)))

(def-suite incremental :in parsing)
(in-suite incremental)

(test parse-next-basics
  (with-parser (parser "{\"x\": 42, \"y\": [1, 2, 3], \"z\": [true, false, null]}")
    (is (eq :begin-object (parse-next parser)))
    (is (eq :object-key (parse-next parser)))
    (is (eq :value (parse-next parser)))
    (is (eq :object-key (parse-next parser)))
    (is (eq :begin-array (parse-next parser)))
    (is (eq :value (parse-next parser)))
    (is (eq :value (parse-next parser)))
    (is (eq :value (parse-next parser)))
    (is (eq :end-array (parse-next parser)))
    (is (eq :object-key (parse-next parser)))
    (is (eq :begin-array (parse-next parser)))
    (is (eq :value (parse-next parser)))
    (is (eq :value (parse-next parser)))
    (is (eq :value (parse-next parser)))
    (is (eq :end-array (parse-next parser)))
    (is (eq :end-object (parse-next parser)))))

(test parse-next-errors-after-toplevel
  (with-parser (parser "42 24")
    (is (eq :value (parse-next parser)))
    (signals (json-parse-error) (parse-next parser))))

(test parse-next-after-toplevel-continues-failing
  (with-parser (parser "{} {")
    (is (eq :begin-object (parse-next parser)))
    (is (eq :end-object (parse-next parser)))
    (signals (json-parse-error) (parse-next parser))
    (signals (json-parse-error) (parse-next parser))))

(test parse-next-allows-multiple-content-when-asked
  (with-parser (parser "42 24" :allow-multiple-content t)
    (multiple-value-bind (event value) (parse-next parser)
      (is (eq :value event))
      (is (= value 42)))
    (multiple-value-bind (event value) (parse-next parser)
      (is (eq :value event))
      (is (= value 24)))
    (is (null (parse-next parser)))))

(test parse-next-element-basics
  (with-parser (p "[1,2,3]")
    (is (eq :begin-array (parse-next p)))
    (is (= 1 (parse-next-element p)))
    (is (= 2 (parse-next-element p)))
    (is (= 3 (parse-next-element p)))
    (is (null (parse-next-element p :eof-error-p  nil)))
    (is (eq nil (parse-next p)))))

(test parse-next-element-nested-array-in-array
  (with-parser (p "[[1,2,3]]")
    (is (eq :begin-array (parse-next p)))
    (is (equalp #(1 2 3) (parse-next-element p)))
    (is (eq :end-array (parse-next p)))))

(test parse-next-element-nested-object-in-array
  (with-parser (p "[{\"x\":42}]")
    (is (eq :begin-array (parse-next p)))
    (is (equalp (ph "x" 42) (parse-next-element p)))
    (is (eq :end-array (parse-next p)))))

(test parse-next-element-nested-array-in-object
  (with-parser (p "{\"foo\":[1,2,3]}")
    (is (eq :begin-object (parse-next p)))
    (is (eq :object-key (parse-next p)))
    (is (equalp #(1 2 3) (parse-next-element p)))
    (is (eq :end-object (parse-next p)))))

(test parse-next-element-nested-object-in-object
  (with-parser (p "{\"foo\":{\"x\":42}}")
    (is (eq :begin-object (parse-next p)))
    (is (eq :object-key (parse-next p)))
    (is (equalp (ph "x" 42) (parse-next-element p)))
    (is (eq :end-object (parse-next p)))))

(test parse-next-element-errors-on-bad-position-begin-object
  (with-parser (p "{\"x\":0}")
    (is (eq :begin-object (parse-next p)))
    (signals (error) (parse-next-element p))))

(test parse-next-element-errors-on-bad-position-after-property
  (with-parser (p "{\"x\":0,\"y\":1}")
    (is (eq :begin-object (parse-next p)))
    (is (eq :object-key (parse-next p)))
    (is (eq :value (parse-next p)))
    (signals (error) (parse-next-element p))))

(test parse-next-element-errors-on-bad-position-after-toplevel
  (with-parser (p "{\"x\":0,\"y\":1}")
    (is (eq :begin-object (parse-next p)))
    (is (eq :object-key (parse-next p)))
    (is (eq :value (parse-next p)))
    (is (eq :object-key (parse-next p)))
    (is (eq :value (parse-next p)))
    (is (eq :end-object (parse-next p)))
    (signals (error) (parse-next-element p))))

(test parse-next-element-allows-after-toplevel-when-multiple-content
  (with-parser (p "{\"x\":0,\"y\":1}" :allow-multiple-content t)
    (is (eq :begin-object (parse-next p)))
    (is (eq :object-key (parse-next p)))
    (is (eq :value (parse-next p)))
    (is (eq :object-key (parse-next p)))
    (is (eq :value (parse-next p)))
    (is (eq :end-object (parse-next p)))
    (is (null (parse-next-element p :eof-error-p nil)))))

(test parse-next-element-uses-max-depth-array
  (with-parser (p "{ \"foo\": [1, [2], 3] }")
    (is (eq :begin-object (parse-next p)))
    (is (eq :object-key (parse-next p)))
    (signals (json-parse-limit-error) (parse-next-element p :max-depth 1))))

(test parse-next-element-uses-max-depth-object
  (with-parser (p "{ \"foo\": [1, {\"x\": 2}, 3] }")
    (is (eq :begin-object (parse-next p)))
    (is (eq :object-key (parse-next p)))
    (signals (json-parse-limit-error) (parse-next-element p :max-depth 1))))

(test multi-close-ok
  (with-parser (parser "{}")
    (close-parser parser)
    (close-parser parser)))

(test parse-next-after-complete-returns-nil
  (with-parser (parser "42")
    (is (eq :value (parse-next parser)))
    (is (eq nil (parse-next parser)))
    (is (eq nil (parse-next parser)))
    (is (eq nil (parse-next parser)))
    (is (eq nil (parse-next parser)))
    (is (eq nil (parse-next parser)))
    (is (eq nil (parse-next parser)))))

(test parse-next-after-close-errors
  (with-parser (parser "{}")
    (close-parser parser)
    (signals (json-error) (parse-next parser))))

(test parse-next-pools-keys
  (with-parser (parser "[{\"x\": 5}, {\"x\": 10}, {\"x\": 15}]")
    (let (x1 x2 x3)
      (is (eq :begin-array (parse-next parser)))
      (is (eq :begin-object (parse-next parser)))
      (multiple-value-bind (event value) (parse-next parser)
        (is (eq event :object-key))
        (setf x1 value))
      (is (eq :value (parse-next parser)))
      (is (eq :end-object (parse-next parser)))
      (is (eq :begin-object (parse-next parser)))
      (multiple-value-bind (event value) (parse-next parser)
        (is (eq event :object-key))
        (setf x2 value))
      (is (eq :value (parse-next parser)))
      (is (eq :end-object (parse-next parser)))
      (is (eq :begin-object (parse-next parser)))
      (multiple-value-bind (event value) (parse-next parser)
        (is (eq event :object-key))
        (setf x3 value))
      (is (eq :value (parse-next parser)))
      (is (eq :end-object (parse-next parser)))
      (is (eq :end-array (parse-next parser)))
      (is (null (parse-next parser)))
      (is (eq x1 x2))
      (is (eq x2 x3)))))

(test parse-next-pools-keys-on-t
  (with-parser (parser "[{\"x\": 5}, {\"x\": 10}, {\"x\": 15}]" :key-fn t)
    (let (x1 x2 x3)
      (is (eq :begin-array (parse-next parser)))
      (is (eq :begin-object (parse-next parser)))
      (multiple-value-bind (event value) (parse-next parser)
        (is (eq event :object-key))
        (setf x1 value))
      (is (eq :value (parse-next parser)))
      (is (eq :end-object (parse-next parser)))
      (is (eq :begin-object (parse-next parser)))
      (multiple-value-bind (event value) (parse-next parser)
        (is (eq event :object-key))
        (setf x2 value))
      (is (eq :value (parse-next parser)))
      (is (eq :end-object (parse-next parser)))
      (is (eq :begin-object (parse-next parser)))
      (multiple-value-bind (event value) (parse-next parser)
        (is (eq event :object-key))
        (setf x3 value))
      (is (eq :value (parse-next parser)))
      (is (eq :end-object (parse-next parser)))
      (is (eq :end-array (parse-next parser)))
      (is (null (parse-next parser)))
      (is (eq x1 x2))
      (is (eq x2 x3)))))

(test parse-next-no-pools-keys-on-nil
  (with-parser (parser "[{\"x\": 5}, {\"x\": 10}, {\"x\": 15}]" :key-fn nil)
    (let (x1 x2 x3)
      (is (eq :begin-array (parse-next parser)))
      (is (eq :begin-object (parse-next parser)))
      (multiple-value-bind (event value) (parse-next parser)
        (is (eq event :object-key))
        (setf x1 value))
      (is (eq :value (parse-next parser)))
      (is (eq :end-object (parse-next parser)))
      (is (eq :begin-object (parse-next parser)))
      (multiple-value-bind (event value) (parse-next parser)
        (is (eq event :object-key))
        (setf x2 value))
      (is (eq :value (parse-next parser)))
      (is (eq :end-object (parse-next parser)))
      (is (eq :begin-object (parse-next parser)))
      (multiple-value-bind (event value) (parse-next parser)
        (is (eq event :object-key))
        (setf x3 value))
      (is (eq :value (parse-next parser)))
      (is (eq :end-object (parse-next parser)))
      (is (eq :end-array (parse-next parser)))
      (is (null (parse-next parser)))
      (is (not (eq x1 x2)))
      (is (not (eq x2 x3))))))

(test parse-next-need-whitespace-for-bare-tokens-nullnull
  (with-parser (p "nullnull" :allow-multiple-content t)
    (signals (json-parse-error) (parse-next p))))

(test parse-next-no-need-whitespace-for-bare-tokens-nulllbrace
  (with-parser (p "null[" :allow-multiple-content t)
    (is (eq :value (parse-next p))))
  (with-parser (p "null{" :allow-multiple-content t)
    (is (eq :value (parse-next p)))))

(test parse-next-no-need-whitespace-for-bare-tokens-nullquote
  (with-parser (p "null\"" :allow-multiple-content t)
    (is (eq :value (parse-next p)))))

(test parse-next-need-whitespace-for-bare-tokens-falsenull
  (with-parser (p "falsenull" :allow-multiple-content t)
    (signals (json-parse-error) (parse-next p))))

(test parse-next-no-need-whitespace-for-bare-tokens-falselbrace
  (with-parser (p "false[" :allow-multiple-content t)
    (is (eq :value (parse-next p))))
  (with-parser (p "false{" :allow-multiple-content t)
    (is (eq :value (parse-next p)))))

(test parse-next-no-need-whitespace-for-bare-tokens-falsequote
  (with-parser (p "false\"" :allow-multiple-content t)
    (is (eq :value (parse-next p)))))

(test parse-next-need-whitespace-for-bare-tokens-truenull
  (with-parser (p "truenull" :allow-multiple-content t)
    (signals (json-parse-error) (parse-next p))))

(test parse-next-no-need-whitespace-for-bare-tokens-truelbrace
  (with-parser (p "true[" :allow-multiple-content t)
    (is (eq :value (parse-next p))))
  (with-parser (p "true{" :allow-multiple-content t)
    (is (eq :value (parse-next p)))))

(test parse-next-no-need-whitespace-for-bare-tokens-truequote
  (with-parser (p "true\"" :allow-multiple-content t)
    (is (eq :value (parse-next p)))))

(test parse-next-need-whitespace-for-bare-tokens-1234null
  (with-parser (p "1234null" :allow-multiple-content t)
    (signals (json-parse-error) (parse-next p))))

(test parse-next-no-need-whitespace-for-bare-tokens-1234lbrace
  (with-parser (p "1234[" :allow-multiple-content t)
    (is (eq :value (parse-next p))))
  (with-parser (p "1234{" :allow-multiple-content t)
    (is (eq :value (parse-next p)))))

(test parse-next-no-need-whitespace-for-bare-tokens-1234quote
  (with-parser (p "1234\"" :allow-multiple-content t)
    (is (eq :value (parse-next p)))))

(test parse-next-return-2-values-on-object-key
  (with-parser (p "{\"x\":42}")
    (parse-next p)
    (is (equalp '(:object-key "x") (multiple-value-list (parse-next p))))))

(def-suite writer :in jzon)
(in-suite writer)

(defmacro with-writer-to-string ((writer &key pretty max-depth coerce-key) &body body)
  (let ((str-sym (gensym "STR")))
    `(with-output-to-string (,str-sym)
       (with-writer (,writer :stream ,str-sym :pretty ,pretty :max-depth ,max-depth :coerce-key ,coerce-key)
         ,@body))))

(test writer-write-values-works
  (is (string= "[1,2,3]" (with-writer-to-string (writer)
                           (with-array writer
                             (write-values writer 1 2 3))))))

(test writer-max-depth-works
  (signals (json-write-limit-error)
    (with-writer-to-string (writer :max-depth 1)
      (with-array writer
        (with-array writer))))
  (finishes
    (with-writer-to-string (writer :max-depth 1)
      (with-array writer
        (write-value writer 42)))))

(test writer-disallows-more-than-one-toplevel-value
  (with-writer (writer)
    (write-value writer 42)
    (signals (json-write-error) (write-value writer 42)))
  (with-writer (writer)
    (with-object writer)
    (signals (json-write-error) (write-value writer 42)))
  (with-writer (writer)
    (write-value writer 42)
    (signals (json-write-error) (with-object writer)))
  (with-writer (writer)
    (with-array writer)
    (signals (json-write-error) (write-value writer 42)))
  (with-writer (writer)
    (write-value writer 42)
    (signals (json-write-error) (with-array writer)))
  (with-writer (writer)
    (with-array writer)
    (signals (json-write-error) (with-array writer)))
  (with-writer (writer)
    (with-array writer)
    (signals (json-write-error) (with-object writer)))
  (with-writer (writer)
    (with-object writer)
    (signals (json-write-error) (with-array writer)))
  (with-writer (writer)
    (with-object writer)
    (signals (json-write-error) (with-object writer))))

(test write-properties-returns-writer
  (with-writer (writer)
    (with-object writer
      (is (eq writer (write-properties writer 0 0))))))

(test write-array-works
  (is (string= "[1,2,3]"
       (with-writer-to-string (writer)
         (write-array writer 1 2 3)))))

(test writer-errors-after-closed
  (let ((writer (make-writer)))
    (close-writer writer)
    (signals (json-write-error) (write-value writer 42)))
  (let ((writer (make-writer)))
    (close-writer writer)
    (signals (json-write-error) (begin-array writer)))
  (let ((writer (make-writer)))
    (close-writer writer)
    (signals (json-write-error) (begin-object writer))))

(test write-*-functions-use-bound-writer
  (is (string= "42" (with-writer-to-string (*writer*) (write-value* 42))))
  (is (string= "[42]" (with-writer-to-string (*writer*) (write-array* 42))))
  (is (string= "{\"24\":42}" (with-writer-to-string (*writer*) (write-object* 24 42))))
  (is (string= "[42]" (with-writer-to-string (*writer*) (with-array* (write-value* 42)))))
  (is (string= "{\"24\":42}"
               (with-writer-to-string (*writer*)
                 (with-object*
                   (write-key* "24")
                   (write-value* 42)))))
  (is (string= "{\"24\":42}"
               (with-writer-to-string (*writer*)
                 (with-object*
                   (write-property* "24" 42)))))
  (is (string= "{\"24\":42,\"null\":null}"
               (with-writer-to-string (*writer*)
                 (with-object*
                   (write-properties* "24" 42
                                           "null" 'null))))))

(test with-writer-*-binds-writer
  (is (string= "\"hello\"" (with-output-to-string (stream)
                             (with-writer* (:stream stream)
                               (write-value* "hello"))))))

(test writer-coerce-key-symbol-is-not-coerced-to-fn-eagerly
  (is (string= "{\"HELLO\":42}"
               (let ((key-fn (make-symbol (string '#:test-coerce-key))))
                 (with-writer-to-string (*writer* :coerce-key key-fn)
                   (setf (symbol-function key-fn) #'string-upcase)
                   (write-object* "hello" 42))))))

(test writer-defaults-nil-coerce-key
  (is (string= "{\"hello\":42}" (with-writer-to-string (*writer* :coerce-key nil)
                                  (write-object* "hello" 42)))))

(test writer-max-depth-disabled-when-nil
  (is (string= (concatenate 'string (make-string 130 :initial-element #\[) (make-string 130 :initial-element #\]))
               (with-output-to-string (s)
                 (with-writer* (:stream s :max-depth nil)
                   (loop :repeat 130 :do (begin-array*))
                   (loop :repeat 130 :do (end-array*)))))))

(test writer-max-depth-defaults-when-t
  (signals (json-write-limit-error)
    (with-output-to-string (s)
      (with-writer* (:stream s :max-depth t)
        (loop :repeat 130 :do (begin-array*))
        (loop :repeat 130 :do (end-array*))))))

(test writer-pretty-object-newlines-multiple-kv
  (is (string= "{
  \"x\": 0,
  \"y\": 5
}"
               (with-writer-to-string (*writer* :pretty t)
                 (write-object* "x" 0 "y" 5)))))

(test writer-pretty-object-newlines-if-nested-object
  (is (string= "{
  \"obj\": {
    \"x\": 0,
    \"y\": 5
  }
}"
               (with-writer-to-string (*writer* :pretty t)
                 (with-object*
                   (write-key* "obj")
                   (write-object* "x" 0 "y" 5))))))

(test writer-pretty-array-newlines-if-nested-object
  (is (string= "[
  1,
  {
    \"x\": 0,
    \"y\": 5
  }
]"
               (with-writer-to-string (*writer* :pretty t)
                 (with-array*
                   (write-value* 1)
                   (write-object* "x" 0 "y" 5))))))

(def-suite stringify :in jzon)

(in-suite stringify)

(test stringify-to-nil-returns-string
  (is (string= "42" (stringify 42))))

(test stringify-to-string-writes-to-string
  (is (string= "42" (let ((str (make-array 0 :element-type 'character :fill-pointer t :adjustable t)))
                      (stringify 42 :stream str)
                      str))))

(test stringify-to-t-writes-to-stdout
  (is (string= "42" (with-output-to-string (*standard-output*)
                      (stringify 42 :stream t)))))

(test stringify-to-stream-writes-to-stream
  (is (string= "42" (with-output-to-string (stream)
                      (stringify 42 :stream stream)))))

(test stringify-works-on-binary-streams
  (is (string= "42" (let ((stream (flexi-streams:make-in-memory-output-stream)))
                      (stringify 42 :stream stream)
                      (flexi-streams:octets-to-string (flexi-streams:get-output-stream-sequence stream))))))

(test stringify-pretty-array-spaces-elements
  (is (string= "[
  1,
  2,
  3
]" (stringify #(1 2 3) :pretty t))))

(test stringify-pretty-object-spaces-kv
  (is (string= "{
  \"x\": 0
}" (stringify (ph "x" 0) :pretty t))))


(test string-expands-special-escapes
  (is-every string=
    (#\Backspace (parse "\"\\b\""))
    (#\Page      (parse "\"\\f\""))
    (#\Linefeed  (parse "\"\\n\""))
    (#\Return    (parse "\"\\r\""))
    (#\Tab       (parse "\"\\t\""))))

(test stringify-atoms
  (is-every string=
    ("true"   (stringify t))
    ("false"  (stringify nil))
    ("null"   (stringify 'null))))

(test stringify-integers
  (is (string= "5" (stringify 5)))
  (is (string= "0" (stringify 0))))

(test stringify-1.0f0
  (is (string= "1.0" (stringify 1.0f0))))

(test stringify-1.0d0
  (is (string= "1.0" (stringify 1.0d0))))

(test strigify-12.0d0
  (is (string= "12.0" (stringify 12.0d0))))

(test stringify-123456.0f0
  (is (string= "123456.0" (stringify 123456.0f0))))

(test stringify-1.2d0
  (is (string= "1.2" (stringify 1.2d0))))

(test stringify-1.2f0
  (is (string= "1.2" (stringify 1.2f0))))

(test stringify-strings
  (is (string= "\"hello, world!\"" (stringify "hello, world!")))
  (is (string= "\"\"" (stringify ""))))

(test stringify-string-handles-special-escapes
  (is-every string=
    ("\"\\b\"" (stringify (string #\Backspace)))
    ("\"\\f\"" (stringify (string #\Page)))
    ("\"\\n\"" (stringify (string #\Linefeed)))
    ("\"\\r\"" (stringify (string #\Return)))
    ("\"\\t\"" (stringify (string #\Tab)))))

(test stringify-array
  (is (string= "[]" (stringify #())))
  (is (string= "[42,\"hello\",[]]" (stringify #(42 "hello" #())))))

(test stringify-nested-array
  (is (string= "[[1,2],[3,4]]" (stringify #(#(1 2) #(3 4))))))

(test stringify-nested-array-pretty
  (is (string= "[
  [
    1,
    2
  ],
  [
    3,
    4
  ]
]" (stringify #(#(1 2) #(3 4)) :pretty t))))

(test stringify-multidimensional-array
  (is (string= "[[1,2],[3,4]]" (stringify #2A((1 2) (3 4))))))

(test stringify-non-square-multidimensional-arrays-2×3
  (is (string= "[[0,1,2],[3,4,5]]" (stringify #2A((0 1 2) (3 4 5))))))

(test stringify-non-square-multidimensional-arrays-3x2
  (is (string= "[[0,1],[2,3],[4,5]]" (stringify #2A((0 1) (2 3) (4 5))))))

(test 0-dimension-array
  (is (string= "42" (stringify #0A42))))

(defun recode (value)
  "Shorthand for (parse (stringify value))"
  (parse (stringify value)))

(test stringify-object
  (is (string= "{}" (stringify (ph))))
  ;; Note - We can't reliably test object string output because hash tables ordering might differ
  ;; So instead, parse and verify structure matches
  (is (equalp (ph "x" 100 "y" 45 "name" "Rock") (recode (ph "x" 100 "y" 45 "name" "Rock")))))

(test stringify-coerce-key-writes-integers-base-10
  (with-standard-io-syntax
    (is (string= "{\"10\":10}" (stringify (ph 10 10))))

    (let ((*print-base* 2))
      (is (string= "{\"10\":10}" (stringify (ph 10 10)))))))

(test stringify-coerce-key-writes-single-floats-without-f0
  (with-standard-io-syntax
    (is (string= "{\"1.5\":1.5}" (stringify (ph 1.5f0 1.5f0))))
    (let ((*read-default-float-format* 'double-float))
      (is (string= "{\"1.5\":1.5}" (stringify (ph 1.5f0 1.5f0)))))))

(test stringify-coerce-key-writes-double-floats-without-d0
  (with-standard-io-syntax
    (is (string= "{\"1.5\":1.5}" (stringify (ph 1.5d0 1.5d0))))
    (let ((*read-default-float-format* 'double-float))
      (is (string= "{\"1.5\":1.5}" (stringify (ph 1.5f0 1.5f0)))))))

(test stringify-coerce-key-writes-rationals-like-floats
  (with-standard-io-syntax
    (is (string= "{\"1.5\":1.5}" (stringify (ph 3/2 3/2))))))

(test stringify-coerce-key-ignores-print-base
  (let ((*print-base* 2))
    (is (string= "{\"1.5\":1.5}" (stringify (ph 3/2 3/2))))))

(defclass test-class ()
  ((a
    :initarg :a
    :type t)
   (b
    :initarg :b
    :type list)
   (c
    :initarg :c
    :type boolean)))

(defun test-class (&rest args)
  "Shorthand for (make-instance 'test-class ...)"
  (apply #'make-instance 'test-class args))

(test stringify-class-includes-only-bound-slots
  (is-every equalp
    ((ph) (recode (test-class)))
    ((ph "a" 'null) (recode (test-class :a nil)))
    ((ph "b" #(1 2 3)) (recode (test-class :b '(1 2 3))))
    ((ph "a" 42 "b" #(1 2 3)) (recode (test-class :a 42 :b '(1 2 3))))))

(test stringify-class-uses-type-for-nil
  (is (equalp (ph "a" 'null "b" #() "c" nil) (recode (test-class :a nil :b nil :c nil)))))

(test stringify-class-recurses
  (is (equalp (ph "a" (ph "a" 42)) (recode (test-class :a (test-class :a 42))))))

(test stringify-class-in-plain-data
  (is (equalp (ph "a" (ph "a" 42)) (recode (ph "a" (test-class :a 42))))))

(defclass test-class-case ()
  ((all-upper :initform 0)
   (|mixedCase| :initform 0)))

(test stringify-class-downcases-symbols-except-mixed-case
  (is (equalp (ph "all-upper" 0 "mixedCase" 0) (recode (make-instance 'test-class-case)))))

(defclass test-class/stringify-coerced-fields ()
  ())

(defmethod coerced-fields ((a test-class/stringify-coerced-fields))
  (declare (ignore a))
  (list (list "foo" 42)
        (list "bar" 101.1d0)
        (list "baz" #(192 168 1 1))))

(test stringify-pretty-argorder-bugfix
  (is (string= "[
  {
    \"x\": 0
  }
]" (stringify (vector (ph "x" 0)) :pretty t))))

(test stringify-pretty-prints-keys
  (is (string= "{\"#(1 2)\":0}" (stringify (ph #(1 2) 0)))))

(test stringify-errors-on-circular-references
  (signals (json-recursive-write-error)
    (let ((ht (ph "x" 0)))
      (setf (gethash "self" ht) ht)
      (stringify ht))))

(test stringify-errors-on-circular-reference-during-pretty-print
  (signals (json-recursive-write-error)
    (let ((ht (ph "x" 0)))
      (setf (gethash "self" ht) ht)
      (stringify ht :pretty t))))

(test stringify-errors-on-circular-reference-vector-during-pretty-print
  (signals (json-recursive-write-error)
    (let ((v (vector 0 1 2)))
      (setf (aref v 0) v)
      (stringify v :pretty t))))

(test stringify-errors-on-non-symbol-coerce-key
  (signals (type-error) (stringify 42 :coerce-key 0)))

(test stringify-allows-symbols-on-coerce-key
  (finishes (stringify 42 :coerce-key (constantly "42"))))

(test stringify-coerce-key-calls-fn
  (is (string= "{\"something-else\":42}" (stringify (ph "something" 42) :coerce-key (constantly "something-else")))))

(test stringify-no-alist-when-integers
  (is (string= "[[1,5],[2,42]]" (stringify '((1 5) (2 42))))))

(test stringify-no-plist-when-integers
  (is (string= "[1,5,2,42]" (stringify '(1 5 2 42)))))

(test stringify-coerces-pathname-to-namestring
  (is (string= "\"hello.lisp\""(stringify #p"hello.lisp"))))

(test stringify-signals-type-error-on-improper-sequence
  (signals (type-error)
    (stringify (cons 1 2))))

(test stringify-replacer-keeps-keys-on-t
  (is (string= "{\"x\":0}"
               (stringify (ph :x 0)
                          :replacer (lambda (k v)
                                      (declare (ignore k v))
                                      t)))))

(test stringify-replacer-filters-keys-on-nil
  (is (string= "{}"
               (stringify (ph :x 0)
                          :replacer (lambda (k v)
                                      (declare (ignore v))
                                      (case k
                                        ((nil) t)
                                        (t nil)))))))

(test stringify-replacer-filters-some-keys-on-nil
  (is (string= "{\"y\":0}"
               (stringify (ph :x 0 :y 0)
                          :replacer (lambda (k v)
                                      (declare (ignore v))
                                      (case k
                                        ((nil) t)
                                        (t (eq k :y))))))))

(test stringify-replacer-replaces-values-using-multiple-values
  (is (string= "{\"x\":42}"
               (stringify (ph :x 0)
                          :replacer (lambda (k v)
                                      (declare (ignore v))
                                      (case k
                                        ((nil) t)
                                        (t (values t 42))))))))

(test stringify-replacer-ignores-second-value-on-nil
  (is (string= "{}"
               (stringify (ph :x 0)
                          :replacer (lambda (k v)
                                      (declare (ignore v))
                                      (case k
                                        ((nil) t)
                                        (t (values nil 42))))))))

(test stringify-replacer-is-called-on-sub-objects
  (is (string= "{\"x\":{\"a\":42}}"
               (stringify (ph :x (ph :a 0))
                          :replacer (lambda (k v)
                                      (declare (ignore v))
                                      (if (eq k :a)
                                          (values t 42)
                                          t))))))

(test stringify-replacer-is-called-recursively
  (is (string= "{\"x\":{\"y\":{\"z\":0}}}"
               (stringify (ph :x 0)
                          :replacer (lambda (k v)
                                      (declare (ignore v))
                                      (case k
                                        (:x (values t (ph :y 0)))
                                        (:y (values t(ph :z 0)))
                                        (t t)))))))

(test stringify-replacer-can-ommit-toplevel
  (is (string= "" (stringify (ph :x 0) :replacer (constantly nil))))
  (is (string= "" (stringify 42 :replacer (constantly nil)))))

(test stringify-replacer-is-called-on-toplevel-value-with-nil-key
  (5am:is-true
   (let ((key-is-nil nil))
     (stringify 0 :replacer (lambda (k v)
                              (declare (ignore v))
                              (setf key-is-nil (null k))))
     key-is-nil))
  (5am:is-true
   (let ((value 0)
         (value-is-same nil))
     (stringify value :replacer (lambda (k v)
                                  (declare (ignore k))
                                  (setf value-is-same (eq value v) )))
     value-is-same)))

(test stringify-replacer-can-replace-toplevel-value
  (is (string= "42" (stringify 0 :replacer (lambda (k v)
                                             (declare (ignore k v))
                                             (values t 42))))))

(test stringify-replacer-is-called-on-array-elements-with-element-indexes
  (is (equalp #(0 1 2)
              (let ((keys (list)))
                (stringify #(t t t) :replacer (lambda (k v)
                                                (declare (ignore v))
                                                (when k
                                                  (push k keys))
                                                t)) ;; Gotta return t or it'll remove the elements
                (coerce (nreverse keys) 'vector)))))

(test stringify-replacer-is-called-on-list-elements-with-element-indexes
  (is (equalp #(0 1 2)
              (let ((keys (list)))
                (stringify '(t t t) :replacer (lambda (k v)
                                                (declare (ignore v))
                                                (when k
                                                  (push k keys))
                                                t)) ;; Gotta return t or it'll remove the elements
                (coerce (nreverse keys) 'vector)))))

(test stringify-replacer-is-only-called-with-nil-on-toplevel-value
  (is (equalp '(#(1 2 3))
              (let ((called-on (list)))
                (stringify #(1 2 3) :replacer (lambda (k v)
                                                (when (null k)
                                                  (push v called-on))
                                                t))
                (nreverse called-on)))))

;; This lies in (or (< e10 -3) (>= e10 7)) for schubfach
;; where we previously were generating garbage strings
(test stringify-1.0e-4
  (is (equalp "1.0e-4" (stringify 1.0e-4))))

(def-suite jzon.json-checker :in jzon)

(in-suite jzon.json-checker)

;; fail1 in json-checker goes against RFC
;; (test fail1
;;   (signals json-parse-error (parse "\"A JSON payload should be an object or array, not a string.\"")))

(test fail2
  (signals json-parse-error (parse "[\"Unclosed array\"")))

(test fail3
  (signals json-parse-error (parse "{unquoted_key: \"keys must be quoted\"}")))

(test fail4
  (signals json-parse-error (parse "[\"extra comma\",]")))

(test fail5
  (signals json-parse-error (parse "[\"double extra comma\",,]")))

(test fail6
  (signals json-parse-error (parse "[   , \"<-- missing value\"]")))

(test fail7
  (signals json-parse-error (parse "[\"Comma after the close\"],")))

(test fail8
  (signals json-parse-error (parse "[\"Extra close\"]]")))

(test fail9
  (signals json-parse-error (parse "{\"Extra comma\": true,}")))

(test fail10
  (signals json-parse-error (parse "{\"Extra value after close\": true} \"misplaced quoted value\"")))

(test fail11
  (signals json-parse-error (parse "{\"Illegal expression\": 1 + 2}")))

(test fail12
  (signals json-parse-error (parse "{\"Illegal invocation\": alert()}")))

(test fail13
  (signals json-parse-error (parse "{\"Numbers cannot have leading zeroes\": 013}")))

(test fail14
  (signals json-parse-error (parse "{\"Numbers cannot be hex\": 0x14}")))

(test fail15
  (signals json-parse-error (parse "[\"Illegal backslash escape: \\x15\"]")))

(test fail16
  (signals json-parse-error (parse "[\\naked]")))

(test fail17
  (signals json-parse-error (parse "[\"Illegal backslash escape: \\017\"]")))

(test fail18
  (signals json-parse-limit-error (parse "[[[[[[[[[[[[[[[[[[[[\"Too deep\"]]]]]]]]]]]]]]]]]]]]" :max-depth 19)))

(test fail19
  (signals json-parse-error (parse "{\"Missing colon\" null}")))

(test fail20
  (signals json-parse-error (parse "{\"Double colon\":: null}")))

(test fail21
  (signals json-parse-error (parse "{\"Comma instead of colon\", null}")))

(test fail22
  (signals json-parse-error (parse "[\"Colon instead of comma\": false]")))

(test fail23
  (signals json-parse-error (parse "[\"Bad value\", truth]")))

(test fail24
  (signals json-parse-error (parse "['single quote']")))

(test fail25
  (signals json-parse-error (parse "[\"	tab	character	in	string	\"]")))

(test fail26
  (signals json-parse-error (parse "[\"tab\\   character\\   in\\  string\\  \"]")))

(test fail27
  (signals json-parse-error (parse "[\"line
break\"]")))

(test fail28
  (signals json-parse-error (parse "[\"line\\
break\"]")))

(test fail29
  (signals json-parse-error (parse "[0e]")))

(test fail30
  (signals json-parse-error (parse "[0e+]")))

(test fail31
  (signals json-parse-error (parse "[0e+-1]")))

(test fail32
  (signals json-parse-error (parse "{\"Comma instead if closing brace\": true,")))

(test fail33
  (signals json-parse-error (parse "[\"mismatch\"}")))

(test pass1
  (is (equalp
       (vector "JSON Test Pattern pass1"
               (ph "object with 1 member" (vector "array with 1 element"))
               (ph)
               (vector)
               -42
               t
               nil
               'null
               (ph "integer" 1234567890
                   "real" -9876.54321d0
                   "e" 1.23456789d-13
                   "E" 1.23456789d34
                   "" 2.3456789012d76
                   "zero" 0
                   "one" 1
                   "space" " "
                   "quote" "\""
                   "backslash" "\\"
                   "controls" "
	"
                   "slash" "/ & /"
                   "alpha" "abcdefghijklmnopqrstuvwyz"
                   "ALPHA" "ABCDEFGHIJKLMNOPQRSTUVWYZ"
                   "digit" "0123456789"
                   "0123456789" "digit"
                   "special" "`1~!@#$%^&*()_+-={':[,]}|;.</>?"
                   "hex" "ģ䕧覫췯ꯍ"
                   "true" t
                   "false" nil
                   "null" 'null
                   "array" (vector)
                   "object" (ph)
                   "address" "50 St. James Street"
                   "url" "http://www.JSON.org/"
                   "comment" "// /* <!-- --"
                   "# -- --> */" " "
                   " s p a c e d " #(1 2 3 4 5 6 7)
                   "compact" #(1 2 3 4 5 6 7)
                   "jsontext" "{\"object with 1 member\":[\"array with 1 element\"]}"
                   "quotes" "&#34; \" %22 0x22 034 &#x22;"
                   "/\\\"쫾몾ꮘﳞ볚
	`1~!@#$%^&*()_+-=[]{}|;:',./<>?" "A key can be any string")
         0.5d0 98.6d0 99.44d0 1066 10.0d0 1.0d0 0.1d0 1.0d0 2.0d0 2.0d0 "rosebud")
       (parse "[
    \"JSON Test Pattern pass1\",
    {\"object with 1 member\":[\"array with 1 element\"]},
    {},
    [],
    -42,
    true,
    false,
    null,
    {
        \"integer\": 1234567890,
        \"real\": -9876.543210,
        \"e\": 0.123456789e-12,
        \"E\": 1.234567890E+34,
        \"\":  23456789012E66,
        \"zero\": 0,
        \"one\": 1,
        \"space\": \" \",
        \"quote\": \"\\\"\",
        \"backslash\": \"\\\\\",
        \"controls\": \"\\b\\f\\n\\r\\t\",
        \"slash\": \"/ & \\/\",
        \"alpha\": \"abcdefghijklmnopqrstuvwyz\",
        \"ALPHA\": \"ABCDEFGHIJKLMNOPQRSTUVWYZ\",
        \"digit\": \"0123456789\",
        \"0123456789\": \"digit\",
        \"special\": \"`1~!@#$%^&*()_+-={':[,]}|;.</>?\",
        \"hex\": \"\\u0123\\u4567\\u89AB\\uCDEF\\uabcd\\uef4A\",
        \"true\": true,
        \"false\": false,
        \"null\": null,
        \"array\":[  ],
        \"object\":{  },
        \"address\": \"50 St. James Street\",
        \"url\": \"http://www.JSON.org/\",
        \"comment\": \"// /* <!-- --\",
        \"# -- --> */\": \" \",
        \" s p a c e d \" :[1,2 , 3

,

4 , 5        ,          6           ,7        ],\"compact\":[1,2,3,4,5,6,7],
        \"jsontext\": \"{\\\"object with 1 member\\\":[\\\"array with 1 element\\\"]}\",
        \"quotes\": \"&#34; \\u0022 %22 0x22 034 &#x22;\",
        \"\\/\\\\\\\"\\uCAFE\\uBABE\\uAB98\\uFCDE\\ubcda\\uef4A\\b\\f\\n\\r\\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?\"
: \"A key can be any string\"
    },
    0.5 ,98.6
,
99.44
,

1066,
1e1,
0.1e1,
1e-1,
1e00,2e+00,2e-00
,\"rosebud\"]"))))

(test pass2
  (is (equalp #(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#(#("Not too deep")))))))))))))))))))
              (parse "[[[[[[[[[[[[[[[[[[[\"Not too deep\"]]]]]]]]]]]]]]]]]]]"))))

(test pass3
  (is (equalp (ph "JSON Test Pattern pass3"
                  (ph "The outermost value" "must be an object or array."
                      "In this test" "It is an object."))
              (parse "{
    \"JSON Test Pattern pass3\": {
        \"The outermost value\": \"must be an object or array.\",
        \"In this test\": \"It is an object.\"
    }
}
"))))
