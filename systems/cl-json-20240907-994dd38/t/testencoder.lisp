;;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Base:10; Package: JSON-TEST -*-
(in-package :json-test)
(in-suite json)

(defmacro with-objects-as-hashtables (&body body)
  ;;For testing, keys are stored as strings
  (let ((ht (gensym)) (key (gensym)))
    `(let (,ht ,key)
       (declare (special ,ht ,key))
       (json:bind-custom-vars
           (:beginning-of-object #'(lambda () (setq ,ht (make-hash-table :test #'equalp)))
            :object-key #'(lambda (key) (setq ,key key))
            :object-value #'(lambda (value) (setf (gethash ,key ,ht) value))
            :end-of-object #'(lambda () ,ht)
            :object-scope '(,ht ,key))
         ,@body))))

(test json-string()
   (is (string= (encode-json-to-string (format nil "hello~&hello"))
                 "\"hello\\nhello\""))
   (is (string= (encode-json-to-string (format nil "\"aquote"))
                 "\"\\\"aquote\"")))

(test json-literals
  (is (string= "true" (encode-json-to-string t)))
  (is (string= "null" (encode-json-to-string nil))))

(defun is-same-number(nr)
  "If it gets decoded back ok then it was encoded ok"
  (is (= nr (decode-json-from-string (encode-json-to-string nr)))))

(test json-number
  (is (string= "0" (encode-json-to-string 0)))
  (is (string= "13" (encode-json-to-string 13)))
  (is (string= "13.02" (encode-json-to-string 13.02)))
  (is (string= "13.02" (encode-json-to-string 13.02D0)))
  (is (string= "13.02" (encode-json-to-string 13.02L0)))
  (is (string= "13.02" (encode-json-to-string 13.02S0)))
  (is (string= "13.02" (encode-json-to-string 13.02E0)))

  (is-same-number 2e10)
  (is-same-number  -1.3234e-10)
  (is-same-number -1280.12356)
  (is-same-number 1d2)
  (is-same-number 1l2)
  (is-same-number 1s2)
  (is-same-number 1f2)
  (is-same-number 1e2))

(defun decode-then-encode (json)
  (with-objects-as-hashtables
    (assert (member (elt json 0) '(#\{ #\[ #\" ))) ;must be json
    (flet ((normalize (string)
             (remove #\Newline (remove #\Space string))))
      (let* ((decoded (decode-json-from-string json))
             (encoded (encode-json-to-string decoded)))
;;        (format t "Decoded:~a~&" decoded)
;;        (format t "Encoded:~a" encoded)    
        (is (string= (normalize json)
                     (normalize encoded)))))))

(test test-encode-json-nathan-hawkins
  (let ((foo '((a . 1) (b . 2) (c . 3))))
    (is (string= (encode-json-to-string foo)
                 "{\"a\":1,\"b\":2,\"c\":3}"))))

(test test-encode-json-alist
      (let ((alist `((:HELLO . 100)(:hi . 5)))
            (expected "{\"hello\":100,\"hi\":5}"))
        (is (string= (with-output-to-string (s) (encode-json-alist alist s))
                     expected))))
 
(test test-encode-json-alist-two
  (let ((alist `((HELLO . 100)(hi . 5)))
        (expected "{\"hello\":100,\"hi\":5}"))
    (is (string= (with-output-to-string (s) (encode-json-alist alist s))
                   expected))))

(test test-encode-json-alist-nested
      (let ((alist `((:hello . 100)(:hi . ((:a . 1)
                                           (:b . 2)))))
            (expected "{\"hello\":100,\"hi\":{\"a\":1,\"b\":2}}"))
        (is (string= (with-output-to-string (s) (encode-json-alist alist s))
                     expected))))

(test test-encode-json-alist-string
      (let ((alist `((:hello . "hej")(:hi . "tjena")))
            (expected "{\"hello\":\"hej\",\"hi\":\"tjena\"}"))
        (is (string= (with-output-to-string (s) (encode-json-alist alist s))
                     expected))))

(test test-encode-json-alist-camel-case
      (let ((alist `((:hello-message . "hej")(*also-starting-with-upper . "hej")))
            (expected "{\"helloMessage\":\"hej\",\"AlsoStartingWithUpper\":\"hej\"}"))
        (is (string= (with-output-to-string (s) (encode-json-alist alist s))
                     expected))))

(test test-encode-json-alist-embedded-nil
  ;; It makes construction easier in some cases, such as this:
      (let ((alist `((:hello . "england")
                     ,(when nil
                            '(:ciao . "italy"))
                     (:hej . "sweden")))
            (expected "{\"hello\":\"england\",\"hej\":\"sweden\"}"))
        (is (string= (with-output-to-string (s) (encode-json-alist alist s))
                     expected))))

;;; Invalidated by the patch ``Various modifications, mostly concerning
;;; the encoder.'' (Wed Jan 21 18:34:49 MSK 2009)
;
; (test test-encode-json-alist-with-prototype
;   (let ((alist `((hello . 100) (hi . 5)))
;         (expected "{\"hello\":100,\"hi\":5,\"prototype\":{\"lispClass\":\"cons\",\"lispSuperclasses\":null,\"lispPackage\":\"jsonTest\"}}")
;         (*prototype-name* 'prototype))
;     (is (string= (encode-json-to-string alist) expected))))

(test test-encode-json-plist
      (let ((plist '(:foo 1 :bar "blub"))
            (expected "{\"foo\":1,\"bar\":\"blub\"}"))
        (is (string= (with-output-to-string (s) (encode-json-plist plist s))
                     expected))
        (is (string= (encode-json-plist-to-string plist)
                     expected))))

(test encode-pass-2
  (decode-then-encode "[[[[[[[[[[[[[[[[[[[\"Not too deep\"]]]]]]]]]]]]]]]]]]]"))

(test encode-pass-3
  (decode-then-encode "{
    \"JSON Test Pattern pass3\": {
        \"The outermost value\": \"must be an object or array.\"
    }
}
"))

(defclass foo () ((bar :initarg :bar) (baz :initarg :baz)))
(defclass goo () ((quux :initarg :quux :initform 933)))
(defclass frob (foo goo) ())

#+cl-json-clos
(test test-encode-json-clos
  (finalize-inheritance (find-class 'goo))
  (let ((obj (make-instance 'foo
               :bar (json:make-object '((hello . 100) (hi . 5))
                                      nil '(goo))
               :baz (make-instance 'frob
                      :bar 'xyzzy
                      :baz (make-instance 'fluid-object))))
        (expected "{\"bar\":{\"quux\":933,\"hello\":100,\"hi\":5},\"baz\":{\"quux\":933,\"bar\":\"xyzzy\",\"baz\":{}}}"))
    (is (string= (encode-json-to-string obj) expected))))

;;; Invalidated by the patch ``Various modifications, mostly concerning
;;; the encoder.'' (Wed Jan 21 18:34:49 MSK 2009)
; 
; (test test-encode-json-clos-with-prototype
;   (finalize-inheritance (find-class 'goo))
;   (let ((obj (make-instance 'foo
;                :bar (json:make-object '((hello . 100) (hi . 5))
;                                       nil '(goo))
;                :baz (make-instance 'frob :bar 'xyzzy :baz 'blub)))
;         (expected "{\"bar\":{\"quux\":933,\"hello\":100,\"hi\":5,\"prototype\":{\"lispClass\":null,\"lispSuperclasses\":[\"goo\"],\"lispPackage\":\"jsonTest\"}},\"baz\":{\"quux\":933,\"bar\":\"xyzzy\",\"baz\":\"blub\",\"prototype\":{\"lispClass\":\"frob\",\"lispSuperclasses\":null,\"lispPackage\":\"jsonTest\"}},\"prototype\":{\"lispClass\":\"foo\",\"lispSuperclasses\":null,\"lispPackage\":\"jsonTest\"}}")
;         (*prototype-name* 'prototype))
;     (is (string= (encode-json-to-string obj) expected))))

#.(export 'json::emotional (find-package '#:json))
#+cl-json-clos
(test test-encode-json-clos-max-package
  (let ((obj (json:make-object
              `((rational . 100) (emotional . 5)
                (prototype . ,(json:make-object-prototype nil '(rational emotional prototype))))
              nil))
        (expected "{\"rational\":100,\"emotional\":5,\"prototype\":{\"lispClass\":null,\"lispSuperclasses\":null,\"lispPackage\":\"json\"}}"))
    (is (string= (with-output-to-string (s) (encode-json obj s))
                 expected))))

;; Test inspired by the file pass1. 
;; There are too many small differences just to decode-encode the whole pass1 file,
;; Instead the difficult parts are in separate tests below.

(test controls
   (decode-then-encode "\"\\\\b\\\\f\\\\n\\\\r\\\\\""))

(test slash
  (let* ((z "\"/ & /\"")
         (also-z "\"/ & \/\"") ;Extra quote
         (x (encode-json-to-string z))
         (also-x (encode-json-to-string also-z))
         (y (decode-json-from-string x))
         (also-y (decode-json-from-string also-x)))
    (is (string= x also-x))
    (is (string= y also-y))
    (is (string= z y))))


(test quoted
  (decode-then-encode "\"&#34; %22 0x22 034 &#x22;\""))

(test alpha-1
  (decode-then-encode "\"abcdefghijklmnopqrstuvwyz\""))

(test alpha-2
  (decode-then-encode "\"ABCDEFGHIJKLMNOPQRSTUVWYZ\""))

(test digit
  (decode-then-encode "\"0123456789\""))

(test special
  (decode-then-encode "\"`1~!@#$%^&*()_+-={':[,]}|;.<>?\""))

(test hex
  (decode-then-encode "\"\u0123\u4567\u89AB\uCDEF\uabcd\uef4A\""))

(test true
  (decode-then-encode "[ true]"))

(test false
  (is (string= (encode-json-to-string (decode-json-from-string "[false]"))
               "[null]")));;We dont separate between false and null
(test null
  (decode-then-encode "[null]"))

(test array
  (with-decoder-simple-list-semantics
    ;;Since empty lists becomes nil in lisp, they are converted back to null
    (is (string= (encode-json-to-string (decode-json-from-string "[  ]"))
                 "null")))
  #+cl-json-clos
  (with-decoder-simple-clos-semantics
    ;;Since empty lists becomes #() in lisp, they are converted back to empty list
    (is (string= (encode-json-to-string (decode-json-from-string "[  ]"))
                 "[]")))
  ;;But you can use vectors
  (is (string= (encode-json-to-string (vector 1 2))
               "[1,2]")))

(test character
  ;;Characters are encoded to strings, but when decoded back to string
  (is (string= (encode-json-to-string #\a) "\"a\"")))


(test hash-table-symbol
  (let ((ht (make-hash-table)))
    (setf (gethash 'symbols-are-now-converted-to-camel-case ht) 5)
    (is (string= (encode-json-to-string ht)
                 "{\"symbolsAreNowConvertedToCamelCase\":5}"))))

;;; Invalidated by the patch ``Various modifications, mostly concerning
;;; the encoder.'' (Wed Jan 21 18:34:49 MSK 2009)
;
; (test hash-table-symbol-with-prototype
;   (let ((ht (make-hash-table))
;         (*prototype-name* 'prototype))
;     (setf (gethash 'five ht) 5)
;     (is (string= (encode-json-to-string ht)
;                  "{\"five\":5,\"prototype\":{\"lispClass\":\"hashTable\",\"lispSuperclasses\":null,\"lispPackage\":\"jsonTest\"}}"))))

(test hash-table-string
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "lower x" ht) 5)
    (is (string= (encode-json-to-string ht)
                 "{\"lower x\":5}"))))


(defparameter *encode-performace-test-string*
  "{
    \"JSON Test Pattern pass3\": {
        \"The outermost value\": \"must be an object or array.\",
        \"In this test\": \"It is an object.\",
        \"Performance-1\" : 123465.578,
        \"Performance-2\" : 12e4,
        \"Performance-2\" : \"asdasdsadsasdasdsdasdasdasdsaaaaaaaaaaaaasdasdasdasdasdsd\",
        \"Performance-3\" : [\"asdasdsadsasdasdsdasdasdasdsaaaaaaaaaaaaasdasdasdasdasdsd\",
                         \"asdasdsadsasdasdsdasdasdasdsaaaaaaaaaaaaasdasdasdasdasdsd\",
                         \"asdasdsadsasdasdsdasdasdasdsaaaaaaaaaaaaasdasdasdasdasdsd\",
                         \"asdasdsadsasdasdsdasdasdasdsaaaaaaaaaaaaasdasdasdasdasdsd\"]
    }
}
")

(test encoder-performance
  (with-objects-as-hashtables      
    (let* ((json-string *encode-performace-test-string*)
           (chars (length json-string))
           (lisp-obj (decode-json-from-string json-string))
           (count 2000))
      (format t "Encoding ~a varying chars  from memory ~a times." chars count)
      (time
       (dotimes (x count) 
         (let ((discard-soon (encode-json-to-string lisp-obj)))
           (funcall #'identity discard-soon)))))))

(test streaming-encoder
  (defpackage json-test-foo
    (:use)
    (:export #:bar #:baz #:quux))
  (defpackage json-test-greek
    (:use)
    (:export #:lorem #:ipsum #:dolor #:sit #:amet))
  (let* ((encoded
          (with-output-to-string (out)
            (with-object (out)
              (with-input-from-string (in "json-test-foo json-test-greek")
                (loop for pkgname = (read in nil) while pkgname
                   do (as-object-member (pkgname out)
                        (with-array (out)
                          (do-symbols (sym (find-package pkgname))
                            (encode-array-member sym out)))))))))
         (package-alist
          (with-decoder-simple-list-semantics
            (let ((*json-symbols-package* 'json-test))
              (decode-json-from-string encoded))))
         (foo-symbols
          (cdr (assoc 'json-test-foo package-alist)))
         (greek-symbols
          (cdr (assoc 'json-test-greek package-alist))))
    (flet ((same-string-sets (a b)
             (null (set-exclusive-or a b :test #'string-equal))))
      (is (same-string-sets foo-symbols '("BAR" "BAZ" "QUUX")))
      (is (same-string-sets greek-symbols '("LOREM" "IPSUM" "DOLOR" "SIT" "AMET"))))))

(test explicit-encoder
  (is (string= "true" (with-explicit-encoder
                        (encode-json-to-string '(:true))))
      "True")
  (is (string= "false"
               (with-explicit-encoder
                 (encode-json-to-string '(:false))))
      "False")
  (is (string= "null"
               (with-explicit-encoder
                 (encode-json-to-string '(:null))))
      "False")
  (is (string= "[1,\"a\"]"
               (with-explicit-encoder
                 (encode-json-to-string '(:list 1 "a"))))
      "List")
  (is (string= "[1,\"a\"]"
               (with-explicit-encoder
                 (encode-json-to-string '(:array 1 "a"))))
      "Array")
  (is (string= "{\"a\":1,\"b\":2}"
               (with-explicit-encoder
                 (encode-json-to-string '(:object "a" 1 "b" 2))))
      "Plist")
  (is (string= "{\"a\":1,\"b\":2}"
               (with-explicit-encoder
                 (encode-json-to-string '(:object ("a" . 1) ( "b" . 2)))))
      "Alist")
  (is (string= "{\"a\":1,\"b\":2}"
               (with-explicit-encoder
                 (encode-json-to-string '(:alist ("a" . 1) ( "b" . 2)))))
      "Explicit Alist")
  
  (is (string= "{\"a\":1,\"b\":2}"
               (with-explicit-encoder
                 (encode-json-to-string '(:plist "a" 1 "b" 2))))
      "Explicit Plist")
  
  (is (string= "{\"a\":{\"c\":1,\"d\",2},\"b\":2}"
               (with-explicit-encoder
                 (encode-json-to-string '(:object
                                          "a" (:json "{\"c\":1,\"d\",2}")
                                          "b" 2))))
      "Embedded json")
  (is (string= "{\"a\":{\"c\":1,\"d\":2},\"b\":2}"
               (with-explicit-encoder
                 (encode-json-to-string '(:object
                                          "a" (:object "c" 1 "d" 2)
                                          "b" 2))))
      "Object in object")
  (is (string= "{\"a\":{\"c\":1,\"d\":2},\"b\":2}"
               (with-explicit-encoder
                 (encode-json-to-string '(:object
                                          "a" (:object ( "c" . 1) ( "d" . 2))
                                          "b" 2))))
      "Mixed alist and plist")

  (is (string= "{\"a\":1,\"b\":2}"
               (with-explicit-encoder
                 (encode-json-to-string '(:object ("a" . 1) nil ( "b" . 2)))))
      "Alist with a nil value"))


(test explicit-encoder-complex-objects
  (let ((sample-1-alists
         `(:object
           (:method . some-function)
           (:id . 1)
           (:params .
                    (:list
                     (:object (:id . bar-id)
                              (:name . bar))
                     (:true)
                     (:list (:object
                             (:name . a)
                             (:id . b)))
                     (:list (:object
                             (:name . foo)
                             (:id . foo-id)))
                     ))))
        (sample-2-plists
         `(:object
           :method some-function
           :id  1
           :params (:list
                    (:json "{\"id\":\"barId\",\"name\":\"bar\"}")
                    (:true)
                    (:list (:object "name"  a :id b))
                    (:list (:object
                            :name  foo
                            :id  foo-id))
            )))
        (correct-json "
          {\"method\":\"someFunction\",
            \"id\":1,\"params\":
                 [{\"id\":\"barId\",\"name\":\"bar\"},
                  true,
                 [{\"name\":\"a\",\"id\":\"b\"}],
                 [{\"name\":\"foo\",\"id\":\"fooId\"}]]}")
        exact-json sample-1-json sample-2-json)
    (setf sample-1-json 
          (with-explicit-encoder
            (encode-json-to-string sample-1-alists)))
    (setf sample-2-json 
          (with-explicit-encoder
            (encode-json-to-string sample-2-plists)))
    (is (string= sample-1-json sample-2-json))
    (setf exact-json (remove #\Newline
                             (remove #\Space correct-json :test #'char=)
                             :test #'char=))
    (is (string= sample-1-json exact-json))))

(test json-bool
  (is (equal (json-bool t) '(:true)))
  (is (equal (json-bool 1) '(:true)))
  (is (equal (json-bool nil) '(:false))))

(test json-or-null
  (is (equal (json-or-null 'something) 'something))
  (is (equal (json-or-null '(:list)) '(:list)))
  (is (equal (json-or-null nil) '(:null)))
  (is (equal (json-or-null (when t  '(:list)))
             '(:list)))
  (is (equal (json-or-null (when nil  '(:list)))
             '(:null))))

(test sample-explict-decoder-building-with-json-bool
  (labels
      ((foo-p (thing)
         (eq thing 'foo))
       (bar-p (thing)
         (eq thing 'bar))
       (foo-bar (thing)
         (when (foo-p thing)
           (cons :foo-bar "FUBAR!")))
       (bar-json ()
         "{\"bar\":true}")
       (get-json (thing)
         (with-explicit-encoder
           (encode-json-to-string
            `(:object :method foo-bar-check :id 0
                      :params (:object
                               (:bar . (:json ,(bar-json)))
                               (:is-foo . ,(json-bool (foo-p thing)))
                               (:is-bar . ,(json-bool (bar-p thing)))
                               ,(foo-bar thing)))))))
    (let ((json-foo (get-json 'foo))
          (json-bar (get-json 'bar)))
      (is (string= json-foo
"{\"method\":\"fooBarCheck\",\"id\":0,\"params\":{\"bar\":{\"bar\":true},\"isFoo\":true,\"isBar\":false,\"fooBar\":\"FUBAR!\"}}"))
      (is (string= json-bar
"{\"method\":\"fooBarCheck\",\"id\":0,\"params\":{\"bar\":{\"bar\":true},\"isFoo\":false,\"isBar\":true}}")))))

