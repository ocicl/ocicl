;;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Base:10; Package: JSON-TEST -*-
(in-package :json-test)

(in-suite json)

;; Test decoder

(defun make-json-array-type (&rest elements)
  (map json:*json-array-type* #'identity elements))

(test json-literal
  (is-true (decode-json-from-string "  true"))
  (is-true (decode-json-from-string "  true "))
  (is-true (decode-json-from-string "true "))
  (is-true (decode-json-from-string "true"))
;;; Invalidated by the patch ``Moved the customizable decoder
;;; (special-vars flavour) over to the main branch.'' (Thu Dec 4
;;; 22:02:02 MSK 2008).  This is indeed an error situation, not one
;;; where a false value may be legally returned.
;   (is-false (decode-json-from-string "trUe "))
  (is-false (decode-json-from-string "false"))
  (is-false (decode-json-from-string "null"))
  )

(test json-string
  (is (string= "hello"
              (decode-json-from-string "  \"hello\"")))
  (is (string= "new-line
returned!"
              (decode-json-from-string "\"new-line\\nreturned!\"")))
  (is (string= (make-string 1 :initial-element (code-char (+ (* 10 16) 11)))
              (decode-json-from-string "  \"\\u00ab\""))))

(test json-array
  (is (equalp
       (make-json-array-type "hello" "hej" "ciao")
       (decode-json-from-string " [ \"hello\",  \"hej\",
                   \"ciao\" ]")))
  (is (equalp (make-json-array-type 1 2 3)
              (decode-json-from-string "[1,2,3]")))
  (is (equalp (make-json-array-type t nil nil)
              (decode-json-from-string "[true,null,false]")))
  (is (equalp (make-json-array-type)
              (decode-json-from-string "[]"))))

#+cl-json-clos
(defgeneric equal-objects-p (object1 object2)
  (:method (object1 object2)
    (equalp object1 object2))
  (:method ((object1 standard-object) (object2 standard-object))
    (let ((class1 (class-of object1))
          (class2 (class-of object2)))
      (and (eql class1 class2)
           (loop for slot in (json::class-slots class1)
              for slot-name = (json::slot-definition-name slot)
              always (if (slot-boundp object1 slot-name)
                         (and (slot-boundp object2 slot-name)
                              (equal-objects-p
                               (slot-value object1 slot-name)
                               (slot-value object2 slot-name)))
                         (not (slot-boundp object2 slot-name))))))))

(test json-object
  (let ((input " { \"hello\" : \"hej\" ,
                       \"hi\" : \"tjena\"
                     }"))
    (let ((*json-symbols-package* (find-package :keyword)))
      (with-decoder-simple-list-semantics
        (is (equalp '((:hello . "hej") (:hi . "tjena"))
                    (decode-json-from-string input)))
        (is-false (decode-json-from-string " {  } "))
        (is-false (decode-json-from-string "{}"))))
    #+cl-json-clos
    (let ((*json-symbols-package* (find-package :json-test)))
      (with-decoder-simple-clos-semantics
        (is (equal-objects-p
             (json:make-object '((hello . "hej") (hi . "tjena")) nil)
             (decode-json-from-string input)))
        (is-false (#-cmu identity #+cmu symbol-package
                   (class-name (class-of (decode-json-from-string input)))))
        (is (equal-objects-p
             (decode-json-from-string "{ }")
             (json:make-object nil nil)))))))

(defclass foo () ((bar :initarg :bar) (baz :initarg :baz)))
(defclass goo () ((quux :initarg :quux :initform 933)))
(defclass frob (foo goo) ())
(defclass p-frob (frob) ((prototype :reader prototype)))

#+cl-json-clos
(test json-object-with-prototype
  (let ((*json-symbols-package* (find-package :keyword))
        (*prototype-name* 'prototype)
        (input "{\"bar\": 46,
                 \"xyzzy\": true,
                 \"quux\": 98,
                 \"prototype\": ~A}"))
    (with-decoder-simple-list-semantics
      (is (equalp
           (decode-json-from-string
            (format nil input "{\"lispPackage\":\"jsonTest\",
                                \"lispClass\":\"frob\"}"))
           '((:bar . 46) (:xyzzy . t) (:quux . 98)
             (:prototype . ((:lisp-package . "jsonTest")
                            (:lisp-class . "frob")))))))
    (with-decoder-simple-clos-semantics
      (is (equal-objects-p
           (make-instance 'frob :bar 46 :quux 98)
           (decode-json-from-string
            (format nil input "{\"lispPackage\":\"jsonTest\",
                                \"lispClass\":\"frob\"}"))))
      (finalize-inheritance (find-class 'foo))
      (finalize-inheritance (find-class 'goo))
      (is (equal-objects-p
           (json:make-object '((bar . 46) (xyzzy . t) (quux . 98))
                             nil '(foo goo))
           (decode-json-from-string
            (format nil input "{\"lispSuperclasses\": [\"foo\", \"goo\"],
                                \"lispPackage\":\"jsonTest\"}"))))
      (is (typep
           (prototype (decode-json-from-string
                       (format nil input "{\"lispPackage\":\"jsonTest\",
                                           \"lispClass\":\"pFrob\"}")))
           'prototype))
      (is (equalp
           '((bar . 46) (xyzzy . t) (quux . 98))
           (decode-json-from-string
            (format nil input "{\"lispClass\": \"cons\",
                                \"lispPackage\":\"jsonTest\"}"))))
      (is (loop with ht = (decode-json-from-string
                           (format nil input "{\"lispClass\": \"hashTable\",
                                               \"lispPackage\":\"jsonTest\"}"))
             initially (if (/= (hash-table-count ht) 3) (return nil))
             for k being each hash-key of ht
             using (hash-value v)
             always (case k
                      (bar (eql v 46))
                      (xyzzy (eql v t))
                      (quux (eql v 98))
                      (t nil)))))))


(test json-object-factory
  (let (*ht* *key* obj
        (*json-symbols-package* 'json-test))
    (declare (special *ht* *key*))
    (json:bind-custom-vars
        (:beginning-of-object #'(lambda () (setq *ht* (make-hash-table)))
         :object-key #'(lambda (key)
                         (setq *key* (json-intern (camel-case-to-lisp key))))
         :object-value #'(lambda (value) (setf (gethash *key* *ht*) value))
         :end-of-object #'(lambda () *ht*)
         :object-scope '(*ht* *key*))
      (setf obj (decode-json-from-string " { \"hello\" : \"hej\" ,
                       \"hi\" : \"tjena\"
                     }"))
      (is (string= "hej" (gethash 'hello obj)))
      (is (string= "tjena" (gethash 'hi obj))))))

(test set-list-decoder-semantics
  (with-shadowed-custom-vars
    (let ((tricky-json "{\"start_XPos\":98,\"start_YPos\":4}")
          (*json-symbols-package* (find-package :keyword)))
      (set-decoder-simple-list-semantics)
      (is (equal '((:START--*X-POS . 98) (:START--*Y-POS . 4))
                 (decode-json-from-string tricky-json))))))

(test custom-identifier-name-to-key
  "Interns of many unique symbols could potentially use a lot of memory.
An attack could exploit this by submitting something that is passed
through cl-json that has many very large, unique symbols. See the
safe-symbols-parsing function here for a cure."
  (with-decoder-simple-list-semantics
      (flet ((safe-symbols-parsing (name)
               (or (find-symbol name *json-symbols-package*)
                   (error "unknown symbols not allowed"))))
        (let ((good-symbols "{\"car\":1,\"cdr\":2}")
              (bad-symbols "{\"could-be\":1,\"a-denial-of-service-attack\":2}")
              (*json-symbols-package* (find-package :cl))
              (*identifier-name-to-key* #'safe-symbols-parsing))
          (is (equal '((car . 1) (cdr . 2))
                     (decode-json-from-string good-symbols)))
          (signals error (decode-json-from-string bad-symbols))))))

(test safe-json-intern
  (with-decoder-simple-list-semantics
      (let ((good-symbols "{\"car\":1,\"cdr\":2}")
            (bad-symbols "{\"could-be\":1,\"a-denial-of-service-attack\":2}")
            (*json-symbols-package* (find-package :cl))
            (*identifier-name-to-key* #'safe-json-intern))
        (is (equal '((car . 1) (cdr . 2))
                   (decode-json-from-string good-symbols)))
        (signals unknown-symbol-error (decode-json-from-string bad-symbols)))))


(test json-object-camel-case
  (with-decoder-simple-list-semantics
      (let ((*json-symbols-package* (find-package :keyword)))
        (is (equalp '((:hello-key . "hej")
                      (:*hi-starts-with-upper-case . "tjena")
                      (:+json+-all-capitals . "totae majusculae")
                      (:+two-words+ . "duo verba")
                      (:camel-case--*mixed--+4-parts+ . "partes miscella quatuor"))
                    (decode-json-from-string " { \"helloKey\" : \"hej\" ,
                 \"HiStartsWithUpperCase\" : \"tjena\",
                 \"JSONAllCapitals\": \"totae majusculae\",
                 \"TWO_WORDS\": \"duo verba\",
                 \"camelCase_Mixed_4_PARTS\": \"partes miscella quatuor\"
               }"))))))

(test json-object-simplified-camel-case
  ;; Compare this with json-object-camel-case above
  (with-decoder-simple-list-semantics
      (let ((*json-symbols-package* (find-package :keyword))
            (*json-identifier-name-to-lisp* #'simplified-camel-case-to-lisp))
        (is (equalp '((:hello-key . "hej")
                      (:hi-starts-with-upper-case . "tjena")
                      (:jsonall-capitals . "totae majusculae")
                      (:two_words . "duo verba")
                      (:camel-case_mixed_4_parts . "partes miscella quatuor"))
                    (decode-json-from-string " { \"helloKey\" : \"hej\" ,
                 \"HiStartsWithUpperCase\" : \"tjena\",
                 \"JSONAllCapitals\": \"totae majusculae\",
                 \"TWO_WORDS\": \"duo verba\",
                 \"camelCase_Mixed_4_PARTS\": \"partes miscella quatuor\"
               }"))))))

(defmacro with-fp-overflow-handler (handler-expr &body body)
  (let ((err (gensym)))
    `(handler-bind ((floating-point-overflow
                     (lambda (,err)
                       (declare (ignore ,err))
                       ,handler-expr)))
       ,@body)))

(defmacro with-no-char-handler (handler-expr &body body)
  (let ((err (gensym)))
    `(handler-bind ((no-char-for-code
                     (lambda (,err)
                       (declare (ignore ,err))
                       ,handler-expr)))
       ,@body)))

(test json-number
  (is (= 100 (decode-json-from-string "100")))
  (is (= 10.01 (decode-json-from-string "10.01")))
  (is (= -2.3 (decode-json-from-string "-2.3")))
  (is (= -2.3e3 (decode-json-from-string "-2.3e3")))
  (is (= -3e4 (decode-json-from-string "-3e4")))
  (is (= 3e4 (decode-json-from-string "3e4")))
  (let ((*read-default-float-format* 'double-float))
    (is (= 2d40 (decode-json-from-string "2e40"))))
  #-(or (and sbcl darwin) (and allegro macosx))
  (is (equalp "BIG:2e444"
              (with-fp-overflow-handler
                  (invoke-restart 'bignumber-string "BIG:")
                (decode-json-from-string "2e444"))))
  #-(or (and sbcl darwin) (and allegro macosx))
  (is (= (* 2 (expt 10 444))
         (with-fp-overflow-handler
             (invoke-restart 'rational-approximation)
           (decode-json-from-string "2e444"))))
  ;; In SBCL on Darwin, constructing the float from parts by explicit
  ;; operations yields #.SB-EXT:SINGLE-FLOAT-POSITIVE-INFINITY.
  #+(and sbcl darwin)
  (is (= (* 2.0 (expt 10.0 444))
         (decode-json-from-string "2e444"))))


(defparameter *json-test-files-path*
  (asdf:system-relative-pathname "cl-json/test" "t/"))

(defun test-file (name)
  (make-pathname :name name :type "json" :defaults *json-test-files-path*))

(defun decode-file (path)
  (with-open-file (stream path :direction :input)
    (with-fp-overflow-handler (invoke-restart 'placeholder :infty)
      (with-no-char-handler (invoke-restart 'substitute-char #\?)
        (decode-json-strict stream)))))

;; All test files are taken from http://www.crockford.com/JSON/JSON_checker/test/

(test pass-1
  (decode-file (test-file "pass1")))

(test pass-2
  (decode-file (test-file "pass2")))

(test pass-3
  (decode-file (test-file "pass3")))

(defparameter *ignore-tests* '(
  1 ; says: "A JSON payload should be an object or array, not a string.", but who cares?
  7 ; says: ["Comma after the close"],  ,but decode-file stops parsing after one object has been retrieved
  8 ; says ["Extra close"]] ,but decode-file stops parsing after one object has been retrieved
  10; says {"Extra value after close": true} "misplaced quoted value", but
    ;   decode-file stops parsing after one object has been retrieved
  18; says [[[[[[[[[[[[[[[[[[[["Too deep"]]]]]]]]]]]]]]]]]]]], but there is no formal limit
))

(defparameter *ignore-tests-strict* '(
  18; says [[[[[[[[[[[[[[[[[[[["Too deep"]]]]]]]]]]]]]]]]]]]], but there is no formal limit
))

(test fail-files
  (dotimes (x 24)
    (if (member x *ignore-tests-strict*)
        (is-true t)
        (5am:signals error
          (decode-file (test-file (format nil "fail~a" x)))))))

(defun contents-of-file(file)
  (with-open-file (stream file :direction :input)
     (let ((s (make-string (file-length stream))))
      (read-sequence s stream)
      s)))

(test decoder-performance
  (let* ((json-string (contents-of-file (test-file "pass1")))
         (chars (length json-string))
         (count 1000))
    (format t "Decoding ~a varying chars from memory ~a times." chars count)
    (time
     (dotimes (x count)
       (let ((discard-soon
              (with-fp-overflow-handler (invoke-restart 'placeholder :infty)
                (with-no-char-handler (invoke-restart 'substitute-char #\?)
                  (decode-json-from-string json-string)))))
         (funcall #'identity discard-soon))))));Do something so the compiler don't optimize too much

(test decoder-performance-with-simplified-camel-case
  (let* ((json-string (contents-of-file (test-file "pass1")))
         (chars (length json-string))
         (count 1000))
    (format t "Decoding ~a varying chars from memory ~a times." chars count)
    (time
     (with-shadowed-custom-vars
         (let ((*json-identifier-name-to-lisp* #'simplified-camel-case-to-lisp))
           (dotimes (x count)
             (let ((discard-soon
                    (with-fp-overflow-handler (invoke-restart 'placeholder :infty)
                      (with-no-char-handler (invoke-restart 'substitute-char #\?)
                        (decode-json-from-string json-string)))))
               (funcall #'identity discard-soon)))))))) ;Do something so the compiler don't optimize too much

;;#+when-u-want-profiling
;;(defun profile-decoder-performance()
;;  #+sbcl
;;  (progn
;;    (let ((json-string (contents-of-file (test-file "pass1")))
;;          (count 10))
;;      (format t "Parsing test-file pass1 from memory ~a times." count)
;;      (sb-sprof:with-profiling ()
;;        (dotimes (x count)
;;          (let ((discard-soon (decode-json-from-string json-string)))
;;            (funcall #'identity discard-soon))))
;;      (sb-sprof:report)
;;      nil)))

(test non-strict-json
   (let ((not-strictly-valid "\"right\\'s of man\""))
     (5am:signals json:json-syntax-error
       (json:decode-json-from-string not-strictly-valid))
     (let ((*use-strict-json-rules* nil))
       (declare (special *use-strict-json-rules*))
       (is (string= (json:decode-json-from-string not-strictly-valid)
                    "right's of man")))))

#+cl-json-clos
(defun first-bound-slot-name (x)
  (loop for slot in (json::class-slots (class-of x))
     for slot-name = (json::slot-definition-name slot)
     if (slot-boundp x slot-name)
       return slot-name))

(test test*json-symbols-package*
  (let ((*json-symbols-package* nil)
        (*package* (find-package :json-test))
        x)
    (with-decoder-simple-list-semantics
      (setf x (decode-json-from-string "{\"x\":1}"))
      (is (equal (symbol-package (caar x))
                 (find-package :json-test))))
    #+cl-json-clos
    (with-decoder-simple-clos-semantics
      (setf x (decode-json-from-string "{\"x\":1}"))
      (is (equal (symbol-package (first-bound-slot-name x))
                 (find-package :json-test)))))
  (let ((*json-symbols-package* (find-package :cl-user))
        x)
    (with-decoder-simple-list-semantics
      (setf x (decode-json-from-string "{\"x\":1}"))
      (is (equal (symbol-package (caar x))
                 (find-package :cl-user))))
    #+cl-json-clos
    (with-decoder-simple-clos-semantics
      (setf x (decode-json-from-string "{\"x\":1}"))
      (is (equal (symbol-package (first-bound-slot-name x))
                 (find-package :cl-user)))))
  (when (eq *json-symbols-package* (find-package :keyword))
    (let (x)
      (with-decoder-simple-list-semantics
        (setf x (decode-json-from-string "{\"x\":1}"))
        (is (equal (symbol-package (caar x))
                   (find-package :keyword))))
      #+(and cl-json-clos
             (not allegro) ; seems like allegro doesn't, either.
             (not cmu))   ; CMUCL does not allow keywords as slot names
      (with-decoder-simple-clos-semantics
        (setf x (decode-json-from-string "{\"x\":1}"))
        (is (equal (symbol-package (first-bound-slot-name x))
                   (find-package :keyword)))))))

