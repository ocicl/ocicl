(cl:in-package #:eclector.concrete-syntax-tree.test)

(in-suite :eclector.concrete-syntax-tree)

;;; Smoke test

(test read/smoke
  "Smoke test for the READ function."
  (do-stream-input-cases ((input length) eof-error expected-raw
                          &optional expected-location
                                    (expected-position length)
                                    (expected-length 1))
    (flet ((do-it ()
             (with-stream (stream)
               (eclector.concrete-syntax-tree:read stream eof-error :eof))))
      (error-case (input expected-raw expected-position expected-length)
        (error (do-it))
        (:eof (is (eq :eof (do-it))))
        (t
         (multiple-value-bind (result orphan-results position) (do-it)
           (declare (ignore orphan-results))
           ;; CST result and its raw content.
           (is (typep result 'cst:cst))
           (is-true (valid-cst-parse-result-p
                     eclector.concrete-syntax-tree::*cst-client* result)
                    "~@<For input ~S, the result CST ~S it not valid~@:>"
                    input result)
           (is-consistent-with-raw result)
           (expect "raw result" (equal* expected-raw (cst:raw result)))
           ;; Expected source location.
           (expect "source location" (equal expected-location (cst:source result)))
           ;; Consumed all input.
           (expect "position" (eql length position))))))
    '(;; End of input
      (""                    t   eclector.reader:end-of-file nil 0 0)
      (""                    nil :eof)
      ("; comment"           t   eclector.reader:end-of-file nil 9 0)
      ("; comment"           nil :eof)
      ;; Actually reading something
      ("(cons 1 2)"          t   (cons 1 2)          ( 0 . 10))
      ("#+(or) `1 2"         t   2                   (10 . 11))
      ("#|comment|# 1"       t   1                   (12 . 13))
      ("; comment~%1"        t   1                   (10 . 11))
      ("(a . 2)"             t   (a . 2)             ( 0 .  7))
      ("(#1=1 #1#)"          t   (#1=1 #1#)          ( 0 . 10))
      ("#1=(#1#)"            t   #2=(#2#)            ( 3 .  8))
      ("#1=(1 #2=(#1# #2#))" t   #3=(1 #4=(#3# #4#)) ( 3 . 19)))))

(test read-preserving-whitespace/smoke
  "Smoke test for the READ-PRESERVING-WHITESPACE function."
  (do-stream-input-cases ((input length) eof-error-p eof-value
                          expected-result &optional (expected-position length)
                                                    (expected-length 1))
      (flet ((do-it ()
               (with-stream (stream)
                 (eclector.concrete-syntax-tree:read-preserving-whitespace
                  stream eof-error-p eof-value))))
        (error-case (input expected-result expected-position expected-length)
          (error (do-it))
          (:eof
           (multiple-value-bind (result orphan-results position) (do-it)
             (expect "result"         (eq :eof               result))
             (expect "orphan results" (eq '()                orphan-results))
             (expect "position"       (eql expected-position position))))
          (t
           (multiple-value-bind (result orphan-results position) (do-it)
             (is (typep result 'cst:cst))
             (is-true (valid-cst-parse-result-p
                       eclector.concrete-syntax-tree::*cst-client* result)
                      "~@<For input ~S, the result CST ~S is not valid~@:>"
                      input result)
             (is-consistent-with-raw result)
             (expect "raw results"    (equal expected-result (cst:raw result)))
             (expect "orphan results" (eq  '()               orphan-results))
             (expect "position"       (eql expected-position position))))))
    '(;; End of input
      (""        t   nil  eclector.reader:end-of-file 0 0)
      (""        nil :eof :eof)
      ;; Valid
      (":foo"    t   nil  :foo)
      (":foo "   t   nil  :foo                        4)
      (":foo  "  t   nil  :foo                        4)
      (":foo  1" t   nil  :foo                        4))))

(test read-from-string/smoke
  "Smoke test for the READ-FROM-STRING function."
  (do-input-cases ((input length) args
                   expected-value &optional (expected-position length)
                                            (expected-length 1))
      (flet ((do-it ()
               (apply #'eclector.concrete-syntax-tree:read-from-string
                      input args)))
        (error-case (input expected-value expected-position expected-length)
          (error (do-it))
          (:eof
           (multiple-value-bind (result position) (do-it)
             (expect "result"   (eq :eof               result))
             (expect "position" (eql expected-position position))))
          (t
           (multiple-value-bind (result position) (do-it)
             (is (typep result 'cst:cst))
             (is-true (valid-cst-parse-result-p
                       eclector.concrete-syntax-tree::*cst-client* result)
                      "~@<For input ~S, the result CST ~S is not valid~@:>"
                      input result)
             (is-consistent-with-raw result)
             (expect "raw result" (equal expected-value (cst:raw result)))
             (expect "position"   (eql expected-position position))))))
    '(;; End of input
      (""         ()                               eclector.reader:end-of-file  0 0)
      (""         (nil :eof)                       :eof                         0)
      ;; Valid
      (":foo 1 2" ()                               :foo                         5)
      ;; Start and end
      (":foo 1 2" (t nil :start 4)                 1                            7)
      (":foo 1 2" (t nil :end 3)                   :fo                          3)
      ;; Preserving whitespace
      (":foo 1"   (t nil :preserve-whitespace nil) :foo                         5)
      (":foo 1 "  (t nil :preserve-whitespace nil) :foo                         5)
      (":foo 1  " (t nil :preserve-whitespace nil) :foo                         5)
      (":foo 1 2" (t nil :preserve-whitespace nil) :foo                         5)
      ("#*11 "    (t nil :preserve-whitespace nil) #*11                         5)
      ("#1*1 "    (t nil :preserve-whitespace nil) #1*1                         5)

      (":foo 1"   (t nil :preserve-whitespace t)   :foo                         4)
      (":foo 1 "  (t nil :preserve-whitespace t)   :foo                         4)
      (":foo 1  " (t nil :preserve-whitespace t)   :foo                         4)
      (":foo 1 2" (t nil :preserve-whitespace t)   :foo                         4)
      ("#*11 "    (t nil :preserve-whitespace t)   #*11                         4)
      ("#1*1 "    (t nil :preserve-whitespace t)   #1*1                         4))))

;;; Source locations

(defun check-source-locations (cst expected-source-locations)
  (labels ((check (cst expected)
             (destructuring-bind (expected-location . children) expected
               (is (equal expected-location (cst:source cst)))
               (cond ((not children)
                      (is-true (cst:atom cst)))
                     ((not (cst:consp cst))
                      (fail "Expected CONS CST, but got ~S" cst))
                     (t
                      (check (cst:first cst) (first children))
                      (check (cst:rest cst) (rest children)))))))
    (check cst expected-source-locations)))

(test read/source-locations
  "Test source locations assigned by READ."
  (do-stream-input-cases (() expected)
      (let ((result (with-stream (stream)
                      (eclector.concrete-syntax-tree:read stream))))
        (check-source-locations result expected))
    (macrolet ((scons ((&optional start end) &optional car cdr)
                 `(cons ,(if start `(cons ,start ,end) 'nil)
                        ,(if car `(cons ,car ,cdr) 'nil))))
      `(;; Sanity check
        ("(1 2 3)"      ,(scons (0 7)
                                (scons (1 2)) ; 1
                                (scons ()
                                       (scons (3 4)) ; 2
                                       (scons ()
                                              (scons (5 6)) ; 3
                                              (scons ())))))
        ;; EQL children
        ("(1 1)"        ,(scons (0 5)
                                (scons (1 2)) ; first 1
                                (scons ()
                                       (scons (3 4)) ; second 1
                                       (scons ()))))
        ;; Simple reader macro
        ("#.(list 1 2)" ,(scons (0 12)
                                (scons (8 9)) ; 1
                                (scons ()
                                       (scons (10 11)) ; 2
                                       (scons ()))))
        ;; Nested reader macros
        ("#.(list* 1 '#.(list 2))" ,(scons (0 23)
                                           (scons (9 10)) ; 1
                                           (scons (12 22) ; #.(...)
                                                  (scons (20 21)) ; 2
                                                  (scons ()))))
        ;; Heuristic fails here
        ("#.(list 1 1)" ,(scons (0 12)
                                (scons (10 11)) ; second 1 (arbitrarily)
                                (scons ()
                                       (scons (10 11)) ; second 1 (arbitrarily)
                                       (scons ()))))
        ;; Quote injects expressions
        ("'(123)"       ,(scons (0 6)
                                (scons ()) ; quote, no source
                                (scons () ; ((123)), no source
                                       (scons (1 6) ; (123)
                                              (scons (2 5)) ; 123
                                              (scons ())) ; nil
                                       (scons ())))))))) ; nil

;;; Identity

(test read/identity
  "Test node identity properties in constructed CSTs."
  ;; Test special case for proper list.
  (let* ((result (eclector.concrete-syntax-tree:read-from-string "(2 2 2)"))
         (first  (cst:first result))
         (second (cst:first (cst:rest result)))
         (third  (cst:first (cst:rest (cst:rest result)))))
    (is (eql 2 (cst:raw first)))
    (is (eql 2 (cst:raw second)))
    (is (eql 2 (cst:raw third)))
    (is (notany #'eq (list first second third) (list second third first))))
  (let* ((result (eclector.concrete-syntax-tree:read-from-string
                  "(#1=2 2 #1#)"))
         (first  (cst:first result))
         (second (cst:first (cst:rest result)))
         (third  (cst:first (cst:rest (cst:rest result)))))
    (is (eql 2 (cst:raw first)))
    (is (eql 2 (cst:raw second)))
    (is (eql 2 (cst:raw third)))
    (is (not (eq first second)))
    (is (eq first third)))
  ;; Test special case for dotted list.
  (let* ((result (eclector.concrete-syntax-tree:read-from-string "(2 . 2)"))
         (car    (cst:first result))
         (cdr    (cst:rest result)))
    (is (eql 2 (cst:raw car)))
    (is (eql 2 (cst:raw cdr)))
    (is (not (eq car cdr))))
  (let* ((result (eclector.concrete-syntax-tree:read-from-string
                  "(#1=2 . #1#)"))
         (car    (cst:first result))
         (cdr    (cst:rest result)))
    (is (eql 2 (cst:raw car)))
    (is (eql 2 (cst:raw cdr)))
    (is (eq car cdr))))

;;; Custom client

(defclass custom-client (eclector.concrete-syntax-tree:cst-client)
  ())

(defmethod eclector.base:source-position
    ((client custom-client) (stream t))
  (- (call-next-method)))

(defmethod eclector.base:make-source-range
    ((client custom-client) (start t) (end t))
  (vector start end))

(test read-cst/custom-client
  "Test using a custom client with READ."
  (let* ((client (make-instance 'custom-client))
         (result (with-input-from-string (stream "#||# 1")
                   (let ((eclector.reader:*client* client))
                     (eclector.concrete-syntax-tree:read stream)))))
    (is-true (valid-cst-parse-result-p client result))
    (is-consistent-with-raw result)
    (is (equalp #(-5 -6) (cst:source result)))))

;;; Skipped input

(defclass skipped-input-recording-client
    (eclector.concrete-syntax-tree:cst-client)
  ((%skipped :accessor skipped
             :initform '())))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client skipped-input-recording-client)
     (stream t)
     (reason t)
     (children t)
     (source t))
  (alexandria:appendf (skipped client) (list (list reason source))))

(test make-skipped-input-result/smoke
  "Smoke test for the MAKE-SKIPPED-INPUT-RESULT function."
  (do-stream-input-cases ((input length) read-suppress expected)
    (flet ((do-it ()
             (let ((client
                     (make-instance 'skipped-input-recording-client)))
               (with-stream (stream)
                 (let ((eclector.reader:*client* client)
                       (*read-suppress* read-suppress))
                   (eclector.concrete-syntax-tree:read stream))
                 (skipped client)))))
      (multiple-value-bind (skipped position) (do-it)
        (expect "position"       (eql   length   position))
        (expect "skipped inputs" (equal expected skipped))))
    '(;; No skipping
      ("1"             nil ())
      ;; Comments
      ("#||# 1"        nil ((:block-comment (0 . 4))))
      ("; test~% 1"    nil (((:line-comment . 1) (0 . 7))))
      (";; test~% 1"   nil (((:line-comment . 2) (0 . 8))))
      (";;; test~% 1"  nil (((:line-comment . 3) (0 . 9))))
      ;; Reader conditionals
      ("#+(or) 1 2"    nil ((*read-suppress* (7 . 8))
                            ((:sharpsign-plus . (:or)) (0 . 8))))
      ;; Order of skipped inputs
      ("#|1|# #|2|# 3" nil ((:block-comment (0 . 5))
                            (:block-comment (6 . 11))))
      ("#|1|# #|2|# 3" t   ((:block-comment (0 . 5))
                            (:block-comment (6 . 11))
                            (*read-suppress* (12 . 13))))
      ;; Non-toplevel suppressed objects
      ("(nil)"         t   ((*read-suppress* (1 . 4))
                            (*read-suppress* (0 . 5))))
      ("#|1|# (nil)"   t   ((:block-comment (0 . 5))
                            (*read-suppress* (7 . 10))
                            (*read-suppress* (6 . 11)))))))

;;; Regressions and other tests

(test make-expression-result/long-list
  "The method on MAKE-EXPRESSION-RESULT used to blow the stack for
long lists."
  (let* ((length 200000)
         (input (format nil "(~{~A~^ ~})" (alexandria:iota length)))
         (result (with-input-from-string (stream input)
                   (eclector.concrete-syntax-tree:read stream)))
         (actual-length (loop for i from 0
                              for cst = result then (cst:rest cst)
                              while (cst:consp cst)
                              do (let ((raw (cst:raw (cst:first cst))))
                                   (unless (eql i raw)
                                     (fail "~@<Mismatch at index ~:D: ~
                                            ~S is not ~S to ~S~@:>"
                                           i raw 'eql i)))
                              count 1)))
    (is (= length actual-length))))
