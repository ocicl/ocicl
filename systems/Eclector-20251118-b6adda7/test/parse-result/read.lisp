(cl:in-package #:eclector.parse-result.test)

(in-suite :eclector.parse-result)

;;; The {PARSE,ATOM,CONS}-RESULT classes and the RESULTIFY function
;;; simulate what a client might do to represent parse results.

(defclass parse-result ()
  ((%raw    :initarg :raw
            :reader raw)
   (%source :initarg :source
            :reader source)))

(defclass atom-result (parse-result)
  ())

(defclass cons-result (parse-result)
  ((%first-child :initarg :first
                 :reader first-child)
   (%rest-child  :initarg :rest
                 :reader rest-child)))

(defun resultify (raw results &optional source)
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((rec (raw-rest result-rest &optional source)
               (cond ((and (typep result-rest 'parse-result)
                           (eq raw-rest (raw result-rest)))
                      result-rest)
                     ((atom raw-rest)
                      (make-instance 'atom-result :raw raw-rest :source nil))
                     (t
                      (or (gethash raw-rest seen)
                          (let ((result (make-instance 'cons-result :raw raw-rest
                                                                    :source source)))
                            (setf (gethash raw-rest seen) result)
                            (reinitialize-instance
                             result :first (rec (first raw-rest)
                                                (when (consp result-rest)
                                                  (first result-rest)))
                                    :rest (rec (rest raw-rest)
                                               (when (consp result-rest)
                                                 (rest result-rest))))))))))
      (rec raw results source))))

(defclass simple-result-client (eclector.parse-result:parse-result-client)
  ())

(defmethod eclector.parse-result:make-expression-result
    ((client simple-result-client) (result cons) (children t) (source t))
  (resultify result children source))

(defmethod eclector.parse-result:make-expression-result
    ((client simple-result-client) (result t) (children t) (source t))
  (make-instance 'atom-result :raw result :source source))

(defmethod eclector.reader:fixup ((client simple-result-client)
                                  (object atom-result)
                                  (traversal-state t))
  nil) ; Simplified, things like arrays and structures need fixup.

(defmethod eclector.reader:fixup ((client simple-result-client)
                                  (object cons-result)
                                  (traversal-state t))
  (macrolet ((fixup-place (place)
               `(let* ((current-value ,place)
                       (current-raw-value (raw current-value)))
                  (eclector.reader:fixup-case (client current-raw-value)
                    (()
                     (eclector.reader:fixup client current-value traversal-state))
                    (()
                     (let ((reference (eclector.parse-result:make-reference
                                       current-raw-value))
                           (source (source current-value)))
                       (setf ,place (eclector.parse-result:make-expression-result
                                     client reference '() source))))))))
    (fixup-place (slot-value object '%first-child))
    (fixup-place (slot-value object '%rest-child))))

;;; A client that stores all result information in lists.

(defclass list-result-client (eclector.parse-result:parse-result-client)
  ())

(defmethod eclector.parse-result:make-expression-result
    ((client list-result-client) (result t) (children t) (source t))
  (list :result result :children children :source source))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client list-result-client) (stream t) (reason t) (children t) (source t))
  (list :reason reason :children children :source source))

;;; Smoke test with parse results

(test read/smoke
  "Smoke test for the READ function."
  (do-stream-input-cases ((input length) eof-error expected-raw
                          &optional expected-location
                                    (expected-position length)
                                    (expected-length 1))
    (flet ((do-it ()
             (with-stream (stream)
               (eclector.parse-result:read
                (make-instance 'simple-result-client) stream eof-error :eof))))
      (error-case (input expected-raw expected-position expected-length)
        (error (do-it))
        (:eof
         (multiple-value-bind (result orphan-results position) (do-it)
           (expect "result"         (eq    expected-raw      result))
           (expect "orphan results" (equal '()               orphan-results))
           (expect "position"       (eql   expected-position position))))
        (t
         (multiple-value-bind (result orphan-results position) (do-it)
           (is (typep result 'parse-result))
           (expect "raw result"      (equal* expected-raw      (raw result)))
           (expect "source location" (equal  expected-location (source result)))
           (expect "orphan results"  (equal  '()               orphan-results))
           (expect "position"        (eql    expected-position position))))))
    '(;; End of input
      (""                    t   eclector.reader:end-of-file nil 0 0)
      (""                    nil :eof)
      ("; comment"           t   eclector.reader:end-of-file nil 9 0)
      ("; comment"           nil :eof)
      ;; Actually reading something
      ("1"                   t   1                   ( 0 .  1))
      (" 1"                  t   1                   ( 1 .  2))
      ("1 "                  t   1                   ( 0 .  1))
      ("1 2"                 t   1                   ( 0 .  1) 2)
      ("(cons 1 2)"          t   (cons 1 2)          ( 0 . 10))
      ("#+(or) `1 2"         t   2                   (10 . 11))
      ("#|comment|# 1"       t   1                   (12 . 13))
      ("; comment~%1"        t   1                   (10 . 11))
      ("(a . 2)"             t   (a . 2)             ( 0 .  7))
      ("(#1=1 #1#)"          t   (#1=1 #1#)          ( 0 . 10))
      ("#1=(#1#)"            t   #2=(#2#)            ( 3 .  8))
      ("#1=(1 #2=(#1# #2#))" t   #3=(1 #4=(#3# #4#)) ( 3 . 19)))))

(defclass eof-value-client (simple-result-client) ())

(defmethod eclector.parse-result:make-skipped-input-result
    ((client eof-value-client) (stream t) (reason t) (children t) (source t))
  (list reason source))

(test read/eof-value
  "Ensure that different eof-values work for READing into parse results."
  (do-stream-input-cases ((input length) eof-value expected-raw
                          &optional (expected-location (cons 0 length))
                                    (expected-position length)
                                    (expected-orphan-results '()))
    (multiple-value-bind (parse-result orphan-results position)
        (with-stream (stream)
          (eclector.parse-result:read
           (make-instance 'eof-value-client) stream nil eof-value))
      (case expected-raw
        (:eof
         (is (eq eof-value parse-result)))
        (t
         (is-true (typep parse-result 'parse-result))
         (is (eql   expected-raw      (raw parse-result)))
         (is (equal expected-location (source parse-result)))))
      (is (eql   expected-position       position))
      (is (equal expected-orphan-results orphan-results)))
    '(;; End of input
      (""             nil   :eof)
      (""             #:eof :eof)
      ("#+(or) t"     nil   :eof nil       8 (((:sharpsign-plus :or) (0 . 8))))
      ("#+(or) t"     #:eof :eof nil       8 (((:sharpsign-plus :or) (0 . 8))))
      ("#|foo|#"      nil   :eof nil       7 ((:block-comment (0 . 7))))
      ("#|foo|#"      #:eof :eof nil       7 ((:block-comment (0 . 7))))
      ;; Valid
      ("nil"          nil   nil)
      ("nil"          #:eof nil)
      ("#+(or) t nil" nil   nil  (9 . 12) 12 (((:sharpsign-plus :or) (0 . 8))))
      ("#+(or) t nil" #:eof nil  (9 . 12) 12 (((:sharpsign-plus :or) (0 . 8)))))))

(test read-preserving-whitespace/smoke
  "Smoke test for the READ-PRESERVING-WHITESPACE function."
  (do-stream-input-cases ((input length) eof-error-p eof-value expected-raw
                          &optional expected-location
                                    (expected-position length)
                                    (expected-length 1))
      (flet ((do-it ()
               (with-stream (stream)
                 (eclector.parse-result:read-preserving-whitespace
                  (make-instance 'simple-result-client)
                  stream eof-error-p eof-value))))
        (error-case (input expected-raw expected-position expected-length)
          (error (do-it))
          (:eof
           (multiple-value-bind (result orphan-results position) (do-it)
             (expect "result"         (eq    expected-raw      result))
             (expect "orphan results" (equal '()               orphan-results))
             (expect "position"       (eql   expected-position position))))
          (t
           (multiple-value-bind (result orphan-results position) (do-it)
             (is (typep result 'parse-result))
             (expect "raw result"      (equal expected-raw      (raw result)))
             (expect "source location" (equal expected-location (source result)))
             (expect "orphan results"  (equal '()               orphan-results))
             (expect "position"        (eql   expected-position position))))))
    '(;; End of input
      (""        t   nil  eclector.reader:end-of-file nil     0 0)
      (""        nil :eof :eof                        nil     0)
      ;; Valid
      (":foo"    t   nil  :foo                        (0 . 4) 4)
      (":foo "   t   nil  :foo                        (0 . 4) 4)
      (":foo  "  t   nil  :foo                        (0 . 4) 4)
      (":foo  1" t   nil  :foo                        (0 . 4) 4)
      ("#*11 "   t   nil  #*11                        (0 . 4) 4)
      ("#1*1 "   t   nil  #1*1                        (0 . 4) 4))))

(test read-from-string/smoke
  "Smoke test for the READ-FROM-STRING function."
  (do-input-cases ((input length) args
                   expected-raw &optional expected-location
                                          (expected-position length)
                                          (expected-length 1))
    (flet ((do-it ()
             (apply #'eclector.parse-result:read-from-string
                    (make-instance 'simple-result-client) input args)))
      (error-case (input expected-raw expected-position expected-length)
        (error (do-it))
        (:eof
         (multiple-value-bind (result position orphan-results) (do-it)
           (expect "result"          (eq    expected-raw      result))
           (expect "orphan results"  (equal '()               orphan-results))
           (expect "position"        (eql   expected-position position))))
        (t
         (multiple-value-bind (result position orphan-results) (do-it)
           (expect "value"           (equal expected-raw      (raw result)))
           (expect "source location" (equal expected-location (source result)))
           (expect "orphan results"  (equal '()               orphan-results))
           (expect "position"        (eql   expected-position position))))))
    '(;; End of input
      (""         ()                               eclector.reader:end-of-file nil 0 0)
      (""         (nil :eof)                       :eof)
      ;; Valid
      (":foo 1 2" ()                               :foo (0 . 4) 5)
      ;; Start and end
      ;;
      ;; Implementations do not agree regarding what
      ;;
      ;;   (with-input-from-string (stream STRING :start START)
      ;;     (file-position stream))
      ;;
      ;; should return. So on some implementations, the source
      ;; position will be (5 . 6) instead of (1 . 2).
      (":foo 1 2" (t nil :start 4)                 1    #-ecl (1 . 2)
                                                        #+ecl (5 . 6)
                                                        7)
      (":foo 1 2" (t nil :end 3)                   :fo  (0 . 3) 3)
      ;; Preserving whitespace
      (":foo 1"   (t nil :preserve-whitespace nil) :foo (0 . 4) 5)
      (":foo 1  " (t nil :preserve-whitespace nil) :foo (0 . 4) 5)
      (":foo 1 2" (t nil :preserve-whitespace nil) :foo (0 . 4) 5)
      ("#*11 "    (t nil :preserve-whitespace nil) #*11 (0 . 4) 5)
      ("#1*1 "    (t nil :preserve-whitespace nil) #1*1 (0 . 4) 5)

      (":foo 1"   (t nil :preserve-whitespace t)   :foo (0 . 4) 4)
      (":foo 1  " (t nil :preserve-whitespace t)   :foo (0 . 4) 4)
      (":foo 1 2" (t nil :preserve-whitespace t)   :foo (0 . 4) 4)
      ("#*11 "    (t nil :preserve-whitespace t)   #*11 (0 . 4) 4)
      ("#1*1 "    (t nil :preserve-whitespace t)   #1*1 (0 . 4) 4))))

(test read-maybe-nothing/smoke
  "Smoke test for the READ-MAYBE-NOTHING function."
  (do-stream-input-cases ((input length) (eof-error-p read-suppress)
                          expected-value &optional expected-kind
                                                   expected-parse-result
                                                   (expected-position length)
                                                   (expected-length 1))
      (flet ((do-it ()
               (let ((client (make-instance 'list-result-client)))
                 (with-stream (stream)
                   (let (value kind parse-result)
                     (eclector.reader:call-as-top-level-read
                      client (lambda ()
                               (setf (values value kind parse-result)
                                     (let ((*read-suppress* read-suppress))
                                       (eclector.reader:read-maybe-nothing
                                        client stream eof-error-p :eof)))
                               (if (eq kind :eof)
                                   value
                                   (values value kind)))
                      stream eof-error-p :eof t)
                     (values value kind parse-result))))))
        (error-case (input expected-value expected-position expected-length)
          (error (do-it))
          (t (multiple-value-bind (value kind parse-result position)
                 (do-it)
               (expect "value"        (equal expected-value        value))
               (expect "kind"         (eq    expected-kind         kind))
               (expect "parse result" (equal expected-parse-result parse-result))
               (expect "position"     (eql   expected-position     position))))))
    '(;; End of input
      (""       (t   nil) eclector.reader:end-of-file nil nil 0 0)
      (""       (nil nil) :eof :eof)
      ;; Whitespace
      ("   "    (nil nil) nil :whitespace)
      ("   "    (nil nil) nil :whitespace)
      ;; Skip
      (";  "    (nil nil) nil :skip       (:reason (:line-comment . 1) :children () :source (0 . 3))  )

      ("#||#"   (nil nil) nil :skip       (:reason :block-comment      :children () :source (0 . 4))  )
      ("#||# "  (nil nil) nil :skip       (:reason :block-comment      :children () :source (0 . 4)) 4)
      ("#||#  " (nil nil) nil :skip       (:reason :block-comment      :children () :source (0 . 4)) 4)
      ("#||#"   (nil t)   nil :skip       (:reason :block-comment      :children () :source (0 . 4))  )
      ;; Object
      ("1"      (nil nil) 1   :object     (:result 1                   :children () :source (0 . 1))  )
      ("1 "     (nil nil) 1   :object     (:result 1                   :children () :source (0 . 1)) 1)
      ("1"      (nil t)   nil :suppress   (:reason *read-suppress*     :children () :source (0 . 1))  )
      ("1 "     (nil t)   nil :suppress   (:reason *read-suppress*     :children () :source (0 . 1)) 1))))

;;; Source locations

(defun check-source-locations (result expected-source-locations)
  (labels ((check (result expected)
             (destructuring-bind (expected-location . children) expected
               (is (equal expected-location (source result)))
               (cond ((not children)
                      (is (typep result 'atom-result)))
                     ((not (typep result 'cons-result))
                      (fail "Expected CONS-RESULT, but got ~S" result))
                     (t
                      (check (first-child result) (first children))
                      (check (rest-child result) (rest children)))))))
    (check result expected-source-locations))
  (is (not (null result))))

(test read/source-locations
  "Test source locations assigned by READ."
  (do-stream-input-cases (() expected)
    (let ((result (with-stream (stream)
                    (eclector.parse-result:read
                     (make-instance 'simple-result-client) stream))))
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
                                (scons nil) ; 1
                                (scons ()
                                       (scons nil) ; 2
                                       (scons ()))))
        ;; Nested reader macros
        ("#.(list* 1 '#.(list 2))" ,(scons (0 23)
                                           (scons nil) ; 1
                                           (scons nil ; #.(...)
                                                  (scons nil) ; 2
                                                  (scons ()))))
        ;; Heuristic fails here
        ("#.(list 1 1)" ,(scons (0 12)
                                (scons nil) ; second 1 (arbitrarily)
                                (scons ()
                                       (scons nil) ; second 1 (arbitrarily)
                                       (scons ()))))))))

;;; Custom source position

(defclass custom-source-position-client (simple-result-client)
  ())

(defmethod eclector.base:source-position
    ((client custom-source-position-client) (stream t))
  (- (call-next-method)))

(defmethod eclector.base:make-source-range
    ((client custom-source-position-client) (start t) (end t))
  (vector start end))

(test read/custom-source-position-client
  "Test using a custom client with READ."
  (let ((result (with-input-from-string (stream "#||# 1")
                  (eclector.parse-result:read
                   (make-instance 'custom-source-position-client) stream))))
    (is (equalp #(-5 -6) (source result)))))

;;; Skipped input

(defclass skipped-input-recording-client
    (eclector.parse-result:parse-result-client)
  ())

(defmethod eclector.parse-result:make-expression-result
    ((client skipped-input-recording-client) (result t) (children t) (source t))
  (if (null children)
      result
      (cons result children)))

(defmethod eclector.parse-result:make-expression-result
    ((client   skipped-input-recording-client)
     (result   eclector.parse-result:definition)
     (children t)
     (source   t))
  (let ((labeled-object (eclector.parse-result:labeled-object result)))
    (multiple-value-bind (state object target-parse-result inner-labeled-object)
        (eclector.reader:labeled-object-state client labeled-object)
      (multiple-value-bind (inner-state inner-object)
          (eclector.reader:labeled-object-state client inner-labeled-object)
        (assert (eql state inner-state))
        (assert (eql object inner-object)))
      (list :definition target-parse-result children source))))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client   skipped-input-recording-client)
     (stream   t)
     (reason   t)
     (children t)
     (source   t))
  (list reason children source))

(test make-skipped-input-result/smoke
  "Smoke test for the MAKE-SKIPPED-INPUT-RESULT function."
  (do-stream-input-cases ((input length) read-suppress expected-result
                          &optional (expected-orphan-results '())
                                    (expected-position length))
    (flet ((do-it ()
             (with-stream (stream)
               (let ((*read-suppress* read-suppress))
                 (eclector.parse-result:read
                  (make-instance 'skipped-input-recording-client)
                  stream nil :eof)))))
      (multiple-value-bind (result orphan-results position) (do-it)
        (expect "result"         (equal expected-result         result))
        (expect "orphan results" (equal expected-orphan-results orphan-results))
        (expect "position"       (eql   expected-position       position))))
    '(;; Whitespace is not skipped input.
      ("1"                nil 1)
      (" 1"               nil 1)
      ("1 "               nil 1)
      ("1 2"              nil 1 () 2)
      ;; Toplevel comments
      ("#||# 1"           nil 1                             ((:block-comment () (0 . 4))))
      ("#||# 1"           t   (*read-suppress* () (5 . 6))  ((:block-comment () (0 . 4))))
      ("#|#||#|# 1"       nil 1                             ((:block-comment () (0 . 8))))
      ("#|#||#|# 1"       t   (*read-suppress* () (9 . 10)) ((:block-comment () (0 . 8))))
      ("; test"           nil :eof                          (((:line-comment . 1) () (0 . 6))))
      ("; test~% 1"       nil 1                             (((:line-comment . 1) () (0 . 7))))
      (";; test~% 1"      nil 1                             (((:line-comment . 2) () (0 . 8))))
      (";;; test~% 1"     nil 1                             (((:line-comment . 3) () (0 . 9))))
      ;; Toplevel reader conditionals
      ("#+(or) 1 2"       nil 2                        (((:sharpsign-plus . (:or))
                                                         (((:or) . (:or))
                                                          (*read-suppress* () (7 . 8)))
                                                         (0 . 8))))
      ("#+(and) 1"        nil (1 . (((:and) :and) 1)))
      ("#-(and) 1 2"      nil 2                        (((:sharpsign-minus . (:and))
                                                         (((:and) . (:and))
                                                          (*read-suppress* () (8 . 9)))
                                                         (0 . 9))))
      ("#+(or) 1 (2 3)"   nil ((2 3) . (2 3))          (((:sharpsign-plus . (:or))
                                                         (((:or) . (:or))
                                                          (*read-suppress* () (7 . 8)))
                                                         (0 . 8))))
      ;; Non-toplevel comments
      ("(#||# 1)"         nil ((1) . ((:block-comment () (1 . 5))
                                      1)))
      ("(~%; test~% 1)"   nil ((1) . (((:line-comment . 1) () (2 . 9))
                                      1)))
      ("(~%;; test~% 1)"  nil ((1) . (((:line-comment . 2) () (2 . 10))
                                      1)))
      ("(~%;;; test~% 1)" nil ((1) . (((:line-comment . 3) () (2 . 11))
                                      1)))
      ("#1=#|foo|#2"      nil (:definition #1=2 ((:block-comment () (3 . 10)) #1#) (0 . 11)))
      ;; Non-toplevel reader conditionals
      ("(#+(or) 1 2)"     nil ((2) . (((:sharpsign-plus . (:or))
                                       (((:or) . (:or))
                                        (*read-suppress* () (8 . 9)))
                                       (1 . 9))
                                      2)))
      ;; Order of skipped inputs
      ("#|1|# #|2|# 3"    nil 3                              ((:block-comment () (0 . 5))
                                                              (:block-comment () (6 . 11))))
      ("#|1|# #|2|# 3"    t   (*read-suppress* () (12 . 13)) ((:block-comment () (0 . 5))
                                                              (:block-comment () (6 . 11))))
      ;; Non-toplevel suppressed objects
      ("(nil)"            t   (*read-suppress*
                               ((*read-suppress* () (1 . 4)))
                               (0 . 5)))
      ("#|1|# (nil)"      t   (*read-suppress*
                               ((*read-suppress* () (7 . 10)))
                               (6 . 11))
                              ((:block-comment () (0 . 5)))))))

;;; Custom reader macros

(defun bypassing-left-parenthesis (stream char)
  (declare (ignore char))
  (loop for peek = (eclector.reader:peek-char t stream t nil t)
        when (eq peek #\))
          do (eclector.reader:read-char stream t nil t)
             (loop-finish)
        ;; Instead of unconditionally calling READ recursively, do the
        ;; reader macro lookup "manually".
        collect (let ((function (eclector.readtable:get-macro-character
                                 eclector.reader:*readtable* peek)))
                  (cond (function
                         (eclector.reader:read-char stream t nil t)
                         (funcall function stream peek))
                        (t
                         (eclector.reader:read stream t nil t))))))

(test custom-reader-macro/smoke
  "Ensure that read objects and parse results remain consistent in the
presence of custom reader macros that bypass some reader functionality."
  (do-stream-input-cases ((input length)
                          expected-result
                          &optional (expected-source `(0 . ,length)))
      (let ((eclector.reader:*client* (make-instance 'simple-result-client))
            (eclector.reader:*readtable* (eclector.readtable:copy-readtable
                                          eclector.reader:*readtable*)))
        (eclector.readtable:set-macro-character
         eclector.reader:*readtable* #\( #'bypassing-left-parenthesis)
        (multiple-value-bind (object result) (with-stream (stream)
                                               (eclector.reader:read stream))
          (expect "object" (equalp* expected-result object))
          (expect "raw"    (equalp* expected-result (raw result)))
          (expect "source" (equal expected-source (source result)))))
    '(("(print (quote (member :floor :ceiling)))"
       (print (quote (member :floor :ceiling))))
      ("(print '(member :floor :ceiling))"
       (print '(member :floor :ceiling)))
      ("(print #((member :floor :ceiling)))"
       (print #((member :floor :ceiling))))
      ("(print `(member ,:floor :ceiling))"
       (print (eclector.reader:quasiquote
               (member (eclector.reader:unquote :floor) :ceiling))))
      ("(print (quote #1=(member :floor :ceiling)))"
       (print (quote (member :floor :ceiling))))
      ;; We ignore the `make-expression-result' call with a
      ;; `definition' instance and a source range for the entire
      ;; input.  As a result, we must adjust the expected source range
      ;; for the root parse result.
      ("#1=(print (quote #2=(member :floor :ceiling)))"
       (print (quote (member :floor :ceiling)))
       (3 . 46))
      ("(print (quote #1=(member #1# :ceiling)))"
       (print (quote #1=(member #1# :ceiling)))))))
