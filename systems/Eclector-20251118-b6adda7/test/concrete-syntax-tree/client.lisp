(cl:in-package #:eclector.concrete-syntax-tree.test)

(def-suite* :eclector.concrete-syntax-tree.client
  :in :eclector.concrete-syntax-tree)

;;; Test annotating labeled object references

(defclass annotating-cst-client (eclector.reader.test::label-reference-annotation-mixin
                                 eclector.concrete-syntax-tree:cst-client)
  ())

(test labeled-object-annotation
  "Test custom labeled object reference processing."
  (let* ((input "(A #1=(b #1# c #1# d) e #1# f)")
         (client (make-instance 'annotating-cst-client))
         (result (let ((eclector.base:*client* client))
                   (eclector.concrete-syntax-tree:read-from-string input))))
    (is-true (valid-cst-parse-result-p client result))
    (is-consistent-with-raw result)
    (is (equal* '(a #1=(b (:circular-reference #1#)
                        c (:another-circular-reference #1#)
                        d)
                  e (:ordinary-reference #1#) f)
                (cst:raw result)))))

;;; Test wrapper CST classes

(defclass wrapper-cst-client (eclector.concrete-syntax-tree:definition-csts-mixin
                              eclector.concrete-syntax-tree:reference-csts-mixin
                              eclector.concrete-syntax-tree:cst-client)
  ())

(test wrapper-labeled-object-csts/random
  "Random test for reading labeled object expressions into wrapper CSTs."
  (labels ((raw* (cst)
             (typecase cst
               (eclector.concrete-syntax-tree:wrapper-cst
                (raw* (eclector.concrete-syntax-tree:target cst)))
               (t
                (cst:raw cst)))))
    (let ((*test-dribble* (make-broadcast-stream)) ; too much output otherwise
          (*num-trials* 10000)
          (*max-trials* 10000))
      (for-all ((expression (gen-labels-and-references)))
        (let* ((input (let ((*print-circle* t))
                        (prin1-to-string expression)))
               (client (make-instance 'wrapper-cst-client))
               (result (let ((eclector.base:*client* client))
                         (eclector.concrete-syntax-tree:read-from-string input))))
          (assert (equal* expression (read-from-string input)))
          (is-true (valid-cst-parse-result-p client result)
                   "~@<For input ~S, the result CST ~S is not valid.~@:>"
                   input result)
          (is-consistent-with-raw result)
          (expect input "cst:raw" (equal* expression (cst:raw result)))
          (expect input "raw*"    (equal* expression (raw* result))))))))

(test wrapper-labeled-object-csts/missed-labeled-object
  "Check that no labeled objects remain in the parse result tree."
  (let* ((input "#1=(#2=(#1#))")
         (client (make-instance 'wrapper-cst-client))
         (result (let ((eclector.base:*client* client))
                   (eclector.concrete-syntax-tree:read-from-string input))))
    (is-true (valid-cst-parse-result-p client result))
    (is-consistent-with-raw result)))

;;; Test the interaction between wrapper CSTs for labeled objects and
;;; explicitly represented skipped input.  The potential problem
;;; affects only definitions of labeled objects since references
;;; cannot syntactically contain skipped material.

(defclass definition-with-children (eclector.concrete-syntax-tree:definition-cst)
  ((%children :initarg :children
              :reader  children)))

(defclass skipped-input+wrapper-cst-client (wrapper-cst-client) ())

(defmethod eclector.parse-result:make-expression-result
    ((child    skipped-input+wrapper-cst-client)
     (result   eclector.parse-result:definition)
     (children t)
     (source   t))
  (let ((result (call-next-method)))
    (change-class result 'definition-with-children :children children)))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client   skipped-input+wrapper-cst-client)
     (stream   t)
     (reason   t)
     (children t)
     (source   t))
  ;; The representation does not matter here.  We just need some
  ;; representation of the skipped input.
  (list reason t))

(test wrapper-labeled-object-csts/skipped-input-children
  "Check that children which represent skipped input do not throw off
labeled object definition wrapper CSTs."
  (let* ((client (make-instance 'skipped-input+wrapper-cst-client))
         (result (let ((eclector.base:*client* client))
                   (eclector.concrete-syntax-tree:read-from-string
                    "#1=#|foo|#2")))
         (target (eclector.concrete-syntax-tree:target result)))
    (is-true (valid-cst-parse-result-p client result))
    (is-consistent-with-raw result)
    (is (equal (list '(:block-comment t) target) (children result)))
    (is (cst:atom target))
    (is (eql 2 (cst:raw target)))))

;;; Make sure that a custom reader macro which bypasses some reader
;;; functionality does not lead to invalid parse results, also in
;;; combination with wrapper CSTs that represent labeled object
;;; definitions and references.

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
  (do-stream-input-cases ((input length) expected-result)
      (let ((expected-source `(0 . ,length))
            (client (make-instance 'wrapper-cst-client))
            (eclector.reader:*readtable* (eclector.readtable:copy-readtable
                                          eclector.reader:*readtable*)))
        (eclector.readtable:set-macro-character
         eclector.reader:*readtable* #\( #'bypassing-left-parenthesis)
        (let ((result (let ((eclector.base:*client* client))
                        (eclector.concrete-syntax-tree:read-from-string
                         input))))
          (expect "raw"    (equalp* expected-result (cst:raw result)))
          (expect "source" (equal expected-source (cst:source result)))
          (is-true (valid-cst-parse-result-p result result)
                   "~@<For input ~S, the result CST ~S is not valid~@:>"
                   input result)
          (is-consistent-with-raw result)))
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
      ("#1=(print (quote #2=(member :floor :ceiling)))"
       (print (quote (member :floor :ceiling))))
      ("(print (quote #1=(member #1# :ceiling)))"
       (print (quote #1=(member #1# :ceiling)))))))
