(cl:in-package #:eclector.concrete-syntax-tree.test)

(def-suite* :eclector.concrete-syntax-tree.labeled-objects
  :in :eclector.concrete-syntax-tree)

(test labeled-objects/random
  "Random test for reading labeled object expressions into CSTs.

Test with the ordinary CST client and a client that creates additional
CSTs for labeled object definitions and references."
  (labels ((raw* (cst)
             (typecase cst
               (eclector.concrete-syntax-tree:wrapper-cst
                (raw* (eclector.concrete-syntax-tree:target cst)))
               (t
                (cst:raw cst))))
           (do-client (client)
             (let ((*test-dribble* (make-broadcast-stream)) ; too much output otherwise
                   (*num-trials* 10000)
                   (*max-trials* 10000))
               (for-all ((expression (gen-labels-and-references)))
                 (let* ((input (let ((*print-circle* t))
                                 (prin1-to-string expression)))
                        (result (let ((eclector.base:*client* client))
                                  (eclector.concrete-syntax-tree:read-from-string
                                   input))))
                   (assert (equal* expression (read-from-string input)))
                   (expect input "cst:raw" (equal* expression (cst:raw result)))
                   (expect input "raw*"    (equal* expression (raw* result)))
                   (is-true (valid-cst-parse-result-p
                             eclector.concrete-syntax-tree::*cst-client* result)
                            "~@<For input ~S, the result CST ~S is not valid~@:>"
                            input result)
                   (is-consistent-with-raw result))))))
    (do-client eclector.concrete-syntax-tree::*cst-client*)
    (do-client (make-instance 'wrapper-cst-client))))

;;; Tests for fixing up deeply nested structures

(defclass call-counting-cst-client (call-counting-client
                                    eclector.concrete-syntax-tree:cst-client)
  ())

(test recursive-fixup.cdr-spine-list
  "Read and fixup a structure of the form #1=(1 2 3 . #1#) but much
deeper."
  (let ((fiveam:*test-dribble* nil)) ; too much output otherwise
    (let* ((length 200000)
           (client (make-instance 'call-counting-cst-client))
           (list-maker (lambda ()
                         (let ((root (alexandria:iota length :start 1)))
                           (values root
                                   (lambda (leaf)
                                     (setf (cdr (last root)) leaf)))))))
      (multiple-value-bind (result cst) (read-long-list client list-maker)
        (is-true (typep cst 'cst:cons-cst))
        (is-true (valid-cst-parse-result-p client cst))
        (is (eq result (cst:raw cst)))
        (is-consistent-with-raw cst)
        (is (eq cst (loop repeat (1+ length)
                          for sub-cst = cst then (cst:rest sub-cst)
                          finally (return sub-cst))))
        ;; One for s-expression results and one for the parse results.
        (is (= 2 (fixup-graph-count client)))
        ;; `fixup' is called once for each cons of the returned
        ;; s-expression list. `fixup' is called for each `cons-cst'
        ;; and the `atom-cst' in its `cst:first' slot, so 3 `fixup'
        ;; calls for each element of the read list.
        (is (= (* 3 length) (fixup-count client)))))))

(test recursive-fixup.car-spine-list
  "Read and fixup a structure of the form #1=(((#1# . 1) . 2) . 3) but much
deeper. This test ensures that we don't just get lucky via structural tail
recursion on the cdr slot."
  (let ((fiveam:*test-dribble* nil)) ; too much output otherwise
    (let* ((length 200000)
           (client (make-instance 'call-counting-cst-client))
           (list-maker (lambda () (make-car-spine-list length))))
      (multiple-value-bind (result cst) (read-long-list client list-maker)
        (is-true (typep cst 'cst:cons-cst))
        (is-true (valid-cst-parse-result-p client cst))
        (is (eq result (cst:raw cst)))
        (is-consistent-with-raw cst)
        (is (eq result (loop repeat (1+ length)
                             for object = result then (car object)
                             finally (return object))))
        ;; One for s-expression results and one for the parse results.
        (is (= 2 (fixup-graph-count client)))
        ;; `fixup' is called once for each cons of the returned
        ;; s-expression list. `fixup' is called for each `cons-cst'
        ;; and the `atom-cst' in its `cst:rest' slot, so 3 `fixup'
        ;; calls for each element of the read list.
        (is (= (* 3 length) (fixup-count client)))))))

(test recursive-fixup.tree
  "Read and fixup a structure of the form #1=((L . L) . (L . L)) where
each L is of the form (((#1# . 1) . 2) . 3).  The actual structure is
much deeper, of course.  The outer tree structure ensures that work
items actually build up in the worklist instead of one item being
added and immediately processed like for the list structures."
  (let ((fiveam:*test-dribble* nil)) ; too much output otherwise
    (let* ((depth 10000)
           (client (make-instance 'call-counting-cst-client))
           (list-maker (lambda () (make-tree depth))))
      (multiple-value-bind (result cst) (read-long-list client list-maker)
        (is-true (typep cst 'cst:cons-cst))
        (is-true (valid-cst-parse-result-p client cst))
        (is (eq result (cst:raw cst)))
        (is-consistent-with-raw cst)
        (is (eq result (loop repeat (1+ depth)
                             for object = result then (car object)
                             finally (return object))))
        ;; One for s-expression results and one for the parse results.
        (is (= 2 (fixup-graph-count client)))
        ;; For tree nodes, `fixup' is called once for the s-expression
        ;; cons and once for the `cons-cst'. For the list elements, 3
        ;; calls, like in the cases above.
        (is (= (+ (* 2 (1- (ash 1 5)))           ; 2 * node count
                  (* 3 (ash 1 4) 2 (- depth 5))) ; 3 * leaf count * 2 * list length
               (fixup-count client)))))))
