(cl:in-package #:eclector.concrete-syntax-tree)

(defclass cst-client (eclector.parse-result:parse-result-client)
  ())

(defvar *cst-client* (make-instance 'cst-client))

;;; This method is responsible for constructing CST results from
;;; ordinary s-expression results.  It can do this by itself for a
;;; number of simple cases and it calls
;;; `concrete-syntax-tree:reconstruct' for difficult cases.
(defmethod eclector.parse-result:make-expression-result
    ((client cst-client) expression children source)
  ;; Our goal is to return a CST, say c, with the following properties:
  ;;
  ;; 1. The structure and raw values of c should match the structure
  ;;    of EXPRESSION, abbreviated as e, in the sense that for any
  ;;    "path" p from the set U_{L >= 0} {car,cdr}^L that is "valid"
  ;;    for e the following should hold:
  ;;
  ;;      (eql (cst:raw (apply-path/cst p c)) (apply-path p e))
  ;;
  ;;    where the `apply-path' functions repeatedly apply appropriate
  ;;    readers according to the supplied path.
  ;;
  ;; 2. The elements of CHILDREN, which is a "pool" of available
  ;;    sub-CSTs, should be incorporated as nodes into the CST rooted
  ;;    at c whenever possible.
  ;;
  ;; Note that property 2. does not imply that all elements of
  ;; CHILDREN should appear in the CST rooted at c.  For example, when
  ;; this method is called for an EXPRESSION of the form (0 . 0),
  ;; there will be three elements in CHILDREN: an atom for the first
  ;; 0, an atom for the consing dot and another atom for the second 0.
  ;; The middle child which represents the consing dot should not
  ;; appear as a node in the CST rooted at c.
  ;;
  ;; Furthermore, there are often multiple ways for c to satisfy the
  ;; properties 1. and 2.  Consider again the example (0 . 0).
  ;; Property 1. can be fulfilled by setting the car and cdr of c to
  ;; either the first or the third child, so there are four equally
  ;; valid combinations.
  ;;
  ;; The code below tries to construct good CSTs by picking off a few
  ;; special cases and falling back to
  ;; `concrete-syntax-tree:reconstruct' for the general case. There
  ;; are two reasons for this approach:
  ;;
  ;; 1. For special cases, more information may be available.
  ;;    Consider once again (0 . 0).  It is obvious that the car of c
  ;;    should be the `atom-cst' which corresponds to the first 0 and
  ;;    the cdr of c should be the `atom-cst' which corresponds to the
  ;;    second 0.  In contrast, the reconstructing heuristic for the
  ;;    general case would use the first `atom-cst' in both cases
  ;;    since it has no way of distinguishing (0 . 0) and (morally)
  ;;    (#1=0 . #1#).
  ;;
  ;; 2. `concrete-syntax-tree:reconstruct' is an expensive operation.
  ;;    Special-casing common expression shapes improves performance
  ;;    for typical inputs.
  (let (children-length)
    (cond ((atom expression)
           (make-instance 'cst:atom-cst :raw expression :source source))
          ;; EXPRESSION has a list structure with elements
          ;; corresponding to the elements of CHILDREN.
          ((and (eql (ignore-errors (list-length expression))
                     (setf children-length (length children)))
                (every (lambda (sub-expression child)
                         (eql sub-expression (cst:raw child)))
                       expression children))
           (loop for expression in (loop with reversed = '()
                                         for sub-expression on expression
                                         do (push sub-expression reversed)
                                         finally (return reversed))
                 for child in (reverse children)
                 for previous = (make-instance 'cst:atom-cst :raw nil) then node
                 for node = (make-instance 'cst:cons-cst :raw expression
                                                         :first child
                                                         :rest previous)
                 finally (return (reinitialize-instance node :source source))))
          ;; EXPRESSION is a CONS that resulted from reading a dotted
          ;; list such that the elements of CHILDREN correspond to car
          ;; of EXPRESSION, the consing dot and the cdr of EXPRESSION.
          ((and (not (consp (cdr expression)))
                (= 3 children-length)
                (destructuring-bind (car dot cdr) children
                  (eql (car expression)               (cst:raw car))
                  (eql eclector.reader::*consing-dot* (cst:raw dot))
                  (eql (cdr expression)               (cst:raw cdr))))
           (make-instance 'cst:cons-cst :raw expression
                                        :first (first children)
                                        :rest (third children)
                                        :source source))
          ;; Structure mismatch, try heuristic reconstruction.
          (t
           ;; We don't use
           ;;
           ;;   (cst:reconstruct client expression children)
           ;;
           ;; because we want SOURCE for the outer `cons-cst' but not
           ;; any of its children.
           (destructuring-bind (car . cdr) expression
             (make-instance 'cst:cons-cst
                            :raw expression
                            :first (cst:reconstruct client car children)
                            :rest (cst:reconstruct client cdr children)
                            :source source))))))
