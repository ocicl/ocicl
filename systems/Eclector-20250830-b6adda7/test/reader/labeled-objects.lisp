(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.labeled-objects
  :in :eclector.reader)

;;; Count fixup calls
;;;
;;; Avoiding unnecessary traversal of read objects and unnecessary
;;; `fixup' calls is an important aspect of the labeled objects
;;; sub-system.  The following client helps ensuring that no
;;; unnecessary `fixup' calls are made.  The client counts only
;;; `fixup' calls that were not stopped early by the `:around' method
;;; on `fixup'.

(defclass call-counting-client ()
  ((%fixup-graph-count :accessor fixup-graph-count
                       :initform 0)
   (%fixup-count :accessor fixup-count
                 :initform 0)))

(defmethod eclector.reader:fixup-graph :after ((client call-counting-client)
                                               (labeled-object t)
                                               &key object-key)
  (declare (ignore object-key))
  (incf (fixup-graph-count client)))

(defmethod eclector.reader:fixup :after ((client call-counting-client)
                                         (object t)
                                         (traversal-state t))
  (incf (fixup-count client)))

;;; Tests

(defclass mock-object ()
  ((%a :initarg :a :reader a)
   (%b :initarg :b :reader b)))

(test fixup/smoke
  "Smoke test for the FIXUP generic function."
  (mapc (lambda (object-expected)
          (destructuring-bind (object expected fixup-count) object-expected
            (let ((client (make-instance 'call-counting-client))
                  (seen (make-hash-table :test #'eq)))
              (eclector.reader:fixup client object seen)
              (typecase object
                (mock-object
                 (let ((slot-values (list (if (slot-boundp object '%a)
                                              (a object)
                                              'unbound)
                                          (b object))))
                   (is (equalp expected slot-values))))
                (hash-table
                 (let ((alist (alexandria:hash-table-alist object)))
                   (is (alexandria:set-equal expected alist :test #'equal)
                       "~@<Expected hash table entries ~S but got ~S. ~
                        Mismatches: ~S and ~S~@:>"
                       expected alist
                       (set-difference expected alist :test #'equal)
                       (set-difference alist expected :test #'equal))))
                (t
                 (is (equalp expected object))))
              (is (= fixup-count (fixup-count client))
                  "~@<For object, ~S expected ~A to be called ~D time~:P, ~
                   but it was called ~D time~:P~@:>"
                  object 'eclector.reader:fixup
                  fixup-count (fixup-count client)))))
        (flet ((labeled-object (object &optional (label 1) (finalp t))
                 (eclector.reader:call-with-label-tracking
                  nil (lambda ()
                        (let ((labeled-object (eclector.reader:note-labeled-object
                                               nil nil label nil)))
                          (if finalp
                              (eclector.reader:finalize-labeled-object
                               nil labeled-object object)
                              labeled-object))))))
          (list ;; cons
                (let* ((a (gensym))
                       (marker (labeled-object a)))
                  (list (list 1 marker a (cons 2 marker))
                        (list 1 a      a (cons 2 a))
                        5))
                ;; vector
                (let* ((a (gensym))
                       (marker (labeled-object a)))
                  (list (vector a marker)
                        (vector a a)
                        1))
                ;; Specialized arrays (smoke test since nothing has to be fixed up)
                (list "foo" "foo" 1)
                #.(if (subtypep (upgraded-array-element-type '(unsigned-byte 8))
                                'number)
                      '(list (make-array 2 :element-type     '(unsigned-byte 8)
                                           :initial-contents '(1 2))
                             (make-array 2 :element-type     '(unsigned-byte 8)
                                           :initial-contents '(1 2))
                             1)
                      '(list nil nil 1))
                ;; standard-object
                (let* ((a (gensym))
                       (marker (labeled-object a)))
                  (list (make-instance 'mock-object :a a :b marker)
                        (list a a)
                        1))
                ;; standard-object with unbound slot
                (let* ((a (gensym))
                       (marker (labeled-object a)))
                  (list (make-instance 'mock-object :b marker)
                        (list 'unbound a)
                        1))
                ;; hash-table
                (let* ((a (gensym))
                       (b (gensym))
                       (c (gensym))
                       (d (gensym))
                       (e (gensym))
                       (f (gensym))
                       (g (gensym))
                       (marker1 (labeled-object a 1))
                       (marker2 (labeled-object b 2))
                       (marker3 (labeled-object c 3))
                       (marker4 (labeled-object d 4))
                       (marker5 (labeled-object e 5))
                       ;; The following two labeled objects are not
                       ;; finalized and should remain untouched.
                       (marker6 (labeled-object f 6 nil))
                       (marker7 (labeled-object g 7 nil)))
                  (list (alexandria:alist-hash-table
                         (list (cons (cons a marker2) 1) (cons 2 a) (cons 3 marker1)
                               (cons b 4) (cons 5 marker2) (cons 6 b)
                               (cons 7 (cons 8 marker3))
                               (cons marker4 9) (cons marker5 marker4)
                               (cons marker6 (cons 10 marker2)) (cons 11 marker7)))
                        (list (cons (cons a b) 1) (cons 2 a) (cons 3 a)
                              (cons b 4) (cons 5 b) (cons 6 b)
                              (cons 7 (cons 8 c))
                              (cons d 9) (cons e d)
                              (cons marker6 (cons 10 b)) (cons 11 marker7))
                        4))
                ;; pathname (immutable and special-cased)
                (let ((object #P"foo"))
                  (list object object 0))
                ;; random-state (immutable but not special-cased)
                (let ((object (make-random-state)))
                  (list object object 1))))))

(test fixup/call-count
  "Ensure absence of redundant `fixup' calls."
  (do-stream-input-cases (() expected-fixup-graph-count expected-fixup-count)
    (let* ((client (make-instance 'call-counting-client))
           (result (with-stream (stream)
                     (let ((eclector.base:*client* client))
                       (eclector.reader:read stream))))
           (expected (with-stream (stream)
                       (read stream))))
      (expect "read object "(equal* expected result))
      (expect "fixup graph call count"
              (= expected-fixup-graph-count (fixup-graph-count client)))
      (expect "fixup call count"
              (= expected-fixup-count (fixup-count client))))
    '(("(#1=(:a :b) #1#)"               0 0)
      ("#1=(#2=(:a #2# :b) :c #1# :d)"  1 7)
      ("(#1=(:a #2=(#3=(:b #3#))) #1#)" 1 2)
      ("(#1=(:a #1#) :b #2=(:b #2#))"   2 4))))

;;; Random test

(test labeled-objects/random
  "Random test for labeled objects."
  (let ((*test-dribble* (make-broadcast-stream)) ; too much output otherwise
        (*num-trials* 10000)
        (*max-trials* 10000))
    (for-all ((expression (gen-labels-and-references)))
      (let* ((input (let ((*print-circle* t))
                      (prin1-to-string expression)))
             (result (eclector.reader:read-from-string input)))
        (assert (equal* expression (read-from-string input)))
        (is (equal* expression result))))))

;;; Tests for fixing up deeply nested structures

(test recursive-fixup.cdr-spine-list
  "Read and fixup a structure of the form #1=(1 2 3 . #1#) but much
deeper."
  (let* ((length 200000)
         (client (make-instance 'call-counting-client))
         (list-maker (lambda ()
                       (let ((root (alexandria:iota length :start 1)))
                         (values root
                                 (lambda (leaf)
                                   (setf (cdr (last root)) leaf))))))
         (result (read-long-list client list-maker)))
    (is-true (typep result 'list))
    (is (eq result (loop repeat (1+ length)
                         for object = result then (cdr object)
                         finally (return object))))
    (is (= 1 (fixup-graph-count client)))
    (is (= length (fixup-count client)))))

(test recursive-fixup.car-spine-list
  "Read and fixup a structure of the form #1=(((#1# . 1) . 2) . 3) but much
deeper.  This test ensures that we don't just get lucky via structural
tail recursion on the cdr slot."
  (let* ((length 200000)
         (client (make-instance 'call-counting-client))
         (list-maker (lambda () (make-car-spine-list length)))
         (result (read-long-list client list-maker)))
    (is-true (typep result 'list))
    (is (eq result (loop repeat (1+ length)
                         for object = result then (car object)
                         finally (return object))))
    (is (= 1 (fixup-graph-count client)))
    (is (= length (fixup-count client)))))

(test recursive-fixup.tree
  "Read and fixup a structure of the form #1=((L . L) . (L . L)) where
each L is of the form (((#1# . 1) . 2) . 3).  The actual structure is
much deeper, of course.  The outer tree structure ensures that work
items actually build up in the worklist instead of one item being
added and immediately processed like for the list structures."
  (let* ((depth 10000)
         (client (make-instance 'call-counting-client))
         (tree-maker (lambda () (make-tree depth)))
         (result (read-long-list client tree-maker)))
    (is-true (typep result 'list))
    (is (eq result (loop repeat (1+ depth)
                         for object = result then (car object)
                         finally (return object))))
    (is (= 1 (fixup-graph-count client)))
    (is (= (+ (1- (ash 1 5))               ; node count
              (* (ash 1 4) 2 (- depth 5))) ; leaf count * 2 * list length
           (fixup-count client)))))
