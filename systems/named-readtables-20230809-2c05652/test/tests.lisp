;;; -*- Mode:Lisp -*-

(in-package :named-readtables-test)

(defun map-alist (car-fn cdr-fn alist)
  (mapcar #'(lambda (entry)
              (cons (funcall car-fn (car entry))
                    (funcall cdr-fn (cdr entry))))
          alist))

(defun length=1 (list)
  (and list (null (cdr list))))

(defmacro continue-condition (name &body body)
  `(handler-bind ((,(second name) #'continue))
     ,@body))

(defun read-with-readtable (name string)
  (let ((*package* '#.*package*)
        (*readtable* (find-readtable name)))
    (values (read-from-string string))))

(defun random-named-readtable ()
  (let ((readtables (list-all-named-readtables)))
    (nth (random (length readtables)) readtables)))


(defun readtable-content (named-readtable-designator)
  (let ((readtable (ensure-readtable named-readtable-designator))
        (result '()))
    ;; Make sure to canonicalize the order and function designators so
    ;; we can compare easily.
    (do-readtable ((char reader-fn ntp disp? table) readtable)
      (setq table (sort (copy-list table) #'char< :key #'car))
      (push (list* char
                   (ensure-function reader-fn)
                   ntp
                   (and disp? (list (map-alist #'identity
                                               #'ensure-function
                                               table))))
            result))
    (sort result #'char< :key #'car)))

(defun readtable= (rt1 rt2)
  (tree-equal (readtable-content rt1) (readtable-content rt2)
              :test #'(lambda (x y)
                        (if (and (functionp x) (functionp y))
                            (function= x y)
                            (eql x y)))))


(defun read-A (stream c &optional n)
  (declare (ignore stream c n))
  :a)

(defun read-A-as-X (stream c &optional n)
  (declare (ignore stream c n))
  :x)

(defun read-B (stream c)
  (declare (ignore stream c))
  :b)

(defun read-sharp-paren (stream c n)
  (declare (ignore stream c n))
  'sharp-paren)

(defun read-C (stream c)
  (declare (ignore stream c))
  :c)

(defreadtable A
  (:macro-char #\A #'read-A))

(defreadtable A-as-X
  (:macro-char #\A #'read-A-as-X))

(defreadtable A-dispatch
  (:macro-char #\A :dispatch)
  (:dispatch-macro-char #\A #\A #'read-A))

(defreadtable A-dispatch-as-X
  (:macro-char #\A :dispatch)
  (:dispatch-macro-char #\A #\A #'read-A-as-X))

(defreadtable B
  (:macro-char #\B #'read-B))

(defreadtable C
  (:macro-char #\C #'read-C))

(defreadtable A+B+C
  (:merge A B C))

(defreadtable standard+A+B+C
  (:merge :standard A+B+C))

(defreadtable sharp-paren
  (:macro-char #\# :dispatch)
  (:dispatch-macro-char #\# #\( #'read-sharp-paren))


(deftest cruft.1 ()
  (is (function= (get-macro-character #\" (copy-readtable nil))
                 (get-macro-character #\" (copy-readtable nil)))))

(deftest cruft.2 ()
  (is (dispatch-macro-char-p #\# (find-readtable :standard))))

(deftest cruft.3 ()
  (is (not (dispatch-macro-char-p #\# (make-readtable)))))

(deftest cruft.4 ()
  (let ((rt (copy-named-readtable :standard)))
    (ensure-dispatch-macro-character #\# t rt)
    (is (dispatch-macro-char-p #\# rt))))

(deftest cruft.5 ()
  (let ((rt (make-readtable)))
    (is (not (dispatch-macro-char-p #\$ rt)))
    (is (ensure-dispatch-macro-character #\$ t rt))
    (is (dispatch-macro-char-p #\$ rt))))

(deftest cruft.6 ()
  (let ((rt (make-readtable))
        (fn (constantly nil)))
    (ensure-dispatch-macro-character #\$ t rt)
    (set-dispatch-macro-character #\$ #\# fn rt)
    (is (eq fn (get-dispatch-macro-character #\$ #\# rt)))
    (is (length=1 (readtable-content rt)))))

(deftest cruft.7 ()
  (let ((rt (make-readtable))
        (fn (constantly nil)))
    (set-macro-character #\$ fn t rt)
    (is (eq fn (get-macro-character #\$ rt)))
    (is (length=1 (readtable-content rt)))))


(deftest standard.1 ()
  (is (eq (read-with-readtable :standard "ABC") 'abc)))

(deftest standard.2 ()
  (is (equal (read-with-readtable :standard "(A B C)") '(a b c))))

(deftest standard.3 ()
  (let ((x (find-readtable nil))
        (y (find-readtable :standard))
        (z (find-readtable :common-lisp)))
    (is (eq x y))
    (is (eq y z))))


(deftest modern.1 ()
  (is (eq (read-with-readtable :modern "FooF") '|FooF|)))


(deftest empty.1 ()
  (is (null (readtable-content (make-readtable)))))

(deftest empty.2 ()
  (is (readtable= (merge-readtables-into (make-readtable) :standard)
                  (find-readtable :standard))))

(deftest empty.3 ()
  (let ((rt (copy-named-readtable :standard)))
    (is (readtable= (merge-readtables-into (make-readtable) rt)
                    (merge-readtables-into rt (make-readtable))))))


(deftest basics.1 ()
  (is (eq (read-with-readtable 'A "A") :a)))

(deftest basics.2 ()
  (is (eq (read-with-readtable 'A-as-X "A") :x)))

(deftest basics.3 ()
  (is (eq (read-with-readtable 'A "B") 'b)))

(deftest basics.4 ()
  (is (eq (read-with-readtable 'A "(A B C)") '|(|)))


(deftest unregister.1 ()
  (let ((rt (find-readtable 'A)))
    (register-readtable 'does-not-exist rt)
    (is (find-readtable 'does-not-exist))
    (is (unregister-readtable 'does-not-exist))
    (is (not (find-readtable 'does-not-exist)))))


(deftest name.1 ()
  (let ((rt (random-named-readtable)))
    (is (eq rt (find-readtable (readtable-name rt))))))

(deftest ensure.1 ()
  (unwind-protect
       (let* ((x (ensure-readtable 'does-not-exist (find-readtable 'A)))
              (y (find-readtable 'A))
              (z (find-readtable 'does-not-exist)))
         (is (eq x y))
         (is (eq y z)))
    (unregister-readtable 'does-not-exist)))


(deftest merge.1 ()
  (is (eq (read-with-readtable 'A+B+C "A") :a))
  (is (eq (read-with-readtable 'A+B+C "B") :b))
  (is (eq (read-with-readtable 'A+B+C "C") :c)))

(deftest merge.2 ()
  (is (equal (read-with-readtable 'standard+A+B+C "(A B C)") '(:a :b :c))))

(deftest merge.3 ()
  (is (equalp (read-with-readtable 'standard+A+B+C "#(A B C)") #(:a :b :c))))

(deftest merge.4 ()
  (let ((A+B+C+standard (merge-readtables-into (copy-named-readtable 'A+B+C)
                                               :standard)))
    (is (readtable= 'standard+A+B+C A+B+C+standard))))


(deftest rename.1 ()
  (unwind-protect
       (progn (make-readtable 'A* :merge '(A))
              (rename-readtable 'A* 'A**)
              (is (not (and (find-readtable 'A*) t)))
              (is (find-readtable 'A**)))
    (unregister-readtable 'A*)
    (unregister-readtable 'A**)))


(deftest reader-macro-conflict.1 ()
  (signals (reader-macro-conflict)
    (merge-readtables-into (make-readtable) 'A 'A-as-X)))

(deftest reader-macro-conflict.2 ()
  (signals-not (reader-macro-conflict)
    (merge-readtables-into (make-readtable) :standard :standard)))

(deftest reader-macro-conflict.3 ()
  (signals-not (reader-macro-conflict)
    (merge-readtables-into (make-readtable) 'A+B+C 'A)))

(deftest reader-macro-conflict.4 ()
  (signals (reader-macro-conflict)
    (merge-readtables-into (make-readtable) :standard 'sharp-paren)))

(deftest reader-macro-conflict.5 ()
  (signals (reader-macro-conflict)
    (merge-readtables-into (make-readtable) 'A 'A-dispatch)))

(deftest reader-macro-conflict.6 ()
  (signals (reader-macro-conflict)
    (merge-readtables-into (make-readtable) 'A-dispatch 'A)))

(deftest reader-macro-conflict.7 ()
  (signals (reader-macro-conflict)
    (merge-readtables-into (make-readtable) 'A-dispatch 'A-dispatch-as-X)))

(deftest reader-macro-conflict.8 ()
  (signals-not (reader-macro-conflict)
    (merge-readtables-into (make-readtable) 'A 'A)))

(deftest reader-macro-conflict.9 ()
  (signals-not (reader-macro-conflict)
    (merge-readtables-into (make-readtable) 'A-dispatch 'A-dispatch)))


(deftest readtable-does-not-exist.1 ()
  (signals (readtable-does-not-exist)
    (ensure-readtable 'does-not-exist)))


(deftest readtable-does-already-exist.1 ()
  (signals (readtable-does-already-exist)
    (make-readtable 'A)))

(deftest readtable-does-already-exist.2 ()
  (signals (readtable-does-already-exist)
    (make-readtable 'A)))

(deftest readtable-does-already-exist.3 ()
  (let ((rt (make-readtable 'does-not-exist :merge '(:standard A B))))
    (declare (ignore rt))
    (unwind-protect
         (is (equal (read-with-readtable
                     (continue-condition 'readtable-does-already-exist
                       (make-readtable 'does-not-exist
                                       :merge '(:standard A C)))
                     "(A B C)")
                    '(:a b :c)))
      (unregister-readtable 'does-not-exist))))

(deftest defreadtable.1 ()
  (unwind-protect
       (signals (reader-macro-conflict)
         (eval `(defreadtable does-not-exist
                  (:merge A A-as-X))))
    (unregister-readtable 'does-not-exist)))

(deftest defreadtable.2 ()
  (unwind-protect
       (signals-not (t)
         (eval `(defreadtable does-not-exist
                  (:fuse A A-as-X))))
    (unregister-readtable 'does-not-exist)))

(deftest readtable-documentation ()
  (unwind-protect
       (let ((readtable (eval '(defreadtable does-not-exist
                                "docstring"))))
         (is (equal (documentation 'does-not-exist 'readtable) "docstring"))
         (is (equal (documentation readtable 'readtable) "docstring"))
         (setf (documentation 'does-not-exist 'readtable) "docstring2")
         (is (equal (documentation 'does-not-exist 'readtable) "docstring2"))
         (setf (documentation readtable 'readtable) "docstring3")
         (is (equal (documentation 'does-not-exist 'readtable) "docstring3"))
         (rename-readtable 'does-not-exist 'new-name)
         (is (null (documentation 'does-not-exist 'readtable)))
         (is (equal (documentation 'new-name 'readtable) "docstring3")))
    (unregister-readtable 'does-not-exist)
    (unregister-readtable 'new-name)))

(defun test (&key (debug nil) (print 'unexpected) (describe 'unexpected))
  ;; Bind *PACKAGE* so that names of tests printed have package names,
  ;; and M-. works on them in Slime.
  (let ((*package* (find-package :common-lisp))
        (*print-duration* nil)
        (*print-compactly* nil)
        (*defer-describe* nil))
    (print (try (find-package :named-readtables-test)
                :debug debug :print print :describe describe))))

#+nil
(test)
