
(in-package :lispn)

;; TODO namespace-let

;; renaming candidate:
;; namespace-let 
;; where --- eazy to type in, but incosistent with common lisp
;; bind --- I dont think it is cool, similar reason to where
;; overwriting cl:let --- well, maybe optional

(defmacro namespace-let (bindings &body body)
  "Bindings is a list of bindings where each car is of form (NAMESPACE NAME),
 or a symbol NAME for a variable namespace.

 function, macro, label, symbol-macro, handler, restart is by default recognized as a namespace.

Example:
(namespace-let ((#'x (y) (1+ y)) ; -- equivalent to ((function x) (y) (1+ y))
                ((macro x) (y) (1+ y))
                ((macro y) (y) (1+ y))
                (#'x (y) (1+ y))
                ((label y) (y) (y y))
                ((symbol-macro sm) 0)
                (b 0))
  (let ((b 1))
    (print :x)))
"
  (%pickone (reverse bindings) `((progn ,@body))))

(setf (macro-function 'nslet)
      (macro-function 'namespace-let))

;; mutual recursion

(defun %pickone (bindings body)
  (if bindings
      (destructuring-bind ((specifier &rest definition) &rest rest) bindings
        (cond
          ((listp specifier)
           (destructuring-bind (namespace name) specifier
             (case namespace
               ;; function-like binding
               (function
                (%merge 'flet name definition body rest))
               (label
                (%merge 'labels name definition body rest))
               (macro
                (%merge 'macrolet name definition body rest))
               ;; variable-like binding
               (symbol-macro
                (%merge 'symbol-macrolet name definition body rest))
               ;; handler binding
               (handler
                (%merge 'handler-bind name definition body rest))
               (restart
                (%merge 'restart-bind name definition body rest))
               (otherwise
                (if (namespace-boundp namespace)
                    (%pickone rest (%wrap namespace name definition body))
                    (error "unknown namespace ~a !" namespace))))))
          ((symbolp specifier)
           (%merge 'let specifier definition body rest))))
      `(progn ,@body)))

(defun %merge (kind name def body rest)
  (%pickone
   rest
   (handler-case
       (destructuring-bind ((kind2 bindings &rest newbody)) body
         (assert (eq kind kind2))
         `((,kind ((,name ,@def) ,@bindings) ,@newbody)))
     (error ()
       `((,kind ((,name ,@def)) ,@body))))))

(defun %wrap (namespace name definition body)
  (with-slots (accessor type) (symbol-namespace namespace)
     (with-gensyms (temp)
       `((let ((,temp ,@definition))
           (declare (type (,type) ,temp))
           (macrolet ((,accessor (&whole whole x)
                        (if (equal x '(quote ,name))
                            ',temp
                            whole)))
             ,@body))))))

;; lexical nickname for packages : abondoned

#+nil
(defun %bind-package (def body rest-bindings)
  ;;  This one is special. All symbols are interned in the current package at
  ;; the read time, but this binder parse them again, then intern in the
  ;; target package.
  ;;  Also, it runs in compile-time, not in runtime. Therefore, the target
  ;; package should also exist in compile-time.
  (assert (find-package def) nil
          "The specified package ~a should exist in compilation time!" def)
  (let ((pkg (find-package def)))
    (%pickone
     rest-bindings
     (maptree (lambda (s)
                (match s
                  ((symbol name)
                   (intern name pkg))
                  (_ s)))
              body))))

#+nil
(defun maptree (fn tree)
  (match tree
    ((cons car cdr)
     (cons (maptree fn car)
           (maptree fn cdr)))
    ((type array)
     (let ((a (copy-array tree)))
       (dotimes (i (array-total-size a) a)
         (setf (row-major-aref a i)
               (funcall fn (row-major-aref tree i))))))
    (_ (funcall fn tree))))

#+nil
(maptree #'print '(let (x y)
                   (test-let ((a 1))
                     (setf x (lambda () (symbol-test 'a)))
                     (test-let ((a 2))
                       (setf y (lambda () (symbol-test 'a)))))
                   (is (= 1 (funcall x)))
                   (is (= 2 (funcall y)))))
#+nil
(maptree #'print #(a b c d e))
#+nil
(maptree #'print #2a((a b c d e)))
#+nil
(read-from-string "`(a ,b)")

