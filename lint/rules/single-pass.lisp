;;;; single-pass.lisp
;;;;
;;;; Single-pass visitor-based linting rules for performance
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025 Anthony Green

(in-package #:ocicl.lint)

;; Quote-aware walker that tracks when we're inside quoted contexts
(defun walk-forms-with-positions-quote-aware-ctx (ctx visitor)
  "Walk forms while tracking quote contexts using context. VISITOR gets (form ln col quoted-p)."
  (let ((flat-nodes (parse-context-flat-nodes ctx)))
    (when (null flat-nodes)
      (let ((acc nil))
        (let ((stack (mapcar (lambda (tree) (list tree nil))
                            (copy-list (parse-context-parse-trees ctx)))))
          (loop while stack do
               (let* ((item (pop stack))
                      (tree (first item))
                      (quoted-p (second item)))
                 (when (and (listp tree) (getf tree :form))
                   (let* ((form (getf tree :form))
                          (source (getf tree :source))
                          (children (getf tree :children))
                          (file-pos (if (consp source) (first source) source))
                          (new-quoted-p (or quoted-p
                                            (and (consp form) (eq (first form) 'quote)))))
                     (when (and file-pos (numberp file-pos))
                       (let ((computed-line
                              (nth-value 0 (index->line/col
                                            file-pos (parse-context-line-index ctx))))
                             (computed-column
                              (nth-value 1 (index->line/col
                                            file-pos (parse-context-line-index ctx)))))
                         (when (or (not (numberp computed-line))
                                   (not (numberp computed-column)))
                           (format *error-output*
                                   "; BUG: non-numeric position for form ~S: line=~S col=~S ~
                                    file-pos=~S~%"
                                   form computed-line computed-column file-pos))
                         (push (list form computed-line computed-column quoted-p) acc)))
                     (when children
                       ;; push children onto stack with inherited quote context
                       (setf stack (nconc (mapcar (lambda (child) (list child new-quoted-p))
                                                  (copy-list children)) stack))))))))
        (setf flat-nodes (nreverse acc))
        (setf (parse-context-flat-nodes ctx) flat-nodes)))
    (dolist (quad flat-nodes)
      (destructuring-bind (form line-num col-num quoted-p) quad
        (funcall visitor form line-num col-num quoted-p)))))


;; Single-pass dispatcher: run multiple simple pattern checks in one traversal using context
(defun run-single-pass-visitors-ctx (path ctx)
  "Traverse the parse tree once and evaluate a set of fast, local pattern rules using context.
Returns a list of issues."
  (let ((issues nil))
    (when *verbose*
      (logf "; single-pass: running on ~A~%" path))
    (flet ((push-iss (ln col rule msg)
             (push (%make-issue path ln col rule msg) issues)))
      (walk-forms-with-positions-quote-aware-ctx
       ctx
       (lambda (form ln col quoted-p)
         (when (and form (consp form))
           (let ((head (first form)))
             ;; More explicit boolean context patterns
             (when (and (eq head 'if)
                        (= (length form) 3)
                        (eq (third form) nil))
               (push-iss ln col "boolean-context"
                         "Simplify: condition is already boolean, no need for (IF test T NIL)"))
             (when (and (eq head 'if)
                        (= (length form) 3)
                        (eq (third form) t)
                        (eq (second form) nil))
               (push-iss ln col "boolean-context"
                         "Replace (IF test NIL T) with (NOT test)"))

             ;; IF with single branch -> WHEN/UNLESS
             (when (and (eq head 'if)
                        (= (length form) 3))
               (push-iss ln col "if-single-branch"
                         "Use WHEN or UNLESS instead of IF for single-branch conditionals"))
             ;; (+ x 1) -> (1+ x)
             (when (and (eq head '+)
                        (= (length form) 3)
                        (or (and (numberp (second form)) (eql (second form) 1))
                            (and (numberp (third form)) (eql (third form) 1))))
               (let ((var (if (numberp (second form)) (third form) (second form))))
                 (push-iss ln col "plus-one"
                           (format nil "Use (1+ ~A) instead of (+ ~A 1)" var var))))

             ;; (- x 1) -> (1- x)
             (when (and (eq head '-)
                        (= (length form) 3)
                        (numberp (third form))
                        (eql (third form) 1))
               (push-iss ln col "minus-one"
                        (format nil "Use (1- ~A) instead of (- ~A 1)" (second form) (second form))))

             ;; (= x 0) -> (zerop x)
             (when (and (eq head '=)
                        (= (length form) 3)
                        (or (and (numberp (second form)) (zerop (second form)))
                            (and (numberp (third form)) (zerop (third form)))))
               (let ((var (if (numberp (second form)) (third form) (second form))))
                 (push-iss ln col "use-zerop"
                           (format nil "Use (ZEROP ~A) instead of (= ~A 0)" var var))))

             ;; (eql number number) -> (= ...) [DISABLED - unsafe in linting context]
             ;; EQL is safer than = when we can't guarantee both operands are numbers
             ;; (when (and (eq head 'eql)
             ;;            (= (length form) 3)
             ;;            (or (numberp (second form)) (numberp (third form))))
             ;;   (push-iss ln col "eql-on-numbers" "Use = instead of EQL for comparing numbers"))

             ;; (when (not ...)) -> unless
             (when (and (eq head 'when)
                        (consp (second form))
                        (eq (first (second form)) 'not))
               (push-iss ln col "when-for-unless"
                         (format nil "Use (UNLESS ~A ...) instead of (WHEN (NOT ~A) ...)"
                                 (second (second form)) (second (second form)))))

            ;; Suggest ALEXANDRIA:WHEN-LET for conditional binding patterns
            (when (and (eq head 'when)
                       (consp (second form))
                       (eq (first (second form)) 'not)
                       (consp (second (second form)))
                       (eq (first (second (second form))) 'null))
              (push-iss ln col "use-alexandria"
                        "Consider using ALEXANDRIA:WHEN-LET for conditional binding"))

             ;; (unless (not ...)) -> when
             (when (and (eq head 'unless)
                        (consp (second form))
                        (eq (first (second form)) 'not))
               (push-iss ln col "unless-for-when"
                         (format nil "Use (WHEN ~A ...) instead of (UNLESS (NOT ~A) ...)"
                                 (second (second form)) (second (second form)))))

             ;; (setf v (+ v n)) -> (incf v n)
             (when (and (eq head 'setf)
                        (= (length form) 3)
                        (symbolp (second form))
                        (consp (third form))
                        (eq (first (third form)) '+)
                        (= (length (third form)) 3)
                        (or (eq (second (third form)) (second form))
                            (eq (third (third form)) (second form))))
               (let ((var (second form))
                     (amount (if (eq (second (third form)) (second form))
                                 (third (third form))
                                 (second (third form)))))
                 (push-iss ln col "setf-incf"
                           (format nil "Use (INCF ~A~@[ ~A~]) instead of (SETF ~A (+ ~A ~A))"
                                   var (unless (eql amount 1) amount) var var amount))))

             ;; (setf var (cons item var)) -> (push item var)
             (when (and (eq head 'setf)
                        (= (length form) 3)
                        (consp (third form))
                        (eq (first (third form)) 'cons)
                        (= (length (third form)) 3)
                        (equal (second form) (third (third form))))
               (let ((var (second form))
                     (item (second (third form))))
                 (push-iss ln col "use-push"
                           (format nil "Use (PUSH ~A ~A) instead of (SETF ~A (CONS ~A ~A))"
                                   item var var item var))))

             ;; (first (rest x)) -> (second x)
             (when (and (eq head 'car)
                        (= (length form) 2)
                        (consp (second form))
                        (eq (first (second form)) 'cdr))
               (let ((expr (second (second form))))
                 (push-iss ln col "car-cdr"
                           (format nil "Use (CADR ~A) instead of (CAR (CDR ~A))" expr expr))))

             ;; (rest (rest x)) -> (cddr x)
             (when (and (eq head 'cdr)
                        (= (length form) 2)
                        (consp (second form))
                        (eq (first (second form)) 'cdr))
               (let ((expr (second (second form))))
                 (push-iss ln col "cdr-cdr"
                           (format nil "Use (CDDR ~A) instead of (CDR (CDR ~A))" expr expr))))

             ;; Preference hints: FIRST/REST instead of CAR/CDR
             (when (eq head 'car)
               (push-iss ln col "use-first-rest" "Use FIRST instead of CAR for better readability"))
             (when (eq head 'cdr)
               (push-iss ln col "use-first-rest" "Use REST instead of CDR for better readability"))

             ;; (lambda (x) x) -> identity
             (when (and (eq head 'lambda)
                        (>= (length form) 3)
                        (listp (second form))
                        (= (length (second form)) 1)
                        (eq (third form) (first (second form))))
               (push-iss ln col "use-identity" "Use #'IDENTITY instead of (LAMBDA (X) X)"))

             ;; (lambda () const) -> constantly
             (when (and (eq head 'lambda)
                        (>= (length form) 3)
                        (listp (second form))
                        (null (second form))
                        (atom (third form)))
               (push-iss ln col "use-constantly"
                         (format nil "Use (CONSTANTLY ~A) instead of (LAMBDA () ~A)"
                                 (third form) (third form))))

             ;; setq preference vs setf (skip if quoted)
             (when (and (eq head 'setq) (not quoted-p))
               (push-iss ln col "setf-vs-setq" "Prefer SETF over SETQ for consistency"))

             ;; EVAL usage is a red flag
             (when (eq head 'eval)
               (push-iss ln col "eval-usage" "Avoid EVAL (consider alternatives like FUNCALL or macros)"))

             ;; (= (length X) 0) -> null X
             (when (and (eq head '=)
                        (= (length form) 3)
                        (or (and (consp (second form))
                                 (eq (first (second form)) 'length)
                                 (eql (third form) 0))
                            (and (consp (third form))
                                 (eq (first (third form)) 'length)
                                 (eql (second form) 0))))
               (let ((list-expr (if (consp (second form))
                                    (second (second form))
                                    (second (third form)))))
                 (push-iss ln col "length-zero"
                           (format nil "Use (NULL ~A) instead of (= (LENGTH ~A) 0)"
                                   list-expr list-expr))))

             ;; Case-insensitive compare: use STRING-EQUAL
             (when (and (eq head 'equal)
                        (= (length form) 3)
                        (or (and (consp (second form)) (eq (first (second form)) 'string-downcase))
                            (and (consp (third form)) (eq (first (third form)) 'string-downcase))))
               (push-iss ln col "use-string-equal"
                         "Use STRING-EQUAL for case-insensitive string comparison"))

             ;; Case-insensitive char compare: use CHAR-EQUAL
             (when (and (eq head 'equal)
                        (= (length form) 3)
                        (or (and (consp (second form)) (eq (first (second form)) 'char-downcase))
                            (and (consp (third form)) (eq (first (second form)) 'char-downcase))))
              (push-iss ln col "use-char-equal"
                        "Use CHAR-EQUAL for case-insensitive character comparison"))

             ;; OR of many (EQL X v) -> MEMBER
             (when (and (eq head 'or)
                        (> (length form) 3)
                        (every (lambda (clause)
                                 (and (consp clause)
                                      (eq (first clause) 'eql)
                                      (= (length clause) 3)))
                               (rest form)))
               (push-iss ln col "use-member"
                         "Use MEMBER instead of multiple OR with EQL tests"))

             ;; APPEND single arg with NIL -> COPY-LIST
             (when (and (eq head 'append)
                        (= (length form) 3)
                        (null (third form)))
               (push-iss ln col "append-single" "Use COPY-LIST instead of (APPEND x NIL)"))

             ;; APPLY with non-list final args -> FUNCALL - DISABLED (too many false positives)
             ;; Cannot distinguish between variables containing lists vs non-lists at lint time
             ;; Common legitimate patterns like (apply #'append chunks) get flagged incorrectly
             ;; (when (and (eq head 'apply)
             ;;            (>= (length form) 3)
             ;;            (not (some #'listp (cddr form))))
             ;;   (push-iss ln col "apply-for-funcall"
             ;;            "Use FUNCALL instead of APPLY with non-list arguments"))

            ;; COND with single clause -> IF
            (when (and (eq head 'cond)
                       (= (length form) 2))
              (push-iss ln col "cond-vs-if" "Use IF instead of COND with single clause"))

            ;; COND without default -> warn
            (when (and (eq head 'cond)
                       (> (length form) 1))
              (let ((last-clause (first (last (rest form)))))
                (unless (and last-clause (member (first last-clause) '(t otherwise)))
                  (push-iss ln col "cond-without-default" "COND without T or OTHERWISE clause"))))

            ;; COND of multiple TYPEP tests -> TYPECASE
            (when (and (eq head 'cond)
                       (>= (length (rest form)) 2)
                       (every (lambda (clause)
                                (and (consp clause)
                                     (consp (first clause))
                                     (eq (first (first clause)) 'typep)))
                              (rest form)))
              (when *verbose*
                (logf "; single-pass: matched COND->TYPECASE at ~D:~D: ~S~%" ln col form))
              (push-iss ln col "use-typecase"
                        "Use TYPECASE instead of COND with multiple TYPEP tests"))

             ;; EQUAL with NIL -> NULL/NOT
             (when (and (eq head 'equal)
                        (= (length form) 3)
                        (or (null (second form)) (null (third form))))
               (push-iss ln col "equal-with-nil" "Use NULL or NOT instead of EQUAL with NIL"))

             ;; (NOT (NULL x)) -> simplify
             (when (and (eq head 'not)
                        (consp (second form))
                        (eq (first (second form)) 'null))
               (push-iss ln col "not-null" "Simplify (NOT (NULL x)) to just x"))

             ;; 'NIL quoted -> NIL
             (when (and (eq head 'quote)
                        (null (second form)))
               (push-iss ln col "quoted-nil" "Use NIL instead of 'NIL"))

             ;; PROGN single form -> redundant
             (when (and (eq head 'progn)
                        (= (length form) 2))
               (push-iss ln col "redundant-progn" "PROGN with single form is redundant"))

             ;; Nested WHEN/UNLESS can be combined or flattened
             ;; Only flag if the nested form is the ONLY body form (no other expressions after it)
             (when (and (member head '(when unless))
                        (= (length form) 3)  ; Only condition + single nested form
                        (consp (third form))
                        (member (first (third form)) '(when unless)))
               (push-iss ln col "avoid-nesting"
                         "Avoid nested WHEN/UNLESS; consider combining conditions"))

             ;; BLOCK with a single body form is often redundant
             (when (and (eq head 'block)
                        (>= (length form) 2)
                        (= (length (cddr form)) 1))
               (push-iss ln col "redundant-block" "BLOCK with a single form is redundant"))

             ;; AND/OR simplifications with constants (but avoid common idioms)
             (when (and (eq head 'and)
                        (> (length form) 2))
               (let ((args (rest form)))
                 (cond
                   ;; (and ... nil) or (and nil ...) - always nil
                   ((member nil args)
                    (push-iss ln col "and-or-simplification" "AND with NIL always returns NIL"))
                   ;; (and t expr) - can be simplified to expr
                   ((and (eq (first args) t) (= (length args) 2))
                    (push-iss ln col "and-or-simplification" "AND with leading T is redundant"))
                   ;; (and expr1 expr2 ... t) where t is not the last element - redundant t
                   ((and (member t args)
                         (not (eq (car (last args)) t))
                         (> (length args) 2))
                    (push-iss ln col "and-or-simplification" "Redundant T in AND expression")))))
             (when (and (eq head 'or)
                        (> (length form) 2))
               (let ((args (rest form)))
                 (cond
                   ;; (or ... t) or (or t ...) - always t
                   ((member t args)
                    (push-iss ln col "and-or-simplification" "OR with T always returns T"))
                   ;; (or nil expr) - can be simplified to expr
                   ((and (eq (first args) nil) (= (length args) 2))
                    (push-iss ln col "and-or-simplification" "OR with leading NIL is redundant"))
                   ;; (or expr1 expr2 ... nil) where nil is not the last element - redundant nil
                   ((and (member nil args)
                         (not (eq (car (last args)) nil))
                         (> (length args) 2))
                    (push-iss ln col "and-or-simplification" "Redundant NIL in OR expression")))))

             ;; TYPEP with primitive types T or NIL
             (when (and (eq head 'typep)
                        (= (length form) 3)
                        (or (eq (third form) t)
                            (null (third form))))
               (push-iss ln col "typep-primitive" "Avoid TYPEP with T or NIL; result is trivial"))

             ;; (SETF v (- v n)) -> DECF
             (when (and (eq head 'setf)
                        (= (length form) 3)
                        (consp (third form))
                        (eq (first (third form)) '-)
                        (= (length (third form)) 3)
                        (equal (second form) (second (third form))))
               (let ((var (second form)) (amount (third (third form))))
                 (push-iss ln col "setf-decf"
                          (format nil "Use (DECF ~A ~A) instead of (SETF ~A (- ~A ~A))"
                                  var amount var var amount))))

             ;; Unnecessary (lambda (x) (f x)) wrappers on map fns
             (when (and (member head '(mapcar mapc mapcan maplist mapcon))
                        (consp (second form))
                        (eq (first (second form)) 'lambda)
                        (= (length (second form)) 3)
                        (= (length (second (second form))) 1)
                        (consp (third (second form)))
                        (equal (second (second form)) (rest (third (second form)))))
               (push-iss ln col "unnecessary-lambda" "Remove unnecessary lambda wrapper"))

             ;; DEFVAR without initial value
             (when (and (eq head 'defvar)
                        (= (length form) 2))
               (push-iss ln col "defvar-without-value" "DEFVAR without initial value"))

             ;; OPEN/CLOSE in LET -> use WITH-OPEN-FILE
             (when (and (member head '(let let*))
                       (consp (second form))
                       (some (lambda (binding)
                               (and (consp binding)
                                    (consp (rest binding))
                                    (consp (second binding))
                                    (eq (first (second binding)) 'open)))
                             (second form)))
               (when *verbose*
                 (logf "; single-pass: matched LET OPEN at ~D:~D: ~S~%" ln col form))
               (push-iss ln col "use-with-open-file" "Use WITH-OPEN-FILE instead of manual OPEN/CLOSE"))

             ;; MAPCAR used only for side-effects - simplified heuristic
             ;; Only flag in obvious cases where result is clearly unused
             (when (and (eq head 'mapcar) (>= (length form) 3))
               ;; Simple heuristic: flag if MAPCAR function is clearly for side effects
               (let* ((mapper-fn (second form))
                      (suggests-side-effects
                       (cond
                         ;; (mapcar #'print ...) - clear side effects
                         ((and (consp mapper-fn) (eq (first mapper-fn) 'function)
                               (member (second mapper-fn) '(print princ prin1 terpri)))
                          t)
                         ;; (mapcar (lambda (x) (print x)) ...) - lambda with side effects only
                         ((and (consp mapper-fn) (eq (first mapper-fn) 'lambda)
                               (let ((body (cddr mapper-fn)))
                                 (and (= (length body) 1)
                                      (let ((stmt (first body)))
                                        (and (consp stmt)
                                             (cond
                                               ;; (format t ...) = side effect, (format nil ...) = not side effect
                                               ((eq (first stmt) 'format)
                                                (and (>= (length stmt) 2)
                                                     (eq (second stmt) t)))
                                               ;; Other clear side effects
                                               ((member (first stmt) '(print princ prin1 terpri))
                                                t)
                                               (t nil)))))))
                          t)
                         (t nil))))
                 (when suggests-side-effects
                   (push-iss ln col "mapcar-for-mapc" "Consider MAPC if you don't need the result list"))))

             ;; DOLIST with a single function call body -> MAPC
             (when (and (eq head 'dolist)
                        (consp (second form))
                        (let* ((var (caadr form))
                               (seq (cadadr form))
                               (body (cddr form))
                               ;; Allow leading (declare ...) forms in the body
                               (body* (let ((b body))
                                        (loop while (and b (consp (first b)) (eq (caar b) 'declare)) do
                                             (setf b (rest b)))
                                        b)))
                          (and (= (length body*) 1)
                               (let* ((call (first body*)))
                                 (when *verbose*
                                   (logf "; checking DOLIST var=~S seq=~S body=~S~%" var seq call))
                                 (or
                                  ;; exact shape (fn var)
                                  (and (consp call)
                                       (symbolp (first call))
                                       (= (length call) 2)
                                       (eq (second call) var))
                                  ;; funcall shapes: (funcall fn var) or (funcall #'fn var)
                                  (and (consp call)
                                       (eq (first call) 'funcall)
                                       (= (length call) 3)
                                       (eq (third call) var)))))))
               (let* ((var (caadr form))
                      (seq (cadadr form))
                      (body (cddr form))
                      (body* (let ((b body))
                               (loop while (and b (consp (first b)) (eq (caar b) 'declare)) do
                                    (setf b (rest b)))
                               b))
                      (call (first body*))
                      (fn (cond
                            ((and (consp call) (eq (first call) 'funcall))
                             (let ((f (second call)))
                               (if (and (consp f) (eq (first f) 'function)) (second f) f)))
                            (t (first call)))))
                 (when *verbose*
                   (logf "; matched DOLIST -> MAPC: fn=~S seq=~S var=~S~%" fn seq var))
                 (push-iss ln col "use-mapc"
                           (format nil "Use (~S #'~S ~S) instead of (DOLIST (~S ~S) (~S ~S))"
                                   'mapc fn seq var seq fn var))))

             ;; Unnecessary PROGN in IF/WHEN (only flag PROGN in "then" branch with no else)
             (when (and (eq head 'if)
                        (= (length form) 3)  ; if with no else clause: (if test then)
                        (consp (third form)) ; then-branch is a list
                        (eq (first (third form)) 'progn)) ; then-branch is (progn ...)
               (push-iss ln col "progn-in-if" "Unnecessary PROGN in IF with no else - use WHEN instead"))
             (when (and (eq head 'when)
                        (>= (length form) 3)
                        (consp (third form))
                        (eq (first (third form)) 'progn))
               (push-iss ln col "progn-in-when" "Unnecessary PROGN in WHEN"))

             ;; Quote keyword or T
             (when (and (eq head 'quote)
                        (keywordp (second form)))
               (push-iss ln col "quote-keyword" "Keywords are self-evaluating - no need to quote"))
             (when (and (eq head 'quote)
                        (eq (second form) t))
               (push-iss ln col "quote-true" "Use T instead of 'T"))

             ;; (SETQ v (+ v n)) -> INCF
             (when (and (eq head 'setq)
                        (= (length form) 3)
                        (consp (third form))
                        (eq (first (third form)) '+)
                        (= (length (third form)) 3)
                        (equal (second form) (second (third form))))
               (let ((var (second form)) (amount (third (third form))))
                 (push-iss ln col "setq-incf"
                          (format nil "Use (INCF ~A ~A) instead of (SETQ ~A (+ ~A ~A))"
                                  var amount var var amount))))

             ;; Smart find/position membership suggestion - simplified approach
             ;; This rule is too complex to implement safely in the current architecture
             ;; Disable for now to avoid false positives like (1+ (position #\Tab line))
             ;; (when (and (member head '(find position))
             ;;            (= (length form) 3)
             ;;            (not (keywordp (second form))))
             ;;   (push-iss ln col "use-member"
             ;;            (format nil "Consider MEMBER instead of ~A for list membership test" head)))

             ;; FIND-IF with NOT -> FIND-IF-NOT
             (when (and (eq head 'find-if)
                        (consp (second form))
                        (eq (first (second form)) 'lambda)
                        (consp (third (second form)))
                        (eq (first (third (second form))) 'not))
               (push-iss ln col "use-find-if-not"
                         "Use FIND-IF-NOT instead of FIND-IF with negated predicate"))

             ;; POSITION-IF with NOT -> POSITION-IF-NOT
             (when (and (eq head 'position-if)
                        (consp (second form))
                        (eq (first (second form)) 'lambda)
                        (consp (third (second form)))
                        (eq (first (third (second form))) 'not))
               (push-iss ln col "use-position-if-not"
                         "Use POSITION-IF-NOT instead of POSITION-IF with negated predicate"))

             ;; REMOVE-IF with NOT -> REMOVE-IF-NOT
             (when (and (eq head 'remove-if)
                        (consp (second form))
                        (eq (first (second form)) 'lambda)
                        (consp (third (second form)))
                        (eq (first (third (second form))) 'not))
               (push-iss ln col "use-remove-if-not"
                         "Use REMOVE-IF-NOT instead of REMOVE-IF with negated predicate"))

             ;; Ecclesia-based validations
             (when (eq head 'destructuring-bind)
               (let* ((pattern (second form))
                      (source (third form))
                      (body (cdddr form)))
                 (handler-case
                     (progn (parse-destructuring-bind pattern source body) nil)
                   (condition (c1)
                     (push-iss ln col "destructuring-bind-invalid" (princ-to-string c1))))))
             (when (member head '(defun defmacro))
               (let ((lambda-list (third form)))
                 (handler-case
                     (progn
                       (if (eq head 'defmacro)
                           (canonicalize-macro-lambda-list lambda-list)
                           (canonicalize-ordinary-lambda-list lambda-list))
                       nil)
                   (condition (c2)
                     (push-iss ln col "lambda-list-invalid" (princ-to-string c2))))))

             ;; === Moved from AST rules ===

             ;; EQ misuse with numbers/chars/strings
             (when (eq head 'eq)
               (let ((a (second form)) (b (third form)))
                 (when (or (numberp a) (numberp b)
                           (characterp a) (characterp b)
                           (stringp a) (stringp b))
                   (push-iss ln col "eq-misuse"
                             "Use EQL/= or STRING= for numbers/chars/strings"))))

             ;; CASE duplicate keys
             (when (eq head 'case)
               (let ((clauses (cddr form))
                     (seen (make-hash-table :test 'equal)))
                 (dolist (clause clauses)
                   (when (consp clause)
                     (let ((keys (first clause)))
                       (cond
                         ((listp keys)
                          (dolist (k keys)
                            (when (and (or (symbolp k) (characterp k) (integerp k))
                                       (gethash k seen))
                              (push-iss ln col "case-duplicate-key"
                                        (format nil "Duplicate CASE key: ~S" k)))
                            (setf (gethash k seen) t)))
                         ((or (symbolp keys) (characterp keys) (integerp keys))
                          (when (gethash keys seen)
                            (push-iss ln col "case-duplicate-key"
                                      (format nil "Duplicate CASE key: ~S" keys)))
                          (setf (gethash keys seen) t))
                         ;; Ignore other key types
                         (t nil)))))))

             ;; Destructive operations on constants
             (when (member head '(nreverse nconc sort stable-sort delete))
               (let ((arg (second form)))
                 (when (and (consp arg) (eq (first arg) 'quote))
                   (push-iss ln col "destructive-on-constant"
                             (format nil "Destructive operation ~A on quoted constant" head)))))

             ;; Lambda lists with both &optional and &key
             (when (member head '(defun defmacro lambda)) ; lint:suppress lambda-list-invalid
               (let ((lambda-list (if (eq head 'lambda) (second form) (third form))))
                 (when (and (listp lambda-list)  ; Only proceed if lambda-list is actually a list
                            (>= (length form) (if (eq head 'lambda) 2 3))  ; Has minimum elements
                            (member '&optional lambda-list)
                            (member '&key lambda-list))
                   (push-iss ln col "avoid-optional-and-key"
                             "Avoid using both &OPTIONAL and &KEY in the same lambda list"))))

             ;; Reader evaluation #. - this is hard to detect in parsed form
             ;; The original rule checked the raw content, so we'll skip this one for now

             ;; EVAL usage (duplicate rule removed - already handled above)

             ;; Missing docstrings in defun/defmacro
             (when (and (member head '(defun defmacro))
                        (not quoted-p)
                        (>= (length form) 3))
               (let* ((name (second form))
                      (lambda-list (third form))
                      (body (cdddr form)))
                 (when (and (symbolp name)
                            (or (null body)
                                (not (stringp (first body)))))
                   (push-iss ln col "missing-docstring"
                             (format nil "~(~A~) ~A missing docstring" head name)))))

             ;; Missing docstrings in defclass
             (when (and (eq head 'defclass) (>= (length form) 2))
               (let* ((name (second form))
                      (options (cddddr form))
                      (doc-p (loop for opt in options
                                   thereis (and (consp opt)
                                                (eql (first opt) :documentation)))))
                 (unless doc-p
                   (push-iss ln col "missing-docstring"
                             (format nil "defclass ~A missing :documentation option" name)))))

             ;; Missing docstrings in defpackage
             (when (and (eq head 'defpackage) (>= (length form) 2))
               (let* ((name (second form))
                      (options (cddr form))
                      (doc-p (loop for opt in options
                                   thereis (and (consp opt)
                                                (eql (first opt) :documentation)))))
                 (unless doc-p
                   (push-iss ln col "missing-docstring"
                             (format nil "defpackage ~A missing :documentation option" name)))))

             ;; ===== ADDITIONAL LISP-CRITIC RULES =====

             ;; Sets global variables - DISABLED (too many false positives without lexical scope analysis)
             ;; (when (and (member head '(setq setf))
             ;;            (>= (length form) 3)
             ;;            (symbolp (second form))
             ;;            (not (boundp (second form))))
             ;;   (push-iss ln col "sets-globals"
             ;;             (format nil "Don't use global variables: ~S" (second form))))

             ;; Sets parameters - DISABLED (too many false positives without lexical scope analysis)
             ;; (when (and (member head '(setq setf incf decf))
             ;;            (>= (length form) 3)
             ;;            (symbolp (second form)))
             ;;   (push-iss ln col "sets-parameters"
             ;;             (format nil "Bad style to reassign variables like ~S" (second form))))

             ;; TYPEP with primitive types
             (when (and (eq head 'typep)
                        (= (length form) 3)
                        (consp (third form))
                        (eq (first (third form)) 'quote)
                        (member (second (third form)) '(integer number string cons atom list)))
               (push-iss ln col "typep-primitive"
                         (format nil "Use ~SP instead of TYPEP for basic type ~S"
                                 (second (third form)) (second (third form)))))

             ;; COND test with no expression
             (when (and (eq head 'cond)
                        (> (length form) 1))
               (dolist (clause (rest form))
                 (when (and (consp clause)
                            (= (length clause) 1))
                   (push-iss ln col "cond-test-no-exp"
                             (format nil "COND branch with test ~S but no action - try (OR ~S ...)"
                                     (first clause) (first clause))))))

             ;; IF with no else
             (when (and (eq head 'if)
                        (= (length form) 3))
               (push-iss ln col "if-no-else"
                         "IF without else branch: use WHEN or UNLESS instead"))

             ;; Nested AND inside AND or OR inside OR
             (when (and (eq head 'and)
                        (> (length form) 1))
               (dolist (arg (rest form))
                 (when (and (consp arg) (eq (first arg) 'and))
                   (push-iss ln col "nested-and-or"
                             "Nested AND inside AND can be flattened"))))
             (when (and (eq head 'or)
                        (> (length form) 1))
               (dolist (arg (rest form))
                 (when (and (consp arg) (eq (first arg) 'or))
                   (push-iss ln col "nested-and-or"
                             "Nested OR inside OR can be flattened"))))

             ;; Needless COND patterns
             (when (and (eq head 'cond)
                        (= (length form) 3)
                        (equal (second form) '(t t))
                        (equal (third form) '(t nil)))
               (push-iss ln col "needless-cond"
                         "Unnecessary COND - can be simplified"))
             (when (and (eq head 'cond)
                        (= (length form) 3)
                        (equal (second form) '(t nil))
                        (equal (third form) '(t t)))
               (push-iss ln col "needless-cond-not"
                         "COND can be replaced with NOT"))

             ;; Needless IF patterns
             (when (and (eq head 'if)
                        (= (length form) 4)
                        (eq (third form) t)
                        (eq (fourth form) nil))
               (push-iss ln col "needless-if"
                         "No IF needed - just use the test condition"))
             (when (and (eq head 'if)
                        (= (length form) 3)
                        (eq (third form) t))
               (push-iss ln col "needless-if"
                         "No IF needed - just use the test condition"))

             ;; Needless WHEN/UNLESS with constants
             (when (and (member head '(when unless))
                        (= (length form) 3)
                        (member (third form) '(t nil)))
               (push-iss ln col "needless-when"
                         (format nil "No ~S needed with constant ~S" head (third form))))

             ;; IF->OR pattern: (IF test T else) -> (OR test else)
             (when (and (eq head 'if)
                        (= (length form) 4)
                        (eq (third form) t))
               (push-iss ln col "if-or"
                         "Use (OR test else) instead of (IF test T else)"))

             ;; COND->OR pattern
             (when (and (eq head 'cond)
                        (= (length form) 3)
                        (equal (second form) '(t t))
                        (consp (third form))
                        (eq (first (third form)) t))
               (push-iss ln col "cond-or"
                         "Use (OR test else) instead of (COND (test T) (T else))"))

             ;; AND ending with T
             (when (and (eq head 'and)
                        (> (length form) 2)
                        (eq (first (last form)) t))
               (push-iss ln col "needless-and-t"
                         "Trailing T in AND expression is redundant"))

             ;; OR ending with NIL
             (when (and (eq head 'or)
                        (> (length form) 2)
                        (eq (first (last form)) nil))
               (push-iss ln col "needless-or-nil"
                         "Trailing NIL in OR expression is redundant"))

             ;; Single argument AND/OR
             (when (and (eq head 'and)
                        (= (length form) 2))
               (push-iss ln col "needless-and"
                         "AND with single argument is redundant"))
             (when (and (eq head 'or)
                        (= (length form) 2))
               (push-iss ln col "needless-or"
                         "OR with single argument is redundant"))

             ;; (IF test NIL T) -> (NOT test)
             (when (and (eq head 'if)
                        (= (length form) 4)
                        (eq (third form) nil)
                        (eq (fourth form) t))
               (push-iss ln col "if-for-not"
                         "Use (NOT test) instead of (IF test NIL T)"))

             ;; Quote numbers
             (when (and (eq head 'quote)
                        (numberp (second form)))
               (push-iss ln col "quote-number"
                         (format nil "Don't quote numbers like ~S" (second form))))

             ;; NTH with 1 -> CDR/REST
             (when (and (eq head 'nth)
                        (= (length form) 3)
                        (eql (second form) 1))
               (push-iss ln col "nth-for-cdr"
                         "Use REST or CDR instead of (NTH 1 ...)"))

             ;; CONS with NIL -> LIST
             (when (and (eq head 'cons)
                        (= (length form) 3)
                        (eq (third form) nil))
               (push-iss ln col "cons-with-nil"
                         (format nil "Use (LIST ~S) instead of (CONS ~S NIL)"
                                 (second form) (second form))))

             ;; CONS with LIST
             (when (and (eq head 'cons)
                        (= (length form) 3)
                        (consp (third form))
                        (eq (first (third form)) 'list))
               (let* ((first-elem (second form))
                      (rest-elems (rest (third form))))
                 (push-iss ln col "cons-list"
                           (format nil "Use (LIST ~S~{ ~S~}) instead of (CONS ~S (LIST~{ ~S~}))"
                                   first-elem rest-elems first-elem rest-elems))))

             ;; (APPEND (LIST x) y) -> (CONS x y)
             (when (and (eq head 'append)
                        (= (length form) 3)
                        (consp (second form))
                        (eq (first (second form)) 'list)
                        (= (length (second form)) 2))
               (push-iss ln col "append-list-list"
                         (format nil "Use (CONS ~S ~S) instead of (APPEND (LIST ~S) ~S)"
                                 (second (second form)) (third form)
                                 (second (second form)) (third form))))

             ;; CONCATENATE 'LIST
             (when (and (eq head 'concatenate)
                        (>= (length form) 2)
                        (consp (second form))
                        (eq (first (second form)) 'quote)
                        (eq (second (second form)) 'list))
               (push-iss ln col "concatenate-list"
                         "Use APPEND for list construction instead of CONCATENATE"))

             ;; (CONS (CONS x y) z) -> (ACONS x y z)
             (when (and (eq head 'cons)
                        (= (length form) 3)
                        (consp (second form))
                        (eq (first (second form)) 'cons)
                        (= (length (second form)) 3))
               (push-iss ln col "cons-cons-acons"
                         "Use ACONS for alist construction"))

             ;; Nested CONS -> LIST*
             (when (and (eq head 'cons)
                        (= (length form) 3)
                        (consp (third form))
                        (eq (first (third form)) 'cons))
               (push-iss ln col "cons-cons-list*"
                         "Consider LIST* for nested CONS expressions"))

             ;; EQ/EQUAL/EQUALP -> EQL (only for numbers, characters, keywords)
             (when (and (member head '(eq equal equalp))
                        (= (length form) 3))
               (let ((arg1 (second form))
                     (arg2 (third form)))
                 (when (or
                        ;; Comparing with numbers - EQL is safer than EQ
                        (numberp arg1) (numberp arg2)
                        ;; Comparing with characters - EQL is safer than EQ
                        (characterp arg1) (characterp arg2)
                        ;; Comparing with keywords - EQL is more semantic
                        (keywordp arg1) (keywordp arg2))
                   (push-iss ln col "use-eql"
                             (format nil "Use EQL instead of ~S for comparing numbers, characters, or keywords"
                                     head)))))

             ;; Adding zero
             (when (and (eq head '+)
                        (= (length form) 3)
                        (or (eql (second form) 0) (eql (third form) 0)))
               (push-iss ln col "add-zero"
                         "Adding zero has no effect"))

             ;; INCF/DECF with 1
             (when (and (member head '(incf decf))
                        (= (length form) 3)
                        (eql (third form) 1))
               (push-iss ln col "incf-1"
                         "INCF/DECF default increment is 1 (explicit 1 is redundant)"))

             ;; FLOOR/CEILING/ROUND with /
             (when (and (member head '(floor ceiling round))
                        (= (length form) 2)
                        (consp (second form))
                        (eq (first (second form)) '/))
               (push-iss ln col "floor-with-/"
                         (format nil "~S with two arguments does division already - no need for /" head)))

             ;; Quote FALSE/TRUE
             (when (and (eq head 'quote)
                        (eq (second form) 'false))
               (push-iss ln col "quote-false"
                         "Quoted FALSE evaluates to true in Lisp (use NIL for false)"))
             (when (and (eq head 'quote)
                        (eq (second form) 'true))
               (push-iss ln col "quote-true"
                         "Use T instead of 'TRUE"))

             ;; Multiple optional arguments
             (when (and (eq head 'defun)
                        (>= (length form) 3)
                        (consp (third form)))
               (let ((lambda-list (third form))
                     (optional-count 0))
                 (dolist (param lambda-list)
                   (when (eq param '&optional)
                     (setf optional-count 0))
                   (when (and (> optional-count 0) optional-count)
                     (incf optional-count)))
                 (when (> optional-count 2)
                   (push-iss ln col "multiple-optionals"
                             (format nil "Multiple optional arguments get confusing - use &KEY for ~S"
                                     (second form))))))

             ;; LIST-LENGTH instead of LENGTH
             (when (eq head 'list-length)
               (push-iss ln col "list-length"
                         "LIST-LENGTH is for circular lists (use LENGTH for known-proper lists)"))


             ;; (NOT (CONSP ...)) -> ATOM
             (when (and (eq head 'not)
                        (= (length form) 2)
                        (consp (second form))
                        (eq (first (second form)) 'consp))
               (push-iss ln col "not-consp"
                         "Use ATOM instead of (NOT (CONSP ...))"))

             ;; FIND/MEMBER with :KEY CAR -> ASSOC
             (when (and (member head '(find member))
                        (>= (length form) 4)
                        (member :key (cddr form))
                        (let ((key-pos (position :key (cddr form))))
                          (and key-pos
                               (< (1+ key-pos) (length (cddr form)))
                               (let ((args (cddr form)))
                                 (and (> (length args) key-pos)
                                      (member (nth key-pos args) '('car #'car car)))))))
               (push-iss ln col "find-member-for-assoc"
                         (if (eq head 'find)
                             "Consider ASSOC for association lists (FIND works on sequences including vectors)"
                             "Consider ASSOC instead of MEMBER with :KEY CAR for association lists")))

             ;; READ with bad EOF markers
             (when (and (eq head 'read)
                        (>= (length form) 4)
                        (member (fourth form) '(nil t)))
               (push-iss ln col "constant-bad-eof"
                         (format nil "~S is a bad EOF marker - use a unique runtime-generated object"
                                 (fourth form))))

             ;; RPLACA/RPLACD
             (when (and (eq head 'rplaca)
                        (= (length form) 3))
               (push-iss ln col "rplaca"
                         (format nil "Use (SETF (CAR ~S) ...) instead of RPLACA" (second form))))
             (when (and (eq head 'rplacd)
                        (= (length form) 3))
               (push-iss ln col "rplacd"
                         (format nil "Use (SETF (CDR ~S) ...) instead of RPLACD" (second form))))

             ;; SUBSTITUTE warning
             (when (eq head 'substitute)
               (push-iss ln col "substitute-use"
                         "SUBSTITUTE copies entire sequence (use only when necessary)"))

             ;; PROG warning
             (when (eq head 'prog)
               (push-iss ln col "uses-prog"
                         "PROG is obsolete (use LET, BLOCK, TAGBODY, or LOOP instead)"))

             ;; LET* with single binding
             (when (and (eq head 'let*)
                        (= (length form) 3)
                        (consp (second form))
                        (= (length (second form)) 1))
               (push-iss ln col "let*-single"
                         "No need for LET* with single binding - use LET"))

             ;; SHIFTF with 2 arguments
             (when (and (eq head 'shiftf)
                        (= (length form) 3))
               (push-iss ln col "needless-shiftf"
                         "No need for SHIFTF with just 2 arguments"))

             ;; MULTIPLE-VALUE-LIST warning
             (when (eq head 'multiple-value-list)
               (push-iss ln col "multiple-value-list"
                         "Multiple values avoid consing - MULTIPLE-VALUE-LIST defeats this purpose"))

             ;; DO* with single variable
             (when (and (eq head 'do*)
                        (>= (length form) 3)
                        (consp (second form))
                        (= (length (second form)) 1))
               (push-iss ln col "do*-single-var"
                         "DO* implies later variables depend on earlier ones - use DO here"))

             ;; Generic variable names
             (when (and (eq head 'defun)
                        (>= (length form) 4)
                        (consp (third form)))
               (dolist (param (third form))
                 (when (and (symbolp param)
                            (member param '(number input-list output-list data)))
                   (push-iss ln col "verbose-generic-var-name"
                             (format nil "~S is very generic - use specific name or standard short name" param)))))

             ;; Predicate names ending with ?
             (when (and (eq head 'defun)
                        (>= (length form) 3)
                        (symbolp (second form))
                        (let ((name (symbol-name (second form))))
                          (and (> (length name) 0)
                               (char= (char name (1- (length name))) #\?))))
               (push-iss ln col "?-for-predicate"
                         "Use -P suffix for predicates, not ? (this is Common Lisp, not Scheme)"))

             ;; Function names starting with "check"
             (when (and (eq head 'defun)
                        (>= (length form) 3)
                        (symbolp (second form))
                        (let ((name (string-downcase (symbol-name (second form)))))
                          (and (>= (length name) 5)
                               (string= (subseq name 0 5) "check"))))
               (push-iss ln col "check-prefix"
                         "CHECK prefix is vague (describe what the function does or returns)"))

             ;; Function names ending with "helper"
             (when (and (eq head 'defun)
                        (>= (length form) 3)
                        (symbolp (second form))
                        (let ((name (string-downcase (symbol-name (second form)))))
                          (and (>= (length name) 6)
                               (string= (subseq name (- (length name) 6)) "helper"))))
               (push-iss ln col "helper-suffix"
                         "HELPER suffix is vague (use name that describes what function does)"))

             ;; INSERT MORE RULES HERE

             )))))
    (nreverse issues)))
