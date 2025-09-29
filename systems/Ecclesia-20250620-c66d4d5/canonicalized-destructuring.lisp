(cl:in-package #:ecclesia)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function DESTRUCTURE-CANONICALIZED-LAMBDA-LIST.
;;;
;;; Destructuring a tree according to a lambda list.
;;;
;;; The destructuring itself is typically done when a macro function
;;; is run, and the purpose is to take the macro form apart and assign
;;; parts of it to the parameters of the lambda list of the macro.
;;;
;;; The function DESTRUCTURE-CANONICALIZED-LAMBDA-LIST generates the
;;; code for doing the destrucuring.  It is typically run by the
;;; expansion of DEFMACRO.  Recall that DEFMACRO must take the
;;; definition of a macro, in particular its lambda list, and generate
;;; a macro function.  The macro function takes the macro form as
;;; input and generates the expanded form.  Destructuring is done by a
;;; LET* form, and this code generates the bindings of that LET* form.
;;;
;;; It would have been more elegant to generate nested LET
;;; expressions, rather than a single LET*, because there are some
;;; arbitrary forms that need to be evaluated in between bindings, and
;;; those forms would fit more naturally into the body of a LET.  With
;;; a single LET* those forms must be part of the binding form of the
;;; LET*, and there is not always an obvious lexical variable to bind
;;; the result to.  So we must generate new variables and then ignore
;;; them in the LET* body.  But we do it this way because the DEFMACRO
;;; form may contain declarations that mention the variables in the
;;; DEFMACRO lambda list, and with nested LET expressions, some of
;;; those variables would then be introduced in a LET expression that
;;; is not the innermost one.  We could handle some such cases with
;;; LOCALLY, but IGNORE declarations result in warnings in some
;;; implementations.
;;;
;;; So, the bindings that we create will contain generated variables
;;; that are not used in the body of the macro definition, and we want
;;; them to be declared IGNORE.  For that reason,
;;; DESTRUCTURE-CANONICALIZED-LAMBDA-LIST returns two values: the
;;; bindings mentioned above, and a list of variables to declare
;;; IGNORE in the beginning of the body of the macro function.
;;;
;;; The bindings return by DESTRUCTURE-CANONICALIZED-LAMBDA-LIST and
;;; its subroutines are in the reverse order compared to the order it
;;; which they should appear in the expanded expression.  We do it
;;; this way in order to avoid too much consing.
;;;
;;; We assume that the lambda-list is syntactically correct.  It
;;; should be, because this function takes as input not the raw lambda
;;; list, but a canonicalized lambda list that has already been
;;; checked for errors.

;;; Given a list of the remaining groups of a lambda list, return true
;;; if and only if the list is not empty, and the first group of the
;;; list starts with LAMBDA-LIST-KEYWORD.
(defun first-group-is (remaining lambda-list-keyword)
  (and (not (null remaining))
       (not (null (first remaining)))
       (eq (first (first remaining)) lambda-list-keyword)))

;;; Destructure a list of required parameters.  A required parameter
;;; may be a variable or a pattern.  Return a list of bindings and a
;;; list of variables to ignore.
(defun destructure-canonicalized-required
    (required variable canonicalized-lambda-list)
  (let* ((bindings '())
         (ignored-variables '())
         (e-lambda-list (reduce #'append canonicalized-lambda-list))
         (not-enough-arguments-form
           `(error 'too-few-arguments
                   :lambda-list ',e-lambda-list)))
    (loop for pattern in required
          for bind = (if (and (symbolp pattern) (not (null pattern)))
                         pattern
                         (gensym))
          do (push `(,bind (if (null ,variable)
                               ,not-enough-arguments-form
                               (first ,variable)))
                   bindings)
             (push `(,variable (rest ,variable)) bindings)
             (cond ((null pattern)
                    (let ((check (gensym)))
                      (push `(,check (unless (null ,bind)
                                       (error 'too-many-arguments
                                              :lambda-list
                                              ',e-lambda-list)))
                            bindings)
                      (push check ignored-variables)))
                   ((symbolp pattern))
                   (t (multiple-value-bind
                            (nested-bindings nested-ignored-variables)
                          (destructure-canonicalized-lambda-list
                           pattern bind)
                        (setf bindings
                              (append nested-bindings bindings))
                        (setf ignored-variables
                              (append nested-ignored-variables ignored-variables))))))
    (values bindings ignored-variables)))

;;; Destructure a list of optional parameters.
;;; Return a list of bindings and a list of variables to ignore.
(defun destructure-canonicalized-optional (optional variable
                                           canonicalized-lambda-list)
  (let ((bindings '()) (ignored '()))
    (loop for (var default supplied-p) in optional
          for bind = (if (and (symbolp var) (not (null var))) var (gensym))
          do (unless (null supplied-p)
               (push `(,supplied-p (not (null ,variable)))
                     bindings))
             (push `(,bind (if (null ,variable)
                               ,default
                               (first ,variable)))
                   bindings)
             (push `(,variable (if (null ,variable)
                                   ,variable
                                   (rest ,variable)))
                   bindings)
             (cond ((null var)
                    (let ((check (gensym)))
                      (push `(,check (unless (null ,bind)
                                       (error 'too-many-arguments
                                              :lambda-list
                                              ',(reduce #'append canonicalized-lambda-list))))
                            bindings)
                      (push check ignored)))
                   ((symbolp var))
                   (t (multiple-value-bind (nested-bindings nested-ignore)
                          (destructure-canonicalized-lambda-list var bind)
                        (setf bindings (append nested-bindings bindings)
                              ignored (append nested-ignore ignored))))))
    (values bindings ignored)))

;;; Destructure a &REST or &BODY parameter which can be a variable or
;;; a pattern.  Return a list of bindings and a list of variables to
;;; ignore.
(defun destructure-canonicalized-rest/body
    (pattern variable)
  (let ((bindings '())
        (ignored-variables '()))
    (if (symbolp pattern)
        (push `(,pattern ,variable) bindings)
        (let ((temp (gensym)))
          (push `(,temp ,variable)
                bindings)
          (multiple-value-bind (nested-bindings nested-ignored-variables)
              (destructure-canonicalized-lambda-list
               pattern temp)
            (setf bindings
                  (append nested-bindings bindings))
            (setf ignored-variables
                  (append nested-ignored-variables ignored-variables)))))
    (values bindings ignored-variables)))

;;; Destructure a list of &KEY parameters.  Return a list of bindings
;;; and a list of variables to ignore.
(defun destructure-canonicalized-key
    (key variable canonicalized-lambda-list allow-other-keys)
  (let* ((bindings '())
         (ignored-variables '())
         (keywords (mapcar #'caar key))
         (odd-number-of-keyword-arguments-form
           `(error 'odd-number-of-keyword-arguments
                   :lambda-list
                   ',(reduce #'append canonicalized-lambda-list)))
         (check-keywords-form
           (let ((temp (gensym)))
             `(let ((,temp ,variable))
                (tagbody
                 again
                   (if (null ,temp) (go out))
                   (if (not (member (first ,temp)
                                    '(:allow-other-keys ,@keywords)
                                    :test #'eq))
                       (error 'invalid-keyword
                              :keyword (first ,temp)
                              :lambda-list
                              ',(reduce #'append canonicalized-lambda-list))
                       (progn (setf ,temp (cddr ,temp))
                              (go again)))
                 out)))))
    (let ((ignored (gensym)))
      (push ignored ignored-variables)
      (push `(,ignored (if (oddp (length ,variable))
                           ,odd-number-of-keyword-arguments-form))
            bindings))
    (unless allow-other-keys
      (let ((ignored (gensym)))
        (push ignored ignored-variables)
        (push `(,ignored (if (not (getf ,variable :allow-other-keys))
                             ,check-keywords-form))
              bindings)))
    (loop for ((keyword var) default supplied-p) in key
          for temp1 = (gensym)
          for temp2 = (gensym)
          for bind = (if (and (symbolp var) (not (null var))) var (gensym))
          do (push `(,temp1 (list nil)) bindings)
             (push `(,temp2 (getf ,variable ,keyword ,temp1))
                   bindings)
             (if (null supplied-p)
                 nil
                 (push `(,supplied-p (not (eq ,temp2 ,temp1)))
                       bindings))
             (push `(,bind (if (eq ,temp2 ,temp1)
                              ,default
                              ,temp2))
                   bindings)
             (cond ((null var)
                    (let ((check (gensym)))
                      (push `(,check (unless (null ,bind)
                                       (error 'too-many-arguments
                                              :lambda-list
                                              ',(reduce #'append canonicalized-lambda-list))))
                            bindings)
                      (push check ignored-variables)))
                   ((symbolp var))
                   (t (multiple-value-bind (nested-bindings nested-ignore)
                          (destructure-canonicalized-lambda-list var bind)
                        (setf bindings (append nested-bindings bindings)
                              ignored-variables (append nested-ignore ignored-variables))))))
    (values bindings ignored-variables)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DESTRUCTURE-CANONICALIZED-LAMBDA-LIST

(defun destructure-canonicalized-lambda-list
    (canonicalized-lambda-list variable)
  (let* ((remaining canonicalized-lambda-list)
         (bindings '())
         (ignored-variables '()))
    (when (first-group-is remaining '&whole)
      (push `(,(second (pop remaining)) ,variable) bindings))
    (unless (or (null remaining)
                (member (first (first remaining)) (intrinsic-keywords)
                        :test #'eq))
      (multiple-value-bind (nested-bindings nested-ignored-variables)
          (destructure-canonicalized-required
           (pop remaining) variable canonicalized-lambda-list)
        (setf bindings
              (append nested-bindings bindings))
        (setf ignored-variables
              (append nested-ignored-variables ignored-variables))))
    (when (first-group-is remaining '&optional)
      (multiple-value-bind (nested-bindings nested-ignore)
          (destructure-canonicalized-optional
           (rest (pop remaining)) variable canonicalized-lambda-list)
        (setf bindings (append nested-bindings bindings)
              ignored-variables (append nested-ignore ignored-variables))))
    (unless (or (member '&rest remaining :key #'first :test #'eq)
                (member '&body remaining :key #'first :test #'eq)
                (member '&key remaining :key #'first :test #'eq))
      (let ((temp (gensym)))
        (push temp ignored-variables)
        (push `(,temp (if (not (null ,variable))
                          (error 'too-many-arguments
                                 :lambda-list
                                 ',(reduce #'append canonicalized-lambda-list))))
              bindings)))
    (when (or (first-group-is remaining '&rest)
              (first-group-is remaining '&body))
      (multiple-value-bind (nested-bindings nested-ignored-variables)
          (destructure-canonicalized-rest/body
           (second (pop remaining)) variable)
        (setf bindings
              (append nested-bindings bindings))
        (setf ignored-variables
              (append nested-ignored-variables ignored-variables))))
    (when (first-group-is remaining '&key)
      (let* ((group (pop remaining))
             (allow-other-keys
               (if (first-group-is remaining '&allow-other-keys)
                   (progn (pop remaining) t)
                   nil)))
        (multiple-value-bind (nested-bindings nested-ignored-variables)
            (destructure-canonicalized-key
             (rest group)
             variable
             canonicalized-lambda-list
             allow-other-keys)
          (setf bindings
                (append nested-bindings bindings))
          (setf ignored-variables
                (append nested-ignored-variables ignored-variables)))))
    (when (first-group-is remaining '&aux)
      (setf bindings
            (append (reverse (rest (pop remaining))) bindings)))
    (values bindings ignored-variables)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-MACRO
;;;
;;; According to CLtL2.

(defun parse-macro-using-canonicalization
    (name lambda-list body &optional environment header-declarations)
  (declare (ignore environment)) ; For now.
  (let* ((canonicalized-lambda-list
           (canonicalize-macro-lambda-list lambda-list))
         (environment-group
           (extract-named-group canonicalized-lambda-list '&environment))
         (environment-parameter
           (if (null environment-group) (gensym) (second environment-group)))
         (whole-group
           (extract-named-group canonicalized-lambda-list '&whole))
         (whole-parameter
           (if (null whole-group) (gensym) (second whole-group)))
         (remaining
           (remove '&environment
                   (remove '&whole canonicalized-lambda-list
                           :key #'first :test #'eq)
                   :key #'first :test #'eq))
         (args-var (gensym)))
    (multiple-value-bind (declarations documentation forms)
        (separate-function-body body)
      (multiple-value-bind (bindings ignored-variables)
          (destructure-canonicalized-lambda-list remaining args-var)
        `(lambda (,whole-parameter ,environment-parameter)
           ,@(if (null documentation) '() (list documentation))
           ;; If the lambda list does not contain &environment, then
           ;; we IGNORE the GENSYMed parameter to avoid warnings.
           ;; If the lambda list does contain &environment, we do
           ;; not want to make it IGNORABLE because we would want a
           ;; warning if it is not used then.
           ,@(if (null environment-group)
                 `((declare (ignore ,environment-parameter)))
                 `())
           (declare ,@header-declarations)
           (block ,name
             (let ((,args-var (rest ,whole-parameter)))
               (let* ,(reverse bindings)
                 (declare (ignore ,@ignored-variables))
                 ,@declarations
                 ,@forms))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-COMPILER-MACRO
;;;
;;; This function differs from parse-macro only in the code that
;;; destructures the lambda list from the arguments.

(defun parse-compiler-macro-using-canonicalization
    (name lambda-list body &optional environment header-declarations)
  (declare (ignore environment)) ; For now.
  (let* ((canonicalized-lambda-list
           (canonicalize-macro-lambda-list lambda-list))
         (environment-group
           (extract-named-group canonicalized-lambda-list '&environment))
         (environment-parameter
           (if (null environment-group) (gensym) (second environment-group)))
         (whole-group
           (extract-named-group canonicalized-lambda-list '&whole))
         (whole-parameter
           (if (null whole-group) (gensym) (second whole-group)))
         (remaining
           (remove '&environment
                   (remove '&whole canonicalized-lambda-list
                           :key #'first :test #'eq)
                   :key #'first :test #'eq))
         (args-var (gensym)))
    (multiple-value-bind (declarations documentation forms)
        (separate-function-body body)
      (multiple-value-bind (bindings ignored-variables)
          (destructure-canonicalized-lambda-list remaining args-var)
        `(lambda (,whole-parameter ,environment-parameter)
           ,@(if (null documentation) '() (list documentation))
           ;; If the lambda list does not contain &environment, then
           ;; we IGNORE the GENSYMed parameter to avoid warnings.
           ;; If the lambda list does contain &environment, we do
           ;; not want to make it IGNORABLE because we would want a
           ;; warning if it is not used then.
           ,@(if (null environment-group)
                 `((declare (ignore ,environment-parameter)))
                 `())
           (declare ,@header-declarations)
           (block ,name
             (let ((,args-var (if (and (eq (car ,whole-parameter) 'funcall)
                                       (consp (cdr ,whole-parameter))
                                       (consp (cadr ,whole-parameter))
                                       (eq (car (cadr ,whole-parameter)) 'function))
                                  (cddr ,whole-parameter)
                                  (cdr ,whole-parameter))))
               (let* ,(reverse bindings)
                 (declare (ignore ,@ignored-variables))
                 ,@declarations
                 ,@forms))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-DEFTYPE

(defun parse-deftype-using-canonicalization
    (name lambda-list body &optional environment header-declarations)
  (declare (ignore name))
  (let* ((canonicalized-lambda-list
           (canonicalize-deftype-lambda-list lambda-list))
         (environment-group
           (extract-named-group canonicalized-lambda-list '&environment))
         (environment-parameter
           (if (null environment-group) (gensym) (second environment-group)))
         (whole-group
           (extract-named-group canonicalized-lambda-list '&whole))
         (whole-parameter
           (if (null whole-group) (gensym) (second whole-group)))
         (remaining
           (remove '&environment
                   (remove '&whole canonicalized-lambda-list
                           :key #'first :test #'eq)
                   :key #'first :test #'eq))
         (args-var (gensym)))
    (multiple-value-bind (declarations documentation forms)
        (separate-function-body body)
      (multiple-value-bind (bindings ignored-variables)
          (destructure-canonicalized-lambda-list remaining args-var)
        `(lambda (,whole-parameter ,environment-parameter)
           ,@(if (null documentation) '() (list documentation))
           ;; If the lambda list does not contain &environment, then
           ;; we IGNORE the GENSYMed parameter to avoid warnings.
           ;; If the lambda list does contain &environment, we do
           ;; not want to make it IGNORABLE because we would want a
           ;; warning if it is not used then.
           ,@(if (null environment-group)
                 `((declare (ignore ,environment-parameter)))
                 `())
           (let ((,args-var (rest ,whole-parameter)))
             (let* ,(reverse bindings)
               (declare (ignore ,@ignored-variables))
               ,@declarations
               ,@forms)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-DESTRUCTURING-BIND

(defun parse-destructuring-bind (lambda-list form body)
  (let* ((canonicalized-lambda-list
           (canonicalize-destructuring-lambda-list lambda-list))
         (whole-group
           (extract-named-group canonicalized-lambda-list '&whole))
         (whole-parameter
           (if (null whole-group) (gensym) (second whole-group)))
         (remaining
           (remove '&whole canonicalized-lambda-list
                   :key #'first :test #'eq))
         (args-var (gensym)))
    (multiple-value-bind (declarations forms)
        (separate-ordinary-body body)
      (multiple-value-bind (bindings ignored-variables)
          (destructure-canonicalized-lambda-list remaining args-var)
        `(let* ((,whole-parameter ,form)
                (,args-var ,whole-parameter)
                ,@(reverse bindings))
           (declare (ignore ,@ignored-variables))
           ,@declarations
           ,@forms)))))
