(in-package :dref)

;;;; Packages and systems

(defun find-package* (name)
  ;; On AllegroCL, FIND-PACKAGE will signal an error if a relative
  ;; package name has too many leading dots. On CMUCL, (FIND-PACKAGE
  ;; "..") fails.
  #+(or allegro cmucl)
  (ignore-errors (find-package name))
  #-(or allegro cmucl)
  (find-package name))


;;;; Variables

(defun special-variable-name-p (obj)
  (and (symbolp obj)
       #+ccl (member (ccl::variable-information obj) '(:special :constant))
       #+sbcl (member (sb-int:info :variable :kind obj)
                      '(:special :constant))))

(defun constant-variable-name-p (obj)
  (and (symbolp obj)
       (not (keywordp obj))
       ;; CONSTANTP may detect constant symbol macros, for example.
       (boundp obj)
       (constantp obj)))


;;;; Types

;;; Like TYPEP, but never warn or error and indicate in the second
;;; return value whether TYPE is valid.
(defun typep* (obj type)
  #+abcl
  (if (valid-type-specifier-p type)
      (values (typep obj type) t)
      (values nil nil))
  #-abcl
  (on-unknown-type-warning ((values nil nil))
    (handler-case
        (values (typep obj type) t)
      (error ()
        (values nil nil)))))

(defun subtypep* (type1 type2)
  (on-unknown-type-warning ((values nil t))
    (handler-case
        (subtypep type1 type2)
      (error ()
        (values nil t)))))


;;;; Macros

(defun name-of-macro-p (name &optional fn)
  (and (symbolp name)
       (if fn
           (eq fn (macro-function name))
           (macro-function name))))

(defun special-operator-p* (name)
  (and (symbolp name)
       (or (special-operator-p name)
           ;; KLUDGE: CCL is mistaken about DECLARE.
           #+ccl (eq name 'declare))))

(defun symbol-macro-p (name)
  (and (symbolp name)
       #+ccl (gethash name ccl::*symbol-macros*)
       #+sbcl (sb-int:info :variable :macro-expansion name)))


;;;; Compiler macros

(defun compiler-macro-function* (name)
  #+clisp (handler-bind ((warning #'muffle-warning))
            (ignore-errors (compiler-macro-function name)))
  #-clisp (compiler-macro-function name))


;;;; SETF

(defun setf-name-p (name)
  (and (listp name)
       (= (length name) 2)
       (eq (first name) 'setf)
       (symbolp (second name))))

;;; See if SYMBOL has a [setf expander][clhs] or [setf
;;; function][clhs].
(defun has-setf-p (symbol)
  (or (has-setf-expander-p symbol)
      (has-setf-function-p symbol)))

(defun has-setf-expander-p (symbol)
  ;; FIXME: other implementations
  #+ccl (ccl::%setf-method symbol)
  #+clisp (get symbol 'system::setf-expander)
  #+sbcl (swank/sbcl::setf-expander symbol)
  #-(or ccl clisp sbcl)
  ;; KLUDGE: When there is no setf expansion, we get this:
  ;;
  ;;   (nth-value 3 (get-setf-expansion '(undefined)))
  ;;   => (FUNCALL #'(SETF UNDEFINED) #:NEW1)
  ;;
  ;; which is non-portable in theory, but luckily, portable in
  ;; practice.
  (let ((storing-form (nth-value 3 (ignore-errors
                                    (get-setf-expansion `(,symbol))))))
    ;; Sadly, using (NTH-VALUE 3 (GET-SETF-EXPANSION `(,SYMBOL))) to
    ;; tell whether SYMBOL has a setf expansion doesn't work in
    ;; general because GET-SETF-EXPANSION may fail due to either
    ;; DEFINE-SETF-EXPANDER or the macro named by the value of SYMBOL
    ;; failing (e.g. with an insufficient number of arguments). For
    ;; this reason, DEFSETFs can be detected, but
    ;; DEFINE-SETF-EXPANDERs cannot in general.
    (and storing-form
         (not (and (eq (first storing-form) 'funcall)
                   (equal (second storing-form) `#'(setf ,symbol)))))))

(defun has-setf-function-p (symbol)
  (values (ignore-errors (fdefinition* `(setf ,symbol)))))


;;;; Functions

(defun function-name (function)
  (let* ((function (unencapsulated-function function))
         (name #-clisp (swank-backend:function-name function)
               #+clisp (system::function-name function)))
    #-abcl
    (let ((kind (and (listp name)
                     (= (length name) 2)
                     (case (first name)
                       ((macro-function :macro) 'macro)
                       ((compiler-macro compiler-macro-function
                                        :compiler-macro)
                        'compiler-macro)))))
      (if kind
          (values (second name) kind)
          name))
    ;; ABCL has function names like (FOO (SYSTEM::INTERPRETED)).
    #+abcl
    (if (and (listp name) (not (eq (first name) 'setf)))
        (first name)
        name)))

;;; Like SYMBOL-FUNCTION but sees through encapsulated functions.
(defun symbol-function* (symbol)
  #+abcl
  (or (system::untraced-function symbol)
      (symbol-function symbol))
  #+clisp
  (or (system::get-traced-definition symbol)
      (symbol-function symbol))
  #-(or abcl clisp)
  (unencapsulated-function (symbol-function symbol)))

;;; See "function name" (clhs).
(defun valid-function-name-p (name)
  (or (symbolp name) (setf-name-p name)))

(defun extended-function-name-p (name)
  (or (valid-function-name-p name)
      #+sbcl (sb-impl::legal-fun-name-p name)))

(defun consistent-fdefinition (name)
  (let ((fn (ignore-errors (fdefinition* name))))
    (when (and fn (equal (function-name fn) name))
      fn)))

(defun has-fdefinition-p (name)
  (ignore-errors (fdefinition* name)))

(defun fdefinition* (name)
  #+abcl
  (or (system::untraced-function name)
      (fdefinition name))
  #+clisp
  (if (listp name)
      (eval `(function ,name))
      (or (system::get-traced-definition name)
          (fdefinition name)))
  #-(or abcl clisp)
  (unencapsulated-function (fdefinition name)))

(defun unencapsulated-function (function)
  (or #+ccl (ccl::find-unencapsulated-definition function)
      #+cmucl (loop for fn = function then (fwrappers:fwrapper-next fn)
                    while (typep fn 'fwrappers:fwrapper)
                    finally (return fn))
      #+ecl (when (and (consp function)
                       (eq (car function) 'si:macro))
              function)
      #+ecl (find-type-in-sexp (function-lambda-expression function) 'function)
      #+sbcl (maybe-find-encapsulated-function function)
      function))

#+ecl
(defun find-type-in-sexp (form type)
  (dolist (x form)
    (cond ((listp x)
           (let ((r (find-type-in-sexp x type)))
             (when r
               (return-from find-type-in-sexp r))))
          ((typep x type)
           (return-from find-type-in-sexp x))
          (t
           nil))))

#+sbcl
;;; Tracing typically encapsulates a function in a closure. The
;;; function we need is at the end of the encapsulation chain.
(defun maybe-find-encapsulated-function (function)
  (declare (type function function))
  (if (eq (sb-impl::%fun-name function) 'sb-impl::encapsulation)
      (maybe-find-encapsulated-function
       (sb-impl::encapsulation-info-definition
        (sb-impl::encapsulation-info function)))
      function))


;;; Return either the arglist and FOUNDP, or NIL and NIL if the
;;; arglist was not found.
(defun function-arglist (function-designator &optional (foundp t))
  (let ((function-designator
          (cond ((functionp function-designator)
                 (unencapsulated-function function-designator))
                ((valid-function-name-p function-designator)
                 function-designator))))
    #+cmucl
    (unless (functionp function-designator)
      ;; On CMUCL, SWANK-BACKEND:ARGLIST works better with function
      ;; objects than with names.
      (setq function-designator (fdefinition* function-designator)))
    #+ecl
    (when (listp function-designator)
      ;; On ECL, SWANK-BACKEND:ARGLIST errors on setf function names.
      (setq function-designator (fdefinition* function-designator)))
    (multiple-value-bind (function-name function)
        (if (functionp function-designator)
            (values (function-name function-designator) function-designator)
            (values function-designator (fdefinition* function-designator)))
      (declare (ignorable function-name function))
      #-(or abcl allegro ccl)
      (let ((arglist (swank-backend:arglist function-designator)))
        (if (eq arglist :not-available)
            (values nil nil)
            (values arglist foundp)))
      #+abcl
      (multiple-value-bind (arglist foundp*)
          (extensions:arglist function-designator)
        (cond (foundp*
               (values arglist foundp))
              ((typep function-designator 'generic-function)
               (values (mop:generic-function-lambda-list function-designator)
                       foundp))
              ((and (symbolp function-designator)
                    (typep (symbol-function* function-designator)
                           'generic-function))
               (values (mop:generic-function-lambda-list
                        (symbol-function* function-designator))
                       foundp))))
      #+allegro
      (handler-case
          (let* ((symbol (if (symbolp function-designator)
                             function-designator
                             (function-name function-designator)))
                 (lambda-expression (ignore-errors
                                     (function-lambda-expression
                                      (symbol-function symbol)))))
            (values (if lambda-expression
                        (second lambda-expression)
                        (excl:arglist function-designator))
                    foundp))
        (simple-error () nil))
      #+ccl
      (alexandria:nth-value-or 1
        ;; Function arglist don't have the default values of &KEY and
        ;; &OPTIONAL arguments. Get those from
        ;; CCL:FUNCTION-SOURCE-NOTE. This is also the way to get
        ;; DEFTYPE expanders arglists.
        (function-arglist-from-source-note function foundp)
        (let ((arglist (swank-backend:arglist
                        (or (and (functionp function-designator)
                                 (function-name function-designator))
                            function-designator))))
          (when (listp arglist)
            (values
             ;; &KEY arguments are given as keywords, which screws up
             ;; WITH-DISLOCATED-SYMBOLS when generating documentation
             ;; for functions.
             (mapcar (lambda (x)
                       (if (keywordp x)
                           (intern (string x))
                           x))
                     arglist)
             foundp)))))))

#+ccl
(defun function-arglist-from-source-note (function foundp)
  (let ((function-name (function-name function)))
    (when function-name
      (let ((source-note (ccl:function-source-note function)))
        (when source-note
          (let ((text (ccl:source-note-text source-note)))
            (when text
              (lambda-list-from-source-note-text text function-name
                                                 foundp))))))))

;;; Extract the lambda list from TEXT, which is like "(defun foo (x
;;; &optional (o 1)) ...". Or the same with DEFTYPE.
#+ccl
(defun lambda-list-from-source-note-text (text name foundp)
  ;; This is a heuristic. It is impossible to determine what *PACKAGE*
  ;; was when the definition form was read.
  (let ((symbol (if (listp name)
                    (second name)
                    name)))
    (when (symbolp symbol)
      (let ((*package* (symbol-package symbol)))
        (with-input-from-string (s text)
          (when (eql (read-char s nil) #\()
            ;; Skip DEFUN, DEFTYPE or similar and the name.
            (let ((*read-suppress* t))
              (read s nil)
              ;; FIXME: check that it's similar to NAME.
              (read s nil))
            (multiple-value-bind (arglist error)
                (ignore-errors (read s))
              (when (and (null error)
                         (listp arglist))
                (values arglist foundp)))))))))

;;; Return the names of the function arguments in ARGLIST, which is an
;;; [ordinary lambda list][clhs]. Handles &KEY, &OPTIONAL, &REST,
;;; &AUX, &ALLOW-OTHER-KEYS.
(defun function-arg-names (arglist)
  (multiple-value-bind (requireds optionals rest keywords other-keys-p auxs)
      (alexandria:parse-ordinary-lambda-list arglist)
    (declare (ignore other-keys-p))
    (let ((names requireds))
      (dolist (optional optionals)
        (push (first optional) names)
        ;; SUPPLIEDP
        (when (third optional)
          (push (third optional) names)))
      (when rest
        (push rest names))
      (dolist (keyword keywords)
        (push (second (first keyword)) names)
        (when (third keyword)
          (push (third keyword) names)))
      (dolist (aux auxs)
        (push (first aux) names))
      (reverse names))))

(defun method-arglist (method)
  (let ((arglist (swank-mop:method-lambda-list method))
        (seen-special-p nil))
    ;; Some implementations include the specializers. Remove them.
    (loop for arg in arglist
          do (when (member arg '(&key &optional &rest &aux &allow-other-keys))
               (setq seen-special-p t))
          collect (if (and (not seen-special-p)
                           (listp arg)
                           (= (length arg) 2))
                      (first arg)
                      arg))))


;;;; Methods

(defun find-method* (function-designator qualifiers specializers
                     &optional (errorp t))
  (find-method (if (functionp function-designator)
                   function-designator
                   (fdefinition* function-designator))
               qualifiers
               (specializers-to-objects specializers)
               errorp))

(defun specializers-to-objects (specializers)
  #-(or allegro ccl clisp) specializers
  #+(or allegro ccl clisp) (mapcar #'specializer-to-object specializers))

(defun objects-to-specializers (objects)
  #-(or allegro ccl clisp) objects
  #+(or allegro ccl clisp) (mapcar #'object-to-specializer objects))

#+(or allegro ccl clisp)
(defun specializer-to-object (specializer)
  (cond ((symbolp specializer)
         (find-class specializer))
        ((and (listp specializer)
              (= (length specializer) 2)
              (eq (first specializer) 'eql))
         #+allegro (aclmop:intern-eql-specializer (second specializer))
         #+ccl (ccl:intern-eql-specializer (second specializer))
         #+clisp specializer)
        (t specializer)))

#+(or allegro ccl clisp)
(defun object-to-specializer (object)
  (cond ((typep object 'class)
         (class-name object))
        #+ccl
        ((typep object 'ccl:eql-specializer)
         `(eql ,(ccl:eql-specializer-object object)))
        (t object)))


;;;; Strings

;;; Convert to full width character string. Useful for prettier
;;; printing and ensuring canonical form.
(defun character-string (string)
  (make-array (length string) :element-type 'character
              :initial-contents string))

(defun adjust-string-case (string)
  (declare (type string string))
  (ecase (readtable-case *readtable*)
    ((:upcase) (string-upcase string))
    ((:downcase) (string-downcase string))
    ;; We don't care about convenience with :INVERT.
    ((:preserve :invert) string)))

(defun first-lines (string &optional (n-lines 1))
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for i below n-lines do
        (let ((line (read-line in nil nil)))
          (when line
            (cond ((< i (1- n-lines))
                   (write-line line out))
                  ((= i (1- n-lines))
                   (write-string line out)))))))))

(defun first-line (string)
  (first-lines string))


;;;; I/O

(defmacro with-standard-io-syntax* (&body body)
  `(with-standard-io-syntax
     ;; With *PRINT-READABLY*, CLISP insists on printing FOO as |FOO|.
     (let (#+clisp (*print-readably* nil))
       ,@body)))


;;;; DOCUMENTATION

(defun documentation* (object doc-type)
  "A small wrapper around CL:DOCUMENTATION to smooth over differences
  between implementations."
  ;; Some just can't decide where the documentation is.
  (flet ((%documentation (x y)
           ;; On CLISP this gets a no-applicable-method error:
           ;;    (documentation '(setf documentation) t)
           ;; on ABCL this:
           ;;    (documentation (fdefinition 'function) 'function)
           (let ((docstring (ignore-errors (documentation x y))))
             (cond ((or (null docstring)
                        (stringp docstring))
                    docstring)
                   (t
                    (warn "~@<~S returned ~S, which is not ~S.~:@>"
                          `(documentation ',x ',y) docstring
                          '(or null string))
                    nil)))))
    (filter-junk-docstrings
     (cond ((and (functionp object)
                 (member doc-type '(function t)))
            (or (%documentation object 'function)
                (%documentation (function-name object) 'function)))
           ((eq doc-type 'function)
            (or (%documentation object 'function)
                (%documentation (ignore-errors (fdefinition* object))
                                'function)))
           ((eq doc-type 'setf)
            (or (%documentation object 'setf)
                (%documentation `(setf ,object) 'function)))
           #+cmucl
           ((typep object 'class)
            (%documentation (class-name object) 'type))
           (t
            (%documentation object doc-type))))))

(defun filter-junk-docstrings (docstring)
  #-sbcl docstring
  #+sbcl
  (if (member docstring
              '("Return whether debug-block represents elsewhere code."
                "automatically generated accessor method"
                "automatically generated reader method"
                "automatically generated writer method")
              :test #'equal)
      nil
      docstring))


;;;; Misc

;;; Only compute the key for each element once.
(defun sort-list-with-precomputed-key (list pred &key key)
  (map 'list #'car (sort (map 'vector (lambda (x)
                                        (cons x (funcall key x)))
                              list)
                         pred :key #'cdr)))

(defun unlist1 (obj)
  (if (and (listp obj) (= (length obj) 1))
      (first obj)
      obj))
