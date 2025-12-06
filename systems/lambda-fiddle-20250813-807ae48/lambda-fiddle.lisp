(in-package #:cl-user)
(defpackage #:lambda-fiddle
  (:nicknames #:org.shirakumo.lambda-fiddle)
  (:use #:cl)
  (:export
   #:*lambda-keywords*
   #:*single-argument-keywords*
   #:lambda-keyword-p
   #:single-argument-keyword-p
   #:collect-for-keyword
   #:exclude-for-keyword
   #:flatten-lambda-list
   #:flatten-method-lambda-list
   #:extract-lambda-vars
   #:extract-all-lambda-vars
   #:remove-whole-part
   #:remove-environment-part
   #:remove-required-part
   #:remove-optional-part
   #:remove-rest-part
   #:remove-body-part
   #:remove-key-part
   #:remove-aux-part
   #:whole-lambda-var
   #:environment-lambda-var
   #:required-lambda-vars
   #:optional-lambda-vars
   #:rest-lambda-var
   #:body-lambda-var
   #:key-lambda-vars
   #:aux-lambda-vars
   #:split-lambda-list
   #:with-destructured-lambda-list
   #:construct-lambda-list))
(in-package #:lambda-fiddle)

(defvar *lambda-keywords* '(&whole &environment &optional &rest &body &key &allow-other-keys &aux)
  "List of all standard lambda-keywords.")
(defvar *single-argument-keywords* '(&whole &environment &rest &body)
  "List of all standard lambda-keywords that only allow one argument.")

(defun unlist (thing &optional (n 0))
  (if (consp thing) (nth n thing) thing))

(defun lambda-keyword-p (symbol)
  "Returns the symbol if it is a lambda-keyword symbol (the &-options)."
  (find symbol *lambda-keywords*))

(defun single-argument-keyword-p (symbol)
  "Returns the symbol if it is a single-argument-keyword symbol."
  (find symbol *single-argument-keywords*))

(defun collect-for-keyword (keyword lambda-list &key (spanning T))
  "Returns a fresh list of elements listed after the KEYWORD in the lambda-list.
If SPANNING is T, it includes everything up until the next keyword, otherwise only one token each."
  (loop with in-keyword = NIL
        with list = ()
        for i in lambda-list
        do (cond ((eql i keyword)
                  (setf in-keyword T))
                 ((and in-keyword (lambda-keyword-p i))
                  (setf in-keyword NIL))
                 (in-keyword
                  (push i list)
                  (unless spanning
                    (setf in-keyword NIL))))
        finally (return (nreverse list))))

(defun exclude-for-keyword (keyword lambda-list &key (spanning T))
  "Returns a fresh lambda-list but without the parts identified by KEYWORD.
If SPANNING is T, it excludes everything up until the next keyword, otherwise only one token each."
  (loop with in-keyword = NIL
        with list = ()
        for i in lambda-list
        do (cond ((eql i keyword)
                  (setf in-keyword T))
                 ((and in-keyword (lambda-keyword-p i))
                  (setf in-keyword NIL))
                 ((and in-keyword (not spanning))
                  (setf in-keyword NIL))
                 ((not in-keyword)
                  (push i list)))
        finally (return (nreverse list))))

(defun flatten! (list)
  "Destructively flatten the list one level."
  (loop for sublist on list
        for item = (car sublist)
        while sublist
        do (if (listp item)
               (setf (car sublist) (car item)))
        finally (return list)))

(defun flatten-lambda-list (lambda-list)
  "Flattens the lambda-list by replacing all lists within it with their respective first symbol.
This also properly flattens inner lambda-lists of macro-lambda-lists."
  (loop with in-opt = NIL
        with results = ()
        for element in lambda-list
        do (cond ((or (member element '(&key &aux &optional)))
                  (push (setf in-opt element)
                        results))
                 (in-opt
                  (push (unlist element)
                        results))
                 ;; Flatten inner lists recursively
                 ((listp element)
                  (setf results (nconc (nreverse (flatten-lambda-list element)) results)))
                 (T (push element results)))
        finally (return (nreverse results))))

(defun flatten-method-lambda-list (lambda-list)
  "Flattens the lambda-list by replacing all lists within it with their respective first symbol.
Unlike FLATTEN-LAMBDA-LIST, this works for method lambda lists."
  (loop for item in lambda-list
        collect (etypecase item
                  (symbol item)
                  (list (first item)))))

(defun extract-lambda-vars (lambda-list)
  "Extracts the symbols that name the variables in the lambda-list."
  (loop for part in (flatten-lambda-list (remove-aux-part lambda-list))
        unless (lambda-keyword-p part)
        collect (unlist part 1)))

(defun extract-all-lambda-vars (lambda-list)
  "Extracts all variable bindings from the lambda-list, including the present-p ones."
  (loop for item in lambda-list
        unless (find item *lambda-keywords*)
        nconc (cond ((and (listp item) (cddr item))
                     (list (unlist (first item) 1) (third item)))
                    ((listp item)
                     (list (unlist (first item) 1)))
                    (T
                     (list item)))))

(defun remove-whole-part (lambda-list)
  "Returns a fresh lambda-list without the &whole part."
  (exclude-for-keyword '&whole lambda-list :spanning NIL))

(defun remove-environment-part (lambda-list)
  "Returns a fresh lambda-list without the &environment part."
  (exclude-for-keyword '&environment lambda-list :spanning NIL))

(defun remove-required-part (lambda-list)
  "Returns a fresh lambda-list without the required variables part."
  (loop for sublist on lambda-list
        for item = (car sublist)
        until (find item (cddr *lambda-keywords*))
        if (or (eq item '&whole) (eq item '&environment))
        do (setf sublist (cdr sublist))
        else
        collect item))

(defun remove-optional-part (lambda-list)
  "Returns a fresh lambda-list without the &optional part."
  (exclude-for-keyword '&optional lambda-list :spanning T))

(defun remove-rest-part (lambda-list)
  "Returns a fresh lambda-list without the &rest part."
  (exclude-for-keyword '&rest lambda-list :spanning NIL))

(defun remove-body-part (lambda-list)
  "Returns a fresh lambda-list without the &body part."
  (exclude-for-keyword '&body lambda-list :spanning NIL))

(defun remove-key-part (lambda-list)
  "Returns a fresh lambda-list without the &key part (also removing &allow-other-keys if present)."
  (delete '&allow-other-keys (exclude-for-keyword '&key lambda-list :spanning T)))

(defun remove-aux-part (lambda-list)
  "Returns a fresh lambda-list without the &aux part."
  (exclude-for-keyword '&aux lambda-list :spanning T))

(defun whole-lambda-var (lambda-list)
  "Returns the &whole variable of the lambda-list."
  (when (eq (car lambda-list) '&whole)
    (second lambda-list)))

(defun environment-lambda-var (lambda-list)
  "Returns the environment variable of the lambda-list"
  (car (collect-for-keyword '&environment lambda-list :spanning NIL)))

(defun required-lambda-vars (lambda-list)
  "Returns all required variables of the ordinary-lambda-list."
  (loop for i in (if (eql '&whole (first lambda-list))
                     (cddr lambda-list)
                     lambda-list)
        until (lambda-keyword-p i)
        collect i))

(defun optional-lambda-vars (lambda-list)
  "Returns all optional variables of the ordinary-lambda-list."
  (flatten! (collect-for-keyword '&optional lambda-list)))

(defun rest-lambda-var (lambda-list)
  "Returns the rest variable of the ordinary-lambda-list."
  (car (collect-for-keyword '&rest lambda-list :spanning NIL)))

(defun body-lambda-var (lambda-list)
  "Returns the body variable of the ordinary-lambda-list."
  (car (collect-for-keyword '&body lambda-list :spanning NIL)))

(defun key-lambda-vars (lambda-list)
  "Returns all keyword variables of the ordinary-lambda-list."
  (loop for binding in (flatten! (collect-for-keyword '&key lambda-list))
        collect (unlist binding 1)))

(defun aux-lambda-vars (lambda-list)
  "Returns all auxiliary variables of the ordinary-lambda-list."
  (flatten! (collect-for-keyword '&aux lambda-list)))

(defun split-lambda-list (lambda-list)
  "Splits the lambda-list into its individual definition parts. Returns a list of values as follows:
 ((REQUIRED*) WHOLE ENVIRONMENT (OPTIONAL*) REST BODY (KEY*) (AUX*))"
  (loop with table = (make-hash-table)
        with in-keyword = NIL
        for i in lambda-list
        do (cond ((lambda-keyword-p i)
                  (setf in-keyword i))
                 (T
                  (push i (gethash in-keyword table))
                  (when (single-argument-keyword-p in-keyword)
                    (setf in-keyword NIL))))
        ;; Recollect and reverse
        finally (return (cons (nreverse (gethash NIL table))
                              (loop for keyword in *lambda-keywords*
                                    for vars = (gethash keyword table)
                                    unless (eq keyword '&allow-other-keys) ; Specially exclude
                                    collect (if (single-argument-keyword-p keyword)
                                                (car vars)
                                                (nreverse vars)))))))

(defmacro with-destructured-lambda-list ((&rest parts &key whole environment required optional rest body key aux &allow-other-keys) expression &body forms)
  "Destructures the given EXPRESSION into its lambda-list parts."
  (declare (ignore whole environment optional rest body key aux))
  (let ((bindings (list* (or required (gensym "REQUIRED"))
                         (loop for keyword in *lambda-keywords*
                               unless (eq keyword '&allow-other-keys)
                               collect (or (loop for (key val) on parts by #'cddr
                                                 do (when (string= key keyword :start2 1) (return val)))
                                           (gensym (subseq (string keyword) 1)))))))
    `(destructuring-bind ,bindings
         (split-lambda-list ,expression)
       (declare (ignore ,@(loop for symb in bindings
                                unless (symbol-package symb)
                                collect symb)))
       ,@forms)))

(defun construct-lambda-list (&key whole environment required optional rest body key allow-other-keys aux)
  "Construct a lambda-list out of the given parts."
  (macrolet ((splice (arg)
               (let ((symb (find-symbol (concatenate 'string "&" (symbol-name arg)))))
                 `(etypecase ,arg
                    (null NIL)
                    (list (list* ',symb ,arg))
                    (symbol (list ',symb ,arg))))))
    (append (splice whole)
            (splice environment)
            (when required required)
            (splice optional)
            (splice rest)
            (splice body)
            (splice key)
            (when allow-other-keys (list '&allow-other-keys))
            (splice aux))))
