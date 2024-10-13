;;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Base: 10; Package: JSON -*-
;;;; Copyright (c) 2006-2008 Henrik Hjelte
;;;; All rights reserved.
;;;; See the file LICENSE for terms of use and distribution.

(in-package :json)

(defun range-keys (var-keys)
  (loop for var-key in var-keys
     for (primary-key . subkey) =
       (destructuring-bind (var . key) var-key
         (let ((dot (position #\. key :test #'char=)))
           (if dot
               (cons (subseq key 0 dot) (cons var (subseq key (1+ dot))))
               (cons key var))))
     for subkeys-of-primary =
       (assoc primary-key subkeys :test #'string=)
     if subkeys-of-primary
       do (push subkey (cdr subkeys-of-primary))
     else
       collect (cons primary-key (list subkey)) into subkeys
     finally (return subkeys)))

(defun json-bind-level-customizations (level-keys value-required
                                       decoder validator
                                       key-handler value-handler pass)
  (loop for (key . subs) in (range-keys level-keys)
    with subkeys and vars-to-bind
    do (loop for sub in subs
          initially (setq subkeys nil vars-to-bind nil)
          if (consp sub) do (push sub subkeys)
          else do (push sub vars-to-bind))
    collect
      `((string= key ,key)
        (set-custom-vars
         :internal-decoder
           ,(if (endp subkeys)
                (if vars-to-bind decoder validator)
                (cons 'custom-decoder
                      (json-bind-level-customizations
                       subkeys vars-to-bind decoder validator
                       key-handler value-handler pass)))
         :object-value
           (lambda (value)
             (declare (ignorable value))
             ,@(loop for var in vars-to-bind collect `(setq ,var value))
             ,(if value-required `(funcall ,value-handler value)))))
      into match-clauses
    finally
      (return
        `(:object-key
          (lambda (key)
            (let ((key (funcall *json-identifier-name-to-lisp* key)))
              (cond
                ,@match-clauses
                (t ,(if value-required
                        (list 'set-custom-vars
                              :internal-decoder decoder
                              :object-value value-handler)
                        (list 'set-custom-vars
                              :internal-decoder validator
                              :object-value pass)))))
            ,(if value-required
                 `(funcall ,key-handler key)))))))

(defmacro json-bind ((&rest vars) json-source &body body)
  (let-gensyms (decoder validator value-handler key-handler pass)
    (let ((vars-tmp (loop repeat (length vars) collect (gensym))))
      `(let (,@vars-tmp (,pass (constantly t)))
         (let ((,validator
                (custom-decoder
                 :beginning-of-object ,pass :object-key ,pass
                 :object-value ,pass :end-of-object ,pass
                 :beginning-of-array ,pass :array-member ,pass
                 :end-of-array ,pass :beginning-of-string ,pass
                 :string-char ,pass :end-of-string ,pass
                 :internal-decoder 'decode-json))
               (,decoder (current-decoder))
               (,key-handler *object-key-handler*)
               (,value-handler *object-value-handler*))
           (declare (ignorable ,decoder ,key-handler ,value-handler))
           ,(if (null vars)
                `(decode-json-from-source ,json-source ,validator)
                `(bind-custom-vars
                     (,@(json-bind-level-customizations
                         (loop for var in vars for var-tmp in vars-tmp
                           collect (cons var-tmp (symbol-name var)))
                         nil decoder validator
                         key-handler value-handler pass)
                      :aggregate-scope
                        (union *aggregate-scope-variables*
                               '(*object-key-handler*
                                 *object-value-handler*
                                 *internal-decoder*)))
                   (decode-json-from-source ,json-source))))
         (let ,(mapcar #'list vars vars-tmp)
           ,@body)))))

;;; Old code:

;;; helpers for json-bind
;(defun cdas(item alist)
;  "Alias for (cdr (assoc item alist))"
;  (cdr (assoc item alist)))
;
;(defun last1 (lst)
;  (first (last lst)))
;
;(defmacro assoc-lookup (&rest lookuplist)
;  "(assoc-lookup :x :y alist) => (cdr (assoc :y (cdr (assoc :x alist))))"
;  (let ((alist-form (last1 lookuplist))
;        (lookups (reverse (butlast lookuplist))))
;    (labels ((mk-assoc-lookup (lookuplist)
;               (if lookuplist
;                  `(cdas ,(first lookuplist) ,(mk-assoc-lookup (rest lookuplist)))
;                  alist-form)))
;      (mk-assoc-lookup lookups))))
;
;(defmacro json-bind (vars json-string-or-alist &body body)
;  (labels ((symbol-as-string (symbol)
;           (string-downcase (symbol-name symbol)))
;          (split-by-dots (string)
;           (loop for ch across string
;                 with x
;                 with b
;                 do (if (char= #\. ch)
;                        (progn 
;                          (push (concatenate 'string (nreverse b)) x)
;                          (setf b nil))
;                        (push ch b))
;                 finally (progn
;                           (push (concatenate 'string (nreverse b)) x)
;                           (return (nreverse x)))))
;         (lookup-deep (variable)
;           (mapcar (lambda (nm) `(json-intern ,nm))
;                   (split-by-dots (symbol-as-string variable)))))
;     (let ((a-list (gensym)))
;      `(let ((,a-list (if (stringp ,json-string-or-alist)
;                          (decode-json-from-string ,json-string-or-alist)
;                          ,json-string-or-alist)))
;        (let ,(loop for v in vars collect `(,v (assoc-lookup ,@(lookup-deep v)
;                                                ,a-list)))
;          ,@body)))))
