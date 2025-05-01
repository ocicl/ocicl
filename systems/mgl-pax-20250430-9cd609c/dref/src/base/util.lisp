(in-package :dref)

(defun ensure-list* (obj)
  (if (listp obj)
      obj
      (list obj)))

;;; https://gitlab.common-lisp.net/alexandria/alexandria/-/issues/46#note_16734
(defmacro nth-value-or* (nth-value &body forms)
  (case (length forms)
    ((0) nil)
    ((1) (first forms))
    (t
     (let ((%nth-value (gensym "NTH-VALUE"))
           (%values (gensym "VALUES")))
       `(let ((,%nth-value ,nth-value)
              (,%values (multiple-value-list ,(first forms))))
          (if (nth ,%nth-value ,%values)
              (values-list ,%values)
              (nth-value-or* ,%nth-value ,@(rest forms))))))))

(defun parse-body-docstring (body)
  (if (and (stringp (first body))
           (<= 2 (length body)))
      (values (first body) (rest body))
      (values () body)))

(defun parse-body-declare (body)
  (loop for form in body
        for rest on body
        while (and (listp form)
                   (eq (first form) 'declare))
        collect form into declarations
        finally (return (values declarations rest))))

;;; Return the names of the arguments in the [macro lambda list][clhs]
;;; ARGLIST.
(defun macro-arg-names (arglist)
  (unless (eq arglist :not-available)
    (let ((names ()))
      (labels ((foo (arglist)
                 (let ((seen-special-p nil))
                   (loop for rest on arglist
                         do (let ((arg (car rest)))
                              (cond ((member arg '(&key &optional &rest &body
                                                   &allow-other-keys))
                                     (setq seen-special-p t))
                                    ((symbolp arg)
                                     (push arg names))
                                    (seen-special-p
                                     (when (symbolp (first arg))
                                       (push (first arg) names)))
                                    (t
                                     (foo arg))))
                            (unless (listp (cdr rest))
                              (push (cdr rest) names))))))
        (foo arglist))
      (reverse names))))

(defmacro succeedsp (&body body)
  `(null (nth-value 1 (ignore-errors (values ,@body)))))


(defmacro on-unknown-type-warning ((&optional (value-form nil)) &body body)
  #-(or sbcl cmucl) (declare (ignore value-form))
  `(handler-case
       (progn ,@body)
     ;; Avoid "WARNING: * is not permitted as a type specifier" on
     ;; SBCL.
     #+sbcl
     (warning (c) (ignore-errors (muffle-warning c))
       ,value-form)
     ;; Silence compiler notes on SBCL when run via ASDF:TEST-SYSTEM.
     #+sbcl
     (sb-kernel:parse-unknown-type ()
       ,value-form)
     #+cmucl
     (sys::parse-unknown-type ()
       ,value-form)))

(defun valid-type-specifier-p (type-specifier)
  (cond ((member-type-specifier-p type-specifier))
        ((satisfies-type-specifier-p type-specifier)
         (valid-satisisfies-type-specifier-args-p (rest type-specifier)))
        (t
         ;; TYPEP does not signal errors on ABCL
         #+abcl
         (or (and (atom type-specifier)
                  (or (gethash type-specifier system::*known-types*)
                      (find-class type-specifier nil)
                      (eq type-specifier nil)))
             (let ((name (type-specifier-name type-specifier)))
               (if (and (not (atom type-specifier))
                        (member name '(and or not)))
                   (every #'valid-type-specifier-p (rest type-specifier))
                   (let ((expander (get name 'system::deftype-definition)))
                     (or (and expander
                              (succeedsp (funcall expander
                                                  (if (atom type-specifier)
                                                      ()
                                                      (rest type-specifier)))))
                         ;; (INTEGER 3 5) and co have no expanders.
                         (subtypep name 'number))))))
         #-abcl
         (on-unknown-type-warning (nil)
           (succeedsp (typep nil type-specifier))))))

(defun type-specifier-name (type-specifier)
  (if (listp type-specifier)
      (first type-specifier)
      type-specifier))

(defun member-type-specifier-p (type-specifier)
  (and (not (atom type-specifier))
       (eq (first type-specifier) 'member)))

(defun satisfies-type-specifier-p (type-specifier)
  (and (not (atom type-specifier))
       (eq (first type-specifier) 'satisfies)))

(defun valid-satisisfies-type-specifier-args-p (args)
  (and (= (length args) 1)
       (symbolp (first args))))
