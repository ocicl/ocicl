(uiop:define-package #:40ants-doc/autodoc/sections
  (:use #:cl))
(in-package #:40ants-doc/autodoc/sections)


(defvar *subsections*)

(setf (documentation '*subsections* 'variable)
      "This var will collect all subsections, collected inside WITH-SUBSECTION-COLLECTOR's body.")


(defun register-subsection (definition)
  (unless (boundp '*subsections*)
    (error "Function REGISTER-SUBSECTION should be called inside WITH-SUBSECTION-COLLECTOR body."))
  (push definition *subsections*))


(defun registered-subsections ()
  "Returns registered subsections in the order of their registration."
  (unless (boundp '*subsections*)
    (error "Function REGISTERED-SUBSECTIONS should be called inside WITH-SUBSECTION-COLLECTOR body."))
  (reverse *subsections*))


(defmacro with-subsection-collector (() &body body)
  `(let ((*subsections* (if (boundp '*subsections*)
                            ;; Allow to nest this macro and reuse
                            ;; the list in this case.
                            *subsections*
                            nil)))
     ,@body))

