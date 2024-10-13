(uiop:define-package #:40ants-doc/glossary
  (:use #:cl)
  (:import-from #:40ants-doc/core)
  (:export
   #:define-glossary-term))
(in-package #:40ants-doc/glossary)


(defclass glossary-term ()
  ((name
    :initarg :name :reader glossary-term-name
    :documentation "The name of the global variable whose value is
    this GLOSSARY-TERM object.")
   (title
    :initarg :title :reader glossary-term-title
    :documentation "Used in generated documentation.")
   (docstring
    :initarg :docstring :reader glossary-term-docstring)))


(defmacro define-glossary-term
    (name (&key title (discard-documentation-p 40ants-doc/core:*discard-documentation-p*))
     docstring)
  "Define a global variable with NAME and set it to a glossary term
  object. A glossary term is just a symbol to hang a docstring on. It
  is a bit like a 40ANTS-DOC:SECTION in that, when linked to, its TITLE will be
  the link text instead of the name of the symbol. Unlike sections
  though, glossary terms are not rendered with headings, but in the
  more lightweight bullet + locative + name/title style.

  When DISCARD-DOCUMENTATION-P (defaults to 40ANTS-DOC:*DISCARD-DOCUMENTATION-P*)
  is true, DOCSTRING will not be recorded to save memory."
  `(defparameter ,name
     (make-instance 'glossary-term
                    :name ',name :title ,title
                    :docstring ,(unless discard-documentation-p
                                  docstring))))


(defmethod print-object ((glossary-term glossary-term) stream)
  (print-unreadable-object (glossary-term stream :type t)
    (format stream "~a" (glossary-term-name glossary-term))))

