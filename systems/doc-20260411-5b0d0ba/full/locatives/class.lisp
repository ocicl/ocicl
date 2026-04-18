(uiop:define-package #:40ants-doc-full/locatives/class
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc-full/commondoc/bullet)
  (:import-from #:40ants-doc-full/commondoc/arglist)
  (:import-from #:40ants-doc/docstring)
  (:import-from #:40ants-doc-full/commondoc/markdown)
  (:import-from #:40ants-doc-full/commondoc/builder
                #:to-commondoc))
(in-package #:40ants-doc-full/locatives/class)

(define-locative-type class ())

(define-locative-type condition ())

(defmethod locate-object (symbol (locative-type (eql 'class)) locative-args)
  (declare (ignore locative-args))
  (or (find-class symbol :errorp nil)
      (locate-error)))

(defmethod locate-object (symbol (locative-type (eql 'condition))
                          locative-args)
  (assert (= 0 (length locative-args)))
  (let ((class (find-class symbol :errorp nil)))
    (unless (subtypep class 'condition)
      (locate-error))
    class))


(defmethod canonical-reference ((class class))
  (if (subtypep class 'condition)
      (40ants-doc/reference:make-reference (class-name class) 'condition)
      (40ants-doc/reference:make-reference (class-name class) 'class)))


(defmethod to-commondoc ((class class))
  (let* ((conditionp (subtypep class 'condition))
         (symbol (class-name class))
         (superclasses
           (remove-if (lambda (name)
                        (or (eq name 'standard-object)
                            (and conditionp (eq name 'condition))))
                      (mapcar #'class-name
                              (swank-mop:class-direct-superclasses class))))
         (docstring (40ants-doc/docstring:get-docstring class t))
         (children (when docstring
                     (40ants-doc-full/commondoc/markdown:parse-markdown docstring))))

    (40ants-doc-full/commondoc/bullet:make-bullet (canonical-reference class)
                                                  ;; TODO: transform superclasses to XREFs
                                                  :arglist (40ants-doc-full/commondoc/arglist::make-arglist superclasses)
                                                  :children children
                                                  :ignore-words symbol)))

(defun find-known-reference (reference)
  (find reference 40ants-doc/reference::*references* :test #'40ants-doc/reference::reference=))
