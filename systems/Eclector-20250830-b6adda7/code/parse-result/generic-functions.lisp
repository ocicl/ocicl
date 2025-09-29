(cl:in-package #:eclector.parse-result)

;;; Parse result protocol

(defgeneric make-expression-result (client result children source)
  (:argument-precedence-order result client children source))

(defgeneric make-skipped-input-result (client stream reason children source)
  (:method ((client t) (stream t) (reason t) (children t) (source t))
    nil))

;;; The purpose of the following structure classes is to
;;; 1) indicate to the client that a parse result represents a labeled
;;;    object definition or labeled object reference
;;; 2) pass the labeled object to the client
;;; To this end, Eclector calls the MAKE-EXPRESSION-RESULT generic
;;; function with instances of the DEFINITION and REFERENCE classes as
;;; the RESULT argument.  Clients can specialize the RESULT parameter
;;; of methods on MAKE-EXPRESSION-RESULT to DEFINITION and REFERENCE
;;; and use the LABELED-OBJECT reader to obtain the labeled object
;;; from the RESULT argument.
(defstruct (labeled-object-result (:conc-name nil)
                                  (:constructor nil)
                                  (:predicate nil)
                                  (:copier nil))
  (labeled-object nil :read-only t))

(declaim (inline make-definition make-reference))
(defstruct (definition (:include labeled-object-result)
                       (:constructor make-definition (labeled-object))
                       (:predicate nil)
                       (:copier nil)))

(defstruct (reference (:include labeled-object-result)
                      (:constructor make-reference (labeled-object))
                      (:predicate nil)
                      (:copier nil)))
