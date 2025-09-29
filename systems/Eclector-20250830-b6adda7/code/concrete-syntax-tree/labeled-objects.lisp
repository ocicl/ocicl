(cl:in-package #:eclector.concrete-syntax-tree)

;;; Fixup

(defmethod eclector.reader:fixup ((client cst-client)
                                  (object cst:atom-cst)
                                  (traversal-state t))
  nil)

(defmethod eclector.reader:fixup ((client cst-client)
                                  (object cst:cons-cst)
                                  (traversal-state t))
  (macrolet ((fixup-place (place)
               ;; Determine the labeled object state of the raw value
               ;; of the CST stored in PLACE.  For a finalized labeled
               ;; object, create a "reference" CST object (which may
               ;; just be the parse result for the circular object,
               ;; depending on the client) and replace the value of
               ;; PLACE with that reference CST object.
               `(let* ((current-value ,place)
                       (labeled-object (cst:raw current-value)))
                  (eclector.reader:fixup-case (client labeled-object)
                    (() ; not a labeled object
                     (eclector.reader:fixup client current-value traversal-state))
                    ((final-value) ; finalized labeled object
                      (setf ,place (eclector.reader:new-value-for-fixup
                                    client labeled-object current-value final-value)))))))
    (fixup-place (slot-value object 'cst::%first))
    (fixup-place (slot-value object 'cst::%rest))))

(defmethod eclector.reader:new-value-for-fixup ((client         cst-client)
                                                (labeled-object t)
                                                (current-value  cst:cst)
                                                (final-value    t))
  ;; LABELED-OBJECT is a finalized labeled object from which the
  ;; (circular) final raw value and a not-yet-finalized parse result
  ;; can be obtained.  CURRENT-VALUE is that not-yet-finalized parse
  ;; result.  FINAL-VALUE is the final raw value.
  ;;
  ;; From these ingredients, we have to construct a parse result that
  ;; will replace CURRENT-VALUE in order to make the parse result tree
  ;; circular.  We make a `reference' instance which contains
  ;; LABELED-OBJECT and call `make-expression-result' so that the
  ;; client can decide whether to simply add the contained parse
  ;; result to the parse result tree and form a back-edge or to insert
  ;; some kind of first-class representation of the back-edge.
  (let* ((source (cst:source current-value))
         (reference (eclector.parse-result:make-reference labeled-object)))
    (declare (dynamic-extent reference))
    (eclector.parse-result:make-expression-result client reference '() source)))

;;; Explicit definition and reference CSTs
;;;
;;; These mixins allow clients to wrap CSTs for defined and referenced
;;; labeled objects in DEFINITION-CST and REFERENCE-CST instances
;;; respectively.

(defclass wrapper-cst (cst:cst)
  ((%target :initarg :target
            :reader  target)))

(defclass definition-cst (wrapper-cst) ())

(defclass reference-cst (wrapper-cst) ())

(defclass definition-csts-mixin () ())

(defclass reference-csts-mixin () ())

(defmethod eclector.reader:fixup ((client cst-client)
                                  (object definition-cst)
                                  (seen-objects t))
  (eclector.reader:fixup client (target object) seen-objects))

(defmethod eclector.reader:fixup ((client cst-client)
                                  (object reference-cst)
                                  (seen-objects t))
  nil) ; nothing to do

(macrolet ((labeled-object-result (client result source class)
             `(let ((labeled-object (eclector.parse-result:labeled-object ,result)))
                (multiple-value-bind (state object parse-result)
                    (eclector.reader:labeled-object-state ,client labeled-object)
                  (declare (ignore state))
                  (make-instance ',class :source ,source
                                         :raw object
                                         :target parse-result)))))

  (defmethod eclector.parse-result:make-expression-result
      ((client definition-csts-mixin)
       (result eclector.parse-result:definition)
       (children t)
       (source t))
    (labeled-object-result client result source definition-cst))

  (defmethod eclector.parse-result:make-expression-result
      ((client reference-csts-mixin)
       (result eclector.parse-result:reference)
       (children t)
       (source t))
    (labeled-object-result client result source reference-cst)))
