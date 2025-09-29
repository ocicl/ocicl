(cl:in-package #:eclector.parse-result)

;;; Each instance wraps an "inner", ordinary labeled object (that is
;;; whatever the next MAKE-LABELED-OBJECT method for the current
;;; client returns) so that a parse result can be associated with the
;;; labeled object in addition to the final object which the inner
;;; labeled object stores.  An additional "outer" labeled object is
;;; stored in case additional methods on MAKE-LABELED-OBJECT in turn
;;; wrap the %WRAPPER instance in some other object (which is the case
;;; by default; see the "Fixup work tree" section in
;;; reader/labeled-objects.lisp).
;;;
;;; The STATE and PARSE-RESULT slots track the associated parse
;;; result.  The initial state is :UNDEFINED which is changed to
;;; :AVAILABLE when a nested READ call produces the required parse
;;; result (this transition may never happen due to error recovery).
;;; Once an expression result has been produced for the (circular or
;;; non-circular) labeled object, the state changes again to :DEFINED
;;; which signals to the parse result construction for subsequent
;;; references to the labeled object that the stored parse result can
;;; be used.  In uncommon cases like error recovery, a
;;; FORGET-LABELED-OBJECT call changes the state to :FORGOTTEN.
(defstruct (%wrapper (:constructor %make-wrapper (inner))
                     (:predicate nil)
                     (:copier nil))
  (inner (error "required") :read-only t)
  (outer)
  (state :undefined :type (member :undefined :available :defined :forgotten))
  (parse-result nil))

;;; A binding with value NIL is established in the READ-MAYBE-NOTHING
;;; method specialized to PARSE-RESULT-CLIENT.  The value of the
;;; binding is changed to a %WRAPPER object in a MAKE-LABELED-OBJECT
;;; below.
(defvar *wrapper* nil)

(defmethod eclector.reader:forget-labeled-object
    :before ((client parse-result-client) (label integer))
  ;; If the labeled object designated by LABEL is forgotten due to
  ;; error recovery, mark the current wrapper (if any) as forgotten.
  ;; Without marking the wrapper like that, the parse result
  ;; associated with the wrapper could appear in the final parse
  ;; result graph which would be bad because that parse result
  ;; corresponds to a labeled object marker that has not undergone
  ;; fixup processing (because of the error recovery).
  (when-let ((wrapper *wrapper*))
    (setf (%wrapper-state wrapper) :forgotten)))

(defmethod eclector.reader:make-labeled-object ((client parse-result-client)
                                                (input-stream t)
                                                (label integer)
                                                (parent t))
  ;; Wrap inner labeled object.
  (let ((labeled-object (call-next-method)))
    (setf *wrapper* (%make-wrapper labeled-object))))

(defmethod eclector.reader:make-labeled-object :around ((client parse-result-client)
                                                        (input-stream t)
                                                        (label integer)
                                                        (parent t))
  ;; Remember outermost labeled object in wrapper.
  (let ((labeled-object (call-next-method)))
    (setf (%wrapper-outer *wrapper*) labeled-object)
    labeled-object))

(defmethod eclector.reader:labeled-object-state ((client parse-result-client)
                                                 (labeled-object %wrapper))
  ;; Get state from inner labeled object and return the associated
  ;; parse result as an additional value.
  (let* ((inner-labeled-object (%wrapper-inner labeled-object))
         (parse-result (%wrapper-parse-result labeled-object)))
    (multiple-value-bind (state object)
        (eclector.reader:labeled-object-state client inner-labeled-object)
      (values state object parse-result inner-labeled-object))))

(defmethod eclector.reader:finalize-labeled-object ((client parse-result-client)
                                                    (labeled-object %wrapper)
                                                    (object t))
  ;; Delegate everything to the inner labeled object.
  (let* ((inner-labeled-object (%wrapper-inner labeled-object))
         (new-state (nth-value
                     1 (eclector.reader:finalize-labeled-object
                        client inner-labeled-object object))))
    (values labeled-object new-state)))

(defmethod eclector.reader:reference-labeled-object ((client parse-result-client)
                                                     (input-stream t)
                                                     (labeled-object %wrapper))
  ;; Stash LABELED-OBJECT for MAKE-EXPRESSION-RESULT, delegate
  ;; everything else to the inner labeled object.
  (setf *wrapper* labeled-object)
  (let* ((inner-labeled-object (%wrapper-inner labeled-object))
         (result (eclector.reader:reference-labeled-object
                  client input-stream inner-labeled-object)))
    (if (eq result inner-labeled-object)
        labeled-object
        result)))

(defmethod make-expression-result :around ((client parse-result-client)
                                           (result t)
                                           (children t)
                                           (source t))
  ;; This is the complicated one.  In case MAKE-LABELED-OBJECT or
  ;; REFERENCE-LABELED-OBJECT was called in the READ call for which we
  ;; construct the expression result, *WRAPPER* is bound to a %WRAPPER
  ;; instance.
  (if-let ((wrapper *wrapper*))
    (let* ((inner-labeled-object (%wrapper-inner wrapper))
           (state (eclector.reader:labeled-object-state
                   client inner-labeled-object)))
      (cond ;; In this case, the parse result has been "forgotten",
            ;; for example due to error recovery in a case like
            ;; #1=#1#.  We have to remove the labeled object marker
            ;; from CHILDREN since there will be no fixup.
            ((eq (%wrapper-state wrapper) :forgotten)
             ;; We don't use
             ;;
             ;;   (call-next-method client result other-children source)
             ;;
             ;; since CHILDREN could be a proper list and
             ;; OTHER-CHILDREN could be null which could change the
             ;; set of applicable methods (which is illegal).
             (let* ((parse-result (%wrapper-parse-result wrapper))
                    (other-children (remove parse-result children))
                    (*wrapper* nil)) ; avoid re-entry
               (make-expression-result client result other-children source)))
            ;; In this case, the inner parse result is not yet finalized.
            ((not (find state '(:final :final/circular)))
             (call-next-method))
            ;; In this case the parse result which corresponds to the
            ;; definition of the labeled is fully defined, so this
            ;; occurrence must be a subsequent reference.
            ((eq (%wrapper-state wrapper) :defined)
             (let ((reference (make-reference wrapper))
                   (*wrapper* nil)) ; avoid re-entry
               (declare (dynamic-extent reference))
               (make-expression-result client reference children source)))
            ;; In this case, the inner parse result is finalized and a
            ;; nested READ call has made available a parse result
            ;; which corresponds to the definition of the labeled
            ;; object.
            ((eq (%wrapper-state wrapper) :available)
             (let ((parse-result (%wrapper-parse-result wrapper))
                   (*wrapper* nil)) ; avoid re-entry
               ;; The outermost wrapping labeled object may have more
               ;; information regarding when to fix things up.
               (let ((outer (%wrapper-outer wrapper)))
                 (when (eclector.reader:fixup-graph-p client outer)
                   ;; Fix up the graph of parse results.
                   (eclector.reader:fixup-graph
                    client outer
                    :object-key (lambda (client labeled-object)
                                  (declare (ignore client))
                                  (%wrapper-parse-result labeled-object)))))
               ;; The parse result which corresponds to the labeled
               ;; object is now fully defined.  Subsequent references
               ;; to the labeled object can use the information in
               ;; WRAPPER.
               (setf (%wrapper-state wrapper) :defined)
               ;; Due to custom reader macros which may have bypassed
               ;; our machinery or due to error recovery, RESULT may
               ;; be different from the object in WRAPPER.
               (let ((definition (make-definition wrapper)))
                 (declare (dynamic-extent definition))
                 (if (eq result (nth-value
                                 1 (eclector.reader:labeled-object-state
                                    client wrapper)))
                     ;; RESULT is the object of WRAPPER.  That means
                     ;; it is safe to build the parse result from
                     ;; DEFINITION and CHILDREN.
                     (make-expression-result client definition children source)
                     ;; RESULT is different from the object in
                     ;; WRAPPER.  Call the next method with RESULT and
                     ;; CHILDREN so that we can be sure that we return
                     ;; a parse result with RESULT as the "raw" object
                     ;; and CHILDREN as the children (instead of the
                     ;; possible incorrect data we would get from
                     ;; WRAPPER.  However, Try to slip a labeled
                     ;; object definition parse result into children
                     ;; to give the client a chance to reconstruct a
                     ;; suitable parse result sub-tree.
                     (let* ((definition-result (make-expression-result
                                                client definition '() source))
                            (children (substitute
                                       definition-result parse-result children)))
                       (call-next-method client result children source))))))
            ;; In this case, the inner parse result is finalized but
            ;; there is no parse result in WRAPPER, for example due to
            ;; error recovery.
            (t
             ;; This can happen when the clients chooses to recover
             ;; for input like #1=#|comment|#<eof>.  In that case, no
             ;; usable child parse result has been recorded.  This is
             ;; fine however, since the next method will simply
             ;; produce a parse result for the replacement value which
             ;; is the value of RESULT.
             (call-next-method))))
    ;; *WRAPPER* is null, so no definition or reference so nothing to
    ;; do.
    (call-next-method)))

(defmethod make-expression-result ((client parse-result-client)
                                   (result definition)
                                   (children t)
                                   (source t))
  ;; This method implements the default behavior of simply extracting
  ;; and returning the parse result which represents the object that
  ;; is defined by the labeled object definition (and therefore not
  ;; represent the labeled object definition itself as a parse
  ;; result).
  (let ((labeled-object (labeled-object result)))
    (nth-value
     2 (eclector.reader:labeled-object-state client labeled-object))))

(defmethod make-expression-result ((client parse-result-client)
                                   (result reference)
                                   (children t)
                                   (source t))
  ;; This method implements the default behavior of simply extracting
  ;; and returning the parse result which represents the object that
  ;; is referenced by the labeled object reference (and therefore not
  ;; represent the labeled object reference itself as a parse result).
  (let ((labeled-object (labeled-object result)))
    (nth-value
     2 (eclector.reader:labeled-object-state client labeled-object))))

(defmethod eclector.reader:fixup-graph-p ((client parse-result-client)
                                          (root-labeled-object %wrapper))
  (let ((inner-labeled-object (%wrapper-inner root-labeled-object)))
    (eclector.reader:fixup-graph-p client inner-labeled-object)))
