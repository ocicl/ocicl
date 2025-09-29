(cl:in-package #:eclector.reader)

;;; Labeled objects

(deftype labeled-object-state ()
  '(member :defined :circular :final :final/circular))

(defstruct (%labeled-object (:constructor %make-labeled-object ())
                            (:predicate nil)
                            (:copier nil))
  (state :defined :type labeled-object-state)
  (object nil))

(defmethod call-with-label-tracking ((client t) (thunk t))
  ;; Establish a binding but don't allocate the hash table yet.
  (let ((*labels* nil))
    (funcall thunk)))

(defmethod note-labeled-object ((client t)
                                (input-stream t)
                                (label integer)
                                (parent t))
  ;; Allocate hash table lazily.
  (let ((labels (or *labels*
                    (setf *labels* (make-hash-table)))))
    (setf (gethash label labels)
          (make-labeled-object client input-stream label parent))))

(defmethod forget-labeled-object ((client t) (label integer))
  (remhash label *labels*))

(defmethod find-labeled-object ((client t) (label integer))
  (alexandria:when-let ((labels *labels*))
    (values (gethash label labels))))

(defmethod make-labeled-object ((client t)
                                (input-stream t)
                                (label integer)
                                (parent t))
  (%make-labeled-object))

(defmethod labeled-object-state ((client t) (object %labeled-object))
  (values (%labeled-object-state object) (%labeled-object-object object)))

(defmethod finalize-labeled-object ((client t)
                                    (labeled-object %labeled-object)
                                    (object t))
  (let ((new-state (case (%labeled-object-state labeled-object)
                     (:defined  :final)
                     (:circular :final/circular))))
    (setf (%labeled-object-state labeled-object) new-state
          (%labeled-object-object labeled-object) object)
    (values labeled-object new-state)))

(defmethod reference-labeled-object ((client t)
                                     (input-stream t)
                                     (labeled-object %labeled-object))
  (ecase (%labeled-object-state labeled-object)
    ((:final :final/circular) ; Use final object, if it has already been stored
     (%labeled-object-object labeled-object))
    (:defined ; Else, use LABELED-OBJECT as placeholder, fix up later
     (setf (%labeled-object-state labeled-object) :circular)
     labeled-object)
    (:circular ; Same but without changing the state
     labeled-object)))

(defmethod fixup-graph-p ((client t) (root-labeled-object %labeled-object))
  (eq (%labeled-object-state root-labeled-object) :final/circular))

;;; Fixup work tree
;;;
;;; In cases like
;;;
;;;   #1=(1 #1# #2=(2 #2# ... #100=(100 #100#)))
;;;
;;; the decision when to call FIXUP on which object greatly affects
;;; efficiency.  For example, calling FIXUP on the result of every
;;; recursive READ call (which produced an object in need of being
;;; fixed up) would lead to repeatedly traversing the portion of the
;;; object graph that is reachable from the respective object.
;;;
;;; The above problem could be solved by maintaining a stack of
;;; labeled objects which are currently being processed and delaying
;;; FIXUP calls that would be subsumed by FIXUP calls for objects on
;;; the stack.  However, cases like
;;;
;;;   #1=(1 #2=(2 ... #2#) ... #1#)
;;;
;;; would still not be processed efficiently because after reading,
;;; for example, #2=(2 ... #2#), it would not yet be clear that the
;;; outer object #1=(...) would also need fixing up, so the object
;;; arising for #2=(2 ... #2#) would be fixed up immediately.  Later,
;;; after finishing reading the outer object, the whole graph would be
;;; fixed up again.
;;;
;;; To address these efficiency problems, the following code defines
;;; labeled object protocol methods which maintain for each top-level
;;; READ call a tree of labeled objects for which some kind of fixup
;;; work has to be performed.  FIXUP is called for a (sub-)tree rooted
;;; at a particular labeled object if there is no ancestor labeled
;;; object that would include the (sub-)tree in its fixup work.
;;;
;;; This behavior could be tied to a mixin class for client classes if
;;; it turns out that this behavior is not appropriate in certain
;;; cases.  Potential problems could arise from the fact that the
;;; unspecialized (in terms of the client) MAKE-LABELED-OBJECT :AROUND
;;; method below prevents clients from opting out.

(defstruct (%fixup-node (:constructor %make-fixup-node (inner parent))
                        (:predicate nil)
                        (:copier nil))
  (inner (error "required") :read-only t)
  (parent (error "required") :read-only t)
  (children '() :type list))

(defmethod make-labeled-object :around ((client t)
                                        (input-stream t)
                                        (label integer)
                                        (parent t))
  (%make-fixup-node (call-next-method) parent))

(defmethod labeled-object-state (client (object %fixup-node))
  (labeled-object-state client (%fixup-node-inner object)))

(defmethod finalize-labeled-object ((client t)
                                    (labeled-object %fixup-node)
                                    (object t))
  (let* ((inner-labeled-object (%fixup-node-inner labeled-object))
         (new-state (nth-value 1 (finalize-labeled-object
                                  client inner-labeled-object object))))
    (when (or (eq new-state :final/circular)
              (not (null (%fixup-node-children labeled-object))))
      (alexandria:if-let ((parent (%fixup-node-parent labeled-object)))
        (push labeled-object (%fixup-node-children parent))
        (fixup-graph client labeled-object)))
    (values labeled-object new-state)))

(defmethod reference-labeled-object ((client t)
                                     (input-stream t)
                                     (labeled-object %fixup-node))
  (let* ((inner-labeled-object (%fixup-node-inner labeled-object))
         (result (reference-labeled-object
                  client input-stream inner-labeled-object)))
    (if (eq result inner-labeled-object)
        labeled-object
        result)))

;;; Fixup graph

(defmethod fixup-graph-p ((client t) (root-labeled-object %fixup-node))
  (and (null (%fixup-node-parent root-labeled-object))
       (or (not (null (%fixup-node-children root-labeled-object)))
           (let ((inner-labeled-object (%fixup-node-inner root-labeled-object)))
             (fixup-graph-p client inner-labeled-object)))))

(defmethod fixup-graph ((client t) (root-labeled-object %fixup-node)
                        &key (object-key (lambda (client labeled-object)
                                           (nth-value
                                            1 (labeled-object-state
                                               client labeled-object)))))
  (let ((object-key (alexandria:ensure-function object-key))
        (seen (make-hash-table :test #'eq)))
    (flet ((visit (client labeled-object)
             (let ((object (funcall object-key client labeled-object)))
               (fixup-all client object seen))))
      (declare (dynamic-extent #'visit))
      (walk-fixup-tree client #'visit root-labeled-object))))

(defmethod walk-fixup-tree ((client t)
                            (function t)
                            (root-labeled-object %fixup-node))
  (let ((function (alexandria:ensure-function function)))
    (labels ((rec (node)
               (let ((labeled-object (%fixup-node-inner node)))
                 (if (eq (labeled-object-state client labeled-object)
                         :final/circular)
                     (funcall function client labeled-object)
                     (mapc #'rec (%fixup-node-children node))))))
      (declare (dynamic-extent #'rec))
      (rec root-labeled-object))))

;;; Recursive object fixup
;;;
;;; We start from an object that was read and is known to contain
;;; labeled objects as placeholders for circular references.  All such
;;; placeholders have been finalized which means that they contain the
;;; respective object they stand in for.  The fixup processing
;;; recursively traverses places in the object and its mutable
;;; descendant objects and replaces placeholders with their respective
;;; final objects by modifying the containing places.  If the
;;; recursion gets too deep, unprocessed objects at the fringe are put
;;; onto a worklist and processed later via "fresh" `fixup' calls.

(defconstant +recursion-depth-limit+ 1000)

(deftype traversal-depth ()
  `(integer 0 ,+recursion-depth-limit+))

(defstruct (traversal-state
            (:constructor make-traversal-state (seen-objects enqueue))
            (:conc-name traversal-)
            (:predicate nil)
            (:copier nil))
  ;; Used as a set of all objects that have been encountered (not
  ;; necessarily fully processed) so far.
  (seen-objects (make-hash-table :test #'eq) :read-only t)
  ;; The nesting depth of `fixup' calls.
  (depth 0 :type traversal-depth)
  ;; A function that accepts an object as its sole argument and puts
  ;; that object onto the worklist for later processing.
  (enqueue (error "required") :type function :read-only t))

(defun fixup-all (client root state)
  ;; In case the recursive `fixup' calls get nested too deeply, we use
  ;; a FIFO queue.  In the common case, the queue is not touched at
  ;; all.
  (let ((tail nil) (worklist '()))
    (flet ((enqueue (object)
             (let ((cell (list object)))
               (if (null worklist)
                   (setf worklist cell)
                   (setf (cdr tail) cell))
               (setf tail cell))))
      (declare (dynamic-extent #'enqueue))
      (let ((traversal-state (make-traversal-state state #'enqueue)))
        (declare (dynamic-extent traversal-state))
        (fixup client root traversal-state)
        (when (not (null worklist))
          (loop until (null worklist)
                do (fixup client (pop worklist) traversal-state))
          ;; Slightly random assertion but this should happen rarely
          ;; and things are pretty bad if the assertion does not hold.
          (assert (zerop (traversal-depth traversal-state))))))))

;;; This method exists for backwards compatibility reasons: there may
;;; be clients which call `fixup' directly.
(defmethod fixup :around ((client t) (object t) (traversal-state hash-table))
  (fixup-all client object traversal-state))

(defmethod fixup :around ((client t)
                          (object t)
                          (traversal-state traversal-state))
  ;; Skip immutable objects (in terms of fixup processing) early to
  ;; lessen pressure on the hash-table in TRAVERSAL-STATE and generic
  ;; function dispatch.
  (unless (typep object '(or symbol character number package pathname))
    (let ((depth (traversal-depth traversal-state))
          (seen-objects (traversal-seen-objects traversal-state)))
      ;; Process OBJECT is the nesting of recursive `fixup' calls
      ;; isn't too deep.  Otherwise, enqueue OBJECT for later
      ;; processing.
      (unless (gethash object seen-objects)
        (cond ((< depth +recursion-depth-limit+)
               (setf (gethash object seen-objects) t)
               (progn
                 (setf (traversal-depth traversal-state) (1+ depth))
                 (call-next-method)
                 (setf (traversal-depth traversal-state) depth)))
              (t
               (funcall (traversal-enqueue traversal-state) object))))))
  nil)

(defmethod fixup ((client t) (object t) (traversal-state t))
  nil)

(defmacro fixup-case ((client value &key ((:state state-var) (gensym "STATE")))
                      (() &body normal-object-forms)
                      ((&optional object-var)
                       &body labeled-object-forms)
                      &optional ((() &body circular-labeled-object-forms) '(())))
  `(multiple-value-bind (,state-var ,@(when object-var `(,object-var)))
       (labeled-object-state ,client ,value)
     ;; If the state (in STATE-VAR) is null, VALUE is not a labeled
     ;; object. If the state is :FINAL or :FINAL/CIRCULAR, VALUE is a
     ;; labeled object that should be replaced by the object in
     ;; OBJECT-VAR. Otherwise (the state is :CIRCULAR), VALUE is a
     ;; labeled object for a different label as in #1=(1 #2=(#2# #1#))
     ;; when FIXUP-GRAPH is called for the object at #2=.
     (cond ((null ,state-var) ; not a labeled object
            ,@normal-object-forms)
           ((find ,state-var '(:final :final/circular)) ; finalized
            ,@labeled-object-forms)
           ,@(when circular-labeled-object-forms
               `((t ; not finalized
                  ,@circular-labeled-object-forms))))))

(defmethod new-value-for-fixup ((client         t)
                                (labeled-object t)
                                (current-value  t)
                                (final-value    t))
  ;; This is the default behavior: Simply replace the labeled object
  ;; marker CURRENT-VALUE with FINAL-VALUE.
  final-value)

(defmacro fixup-place-using-value (client place current-value traversal-state)
  (alexandria:once-only (client)
    (alexandria:with-unique-names (object)
      `(fixup-case (,client ,current-value)
         (()
          (fixup ,client ,current-value ,traversal-state))
         ((,object)
          (setf ,place (new-value-for-fixup
                        ,client ,current-value ,current-value ,object)))))))

(defmacro fixup-place (client place traversal-state)
  `(let ((current-value ,place))
     (fixup-place-using-value ,client ,place current-value ,traversal-state)))

(defmethod fixup ((client t) (object cons) (traversal-state t))
  (fixup-place client (car object) traversal-state)
  (fixup-place client (cdr object) traversal-state))

(defmethod fixup ((client t) (object array) (traversal-state t))
  ;; Fix up array elements unless the array element type indicates
  ;; that no fix up is required.
  (let ((element-type (array-element-type object)))
    (when (or (eq element-type 't) ; fast path
              (not (subtypep element-type '(or character number symbol))))
      (loop for i from 0 below (array-total-size object)
            do (fixup-place client (row-major-aref object i) traversal-state)))))

(defmethod fixup ((client t) (object standard-object) (traversal-state t))
  (loop for slot-definition in (closer-mop:class-slots (class-of object))
        for name = (closer-mop:slot-definition-name slot-definition)
        when (slot-boundp object name)
          do (fixup-place client (slot-value object name) traversal-state)))

(defmethod fixup ((client t) (object hash-table) (traversal-state t))
  (let ((key-changes '()))
    (maphash (lambda (key value)
               ;; If KEY has to be replaced, remove the entry for
               ;; KEY and VALUE (which is the only entry that may be
               ;; removed or changed according to the traversal
               ;; rules) and store the information for adding the
               ;; final key in KEY-CHANGES (since the traversal
               ;; rules forbid adding new entries). Fix up VALUE by
               ;; either fixing up the (gethash key object) place or
               ;; fixing up the value part of the queued KEY-CHANGES
               ;; entry.
               (fixup-case (client key)
                 (() ; not a labeled object
                  (fixup client key traversal-state)
                  (fixup-place-using-value
                   client (gethash key object) value traversal-state))
                 ((final-key) ; finalized labeled object
                  (remhash key object)
                  (let ((change (cons final-key value)))
                    (fixup-place-using-value
                     client (cdr change) value traversal-state)
                    (push change key-changes)))
                 (() ; labeled object not finalized
                  (fixup client value traversal-state))))
             object)
    (loop for (final-key . final-value) in key-changes
          do (setf (gethash final-key object) final-value))))
