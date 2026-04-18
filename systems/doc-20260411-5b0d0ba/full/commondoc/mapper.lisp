(uiop:define-package #:40ants-doc-full/commondoc/mapper
  (:use #:cl)
  (:import-from #:common-doc)
  (:import-from #:common-doc.ops)
  (:import-from #:40ants-doc-full/commondoc/bullet)
  (:import-from #:40ants-doc/object-package
                #:object-package)
  (:import-from #:serapeum
                #:soft-list-of
                #:->)
  (:export
   #:map-nodes
   #:node-supports-children
   #:with-node-package))
(in-package #:40ants-doc-full/commondoc/mapper)


(defvar *on-going-down* nil
  "A list of callbacks to be called inside MAP-NODES.")

(defvar *on-going-up* nil
  "A list of callbacks to be called inside MAP-NODES.")


(defvar *inside-title* nil
  "Will be set to T when mapper goes down into a section title.

   This can be useful if we don't want to do something
   inside titles.")

(defvar *inside-link* nil
  "Will be set to T when mapper goes down into a link

   This can be useful if we don't want to do something
   inside web link.")


(defun do-nothing (node)
  (declare (ignore node))
  (values))


(defun call-with-node-package (func)
  (let ((packages-stack nil))
    (flet ((set-package (node)
             (let ((package (or (object-package node)
                                *package*)))
               (push *package* packages-stack)
               (setf *package* package)))
           (reset-package (node)
             (declare (ignore node))
             (setf *package*
                   (pop packages-stack))))
      
      (let ((*on-going-down* (list* #'set-package *on-going-down*))
            (*on-going-up* (list* #'reset-package *on-going-up*))
            (*package* *package*))
        (funcall func)))))


(defmacro with-node-package (&body body)
  "This macro tracks current documentation piece's package and sets *package* accordingly."
  `(call-with-node-package (lambda () ,@body)))


(defvar *path*)


(defun call-with-node-path (func)
  (flet ((collect-section (node)
           (when (or (typep node 'common-doc:section)
                     (typep node '40ants-doc-full/commondoc/bullet::bullet))
             (push node *path*)))
         (pop-section (node)
           (when (or (typep node 'common-doc:section)
                     (typep node '40ants-doc-full/commondoc/bullet::bullet))
             (pop *path*))))
      
    (let ((*on-going-down* (list* #'collect-section *on-going-down*))
          (*on-going-up* (list* #'pop-section *on-going-up*))
          (*path* nil))
      (funcall func))))


(-> current-path ()
    (values (soft-list-of string) &optional))

(defun current-path ()
  "Returns a list of section titles (strings)."
  (unless (boundp '*path*)
    (error "Function CURRENT-PATH should be called inside WITH-NODE-PATH macro."))

  (loop for item in (reverse *path*)
        for title = (typecase item
                      (common-doc:section
                         (common-doc.ops:collect-all-text
                          (common-doc:title item)))
                      (40ants-doc-full/commondoc/bullet::bullet
                         ;; To always render a package specified version
                         ;; of symbol names, we need to set keyword package here
                         (let ((*package* (find-package :keyword)))
                           (40ants-doc-full/commondoc/bullet::bullet-name item))))
        collect title))


(defmacro with-node-path (&body body)
  "This macro tracks sections and subsections providing a CURRENT-PATH function.

   Useful for referencing a problematic node in a warning, because such path
   makes it easier to locate the node having an issue."
  `(call-with-node-path (lambda () ,@body)))


(defun process-node-with-children (node func &key
                                             (on-going-down #'do-nothing)
                                             (on-going-up #'do-nothing))
  (let* ((result (funcall func node))
         (children (when (node-supports-children result)
                     (common-doc:children result))))
      
    (when (node-supports-children result)
      (loop for callback in *on-going-down*
            do (funcall callback result))
      (funcall on-going-down result)
      
      (setf (common-doc:children result)
            (etypecase children
              (list (loop for child in (common-doc:children result)
                          collect (map-nodes child func
                                             :on-going-up on-going-up
                                             :on-going-down on-going-down)))
              ;; Sometimes (children) contains an object like
              ;; COMMON-DOC:UNORDERED-LIST
              (common-doc:document-node
               (map-nodes children func
                          :on-going-up on-going-up
                          :on-going-down on-going-down))))
      
      (funcall on-going-up result)
      (loop for callback in *on-going-up*
            do (funcall callback result)))
    
    (values result)))


(defgeneric node-supports-children (node)
  (:documentation "We have to use this function because some common-doc node types
                   supporting COMMON-DOC:CHILDREN do not share a common type.")
  
  (:method (node)
    nil)
  
  (:method ((node common-doc:base-list))
    t)
  
  (:method ((node common-doc:document))
    t)
  
  (:method ((node common-doc:content-node))
    t))


(defgeneric map-nodes (node func &key)
  (:documentation "Recursively replaces or modifies a CommonDoc NODE with results of the FUNC call.")
  
  (:method (node func &key
                        (on-going-down #'do-nothing)
                        (on-going-up #'do-nothing))
    (if (node-supports-children node)
        (process-node-with-children node func
                                    :on-going-down on-going-down
                                    :on-going-up on-going-up)
        (funcall func node)))
  
  (:method ((node common-doc:base-list) func &key
                                             (on-going-down #'do-nothing)
                                             (on-going-up #'do-nothing))
    (process-node-with-children node func
                                :on-going-down on-going-down
                                :on-going-up on-going-up))
  
  (:method ((node common-doc:document) func &key
                                            (on-going-down #'do-nothing)
                                            (on-going-up #'do-nothing))
    (process-node-with-children node func
                                :on-going-down on-going-down
                                :on-going-up on-going-up))
  
  (:method ((node common-doc:content-node) func &key
                                                (on-going-down #'do-nothing)
                                                (on-going-up #'do-nothing))
    (process-node-with-children node func
                                :on-going-down on-going-down
                                :on-going-up on-going-up))
  
  (:method ((node common-doc:link) func &key
                                        (on-going-down #'do-nothing)
                                        (on-going-up #'do-nothing))
    (declare (ignore on-going-down on-going-up))
    (let ((*inside-link* t))
      (call-next-method)))

  (:method  ((node common-doc:section) func &key (on-going-down #'do-nothing)
                                                 (on-going-up #'do-nothing))
    (let* ((result (call-next-method))
           (*inside-title* t))
      (loop for callback in *on-going-down*
            do (funcall callback result))
      (funcall on-going-down result)

      (setf (common-doc:title node)
            (mapcar func (common-doc:title node)))

      (funcall on-going-up result)
      (loop for callback in *on-going-up*
            do (funcall callback result))
      
      result)))

