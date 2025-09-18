(uiop:define-package #:40ants-doc-full/locatives/asdf-system
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/reference)
  (:import-from #:common-doc
                #:make-paragraph
                #:make-unordered-list
                #:make-section
                #:make-list-item
                #:make-content
                #:make-web-link
                #:make-text)
  (:import-from #:40ants-doc-full/commondoc/section
                #:make-section-with-reference)
  (:import-from #:40ants-doc-full/commondoc/builder
                #:to-commondoc)
  (:import-from #:40ants-doc/locatives/asdf-system
                #:asdf-system-documentation-title))
(in-package #:40ants-doc-full/locatives/asdf-system)

(define-locative-type asdf:system ()
  "Refers to an asdf system. The generated documentation will include
  meta information extracted from the system definition. This also
  serves as an example of a symbol that's not accessible in the
  current package and consequently is not exported.

  A title of the documentation section can be modified if you'll
  define a method for 40ANTS-DOC/LOCATIVES/ASDF-SYSTEM:ASDF-SYSTEM-DOCUMENTATION-TITLE generic-function.
  Use EQL specifier for the method.")


(defun find-system (name)
  "ASDF:FIND-SYSTEM is 1000 times slower than ASDF:REGISTERED-SYSTEM,
   but REGISTERED-SYSTEM sometimes unable to find a system (for example
   when this is a primary ASDF system, but it's defpackage defines
   package with the name of primary system and a nickname equal to the
   subsystem name. See log4cl-extras/core as example).

   This we first try to use fast method and fallback to the slow one."
  (or (asdf:registered-system name)
      (asdf:find-system name)))


(defmethod locate-object (symbol (locative-type (eql 'asdf:system))
                          locative-args)
  (assert (endp locative-args))
  ;; FIXME: This is slow as hell.
  ;; TODO: check if replacement of find-system with registered-system helped
  (or (find-system symbol)
      (locate-error)))

(defmethod canonical-reference ((system asdf:system))
  (40ants-doc/reference:make-reference (asdf:primary-system-name system)
                                       'asdf:system))

(defmethod find-source ((system asdf:system))
  `(:location
    (:file ,(namestring (asdf/system:system-source-file system)))
    (:position 1)
    (:snippet "")))

(defmethod to-commondoc ((system asdf:system))
  (let ((title (asdf-system-documentation-title system)))
    (flet ((item (name getter &key type)
             (let* ((value (funcall getter system))
                    (href nil))
               (when value
                 (case type
                   (:link (setf href value))
                   (:mailto (setf href (format nil "mailto:~A"
                                               value)))
                   (:source-control (psetf value (format nil "~A"
                                                         (first value))
                                           href (second value))))
                 (make-list-item
                  (make-paragraph
                   (cond
                     ((eql type :asdf-systems)
                      (make-content
                       (list*
                        (make-text
                         (format nil "~A: "
                                 name))
                        (loop with first = t
                              for system-name in value
                              if first
                                do (setf first nil)
                              else
                                collect (make-text ", ")
                              collect (make-web-link (format nil "https://quickdocs.org/~A"
                                                             system-name)
                                                     (make-text system-name))))))
                     (href
                      (make-content
                       (list (make-text
                              (format nil "~A: "
                                      name))
                             (make-web-link href
                                            (make-text value)))))
                     (t
                      (make-text
                       (format nil "~A: ~A"
                               name
                               value))))))))))
      
      (let* ((items (list (item "Version" 'asdf/component:component-version)
                          (item "Description" 'asdf/system:system-description)
                          (item "Licence" 'asdf/system:system-licence)
                          (item "Author" 'asdf/system:system-author)
                          (item "Maintainer" 'asdf/system:system-maintainer)
                          (item "Mailto" 'asdf/system:system-mailto
                                :type :mailto)
                          (item "Homepage" 'asdf/system:system-homepage
                                :type :link)
                          (item "Bug tracker" 'asdf/system:system-bug-tracker
                                :type :link)
                          (item "Source control" 'asdf/system:system-source-control
                                :type :source-control)
                          (item "Depends on" 'asdf-system-dependencies
                                :type :asdf-systems)))
             (children (make-unordered-list
                        (remove nil items)))
             (reference (40ants-doc/reference-api:canonical-reference system)))
        (make-section-with-reference title
                                     children
                                     reference)))))

(defvar end-of-asdf-example)


(defgeneric asdf-system-dependencies (system)
  (:method ((system-name string))
    (asdf-system-dependencies (find-system system-name)))
  (:method ((system-name (eql nil)))
    ;; Sometimes find-system might return NIL
    ;; and if we'll not process it separately, execution will go back
    ;; to the method where system-name is a symbol leading to heap exhaustion.
    nil)
  (:method ((system-name symbol))
    (asdf-system-dependencies (find-system system-name)))
  (:method ((system asdf:system))
    (loop with base-system = (asdf:primary-system-name system)
          with results = nil
          for name in (asdf:system-depends-on system)
          for subsystem = (string-equal base-system
                                        (asdf:primary-system-name name))
          do (if subsystem
                 (setf results
                       (nunion results
                               (asdf-system-dependencies name)
                               :test #'string-equal ))
                 (pushnew (asdf:primary-system-name name) results
                          :test #'string-equal))
          finally (return (sort results
                                #'string<)))))

