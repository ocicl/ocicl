(uiop:define-package #:40ants-doc/reference
  (:use #:cl)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/source-api)
  (:import-from #:40ants-doc/locatives)
  (:import-from #:40ants-doc/locatives/base)
  (:import-from #:40ants-doc/object-package)
  (:export
   #:resolve
   #:reference
   #:reference-object
   #:reference-locative
   #:make-reference))
(in-package #:40ants-doc/reference)


(defclass reference ()
  ((object :initarg :object :reader reference-object)
   (locative :initarg :locative :reader reference-locative))
  (:documentation "A REFERENCE represents a path (REFERENCE-LOCATIVE)
  to take from an object (REFERENCE-OBJECT)."))

(defclass external-reference (reference)
  ((url :type string
        :initarg :url
        :reader external-reference-url))
  (:documentation "A full URL of external entity."))

(defun make-reference (object locative)
  (make-instance 'reference :object object :locative locative))

(defun make-external-reference (object locative url)
  (make-instance 'external-reference :object object
                                     :locative locative
                                     :url url))
(defun external-reference-p (obj)
  (typep obj 'external-reference))

(defmethod print-object ((object reference) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S ~S" (reference-object object)
            (reference-locative object))))

(defmethod 40ants-doc/object-package::object-package ((obj reference))
  (40ants-doc/object-package::object-package (reference-object obj)))

(defun reference= (reference-1 reference-2)
  (and (equal (reference-object reference-1)
              (reference-object reference-2))
       (equal (reference-locative reference-1)
              (reference-locative reference-2))))

(defun reference-locative-type (reference)
  (40ants-doc/locatives/base::locative-type (reference-locative reference)))



(defmethod canonical-reference ((reference reference))
  (handler-case
      (let ((object (resolve reference)))
        (if (typep object 'reference)
            object
            (canonical-reference object)))
    (40ants-doc/locatives/base::locate-error ()
      ;; DISLOCATED ends up here
      reference)))


;;; Return the unescaped name of the HTML anchor for REFERENCE. See
;;; HTML-SAFE-NAME.
(defun reference-to-anchor (reference)
  (let ((reference (canonical-reference reference)))
    (with-standard-io-syntax
      (prin1-to-string (list (reference-object reference)
                             (reference-locative reference))))))


;;; A list of all the references extracted from *LINKS* for
;;; convenience.
(defparameter *references*
  ;; KLUDGE: Include T explicitly, because it's oft used and would not
  ;; be recognized without markup because its name is too short. The
  ;; correct solution would be to add links automatically for the
  ;; hyperspec.
  (list (make-reference t '40ants-doc/locatives:dislocated)))


;;; Return the references from REFS which are for SYMBOL or which are
;;; for a non-symbol but resolve to the same object with SYMBOL.
(defun references-for-symbol (symbol refs n-chars-read)
  (let ((symbol-name (symbol-name symbol)))
    (or (remove-if-not (lambda (ref)
                         (or (eq symbol (reference-object ref))
                             ;; This function is only called when
                             ;; there is an interned symbol for
                             ;; something named by a string.
                             ;;
                             ;; KLUDGE: If the object of REF is
                             ;; replaced with SYMBOL, does it resolve
                             ;; to the same object? This is necessary
                             ;; to get package and asdf systems right,
                             ;; because the object in their canonical
                             ;; references are strings and we compare
                             ;; to symbols.
                             (equalp symbol-name (reference-object ref))))
                       refs)
        ;; Don't codify A, I and similar.
        (if (< 2 n-chars-read)
            (list (make-reference symbol '40ants-doc/locatives:dislocated))
            ()))))


(defun references-for-the-same-symbol-p (refs)
  (= 1 (length (remove-duplicates (mapcar #'reference-object refs)))))

;;; If there is a DISLOCATED reference, then don't link anywhere
;;; (remove all the other references).
(defun resolve-dislocated (refs)
  (let ((ref (find '40ants-doc/locatives:dislocated refs :key #'reference-locative-type)))
    (if ref
        (list ref)
        refs)))

(defun resolve-generic-function-and-methods (refs)
  (flet ((non-method-refs ()
           (remove-if (lambda (ref)
                        (member (reference-locative-type ref)
                                '(accessor reader writer method)))
                      refs)))
    (cond
      ;; If in doubt, prefer the generic function to methods.
      ((find 'generic-function refs :key #'reference-locative-type)
       (non-method-refs))
      ;; No generic function, prefer non-methods to methods.
      ((non-method-refs))
      (t
       refs))))


(defmethod 40ants-doc/source-api:find-source ((reference reference))
  "If REFERENCE can be resolved to a non-reference, call 40ANTS-DOC/SOURCE-API:FIND-SOURCE generic-function
  with it, else call [40ANTS-DOC/LOCATIVES/BASE:LOCATE-AND-FIND-SOURCE][generic-function] on the object,
  locative-type, locative-args slots of REFERENCE."
  (let ((object (resolve reference)))
    (typecase object
      (reference
       (let ((locative (reference-locative reference)))
         (40ants-doc/locatives/base:locate-and-find-source (reference-object reference)
                                                           (40ants-doc/locatives/base:locative-type locative)
                                                           (40ants-doc/locatives/base:locative-args locative))))
      (t
       (40ants-doc/source-api:find-source object)))))


(defun resolve (reference &key (errorp t))
  "A convenience function to 40ANTS-DOC/LOCATIVES/BASE:LOCATE REFERENCE's object with its
  locative."
  (40ants-doc/locatives/base:locate (reference-object reference)
                                    (reference-locative reference)
                                    :errorp errorp))


;;; We need this for more informative ERRORs and WARNINGs
;; TODO: Remove this var
(defvar *reference-being-documented* nil)

