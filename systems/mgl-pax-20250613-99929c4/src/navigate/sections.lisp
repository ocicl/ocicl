(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

;;;; Utilities for listing SECTIONs

(defvar *all-sections*)
(defvar *section-definitions*)
(defvar *parent-sections*)

;;; Lazily cache results of LIST-ALL-SECTIONS in BODY.
(defmacro with-sections-cache (() &body body)
  `(progv (unless (boundp '*all-sections*)
            '(*all-sections* *section-definitions* *parent-sections*))
       (unless (boundp '*all-sections*)
         `(:not-computed
           ,(make-hash-table :test #'eq)
           ,(make-hash-table :test #'eq)))
     ,@body))

;;; This is slow but fast enough not to bother with a SECTION-NAME to
;;; SECTION weak hash table. Some implementations (e.g. SBCL) have
;;; scaling issues with weak pointers.
(defun list-all-sections ()
  (if (boundp '*all-sections*)
      (if (eq *all-sections* :not-computed)
          (setq *all-sections* (list-all-sections-1))
          *all-sections*)
      (list-all-sections-1)))

(defun list-all-sections-1 ()
  (let ((sections ()))
    (do-all-symbols (symbol sections)
      (when (boundp symbol)
        (let ((value (symbol-value symbol)))
          (when (and (typep value 'section)
                     ;; Filter out normal variables with SECTION values.
                     (eq (section-name value) symbol))
            (pushnew value sections)))))))

(defun section-definitions (section)
  (if (boundp '*section-definitions*)
      (or (gethash section *section-definitions*)
          (setf (gethash section *section-definitions*)
                (section-definitions-1 section)))
      (section-definitions-1 section)))

(defun section-definitions-1 (section)
  (loop for entry in (section-entries section)
        ;; It doesn't make sense to talk about containing an INCLUDE,
        ;; and it is also a huge performance bottleneck as it accesses
        ;; files. See DREF* (method (t (eql include) t)).
        when (and (typep entry 'xref)
                  (not (eq (xref-locative-type entry) 'include)))
          collect (or (locate entry nil)
                      entry)))

(defun sections-that-contain (sections ref)
  (unless (eq (xref-locative-type ref) 'include)
    (let ((ref (or (locate ref nil) ref)))
      (remove-if-not (lambda (section)
                       (loop for definition in (section-definitions section)
                               thereis (xref= ref definition)))
                     sections))))

(defun find-parent-sections (object)
  (if (boundp '*parent-sections*)
      (or (gethash object *parent-sections*)
          (setf (gethash object *parent-sections*)
                (find-parent-sections-1 object)))
      (find-parent-sections-1 object)))

(defun find-parent-sections-1 (object)
  (let ((dref (locate object nil)))
    (when dref
      (let ((sections (sections-that-contain (list-all-sections) dref)))
        (sort-by-proximity sections (dref-name dref))))))

(defun home-section (object)
  """The home section of an object is a SECTION that contains the
  object's definition in its SECTION-ENTRIES or NIL. In the
  overwhelming majority of cases there should be at most one
  containing section.

  If there are multiple containing sections, the following apply.

  - If the DREF::@NAME of the definition is a non-keyword symbol, only
    those containing sections are considered whose package is closest
    to the SYMBOL-PACKAGE of the name, where closest is defined as
    having the longest common prefix between the two PACKAGE-NAMEs.

  - If there are multiple sections with equally long matches or the
    name is not a non-keyword symbol, then it's undefined which one is
    the home section.

  For example, `(MGL-PAX:DOCUMENT FUNCTION)` is an entry in the
  `\\MGL-PAX::@BASICS` section. Unless another section that contains
  it is defined in the MGL-PAX package, the home section is guaranteed
  to be `\\MGL-PAX::@BASICS` because the SYMBOL-PACKAGEs of
  MGL-PAX:DOCUMENT and `\\MGL-PAX::@BASICS` are the same (hence their
  common prefix is maximally long).

  This scheme would also work, for example, if the [home package][clhs]
  of DOCUMENT were `MGL-PAX/IMPL`, and it were reexported from
  `MGL-PAX` because the only way to externally change the home package
  would be to define a containing section in a package like
  `MGL-PAX/IMP`.

  Thus, relying on the package system makes it possible to find the
  intended home section of a definition among multiple containing
  sections with high probability. However, for names which are not
  symbols, there is no package system to advantage of."""
  (first (find-parent-sections object)))

(defun sort-by-proximity (sections object)
  (if (and (symbolp object)
           (not (keywordp object)))
      (let ((package-name (package-name (symbol-package object))))
        (sort (copy-list sections) #'>
              :key (lambda (section)
                     (or (mismatch package-name
                                   (package-name
                                    (symbol-package
                                     (section-name section))))
                         most-positive-fixnum))))
      sections))

(defun find-root-section (object)
  (let ((sectionp (typep (resolve object nil) 'section)))
    (multiple-value-bind (section depth)
        (if sectionp
            (values object 0)
            (values (first (find-parent-sections object)) 1))
      (loop for parent = (first (find-parent-sections section))
            while parent
            do (setq section parent)
               (incf depth)
            finally (return (values section depth))))))
