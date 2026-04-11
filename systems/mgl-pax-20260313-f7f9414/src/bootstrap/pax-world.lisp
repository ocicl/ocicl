(in-package :mgl-pax)

(defsection @pax-world (:title "PAX World")
  "PAX World is a registry of documents, which can generate
  cross-linked HTML documentation pages for all the registered
  documents. There is an official [PAX
  World](https://melisgl.github.io/mgl-pax-world/)."
  (register-doc-in-pax-world function)
  "For example, this is how PAX registers itself:"
  (register-doc-example (include (:start (pax-sections function)
                                  :end (end-of-register-doc-example variable))
                                 :header-nl "```"
                                 :footer-nl "```"))
  (update-pax-world function))

;;; (NAME SECTIONS-OR-FUNCTION-DESIGNATOR PAGE-SPECS-OR-FUNCTION-DESIGNATOR)
(defvar *registered-pax-world-docs* ())

(defun list-designator (x)
  (if (or (listp x) (symbolp x) (functionp x))
      x
      (list x)))

(defun register-doc-in-pax-world (name sections page-specs)
  "Register SECTIONS and PAGE-SPECS under NAME (a symbol) in PAX
  World. By default, UPDATE-PAX-WORLD generates documentation for all
  of these. SECTIONS and PAGE-SPECS must be lists of SECTIONs and
  PAGE-SPECs (SEE DOCUMENT) or designators of function of no arguments
  that return such lists."
  (declare (type symbol name))
  (setq *registered-pax-world-docs*
        (remove name *registered-pax-world-docs* :key #'first))
  (push (list name (list-designator sections)
              (list-designator page-specs))
        *registered-pax-world-docs*))

(declaim (ftype function make-git-source-uri-fn))
(declaim (ftype function make-github-source-uri-fn))
