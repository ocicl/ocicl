;;;; parsing.lisp
;;;;
;;;; Eclector-based parsing with position tracking
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025 Anthony Green

(in-package #:ocicl.lint)

;; Custom Eclector client to capture source positions
(defclass position-tracking-client (eclector.parse-result:parse-result-client)
  ()
  (:documentation "Eclector client for capturing source positions during parsing."))

(defmethod eclector.parse-result:make-expression-result
    ((client position-tracking-client) (result t) (children t) (source t))
  ;; Return a cons of the actual result and its source position
  (cons result source))

;; Enhanced client that builds a position-aware parse tree
(defclass parse-tree-client (eclector.parse-result:parse-result-client)
  ()
  (:documentation "Enhanced Eclector client that builds position-aware parse trees."))

;; Lenient client for linting that creates packages on demand
(defclass lenient-parse-client (parse-tree-client)
  ()
  (:documentation "Lenient Eclector client that auto-creates missing packages for linting."))

;; Handle unknown dispatch characters that are configured as supported
(defmethod eclector.reader:call-reader-macro ((client lenient-parse-client)
                                               input-stream
                                               char
                                               sub-char)
  ;; If we get an unknown dispatch character that's in our supported list,
  ;; just read and ignore the content instead of erroring
  (handler-case
      (call-next-method)
    (eclector.reader:unknown-macro-sub-character (condition)
      (let ((supported-chars (config-supported-dispatch-chars)))
        (if (and (char= char #\#)
                 sub-char
                 (member (string sub-char) supported-chars :test #'string-equal))
            ;; For supported dispatch chars, read the next form and return a placeholder
            (handler-case
                (read input-stream nil :reader-macro-placeholder)
              (error ()
                :reader-macro-placeholder))
            ;; If not in supported list, re-signal the error
            (error condition))))))

(defmethod eclector.reader:interpret-symbol ((client lenient-parse-client)
                                              input-stream
                                              package-indicator
                                              symbol-name
                                              internp)
  ;; For linting purposes, we want to continue parsing even if packages are missing
  ;; So we create dummy packages on demand
  (let ((package (case package-indicator
                   (:current *package*)
                   (:keyword (find-package "KEYWORD"))
                   (otherwise (or (find-package package-indicator)
                                  ;; Create a dummy package if it doesn't exist
                                  (make-package package-indicator :use nil))))))
    (if internp
        (intern symbol-name package)
        (or (find-symbol symbol-name package)
            (intern symbol-name package)))))

(defmethod eclector.parse-result:make-expression-result
    ((client parse-tree-client) (result t) (children t) (source t))
  ;; Create a structure that preserves both the form and position info for all sub-forms
  (if children
      ;; For compound forms, create a structure that maps sub-forms to positions
      (list :form result :source source :children children)
      ;; For atoms, just return form with source
      (list :form result :source source)))

;; Parse context structure to replace global state
(defstruct parse-context
  content
  parse-trees
  line-index
  flat-nodes) ;; list of (form ln col)


(defun read-forms-with-full-positions (content)
  "Read all top-level forms with complete position information for all sub-forms."
  (let ((forms nil)
        (line-index (build-line-index content))
        (client (make-instance 'lenient-parse-client)))
    (when *verbose* (logf "; read-forms-with-full-positions: starting parse~%"))
    (with-input-from-string (stream content)
      (loop
        (handler-case
            (let ((result (eclector.parse-result:read client stream nil :eof)))
              (when (eql result :eof)
                (return))
              (when result
                (push result forms)))
          (error (e)
            (when *verbose*
              (logf "; PARSE ERROR: ~A~%" e))
            (return)))))
    (when *verbose* (logf "; read-forms-with-full-positions: parsed ~D forms~%" (length forms)))
    (make-parse-context :content content
                       :parse-trees (nreverse forms)
                       :line-index line-index
                       :flat-nodes nil)))


(defun read-top-forms-with-pos-ctx (ctx)
  "Return top-level forms as ((form . (ln . col)) ...) from parse context."
  (when *verbose*
    (logf "; read-top-forms-with-pos: processing ~D parse trees~%"
          (length (parse-context-parse-trees ctx))))
  (let ((forms nil))
    (dolist (tree (parse-context-parse-trees ctx))
      (when (and (listp tree) (getf tree :form))
        (let* ((form (getf tree :form))
               (source (getf tree :source))
               (file-pos (if (consp source) (first source) source)))
          (when (and file-pos (numberp file-pos))
            (multiple-value-bind (ln col)
                (index->line/col file-pos (parse-context-line-index ctx))
              (push (list* form ln col) forms))))))
    (when *verbose* (logf "; read-top-forms-with-pos: extracted ~D forms~%" (length forms)))
    (nreverse forms)))


