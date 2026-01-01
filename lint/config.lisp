;;;; config.lisp
;;;;
;;;; Project-local configuration system for cl-lint
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025, 2026 Anthony Green

(in-package #:ocicl.lint)

(defstruct cl-lint-config
  "Configuration structure for cl-lint settings."
  (max-line-length 120)
  (suppressed-rules nil)
  (supported-dispatch-chars nil)
  (suggest-libraries nil))  ; List of libraries to suggest (e.g., "alexandria" "uiop")

(defparameter *default-config* (make-cl-lint-config)
  "Default configuration when no config file is found.")

(defparameter *current-config* nil
  "Currently active configuration (loaded from file or defaults).")

(defun find-config-file (start-path)
  "Find .ocicl-lint.conf file by walking up the directory tree from START-PATH."
  (let ((current-dir (uiop:pathname-directory-pathname
                      (uiop:ensure-absolute-pathname start-path *default-pathname-defaults*))))
    (loop with visited = nil
          for dir = current-dir then (uiop:pathname-parent-directory-pathname dir)
          for dir-string = (handler-case (namestring dir)
                             (error () (return nil)))  ; Give up if namestring fails
          when (member dir-string visited :test #'string=)
            do (return nil)  ; Detected loop, give up
          do (push dir-string visited)
          do (let ((config-file (uiop:merge-pathnames* ".ocicl-lint.conf" dir)))
               (when (uiop:file-exists-p config-file)
                 (return config-file)))
          when (or (null dir-string)
                   (string= dir-string "/")           ; Unix root
                   (and (> (length dir-string) 1)     ; Windows root like "C:\"
                        (char= (char dir-string 1) #\:)
                        (= (length dir-string) 3)))
            do (return nil))))

(defun parse-config-line (line)
  "Parse a single configuration line. Returns (key . value) or nil."
  (let ((line (string-trim '(#\Space #\Tab) line)))
    (when (and (> (length line) 0)
               (not (char= (char line 0) #\#))  ; Skip comments
               (not (char= (char line 0) #\;))) ; Skip comments
      (when-let ((eq-pos (position #\= line)))
        (let ((key (string-trim '(#\Space #\Tab) (subseq line 0 eq-pos)))
              (value (string-trim '(#\Space #\Tab) (subseq line (1+ eq-pos)))))
          (cons key value))))))

(defun parse-rule-list (value-string)
  "Parse a comma-separated list of rule names."
  (let ((trimmed (string-trim '(#\Space #\Tab) value-string)))
    (if (emptyp trimmed)
        nil
        (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                (uiop:split-string trimmed :separator '(#\,))))))

(defun load-config-file (config-path)
  "Load configuration from CONFIG-PATH and return a cl-lint-config structure."
  (let ((config (make-cl-lint-config)))
    (when (and config-path (uiop:file-exists-p config-path))
      (with-open-file (in config-path :direction :input :external-format :utf-8)
        (loop for line = (read-line in nil nil)
              while line
              do (when-let ((parsed (parse-config-line line)))
                   (let ((key (first parsed))
                         (value (rest parsed)))
                     (cond
                       ((string-equal key "max-line-length")
                        (let ((num (ignore-errors (parse-integer value))))
                          (when (and num (> num 0))
                            (setf (cl-lint-config-max-line-length config) num))))
                       ((string-equal key "suppress-rules")
                        (setf (cl-lint-config-suppressed-rules config)
                              (parse-rule-list value)))
                       ((string-equal key "supported-dispatch-chars")
                        (setf (cl-lint-config-supported-dispatch-chars config)
                              (parse-rule-list value)))
                       ((string-equal key "suggest-libraries")
                        (setf (cl-lint-config-suggest-libraries config)
                              (parse-rule-list value)))
                       (t
                        (when *verbose*
                          (logf "; config: unknown setting ~A~%" key)))))))))
    config))

(defun load-project-config (path)
  "Load project configuration for the given PATH."
  (let* ((config-file (find-config-file path))
         (config (if-let ((cfg-file config-file))
                     (let ((cfg (load-config-file cfg-file)))
                       (when *verbose*
                         (logf "; config: loading from ~A~%" cfg-file)
                         (logf "; config: suppressed ~D rules~%" (length (cl-lint-config-suppressed-rules cfg)))
                         (dolist (rule (cl-lint-config-suppressed-rules cfg))
                           (logf ";   - ~A~%" rule)))
                       cfg)
                     (progn
                       (when *verbose*
                         (logf "; config: using defaults (no .ocicl-lint.conf found)~%"))
                       *default-config*))))
    (setf *current-config* config)
    config))

(defun get-config ()
  "Get the current configuration, loading defaults if none set."
  (or *current-config* *default-config*))

(defun config-max-line-length ()
  "Get the configured maximum line length."
  (cl-lint-config-max-line-length (get-config)))

(defun config-suppressed-rules ()
  "Get the list of globally suppressed rules."
  (cl-lint-config-suppressed-rules (get-config)))


(defun config-supported-dispatch-chars ()
  "Get the list of supported dispatch sub-characters."
  (cl-lint-config-supported-dispatch-chars (get-config)))

(defun config-suggest-libraries ()
  "Get the list of libraries to suggest."
  (cl-lint-config-suggest-libraries (get-config)))

(defun library-suggestions-enabled-p (library-name)
  "Check if suggestions for LIBRARY-NAME are enabled in the configuration."
  (member library-name (config-suggest-libraries) :test #'string-equal))

(defun rule-suppressed-p (rule-name)
  "Check if a rule is globally suppressed in the current configuration."
  (member rule-name (config-suppressed-rules) :test #'string-equal))
