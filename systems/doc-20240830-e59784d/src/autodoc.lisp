(uiop:define-package #:40ants-doc/autodoc
  (:use #:cl)
  (:import-from #:40ants-doc/core
                #:section
                #:defsection)
  (:import-from #:str)
  (:import-from #:alexandria
                #:symbolicate)
  (:import-from #:40ants-doc/locatives
                #:system)
  (:import-from #:closer-mop
                #:slot-definition-writers
                #:slot-definition-readers
                #:class-direct-slots
                #:class-slots)
  (:import-from #:40ants-doc/autodoc/sections
                #:registered-subsections
                #:register-subsection
                #:with-subsection-collector)
  (:export #:defautodoc)
  (:documentation "This module is not included into asd file because it requires additional dependencies."))
(in-package #:40ants-doc/autodoc)


(defun class-readers-and-accessors (class-name)
  (let* ((class (find-class class-name))
         (slots (class-direct-slots class)))
    (loop for slot in slots
          for readers = (slot-definition-readers slot)
          for writers = (slot-definition-writers slot)
          append readers into all-readers
          append (mapcar #'second writers) into all-writers
          finally (return (values (sort all-readers
                                        #'string<)
                                  (sort all-writers
                                        #'string<))))))


(defun system-packages (system-name)
  (loop for package in (list-all-packages)
        for name = (package-name package)
        when (or (string-equal name system-name)
                 (str:starts-with? (concatenate 'string (string-downcase system-name) "/")
                                   (string-downcase name)))
          collect package into results
        finally (return (sort results
                              #'string<
                              :key #'package-name))))


(defun package-accessors-and-writers (package)
  (loop with result = nil
        for symbol being the external-symbols of package
        when (find-class symbol nil)
          do (multiple-value-bind (readers accessors)
                 (class-readers-and-accessors symbol)
               (setf result
                     (nunion result
                             (nunion readers accessors))))
        finally (return result)))


;; (defun make-class-entry-with-accessors-and-readers (class-name)
;;   (multiple-value-bind (readers accessors)
;;       (class-readers-and-accessors class-name)
;;     (nconc
;;      (list (format nil "# Class ~S"
;;                    class-name))
;;      (list (list class-name 'class))
;;      (when readers
;;        (list "## Readers"))
;;      (loop for reader in readers
;;            collect `(,reader (reader ,class-name)))
;;      (when accessors
;;        (list "## Accessors"))
;;      (loop for accessor in accessors
;;            collect `(,accessor (accessor ,class-name))))))


(defun make-class-entry (class-name package-name)
  (check-type class-name symbol)
  (check-type package-name string)
  
  (multiple-value-bind (readers accessors)
      (class-readers-and-accessors class-name)

    (let* ((title (symbol-name class-name))
           (section-name (symbolicate
                          "@"
                          (package-name
                           (symbol-package class-name))
                          "$"
                          class-name
                          "?CLASS"))
           (entries
             (nconc
              (when readers
                (list "**Readers**"))
              (loop for reader in readers
                    collect `(,reader (reader ,class-name)))
              (when accessors
                (list "**Accessors**"))
              (loop for accessor in accessors
                    collect `(,accessor (accessor ,class-name)))))
           (section-definition
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                (defsection ,section-name (:title ,title
                                           :package ,package-name)
                  (,class-name class)
                  ,@entries))))
      (register-subsection section-definition)
      `(,section-name section))))


(defun make-package-section (section-name package)
  (declare (optimize (debug 3)))
  (let* ((package-name (package-name package))
         (title package-name)
         (accessors-and-readers (package-accessors-and-writers package))
         (entries (loop for symbol being the external-symbols of package
                        ;; Usual functions
                        when (and (fboundp symbol)
                                  (not (macro-function symbol))
                                  (not (typep (symbol-function symbol) 'generic-function)))
                          collect (list symbol 'function) into functions

                        ;; Generic functions
                        when (and (fboundp symbol)
                                  (typep (symbol-function symbol) 'generic-function)
                                  (not (member symbol accessors-and-readers
                                               :test 'eql)))
                          collect (list symbol 'generic-function) into generics

                        ;; Macroses
                        when (and (fboundp symbol)
                                  (macro-function symbol))
                          collect (list symbol 'macro) into macros

                        ;; Classes
                        when (find-class symbol nil)
                          collect (make-class-entry symbol package-name) into classes

                        ;; Variables
                        when (documentation symbol 'variable)
                          collect (list symbol 'variable) into variables

                        finally (return
                                  (uiop:while-collecting (collect)
                                    (flet ((add-subsection (entries title)
                                             (let* ((section-name (symbolicate "@"
                                                                               package-name
                                                                               "?"
                                                                               title
                                                                               "-SECTION")))
                                               (when entries
                                                 (register-subsection
                                                  `(defsection ,section-name (:title ,title
                                                                              :package ,package-name)
                                                     ,@(sort (copy-list entries)
                                                             #'string<
                                                             :key #'first)))
                                                 (collect `(,section-name section))))))
                                      (add-subsection classes "Classes")
                                      (add-subsection generics "Generics")
                                      (add-subsection functions "Functions")
                                      (add-subsection macros "Macros")
                                      (add-subsection variables "Variables")))))))
    (when entries
      `(defsection ,section-name (:title ,title
                                  :package ,package-name)
         (,(symbolicate package-name) package)
         ,@entries))))


(defun make-entries (system &key (show-system-description-p nil))
  (with-subsection-collector ()
    (loop for package in (system-packages system)
          for package-name = (package-name package)
          for section-name = (symbolicate "@" (string-upcase package-name) "?PACKAGE")
          for package-section = (make-package-section section-name package)
          when package-section
            collect (list section-name 'section) into entries
            and
              do (register-subsection package-section)
          finally (return (values (registered-subsections)
                                  (append
                                   (when show-system-description-p
                                     (list (list system 'system)))
                                   entries))))))


(defmacro defautodoc (name (&key system
                              (title "API")
                              (show-system-description-p nil)
                              (readtable-symbol '*readtable*)
                              (section-class '40ants-doc:section)
                              (external-docs nil)
                              (external-links nil)
                              (ignore-words nil)))

  "Macro DEFAUTODOC collects all packages of the ASDF system and analyzes all external symbols.
   In resulting documentation symbols are grouped by packages and types.

   Here is how you can define a section using DEFAUTODOC:

   ```
   (40ants/defautodoc @api (:system :cl-telegram-bot))
   ```

   This form will generate complete API reference for the CL-TELEGRAM-BOT system.

   The most wonderful it that you can integrate this `@api` section with handwritten
   documentation like this:

   ```
   (defsection @index (:title \"cl-telegram-bot - Telegram Bot API\")
     (@installation section)
     (@quickstart section)
     (@api section))

   When SHOW-SYSTEM-DESCRIPTION-P argument is not NIL, section will be started from the
   description of the given ASDF system.
   ```"

  (unless system
    (error ":SYSTEM argument is required for DEFAUTODOC macro."))

  (multiple-value-bind (subsections entries)
      (make-entries system :show-system-description-p show-system-description-p)
    `(progn
       (defsection ,name (:title ,title
                          :readtable-symbol ,readtable-symbol
                          :section-class ,section-class
                          :external-docs ,external-docs
                          :external-links ,external-links
                          :ignore-words ,ignore-words)
         ,@entries)

       ,@subsections)))
