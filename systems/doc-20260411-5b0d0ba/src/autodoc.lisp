(uiop:define-package #:40ants-doc/autodoc
  (:use #:cl)
  (:import-from #:40ants-doc/core
                #:section
                #:defsection)
  (:import-from #:str)
  (:import-from #:swank-backend)
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
  (:import-from #:40ants-doc/ignored-words
                #:ignore-in-package)
  (:export #:defautodoc)
  (:documentation "This module is not included into asd file because it requires additional dependencies."))
(in-package #:40ants-doc/autodoc)


(defun class-readers-and-accessors (class-name &key (ignore-symbol-p 'starts-with-percent-p))
  (let* ((class (find-class class-name))
         (slots (class-direct-slots class)))
    (loop for slot in slots
          for readers = (remove-if ignore-symbol-p
                                   (slot-definition-readers slot))
          for writers = (remove-if ignore-symbol-p
                                   (mapcar #'second
                                           (slot-definition-writers slot)))
          append readers into all-readers
          append writers into all-writers
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


(defun package-accessors-and-writers (package &key (ignore-symbol-p 'starts-with-percent-p))
  (loop with result = nil
        for symbol being the external-symbols of package
        when (find-class symbol nil)
          do (multiple-value-bind (readers accessors)
                 (class-readers-and-accessors symbol
                                              :ignore-symbol-p ignore-symbol-p)
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


(defun make-class-entry (class-name package-name &key (ignore-symbol-p 'starts-with-percent-p))
  (check-type class-name symbol)
  (check-type package-name string)
  
  (multiple-value-bind (readers accessors)
      (class-readers-and-accessors class-name
                                   :ignore-symbol-p ignore-symbol-p)

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


(defun make-package-section (section-name package &key (ignore-symbol-p 'starts-with-percent-p))
  (declare (optimize (debug 3)))
  (flet ((ignore-symbol-p-wrapper (symbol)
           (when (and ignore-symbol-p
                      (funcall ignore-symbol-p symbol))
             (ignore-in-package symbol :package package)
             (values t))))
    
    (let* ((package-name (package-name package))
           (title package-name)
           (accessors-and-readers (package-accessors-and-writers package
                                                                 :ignore-symbol-p #'ignore-symbol-p-wrapper))
           (entries (loop for symbol being the external-symbols of package
                          for should-be-documented = (not (ignore-symbol-p-wrapper symbol))
                          ;; Usual functions
                          when (and (fboundp symbol)
                                    should-be-documented
                                    (not (macro-function symbol))
                                    (not (typep (symbol-function symbol) 'generic-function)))
                            collect (list symbol 'function) into functions

                          ;; Generic functions
                          when (and (fboundp symbol)
                                    should-be-documented
                                    (typep (symbol-function symbol) 'generic-function)
                                    (not (member symbol accessors-and-readers
                                                 :test 'eql)))
                            collect (list symbol 'generic-function) into generics

                          ;; Macroses
                          when (and (fboundp symbol)
                                    should-be-documented
                                    (macro-function symbol))
                            collect (list symbol 'macro) into macros

                          ;; Classes
                          when (and (find-class symbol nil)
                                    should-be-documented)
                            collect (make-class-entry symbol package-name
                                                      :ignore-symbol-p #'ignore-symbol-p-wrapper)
                              into classes

                          ;; Variables
                          when (and (documentation symbol 'variable)
                                    should-be-documented)
                            collect (list symbol 'variable) into variables

                          ;; Types and not classes
                          when (and (not (find-class symbol nil))
                                    should-be-documented
                                    (or (documentation symbol 'type)
                                        (not (eq (swank-backend:type-specifier-arglist symbol)
                                                 :not-available))))
                            collect (list symbol 'type) into types
                        
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
                                        (add-subsection types "Types")
                                        (add-subsection variables "Variables")))))))
      (when entries
        `(defsection ,section-name (:title ,title
                                    :package ,package-name)
           (,(symbolicate package-name) package)
           ,@entries)))))


(defun make-entries (system &key
                            (show-system-description-p nil)
                            (ignore-packages nil)
                            (ignore-symbol-p 'starts-with-percent-p))
  (with-subsection-collector ()
    (loop for package in (system-packages system)
          for package-name = (package-name package)
          for collect-this-package = (not (member package-name
                                                  ignore-packages
                                                  :test #'string-equal))
          for section-name = (when collect-this-package
                               (symbolicate "@" (string-upcase package-name) "?PACKAGE"))
          for package-section = (when section-name
                                  (make-package-section section-name package
                                                        :ignore-symbol-p ignore-symbol-p))
          when package-section
            collect (list section-name 'section) into entries
            and
              do (register-subsection package-section)
          finally (return (values (registered-subsections)
                                  (append
                                   (when show-system-description-p
                                     (list (list system 'system)))
                                   entries))))))


(defun starts-with-percent-p (symbol)
  (str:starts-with-p "%"
                     (symbol-name symbol)))


(defmacro defautodoc (name (&key system
                              (title "API")
                              (show-system-description-p nil)
                              (readtable-symbol '*readtable*)
                              (section-class '40ants-doc:section)
                              (external-docs nil)
                              (external-links nil)
                              (ignore-words nil)
                              (ignore-packages nil)
                              (ignore-symbol-p 'starts-with-percent-p)))

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

   Argument IGNORE-PACKAGES can be used to exclude some packages from autogenerated docs.
   This feature was added to not generate docs for symbols created automatically
   by Telegram API spec in cl-telegram-bot, but might be useful for some other projects,
   especially for ones which are using package-inferred ASDF system style. Warnings about
   mentioning symbols from ignored packages are suppressed. Comparison of package names
   is case-sensitive.
   ```"

  (unless system
    (error ":SYSTEM argument is required for DEFAUTODOC macro."))

  (multiple-value-bind (subsections entries)
      (make-entries system
                    :show-system-description-p show-system-description-p
                    :ignore-packages ignore-packages
                    :ignore-symbol-p ignore-symbol-p)
    `(progn
       (defsection ,name (:title ,title
                          :readtable-symbol ,readtable-symbol
                          :section-class ,section-class
                          :external-docs ,external-docs
                          :external-links ,external-links
                          :ignore-words ,ignore-words
                          :ignore-packages ,ignore-packages)
         ,@entries)

       ,@subsections)))
