(defpackage defclass-std
  (:use cl)
  (:import-from alexandria
                make-keyword
                flatten
                symbolicate)
  (:import-from anaphora
                aif
                it)
  (:export #:defclass/std
           #:*default-std*
           #:*with-prefix*
           #:class/std
           #:define-print-object/std
           #:print-object/std
           #:printing-unreadably)
  (:documentation "Main (and only) project package."))
(in-package defclass-std)

(defparameter *fusioned-keyword-combinations*
  '(:ai :ar :aw :ia :ir :iw :ra :ri :rw :wa :wi :wr)
  "All possible combinations of :a, :i, :r and :w.")

(defparameter *default-added-keywords* '(:a :i)
  "Default abbreviated keywords added when none is found.")

(defparameter *fusionable-keywords* '(:a :i :w :r)
  "All abbreviated keywords that can be fusioned.")

(defparameter *standalone-keywords* '(:a :i :w :r :static :with :with-prefix :@@))

(defparameter *paired-keywords* '(:std :unbound :doc :type))

(defparameter *default-std* t
  "Special var that changes the behaviour of the DEFCLASS/STD macro. If true, adds a :initform nil by default to every field, when unespecified. If false, adds nothing.")

(defparameter *with-prefix* nil
  "Special var that changes the behaviour of the DEFCLASS/STD macro. If tru, adds the class name as a prefix to every accessor/reader/writer function. If false, without the :with/:with-prefix slot option, adds nothing.")

(defun remove-all (els list)
  "Applies remove recursively. Serves as a version of apeWEOFJIAOPWEIF  that keeps the original sequence in the same order."
  (if els
      (remove-all (cdr els) (remove (car els) list))
      list))

(defun extract-slot-names (line)
  "Finds all slot names in the LINE."
  (if (and line
           (not (keywordp (car line))))
      (cons (car line)
            (extract-slot-names (cdr line)))))

(defun extract-unkown-keywords (line)
  "Finds pairs of unknown-keywords (and optional values) in LINE."
  (if line
      (let ((slot (car line)))
        (cond ((or (not (keywordp slot))
                   (member slot *standalone-keywords*))
               (extract-unkown-keywords (cdr line)))
              ((member slot *paired-keywords*)
               (extract-unkown-keywords (cddr line)))
              ((or (member (second line) (append *standalone-keywords*
                                                 *paired-keywords*))
                   (null (cdr line)))
               (cons (car line)
                     (extract-unkown-keywords (cdr line))))
              (t (append (subseq line 0 2)
                         (extract-unkown-keywords (cddr line))))))))

(defun split-fusioned-keywords (line)
  "Splits the fusioned keyword option, if present."
  (aif (intersection line *fusioned-keyword-combinations*)
       (append (remove-all it line)
               (mapcar #'make-keyword
                       (flatten (mapcar (lambda (fus-kw)
                                          (coerce (string fus-kw)
                                                  'list))
                                        it))))
       (if (intersection line *fusionable-keywords*)
           line
           (append line *default-added-keywords*))))

(defun check-for-repeated-keywords (line)
  "Verifies if keyword options were repeated. Mainly useful for avoiding things like (:A :AI) together, or (:R :W) instead of (:A)."
  (cond ((and (member :w line)
              (member :r line))
         (error "Use :A (accessor) instead of :W (writer) and :R (reader) in: ~s"
                line))
        ((and (member :w line)
              (member :a line))
         (error ":W (writer) and :A (accessor) shouldn't be together in: ~s."
                line))
        ((and (member :r line)
              (member :a line))
         (error ":R (reader) and :A (accessor) shouldn't be together in: ~s."
                line))))

(defun replace-keywords (env line prefix)
  "Receives a list of slots with keywords and returns a list of lists. Each sublist is a single slot, with all the options appended at the end."
  (let ((type (aif (member :type line) (cadr it) t)))
    (mapcar (lambda (slot)
              (concatenate 'list
                           (list slot)
                           (if (member :a line)
                               (list :accessor (symbolicate prefix slot)))
                           (if (member :r line)
                               (list :reader (symbolicate prefix slot)))
                           (if (member :w line)
                               (list :writer (symbolicate prefix slot)))
                           (if (member :i line)
                               (list :initarg (make-keyword slot)))
                           (aif (member :std line)
                                (if (eq (cadr it) :unbound)
                                    nil
                                    (list :initform (cadr it)))
                                (if *default-std*
                                    (if (subtypep 'null type env)
                                        (list :initform nil))))
                           (if (or (member :@@ line)
                                   (member :static line))
                               (list :allocation :class))
                           (aif (member :doc line)
                                (list :documentation (cadr it)))
                           (aif (member :type line)
                                (list :type  (cadr it)))
                           (extract-unkown-keywords line)))
            (extract-slot-names line))))

(defmacro defclass/std (name direct-superclasses direct-slots &rest options
                        &environment env)
  "Shortcut macro to the DEFCLASS macro.

  (defclass/std example ()
    ((slot1 slot2 slot3)))

  expands to:

  (DEFCLASS EXAMPLE ()
    ((SLOT1 :ACCESSOR SLOT1 :INITARG :SLOT1 :INITFORM NIL)
    (SLOT2 :ACCESSOR SLOT2 :INITARG :SLOT2 :INITFORM NIL)
    (SLOT3 :ACCESSOR SLOT3 :INITARG :SLOT3 :INITFORM NIL)))

  Each slot definition accepts options:

    :std 1 => changes the :initform to 1. It can be :unbound.
    :with or :with-prefix => creates accessors with the class name as prefix.

  See the README for more.

  See also `class/std' and `print-object/std'.
  "
  `(defclass ,name ,direct-superclasses
     ,(process-slots env direct-slots name)
     ,@options))

(defun process-slots (env direct-slots classname)
  "Returns the expanded list of DIRECT-SLOTS."
  (let ((processed (mapcar
                    (lambda (line)
                      (let ((prefix (if (or (member :with-prefix line)
                                            (member :with line)
                                            *with-prefix*)
                                        (concatenate 'string (string classname) "-")
                                        ""))
                            (split-kws-line (split-fusioned-keywords line)))
                        (check-for-repeated-keywords split-kws-line)
                        (replace-keywords env split-kws-line prefix)))
                    direct-slots)))
    (reduce #'append processed)))

(defmacro class/std (name &body defaulted-slots)
  "The most concise shortcut macro to DEFCLASS.

  (class/std example slot1 slot2 slot3)

  expands to:

  (DEFCLASS/STD EXAMPLE ()
    ((SLOT1 SLOT2 SLOT3)))

  which expands to:

  (DEFCLASS EXAMPLE ()
    ((SLOT1 :ACCESSOR SLOT1 :INITARG :SLOT1 :INITFORM NIL)
    (SLOT2 :ACCESSOR SLOT2 :INITARG :SLOT2 :INITFORM NIL)
    (SLOT3 :ACCESSOR SLOT3 :INITARG :SLOT3 :INITFORM NIL)))
"
  `(defclass/std ,name ()
     ((,@defaulted-slots))))

(defmacro printing-unreadably (fields-list class-std-form
                               &key (type t) (identity t))
  "Automatically generates the unreadable printing boiler plate to print classes and its fields (from FIELDS-LIST)."
  (let ((g!stream (gensym "STREAM"))
        (name (cadr class-std-form)))
    `(progn ,class-std-form
            (defmethod print-object ((,name ,name) ,g!stream)
              (print-unreadable-object (,name ,g!stream
                                        :type ,type
                                        :identity ,identity)
                (format ,g!stream
                        ,(format nil "~{~a: ~~s~^,~^ ~}" fields-list)
                        ,@(mapcar (lambda (a1)
                                    `(,a1 ,name))
                                  fields-list)))))))

(defun collect-object-slots (obj)
  (loop for slot in (closer-mop:class-slots (class-of obj))
                  for name = (closer-mop:slot-definition-name slot)
                  for val = (if (slot-boundp obj name)
                                (slot-value obj name)
                                "UNBOUND")
                  collect (list name val)))

(defmacro define-print-object/std (class)
  "Define a print-object method for objects of class CLASS.

  Print all slots with their values. Prints \"UNBOUND\", as a string, when slots are unbound.

  Usage:

  (defclass/std test () ((a b)))

  (print-object/std test)

  The macro expands to:

  (DEFMETHOD PRINT-OBJECT ((OBJ TEST) STREAM)
    (PRINT-UNREADABLE-OBJECT (OBJ STREAM :TYPE T :IDENTITY T)
        (FORMAT STREAM \"~{~a~^ ~}\" (COLLECT-OBJECT-SLOTS OBJ))))
"
  `(defmethod print-object ((obj ,class) stream)
     (print-unreadable-object (obj stream :type t :identity t)
       (format stream "~{~a~^ ~}"
               (collect-object-slots obj)))))

#|
(defclass/std foo2 ()
  ((bar baz)))

(define-print-object/std foo2)

(defparameter foo2 (make-instance 'foo2))
|#

(defmacro print-object/std (class)
  "Old name for DEFINE-PRINT-OBJECT/STD."
  `(define-print-object/std ,class))
