(in-package :jsown)

;;;; This file contains the helpers which make it easier to edit the parsed json objects

(defun keywords (object)
  "Returns a list of all the keywords contained in the object"
  (mapcar #'car (cdr object)))

(defun keyp (object key)
  "Returns non-nil iff <object> has key <key>."
  (member key (keywords object) :test #'string=))

(defun key-val (object key)
  "Returns the list which represents the key-val pair in the json object"
  (loop for k-v in (cdr object)
     when (string= (car k-v) key)
     do (return-from key-val k-v))
  (error "Key ~A is not available in the given object" key))

(defun val (object key)
  "Returns the value of the given key in object"
  (cdr (key-val object key)))

(defun push-key (object key value)
  "Adds the given key to the object at front"
  (setf (cdr object)
        (cons (cons key value) (cdr object)))
  object)

(defun append-key (object key value)
  "Appends the given key to the object"
  (setf (cdr (last object))
        (list (cons key value)))
  object)

(defun remkey (object key)
  "Removes key from object."
  (setf (rest object)
        (remove-if (lambda (opt) (string= (first opt) key))
                   (rest object)))
  object)

(defun overwrite-val (object key value)
  "Overwrites the given key's value with value.  Errors out if the key didn't exist"
  (setf (cdr (key-val object key)) value)
  object)

(defun val-safe (object key)
  "Returns the value of <key> in <object> if <object> existed, or nil if it did not.
   A second value is returned which indicates whether or not the key was found."
  (handler-case
      (values (val object key) t)
    (error () nil nil)))

(defun jsown-object-p (object)
  "returns non-nil iff we expect <object> to be a jsown object."
  (and (listp object)
       (eq (first object) :obj)))

(define-setf-expander val (place key &environment env)
  "see (setf getf) and val"
  (multiple-value-bind (*temps *vals *store-vars *setter *getter)
      (get-setf-expansion place env)
    (let ((value-v (gensym "value-v"))
          (key-v (gensym "key-v"))
          (result-v (gensym "result-v"))
          (getter-res-v (gensym "getter-res-v")))
      (values (list* key-v *temps)
              (list* key *vals)
              (list  value-v)
              `(let ((,result-v (let ((,getter-res-v ,*getter))
                                  (if (jsown-object-p ,getter-res-v)
                                      ,getter-res-v
                                      (empty-object)))))
                 (handler-case
                     (overwrite-val ,result-v ,key-v ,value-v)
                   (error ()
                     (append-key ,result-v ,key-v ,value-v)))
                 (let ((,(first *store-vars) ,result-v))
                   ,*setter)
                 ,value-v)
              `(val-safe ,*getter ,key-v)))))

(defmacro do-json-keys ((key val) object &body body)
  "Iterates over the json key-value pairs"
  (let ((k-v (gensym)))
    `(loop for ,k-v in (rest ,object)
        for ,key = (car ,k-v)
        for ,val = (cdr ,k-v)
        do (progn ,@body))))

(defun empty-object ()
  "Returns an empty object which can be used to build new objects upon"
  (list :obj))

(defmacro extend-js (obj &body specs)
  "fills in a bunch of jsown values for obj.  each spec should contain a list with the first element being the string which represents the key and the second being the form which evaluates to the value to which the key should be set.

it is heavily related to jsown-object, which fills in an empty object.

eg: (jsown-values (empty-object)
      (\"kind\" \"onyx.Groupbox\")
      (\"components\" (list (jsown-object
                               (\"content\" \"Hello World\")
                               (\"tag\" \"h1\"))
                            (jsown-object (\"tag\" \"p\") (\"content\" \"This is jsown talkin!\")))))"
  (let ((obj-gensym (gensym "obj")))
    `(let ((,obj-gensym ,obj))
       ,@(loop for spec in specs
            collect `(setf (val ,obj-gensym ,(first spec))
                           (progn ,@(rest spec))))
       ,obj-gensym)))

(defmacro new-js (&body specs)
  "creates a new empty object and fills it is per jsown-values"
  `(extend-js (empty-object)
     ,@specs))
