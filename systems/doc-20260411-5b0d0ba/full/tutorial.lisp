(uiop:define-package #:foo-random
  (:nicknames #:40ants-doc-full/tutorial)
  (:documentation "This package provides various utilities for
                   random. See @FOO-RANDOM-MANUAL.")
  (:use #:common-lisp
        #:40ants-doc)
  (:import-from #:40ants-doc/ignored-words
                #:ignore-words-in-package)
  (:export #:foo-random-state
           #:state
           #:*foo-state*
           #:gaussian-random
           #:uniform-random))

(in-package foo-random)

(defsection @foo-random-manual (:title "Foo Random manual"
                                :ignore-words ("FOO"))
  "Here you describe what's common to all the referenced (and
   exported) functions that follow. They work with *FOO-STATE*,
   and have a :RANDOM-STATE keyword arg. Also explain when to
   choose which."
  (foo-random-state class)
  (state (reader foo-random-state))
  
  "Hey we can also print states!"
  
  (print-object (method () (foo-random-state t)))
  (*foo-state* variable)
  (gaussian-random function)
  (uniform-random function)
  ;; this is a subsection
  (@foo-random-examples section))

(defclass foo-random-state ()
  ((state :reader state
          :documentation "Returns random foo's state.")))

(defmethod print-object ((object foo-random-state) stream)
  (print-unreadable-object (object stream :type t)))

(defvar *foo-state* (make-instance 'foo-random-state)
  "Much like *RANDOM-STATE* but uses the FOO algorithm.")

(defun uniform-random (limit &key (random-state *foo-state*))
  "Return a random number from the between 0 and LIMIT (exclusive)
   uniform distribution."
  (declare (ignore limit random-state))
  nil)

(defun gaussian-random (stddev &key (random-state *foo-state*))
  "Return not a random number from a zero mean normal distribution with
   STDDEV."
  (declare (ignore stddev random-state))
  nil)

(defsection @foo-random-examples (:title "Examples")
  "Let's see the transcript of a real session of someone working
   with FOO:

   ```cl-transcript
   (values (princ :hello) (list 1 2))
   .. HELLO
   => :HELLO
   => (1 2)

   (make-instance 'foo-random-state)
   ==> #<FOO-RANDOM-STATE >
   ```")

(defvar +end+)


(ignore-words-in-package '*foo-state*
                         'foo-random-state
                         'state
                         'gaussian-random
                         'uniform-random)
