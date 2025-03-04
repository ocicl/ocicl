# defclass-std - Standard class writing macro

<!-- [![Quicklisp](http://quickdocs.org/badge/defclass-std.svg)](http://quickdocs.org/defclass-std/) -->
<!-- [![Build Status](https://travis-ci.org/EuAndreh/defclass-std.svg?branch=master)](https://travis-ci.org/EuAndreh/defclass-std) -->
<!-- [![Circle CI](https://circleci.com/gh/EuAndreh/defclass-std.svg?style=svg)](https://circleci.com/gh/EuAndreh/defclass-std) -->
<!-- [![Coverage Status](https://coveralls.io/repos/EuAndreh/defclass-std/badge.svg?branch=master)](https://coveralls.io/r/EuAndreh/defclass-std?branch=master) -->

Most times, when sketching out a new class, I often commit lots of typos and forget to add an `:initform`.

Also, the throw away class designed in the beginning may thrive and stay the same. If only there was a way to overcome these problems... There is!

This simple macro atempts to give a very DRY and succint interface to the common `DEFCLASS` form. The goal is to offer most of the capabilities of a normal `DEFCLASS`, only in a more compact way.

Everything compiles down to `defclass`.

But with every `defclass` usually comes a `print-object` method. So we
provide a simple `define-print-object/std` macro that defines a
`print-object` method and prints all the class slots.

**About this fork**:

- the [original project](https://github.com/EuAndreh/defclass-std) by EuAndreh is archived :/
- added `define-print-object/std` (<2024-09-27>)
  - (`print-object/std` is the old name, deprecated)
- added docstrings, organized the README


## Usage

We provide `defclass/std`, which is close to `defclass`, and
`class/std`, which is even shorter and close to `defstruct`.

First, install the library and import the `defclass/std` symbol:

```lisp
* (ql:quickload :defclass-std)  ;; warn: this fork is not in Quicklisp
; => (:DEFCLASS-STD)
* (import 'defclass-std:defclass/std)
; => T
```

### defclass/std

A simple class defined with `DEFCLASS/STD` looks like this:
```lisp
(defclass/std example ()
  ((slot1 slot2 slot3)))

; which expands to:

(DEFCLASS EXAMPLE ()
  ((SLOT1 :ACCESSOR SLOT1 :INITARG :SLOT1 :INITFORM NIL)
   (SLOT2 :ACCESSOR SLOT2 :INITARG :SLOT2 :INITFORM NIL)
   (SLOT3 :ACCESSOR SLOT3 :INITARG :SLOT3 :INITFORM NIL)))
```
   As you can see, by default, the macro adds three options:
   1. `:accessor` + the name of the slot
   2. `:initarg` + the name of the slot
   3. `:initform nil`

So, with this short definition, you have `:initargs` defined for you,
all `:initform`s are set to NIL instead of being unbound, and you now
have generic methods (accessors) to get and set the slots:

~~~lisp
(defparameter *example1* (make-instance 'example :slot1 "one"))
;; *EXAMPLE1*

(slot1 *)
;; => "one"

(slot2 **)
;; => NIL (instead of unbound slot error)
~~~

### defclass/std vs defclass syntax

Don't rush, there's is a little syntax difference, look:

~~~lisp
(defclass example ()
  (slot1 slot2 slot3))
~~~

VS

~~~lisp
(defclass/std example ()
  ((slot1 slot2 slot3)))
~~~

There is an extra level of parentheses, as if you were already giving slot options as in:

~~~lisp
(defclass example ()
  ((slot1 :initarg :slot1)
   (slot2 :initarg slot2)))
~~~

### defclass/std options

To declare the **type** of a slot or to add **documentation** to a slot, use `:type` and `:doc`, respectively.

~~~lisp
(defclass/std example-doc ()
  ((slot1 :doc "doc1")
   (slot2 :doc "doc2")))
~~~

   If you want to **change the `:initform` value**, you can use the `:std` option:
```lisp
(defclass std-test ()
  ((slot :std 1)))

; expands to:

(DEFCLASS STD-TEST ()
  ((SLOT :ACCESSOR SLOT :INITARG :SLOT :INITFORM 1)))
```

If you want to **omit the `:initform` option,** you have two ways:

   1. Use `:std :unbound` explicitly
   2. Change the value of `*default-std*`. By default it is set to `T`, so, when the `:std` option is omitted, `:initform` is set to nil. When `*default-std*` is set to nil, `:initform` is omitted when `:std` is omitted.
```lisp
(defclass/std omit-std ()
  ((slot :std :unbound)))

; which is (semantically) equivalent to:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *default-std* nil))
(defclass/std omit-std ()
  ((slot)))

; which (both) expands to:

(DEFCLASS OMIT-STD ()
  ((SLOT :ACCESSOR SLOT :INITARG :SLOT)))
```

**Enable or disable accessors, readers, writers and initargs**.

   `:a`, `:i`, `:r` and `:w` are connected: when all of them are omitted, `:a` and `:i` are inserted by default.

   `:a` stands for `:accessor`, `:i` stands for `:initarg`, `:r` stands for `:reader` and `:w` stands for `:writer`.

   If any of those is present, the default (`:a` and `:i`) is omitted.
```lisp
(defclass/std airw ()
  ((slot1 slot2)
   (slot3 slot4 :r)
   (slot5 :w)
   (slot6 :a)
   (slot7 :ri)))

; which expands to:

(DEFCLASS AIRW ()
  ((SLOT1 :ACCESSOR SLOT1 :INITARG :SLOT1 :INITFORM NIL)
   (SLOT2 :ACCESSOR SLOT2 :INITARG :SLOT2 :INITFORM NIL)
   (SLOT3 :READER SLOT3 :INITFORM NIL)
   (SLOT4 :READER SLOT4 :INITFORM NIL)
   (SLOT5 :WRITER SLOT5 :INITFORM NIL)
   (SLOT6 :ACCESSOR SLOT6 :INITFORM NIL)
   (SLOT7 :READER SLOT7 :INITARG :SLOT7 :INITFORM NIL)))
```
   Note that slot7 has an `:ri` option. That's just `:r` and `:i` together.

   If you want to use `:r` and `:w` together, use `:a` instead, or you'll get an error. The same stands for `:a` + `:r` and `:a` + `:w`.

   You can choose to **add the class name as a prefix** for the acessor/reader/writer function. Just put `:with` or `:with-prefix` option.

```lisp
(defclass/std example ()
  ((slot1 :with)
   (slot2)))

; which expands to:

(DEFCLASS EXAMPLE ()
  ((SLOT1 :ACCESSOR EXAMPLE-SLOT1 :INITARG :SLOT1 :INITFORM NIL)
   (SLOT2 :ACCESSOR SLOT2 :INITARG :SLOT2 :INITFORM NIL)))
```

   To make a slot **static (class-allocated)**, use `:@@` or `:static`.

   You can also **add the prefix by default** by changing the value of the `*with-prefix*` special variable (defaults to `nil`):
```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *with-prefix* t))
(defclass/std pre ()
  ((fix)))

; which expands to:

(DEFCLASS PRE ()
  ((FIX :ACCESSOR PRE-FIX :INITARG :FIX)))
```

   Unknown keywords are left intact:
```lisp
(defclass/std unknown ()
  ((slot :unknown :keywords)))

; which expands to:

(DEFCLASS UNKNOWN ()
  ((SLOT :ACCESSOR SLOT :INITARG :SLOT :INITFORM NIL :KEYWORDS :UNKNOWN)))


; Or, even using custom accessors:

(defclass/std unknown ()
  ((slot :unknown :wi :keywords)))

; which expands to:

(DEFCLASS UNKNOWN ()
  ((SLOT :WRITER SLOT :INITARG :SLOT :INITFORM NIL :KEYWORDS :UNKNOWN)))
```

## class/std is even shorter

Usage:

~~~lisp
(class/std classname slot1 slot2 … slotn)
~~~

is equivalent to

~~~lisp
(defclass/std classname ()
  ((slot1 slot2 … slotn)))
~~~

is equivalent to

~~~lisp
(defclass classname ()
  ((slot1 :accessor slot1 :initarg :slot1 :initform nil)
   (slot2 :accessor slot2 :initarg :slot2 :initform nil)
   …
   (slotn :accessor slotn :initarg :slotn :initform nil)))
~~~

## class/std vs defstruct

structures are defined with:

~~~lisp
(defstruct structexample slot1 slot2 slot3)
~~~

they are created with `make-structexample` and accessor functions are
named with the struct name prefix, as `structexample-slot1`.

`class/std` has the same creation syntax:

~~~lisp
(class/std classname slot1 slot2 slot3)
~~~

and doesn't follow the naming of structs by default.


## define-print-object/std

Given a class

~~~lisp
(defclass/std example ()
  ((slot1 slot2 slot3)))
~~~

use:

~~~lisp
(define-print-object/std example)
~~~

which expands to

~~~lisp
(defmethod print-object ((obj example) stream)
    (print-unreadable-object (obj stream :type t :identity t)
        (format stream \"~{~a~^ ~}\" (collect-object-slots obj))))
~~~

Now `example` objects show all their slots' values:

~~~lisp
(make-instance 'example)
;; #<EXAMPLE (SLOT1 NIL) (SLOT2 NIL) (SLOT3 NIL) {100ADFFF73}>
~~~

You can use `define-print-object/std` independently of `defclass/std`.

Unbound slots show "UNBOUND" (as a string).

See also `printing-unreadably` to select which slots to print:

~~~lisp
(printing-unreadably (field2 field3) (class/std myclass field1 field2 field3))
~~~

Now what if you want to **remove the print-object method**? You could
write a basic one instead:

~~~lisp
(defmethod print-object ((obj example) stream)
    (print-unreadable-object (obj stream :type t :identity t)
        ;; (format stream \"~a\" (slot1 obj))
        ))
~~~

=> `#<EXAMPLE {1005FC8073}>`.


## Examples

```lisp
(defclass/std computer (gadget)
  ((screen mouse keyboard :a :type string :with-prefix)
   (bluetooth touchpad :wi)
   (speaker microphone :r)
   (place :@@ :with :doc "Where it is" :r)
   (owner :static :std "Me" :w)))

  ; expands to:

(DEFCLASS COMPUTER (GADGET)
  ((SCREEN :ACCESSOR COMPUTER-SCREEN :INITFORM NIL :TYPE STRING)
   (MOUSE :ACCESSOR COMPUTER-MOUSE :INITFORM NIL :TYPE STRING)
   (KEYBOARD :ACCESSOR COMPUTER-KEYBOARD :INITFORM NIL :TYPE STRING)
   (BLUETOOTH :WRITER BLUETOOTH :INITARG :BLUETOOTH :INITFORM NIL)
   (TOUCHPAD :WRITER TOUCHPAD :INITARG :TOUCHPAD :INITFORM NIL)
   (SPEAKER :READER SPEAKER :INITFORM NIL)
   (MICROPHONE :READER MICROPHONE :INITFORM NIL)
   (PLACE :READER COMPUTER-PLACE :INITFORM NIL :ALLOCATION :CLASS
          :DOCUMENTATION "Where it is")
   (OWNER :WRITER OWNER :INITFORM "Me" :ALLOCATION :CLASS)))
```

   Real life examples:

   From [cl-inflector](https://github.com/AccelerationNet/cl-inflector/blob/master/langs.lisp][cl-inflector):
```lisp
(defclass language ()
  ((name :accessor name :initarg :name :initform nil)
   (plurals :accessor plurals :initarg :plurals :initform nil)
   (singulars :accessor singulars :initarg :singulars :initform nil)
   (uncountables :accessor uncountables :initarg :uncountables :initform nil)
   (irregulars :accessor irregulars :initarg :irregulars :initform nil)))

; could be written:

(defclass/std language ()
  ((name plurals singulars uncountables irregulars)))

; or, using CLASS/STD:

(class/std language name plurals singulars uncountables irregulars)
```
   From [clack](https://github.com/fukamachi/clack/blob/9804d0b57350032ebdcf8539bae376b5528ac1f6/src/core/handler.lisp):
```lisp
(defclass <handler> ()
     ((server-name :type keyword
                   :initarg :server-name
                   :accessor server-name)
      (acceptor :initarg :acceptor
                :accessor acceptor)))

; could be written (with *default-std* set to nil)
(defclass/std language ()
  ((server-name :type keyword)
   (acceptor)))
```
   From [RESTAS](https://github.com/archimag/restas/blob/3e37f868141c785d2468fab342d57cca2e2a40dd/src/route.lisp):
```lisp
(defclass route (routes:route)
  ((symbol :initarg :symbol :reader route-symbol)
   (module :initarg :module :initform nil :reader route-module)
   (required-method :initarg :required-method :initform nil
                    :reader route-required-method)
   (arbitrary-requirement :initarg :arbitrary-requirement :initform nil
                          :reader route-arbitrary-requirement)
   (render-method :initarg :render-method :initform #'identity)
   (headers :initarg :headers :initform nil :reader route-headers)
   (variables :initarg :variables :initform nil)
   (additional-variables :initarg :additional-variables :initform nil)))

; could be written
(defclass/std route (routes-route)
  ((symbol :ri :with-prefix :std :unbound)
   (module required-method arbitrary-requirement
           headers variables additional-variables :ri)
   (render-method :i :std #'identity)
   (header :ir)))
```
   From [defclass-star example](http://common-lisp.net/project/defclass-star/configuration.lisp.html):
```lisp
(defclass configuration ()
  ((package-name      :type symbol  :initarg :package-name      :accessor package-name-of)
   (package-nicknames :initform '() :initarg :package-nicknames :accessor package-nicknames-of)
   (included-files    :initform '() :initarg :included-files    :accessor included-files-of)
   (gccxml-path       :initform "gccxml" :initarg :gccxml-path  :accessor gccxml-path-of)
   (gccxml-flags      :initform ""  :initarg :gccxml-flags      :accessor gccxml-flags-of)
   (hidden-symbols    :initform '() :initarg :hidden-symbols    :accessor hidden-symbols-of)
   (output-filename   :initform nil :initarg :output-filename   :accessor output-filename-of)
   (options           :initform (standard-configuration-options)
                      :initarg :options
                      :accessor options-of)
   (symbol-export-filter :initform 'standard-symbol-export-filter
                         :type (or (function (symbol)) symbol)
                         :initarg :symbol-export-filter
                         :accessor symbol-export-filter-of)
   (function-name-transformer :initform 'standard-name-transformer
                              :type (or (function (string)) symbol)
                              :initarg :function-name-transformer
                              :accessor function-name-transformer-of)
   (variable-name-transformer :initform 'standard-name-transformer
                              :type (or (function (string)) symbol)
                              :initarg :variable-name-transformer
                              :accessor variable-name-transformer-of)
   (type-name-transformer :initform 'standard-name-transformer
                          :type (or (function (string)) symbol)
                          :initarg :type-name-transformer
                          :accessor type-name-transformer-of)
   (temp-directory    :initform (make-pathname :directory "/tmp")
                      :initarg :temp-directory
                      :accessor temp-directory-of)
   (working-directory :initform *default-pathname-defaults*
                      :initarg :working-directory
                      :accessor working-directory-of)))

;;; And the equivalent defclass* version (56 tree leaves):
(defclass* configuration ()
  ((package-name
                              :type symbol)
   (package-nicknames         '())
   (included-files            '())
   (gccxml-path               "gccxml")
   (gccxml-flags              "")
   (hidden-symbols            '())
   (output-filename           nil)
   (options                   (standard-configuration-options))
   (symbol-export-filter      'standard-symbol-export-filter
                              :type (or (function (symbol)) symbol))
   (function-name-transformer 'standard-name-transformer
                              :type (or (function (string)) symbol))
   (variable-name-transformer 'standard-name-transformer
                              :type (or (function (string)) symbol))
   (type-name-transformer     'standard-name-transformer
                              :type (or (function (string)) symbol))
   (temp-directory            (make-pathname :directory "/tmp"))
   (working-directory         *default-pathname-defaults*)))

;; And the equivalent defclass/std version (46 tree leaves):
(defclass/std configuration ()
  ((package-name :type symbol :std :unbound)
   (package-nicknames included-files hidden-symbols output-filename)
   (gccxml-path :std "gccxml")
   (gccxml-flags :std "")
   (options :std (standard-configuration-options))
   (symbol-export-filter :std 'standard-symbol-export-filter
                         :type (or (function (symbol)) symbol))
   (function-name-transformer variable-name-transformer type-name-transformer
                              :std 'standard-name-transformer
                              :type (or (function (string)) symbol))
   (temp-directory :std (make-pathname :directory "/tmp"))
   (working-directory :std *default-pathname-defaults*)))
```
   From [cl-hue](https://github.com/jd/cl-hue/blob/master/cl-hue.lisp):
```lisp
(defclass light ()
  ((bridge :initarg :bridge :accessor light-bridge)
   (number :initarg :number :accessor light-number)
   (type :initarg :type :accessor light-type)
   (name :initarg :name :accessor light-name)
   (modelid :initarg :modelid :accessor light-modelid)
   (uniqueid :initarg :uniqueid :accessor light-uniqueid)
   (swversion :initarg :swversion :accessor light-swversion)
   (pointsymbol :initarg :pointsymbol :accessor light-pointsymbol)
   (on :initarg :on :accessor light-on-p)
   (brightness :initarg :brightness :accessor light-brightness)
   (hue :initarg :hue :accessor light-hue)
   (saturation :initarg :saturation :accessor light-saturation)
   (xy :initarg :xy :accessor light-xy)
   (ct :initarg :ct :accessor light-ct)
   (alert :initarg :alert :accessor light-alert)
   (effect :initarg :effect :accessor light-effect)
   (colormode :initarg :colormode :accessor light-colormode)
   (reachable :initarg :reachable :accessor light-reachable-p)))

; could be written:
(defclass/std light ()
  ((bridge number type name modelid uniqueid swversion pointsymbol on brightness
           hue saturation xy ct alert effect colormode reachable
           :with-prefix :std :unbound)))

; or, using class/std:

(class/std light
  bridge number type name modelid uniqueid swversion pointsymbol on brightness
  hue saturation xy ct alert effect colormode reachable
  :std :unbound :with)

; or, with *default-std* set to nil and *with-prefix* set to t:

(class/std light
  bridge number type name modelid uniqueid swversion pointsymbol on brightness
  hue saturation xy ct alert effect colormode reachable)
```

   There's a shortcut to setup a basic printing behaviour of a class, using `printing-unreadably`, but see also `define-print-object/std` for this now.

```lisp
(printing-unreadably (field2 field3) (class/std myclass field1 field2 field3))

; which expands to:

(PROGN
  (CLASS/STD MYCLASS FIELD1 FIELD2 FIELD3)
  (DEFMETHOD PRINT-OBJECT ((MYCLASS MYCLASS) #:STREAM1722)
    (PRINT-UNREADABLE-OBJECT (MYCLASS #:STREAM1722 :TYPE T :IDENTITY T)
      (FORMAT #:STREAM1722 "FIELD2: ~s, FIELD3: ~s"
              (FIELD2 MYCLASS) (FIELD3 MYCLASS)))))
```

## Limitations

Limitations are in the tools integration.

If you are faced with these limitations and the solution doesn't suit you,
just transform your
defclass/std to a regular defclass. You can see the maroexpansion with
`C-c M` (`slime-macroexpand-1`) and copy-paste the expansion (followed
by M-x downcase-region …).

### Limitation 1

In Emacs and Slime (and any good editor), when the point is inside a
class definition, you can press `C-c C-y` (`slime-call-defun`) to send
a `make-instance` form on the REPL:

~~~lisp
(defclass test ()
  (a b)|) ;; <-- | point is here
~~~

C-c C-y =>

    CL-REPL> (make-instance 'home-package::test |)

This doesn't work *by default* inside a `defclass/std` form, you get "not in a function definition". But we can have it.

### Solution 1

We can overwrite 2 Slime functions to have this keybinding back:

```lisp
;; originally in slime-repl.el
(defun slime-call-defun ()
  "Insert a call to the toplevel form defined around point into the REPL."
  (interactive)
  (cl-labels ((insert-call
               (name &key (function t)
                     defclass)
               (let* ((setf (and function
                                 (consp name)
                                 (= (length name) 2)
                                 (eql (car name) 'setf)))
                      (symbol (if setf
                                  (cadr name)
                                name))
                      (qualified-symbol-name
                       (slime-qualify-cl-symbol-name symbol))
                      (symbol-name (slime-cl-symbol-name qualified-symbol-name))
                      (symbol-package (slime-cl-symbol-package
                                       qualified-symbol-name))
                      (call (if (cl-equalp (slime-lisp-package) symbol-package)
                                symbol-name
                              qualified-symbol-name)))
                 (slime-switch-to-output-buffer)
                 (goto-char slime-repl-input-start-mark)
                 (insert (if function
                             "("
                           " "))
                 (when setf
                   (insert "setf ("))
                 (if defclass
                     (insert "make-instance '"))
                 (insert call)
                 (cond (setf
                        (insert " ")
                        (save-excursion (insert ") )")))
                       (function
                        (insert " ")
                        (save-excursion (insert ")"))))
                 (unless function
                   (goto-char slime-repl-input-start-mark)))))
    (let ((toplevel (slime-parse-toplevel-form '(:defun :defgeneric :defmacro :define-compiler-macro
                                                        :defmethod :defparameter :defvar :defconstant :defclass
                                                        :defclass/std))))  ;; <---- ADDED
      (if (symbolp toplevel)
          (error "Not in a function definition")
        (slime-dcase toplevel
          (((:defun :defgeneric :defmacro :define-compiler-macro) symbol)
           (insert-call symbol))
          ((:defmethod symbol &rest args)
           (declare (ignore args))
           (insert-call symbol))
          (((:defparameter :defvar :defconstant) symbol)
           (insert-call symbol :function nil))
          (((:defclass) symbol)
           (insert-call symbol :defclass t))
          (((:defclass/std) symbol)               ;; <----------- ADDED
           (insert-call symbol :defclass t))
          (t
           (error "Not in a function definition")))))))
```

and

```lisp
;; originally in slime-parse.el
(defun slime-parse-context (name)
  (save-excursion
    (cond ((slime-in-expression-p '(defun *))          `(:defun ,name))
          ((slime-in-expression-p '(defmacro *))       `(:defmacro ,name))
          ((slime-in-expression-p '(defgeneric *))     `(:defgeneric ,name))
          ((slime-in-expression-p '(setf *))
           ;;a setf-definition, but which?
           (backward-up-list 1)
           (slime-parse-context `(setf ,name)))
          ((slime-in-expression-p '(defmethod *))
           (unless (looking-at "\\s ")
             (forward-sexp 1)) ; skip over the methodname
           (let (qualifiers arglist)
             (cl-loop for e = (read (current-buffer))
                      until (listp e) do (push e qualifiers)
                      finally (setq arglist e))
             `(:defmethod ,name ,@qualifiers
                          ,(slime-arglist-specializers arglist))))
          ((and (symbolp name)
                (slime-in-expression-p `(,name)))
           ;; looks like a regular call
           (let ((toplevel (ignore-errors (slime-parse-toplevel-form))))
             (cond ((slime-in-expression-p `(setf (*)))  ;a setf-call
                    (if toplevel
                        `(:call ,toplevel (setf ,name))
                      `(setf ,name)))
                   ((not toplevel)
                    name)
                   ((slime-in-expression-p `(labels ((*))))
                    `(:labels ,toplevel ,name))
                   ((slime-in-expression-p `(flet ((*))))
                    `(:flet ,toplevel ,name))
                   (t
                    `(:call ,toplevel ,name)))))
          ((slime-in-expression-p '(define-compiler-macro *))
           `(:define-compiler-macro ,name))
          ((slime-in-expression-p '(define-modify-macro *))
           `(:define-modify-macro ,name))
          ((slime-in-expression-p '(define-setf-expander *))
           `(:define-setf-expander ,name))
          ((slime-in-expression-p '(defsetf *))
           `(:defsetf ,name))
          ((slime-in-expression-p '(defvar *))       `(:defvar ,name))
          ((slime-in-expression-p '(defparameter *)) `(:defparameter ,name))
          ((slime-in-expression-p '(defconstant *))  `(:defconstant ,name))
          ((slime-in-expression-p '(defclass *))     `(:defclass ,name))
          ((slime-in-expression-p '(defclass/std *)) `(:defclass ,name)) ;; <-- ADDED
          ((slime-in-expression-p '(defpackage *))   `(:defpackage ,name))
          ((slime-in-expression-p '(defstruct *))
           `(:defstruct ,(if (consp name)
                             (car name)
                           name)))
          (t
           name))))
```


### ~~Limitation 2~~

Likewise, when the point is on a class name, we can call `M-x
slime-export-class`. This adds the class name and all the
accessors/readers/writers symbols to the `:export` clause of your
package. ~~It doesn't work with a `defclass/std` form.~~ It just works actually.


## Dependencies
This project depends only on [Anaphora](http://common-lisp.net/project/anaphora/) and [Alexandria](https://common-lisp.net/project/alexandria/) libraries. The test package uses the [prove](github.com/fukamachi/prove) test library.

## Installation
Available on [Quicklisp](http://quicklisp.org):
```
(ql:quickload :defclass-std)
```

*warn: this fork is not (yet?) in Quicklisp*

## Bugs
If you find any bug or inconsistency in the code, or if you find it too hard to use, please, feel free to open an issue.

## Tests
This library is tested under [ABCL](https://common-lisp.net/project/armedbear/), [SBCL](http://www.sbcl.org/), [CCL](http://ccl.clozure.com/), [CLISP](http://www.clisp.org/) and [ECL](https://common-lisp.net/project/ecl/) Common Lisp implementations.

To run all the defined tests, use:
```lisp
* (asdf:test-system :defclass-std)
; prints lots of (colorful) stuff...
; => T
```
Tests are ran with [Travis CI](https://travis-ci.org/EuAndreh/defclass-std) and [Circle CI](https://circleci.com/gh/EuAndreh/defclass-std) using [cl-travis](https://github.com/luismbo/cl-travis), [CIM](https://github.com/KeenS/CIM), [cl-coveralls](https://github.com/fukamachi/cl-coveralls) and [Roswell](https://github.com/snmsts/roswell). Check it out!

## Authors
+ [André Miranda](https://github.com/EuAndreh)
+ [Joram Schrijver](https://github.com/jorams)
+ lisp-maintainers

## License
[LLGPL](https://tldrlegal.com/license/lisp-lesser-general-public-license#fulltext).
