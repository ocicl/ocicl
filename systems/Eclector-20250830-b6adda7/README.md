# Eclector: A portable and extensible Common Lisp Reader

## Introduction

The `eclector` system provides a portable implementation of a reader
following the Common Lisp specification.

**``eclector`` is under active development. Its ASDF system structure,
package structure, exported symbols and protocols may change at any
time but are becoming less and less likely to do so in incompatible
ways. Consult the NEWS file or the "Changelog" section of the manual
for lists of changes in specific versions.**

This document only gives a very brief overview and highlights some
features. Proper documentation can be found in the `documentation`
directory.

## Usage Overview and Highlights

### Basics

In the simplest case, the eclector reader can be used like any Common
Lisp reader:

* ```lisp
  (with-input-from-string (stream "(1 2 3)")
    (eclector.reader:read stream))
  ; => (1 2 3)
  ```

* ```lisp
  (eclector.reader:read-from-string "#C(1 1)")
  ; => #C(1 1) 7
  ```

### Error Recovery

In contrast to many other reader implementations, eclector can recover
from most errors in the input supplied to it and continue
reading. This capability is realized as a restart named
`eclector.reader:recover` which is established whenever an error is
signaled for which a recovery strategy is available.

For example, the following code

```lisp
(handler-bind ((error (lambda (condition)
                        (let ((restart (find-restart 'eclector.reader:recover)))
                          (format t "Recovering from error:~%~2@T~A~%using~%~2@T~A~%"
                                  condition restart))
                        (eclector.reader:recover))))
  (eclector.reader:read-from-string "`(::foo ,"))
```

produces this:

```
Recovering from error:
  A symbol token must not start with two package markers as in ::name.
using
  Treat the character as if it had been escaped.
Recovering from error:
  While reading unquote, expected an object when input ended.
using
  Use NIL in place of the missing object.
Recovering from error:
  While reading list, expected the character ) when input ended.
using
  Return a list of the already read elements.
; => (ECLECTOR.READER:QUASIQUOTE (:FOO (ECLECTOR.READER:UNQUOTE NIL))) 9
```

indicating that eclector recovered from multiple errors and consumed
all input. Of course, the returned expression is likely unsuitable for
evaluation, but recovery is useful for detecting multiple errors in
one go and performing further processing such as static analysis.

### Custom Parse Results

Using features provided in the `eclector.parse-result` package,
the reader can produce parse results controlled by the client,
optionally including source tracking and representation of skipped
input (due to e.g. comments and reader conditionals):

```lisp
(defclass my-client (eclector.parse-result:parse-result-client)
  ())

(defmethod eclector.parse-result:make-expression-result
    ((client my-client) (result t) (children t) (source t))
  (list :result result :source source :children children))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client my-client) (stream t) (reason t) (children t) (source t))
  (list :reason reason :source source :children children))

(with-input-from-string (stream "(1 #|comment|# \"string\")")
  (eclector.parse-result:read (make-instance 'my-client) stream))
```

### Concrete Syntax Trees

The `eclector.concrete-syntax-tree` system provides a variant of the
`eclector` reader that produces instances of the concrete syntax tree
classes provided by the [concrete syntax tree library]:

```lisp
(eclector.concrete-syntax-tree:read-from-string "(1 2 3)")
; => #<CONCRETE-SYNTAX-TREE:CONS-CST raw: (1 2 3) {100BF94EF3}> 7 NIL
```

[concrete syntax tree library]: https://github.com/s-expressionists/Concrete-Syntax-Tree
