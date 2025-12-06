## Introduction

This directory contains source files each of which implements a syntax
extension.

## Extended package prefix

Based on an
[SBCL extension](https://sbcl.org/manual/index.html#Extended-Package-Prefix-Syntax),
the syntax `PACKAGE::OBJECT` can be used to read an object in a
package other than the current one.

Examples:

```lisp
(let ((eclector.reader:*client* (make-instance 'eclector.syntax-extensions.extended-package-prefix:extended-package-prefix-syntax-mixin)))
  (read-from-string "cl-user::(foo bar 1)"))
; => (cl-user::foo cl-user::bar 1)
```

```lisp
(let ((eclector.reader:*client* (make-instance 'eclector.syntax-extensions.extended-package-prefix:extended-package-prefix-syntax-mixin)))
  (read-from-string "cl-user::#(foo)"))
; => #(cl-user::foo)
```

```lisp
(let ((eclector.reader:*client* (make-instance 'eclector.syntax-extensions.extended-package-prefix:extended-package-prefix-syntax-mixin)))
  (read-from-string "cl-user::;foo
bar"))
; => cl-user::bar
```

## S-expression comments

A reader macro, `s-expression-comment`, that is loosely based on
[SRFI 62: S-expression comments](https://srfi.schemers.org/srfi-62/srfi-62.html).
One difference is that a numeric infix argument can be used to comment
out a number of s-expressions that is different from 1. For example,
`"(1 #2; 2 3 4)"` is `read` as `(1 4)`. This extension might be useful
for commenting out multiple arguments or keyword arguments:

```lisp
(frob r1 r2 :k3 4 #4; :k5 6 :k6 7)
```

While this syntax extension could be implemented as a portable
library, this particular implementation uses Eclector protocols in
order to produce better error messages and parse results.
