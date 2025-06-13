# trivial-macroexpand-all

Provides a macroexpand-all function that calls the implementation
specific equivalent.

Supports: [ABCL][], [Allegro][], [CCL][], [Clasp][], [CLISP][],
[CMUCL][], [Corman Lisp][], [ECL][], [Lispworks][], [MKCL][], [SBCL][]
and SCL.

The first argument to MACROEXPAND-ALL should be the form to be
expanded. The second argument is an optional environment that may not
be respected by the implementation. The returned form will either be
an expanded form or the original form if the implementation does not
support MACROEXPAND-ALL.

If the implementation supports MACROEXPAND-ALL then the keyword
`:macroexpand-all` will be present in `*features*`. If the
implementation respects the environment argument then
`:macroexpand-all/env` will be present in `*features*`.

## Example

```
CL-USER> (trivial-macroexpand-all:macroexpand-all '(or 1 2 3 4))

(LET ((#:G622 1))
  (IF #:G622
      #:G622
      (LET ((#:G623 2))
        (IF #:G623
            #:G623
            (LET ((#:G624 3))
              (IF #:G624
                  #:G624
                  4)))))
```

[ABCL]: https://armedbear.common-lisp.dev/
[Allegro]: https://franz.com/products/allegro-common-lisp/
[CCL]: https://github.com/Clozure/ccl
[CLISP]: https://gitlab.com/gnu-clisp/clisp
[CMUCL]: https://gitlab.common-lisp.net/cmucl/cmucl
[Clasp]: https://clasp-developers.github.io/
[Corman Lisp]: https://github.com/sharplispers/cormanlisp
[ECL]: https://ecl.common-lisp.dev/
[LispWorks]: https://www.lispworks.com/products/lispworks.html
[MKCL]: https://mkcl.common-lisp.dev/
[SBCL]: http://sbcl.org/
