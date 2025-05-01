Testing
=======

Testing within Lisp
-------------------

- Run the Common Lisp tests with

        (asdf:test-system "mgl-pax")
        (asdf:test-system "dref")

    Or just load the `"mgl-pax/test"` system and do, for example:

        (mgl-pax-test:test :debug 'try:unexpected).
        (dref-test:test :print '(or try:failure try:unexpected))

- To run the Elisp tests, load `test/mgl-pax-tests.el`, `M-x ert` and
  enter `"mgl-pax"` (yes, within quotes) to run only the PAX tests
  (excluding Slime's). Some tests require that Slime is connected to a
  Lisp and load PAX if it isn't already.

The tests do not fail if there are unexpected successes, but it's
expected not to expect them to be failures.

Testing from the command line
-----------------------------

- `test/test.sh` and `dref/test/test.sh` test the Common Lisp side.
  They run the tests on several Lisp implementations assuming that
  they are installed under Roswell (e.g. `ros --lisp sbcl run` works).
  So install ABCL, AllegroCL, CCL, CMUCL, CLISP, ECL, and SBCL under
  Roswell:

        for lisp in allegro abcl-bin ccl-bin clisp cmu-bin ecl sbcl-bin; do
            ros install $lisp
        done

- `test/test-el.sh` runs the Elisp tests. This currently only tests
  with SBCL and needs the value of `SLIME_DIR` to be specified in the
  script.

    Debugging test failures can be easier if `--batch` is removed from
    the script.

Catching changes in behaviour
-----------------------------

Some bugs not caught by the test suite may be caught by transcription
consistency checking or show up in the diffs of the generated
documentation. Check out PAX World in the top-level directory, where
the ASDF files are with

    git clone https://github.com/melisgl/mgl-pax-world.git world/

After changing the code, regenerate the readmes and PAX World (see
`#+nil`ed out forms near the bottom of
`src/document/document-util.lisp`), then check the `git diff`s. Note
that you need to `cd` into `world/` and get the diff there too because
it is a separate git checkout.


Debugging
=========

Most PAX functions invoked from Emacs are wrapped in
`swank/backend:converting-errors-to-error-location`, which handles
`error`s. Do following turn debugging on and off:

    (setq swank/backend:*debug-swank-backend* t)
    (setq swank/backend:*debug-swank-backend* nil)

For Elisp,

    (setq debug-on-error t)
    (setq debug-on-error nil)

HUNCHENTOOT catches errors and logs them. Turn debugging HUNCHENTOOT
handlers on and off with:

    (setq hunchentoot:*catch-errors-p* nil)
    (setq hunchentoot:*catch-errors-p* t)


Versioning
==========

`:VERSION` in the ASDF system definition is in the usual
`MAJOR.MINOR.PATCH` format. Bump `MINOR` for significant changes.
Bumping `PATCH` is optional. `MAJOR` is constant 0 :-).

When making an incompatible change to the Elisp-CL interface or even a
bugfix to the ELisp side, bump `:VERSION` in the ASDF and copy it to
`mgl-pax-version` and `MGL-PAX::CHECK-PAX-ELISP-VERSION`. This way the
user will be notified if the Elisp code is incompatible with the Lisp
side or it lacks available bug fixes.
