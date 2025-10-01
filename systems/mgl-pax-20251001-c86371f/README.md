<a id="x-28MGL-PAX-3A-40PAX-MANUAL-20MGL-PAX-3ASECTION-29"></a>

# PAX Manual

## Table of Contents

- [1 Introduction][685e]
- [2 Links and Systems][ba90]
- [3 Emacs Setup][8541]
    - [3.1 Functionality Provided][d4a9]
    - [3.2 Installing from Quicklisp][f3f4]
    - [3.3 Loading PAX][d3fc]
    - [3.4 Setting up Keys][cfab]
- [4 Background][f74b]
- [5 Basics][94c7]
- [6 PAX Locatives][292a]
- [7 Navigating Sources in Emacs][3386]
    - [7.1 `M-.` Defaulting][460e]
    - [7.2 `M-.` Prompting][ed46]
        - [7.2.1 `M-.` Minibuffer Syntax][8106]
        - [7.2.2 `M-.` Completion][e444]
- [8 Generating Documentation][2c93]
    - [8.1 The `DOCUMENT` Function][dc0a]
        - [8.1.1 `DOCUMENTABLE`][0702]
        - [8.1.2 Return Values][7dc7]
        - [8.1.3 `PAGES`][9c7d]
        - [8.1.4 Package and Readtable][ab7e]
    - [8.2 Browsing Live Documentation][a595]
        - [8.2.1 Browsing with w3m][83d5]
        - [8.2.2 Browsing with Other Browsers][c434]
        - [8.2.3 Apropos][b7fc]
        - [8.2.4 PAX Live Home Page][9d50]
    - [8.3 Markdown Support][c2d3]
        - [8.3.1 Markdown in Docstrings][7bf5]
        - [8.3.2 Markdown in Titles][165c]
        - [8.3.3 Syntax Highlighting][bc83]
        - [8.3.4 MathJax][a17d]
    - [8.4 Codification][f1ab]
    - [8.5 Linking][19e3]
        - [8.5.1 Reflink][cbc4]
        - [8.5.2 Autolink][ec7a]
        - [8.5.3 Linking to the HyperSpec][7cc3]
        - [8.5.4 Linking to Sections][22c2]
        - [8.5.5 Filtering Links][b2e4]
        - [8.5.6 Link Format][c0d2]
    - [8.6 Local Definition][9db9]
    - [8.7 Overview of Escaping][2634]
    - [8.8 Output Formats][8d9b]
        - [8.8.1 Plain Output][c879]
        - [8.8.2 Markdown Output][dd29]
        - [8.8.3 PDF Output][19ad]
        - [8.8.4 Dummy Output][f7e6]
    - [8.9 Documentation Generation Implementation Notes][d1ca]
    - [8.10 Utilities for Generating Documentation][1b1b]
        - [8.10.1 HTML Output][36e1]
        - [8.10.2 GitHub Workflow][dff6]
        - [8.10.3 PAX World][1281]
- [9 Transcripts][6300]
    - [9.1 Transcribing with Emacs][f5bd]
    - [9.2 Transcript API][9dbc]
    - [9.3 Transcript Consistency Checking][4c39]
        - [9.3.1 Finer-Grained Consistency Checks][6e18]
        - [9.3.2 Controlling the Dynamic Environment][6b59]
        - [9.3.3 Utilities for Consistency Checking][8423]
- [10 Parsing][378f]
    - [10.1 Parsing Names][e65d]
        - [10.1.1 Raw Names in Words][f0d5]
        - [10.1.2 Names in Raw Names][016d]
    - [10.2 Parsing Locatives][ab38]
- [11 Writing Extensions][c4ce]
    - [11.1 Adding New Locatives][54d8]
    - [11.2 Locative Aliases][0fa3]
    - [11.3 Extending `DOCUMENT`][574a]
    - [11.4 Sections][8a58]
    - [11.5 Glossary Terms][d1dc]

###### \[in package MGL-PAX with nicknames PAX\]
<a id="x-28MGL-PAX-3A-40INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>

## 1 Introduction

*What if documentation really lived in the code?*

Docstrings are already there. If some narrative glued them together,
we'd be able develop and explore the code along with the
documentation due to their physical proximity. The main tool that
PAX provides for this is [`DEFSECTION`][72b4]:

```
(defsection @foo-random-manual (:title "Foo Random manual")
  "Foo Random is a random number generator library."
  (foo-random-state class)
  (uniform-random function)
  (@foo-random-examples section))
```

Like this one, sections can have docstrings and
[references][5225] to
definitions (e.g. `(UNIFORM-RANDOM FUNCTION)`). These docstrings and
references are the glue. To support interactive development, PAX

- makes [SLIME][6be7]'s [`M-.`][cb15] work with references and

- adds a documentation browser.

See [Emacs Setup][8541].

Beyond interactive workflows, [Generating Documentation][2c93] from
sections and all the referenced items in Markdown or HTML format is
also implemented.

With the simplistic tools provided, one may emphasize the narrative
as with Literate Programming, but documentation is generated from
code, not vice versa, and there is no support for chunking.

*Code is first, code must look pretty, documentation is code*.

##### Docstrings

PAX automatically recognizes and [marks up code][f1ab] with
backticks and [links][19e3] names in code to their definitions.
Take, for instance, SBCL's [`ABORT`][479a] function, whose docstring is
written in the usual style, uppercasing names of symbols:

```
(docstring #'abort)
=> "Transfer control to a restart named ABORT, signalling
a CONTROL-ERROR if none exists."
```

Note how in the generated documentation, `ABORT` is set with a
monospace font, while `CONTROL-ERROR` is [Autolink][ec7a]ed:

- \[function\] **ABORT** *\&OPTIONAL CONDITION*

    Transfer control to a restart named `ABORT`, signalling
    a [`CONTROL-ERROR`][6bc0] if none exists.

[6bc0]: http://www.lispworks.com/documentation/HyperSpec/Body/e_contro.htm "CONTROL-ERROR CONDITION"

The following [transcript][6300] shows the raw Markdown for
the previous example.

```
(document #'abort :format :markdown)
.. - [function] **ABORT** *&OPTIONAL CONDITION*
..
..     Transfer control to a restart named `ABORT`, signalling
..     a [`CONTROL-ERROR`][7c2c] if none exists.
..
..   [7c2c]: http://www.lispworks.com/documentation/HyperSpec/Body/e_contro.htm "CONTROL-ERROR (MGL-PAX:CLHS CONDITION)"
..
```

##### A Complete Example

Here is an example of how it all works together:

```
(mgl-pax:define-package :foo-random
  (:documentation "This package provides various utilities for random.
  See FOO-RANDOM:@FOO-RANDOM-MANUAL.")
  (:use #:common-lisp #:mgl-pax))

(in-package :foo-random)

(defsection @foo-random-manual (:title "Foo Random manual")
  "FOO-RANDOM is a random number generator library inspired by CL:RANDOM.
  Functions such as UNIFORM-RANDOM use *FOO-STATE* and have a
  :RANDOM-STATE keyword arg."
  (foo-random-state class)
  (state (reader foo-random-state))
  "Hey we can also print states!"
  (print-object (method (foo-random-state t)))
  (*foo-state* variable)
  (gaussian-random function)
  (uniform-random function)
  ;; This is a subsection.
  (@foo-random-examples section))

(defclass foo-random-state ()
  ((state :reader state)))

(defmethod print-object ((object foo-random-state) stream)
  (print-unreadable-object (object stream :type t)))

(defvar *foo-state* (make-instance 'foo-random-state)
  "Much like *RANDOM-STATE* but uses the FOO algorithm.")

(defun uniform-random (limit &key (random-state *foo-state*))
  "Return a random number from the between 0 and LIMIT (exclusive)
  uniform distribution."
  nil)

(defun gaussian-random (stddev &key (random-state *foo-state*))
  "Return a random number from a zero mean normal distribution with
  STDDEV."
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
```

Note how `(VARIABLE *FOO-STATE*)` in the [`DEFSECTION`][72b4] form both
exports `*FOO-STATE*` and includes its documentation in
`@FOO-RANDOM-MANUAL`. The symbols [`VARIABLE`][6c83] and
[`FUNCTION`][ba62] are just two instances of [locative][7ac8]s,
which are used in `DEFSECTION` to refer to definitions tied to
symbols.

`(DOCUMENT @FOO-RANDOM-MANUAL)` generates fancy Markdown or HTML
output with [automatic markup][f25f] and [Autolink][ec7a]s uppercase [word][d7b0]s found in docstrings,
numbers sections, and creates a table of contents.

One can even generate documentation for different but related
libraries at the same time with the output going to different files
but with cross-page links being automatically added for symbols
mentioned in docstrings. In fact, this is what [PAX World][1281] does. See
[Generating Documentation][2c93] for some convenience functions to cover
the most common cases.

The [transcript][6300] in the code block tagged with
`cl-transcript` is automatically checked for up-to-dateness when
documentation is generated.

<a id="x-28MGL-PAX-3A-40LINKS-AND-SYSTEMS-20MGL-PAX-3ASECTION-29"></a>

## 2 Links and Systems

Here is the [official
repository](https://github.com/melisgl/mgl-pax) and the [HTML
documentation](http://melisgl.github.io/mgl-pax-world/mgl-pax-manual.html)
for the latest version. There is also a [PAX channel][pax-yt] on
youtube with a couple of videos.

[pax-yt]: https://www.youtube.com/playlist?list=PLxbqYr4DvjX68AEdLky4IiHG69VJu6f5s

PAX is built on top of the [DRef library][5225] (bundled in the same repository).

- *Installation for deployment*

    The base system is [mgl-pax][6fdb]. It has very few
    dependencies and is sufficient as a dependency for systems using
    the [Basics][94c7] to add documentation. This is to keep deployed code
    small. To install only the bare minimum, with no intention of
    using [Navigating Sources in Emacs][3386], [Generating Documentation][2c93],
    [Browsing Live Documentation][a595] or using [Transcripts][6300], under
    Quicklisp for example, PAX could be installed as:

        (ql:quickload "mgl-pax")

- *Installation for development*

    The heavier dependencies are on the other systems, which
    correspond to the main functionalities provided, intended to be
    used primarily during development. To install the dependencies
    for all features under Quicklisp, do

        (ql:quickload "mgl-pax/full")

    Having thus installed the dependencies, it is enough to load the
    base system, which will autoload the other systems as necessary.


<a id="x-28-22mgl-pax-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>

- [system] **"mgl-pax"**

    - _Version:_ 0.4.4
    - _Description:_ Documentation system, browser, generator. See the
        [PAX Manual][2415].
    - _Long Description:_ The base system. See [Links and Systems][ba90].
    - _Licence:_ MIT, see COPYING.
    - _Author:_ Gábor Melis
    - _Mailto:_ [mega@retes.hu](mailto:mega@retes.hu)
    - _Homepage:_ [http://github.com/melisgl/mgl-pax](http://github.com/melisgl/mgl-pax)
    - _Bug tracker:_ [https://github.com/melisgl/mgl-pax/issues](https://github.com/melisgl/mgl-pax/issues)
    - _Source control:_ [GIT](https://github.com/melisgl/mgl-pax.git)
    - *Depends on:* [dref][021a], mgl-pax-bootstrap, named-readtables, pythonic-string-reader
    - *Defsystem depends on:* mgl-pax.asdf

<a id="x-28-22mgl-pax-2Fnavigate-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>

- [system] **"mgl-pax/navigate"**

    - _Description:_ Support for [Navigating Sources in Emacs][3386] via Slime's
        [`M-.`][cb15] in [MGL-PAX][2415].
    - *Depends on:* alexandria, [dref/full][0c7e], [mgl-pax][6fdb], swank(?)
    - *Defsystem depends on:* mgl-pax.asdf

<a id="x-28-22mgl-pax-2Fdocument-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>

- [system] **"mgl-pax/document"**

    - _Description:_ Support for [Generating Documentation][2c93] in
        [MGL-PAX][2415].
    - *Depends on:* 3bmd, 3bmd-ext-code-blocks, 3bmd-ext-math, alexandria, colorize, md5, [mgl-pax/navigate][f155], [mgl-pax/transcribe][5825], trivial-utf-8
    - *Defsystem depends on:* mgl-pax.asdf

<a id="x-28-22mgl-pax-2Fweb-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>

- [system] **"mgl-pax/web"**

    - _Description:_ Web server for [Browsing Live Documentation][a595]
        in [MGL-PAX][2415].
    - *Depends on:* hunchentoot, [mgl-pax/document][4bb8]
    - *Defsystem depends on:* mgl-pax.asdf

<a id="x-28-22mgl-pax-2Ftranscribe-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>

- [system] **"mgl-pax/transcribe"**

    - _Description:_ Support for [Transcripts][6300] in
        [MGL-PAX][2415].
    - *Depends on:* alexandria, [mgl-pax/navigate][f155]
    - *Defsystem depends on:* mgl-pax.asdf

<a id="x-28-22mgl-pax-2Ffull-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>

- [system] **"mgl-pax/full"**

    - _Description:_ The [`mgl-pax`][6fdb] system with all features
        preloaded.
    - *Depends on:* [mgl-pax/document][4bb8], [mgl-pax/navigate][f155], [mgl-pax/transcribe][5825], [mgl-pax/web][a8c5]

<a id="x-28MGL-PAX-3A-40EMACS-SETUP-20MGL-PAX-3ASECTION-29"></a>

## 3 Emacs Setup

Here is a quick recipe for setting up PAX for use via [SLIME][6be7] to
take advantage of the [conveniences on offer][d4a9].
Conversely, there is no need to do any of this just to use
[`DEFSECTION`][72b4], write docstrings and for [Generating Documentation][2c93].

If PAX was installed from [Quicklisp][1539], then evaluate this in CL to
[install][1aed] the Elisp code in a stable location:

    (mgl-pax:install-pax-elisp "~/quicklisp/")

Assuming the Elisp file is in the `~/quicklisp/` directory, add
something like this to your `.emacs`:

```elisp
(add-to-list 'load-path "~/quicklisp/")
(require 'mgl-pax)
(global-set-key (kbd "C-.") 'mgl-pax-document)
(global-set-key (kbd "s-x t") 'mgl-pax-transcribe-last-expression)
(global-set-key (kbd "s-x r") 'mgl-pax-retranscribe-region)
```

When [Browsing with Other Browsers][c434], for clicking on the locative
next to a definition to visit the corresponding source location in
Emacs, permission needs to be given:

```elisp
(setq slime-enable-evaluate-in-emacs t)
```


<a id="x-28MGL-PAX-3A-40EMACS-FUNCTIONALITY-20MGL-PAX-3ASECTION-29"></a>

### 3.1 Functionality Provided

- For [Navigating Sources in Emacs][3386], loading `mgl-pax` extends
  `slime-edit-definitions` ([`M-.`][cb15]) by adding
  `mgl-pax-edit-definitions` to `slime-edit-definition-hooks`. There
  are no related variables to customize.

- For [Browsing Live Documentation][a595], `mgl-pax-browser-function` and
  `mgl-pax-web-server-port` can be customized in Elisp. To browse
  within Emacs, choose `w3m-browse-url` (see [w3m][7439]), and make sure
  both the w3m binary and the w3m Emacs package are installed. On
  Debian, simply install the `w3m-el` package. With other browser
  functions, a HUNCHENTOOT web server is started.

- See [Transcribing with Emacs][f5bd] for how to use the transcription
  features. There are no related variables to customize.

Independently from the Common Lisp side, the Elisp functions
`mgl-pax-hideshow-documentation` and `mgl-pax-hideshow-comments`
help focus on the code only by folding or unfolding
[`MGL-PAX:DEFSECTION`][72b4], [`MGL-PAX:DEFINE-GLOSSARY-TERM`][8ece] forms and long
strings, or comments.

<a id="x-28MGL-PAX-3A-40EMACS-QUICKLISP-20MGL-PAX-3ASECTION-29"></a>

### 3.2 Installing from Quicklisp

If you installed PAX with Quicklisp, the location of `mgl-pax.el`
may change with updates, and you may want to copy the current
version of `mgl-pax.el` to a stable location by evaluating this in
CL:

    (mgl-pax:install-pax-elisp "~/quicklisp/")

If working from, say, a git checkout, there is no need for this
step.

<a id="x-28MGL-PAX-3AINSTALL-PAX-ELISP-20FUNCTION-29"></a>

- [function] **INSTALL-PAX-ELISP** *TARGET-DIR*

    Install `mgl-pax.el` distributed with this package in `TARGET-DIR`.

<a id="x-28MGL-PAX-3A-40EMACS-LOADING-20MGL-PAX-3ASECTION-29"></a>

### 3.3 Loading PAX

Assuming the Elisp file is in the `~/quicklisp/` directory, add
something like this to your `.emacs`:

```elisp
(add-to-list 'load-path "~/quicklisp/")
(require 'mgl-pax)
```

If the Elisp variable `mgl-pax-autoload` is true (the default), then
PAX will be loaded in the connected Lisp on-demand via [SLIME][6be7].

If loading fails, `mgl-pax` will be unloaded from Emacs and any
[overridden Slime key bindings][cfab] restored.

<a id="x-28MGL-PAX-3A-40EMACS-KEYS-20MGL-PAX-3ASECTION-29"></a>

### 3.4 Setting up Keys

The recommended key bindings are this:

```
(global-set-key (kbd "C-.") 'mgl-pax-document)
(global-set-key (kbd "s-x t") 'mgl-pax-transcribe-last-expression)
(global-set-key (kbd "s-x r") 'mgl-pax-retranscribe-region)
```

The global key bindings above are global because their commands work
in any mode. If that's not desired, one may bind `C-.` locally in
all Slime related modes like this:

```elisp
(slime-bind-keys slime-parent-map nil '(("C-." mgl-pax-document)))
```

If the customizable variable `mgl-pax-hijack-slime-doc-keys` is
true, then upon loading `mgl-pax`, the following changes are made to
`slime-doc-map` (assuming it's bound to `C-c C-d`):

- `C-c C-d a`: replaces `slime-apropos` with `mgl-pax-apropos`

- `C-c C-d z`: replaces `slime-apropos-all` with `mgl-pax-apropos-all`

- `C-c C-d p`: replaces `slime-apropos-package` with `mgl-pax-apropos-package`

- `C-c C-d f`: replaces `slime-describe-function` with `mgl-pax-document`

- `C-c C-d d`: replaces `slime-describe-symbol` with
   `mgl-pax-hideshow-documentation`

- `C-c C-d c`: installs `mgl-pax-hideshow-comments`

- `C-c C-d u`: installs `mgl-pax-edit-parent-section`

Calling `mgl-pax-unhijack-slime-doc-keys` reverts these changes.

<a id="x-28MGL-PAX-3A-40BACKGROUND-20MGL-PAX-3ASECTION-29"></a>

## 4 Background

As a user, I frequently run into documentation that's incomplete
and out of date, so I tend to stay in the editor and explore the
code by jumping around with [SLIME][6be7]'s [`M-.`][cb15] (`slime-edit-definition`).
As a library author, I spend a great deal of time polishing code but
precious little writing documentation.

In fact, I rarely write anything more comprehensive than docstrings
for exported stuff. Writing docstrings feels easier than writing a
separate user manual, and they are always close at hand during
development. The drawback of this style is that users of the library
have to piece the big picture together themselves.

That's easy to solve, I thought, let's just put all the narrative
that holds docstrings together in the code and be a bit like a
Literate Programmer turned inside out. The original prototype, which
did almost everything I wanted, was this:

```
(defmacro defsection (name docstring)
  `(defun ,name () ,docstring))
```

Armed with this `DEFSECTION`, I soon found myself
organizing code following the flow of user-level documentation and
relegated comments to implementation details entirely. However, some
parts of `DEFSECTION` docstrings were just listings of
all the functions, macros and variables related to the narrative,
and this list was repeated in the [`DEFPACKAGE`][9b43] form complete with
little comments that were like section names. A clear violation of
[OAOO][7d18], one of them had to go, so `DEFSECTION` got a list
of symbols to export.

That was great, but soon I found that the listing of symbols is
ambiguous if, for example, a function, a compiler macro and a class
were named by the same symbol. This did not concern exporting, of
course, but it didn't help readability. Distractingly, on such
symbols, `M-.` was popping up selection dialogs. There were two
birds to kill, and the symbol got accompanied by a type, which was
later generalized into the concept of locatives:

```
(defsection @introduction ()
  "A single line for one man ..."
  (foo class)
  (bar function))
```

After a bit of elisp hacking, `M-.` was smart enough to
disambiguate based on the locative found in the vicinity of the
symbol, and everything was good for a while.

Then, I realized that sections could refer to other sections if
there were a [`SECTION`][672f] locative. Going down that path, I soon began to
feel the urge to generate pretty documentation as all the necessary
information was available in the `DEFSECTION` forms. The design
constraint imposed on documentation generation was that following
the typical style of upcasing symbols in docstrings, there should be
no need to explicitly mark up links: if `M-.` works, then the
documentation generator shall also be able figure out what's being
referred to.

I settled on [Markdown][a317] as a reasonably non-intrusive format, and a
few thousand lines later PAX was born. Since then, locatives and
references were factored out into the [DRef][5225]
library to let PAX focus on `M-.` and documentation.

<a id="x-28MGL-PAX-3A-40BASICS-20MGL-PAX-3ASECTION-29"></a>

## 5 Basics

Now let's examine the most important pieces.

<a id="x-28MGL-PAX-3ADEFSECTION-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFSECTION** *NAME (&KEY (PACKAGE '\*PACKAGE\*) (READTABLE '\*READTABLE\*) (EXPORT T) TITLE LINK-TITLE-TO (DISCARD-DOCUMENTATION-P \*DISCARD-DOCUMENTATION-P\*)) &BODY ENTRIES*

    Define a documentation section and maybe export referenced symbols.
    A bit behind the scenes, a global variable with `NAME` is defined and
    is bound to a [`SECTION`][5fac] object. By convention, section names
    start with the character `@`. See [Introduction][685e] for an example.
    
    **Entries**
    
    `ENTRIES` consists of docstrings and references in any order.
    Docstrings are arbitrary strings in Markdown format.
    
    References are [`XREF`][1538]s given in the form `(NAME LOCATIVE)`.
    For example, `(FOO FUNCTION)` refers to the function `FOO`, `(@BAR
    SECTION)` says that `@BAR` is a subsection of this
    one. `(BAZ (METHOD (T T T)))` refers to the default method of the
    three argument generic function `BAZ`. `(FOO FUNCTION)` is
    equivalent to `(FOO (FUNCTION))`. See the DRef [Introduction][ad80]
    for more.
    
    The same name may occur in multiple references, typically with
    different locatives, but this is not required.
    
    The references are not [`LOCATE`][8f19]d until documentation is generated, so
    they may refer to things yet to be defined.
    
    **Exporting**
    
    If `EXPORT` is true (the default), `NAME` and the [name][88cf]s of references
    among `ENTRIES` which are [`SYMBOL`][e5af]s are candidates for exporting. A
    candidate symbol is exported if
    
    - it is [accessible][3473] in `PACKAGE`, and
    
    - there is a reference to it in the section being defined which is
      approved by [`EXPORTABLE-REFERENCE-P`][e51f].
    
    See [`DEFINE-PACKAGE`][63f3] if you use the export feature. The idea with
    confounding documentation and exporting is to force documentation of
    all exported symbols.
    
    **Misc**
    
    `TITLE` is a string containing Markdown or `NIL`. If non-`NIL`, it
    determines the text of the heading in the generated output.
    `LINK-TITLE-TO` is a reference given as an `(NAME LOCATIVE)` pair or
    `NIL`, to which the heading will link when generating HTML. If not
    specified, the heading will link to its own anchor.
    
    When `DISCARD-DOCUMENTATION-P` (defaults to [`*DISCARD-DOCUMENTATION-P*`][730f])
    is true, `ENTRIES` will not be recorded to save memory.

<a id="x-28MGL-PAX-3A-2ADISCARD-DOCUMENTATION-P-2A-20VARIABLE-29"></a>

- [variable] **\*DISCARD-DOCUMENTATION-P\*** *NIL*

    The default value of [`DEFSECTION`][72b4]'s `DISCARD-DOCUMENTATION-P` argument.
    One may want to set `*DISCARD-DOCUMENTATION-P*` to true before
    building a binary application.

<a id="x-28MGL-PAX-3ADEFINE-PACKAGE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-PACKAGE** *PACKAGE &REST OPTIONS*

    This is like [`CL:DEFPACKAGE`][9b43] but silences warnings and errors
    signalled when the redefined package is at variance with the current
    state of the package. Typically this situation occurs when symbols
    are exported by calling [`EXPORT`][0c4f] (as is the case with [`DEFSECTION`][72b4]) as
    opposed to adding `:EXPORT` forms to the `DEFPACKAGE` form and the
    package definition is subsequently reevaluated. See the section on
    [package variance](http://www.sbcl.org/manual/#Package-Variance) in
    the SBCL manual.
    
    The bottom line is that if you rely on `DEFSECTION` to do the
    exporting, then you'd better use `DEFINE-PACKAGE`.

<a id="x-28MGL-PAX-3ADEFINE-GLOSSARY-TERM-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFINE-GLOSSARY-TERM** *NAME (&KEY TITLE URL (DISCARD-DOCUMENTATION-P \*DISCARD-DOCUMENTATION-P\*)) &BODY DOCSTRING*

    Define a global variable with `NAME`, and set it to a [`GLOSSARY-TERM`][8251] object. `TITLE`, `URL` and `DOCSTRING` are Markdown strings or
    `NIL`. Glossary terms are [`DOCUMENT`][432c]ed in the lightweight bullet +
    locative + name/title style. See the glossary entry [name][88cf] for an
    example.
    
    When a glossary term is linked to in documentation, its `TITLE` will
    be the link text instead of the name of the symbol (as with
    [`SECTION`][5fac]s).
    
    Glossary entries with a non-`NIL` `URL` are like external links: they
    are linked to their `URL` in the generated documentation. These offer
    a more reliable alternative to using Markdown reference links and
    are usually not included in `SECTION`s.
    
    When `DISCARD-DOCUMENTATION-P` (defaults to [`*DISCARD-DOCUMENTATION-P*`][730f])
    is true, `DOCSTRING` will not be recorded to save memory.

<a id="x-28MGL-PAX-3ANOTE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **NOTE** *&BODY ARGS*

    Define a note with an optional `NAME` and an optional
    `DOCSTRING`. The [`DOCSTRING`][affc] of the note is its
    own docstring concatenated with docstrings of other notes in the
    lexical scope of `BODY`.
    
    `ARGS` has the form \[`NAME`\] \[`DOCSTRING`\] `BODY`, where the square
    brackets indicate optional arguments. See below for the details of
    parsing `ARGS`.
    
    **`NOTE` is experimental and as such subject to change.**
    
    `NOTE` can occur in an any evaluated position without changing its
    `BODY`'s run-time behaviour or introducing any run-time overhead. [Top
    level forms][0f52] remain top level when whrapped in `NOTE`. The names
    of notes live in the same global namespace regardless of nesting or
    whether they are [top level form][0f52]s. *These properties come at
    the price of `NOTE` being weird: it defines named notes at
    macro-expansion time (or load time). But the definitions are
    idempotent, so it's fine to macroexpand `NOTE` any number of times.*
    
    Notes are similar to Lisp comments, but they can be included in the
    documentation. In fact, notes are auto-included: a [Specific Link][0361] to
    a note is equivalent to including it with the [`DOCSTRING`][ce75] locative.
    
    Notes are intended to help reduce the distance between code and its
    documentation when there is no convenient definition docstring to
    use nearby.
    
    ```common-lisp
    (note @xxx "We change the package."
      (in-package :mgl-pax))
    ==> #<PACKAGE "MGL-PAX">
    (values (docstring (dref '@xxx 'note)))
    => "We change the package."
    ```
    
    Here is an example of how to overdo things:
    
    ```common-lisp
    (note @1+*
      "This is a seriously overdone example."
      (defun 1+* (x)
        "[@1+* note][docstring]"
        (if (stringp x)
            (note (@1+*/1 :join #\Newline)
              "- If X is a STRING, then it is parsed as a REAL number."
              (let ((obj (read-from-string x)))
                (note "It is an error if X does not contain a REAL."
                  (unless (realp obj)
                    (assert nil)))
                (1+ obj)))
            (note "- Else, X is assumed to be REAL number, and we simply
                     add 1 to it."
              (1+ x)))))
    
    (1+* "7")
    => 8
    
    (values (docstring (dref '@1+* 'note)))
    => "This is a seriously overdone example.
    
    - If X is a STRING, then it is parsed as a REAL number.
    It is an error if X does not contain a REAL.
    
    - Else, X is assumed to be REAL number, and we simply
    add 1 to it."
    ```
    
    The parsing of `ARGS`:
    
    - If the first element of `ARGS` is not a string, then it is a `NAME` (a
      non-`NIL` [`SYMBOL`][e5af]) or name with options, currently destructured
      as `(NAME &KEY JOIN)`. As in [`DEFSECTION`][72b4] and [`DEFINE-GLOSSARY-TERM`][8ece],
      the convention is that `NAME` starts with a `@` character.
    
        `JOIN` is [`PRINC`][676d]ed before the docstring of a child note is output.
        Its default value is a string of two newline characters.
    
    - The next element of `ARGS` is a Markdown docstring. See
      [Markdown in Docstrings][7bf5].
    
    - The rest of `ARGS` is the `BODY`. If `BODY` is empty, then `NIL` is
      returned.
    
    Note that named and nameless notes can contain other named or
    nameless notes without restriction, but nameless notes without a
    lexically enclosing named note are just an [implicit progn][c2fd]
    with `BODY`, and their docstring is discarded.
    
    If `NOTE` occurs as a [top level form][0f52], then its [`SOURCE-LOCATION`][32da]
    is reliably recorded. Else, the quality of the source location
    varies, but it is at least within the right top level form on all
    implementations. On SBCL, exact source location is supported.

<a id="x-28MGL-PAX-3A-40PAX-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

## 6 PAX Locatives

To the [Basic Locative Types][1d1d] defined by DRef,
PAX adds a few of its own.

<a id="x-28MGL-PAX-3ASECTION-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **SECTION**

    - Direct locative supertypes: [`VARIABLE`][6c83]

    Refers to a [`SECTION`][5fac] defined by [`DEFSECTION`][72b4].
    
    `SECTION` is [`EXPORTABLE-LOCATIVE-TYPE-P`][c930] but not exported by
    default (see [`EXPORTABLE-REFERENCE-P`][e51f]).

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **GLOSSARY-TERM**

    - Direct locative supertypes: [`VARIABLE`][6c83]

    Refers to a [`GLOSSARY-TERM`][8251] defined by [`DEFINE-GLOSSARY-TERM`][8ece].
    
    `GLOSSARY-TERM` is [`EXPORTABLE-LOCATIVE-TYPE-P`][c930] but not exported by
    default (see [`EXPORTABLE-REFERENCE-P`][e51f]).

<a id="x-28MGL-PAX-3ANOTE-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **NOTE**

    Refers to named notes defined by the [`NOTE`][e2ae] macro.
    
    If a single link would be made to a `NOTE` (be it either a
    [Specific Link][0361] or an unambiguous [Unspecific Link][8e71]), then the `NOTE`'s
    [`DOCSTRING`][affc] is included as if with the [`DOCSTRING`][ce75] locative.
    
    `NOTE` is [`EXPORTABLE-LOCATIVE-TYPE-P`][c930] but not exported by default (see
    [`EXPORTABLE-REFERENCE-P`][e51f]).

<a id="x-28MGL-PAX-3ADISLOCATED-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **DISLOCATED**

    Refers to a symbol in a non-specific context. Useful for
    suppressing [Unspecific Autolink][e2a4]ing. For example, if there is a
    function called `FOO` then
    
        `FOO`
    
    will be linked (if [`*DOCUMENT-LINK-CODE*`][d9ee]) to its definition. However,
    
        [`FOO`][dislocated]
    
    will not be. With a dislocated locative, [`LOCATE`][8f19] always fails with a
    [`LOCATE-ERROR`][6334] condition. Also see [Escaping Autolinking][a270].
    
    `DISLOCATED` references do not [`RESOLVE`][63b4].

<a id="x-28MGL-PAX-3AARGUMENT-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **ARGUMENT**

    An alias for [`DISLOCATED`][e391], so that one can refer to an
    argument of a macro without accidentally linking to a class that has
    the same name as that argument. In the following example,
    `FORMAT` may link to `CL:FORMAT` (if we generated
    documentation for it):
    
    ```
    "See FORMAT in DOCUMENT."
    ```
    
    Since `ARGUMENT` is a locative, we can prevent that linking by writing:
    
    ```
    "See the FORMAT argument of DOCUMENT."
    ```
    
    `ARGUMENT` references do not [`RESOLVE`][63b4].

<a id="x-28MGL-PAX-3AINCLUDE-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **INCLUDE** *SOURCE &KEY LINE-PREFIX HEADER FOOTER HEADER-NL FOOTER-NL*

    This [`PSEUDO`][943a] locative refers to a region of a file. `SOURCE` can be a
    [`STRING`][b93c] or a [`PATHNAME`][0317], in which case the whole file
    is being pointed to, or it can explicitly supply `START`, `END`
    locatives. `INCLUDE` is typically used to include non-lisp files in
    the documentation (say Markdown or Elisp as in the next example) or
    regions of Lisp source files. This can reduce clutter and
    duplication.
    
    ```
    (defsection @example-section ()
      (mgl-pax.el (include #.(asdf:system-relative-pathname
                              :mgl-pax "src/mgl-pax.el")
                           :header-nl "```elisp" :footer-nl "```"))
      (foo-example (include (:start (dref-ext:make-source-location function)
                             :end (dref-ext:source-location-p function))
                            :header-nl "```"
                            :footer-nl "```")))
    ```
    
    In the above example, when documentation is generated, the entire
    `src/mgl-pax.el` file is included in the Markdown output surrounded
    by the strings given as `HEADER-NL` and `FOOTER-NL`. The documentation
    of `FOO-EXAMPLE` will be the region of the file from the
    [`SOURCE-LOCATION`][32da] of the `START` reference (inclusive) to the
    `SOURCE-LOCATION` of the `END` reference (exclusive). If only one of
    `START` and `END` is specified, then they default to the beginning and
    end of the file, respectively.
    
    Since `START` and `END` are literal references, pressing `M-.` on
    `PAX.EL` will open the `src/mgl-pax.el` file and put the cursor on
    its first character. `M-.` on `FOO-EXAMPLE` will go to the source
    location of the `FOO` function.
    
    With the [`LAMBDA`][4796] locative, one can specify positions in arbitrary
    files.
    
    - `SOURCE` is either an absolute pathname designator or a list
      matching the [destructuring lambda list][6067] `(&KEY START END)`,
      where `START` and `END` must be `NIL` or `(<NAME> <LOCATIVE>)`
      lists (not evaluated) like a [`DEFSECTION`][72b4] entry. Their
      `SOURCE-LOCATION`s constitute the bounds of the region of the file
      to be included. Note that the file of the source location of `START`
      and `END` must be the same. If `SOURCE` is a pathname designator, then
      it must be absolute so that the locative is context independent.
    
    - If specified, `LINE-PREFIX` is a string that's prepended to each
      line included in the documentation. For example, a string of four
      spaces makes Markdown think it's a code block.
    
    - `HEADER` and `FOOTER`, if non-`NIL`, are printed before the included
      string.
    
    - `HEADER-NL` and `FOOTER-NL`, if non-`NIL`, are printed between two
      [`FRESH-LINE`][3808] calls.
    
    `INCLUDE` is not [`EXPORTABLE-LOCATIVE-TYPE-P`][c930], and `INCLUDE` references do
    not [`RESOLVE`][63b4].

<a id="x-28MGL-PAX-3ADOCSTRING-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **DOCSTRING**

    `DOCSTRING` is a [`PSEUDO`][943a] locative for including the parse tree of
    the Markdown [`DOCSTRING`][affc] of a definition in the parse tree
    of a docstring when generating documentation. It has no source
    location information and only works as an explicit link. This
    construct is intended to allow docstrings to live closer to their
    implementation, which typically involves a non-exported definition.
    
    ```common-lisp
    (defun div2 (x)
      "X must be [even* type][docstring]."
      (/ x 2))
    
    (deftype even* ()
      "an even integer"
      '(satisfies evenp))
    
    (document #'div2)
    .. - [function] DIV2 X
    ..
    ..     X must be an even integer.
    ..
    ```
    
    There is no way to [`LOCATE`][8f19] `DOCSTRING`s, so nothing to [`RESOLVE`][63b4] either.

<a id="x-28GO-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **GO** *(NAME LOCATIVE)*

    Redirect to a definition in the context of the [reference][43bd]
    designated by `NAME` and `LOCATIVE`. This [`PSEUDO`][943a] locative is intended for
    things that have no explicit global definition.
    
    As an example, consider this part of a hypothetical documentation of
    CLOS:
    
        (defsection @clos ()
          (defmethod macro)
          (call-next-method (go (defmethod macro))))
    
    The `GO` reference exports the symbol [`CALL-NEXT-METHOD`][6832] and also
    produces a terse redirection message in the documentation.
    
    `GO` behaves as described below.
    
    - A `GO` reference [`RESOLVE`][63b4]s to what `NAME` with `LOCATIVE` resolves to:
    
        ```common-lisp
        (resolve (dref 'xxx '(go (print function))))
        ==> #<FUNCTION PRINT>
        => T
        ```
    
    - The [`DOCSTRING`][affc] of a `GO` reference is `NIL`.
    
    - [`SOURCE-LOCATION`][32da] (thus `M-.`) returns the source location of the
      embedded reference:
    
        ```common-lisp
        (equal (source-location (dref 'xxx '(go (print function))))
               (source-location (dref 'print 'function)))
        => T
        ```

<a id="x-28MGL-PAX-3ACLHS-20MGL-PAX-3ALOCATIVE-29"></a>

- [locative] **CLHS** *&OPTIONAL NESTED-LOCATIVE*

    Refers to definitions, glossary entries, sections, issues and
    issue summaries in the Common Lisp HyperSpec. These have no source
    location so [`M-.`][cb15] will not work. What works is linking in
    documentation, including [Browsing Live Documentation][a595]. The generated
    links are relative to [`*DOCUMENT-HYPERSPEC-ROOT*`][f585] and work even if
    [`*DOCUMENT-LINK-TO-HYPERSPEC*`][875e] is `NIL`. All matching is
    case-insensitive.
    
    - *definitions*: These are typically unnecessary as [`DOCUMENT`][432c] will
      produce the same link for e.g. `PPRINT`, `[PPRINT][function]`,
      or `[PPRINT][]` if `*DOCUMENT-LINK-TO-HYPERSPEC*` is non-`NIL` and the
      [`PPRINT`][6af6] function in the running Lisp is not [linkable][7eb5]. When
      [Browsing Live Documentation][a595], a slight difference is that
      everything is linkable, so using the `CLHS` link bypasses the page
      with the definition in the running Lisp.
    
        - *unambiguous definition*: `[pprint][clhs]` ([pprint][6af6])
    
        - *disambiguation page*: `[function][clhs]` ([function][aeb6])
    
        - *specific*: `[function][(clhs class)]` ([function][119e])
    
    - *glossary terms*:
    
        - `[lambda list][(clhs glossary-term)]`
          ([lambda list][98ff])
    
    - *issues*:
    
        - `[ISSUE:AREF-1D][clhs]` ([ISSUE:AREF-1D][63ef])
    
        - `[ISSUE:AREF-1D][(clhs section)]` ([ISSUE:AREF-1D][63ef])
    
    - *issue summaries*: These render
       as ([SUMMARY:CHARACTER-PROPOSAL:2-6-5][935f]):
    
        - `[SUMMARY:CHARACTER-PROPOSAL:2-6-5][clhs]`
    
        - `[SUMMARY:CHARACTER-PROPOSAL:2-6-5][(clhs section)]`
    
        Since these summary ids are not particularly reader friendly,
        the anchor text a [Specific Reflink with Text][fb17] may be used:
    
        - `[see this][SUMMARY:CHARACTER-PROPOSAL:2-6-5 (clhs section)]`
          ([see this][935f]).
    
    - *sections*:
    
        - *by section number*: `[3.4][clhs]` or `[3.4][(clhs
          section)]` ([3.4][e442])
    
        - *by section title*: With the locative `(CLHS SECTION)`,
          substring matching against the title starting at word
          boundaries is performed. With the locative [`CLHS`][ed5f] (where
          `SECTION` is not specified explicitly), the name must match
          the title exactly. For example, `[lambda list][(clhs
          section)]` refers to the same definition as `[lambda
          lists][clhs]` ([Lambda Lists][e442]).
    
        - *by filename*: `[03_d][clhs]` or `[03_d][(clhs
          section)]` ([03\_d][e442])
    
        - *by alias*
    
            - [Format directives][d273] are aliases of
              the sections describing them. Thus, `[~c][clhs]` is
              equivalent to `[22.3.1.1][clhs]` and `[Tilde C:
              Character][clhs]`. The full list is
              [~C][0cac] [~%][7bd6] [~\&][0684] [~|][3fa1]
            [~~][65bc] [~R][9927] [~D][3e6e] [~B][6897]
            [~O][76df] [~X][f9fa] [~F][cae2] [~E][1567]
            [~G][76ab] [~\$][5b4d] [~A][f275] [~S][b39f]
            [~W][e6d3] [~\_][31c5] [~\<][ed9f] [~:>][ed9f]
            [~I][1959] [~/][db38] [~T][2352] [~\< Justification][11f1]
            [~>][bf38] [~\*][bdd6] [~\[][76ea] [~\]][945b]
            [~{][550b] [~}][886c] [~?][0db0] [~(][ac30]
            [~)][051b] [~P][60e4] [~;][d296] [~^][02e3]
            [~Newline][0165].
    
            - Similarly, [reader macro][3972] characters are aliases of
              the sections describing them. The full list is
              [(][040b] [)][e43c] ['][8e92] [;][56ba]
            ["][cd66] [\`][309c] [,][826b] [#][698d]
            [\#\\][7b6f] [#'][8a5e] [#(][7a7f] [#\*][adf2]
            [\#:][ac5e] [#.][ffd7] [#B][c93e] [#O][68d2]
            [\#X][227d] [#R][2826] [#C][bfaa] [#A][7163]
            [\#S][dded] [#P][225d] [#=][fa43] [##][dd03]
            [\#+][d5e1] [#-][81b3] [#|][342d] [#\<][0f42]
            [\#)][9478].
    
            - Finally, [loop keywords][21f4] have
              aliases to the sections describing them. For example, the
              strings `loop:for`, `for` and `:for` are aliases of `CLHS`
              [`6.1.2.1`][142c]. The `loop:*` aliases are convenient for
              completion at the prompt when
              [Browsing Live Documentation][a595], while the other aliases are
              for defaulting to buffer contents.
    
    As the above examples show, the `NESTED-LOCATIVE` argument of the [`CLHS`][ed5f]
    locative may be omitted. In that case, definitions, glossary terms,
    issues, issue summaries, and sections are considered in that order.
    Sections are considered last because a substring of a section title
    can be matched by chance easily.
    
    All examples so far used [Reflink][cbc4]s. [Autolink][ec7a]ing also works if the
    [name][88cf] is marked up as code or is [codified][f1ab] (e.g. in
    `COS clhs` ([`COS`][c4a3] clhs).
    
    As mentioned above, `M-.` does not do anything over `CLHS`
    references. Slightly more usefully, the [live documentation
    browser][a595] understands `CLHS` links so one
    can enter inputs like `3.4 clhs`, `"lambda list" clhs` or
    `error (clhs function)`.
    
    `CLHS` references do not [`RESOLVE`][63b4].

<a id="x-28MGL-PAX-3A-40NAVIGATING-IN-EMACS-20MGL-PAX-3ASECTION-29"></a>

## 7 Navigating Sources in Emacs

Integration into [SLIME][6be7]'s [`M-.`][cb15] (`slime-edit-definition`) allows
one to visit the [`SOURCE-LOCATION`][32da] of a [definition][2143]. PAX extends
standard Slime functionality by

- adding support for all kinds of definitions (see e.g.
  [`ASDF:SYSTEM`][c097], [`READTABLE`][7506] in
  [Basic Locative Types][1d1d]), not just the ones Slime knows about,

- providing a portable way to refer to even standard definitions,

- disambiguating the definition based on buffer content, and

- adding more powerful completions.

The definition is either determined from the buffer content at point
or is prompted for. At the prompt, TAB-completion is available for
both names and locatives. With a prefix argument (`C-u M-.`), the
buffer contents are not consulted, and `M-.` always prompts.

The `M-.` extensions can be enabled by loading `src/mgl-pax.el`. See
[Emacs Setup][8541]. In addition, the Elisp command
`mgl-pax-edit-parent-section` visits the source location of the
section containing the definition with point in it.

A close relative of `M-.` is `C-.` for [Browsing Live Documentation][a595].

<a id="x-28MGL-PAX-3A-40M--2E-DEFAULTING-20MGL-PAX-3ASECTION-29"></a>

### 7.1 `M-.` Defaulting

When [`M-.`][cb15] is invoked, it first tries to find a [name][88cf] in the
current buffer at point. If no name is found, then it
[prompts][ed46].

First, `(slime-sexp-at-point)` is taken as a [word][d7b0], from which the
[name][88cf] will be [parsed][378f]. Then, candidate locatives are
looked for before and after the [word][d7b0]. Thus, if a locative is the
previous or the next expression, then `M-.` will go straight to the
definition which corresponds to the locative. If that fails, `M-.`
will try to find the definitions in the normal way, which may
involve popping up an xref buffer and letting the user interactively
select one of possible definitions.

`M-.` works on parenthesized references, such as those in
[`DEFSECTION`][72b4]:

```
(defsection @foo ()
  (cos function))
```

Here, when the cursor is on one of the characters of `COS` or just
after `COS`, pressing `M-.` will visit the definition of the
function [`COS`][c4a3].

To play nice with [Generating Documentation][2c93], forms suitable for
[Autolink][ec7a]ing are recognized:

    function cos
    cos function

... as well as [Reflink][cbc4]s:

    [cos][function]
    [see this][cos function]

... and [Markdown inline code][68c1]:

    cos `function`
    `cos` function
    `cos` `function`

Everything works the same way in comments and docstrings as in code.
In the next example, pressing `M-.` on [`RESOLVE*`][d3b3] will visit its
denoted method:

```
;;; See RESOLVE* (method (function-dref)) for how this all works.
```


<a id="x-28MGL-PAX-3A-40M--2E-PROMPTING-20MGL-PAX-3ASECTION-29"></a>

### 7.2 `M-.` Prompting

<a id="x-28MGL-PAX-3A-40M--2E-MINIBUFFER-SYNTAX-20MGL-PAX-3ASECTION-29"></a>

#### 7.2.1 `M-.` Minibuffer Syntax

At the minibuffer prompt, the [definition][2143]s to edit
can be specified as follows.

- `NAME`: Refers to all [`DREF:DEFINITIONS`][e196] of `NAME` with a [Lisp locative
  type][30ad]. See these `NAME -> DEFINITIONS`
  examples:

        print    ->  PRINT FUNCTION
        PRINT    ->  PRINT FUNCTION
        MGL-PAX  ->  "mgl-pax" ASDF:SYSTEM, "MGL-PAX" package
        pax      ->  "PAX" PACKAGE
        "PAX"    ->  "PAX" PACKAGE

    Note that depending on the Lisp implementation there may be more
    definitions. For example, SBCL has an [`UNKNOWN`][a951]
    `:DEFOPTIMIZER` definition for `PRINT`.

- `NAME` `LOCATIVE`: Refers to a single definition (as in `(DREF:DREF
  NAME LOCATIVE)`). Example inputs of this form:

        print function
        dref-ext:docstring* (method (t))

- `LOCATIVE` `NAME`: This has the same form as the previous: two sexps,
  but here the first one is the locative. If ambiguous, this is
  considered in addition to the previous one. Example inputs:

        function print
        (method (t)) dref-ext:docstring*

In all of the above `NAME` is a [raw name][f5af], meaning that `print`
will be recognized as `PRINT` and `pax` as `"PAX"`.

The package in which symbols are read is the Elisp
`slime-current-package`. In Lisp buffers, this is the buffer's
package, else it's the package of the Slime repl buffer.

<a id="x-28MGL-PAX-3A-40M--2E-COMPLETION-20MGL-PAX-3ASECTION-29"></a>

#### 7.2.2 `M-.` Completion

When `M-.` prompts for the definition to edit, TAB-completion is
available in the minibuffer for both names and locatives. To reduce
clutter, string names are completed only if they are typed
explicitly with an opening quotation mark, and they are
case-sensitive. Examples:

- `pri<TAB>` invokes the usual Slime completion.

- `print <TAB>` (note the space) lists `FUNCTION`([`0`][119e] [`1`][81f7]) and (`PAX:CLHS`
  [`FUNCTION`][aeb6]) as locatives.

- `class dref:<TAB>` lists `DREF:XREF`([`0`][1538] [`1`][cda7]) and `DREF:DREF`([`0`][d930] [`1`][7e92]) (all the classes
  in the package `DREF`).

- `pax:locative <TAB>` lists all [locative type][a11d]s (see the CL
  function [`DREF:LOCATIVE-TYPES`][99b0]).

- `package "MGL<TAB>` lists the names of packages that start with
  `"MGL"`.

- `package <TAB>` lists the names of all packages as strings and
   also [`CLASS`][1f37], `MGL-PAX:LOCATIVE` because [`PACKAGE`][1d5a] denotes a class and
   also a locative.

For more powerful search, see [Apropos][b7fc].

<a id="x-28MGL-PAX-3A-40GENERATING-DOCUMENTATION-20MGL-PAX-3ASECTION-29"></a>

## 8 Generating Documentation

<a id="x-28MGL-PAX-3A-40DOCUMENT-FUNCTION-20MGL-PAX-3ASECTION-29"></a>

### 8.1 The `DOCUMENT` Function

<a id="x-28MGL-PAX-3ADOCUMENT-20FUNCTION-29"></a>

- [function] **DOCUMENT** *DOCUMENTABLE &KEY (STREAM T) PAGES (FORMAT :PLAIN)*

    Write `DOCUMENTABLE` in `FORMAT` to `STREAM` diverting some output to `PAGES`.
    `FORMAT` is one of [`:PLAIN`][c879],
    [`:MARKDOWN`][dd29], [`:HTML`][36e1] and
    [`:PDF`][19ad] or [`NIL`][f7e6]. `STREAM` may be a
    [`STREAM`][d5a9] object, `T` or `NIL` as with [`CL:FORMAT`][ad78].
    
    To look up the documentation of the [`DOCUMENT`][432c] function itself:
    
        (document #'document)
    
    The same with fancy markup:
    
        (document #'document :format :markdown)
    
    To document a [`SECTION`][5fac]:
    
        (document pax::@pax-manual)
    
    To generate the documentation for separate libraries with automatic
    cross-links:
    
        (document (list pax::@pax-manual dref::@dref-manual) :format :markdown)
    
    See [Utilities for Generating Documentation][1b1b] for more.
    
    Definitions that do not define a first-class object are supported
    via [DRef][5225]:
    
        (document (dref:locate 'foo 'type))
    
    There are quite a few special variables that affect how output is
    generated, see [Codification][f1ab], [Linking to the HyperSpec][7cc3],
    [Linking to Sections][22c2], [Link Format][c0d2] and [Output Formats][8d9b].
    
    For the details, see the following sections, starting with
    [`DOCUMENTABLE`][0702]. Also see [Writing Extensions][c4ce] and [`DOCUMENT-OBJECT*`][8269].

<a id="x-28MGL-PAX-3A-40DOCUMENTABLE-20MGL-PAX-3ASECTION-29"></a>

#### 8.1.1 `DOCUMENTABLE`

The `DOCUMENTABLE` argument of [`DOCUMENT`][432c] may be a single object (e.g.
`#'PRINT`'), a [definition][2143] such as `(DREF 'PRINT 'FUNCTION)`,
a string, or a nested list of these. More precisely, `DOCUMENTABLE` is
one of the following:

- *single definition designator*: A [`DREF`][d930] or anything else
  that is [`LOCATE`][8f19]able. This includes non-`DREF` [`XREF`][1538]s and
  first-class objects such as [`FUNCTION`][119e]s. The generated
  documentation typically includes the definition's [`DOCSTRING`][affc]. See
  [Markdown Output][dd29] for more.

- *docstring*: A string, in which case it is processed like a
  docstring in [`DEFSECTION`][72b4]. That is, with [docstring sanitization][7bf5], [Codification][f1ab], and [Linking][19e3].

- *list of documentables*: A nested list of `LOCATE`able objects and
  docstrings. The objects in it are documented in depth-first order.
  The structure of the list is otherwise unimportant.


<a id="x-28MGL-PAX-3A-2ADOCUMENT-TIGHT-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-TIGHT\*** *NIL*

    If `NIL`, then [`DOCUMENT`][432c] adds a newline between consecutive
    [atomic][13c7] documentables on the same [page][9c7d].

<a id="x-28MGL-PAX-3A-40DOCUMENT-RETURN-20MGL-PAX-3ASECTION-29"></a>

#### 8.1.2 Return Values

If `PAGES` are `NIL`, then [`DOCUMENT`][432c] - like [`CL:FORMAT`][ad78] - returns a
string (when [`STREAM`][d5a9] is `NIL`) else `NIL`.

If `PAGES`, then a list of output designators are returned, one for
each non-empty page (to which some output has been written), which
are determined as follows.

- The string itself if the output was to a string.

- The stream if the output was to a stream.

- The pathname of the file if the output was to a file.

If the default page given by the `STREAM` argument of `DOCUMENT` was
written to, then its output designator is the first element of the
returned list. The rest of the designators correspond to the
non-empty pages in the `PAGES` argument of `DOCUMENT` in that order.

<a id="x-28MGL-PAX-3A-40PAGES-20MGL-PAX-3ASECTION-29"></a>

#### 8.1.3 `PAGES`

The `PAGES` argument of [`DOCUMENT`][432c] is to create multi-page documents
by routing some of the generated output to files, strings or
streams. `PAGES` is a list of page specification elements. A page spec
is a [property list][b18e] with keys `:OBJECTS`, `:OUTPUT`,
`:URI-FRAGMENT`, `:SOURCE-URI-FN`, `:HEADER-FN` and `:FOOTER-FN`. `OBJECTS` is
a list of objects (references are allowed but not required) whose
documentation is to be sent to `:OUTPUT`.

`PAGES` may look something like this:

```
`((;; The section about SECTIONs and everything below it ...
   :objects (, @sections)
   ;; ... is so boring that it's not worth the disk space, so
   ;; send it to a string.
   :output (nil)
   ;; Explicitly tell other pages not to link to these guys.
   :uri-fragment nil)
  ;; Send the @EXTENSION-API section and everything reachable
  ;; from it ...
  (:objects (, @extension-api)
   ;; ... to build/tmp/pax-extension-api.html.
   :output "build/tmp/pax-extension-api.html"
   ;; However, on the web server html files will be at this
   ;; location relative to some common root, so override the
   ;; default:
   :uri-fragment "doc/dev/pax-extension-api.html"
   ;; Set html page title, stylesheet, charset.
   :header-fn 'write-html-header
   ;; Just close the body.
   :footer-fn 'write-html-footer)
  ;; Catch references that were not reachable from the above. It
  ;; is important for this page spec to be last.
  (:objects (, @pax-manual)
   :output "build/tmp/manual.html"
   ;; Links from the extension api page to the manual page will
   ;; be to ../user/pax-manual#<anchor>, while links going to
   ;; the opposite direction will be to
   ;; ../dev/pax-extension-api.html#<anchor>.
   :uri-fragment "doc/user/pax-manual.html"
   :header-fn 'write-html-header
   :footer-fn 'write-html-footer))
```

Documentation is initially sent to a default stream (the `STREAM`
argument of `DOCUMENT`), but output is redirected if the thing being
currently documented is the `:OBJECT` of a `PAGE-SPEC`.

- `:OUTPUT` can be a number things:

    - If it's `NIL`, then output will be collected in a string.

    - If it's `T`, then output will be sent to [`*STANDARD-OUTPUT*`][e7ee].

    - If it's a stream, then output will be sent to that stream.

    - If it's a list whose first element is a string or a pathname, then
      output will be sent to the file denoted by that and the rest of
      the elements of the list are passed on to [`CL:OPEN`][6547]. One extra
      keyword argument is `:ENSURE-DIRECTORIES-EXIST`. If it's true,
      [`ENSURE-DIRECTORIES-EXIST`][876d] will be called on the pathname before
      it's opened.

    Note that even if `PAGES` is specified, `STREAM` acts as a catch all,
    absorbing the generated documentation for references not claimed by
    any pages.

- `:HEADER-FN`, if not `NIL`, is a function of a single stream argument,
  which is called just before the first write to the page. Since
  `:FORMAT` `:HTML` only generates HTML fragments, this makes it
  possible to print arbitrary headers, typically setting the title,
  CSS stylesheet, or charset.

- `:FOOTER-FN` is similar to `:HEADER-FN`, but it's called after the
   last write to the page. For HTML, it typically just closes the
   body.

- `:URI-FRAGMENT` is a string such as `"doc/manual.html"` that specifies
  where the page will be deployed on a webserver. It defines how
  links between pages will look. If it's not specified and `:OUTPUT`
  refers to a file, then it defaults to the name of the file. If
  `:URI-FRAGMENT` is `NIL`, then no links will be made to or from that
  page.

- `:SOURCE-URI-FN` is a function of a single, [`DREF`][d930] argument.
  If it returns a value other than `NIL`, then it must be a string
  representing an URI. This affects [`*DOCUMENT-MARK-UP-SIGNATURES*`][8fb6]
  and [`*DOCUMENT-FANCY-HTML-NAVIGATION*`][6ab0]. Also see
  [`MAKE-GIT-SOURCE-URI-FN`][587f].


<a id="x-28MGL-PAX-3A-40PACKAGE-AND-READTABLE-20MGL-PAX-3ASECTION-29"></a>

#### 8.1.4 Package and Readtable

While generating documentation, symbols may be read (e.g. from
docstrings) and printed. What values of [`*PACKAGE*`][5ed1] and [`*READTABLE*`][b79a]
are used is determined separately for each definition being
documented.

- If the values of `*PACKAGE*` and `*READTABLE*` in effect at the time
  of definition were captured (e.g. by [`DEFINE-LOCATIVE-TYPE`][b6c4] and
  [`DEFSECTION`][72b4]), then they are used.

- Else, if the definition has a [Home Section][bdd5] (see below), then the
  home section's [`SECTION-PACKAGE`][a5b1] and [`SECTION-READTABLE`][88a7] are used.

- Else, if the definition has an argument list, then the package of
  the first argument that's not external in any package is used.

- Else, if the definition is [name][5fc4]d by a symbol, then its
  [`SYMBOL-PACKAGE`][e5ab] is used, and `*READTABLE*` is set to the standard
  readtable `(NAMED-READTABLES:FIND-READTABLE :COMMON-LISP)`.

- Else, `*PACKAGE*` is set to the `CL-USER` package and `*READTABLE*` to
  the standard readtable.

The values thus determined come into effect after the name itself is
printed, for printing of the arglist and the docstring.

    CL-USER> (pax:document #'foo)
    - [function] FOO <!> X Y &KEY (ERRORP T)
    
        Do something with X and Y.

In the above, the `<!>` marks the place where `*PACKAGE*` and
`*READTABLE*` are bound.

<a id="x-28MGL-PAX-3A-40HOME-SECTION-20MGL-PAX-3ASECTION-29"></a>

##### Home Section

The home section of an object is a [`SECTION`][5fac] that contains the
object's definition in its [`SECTION-ENTRIES`][d850] or `NIL`. In the
overwhelming majority of cases there should be at most one
containing section.

If there are multiple containing sections, the following apply.

- If the [name][5fc4] of the definition is a non-keyword symbol, only
  those containing sections are considered whose package is closest
  to the [`SYMBOL-PACKAGE`][e5ab] of the name, where closest is defined as
  having the longest common prefix between the two [`PACKAGE-NAME`][db68]s.

- If there are multiple sections with equally long matches or the
  name is not a non-keyword symbol, then it's undefined which one is
  the home section.

For example, `(MGL-PAX:DOCUMENT FUNCTION)` is an entry in the
`MGL-PAX::@BASICS` section. Unless another section that contains
it is defined in the MGL-PAX package, the home section is guaranteed
to be `MGL-PAX::@BASICS` because the `SYMBOL-PACKAGE`s of
[`MGL-PAX:DOCUMENT`][432c] and `MGL-PAX::@BASICS` are the same (hence their
common prefix is maximally long).

This scheme would also work, for example, if the [home package][407c]
of `DOCUMENT` were `MGL-PAX/IMPL`, and it were reexported from
`MGL-PAX` because the only way to externally change the home package
would be to define a containing section in a package like
`MGL-PAX/IMP`.

Thus, relying on the package system makes it possible to find the
intended home section of a definition among multiple containing
sections with high probability. However, for names which are not
symbols, there is no package system to advantage of.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-NORMALIZE-PACKAGES-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-NORMALIZE-PACKAGES\*** *T*

    Whether to print `[in package <package-name>]` in the documentation
    when the package changes.

<a id="x-28MGL-PAX-3A-40BROWSING-LIVE-DOCUMENTATION-20MGL-PAX-3ASECTION-29"></a>

### 8.2 Browsing Live Documentation

Documentation for definitions in the running Lisp can be browsed
directly without generating documentation in the offline manner.
HTML documentation, complete with [Codification][f1ab] and [Linking][19e3], is
generated from docstrings of all kinds of definitions and PAX
[`SECTION`][5fac]s in the running Lisp on the fly. This allows ad-hoc
exploration of the Lisp, much like `describe-function`,
`apropos-command` and other online help commands in Emacs, for which
direct parallels are provided.

Still, even without Emacs and [SLIME][6be7], limited functionality can be
accessed through [PAX Live Home Page][9d50] by starting the live
documentation web server [manually][72cc].

If [Emacs Setup][8541] has been done, the Elisp function
`mgl-pax-document` (maybe bound to `C-.`) generates and displays
documentation as a single HTML page. If necessary, a disambiguation
page is generated with the documentation of all matching
definitions. For example, to view the documentation of this very
`SECTION`, one can do:

    M-x mgl-pax-document
    View Documentation of: pax::@browsing-live-documentation

Alternatively, pressing `C-.` with point over the text
`pax::@browsing-live-documentation` in a buffer achieves the same
effect.

In interactive use, `mgl-pax-document` behaves similarly to
[`M-.`][3386] except:

- It shows the [`DOCUMENT`][432c]ation of some definition and does not visit
  its [`SOURCE-LOCATION`][32da].

- It considers definitions with all [`LOCATIVE-TYPES`][99b0] not just
  [`LISP-LOCATIVE-TYPES`][30ad] because it doesn't need `SOURCE-LOCATION`.

    This also means that completion works for [`CLHS`][ed5f]
    definitions:

    - `"lambda list<TAB>` lists `"lambda list"` and `"lambda list
      keywords"`, both HyperSpec glossary entries. This is similar
      to `common-lisp-hyperspec-glossary-term` in Elisp but also
      works for HyperSpec section titles.

    - `"#<TAB>` lists all sharpsign reader macros (similar to
      `common-lisp-hyperspec-lookup-reader-macro` in Elisp).

    - `"~<TAB>` lists all [`CL:FORMAT`][ad78] directives (similar to
      `common-lisp-hyperspec-format` in Elisp).

    - `"loop:~<TAB>` lists all [loop keywords][e016].

- It works in non-`lisp-mode` buffers by reinterpreting a few lines
  of text surrounding point as lisp code (hence the suggested
  *global* binding).

- It supports fragment syntax at the prompt:

        NAME LOCATIVE FRAGMENT-NAME FRAGMENT-LOCATIVE

    This is like `NAME LOCATIVE`, but the browser scrolls to the
    definition of `FRAGMENT-NAME FRAGMENT-LOCATIVE` within that
    page.

    For example, entering this at the prompt will generate the
    entire PAX manual as a single page and scroll to the very
    section you are reading within it:

        pax::@pax-manual pax:section pax::@browsing-live-documentation pax:section

- If the empty string is entered at the prompt, and there is no
  existing w3m buffer or w3m is not used, then [PAX Live Home Page][9d50]
  is visited. If there is a w3m buffer, then entering the empty
  string displays that buffer.

The convenience function
`mgl-pax-current-definition-toggle-view` (`C-c C-d c`) documents the
definition with point in it.

<a id="x-28MGL-PAX-3A-40BROWSING-WITH-W3M-20MGL-PAX-3ASECTION-29"></a>

#### 8.2.1 Browsing with w3m

When the value of the Elisp variable `mgl-pax-browser-function`
is `w3m-browse-url` (see [Emacs Setup][8541]), the Emacs w3m browser is
used without the need for a web server, and also offering somewhat
tighter integration than [Browsing with Other Browsers][c434].

With [w3m's default key bindings][1743], moving the cursor between links involves
`TAB` and `S-TAB` (or `<up>` and `<down>`). `RET` and `<right>`
follow a link, while `B` and `<left>` go back in history.

In addition, the following PAX-specific key bindings are available:

- `M-.` visits the source location of the definition corresponding
  to the link under the point.

- Invoking `mgl-pax-document` on a section title link will show the
  documentation of that section on its own page.

- `n` moves to the next PAX definition on the page.

- `p` moves to the previous PAX definition on the page.

- `u` follows the first `Up:` link (to the first containing
  [`SECTION`][5fac]) if any.

- `U` is like `u` but positions the cursor at the top of the page.

- `v` visits the source location of the current definition (the one
  under the cursor or the first one above it).

- `V` visits the source location of the first definition on the
  page.


<a id="x-28MGL-PAX-3A-40BROWSING-WITH-OTHER-BROWSERS-20MGL-PAX-3ASECTION-29"></a>

#### 8.2.2 Browsing with Other Browsers

When the value of the Elisp variable `mgl-pax-browser-function`
is not `w3m-browse-url` (see [Emacs Setup][8541]), requests are served via
a web server started in the running Lisp, and documentation is most
likely displayed in a separate browser window.

By default, `mgl-pax-browser-function` is `nil`, which makes PAX use
`browse-url-browser-function`. You may want to customize the related
`browse-url-new-window-flag` or, for Chrome, set
`browse-url-chrome-arguments` to `("--new-window")`.

By default, `mgl-pax-web-server-port` is `nil`, and PAX will pick a
free port automatically.

In the browser, clicking on the locative on the left of the
name (e.g. in `- [function] PRINT`) will raise and focus the Emacs
window (if Emacs is not in text mode, and also subject to window
manager focus stealing settings), then go to the corresponding
source location. For sections, clicking on the lambda link will do
the same (see [`*DOCUMENT-FANCY-HTML-NAVIGATION*`][6ab0]).

Finally, note that the `URL`s exposed by the web server are subject to
change, and even the port used may vary by session if the Elisp
variable `mgl-pax-web-server-port` is nil.

<a id="x-28MGL-PAX-3A-2ABROWSE-HTML-STYLE-2A-20VARIABLE-29"></a>

- [variable] **\*BROWSE-HTML-STYLE\*** *:CHARTER*

    The HTML style to use for browsing live documentation. Affects only
    non-w3m browsers. See [`*DOCUMENT-HTML-DEFAULT-STYLE*`][90fa] for the possible
    values.
    
    If you change this variable, you may need to do a hard refresh in
    the browser (often `C-<f5>`).

<a id="x-28MGL-PAX-3A-40APROPOS-20MGL-PAX-3ASECTION-29"></a>

#### 8.2.3 Apropos

The Elisp functions `mgl-pax-apropos`, `mgl-pax-apropos-all`, and
`mgl-pax-apropos-package` can display the results of [`DREF-APROPOS`][65b4] in
the [live documentation browser][a595].
These extend the functionality of `slime-apropos`,
`slime-apropos-all` and `slime-apropos-package` to support more
kinds of [definition][2143]s in an extensible way. The correspondence
is so close that the PAX versions might [take over the Slime key
bindings][8541].

Note that apropos functionality is also exposed via the
[PAX Live Home Page][9d50].

More concretely, the PAX versions supports the following extensions:

- Definitions with string names. One can search for
  [`ASDF:SYSTEM`s][c097], [`PACKAGE`s][4dd7] and
  [`CLHS`][ed5f] sections, glossary entries, format directives,
  reader macro characters, loop keywords.

- Exact or substring matching of the name and the package.

- Matching only symbol or string names.

On the [PAX Live Home Page][9d50], one may [Browse by Locative Types][659d], which
gives access to some of the apropos functionality via the browser
without involving Emacs.

On the result page:

- A `DREF-APROPOS` form to reproduce the results at the REPL is shown.

- One may toggle the `EXTERNAL-ONLY` and `CASE-SENSITIVE` boolean
  arguments.

- One may switch between list, and detailed view. The list view only
  shows the first, [bulleted line][dd29] for each
  definition, while the detailed view includes the full
  documentation of definitions with the exception of [`SECTION`][5fac]s.

- The returned references are presented in two groups: those with
  non-symbol and those with symbol [name][88cf]s. The non-symbol group is
  sorted by locative type then by name. The symbol group is sorted
  by name then by locative type.

With `mgl-pax-apropos-all` and `mgl-pax-apropos-package` being
simple convenience functions on top of `mgl-pax-apropos`, we only
discuss the latter in detail here. For the others, see the Elisp
docstrings.

<a id="x-28MGL-PAX-3A-40APROPOS-STRING-ARGUMENT-20MGL-PAX-3ASECTION-29"></a>

##### The `STRING` Argument of `mgl-pax-apropos`

The `STRING` argument consists of a name pattern and a [`DTYPE`][a459].

The name pattern has the following forms.

- `:print` matches definitions whose names are the string `print`
  or a symbol with [`SYMBOL-NAME`][0d07] `print`. Vertical bar form as in
  `:|prInt|` is also also supported and is useful in when
  `CASE-SENSITIVE` is true.

- `"print"` matches definitions whose names contain `print` as
  a substring.

- `print` is like the previous, substring matching case. Use this
  form to save typing if the pattern does not contain spaces and
  does not start with a colon.

- The empty string matches everything.

After the name pattern, `STRING` may contain a
[`DTYPE`][a459] that the definitions must match.

- `print t` matches definitions with [`LISP-LOCATIVE-TYPES`][30ad], which is
  the default (equivalent to `print`).

- `print function` matches functions whose names contain
  `print` (e.g. [`CL:PRINT`][d451] and [`CL:PPRINT`][6af6]).

- `:print function` is like the previous example but with exact
  name match (so it matches `CL:PRINT` but not `CL:PPRINT`).

- `print variable` matches for example [`*PRINT-ESCAPE*`][ff76].

- `print (or variable function)` matches all variables and functions
  with `print` in their names.

- `array (or type (not class))` matches [`DEFTYPE`][7f9a]s and but not [`CLASS`][1f37]es
  with the string [`array`][1f99] in their names.

- &nbsp;`pax:section` (note the leading space) matches all PAX
  sections (`EXTERNAL-ONLY` `NIL` is necessary to see many of them).

- `print dref:pseudo` matches definitions with [`PSEUDO-LOCATIVE-TYPES`][c340]
  such as `MGL-PAX:CLHS`.

- `print dref:top` matches definitions with all locative
  types ([`LOCATIVE-TYPES`][99b0]).


<a id="x-28MGL-PAX-3A-40APROPOS-PACKAGE-ARGUMENT-20MGL-PAX-3ASECTION-29"></a>

##### The `PACKAGE` Argument of `mgl-pax-apropos`

When `mgl-pax-apropos` is invoked with a prefix argument, it
prompts for a package pattern among other things. The pattern may be
like the following examples.

- `:none` restricts matches to non-symbol names.

- `:any` restricts matches to symbol names.

- `:cl` restricts matches to symbols in the CL package.

- `:|X Y|` is similar to the previous, but the vertical bar syntax
  allows for spaces in names.

- `mgl` restricts matches to packages whose name contains `mgl` as a
  substring.

- `"x y"` is the same as the previous, but the explicit quotes allow
  for spaces in names.

The above examples assume case-insensitive matching.

<a id="x-28MGL-PAX-3A-40PAX-LIVE-HOME-PAGE-20MGL-PAX-3ASECTION-29"></a>

#### 8.2.4 PAX Live Home Page

When [Browsing Live Documentation][a595], the home page provides
quick access to documentation of the definitions in the system. In
Emacs, when `mgl-pax-document` is invoked with the empty string, it
visits the home page.

The home page may also be accessed directly by going to the root
page of the web server (if one is started). Here, unless the home
page is viewed [with w3m][83d5], one may directly look
up documentation and access [Apropos][b7fc] via the input boxes provided.

<a id="x-28MGL-PAX-3AENSURE-WEB-SERVER-20FUNCTION-29"></a>

- [function] **ENSURE-WEB-SERVER** *&KEY PORT HYPERSPEC-ROOT*

    Start or update a web server on `PORT` for [Browsing Live Documentation][a595].
    Returns the base `URL` of the server (e.g. `http://localhost:32790`),
    which goes to the [PAX Live Home Page][9d50]. If the web server is running
    already `(ENSURE-WEB-SERVER)` simply returns its base `URL`.
    
    Note that even when using Emacs but [Browsing with Other Browsers][c434],
    the web server is started automatically. When [Browsing with w3m][83d5], no
    web server is involved at all. Calling this function explicitly is
    only needed if the Emacs integration is not used, or to override
    `PORT` and `HYPERSPEC-ROOT`.
    
    - If `PORT` is `NIL` or 0, then the server will use any free port.
    
    - If there is a server already running and `PORT` is not `NIL` or 0,
      then the server is restarted on `PORT`.
    
    - If `HYPERSPEC-ROOT` is `NIL`, the HyperSpec pages will be served from
      any previously provided `HYPERSPEC-ROOT` or, failing that, from
      [`*DOCUMENT-HYPERSPEC-ROOT*`][f585].
    
    - If `HYPERSPEC-ROOT` is non-`NIL`, then pages in the HyperSpec will be
      served from `HYPERSPEC-ROOT`. The following command changes the root
      without affecting the server in any other way:
    
            (ensure-web-server :hyperspec-root "/usr/share/doc/hyperspec/")

<a id="x-28MGL-PAX-3A-40TOP-LEVEL-PAX-SECTIONS-20MGL-PAX-3ASECTION-29"></a>

##### Top-level PAX Sections

The [PAX Live Home Page][9d50] lists the top-level PAX sections: those
that have no other [`SECTION`][5fac]s referencing them (see [`DEFSECTION`][72b4]).

<a id="x-28MGL-PAX-3A-40ASDF-SYSTEMS-AND-RELATED-PACKAGES-20MGL-PAX-3ASECTION-29"></a>

##### `ASDF:SYSTEM`s and Related `PACKAGE`s

The [PAX Live Home Page][9d50] lists all `ASDF:SYSTEM`s and [`PACKAGE`][1d5a]s in the Lisp.
For easier overview, the they are grouped based on their
[`SOURCE-LOCATION`][32da]s. Two systems are in the same group if the directory
of one (i.e. the directory of the `.asd` file in which it was
defined) is the same or is below the other's.

A `PACKAGE` presented under a group of systems, if the `SOURCE-LOCATION`
of the package is below the the top-most directory among the systems
in the group.

<a id="x-28MGL-PAX-3A-40SYSTEMLESS-PACKAGES-20MGL-PAX-3ASECTION-29"></a>

##### Systemless Packages

The [PAX Live Home Page][9d50] lists [`PACKAGE`][1d5a]s
[unrelated][b033] to any `ASDF:SYSTEM`
as systemless.

<a id="x-28MGL-PAX-3A-40BROWSE-BY-LOCATIVE-TYPE-20MGL-PAX-3ASECTION-29"></a>

##### Browse by Locative Types

The [PAX Live Home Page][9d50] provides quick links to [Apropos][b7fc] result
pages for all [Basic Locative Types][1d1d] which may have
definitions.

<a id="x-28MGL-PAX-3A-40RELATED-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **related**

    Two definitions are *related* if the directory of one's
    [`SOURCE-LOCATION`][32da]s contains the directory of the other's.

<a id="x-28MGL-PAX-3A-40MARKDOWN-SUPPORT-20MGL-PAX-3ASECTION-29"></a>

### 8.3 Markdown Support

[Markdown][a317] in docstrings and titles is processed with the [3BMD][1904] library.

<a id="x-28MGL-PAX-3A-40MARKDOWN-IN-DOCSTRINGS-20MGL-PAX-3ASECTION-29"></a>

#### 8.3.1 Markdown in Docstrings

- Docstrings can be indented in any of the usual styles. PAX
  normalizes indentation by stripping the longest run of leading
  spaces common to all non-blank lines except the first. Thus, the
  following two docstrings are equivalent:

        (defun foo ()
          "This is
          indented
          differently")
        
        (defun foo ()
          "This is
        indented
        differently")

- When [Browsing Live Documentation][a595], the page displayed can be of,
say, a single function within what would constitute the offline
documentation of a library. Because Markdown reference link
definitions, for example

        [Daring Fireball]: http://daringfireball.net/

    can be defined anywhere, they wouldn't be resolvable in that
    case, their use is discouraged. Currently, only reflink
    definitions within the documentation of the same
    [definition][2143] are guaranteed to be resolvable. This is left
    intentionally vague because the specifics are subject to change.

    See [`DEFINE-GLOSSARY-TERM`][8ece] for a better alternative to Markdown
    reference links.

Docstrings of definitions which do not have a [Home Section][bdd5] and are
not PAX constructs themselves (e.g [`SECTION`][5fac], [`GLOSSARY-TERM`][8251], [`NOTE`][e2ae]) are
assumed to have been written with no knowledge of PAX and to conform
to Markdown only by accident. These docstrings are thus sanitized
more aggressively.

- Indentation of what looks like blocks of Lisp code is rounded up to
a multiple of 4. More precisely, non-zero indented lines between
blank lines or the docstring boundaries are reindented if the first
non-space character of the first line is an `(` or a `;` character.

- Special HTML characters `<&` are escaped.

- Furthermore, to reduce the chance of inadvertently introducing a
Markdown heading, if a line starts with a string of `#` characters,
then the first one is automatically escaped. Thus, the following two
docstrings are equivalent:

        The characters #\Space, #\Tab and
        #Return are in the whitespace group.
    
        The characters #\Space, #\Tab and
        \#Return are in the whitespace group.


<a id="x-28MGL-PAX-3A-40MARKDOWN-IN-TITLES-20MGL-PAX-3ASECTION-29"></a>

#### 8.3.2 Markdown in Titles

<a id="x-28MGL-PAX-3A-40TITLE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **title**

    A title is a `STRING`([`0`][b93c] [`1`][dae6]) associated with a [definition][2143] (e.g. with
    the `TITLE` argument of [`DEFSECTION`][72b4] or [`DEFINE-GLOSSARY-TERM`][8ece]). Titles
    are accessible [`DOCTITLE`][e619] and processed according to
    [Markdown in Titles][165c].

Titles undergo [Codification][f1ab] and may be a single paragraph
containing explicit [Markdown inline code][68c1], [Markdown emphasis][c624],
[Markdown image][d534]s, inline [MathJax][a17d] and HTML entities (e.g. `&quot;`).
Other kinds of Markdown markup and block elements are not allowed.

<a id="x-28MGL-PAX-3ADOCTITLE-20FUNCTION-29"></a>

- [function] **DOCTITLE** *OBJECT*

    Return the [title][090e] of `OBJECT` if it has one or `NIL`. For
    [Codification][f1ab], the title is interpreted in the package returned by
    [`DOCSTRING`][affc]. `DOCTITLE` can be extended via [`DOCTITLE*`][c34e].

<a id="x-28MGL-PAX-3A-40MARKDOWN-SYNTAX-HIGHLIGHTING-20MGL-PAX-3ASECTION-29"></a>

#### 8.3.3 Syntax Highlighting

For syntax highlighting, GitHub's [fenced code blocks][1322] Markdown
extension to mark up code blocks with triple backticks is enabled so
all you need to do is write:

    ```elisp
    (defun foo ())
    ```

to get syntactically marked up HTML output. Copy `src/style.css`
from PAX and you are set. The language tag, `elisp` in this example,
is optional and defaults to `common-lisp`.

See the documentation of [3BMD][1904] and [Colorize][3076] for the details.

<a id="x-28MGL-PAX-3A-40MATHJAX-20MGL-PAX-3ASECTION-29"></a>

#### 8.3.4 MathJax

Displaying pretty mathematics between in TeX format is
supported via MathJax.

- *Inline*

    It can be done inline (within a paragraph):

        Pretty, eh? $\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$ Yes.

    which is displayed as

    Pretty, eh? $\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$ Yes.

    To avoid rendering `between $5 and $6` with inline math, both
    the opening and the closing `$` character must be followed /
    preceded by a non-space character. This agrees with Pandoc.

    Alternatively, the ``$`x_0`$`` syntax may be used (renders as
    $`x_0`$), which has no restriction on spacing.

- *Block*

    The `$$` is supported as a block element:

        Pretty, eh?
        
        $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$
        
        Yes.

    which will be rendered in its own paragraph:

    Pretty, eh?

    $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

    Yes.

    `$$` is also legal to use inline, but it's not recommended as it
    gets rendered inline on GitHub but as display math in
    [PDF Output][19ad].

MathJax will leave inline code (e.g. those between single backticks)
and code blocks (triple backtricks) alone. Outside code, use
`<span>$</span>` to scare MathJax off.

Escaping all those backslashes in TeX fragments embedded in Lisp
strings can be a pain. [Pythonic String Reader][d3fc5] can help with that.

<a id="x-28MGL-PAX-3A-40CODIFICATION-20MGL-PAX-3ASECTION-29"></a>

### 8.4 Codification

<a id="x-28MGL-PAX-3A-2ADOCUMENT-UPPERCASE-IS-CODE-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-UPPERCASE-IS-CODE\*** *T*

    When true, [interesting][7445] [name][88cf]s extracted from [codifiable][b89a] [word][d7b0]s
    marked up as code with backticks. For example, this docstring
    
        "T PRINT CLASSes SECTION *PACKAGE* MGL-PAX ASDF
        CaMeL Capital"
    
    is equivalent to this:
    
        "`T` `PRINT` `CLASS`es `SECTION` `*PACKAGE*` `MGL-PAX` `ASDF`
        CaMel Capital"
    
    and renders as
    
    `T` [`PRINT`][d451] [`CLASS`][1f37]es [`SECTION`][5fac] `MGL-PAX` `ASDF` CaMel Capital
    
    where the links are added due to [`*DOCUMENT-LINK-CODE*`][d9ee].
    
    To suppress codification, add a backslash to the beginning of the
    a [codifiable][b89a] word or right after the leading `*` if it would
    otherwise be parsed as Markdown emphasis:
    
        "\\SECTION *\\PACKAGE*"
    
    The number of backslashes is doubled above because that's how the
    example looks in a docstring. Note that the backslash is discarded
    even if `*DOCUMENT-UPPERCASE-IS-CODE*` is false.

<a id="x-28MGL-PAX-3A-40CODIFIABLE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **codifiable**

    A [word][d7b0] is *codifiable* if
    
    - it has a single uppercase character (e.g. it's `T`) and no
      lowercase characters at all, or
    
    - there is more than one uppercase character and no lowercase
      characters between them (e.g. `CLASSes`, `nonREADable`,
      `CLASS-NAMEs` but not `Classes` or `aTe`.

<a id="x-28MGL-PAX-3A-40INTERESTING-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **interesting**

    A [name][88cf] is *interesting* if
    
    - it names a symbol external to its package, or
    
    - it is at least 3 characters long and names an interned symbol, or
    
    - it names a [Local Definition][9db9].
    
    See [Package and Readtable][ab7e].

<a id="x-28MGL-PAX-3A-2ADOCUMENT-DOWNCASE-UPPERCASE-CODE-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-DOWNCASE-UPPERCASE-CODE\*** *NIL*

    If true, then all [Markdown inline code][68c1] (e.g. \`code\`, *which
    renders as* `code`) – including [Codification][f1ab] – which has no
    lowercase characters is downcased in the output. Characters of
    literal strings in the code may be of any case. If this variable is
    `:ONLY-IN-MARKUP` and the output format does not support markup (e.g.
    it's `:PLAIN`), then no downcasing is performed. For example,
    
        `(PRINT "Hello")`
    
    is downcased to
    
        `(print "Hello")`
    
    because it only contains uppercase characters outside the string.
    However,
    
        `MiXed "RESULTS"`
    
    is not altered because it has lowercase characters.
    
    If the first two characters are backslashes, then no downcasing is
    performed, in addition to [Escaping Autolinking][a270]. Use this to mark
    inline code that's not Lisp.
    
        Press `\\M-.` in Emacs.

<a id="x-28MGL-PAX-3A-40LINKING-20MGL-PAX-3ASECTION-29"></a>

### 8.5 Linking

PAX supports linking to [definition][2143]s either with
explicit [Reflink][cbc4]s or with [Autolink][ec7a]s.

When generating offline documentation, only the definitions in
[`DOCUMENTABLE`][0702] may be [linkable][7eb5], but when
[Browsing Live Documentation][a595], everything is linkable as
documentation is generated on-demand.

Many examples in this section link to standard Common Lisp
definitions. In the offline case, these will link to [external
URLs][f585], while in the live case to
disambiguation pages that list the definition in the running Lisp
and in the HyperSpec.

*Invoking [`M-.`][3386] on `WORD` or `NAME` in
any of the following examples will disambiguate based on the textual
context, determining the locative.* This is because navigation and
linking use the same [Parsing][378f] algorithm, although linking is a bit
more strict about trimming, depluralization, and it performs
[Filtering Links][b2e4]. On the other hand, `M-.` cannot visit the
[`CLHS`][ed5f] references because there are no associated source
locations.

<a id="x-28MGL-PAX-3A-40STABLE-PRINTED-LOCATIVE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **stable printed locative**

    The [Link Format][c0d2] relies on [definition][2143]s having a unique
    textual representation that doesn't change. More concretely, if
    [`PRIN1`][6384] under [`WITH-STANDARD-IO-SYNTAX`][39df] but with [`*PRINT-READABLY*`][8aca] `NIL`
    produces the same unique string deterministically, then linking to
    [definition][2143]s works even with non-readable locatives. The
    uniqueness condition requires that if two definitions are different
    under [`XREF=`][0617], then their textual representations are also different.
    
    On the other hand, for example, a method involving an `EQL`([`0`][db03] [`1`][5fd4])
    specializer with an object printed with [`PRINT-UNREADABLE-OBJECT`][9439]
    `:IDENTITY` `T` does not produce a stable string and links will break.

<a id="x-28MGL-PAX-3A-40REFLINK-20MGL-PAX-3ASECTION-29"></a>

#### 8.5.1 Reflink

The [Markdown reference link][8c00] syntax `[label][id]` is
repurposed for linking to [definition][2143]s. In the following, we
discuss the various forms of reflinks.

<a id="x-28MGL-PAX-3A-40SPECIFIC-REFLINK-20MGL-PAX-3ASECTION-29"></a>

##### Specific Reflink

*Format:* `[` [`WORD`][d7b0] `][` [`LOCATIVE`][0b3a] `]`

The first [name][88cf] in `WORD` (with depluralization) that forms a valid
[`DREF`][d930] with `LOCATIVE` is determined, and that definition is
linked to. If there is no such `DREF`, then an [`UNRESOLVABLE-REFLINK`][64be]
warning is signalled.

*Examples:*

- ``[`EQL`][type]`` *renders as* [`EQL`][5fd4].

- `[EQL][type]` *renders as* [`EQL`][5fd4].

The Markdown link definition (i.e. [`type`][7c9f] above) needs no backticks
to mark it as code, but here and below, the second example relies on
[`*DOCUMENT-UPPERCASE-IS-CODE*`][f25f] being true.

<a id="x-28MGL-PAX-3A-40SPECIFIC-REFLINK-WITH-TEXT-20MGL-PAX-3ASECTION-29"></a>

##### Specific Reflink with Text

*Format:* `[LINK TEXT][` [`NAME`][88cf] [`LOCATIVE`][0b3a] `]`

If `NAME` and `LOCATIVE` form a valid [`DREF`][d930], then that
definition is linked to with link text `LINK TEXT`. If there is no
such `DREF`, then an [`UNRESOLVABLE-REFLINK`][64be] warning is signalled.

In this form, if `NAME` starts with `#\"`, then it's read as a string,
else as a symbol.

*Examples:*

- `[see this][eql type]` *renders as* [see this][5fd4].

- `[see this]["MGL-PAX" package]` *renders as* see this.


<a id="x-28MGL-PAX-3A-40UNSPECIFIC-REFLINK-20MGL-PAX-3ASECTION-29"></a>

##### Unspecific Reflink

*Format:* `[` [`WORD`][d7b0] `][]`

The first [name][88cf] in `WORD` (with depluralization, symbols only) that
has some [`DEFINITIONS`][e196] is determined, and those definitions are linked
to. If no [name][88cf] with any definition is found, then an
[`UNRESOLVABLE-REFLINK`][64be] warning is signalled.

*Examples:*

- single link: `[PRINT][]` *renders as* [`PRINT`][d451].

- multiple links: `[EQL][]` *renders as* `EQL`([`0`][db03] [`1`][5fd4]).

- no definitions: `[BAD-NAME][]` *renders as* BAD-NAME.


<a id="x-28MGL-PAX-3A-40UNSPECIFIC-REFLINK-WITH-TEXT-20MGL-PAX-3ASECTION-29"></a>

##### Unspecific Reflink with Text

*Format:* `[LINK TEXT][` [`NAME`][88cf] `]`

The [`DEFINITIONS`][e196] of `NAME` are determined, and those definitions are
linked to. If `NAME` has no definitions, then an [`UNRESOLVABLE-REFLINK`][64be]
warning is signalled.

In this form, if `NAME` starts with `#\"`, then it's read as a string,
else as as symbol.

*Examples:*

- `[see this][print]` *renders as* [see this][d451].

- `[see this][eql]` *renders as* see this([`0`][db03] [`1`][5fd4]).


<a id="x-28MGL-PAX-3A-40MARKDOWN-REFLINK-20MGL-PAX-3ASECTION-29"></a>

##### Markdown Reflink

*Format:* `[label][id]`

This is a normal [Markdown reference link][8c00] if `id` is not a valid locative.

- `[see this][user-defined]` renders unchanged.

    ```common-lisp
    (dref:dref 'user-defined 'locative)
    .. debugger invoked on LOCATE-ERROR:
    ..   Could not locate USER-DEFINED LOCATIVE.
    ..   USER-DEFINED is not a valid locative type or locative alias.
    ```

    ```common-lisp
    (document "[see this][user-defined]" :format :markdown)
    .. [see this][user-defined]
    ..
    ```

Use `URL`s with [`DEFINE-GLOSSARY-TERM`][8ece] as a better alternative to
Markdown reference links (see [Markdown in Docstrings][7bf5]).

<a id="x-28MGL-PAX-3A-40UNRESOLVABLE-REFLINKS-20MGL-PAX-3ASECTION-29"></a>

##### Unresolvable Links

<a id="x-28MGL-PAX-3AUNRESOLVABLE-REFLINK-20CONDITION-29"></a>

- [condition] **UNRESOLVABLE-REFLINK** *[WARNING][bcb6]*

    When [`DOCUMENT`][432c] encounters a [Reflink][cbc4] that looks
    like a PAX construct but has no matching definition, it signals an
    `UNRESOLVABLE-REFLINK` warning.
    
    - If the [`OUTPUT-REFLINK`][2ca9] restart is invoked, then no warning is
      printed and the Markdown link is left unchanged. `MUFFLE-WARNING`([`0`][b8b4] [`1`][6f51]) is
      equivalent to `OUTPUT-REFLINK`.
    
    - If the [`OUTPUT-LABEL`][c818] restart is invoked, then no warning is printed
      and the Markdown link is replaced by its label. For example,
      `[NONEXISTENT][function]` becomes `NONEXISTENT`.
    
    - If the warning is not handled, then it is printed to
      [`*ERROR-OUTPUT*`][66c6], and it behaves as if `OUTPUT-LABEL` was invoked.

<a id="x-28MGL-PAX-3AOUTPUT-REFLINK-20FUNCTION-29"></a>

- [function] **OUTPUT-REFLINK** *&OPTIONAL CONDITION*

    Invoke the `OUTPUT-REFLINK` restart. See [`UNRESOLVABLE-REFLINK`][64be].

<a id="x-28MGL-PAX-3AOUTPUT-LABEL-20FUNCTION-29"></a>

- [function] **OUTPUT-LABEL** *&OPTIONAL CONDITION*

    Invoke the `OUTPUT-LABEL` restart. See [`UNRESOLVABLE-REFLINK`][64be].

<a id="x-28MGL-PAX-3A-40AUTOLINK-20MGL-PAX-3ASECTION-29"></a>

#### 8.5.2 Autolink

[Markdown inline code][68c1] automatically links to the corresponding
definitions without having to use [Reflink][cbc4]s. This works especially
well in conjunction with [Codification][f1ab]. The following examples
assume that [`*DOCUMENT-UPPERCASE-IS-CODE*`][f25f] is true. If that's not the
case, explicit backticks are required on [`WORD`][d7b0] (but not on
`LOCATIVE`).

<a id="x-28MGL-PAX-3A-40SPECIFIC-AUTOLINK-20MGL-PAX-3ASECTION-29"></a>

##### Specific Autolink

*Format:* [`WORD`][d7b0] [`LOCATIVE`][0b3a] or
[`LOCATIVE`][0b3a] [`WORD`][d7b0]

The first [name][88cf] in `WORD` (with depluralization) that forms a valid
[`DREF`][d930] with `LOCATIVE` is determined, and that definition is
linked to. If no such name is found, then [Unspecific Autolink][e2a4] is
attempted.

*Examples:*

- `PRINT function` *renders as* [`PRINT`][d451] function.

- `type EQL` *renders as* type [`EQL`][5fd4].

- `type EQL function` *renders as* type [`EQL`][db03] function.

If `LOCATIVE` has spaces, then it needs to be marked up as code, too.
For example,

    DREF-NAME `(reader dref)`

*renders as* [`DREF-NAME`][1e36] `(reader dref)`.

<a id="x-28MGL-PAX-3A-40UNSPECIFIC-AUTOLINK-20MGL-PAX-3ASECTION-29"></a>

##### Unspecific Autolink

*Format:* [`WORD`][d7b0]

The first [name][88cf] in `WORD` (with depluralization, symbols only) that
has some [`DEFINITIONS`][e196] is determined, and those definitions are linked
to. If no such name is found or the autolink to this name is
*suppressed* (see below), then `WORD` is left unchanged. If a locative
is found before or after `WORD`, then [Specific Autolink][38de] is tried
first.

*Examples:*

- `PRINT` *renders as* [`PRINT`][d451].

- `EQL` *renders as* `EQL`([`0`][db03] [`1`][5fd4]).

[Unspecific Autolink][e2a4]ing is suppressed if the name found has a
[Local Definition][9db9] or was linked to before in the same docstring:

- "`My other CAR is also a CAR`" *renders as* "My other [`CAR`][d5a2] is also a
  `CAR`".

- "`[COS][] and COS`" *renders as* "[`COS`][c4a3] and `COS`".

- "`[EQL][type] and EQL`" *renders as* "[`EQL`][5fd4] and `EQL`".

- "`EQ and the EQ function`" *renders as* "[`EQ`][5a82] and the [`EQ`][5a82] function".

[Unspecific Autolink][e2a4]ing to `T` and `NIL` is also suppressed (see
[`*DOCUMENT-LINK-TO-HYPERSPEC*`][875e]):

- "`T and NIL`" *renders as* "`T` and `NIL`".

As an exception, a single link (be it either a [Specific Link][0361] or an
unambiguous [Unspecific Link][8e71]) to a [`SECTION`][5fac] or [`GLOSSARY-TERM`][8251] is not
suppressed to allow their titles to be displayed properly:

- "`@NAME and @NAME`" *renders as* "[name][88cf] and [name][88cf]".


<a id="x-28MGL-PAX-3A-40ESCAPING-AUTOLINKING-20MGL-PAX-3ASECTION-29"></a>

##### Escaping Autolinking

In the common case, when [`*DOCUMENT-UPPERCASE-IS-CODE*`][f25f] is true,
prefixing an uppercase [word][d7b0] with a backslash prevents it from being
codified and thus also prevents [Autolink][ec7a]ing form kicking in. For
example,

    \DOCUMENT

renders as DOCUMENT. If it should be marked up as code but not
autolinked, the backslash must be within backticks like this:

    `\DOCUMENT`

This renders as `DOCUMENT`. Alternatively, the
[`DISLOCATED`][e391] or the [`ARGUMENT`][8710] locative may be used as in
`[DOCUMENT][dislocated]`.

<a id="x-28MGL-PAX-3A-40LINKING-TO-THE-HYPERSPEC-20MGL-PAX-3ASECTION-29"></a>

#### 8.5.3 Linking to the HyperSpec

<a id="x-28MGL-PAX-3A-2ADOCUMENT-LINK-TO-HYPERSPEC-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-LINK-TO-HYPERSPEC\*** *T*

    If true, consider definitions found in the Common Lisp HyperSpec
    for linking. For example,
    
    - `PRINT` *renders as* [`PRINT`][d451].
    
    In offline documentation, this would be a link to the hyperspec
    unless `#'PRINT` in the running Lisp is [`DOCUMENTABLE`][0702].
    
    When [Browsing Live Documentation][a595], everything is [linkable][7eb5], so the
    generated link will go to a disambiguation page that lists the
    definition in the Lisp and in the HyperSpec.
    
    Locatives work as expected (see [`*DOCUMENT-LINK-CODE*`][d9ee]): `FIND-IF`
    links to [`FIND-IF`][5884], `FUNCTION` links to `FUNCTION`([`0`][119e] [`1`][81f7]), and
    `[FUNCTION][type]` links to [`FUNCTION`][119e].
    
    [Unspecific Autolink][e2a4]ing to `T` and `NIL` is suppressed. If desired, use
    [Reflink][cbc4]s such as `[T][]` (that links to `T`([`0`][9172] [`1`][fe21])) or
    `[T][constant]` (that links to [`T`][fe21]).
    
    Note that linking explicitly with the [`CLHS`][ed5f] locative is not subject
    to the value of this variable.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HYPERSPEC-ROOT-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-HYPERSPEC-ROOT\*** *"http://www.lispworks.com/documentation/HyperSpec/"*

    A URL of the Common Lisp HyperSpec.
    The default value is the canonical location. When [invoked from
    Emacs][a595], the Elisp variable
    `common-lisp-hyperspec-root` is in effect.

<a id="x-28MGL-PAX-3A-40LINKING-TO-SECTIONS-20MGL-PAX-3ASECTION-29"></a>

#### 8.5.4 Linking to Sections

The following variables control how to generate section numbering,
table of contents and navigation links.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-LINK-SECTIONS-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-LINK-SECTIONS\*** *T*

    When true, HTML anchors and PDF destinations are generated before
    the headings (e.g. of sections), which allows the table of contents
    to contain links and also code-like references to sections (like
    `@FOO-MANUAL`) to be translated to links with the
    [`TITLE`][72b4] being the link text.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-MAX-NUMBERING-LEVEL-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-MAX-NUMBERING-LEVEL\*** *3*

    A non-negative integer. In their hierarchy, sections on levels less
    than this value get numbered in the format of `3.1.2`. Setting it to
    0 turns numbering off.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL\*** *3*

    An integer that determines the depth of the table of contents.
    
    - If negative, then no table of contents is generated.
    
    - If non-negative, and there are multiple top-level sections on a
      page, then they are listed at the top of the page.
    
    - If positive, then for each top-level section a table of contents
      is printed after its heading, which includes a nested tree of
      section titles whose depth is limited by this value.
    
    If [`*DOCUMENT-LINK-SECTIONS*`][1b28] is true, then the tables will link to
    the sections.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-TEXT-NAVIGATION-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-TEXT-NAVIGATION\*** *NIL*

    If true, then before each heading a line is printed with links to
    the previous, parent and next section. Needs
    [`*DOCUMENT-LINK-SECTIONS*`][1b28] to be on to work.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-FANCY-HTML-NAVIGATION-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-FANCY-HTML-NAVIGATION\*** *T*

    If true and the output format is HTML, then headings get a
    navigation component that consists of links to the previous, parent,
    next section, a self-link, and a link to the definition in the
    source code if available (see `:SOURCE-URI-FN` in [`DOCUMENT`][432c]). This
    component is normally hidden, it is visible only when the mouse is
    over the heading. Has no effect if [`*DOCUMENT-LINK-SECTIONS*`][1b28] is
    false.

<a id="x-28MGL-PAX-3A-40FILTERING-LINKS-20MGL-PAX-3ASECTION-29"></a>

#### 8.5.5 Filtering Links

<a id="x-28MGL-PAX-3A-2ADOCUMENT-LINK-CODE-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-LINK-CODE\*** *T*

    Whether definitions of things other than [`SECTION`][5fac]s
    are allowed to be [linkable][7eb5].

<a id="x-28MGL-PAX-3A-40LINKABLE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **linkable**

    When a reference is encountered to [definition][2143] D
    while processing documentation for some page C, we say that
    definition D is *linkable* (from C) if
    
    - D denotes a [`SECTION`][5fac] and [`*DOCUMENT-LINK-SECTIONS*`][1b28] is true, or
    
    - D does not denote a `SECTION` and [`*DOCUMENT-LINK-CODE*`][d9ee] is true
    
    ... and
    
    - We are [Browsing Live Documentation][a595], or
    
    - D is an external definition ([`CLHS`][ed5f] or denotes a
      [`GLOSSARY-TERM`][8251] with a [`URL`][8ece]), or
    
    - D's page is C, or
    
    - D's page is relativizable to C.
    
    In the above, *D's page* is the last of the pages in the
    [`DOCUMENTABLE`][0702] to which D's documentation is written (see `:OBJECTS` in
    [`PAGES`][9c7d]), and we say that a page is *relativizable* to another if it
    is possible to construct a relative link between their
    `:URI-FRAGMENT`s.

<a id="x-28MGL-PAX-3A-40SPECIFIC-LINK-20MGL-PAX-3ASECTION-29"></a>

##### Specific Link

Specific links are those [Reflink][cbc4]s and [Autolink][ec7a]s that have a
single [locative][7ac8] and therefore at most a single matching
[definition][2143]. These are [Specific Reflink][f7a5],
[Specific Reflink with Text][fb17] and [Specific Autolink][38de].

A specific link to a [linkable][7eb5] definition produces a link in the
output. If the definition is not linkable, then the output will
contain only what would otherwise be the link text.

<a id="x-28MGL-PAX-3A-40UNSPECIFIC-LINK-20MGL-PAX-3ASECTION-29"></a>

##### Unspecific Link

Unspecific links are those [Reflink][cbc4]s and [Autolink][ec7a]s that do not
specify a [locative][7ac8] and match all [definitions][d930]
with a name. These are [Unspecific Reflink][4e05],
[Unspecific Reflink with Text][34b8] and [Unspecific Autolink][e2a4].

To make the links predictable and manageable in number, the
following steps are taken.

1. Definitions that are not symbol-based (i.e. whose [`DREF-NAME`][1e36]
is not a symbol) are filtered out to prevent unrelated
[`PACKAGE`][4dd7]s, [`ASDF:SYSTEM`][c097]s and [`CLHS`][ed5f]
sections from cluttering the documentation without the control
provided by importing symbols.

2. All references with [`LOCATIVE-TYPE`][97ba] `LOCATIVE` are filtered out.

3. Non-[linkable][7eb5] definitions are removed.

4. If the definitions include a [`GENERIC-FUNCTION`][5875], then
all definitions with `LOCATIVE-TYPE` [`METHOD`][172e],
[`ACCESSOR`][00d4], [`READER`][cc04] and [`WRITER`][e548] are
removed to avoid linking to a possibly large number of methods.

If at most a single definition remains, then the output is the same
as with a [Specific Link][0361]. If multiple definitions remain, then the
link text is output followed by a number of numbered links, one to
each definition.

<a id="x-28MGL-PAX-3A-40LINK-FORMAT-20MGL-PAX-3ASECTION-29"></a>

#### 8.5.6 Link Format

The following variables control various aspects of links and `URL`s.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-URL-VERSIONS-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-URL-VERSIONS\*** *(2 1)*

    A list of versions of PAX URL formats to support in the
    generated documentation. The first in the list is used to generate
    links.
    
    PAX emits HTML anchors before the documentation of [`SECTION`][5fac]s
    (see [Linking to Sections][22c2]) and other things (see [Linking][19e3]). For the
    function `FOO`, in the current version (version 2), the anchor is
    `<a id="MGL-PAX:FOO%20FUNCTION">`, and its URL will end
    with `#MGL-PAX:FOO%20FUNCTION`.
    
    *Note that to make the URL independent of whether a symbol is
    [internal or external][3473] to their [`SYMBOL-PACKAGE`][e5ab], single
    colon is printed where a double colon would be expected. Package and
    symbol names are both printed verbatim except for escaping colons
    and spaces with a backslash. For exported symbols with no funny
    characters, this coincides with how [`PRIN1`][6384] would print the symbol,
    while having the benefit of making the URL independent of the Lisp
    printer's escaping strategy and producing human-readable output for
    mixed-case symbols. No such promises are made for non-ASCII
    characters, and their URLs may change in future versions. Locatives
    are printed with `PRIN1`.*
    
    Version 1 is based on the more strict HTML4 standard and the id of
    `FOO` is `"x-28MGL-PAX-3A-3AFOO-20FUNCTION-29"`. This is supported
    by GitHub-flavoured Markdown. Version 2 has minimal clutter and is
    obviously preferred. However, in order not to break external links,
    by default, both anchors are generated.
    
    Let's understand the generated Markdown.
    
    ```
    (defun foo (x))
    
    (document #'foo :format :markdown)
    => ("<a id=\"x-28MGL-PAX-3AFOO-20FUNCTION-29\"></a>
    <a id=\"MGL-PAX:FOO%20FUNCTION\"></a>
    
    - [function] **FOO** *X*
    ")
    
    (let ((*document-url-versions* '(1)))
      (document #'foo :format :markdown))
    => ("<a id=\"x-28MGL-PAX-3AFOO-20FUNCTION-29\"></a>
    
    - [function] **FOO** *X*
    ")
    ```

<a id="x-28MGL-PAX-3A-2ADOCUMENT-MIN-LINK-HASH-LENGTH-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-MIN-LINK-HASH-LENGTH\*** *4*

    Recall that [Markdown reference link][8c00]s (like `[label][id]`) are used for
    [Linking][19e3]. It is desirable to have ids that are short to maintain
    legibility of the generated Markdown, but also stable to reduce the
    spurious diffs in the generated documentation, which can be a pain
    in a version control system.
    
    Clearly, there is a tradeoff here. This variable controls how many
    characters of the MD5 sum of the full link id (the reference as a
    string) are retained. If collisions are found due to the low number
    of characters, then the length of the hash of the colliding
    reference is increased.
    
    This variable has no effect on the HTML generated from Markdown, but
    it can make Markdown output more readable.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-BASE-URL-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-BASE-URL\*** *NIL*

    When `*DOCUMENT-BASE-URL*` is non-`NIL`, this is prepended to all
    Markdown relative `URL`s. It must be a valid `URL` without no query and
    fragment parts (that is, *http://lisp.org/doc/* but not
    *http://lisp.org/doc?a=1* or *http://lisp.org/doc#fragment*). Note
    that intra-page links using only `URL` fragments (e.g. and explicit
    HTML links (e.g. `<a href="...">`) in Markdown are not
    affected.

<a id="x-28MGL-PAX-3A-40LOCAL-DEFINITION-20MGL-PAX-3ASECTION-29"></a>

### 8.6 Local Definition

While documentation is generated for a definition, that
definition is considered local. Other local definitions may also be
established. Local definitions inform [Codification][f1ab] through
[interesting][7445] [name][88cf]s and affect [Unspecific Autolink][e2a4]ing.

```
(defun foo (x)
  "FOO adds one to X."
  (1+ x)
```

In this example, while the docstring of `FOO` is being processed, the
global definition `(DREF 'FOO 'FUNCTION)` is also considered local,
which suppresses linking `FOO` in the `FOO`'s docstring back to its
definition. If `FOO` has other definitions, [Unspecific Autolink][e2a4]ing to
those is also suppressed.

Furthermore, the purely local definition `(DREF 'X 'ARGUMENT)` is
established, causing the argument name `X` to be
[codified][f1ab] because `X` is now [interesting][7445].

See [`DOCUMENTING-DEFINITION`][a412] and [`WITH-DISLOCATED-NAMES`][2d48] in
[Extending `DOCUMENT`][574a].

<a id="x-28MGL-PAX-3A-40OVERVIEW-OF-ESCAPING-20MGL-PAX-3ASECTION-29"></a>

### 8.7 Overview of Escaping

Let's recap how escaping [Codification][f1ab],
[downcasing][a5ee], and [Linking][19e3]
works.

- One backslash in front of a [word][d7b0] turns codification off. Use this
  to prevent codification words such as DOCUMENT, which is all
  uppercase hence [codifiable][b89a], and it names an exported symbol hence
  it is [interesting][7445].

- One backslash right after an opening backtick turns autolinking
  off.

- Two backslashes right after an opening backtick turns autolinking
  and downcasing off. Use this for things that are not Lisp code but
  which need to be in a monospace font.


In the following examples capital C/D/A letters mark the presence,
and a/b/c the absence of codification, downcasing, and autolinking
assuming all these features are enabled by
[`*DOCUMENT-UPPERCASE-IS-CODE*`][f25f], [`*DOCUMENT-DOWNCASE-UPPERCASE-CODE*`][a5ee],
and [`*DOCUMENT-LINK-CODE*`][d9ee].

    DOCUMENT                => [`document`][1234]    (CDA)
    \DOCUMENT               => DOCUMENT              (cda)
    `\DOCUMENT`             => `document`            (CDa)
    `\\DOCUMENT`            => `DOCUMENT`            (CdA)
    [DOCUMENT][]            => [`document`][1234]    (CDA)
    [\DOCUMENT][]           => [DOCUMENT][1234]      (cdA)
    [`\DOCUMENT`][]         => [`document`][1234]    (CDA) *
    [`\\DOCUMENT`][]        => [`DOCUMENT`][1234]    (CdA)
    [DOCUMENT][dislocated]  => `document`            (CDa)

Note that in the example marked with `*`, the single backslash,
that would normally turn autolinking off, is ignored because it is
in an explicit link.

<a id="x-28MGL-PAX-3A-40OUTPUT-FORMATS-20MGL-PAX-3ASECTION-29"></a>

### 8.8 Output Formats

<a id="x-28MGL-PAX-3A-2ADOCUMENT-MARK-UP-SIGNATURES-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-MARK-UP-SIGNATURES\*** *T*

    When true, some things such as function names and arglists are
    rendered as bold and italic. In [HTML Output][36e1] and [PDF Output][19ad],
    locative types become links to sources (if `:SOURCE-URI-FN` is
    provided, see [`PAGES`][9c7d]), and the symbol becomes a self-link for your
    permalinking pleasure.
    
    For example, a reference is rendered in Markdown roughly as:
    
        - [function] foo x y
    
    With this option on, the above becomes:
    
        - [function] **foo** *x y*
    
    Also, in HTML `**foo**` will be a link to that very entry and
    `[function]` may turn into a link to sources.

<a id="x-28MGL-PAX-3A-40PLAIN-OUTPUT-20MGL-PAX-3ASECTION-29"></a>

#### 8.8.1 Plain Output

This is the default `:FORMAT` of [`DOCUMENT`][432c], intended to be a
replacement for [`CL:DOCUMENTATION`][c5ae]. `:PLAIN` (short for plain text) is
very similar to `:MARKDOWN`, but most of the markup that would make
reading in, say, the REPL unpleasant is removed.

- Markup for [Markdown emphasis][c624], [Markdown inline code][68c1],
  [Markdown reference link][8c00]s and [fenced code blocks][1322] is stripped from
  the output.

- No link anchors are emitted.

- No [section numbering][f12d].

- No [table of contents][d4e7].


<a id="x-28MGL-PAX-3A-40MARKDOWN-OUTPUT-20MGL-PAX-3ASECTION-29"></a>

#### 8.8.2 Markdown Output

By default, [`DREF`][d930]s are documented in the following format.

```
- [<locative-type>] <name> <arglist>

    <docstring>
```

The line with the bullet is printed with [`DOCUMENTING-DEFINITION`][a412]. The
docstring is processed with [`DOCUMENT-DOCSTRING`][7f1f] while
[Local Definition][9db9]s established with [`WITH-DISLOCATED-NAMES`][2d48] are in
effect for all variables locally bound in a definition with [`ARGLIST`][e6bd],
and [`*PACKAGE*`][5ed1] is bound to the second return value of [`DOCSTRING`][affc].

With this default format, PAX supports all locative types, but for
some [Basic Locative Types][1d1d] defined in DRef and the
[PAX Locatives][292a], special provisions have been made.

- For definitions with a [`VARIABLE`][6c83] or [`CONSTANT`][c819] locative,
their initform is printed as their arglist. The initform is the
`INITFORM` argument of the locative if provided, or the global symbol
value of their name. If no `INITFORM` is provided, and the symbol is
globally unbound, then no arglist is printed.

When the printed initform is too long, it is truncated.

- Depending of what the [`SETF`][d83a] locative refers to, the `ARGLIST` of the
[setf expander][35a2], [setf function][99b05], or the method
signature is printed as with the [`METHOD`][172e] locative.

- For definitions with a [`METHOD`][172e] locative, the arglist printed is
the method signature, which consists of the locative's `QUALIFIERS`
and `SPECIALIZERS` appended.

- For definitions with an [`ACCESSOR`][00d4], [`READER`][cc04] or
[`WRITER`][e548] locative, the class on which they are specialized is printed
as their arglist.

- For definitions with a [`STRUCTURE-ACCESSOR`][090c] locative, the arglist
printed is the locative's `CLASS-NAME` argument if provided.

- For definitions with a [`CLASS`][2060] locative, the arglist printed is the
list of [public superclasses][ff58] with [`STANDARD-OBJECT`][a843] and [`CONDITION`][83e1]
omitted.

- For definitions with a [`STRUCTURE`][da65] locative, the arglist printed is
the list of [public superclasses][ff58] with [`STRUCTURE-OBJECT`][2038] omitted.

- For definitions with a [`CONDITION`][c479] locative, the arglist printed is
the list of [public superclasses][ff58] with `STANDARD-OBJECT` and
`CONDITION` omitted.

- For definitions with a [`ASDF:SYSTEM`][c097] locative, their most
important slots are printed as an unnumbered list.

- For definitions with the [`LOCATIVE`][0b3a] locative type, their
[`LOCATIVE-TYPE-DIRECT-SUPERS`][80a8] and [`LOCATIVE-TYPE-DIRECT-SUBS`][130a] are
printed.

- When documentation is being generated for a definition with
the [`SECTION`][672f] locative, a new (sub)section is opened (see
[`WITH-HEADING`][80e8]), within which documentation for its each of its
[`SECTION-ENTRIES`][d850] is generated. A fresh line is printed after all
entries except the last.

- For definitions with a [`GLOSSARY-TERM`][5119] locative, no arglist is
printed, and if non-`NIL`, [`GLOSSARY-TERM-TITLE`][af78] is printed as name.

- For definitions with a [`GO`][58f6] locative, its [`LOCATIVE-ARGS`][2444] are printed
as its arglist, along with a redirection message.

- See the [`INCLUDE`][5cd7] locative.

- For definitions with a [`CLHS`][ed5f] locative, the `LOCATIVE-ARGS` are printed
as the arglist. For `CLHS` [`SECTION`][7a4e]s, the title is included in the
arglist.

- For definitions with an [`UNKNOWN`][a951] locative, the `LOCATIVE-ARGS` are
printed as the arglist. There is no docstring.


<a id="x-28MGL-PAX-3A-40PUBLIC-SUPERCLASSES-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **public superclasses**

    The public superclasses of a class are tightest envelope of
    superclasses with names exported from some package. This envelope is
    constructed by recursing depth-first into the superclass hierarchy.
    If the name of the superclass currently processed is exported from
    any package, then it is collected as a public superclass, and we do
    not recurse into its superclasses.

<a id="x-28MGL-PAX-3A-40PDF-OUTPUT-20MGL-PAX-3ASECTION-29"></a>

#### 8.8.3 PDF Output

When invoked with `:FORMAT` `:PDF`, [`DOCUMENT`][432c] generates
[Markdown Output][dd29] and converts it to PDF with [Pandoc][59d9], which in turn
uses [LaTeX](https://www.latex-project.org/). Make sure that they
are installed.

```
(with-open-file (s "x.pdf" :direction :output :if-exists :supersede
                           :if-does-not-exist :create)
  (pax:document "Hello, World!" :stream s :format :pdf))
```

To see how the output looks like, visit
[pax-manual-v0.4.1.pdf][pax-manual-sample] and
[dref-manual-v0.4.1.pdf][dref-manual-sample].

[pax-manual-sample]: http://quotenil.com/blog-files/pax-manual-v0.4.1.pdf

[dref-manual-sample]: http://quotenil.com/blog-files/dref-manual-v0.4.1.pdf

PDF output is similar to [`*DOCUMENT-HTML-DEFAULT-STYLE*`][90fa] `:CHARTER`
without the off-white tint and with coloured instead of underlined
links. The latter is because underlining interferes with hyphenation
in LaTeX. As in HTML output, locative types link to the respective
definition in the sources on GitHub (see [`MAKE-GIT-SOURCE-URI-FN`][587f]).

Note that linking from one PDF to another is currently not supported
due to the lack of consistent support in PDF viewers. Therefore,
such links are replaced by their label or the title if any (e.g. of
a [`SECTION`][5fac] or [`GLOSSARY-TERM`][8251]).

The generation of Markdown is subject to the standard
variables (again see [`DOCUMENT`][432c]). The Markdown to PDF conversion
can be customized with the following variables.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-PANDOC-PROGRAM-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-PANDOC-PROGRAM\*** *"pandoc"*

    The name of the Pandoc binary. It need not be an absolute pathname
    as `PATH` is searched.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-PANDOC-PDF-OPTIONS-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-PANDOC-PDF-OPTIONS\*** *(("-V" "papersize=a4") ("-V" "margin-left=1.03in") ("-V" "margin-right=1.03in")
 ("-V" "margin-top=1.435in") ("-V" "margin-bottom=1.435in")
 ("-V" "fontfamily=XCharter") ("-V" "fontsize=11pt") ("-V" "colorlinks=true")
 ("-V" "linkcolor=blue") ("-V" "urlcolor=Maroon") ("-V" "toccolor=blue")
 "--verbose")*

    The command-line options to invoke [`*DOCUMENT-PANDOC-PROGRAM*`][dd37] with.
    For ease of manipulation, related options are grouped into sublists,
    but the entire nested list is flattened to get the list of options
    to pass to Pandoc. If `--verbose` is specified, then in addition to
    Pandoc logging LaTeX sources, PAX will log to [`*ERROR-OUTPUT*`][66c6] the
    Markdown that it converts to PDF via LaTeX. The Markdown includes
    [`*DOCUMENT-PANDOC-PDF-HEADER-INCLUDES*`][2cd5] and
    [`*DOCUMENT-PANDOC-PDF-METADATA-BLOCK*`][ee8b].

<a id="x-28MGL-PAX-3A-2ADOCUMENT-PANDOC-PDF-HEADER-INCLUDES-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-PANDOC-PDF-HEADER-INCLUDES\*** *"\<too messy to include>"*

    LaTeX code (a string) to include in the preamble via
    [`header-includes`](https://pandoc.org/MANUAL.html#layout).
    
    The default includes have no configuration knobs. Look at the value
    to see how to customize it.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-PANDOC-PDF-METADATA-BLOCK-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-PANDOC-PDF-METADATA-BLOCK\*** *""*

    A [Pandoc YAML metadata block][84f2] as a string.
    
    Concatenate to this string to customize it.

<a id="x-28MGL-PAX-3A-40DUMMY-OUTPUT-20MGL-PAX-3ASECTION-29"></a>

#### 8.8.4 Dummy Output

When the `FORMAT` argument of [`DOCUMENT`][432c] is `NIL`, no output is
generated, but [Transcript Consistency Checking][4c39] is still performed.
Use this feature to quickly test documentation examples.

For example, in Try the test would look
like this:

```
(try:signals-not (transcription-consistency-error)
  (document ... :format nil))
```


<a id="x-28MGL-PAX-3A-40DOCUMENT-IMPLEMENTATION-NOTES-20MGL-PAX-3ASECTION-29"></a>

### 8.9 Documentation Generation Implementation Notes

Documentation Generation is supported on ABCL, AllegroCL, CLISP,
CCL, CMUCL, ECL and SBCL, but their outputs may differ due to the
lack of some introspective capability. SBCL generates complete
output. see [`ARGLIST`][e6bd], [`DOCSTRING`][affc] and [`SOURCE-LOCATION`][32da] for
implementation notes.

In addition, CLISP does not support the ambiguous case of
[Browsing Live Documentation][a595] because the current implementation
relies on Swank to list definitions of symbols (as
[`VARIABLE`][6c83], [`FUNCTION`][ba62], etc), and that simply
doesn't work.

<a id="x-28MGL-PAX-3A-40DOCUMENTATION-UTILITIES-20MGL-PAX-3ASECTION-29"></a>

### 8.10 Utilities for Generating Documentation

Two convenience functions are provided to serve the common case of
having an ASDF system with some readmes and a directory with for the
HTML documentation and the default CSS stylesheet.

<a id="x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-READMES-20FUNCTION-29"></a>

- [function] **UPDATE-ASDF-SYSTEM-READMES** *OBJECT ASDF-SYSTEM &KEY (URL-VERSIONS '(1)) (FORMATS '(:MARKDOWN))*

    Convenience function to generate up to two readme files in the
    directory holding the `ASDF-SYSTEM` definition. `OBJECT` is passed on to
    [`DOCUMENT`][432c].
    
    If `:MARKDOWN` is in `FORMATS`, then `README.md` is generated with
    anchors, links, inline code, and other markup added. Not necessarily
    the easiest on the eye in an editor but looks good on GitHub.
    
    If `:PLAIN` is in `FORMATS`, then `README` is generated, which is
    optimized for reading in text format. It has less cluttery markup
    and no [Autolink][ec7a]ing.
    
    Example usage:
    
    ```
    (update-asdf-system-readmes @pax-manual :mgl-pax
                                :formats '(:markdown :plain))
    ```
    
    Note that [`*DOCUMENT-URL-VERSIONS*`][17e0] is bound to `URL-VERSIONS`, which
    defaults to using the uglier, version 1 style of `URL` for the sake of
    GitHub.

<a id="x-28MGL-PAX-3A-40HTML-OUTPUT-20MGL-PAX-3ASECTION-29"></a>

#### 8.10.1 HTML Output

<a id="x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-HTML-DOCS-20FUNCTION-29"></a>

- [function] **UPDATE-ASDF-SYSTEM-HTML-DOCS** *SECTIONS ASDF-SYSTEM &KEY PAGES (TARGET-DIR (ASDF/SYSTEM:SYSTEM-RELATIVE-PATHNAME ASDF-SYSTEM "doc/")) (UPDATE-CSS-P T) (STYLE \*DOCUMENT-HTML-DEFAULT-STYLE\*)*

    Generate pretty HTML documentation for a single ASDF system,
    possibly linking to GitHub. If `UPDATE-CSS-P`, copy the `STYLE` files to
    `TARGET-DIR` (see [`*DOCUMENT-HTML-DEFAULT-STYLE*`][90fa]).
    
    Example usage:
    
    ```
    (update-asdf-system-html-docs @pax-manual :mgl-pax)
    ```
    
    The same, linking to the sources on GitHub:
    
    ```
    (update-asdf-system-html-docs
      @pax-manual :mgl-pax
      :pages
      `((:objects (,mgl-pax::@pax-manual)
         :source-uri-fn ,(make-git-source-uri-fn
                          :mgl-pax
                          "https://github.com/melisgl/mgl-pax"))))
    ```

See the following variables, which control HTML generation.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-DEFAULT-STYLE-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-HTML-DEFAULT-STYLE\*** *:DEFAULT*

    The HTML style to use. It's either `STYLE` is either `:DEFAULT` or
    `:CHARTER`. The `:DEFAULT` CSS stylesheet relies on the default
    fonts (sans-serif, serif, monospace), while `:CHARTER` bundles some
    fonts for a more controlled look.
    
    The value of this variable affects the default style of
    [`UPDATE-ASDF-SYSTEM-HTML-DOCS`][bb12].

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL\*** *NIL*

    `NIL` or a non-negative integer. If non-`NIL`, it overrides
    [`*DOCUMENT-MAX-NUMBERING-LEVEL*`][f12d] in the dynamic HTML table of contents
    on the left of the page.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-LANG-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-HTML-LANG\*** *"en"*

    The value for the `html` element's `xml:lang` and `lang`
    attributes in the generated HTML.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-CHARSET-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-HTML-CHARSET\*** *"UTF-8"*

    The value for `charset` attribute of the `<meta http-equiv='Content-Type'
    content='text/html'>` element in the generated HTML.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-HEAD-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-HTML-HEAD\*** *NIL*

    Stuff to be included in the `<head>` of the generated HTML.
    
    - If `NIL`, nothing is included.
    
    - If a `STRING`([`0`][b93c] [`1`][dae6]), then it is written to the HTML output as is without
      any escaping.
    
    - If a function designator, then it is called with a single
      argument, the HTML stream, where it must write the output.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-SIDEBAR-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-HTML-SIDEBAR\*** *NIL*

    Stuff to be included in the HTML sidebar.
    
    - If `NIL`, a default sidebar is generated, with
      [`*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS*`][e216], followed by the dynamic table
      of contents, and [`*DOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS*`][0ef0].
    
    - If a `STRING`([`0`][b93c] [`1`][dae6]), then it is written to the HTML output as is without
      any escaping.
    
    - If a function designator, then it is called with a single
      argument, the HTML stream, where it must write the output.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-TOP-BLOCKS-OF-LINKS-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS\*** *NIL*

    A list of blocks of links to be displayed on the sidebar on the left,
    above the table of contents. A block is of the form `(&KEY TITLE ID
    LINKS)`, where `TITLE` will be displayed at the top of the block in a
    HTML `DIV` with `ID` followed by the links. `LINKS` is a list of `(URI
    LABEL)` elements, where `URI` maybe a string or an object being
    [`DOCUMENT`][432c]ed or a `REFERENCE` thereof.

<a id="x-28MGL-PAX-3A-2ADOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS-2A-20VARIABLE-29"></a>

- [variable] **\*DOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS\*** *NIL*

    Like [`*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS*`][e216], only it is displayed
    below the table of contents.

<a id="x-28MGL-PAX-3A-40GITHUB-WORKFLOW-20MGL-PAX-3ASECTION-29"></a>

#### 8.10.2 GitHub Workflow

It is generally recommended to commit generated readmes (see
[`UPDATE-ASDF-SYSTEM-READMES`][13a9]), so that users have something to read
without reading the code and sites like GitHub can display them.

HTML documentation can also be committed, but there is an issue with
that: when linking to the sources (see [`MAKE-GIT-SOURCE-URI-FN`][587f]), the
commit id is in the link. This means that code changes need to be
committed first, and only then can HTML documentation be regenerated
and committed in a followup commit.

The second issue is that GitHub is not very good at serving HTML
files from the repository itself (and
[http://htmlpreview.github.io](http://htmlpreview.github.io) chokes
on links to the sources).

The recommended workflow is to use
[gh-pages](https://pages.github.com/), which can be made relatively
painless with the `git worktree` command. The gist of it is to make
the `doc/` directory a checkout of the branch named `gh-pages`.
There is a [good
description](http://sangsoonam.github.io/2019/02/08/using-git-worktree-to-deploy-github-pages.html)
of this general process. Two commits are needed still, but it is
somewhat less painful.

This way the HTML documentation will be available at

    http://<username>.github.io/<repo-name>

It is probably a good idea to add sections like the
[Links and Systems][ba90] section to allow jumping between the repository
and the gh-pages site.

<a id="x-28MGL-PAX-3AMAKE-GITHUB-SOURCE-URI-FN-20FUNCTION-29"></a>

- [function] **MAKE-GITHUB-SOURCE-URI-FN** *ASDF-SYSTEM GITHUB-URI &KEY GIT-VERSION*

    This function is a backward-compatibility wrapper around
    [`MAKE-GIT-SOURCE-URI-FN`][587f], which supersedes `MAKE-GITHUB-SOURCE-URI-FN`.
    All arguments are passed on to `MAKE-GIT-SOURCE-URI-FN`, leaving
    `URI-FORMAT-STRING` at its default, which is suitable for GitHub.

<a id="x-28MGL-PAX-3AMAKE-GIT-SOURCE-URI-FN-20FUNCTION-29"></a>

- [function] **MAKE-GIT-SOURCE-URI-FN** *ASDF-SYSTEM GIT-FORGE-URI &KEY GIT-VERSION (URI-FORMAT-STRING "~A/blob/~A/~A#L~S")*

    Return an object suitable as `:SOURCE-URI-FN` of a page spec (see
    the `PAGES` argument of [`DOCUMENT`][432c]). The function looks at the source
    location of the object passed to it, and if the location is found,
    the path is made relative to the top-level directory of the git
    checkout containing the file of the `ASDF-SYSTEM` and finally an URI
    pointing to your git forge (such as GitHub) is returned. A warning
    is signalled whenever the source location lookup fails or if the
    source location points to a directory not below the directory of
    `ASDF-SYSTEM`.
    
    If `GIT-FORGE-URI` is `"https://github.com/melisgl/mgl-pax/"` and
    `GIT-VERSION` is `"master"`, then the returned URI may look like this:
    
        https://github.com/melisgl/mgl-pax/blob/master/src/pax-early.lisp#L12
    
    If `GIT-VERSION` is `NIL`, then an attempt is made to determine to
    current commit id from the `.git` in the directory holding
    `ASDF-SYSTEM`. If no `.git` directory is found, then no links to
    the git forge will be generated.
    
    `URI-FORMAT-STRING` is a [`CL:FORMAT`][ad78] control string for four arguments:
    
    - `GIT-FORGE-URI`,
    
    - `GIT-VERSION`,
    
    - the relative path to the file of the source location of the reference,
    
    - and the line number.
    
    The default value of `URI-FORMAT-STRING` is for GitHub. If using a
    non-standard git forge, such as Sourcehut or GitLab, simply pass a
    suitable `URI-FORMAT-STRING` matching the URI scheme of your forge.

<a id="x-28MGL-PAX-3A-40PAX-WORLD-20MGL-PAX-3ASECTION-29"></a>

#### 8.10.3 PAX World

PAX World is a registry of documents, which can generate
cross-linked HTML documentation pages for all the registered
documents. There is an official [PAX
World](https://melisgl.github.io/mgl-pax-world/).

<a id="x-28MGL-PAX-3AREGISTER-DOC-IN-PAX-WORLD-20FUNCTION-29"></a>

- [function] **REGISTER-DOC-IN-PAX-WORLD** *NAME SECTIONS PAGE-SPECS*

    Register `SECTIONS` and `PAGE-SPECS` under `NAME` (a symbol) in PAX
    World. By default, [`UPDATE-PAX-WORLD`][ee51] generates documentation for all
    of these. `SECTIONS` and `PAGE-SPECS` must be lists of [`SECTION`][5fac]s and
    `PAGE-SPEC`s (SEE [`DOCUMENT`][432c]) or designators of function of no arguments
    that return such lists.

For example, this is how PAX registers itself:

```
(defun pax-sections ()
  (list @pax-manual))

(defun pax-pages ()
  `((:objects ,(pax-sections)
     :source-uri-fn ,(make-git-source-uri-fn
                      :mgl-pax
                      "https://github.com/melisgl/mgl-pax"))))

(register-doc-in-pax-world :pax 'pax-sections 'pax-pages)

```

<a id="x-28MGL-PAX-3AUPDATE-PAX-WORLD-20FUNCTION-29"></a>

- [function] **UPDATE-PAX-WORLD** *&KEY (DOCS \*REGISTERED-PAX-WORLD-DOCS\*) DIR UPDATE-CSS-P (STYLE \*DOCUMENT-HTML-DEFAULT-STYLE\*)*

    Generate HTML documentation for all `DOCS`. Files are created in
    `DIR` (`(asdf:system-relative-pathname :mgl-pax "world/")` by
    default if `DIR` is `NIL`). `DOCS` is a list of entries of the form (`NAME`
    [`SECTIONS`][5fac] `PAGE-SPECS`). The default for `DOCS` is all the sections and
    pages registered with [`REGISTER-DOC-IN-PAX-WORLD`][f4fd].
    
    In the absence of `:HEADER-FN` `:FOOTER-FN`, `:OUTPUT`, every spec in
    `PAGE-SPECS` is augmented with HTML headers, footers and output
    location specifications (based on the name of the section).
    
    If necessary a default page spec is created for every section.

<a id="x-28MGL-PAX-3A-40TRANSCRIPTS-20MGL-PAX-3ASECTION-29"></a>

## 9 Transcripts

What are transcripts for? When writing a tutorial, one often wants
to include a REPL session with maybe a few defuns and a couple of
forms whose output or return values are shown. Also, in a function's
docstring an example call with concrete arguments and return values
speaks volumes. A transcript is a text that looks like a REPL
session, but which has a light markup for printed output and return
values, while no markup (i.e. prompt) for Lisp forms. PAX
transcripts may include output and return values of all forms, or
only selected ones. In either case, the transcript itself can be
easily generated from the source code.

The main worry associated with including examples in the
documentation is that they tend to get out-of-sync with the code.
This is solved by being able to parse back and update transcripts.
In fact, this is exactly what happens during documentation
generation with PAX. Code sections tagged with `cl-transcript` are
retranscribed and checked for consistency (that is, no difference in
output or return values). If the consistency check fails, an error
is signalled that includes a reference to the object being
documented.

Going beyond documentation, transcript consistency checks can be
used for writing simple tests in a very readable form. For example:

```common-lisp
(+ 1 2)
=> 3

(values (princ :hello) (list 1 2))
.. HELLO
=> :HELLO
=> (1 2)
```

All in all, transcripts are a handy tool especially when combined
with the Emacs support to regenerate them and with
PYTHONIC-STRING-READER's triple-quoted strings, that
allow one to work with nested strings with less noise. The
triple-quote syntax can be enabled with:

    (in-readtable pythonic-string-syntax)


<a id="x-28MGL-PAX-3A-40TRANSCRIBING-WITH-EMACS-20MGL-PAX-3ASECTION-29"></a>

### 9.1 Transcribing with Emacs

Typical transcript usage from within Emacs is simple: add a Lisp
form to a docstring or comment at any indentation level. Move the
cursor right after the end of the form as if you were to evaluate it
with `C-x C-e`. The cursor is marked by `#\^`:

    This is part of a docstring.
    
    ```cl-transcript
    (values (princ :hello) (list 1 2))^
    ```

Note that the use of fenced code blocks with the language tag
`cl-transcript` is only to tell PAX to perform consistency checks at
documentation generation time.

Now invoke the Elisp function `mgl-pax-transcribe` where the cursor
is, and the fenced code block from the docstring becomes:

    (values (princ :hello) (list 1 2))
    .. HELLO
    => :HELLO
    => (1 2)
    ^

Then you change the printed message and add a comment to the second
return value:

    (values (princ :hello-world) (list 1 2))
    .. HELLO
    => :HELLO
    => (1
        ;; This value is arbitrary.
        2)

When generating the documentation you get a
[`TRANSCRIPTION-CONSISTENCY-ERROR`][a249] because the printed output and the
first return value changed, so you regenerate the documentation by
marking the region bounded by `#\|` and the cursor at `#\^` in the
example:

    |(values (princ :hello-world) (list 1 2))
    .. HELLO
    => :HELLO
    => (1
        ;; This value is arbitrary.
        2)
    ^

then invoke the Elisp function `mgl-pax-retranscribe-region` to get:

    (values (princ :hello-world) (list 1 2))
    .. HELLO-WORLD
    => :HELLO-WORLD
    => (1
        ;; This value is arbitrary.
        2)
    ^

Note how the indentation and the comment of `(1 2)` were left alone,
but the output and the first return value got updated.

Alternatively, `C-u 1 mgl-pax-transcribe` will emit commented markup:

    (values (princ :hello) (list 1 2))
    ;.. HELLO
    ;=> :HELLO
    ;=> (1 2)

`C-u 0 mgl-pax-retranscribe-region` will turn commented into
non-commented markup. In general, the numeric prefix argument is the
index of the syntax to be used in [`*TRANSCRIBE-SYNTAXES*`][ebd3]. Without a
prefix argument, `mgl-pax-retranscribe-region` will not change the
markup style.

Finally, not only do both functions work at any indentation level
but in comments too:

    ;;;; (values (princ :hello) (list 1 2))
    ;;;; .. HELLO
    ;;;; => :HELLO
    ;;;; => (1 2)

The dynamic environment of the transcription is determined by the
`:DYNENV` argument of the enclosing `cl-transcript` code block (see
[Controlling the Dynamic Environment][6b59]).

Transcription support in Emacs can be enabled by loading
`src/mgl-pax.el`. See [Emacs Setup][8541].

<a id="x-28MGL-PAX-3A-40TRANSCRIPT-API-20MGL-PAX-3ASECTION-29"></a>

### 9.2 Transcript API

<a id="x-28MGL-PAX-3ATRANSCRIBE-20FUNCTION-29"></a>

- [function] **TRANSCRIBE** *INPUT OUTPUT &KEY UPDATE-ONLY (INCLUDE-NO-OUTPUT UPDATE-ONLY) (INCLUDE-NO-VALUE UPDATE-ONLY) (ECHO T) (CHECK-CONSISTENCY \*TRANSCRIBE-CHECK-CONSISTENCY\*) DEFAULT-SYNTAX (INPUT-SYNTAXES \*TRANSCRIBE-SYNTAXES\*) (OUTPUT-SYNTAXES \*TRANSCRIBE-SYNTAXES\*) DYNENV*

    Read forms from `INPUT` and write them (iff `ECHO`) to `OUTPUT`
    followed by any output and return values produced by calling [`EVAL`][0d6e] on
    the form. The variables [\*][9590], [\*\*][78d1], [\*\*\*][ea37],
    [/][9f2f], [//][e433], [///][3831], [-][5483], [+][72a7],
    [++][fbb1], [+++][4537] are locally bound and updated as in a
    [REPL][f83b]. Since `TRANSCRIBE` `EVAL`uates
    arbitrary code anyway, forms are read with [`*READ-EVAL*`][82f7] `T`.
    
    `INPUT` can be a stream or a string, while `OUTPUT` can be a stream or
    `NIL`, in which case output goes into a string. The return value is
    the `OUTPUT` stream or the string that was constructed.
    
    Go up to [Transcribing with Emacs][f5bd] for nice examples. A more
    mind-bending one is this:
    
    ```common-lisp
    (transcribe "(princ 42) " nil)
    => "(princ 42)
    .. 42
    => 42
    "
    ```
    
    However, the above may be a bit confusing since this documentation
    uses `TRANSCRIBE` markup syntax in this very example, so let's do it
    differently. If we have a file with these contents:
    
    ```
    (values (princ 42) (list 1 2))
    ```
    
    it is transcribed to:
    
    ```
    (values (princ 42) (list 1 2))
    .. 42
    => 42
    => (1 2)
    ```
    
    Output to all standard streams is captured and printed with
    the `:OUTPUT` prefix (`".."`). The return values above are printed
    with the `:READABLE` prefix (`"=>"`). Note how these prefixes are
    always printed on a new line to facilitate parsing.
    
    **Updating**
    
    `TRANSCRIBE` is able to parse its own output. If we transcribe the
    previous output above, we get it back exactly. However, if we remove
    all output markers, leave only a placeholder value marker and
    pass `:UPDATE-ONLY` `T` with source:
    
    ```
    (values (princ 42) (list 1 2))
    =>
    ```
    
    we get this:
    
    ```
    (values (princ 42) (list 1 2))
    => 42
    => (1 2)
    ```
    
    With `UPDATE-ONLY`, the printed output of a form is transcribed only
    if there were output markers in the source. Similarly, with
    `UPDATE-ONLY`, return values are transcribed only if there were value
    markers in the source.
    
    **No Output/Values**
    
    If the form produces no output or returns no values, then whether or
    not output and values are transcribed is controlled by
    `INCLUDE-NO-OUTPUT` and `INCLUDE-NO-VALUE`, respectively. By default,
    neither is on so:
    
    ```
    (values)
    ..
    =>
    ```
    
    is transcribed to
    
    ```
    (values)
    ```
    
    With `UPDATE-ONLY` true, we probably wouldn't like to lose those
    markers since they were put there for a reason. Hence, with
    `UPDATE-ONLY`, `INCLUDE-NO-OUTPUT` and `INCLUDE-NO-VALUE` default to true.
    So, with `UPDATE-ONLY` the above example is transcribed to:
    
    ```
    (values)
    ..
    => ; No value
    ```
    
    where the last line is the `:NO-VALUE` prefix.
    
    **Consistency Checks**
    
    If `CHECK-CONSISTENCY` is true, then `TRANSCRIBE` signals a continuable
    [`TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR`][8492] whenever a form's output as a
    string is different from what was in `INPUT`, provided that `INPUT`
    contained the output. Similarly, for values, a continuable
    [`TRANSCRIPTION-VALUES-CONSISTENCY-ERROR`][238c] is signalled if a value read
    from the source does not print as the as the value returned by `EVAL`.
    This allows readable values to be hand-indented without failing
    consistency checks:
    
    ```
    (list 1 2)
    => ;; This is commented, too.
       (1
          ;; Funny indent.
          2)
    ```
    
    See [Transcript Consistency Checking][4c39] for the full picture.
    
    **Unreadable Values**
    
    The above scheme involves [`READ`][fe58], so consistency of unreadable values
    cannot be treated the same. In fact, unreadable values must even be
    printed differently for transcribe to be able to read them back:
    
    ```
    (defclass some-class () ())
    
    (defmethod print-object ((obj some-class) stream)
      (print-unreadable-object (obj stream :type t)
        (format stream "~%~%end")))
    
    (make-instance 'some-class)
    ==> #<SOME-CLASS 
    -->
    --> end>
    ```
    
    where `"==>"` is the `:UNREADABLE` prefix and `"-->"` is the
    `:UNREADABLE-CONTINUATION` prefix. As with outputs, a consistency
    check between an unreadable value from the source and the value from
    `EVAL` is performed with [`STRING=`][4143] by default. That is, the value from
    `EVAL` is printed to a string and compared to the source value. Hence,
    any change to unreadable values will break consistency checks. This
    is most troublesome with instances of classes with the default
    [`PRINT-OBJECT`][3f2e] method printing the memory address. See
    [Finer-Grained Consistency Checks][6e18].
    
    **Errors**
    
    If an [`ERROR`][d162] condition is signalled, the error is printed to the
    output and no values are returned.
    
    ```common-lisp
    (progn
      (print "hello")
      (error "no greeting"))
    ..
    .. "hello" 
    .. debugger invoked on SIMPLE-ERROR:
    ..   no greeting
    ```
    
    To keep the textual representation somewhat likely to be portable,
    the printing is done with `(FORMAT T "#<~S ~S>" (TYPE-OF
    ERROR) (PRINC-TO-STRING ERROR))`. [`SIMPLE-CONDITION`][f2f5]s are formatted to
    strings with [`SIMPLE-CONDITION-FORMAT-CONTROL`][4841] and
    [`SIMPLE-CONDITION-FORMAT-ARGUMENTS`][da14].
    
    **Syntaxes**
    
    Finally, a transcript may employ different syntaxes for the output
    and values of different forms. When `INPUT` is read, the syntax for
    each form is determined by trying to match all prefixes from all
    syntaxes in `INPUT-SYNTAXES` against a line. If there are no output or
    values for a form in `INPUT`, then the syntax remains undetermined.
    
    When `OUTPUT` is written, the prefixes to be used are looked up in
    `DEFAULT-SYNTAX` of `OUTPUT-SYNTAXES`, if `DEFAULT-SYNTAX` is not `NIL`. If
    `DEFAULT-SYNTAX` is `NIL`, then the syntax used by the same form in the
    `INPUT` is used or (if that could not be determined) the syntax of the
    previous form. If there was no previous form, then the first syntax
    if `OUTPUT-SYNTAXES` is used.
    
    To produce a transcript that's executable Lisp code,
    use `:DEFAULT-SYNTAX` `:COMMENTED-1`:
    
    ```
    (make-instance 'some-class)
    ;==> #<SOME-CLASS
    ;-->
    ;--> end>
    
    (list 1 2)
    ;=> (1
    ;->    2)
    ```
    
    To translate the above to uncommented syntax,
    use `:DEFAULT-SYNTAX` `:DEFAULT`. If `DEFAULT-SYNTAX` is `NIL` (the
    default), the same syntax will be used in the output as in the input
    as much as possible.
    
    **Dynamic Environment**
    
    If `DYNENV` is non-`NIL`, then it must be a function that establishes
    the dynamic environment in which transcription shall take place. It
    is called with a single argument: a thunk (a function of no
    arguments). See [Controlling the Dynamic Environment][6b59] for an example.

<a id="x-28MGL-PAX-3A-2ATRANSCRIBE-CHECK-CONSISTENCY-2A-20VARIABLE-29"></a>

- [variable] **\*TRANSCRIBE-CHECK-CONSISTENCY\*** *NIL*

    The default value of [`TRANSCRIBE`][f1f0]'s `CHECK-CONSISTENCY` argument.

<a id="x-28MGL-PAX-3A-2ATRANSCRIBE-SYNTAXES-2A-20VARIABLE-29"></a>

- [variable] **\*TRANSCRIBE-SYNTAXES\*** *((:DEFAULT (:OUTPUT "..") (:NO-VALUE "=> ; No value") (:READABLE "=>")
  (:UNREADABLE "==>") (:UNREADABLE-CONTINUATION "-->"))
 (:COMMENTED-1 (:OUTPUT ";..") (:NO-VALUE ";=> ; No value") (:READABLE ";=>")
  (:READABLE-CONTINUATION ";->") (:UNREADABLE ";==>")
  (:UNREADABLE-CONTINUATION ";-->"))
 (:COMMENTED-2 (:OUTPUT ";;..") (:NO-VALUE ";;=> ; No value")
  (:READABLE ";;=>") (:READABLE-CONTINUATION ";;->") (:UNREADABLE ";;==>")
  (:UNREADABLE-CONTINUATION ";;-->")))*

    The default syntaxes used by [`TRANSCRIBE`][f1f0] for reading and writing
    lines containing output and values of an evaluated form.
    
    A syntax is a list of of the form `(SYNTAX-ID &REST PREFIXES)` where
    `PREFIXES` is a list of `(PREFIX-ID PREFIX-STRING)` elements. For
    example the syntax `:COMMENTED-1` looks like this:
    
    ```
    (:commented-1
     (:output ";..")
     (:no-value ";=>  No value")
     (:readable ";=>")
     (:readable-continuation ";->")
     (:unreadable ";==>")
     (:unreadable-continuation ";-->"))
    ```
    
    All of the above prefixes must be defined for every syntax except
    for `:READABLE-CONTINUATION`. If that's missing (as in the `:DEFAULT`
    syntax), then the following value is read with [`READ`][fe58] and printed with
    [`PRIN1`][6384] (hence no need to mark up the following lines).
    
    When writing, an extra space is added automatically if the line to
    be prefixed is not empty. Similarly, the first space following the
    prefix is discarded when reading.
    
    See `TRANSCRIBE` for how the actual syntax to be used is selected.

<a id="x-28MGL-PAX-3ATRANSCRIPTION-ERROR-20CONDITION-29"></a>

- [condition] **TRANSCRIPTION-ERROR** *[ERROR][d162]*

    Represents syntactic errors in the `SOURCE` argument
    of [`TRANSCRIBE`][f1f0] and also serves as the superclass of
    [`TRANSCRIPTION-CONSISTENCY-ERROR`][a249].

<a id="x-28MGL-PAX-3ATRANSCRIPTION-CONSISTENCY-ERROR-20CONDITION-29"></a>

- [condition] **TRANSCRIPTION-CONSISTENCY-ERROR** *[TRANSCRIPTION-ERROR][b81d]*

    A common superclass for
    [`TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR`][8492] and
    [`TRANSCRIPTION-VALUES-CONSISTENCY-ERROR`][238c].

<a id="x-28MGL-PAX-3ATRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR-20CONDITION-29"></a>

- [condition] **TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR** *[TRANSCRIPTION-CONSISTENCY-ERROR][a249]*

    Signalled (with [`CERROR`][4317]) by [`TRANSCRIBE`][f1f0] when invoked
    with `:CHECK-CONSISTENCY` and the output of a form is not the same as
    what was parsed.

<a id="x-28MGL-PAX-3ATRANSCRIPTION-VALUES-CONSISTENCY-ERROR-20CONDITION-29"></a>

- [condition] **TRANSCRIPTION-VALUES-CONSISTENCY-ERROR** *[TRANSCRIPTION-CONSISTENCY-ERROR][a249]*

    Signalled (with [`CERROR`][4317]) by [`TRANSCRIBE`][f1f0] when invoked
    with `:CHECK-CONSISTENCY` and the values of a form are inconsistent
    with their parsed representation.

<a id="x-28MGL-PAX-3A-40TRANSCRIPT-CONSISTENCY-CHECKING-20MGL-PAX-3ASECTION-29"></a>

### 9.3 Transcript Consistency Checking

The main use case for consistency checking is detecting
out-of-date examples in documentation, although using it for writing
tests is also a possibility. Here, we focus on the former.

When a Markdown code block tagged `cl-transcript` is processed
during [Generating Documentation][2c93], the code in it is replaced with
the output of with `(TRANSCRIBE <CODE> NIL :UPDATE-ONLY T
:CHECK-CONSISTENCY T)`. Suppose we have the following example of the
function `GREET`, that prints `hello` and returns 7.

    ```cl-transcript
    (greet)
    .. hello
    => 7
    ```

Now, if we change `GREET` to print or return something else, a
[`TRANSCRIPTION-CONSISTENCY-ERROR`][a249] will be signalled during
documentation generation. Then we may fix the documentation or
[`CONTINUE`][1867] from the error.

By default, comparisons of previous to current output, readable and
unreadable return values are performed with [`STRING=`][4143], [`EQUAL`][3fb5], and
`STRING=`, respectively, which is great in the simple case.
Non-determinism aside, exact matching becomes brittle as soon as the
notoriously unportable pretty printer is used or when unreadable
objects are printed with their `#<>` syntax, especially when
[`PRINT-UNREADABLE-OBJECT`][9439] is used with `:IDENTITY T`.

<a id="x-28MGL-PAX-3A-40TRANSCRIPT-FINER-GRAINED-CONSISTENCY-CHECKS-20MGL-PAX-3ASECTION-29"></a>

#### 9.3.1 Finer-Grained Consistency Checks

To get around this problem, consistency checking of output,
readable and unreadable values can be customized individually by
supplying [`TRANSCRIBE`][f1f0] with a `CHECK-CONSISTENCY` argument
like `((:OUTPUT <OUTPUT-CHECK>) (:READABLE
<READABLE-CHECK>) (:UNREADABLE <UNREADABLE-CHECK>))`. In this case,
`<OUTPUT-CHECK>` may be `NIL`, `T`, or a function designator.

- If it's `NIL` or there is no `:OUTPUT` entry in the list, then the
  output is not checked for consistency.

- If it's `T`, then the outputs are compared with the default,
  [`STRING=`][4143].

- If it's a function designator, then it's called with two strings
  and must return whether they are consistent with each other.

The case of `<READABLE-CHECK>` and `<UNREADABLE-CHECK>` is similar.

Code blocks tagged `cl-transcript` can take arguments, which they
pass on to `TRANSCRIBE`. The following shows how to check only the
output.

    ```cl-transcript (:check-consistency ((:output t)))
    (error "Oh, no.")
    .. debugger invoked on SIMPLE-ERROR:
    ..   Oh, no.
    
    (make-condition 'simple-error)
    ==> #<SIMPLE-ERROR {1008A81533}>
    ```


<a id="x-28MGL-PAX-3A-40TRANSCRIPT-DYNENV-20MGL-PAX-3ASECTION-29"></a>

#### 9.3.2 Controlling the Dynamic Environment

The dynamic environment in which forms in the transcript are
evaluated can be controlled via the `:DYNENV` argument of
`cl-transcript`.

    ```cl-transcript (:dynenv my-transcript)
    ...
    ```

In this case, instead of calling [`TRANSCRIBE`][f1f0] directly, the call will
be wrapped in a function of no arguments and passed to the function
`MY-TRANSCRIPT`, which establishes the desired dynamic environment
and calls its argument. The following definition of `MY-TRANSCRIPT`
simply packages up oft-used settings to `TRANSCRIBE`.

```
(defun my-transcript (fn)
  (let ((*transcribe-check-consistency*
          '((:output my-transcript-output=)
            (:readable equal)
            (:unreadable nil))))
    (funcall fn)))

(defun my-transcript-output= (string1 string2)
  (string= (my-transcript-normalize-output string1)
           (my-transcript-normalize-output string2)))

(defun my-transcript-normalize-output (string)
  (squeeze-whitespace (delete-trailing-whitespace (delete-comments string))))
```

A more involved solution could rebind global variables set in
transcripts, unintern symbols created or even create a temporary
package for evaluation.

<a id="x-28MGL-PAX-3A-40TRANSCRIPT-UTILITIES-FOR-CONSISTENCY-CHECKING-20MGL-PAX-3ASECTION-29"></a>

#### 9.3.3 Utilities for Consistency Checking

<a id="x-28MGL-PAX-3ASQUEEZE-WHITESPACE-20FUNCTION-29"></a>

- [function] **SQUEEZE-WHITESPACE** *STRING*

    Replace consecutive whitespace characters with a single space in
    `STRING` and trim whitespace from the right. This is useful to undo
    the effects of pretty printing when building comparison functions
    for [`TRANSCRIBE`][f1f0].

<a id="x-28MGL-PAX-3ADELETE-TRAILING-WHITESPACE-20FUNCTION-29"></a>

- [function] **DELETE-TRAILING-WHITESPACE** *STRING*

    Delete whitespace characters after the last non-whitespace
    character in each line in `STRING`.

<a id="x-28MGL-PAX-3ADELETE-COMMENTS-20FUNCTION-29"></a>

- [function] **DELETE-COMMENTS** *STRING &KEY (PATTERN ";")*

    For each line in `STRING` delete the rest of the line after and
    including the first occurrence of `PATTERN`. On changed lines, delete
    trailing whitespace too. This function does not parse `STRING` as Lisp
    forms, hence all occurrences of `PATTERN` (even those seemingly in
    string literals) are recognized as comments.
    
    Let's define a comparison function:
    
    ```common-lisp
    (defun string=/no-comments (string1 string2)
      (string= (delete-comments string1) (delete-comments string2)))
    ```
    
    And use it to check consistency of output:
    
        ```cl-transcript (:check-consistency ((:output string=/no-comments)))
        (format t "hello~%world")
        .. hello     ; This is the first line.
        .. world     ; This is the second line.
        ```
    
    Just to make sure the above example works, here it is without being
    quoted.
    
    ```common-lisp
    (format t "hello~%world")
    .. hello     ; This is the first line.
    .. world     ; This is the second line.
    ```

<a id="x-28MGL-PAX-3A-40PARSING-20MGL-PAX-3ASECTION-29"></a>

## 10 Parsing

<a id="x-28MGL-PAX-3A-40PARSING-NAMES-20MGL-PAX-3ASECTION-29"></a>

### 10.1 Parsing Names

When encountering a [word][d7b0] such as [`CLASSes`][1f37] in a docstring, PAX
needs to find the [name][88cf], and how that's done varies slightly.
[Codification][f1ab], for example, looks for [interesting][7445] names,
[Navigating Sources in Emacs][3386] for names with [Lisp][30ad] [`DEFINITIONS`][e196], and [Linking][19e3] for names with [any kind of
definition][99b0].

This is not as straightforward as it sounds because it needs to
handle cases like non[`READ`][fe58]able, [`PRINT`][d451]ed, and all the various forms of
[Linking][19e3] in docstrings as well as in comments, and the `(NAME
LOCATIVE)` syntax in [`DEFSECTION`][72b4].

<a id="x-28MGL-PAX-3A-40WORD-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **word**

    A *word* is a string from which we want to extract a [name][88cf]. When
    [Navigating][3386], the word is
    `slime-sexp-at-point` or the label of a [Markdown reference link][8c00] if point
    is over one. Similarly, when [Generating Documentation][2c93], it is a
    non-empty string between whitespace characters in a docstring or the
    label of a [Markdown reference link][8c00].

<a id="x-28MGL-PAX-3A-40RAW-NAME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **raw name**

    A *raw name* is a string from which a [name][88cf] may be read. Raw names
    correspond to an intermediate parsing step between [word][d7b0]s an [name][88cf]s.
    See [Names in Raw Names][016d].

<a id="x-28MGL-PAX-3A-40NAME-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **name**

    A *name* is a [DRef name][5fc4]. That is, a symbol, a
    string or a nested list of the previous associated with a
    definition, whose kind is given by a [locative][7ac8].

Depending on the context, trimming and depluralization may be
enabled (see [Raw Names in Words][f0d5]), while the possible names may be
restricted to symbols (see [Names in Raw Names][016d]).

- *Trimming:* Enabled for [`M-.` Defaulting][460e] and [Codification][f1ab].

- *Depluralization:* Enabled when the [word][d7b0] is part of the normal
  flow of text (i.e. not for [Specific Reflink with Text][fb17],
  [Unspecific Reflink with Text][34b8] and various Elisp functions such as
  `mgl-pax-apropos` unless they determine their argument from buffer
  contents).

- *Symbols only:* This is the case for [Codification][f1ab] and
  [Unspecific Autolink][e2a4] to prevent string-based definitions from
  littering the documentation with links without the control
  provided by explicitly [`IMPORT`][8f46]ing symbols.


For a word, a number of [raw name][f5af]s is generated by trimming
delimiter characters and plural markers, and for each raw name a
number of names are considered until one is found suitable in the
context. The following subsections describe the details of the
parsing algorithm.

<a id="x-28MGL-PAX-3A-40RAW-NAMES-IN-WORDS-20MGL-PAX-3ASECTION-29"></a>

#### 10.1.1 Raw Names in Words

From [word][d7b0]s, [raw name][f5af]s are parsed by trimming some
prefixes and suffixes. For a given word, multiple raw names are
considered in the following order.

1. The entire word.

2. Trimming the following characters from the left of the word:

        #<{;"'`

3. Trimming the following characters from the right of the word:

        ,;:.>}"'`

4. Trimming both of the previous two at the same time.

5. From the result of 4., If a [word][d7b0] ends with what looks like a plural marker (case-insensitive),
then a [name][88cf] is created by removing it. For example, from the word
`BUSES` the plural marker `ES` is removed to produce the name `BUS`.
The list of plural markers considered is `SES` (e.g. `GASSES`),
`ES` (e.g. `BUSES`), `S` (e.g. `CARS`), `ZES` (e.g. `FEZZES`), and
`REN` (e.g. `CHILDREN`).

6. From the result of 4., removing the prefix before the first, and the suffix after the last
uppercase character if it contains at least one lowercase character.


<a id="x-28MGL-PAX-3A-40NAMES-IN-RAW-NAMES-20MGL-PAX-3ASECTION-29"></a>

#### 10.1.2 Names in Raw Names

For each [raw name][f5af] from [Raw Names in Words][f0d5], various [name][88cf]s
may be considered until one is found that's suitable in the context.

The following examples list the names considered for a given raw
name, assuming that [`READTABLE-CASE`][48f1] is `:UPCASE` as well as that `FOO`
and `|Foo|` are interned.

- `"foo"`: `FOO`, `"foo"`, `"FOO"` (rules 1, 2, 3)

- `"FOO"`: `FOO`, `"FOO"` (rules 1, 2)

- `"Foo"`: `"Foo"`, `"FOO"` (rules 2, 3)

- `"|Foo|"`: `|Foo|` (rule 4)

- `"\"foo\""`: `"foo"` (rule 5)

The rules are:

1. If the raw name is not mixed case (i.e. it doesn't have both
   upper- and lowercase characters) and it names an interned
   symbol (subject to the current [Package and Readtable][ab7e]), then that
   symbol is considered as a name.

2. The raw name itself (a string) is considered a name.

3. The raw name upcased or downcased according to
   `READTABLE-CASE` (subject to the [current
   readtable][ab7e]) but still as a string. This
   is to allow `[dref][package]` to refer to the `"DREF"`
   package regardless of whether the symbol `DREF` is interned in
   the current package.

4. If the raw name is explicitly a symbol (it starts with `#\|`),
   and it names an interned symbol (subject to the current
   [Package and Readtable][ab7e]), then that symbol is considered as a name
   and nothing else.

5. If the raw name has an embedded string (it starts with `#\"`) and
   [`READ-FROM-STRING`][d813] can read the embedded string from it, then that
   string is considered as a name and nothing else.


<br/>
For example, when `M-.` is pressed while point is over
`nonREADable.`, the last word of the sentence `It may be
nonREADable.`, the following [raw name][f5af]s are considered until one is
found with a definition:

1. The entire word, `"nonREADable."`.

2. Trimming left does not produce a new raw name.

3. Trimming right removes the dot and gives `"nonREADable"`.

4. Trimming both is the same as trimming right.

5. No plural markers are found.

6. The lowercase prefix and suffix is removed around the uppercase
   core, giving `"READ"`. This names an interned symbol which has a
   definition, so `M-.` will visit it.

When [Generating Documentation][2c93], [Autolink][ec7a]ing behaves similarly.

<a id="x-28MGL-PAX-3A-40PARSING-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 10.2 Parsing Locatives

Locatives are parsed almost as if by [`READ`][fe58]. They are found in
buffer contents around a [word][d7b0] when [`M-.` Defaulting][460e] or
[Generating Documentation][2c93], and in the string entered when
[`M-.` Prompting][ed46], with a similar distinction when
[Browsing Live Documentation][a595].

Parsing deviates from `READ` in the following ways.

- No new symbols are [`INTERN`][b4f0]ed during parsing. If an expression
  contains uninterned symbols, then it is not parsable as a
  locative.

- Read-time evaluation ([#.][ffd7]) follows normal `READ` semantics.
  Thus, `(method ((eql #.(find-package 'cl))))` may `INTERN` the
  symbol `CL`.

- A locative that involves unreadable objects that print using the
  `#<` syntax (e.g. `(METHOD ((EQL #<PACKAGE DREF>)))`) is parsable
  in the context of a [name][88cf] if each unreadable object in the
  locative occurs in one of the [`DEFINITIONS`][e196] of that name and it
  [prints to an equivalent string][2f21] (e.g. `#<PACKAGE DREF>` above).


<a id="x-28MGL-PAX-3A-40PRINTS-TO-AN-EQUIVALENT-STRING-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **prints to an equivalent string**

    An object with an unreadable representation is said to print to
    some string `S` if its [`PRIN1`][6384] representation (under [`WITH-STANDARD-IO-SYNTAX`][39df]
    but in the current package and with [`*PRINT-READABLY*`][8aca] `NIL`) is the same as `S`, where consecutive whitepace characters are
    replaced with a single space in both strings, and the comparison is case-insensitive.
    
    See the related concept of [stable printed locative][3942], that requires
    the printed representation of entire locatives to be unique and
    non-changing to support [Linking][19e3].

<a id="x-28MGL-PAX-3A-40EXTENSION-API-20MGL-PAX-3ASECTION-29"></a>

## 11 Writing Extensions

<a id="x-28MGL-PAX-3A-40ADDING-NEW-LOCATIVES-20MGL-PAX-3ASECTION-29"></a>

### 11.1 Adding New Locatives

Once everything in [Extending DRef][68fb] has been done,
there are only a couple of PAX generic functions left to extend.

<a id="x-28MGL-PAX-3ADOCUMENT-OBJECT-2A-20GENERIC-FUNCTION-29"></a>

- [generic-function] **DOCUMENT-OBJECT\*** *OBJECT STREAM*

    Write `OBJECT` in [`*FORMAT*`][3da8] to `STREAM`.
    Specialize this on a subclass of [`DREF`][d930] if that subclass is
    not [`RESOLVE`][63b4]able, else on the type of object it resolves to. This
    function is for extension only. Don't call it directly.

<a id="x-28MGL-PAX-3AEXPORTABLE-REFERENCE-P-20GENERIC-FUNCTION-29"></a>

- [generic-function] **EXPORTABLE-REFERENCE-P** *PACKAGE SYMBOL LOCATIVE-TYPE LOCATIVE-ARGS*

    Return true if `SYMBOL` is to be exported from
    `PACKAGE` when it occurs in a [`DEFSECTION`][72b4] in a reference with
    `LOCATIVE-TYPE` and `LOCATIVE-ARGS`. `SYMBOL` is [accessible][9b15] in
    `PACKAGE`.
    
    The default method calls [`EXPORTABLE-LOCATIVE-TYPE-P`][c930] with
    `LOCATIVE-TYPE` and ignores the other arguments.
    
    By default, [`SECTION`][5fac]s and [`GLOSSARY-TERM`][8251]s are not exported although
    they are `EXPORTABLE-LOCATIVE-TYPE-P`. To export symbols naming
    sections from MGL-PAX, the following method could be added:
    
    ```
    (defmethod exportable-reference-p ((package (eql (find-package 'mgl-pax)))
                                       symbol (locative-type (eql 'section))
                                       locative-args)
      t)
    ```

<a id="x-28MGL-PAX-3AEXPORTABLE-LOCATIVE-TYPE-P-20GENERIC-FUNCTION-29"></a>

- [generic-function] **EXPORTABLE-LOCATIVE-TYPE-P** *LOCATIVE-TYPE*

    Return true if symbols in references with
    `LOCATIVE-TYPE` are to be exported by default when they occur in a
    [`DEFSECTION`][72b4]. The default method returns `T`, while the methods for
    locative types [`SECTION`][672f], [`GLOSSARY-TERM`][5119],
    [`PACKAGE`][4dd7], [`ASDF:SYSTEM`][c097], [`METHOD`][51c3] and
    [`INCLUDE`][5cd7] return `NIL`.
    
    This function is called by the default method of
    [`EXPORTABLE-REFERENCE-P`][e51f] to decide what symbols `DEFSECTION` shall
    export when its `EXPORT` argument is true.

Also note that due to the [Home Section][bdd5] logic, especially for
locative types with string names, [`DREF-EXT:DOCSTRING*`][9fd4] should
probably return a non-`NIL` package.

<a id="x-28MGL-PAX-3A-40LOCATIVE-ALIASES-20MGL-PAX-3ASECTION-29"></a>

### 11.2 Locative Aliases

[`DEFINE-LOCATIVE-ALIAS`][548e] can be used to help [`M-.`][3386] and [Specific Autolink][38de]s disambiguate
references based on the context of a [name][88cf] as described on [Parsing][378f].

The following example shows how to make docstrings read
more naturally by defining an alias.

```
(defclass my-string ()
  ())

(defgeneric my-string (obj)
  (:documentation "Convert OBJ to MY-STRING."))

;;; This version of FOO has a harder to read docstring because
;;; it needs to disambiguate the MY-STRING reference.
(defun foo (x)
  "FOO takes and argument X, a [MY-STRING][class] object.")

;;; Define OBJECT as an alias for the CLASS locative.
(define-locative-alias object class)

;;; Note how no explicit link is needed anymore.
(defun foo (x)
  "FOO takes an argument X, a MY-CLASS object.")
```

Similarly, defining the indefinite articles as aliases of the [`CLASS`][2060]
locative can reduce the need for explicit linking.

```
(define-locative-alias a class)
(define-locative-alias an class)
```

Since these are unlikely to be universally helpful, make sure not to
export the symbols `A` and `AN`.

<a id="x-28MGL-PAX-3A-40EXTENDING-DOCUMENT-20MGL-PAX-3ASECTION-29"></a>

### 11.3 Extending `DOCUMENT`

For all definitions that it encounters, [`DOCUMENT`][432c] calls
[`DOCUMENT-OBJECT*`][8269] to generate documentation. The following utilities
are for writing new `DOCUMENT-OBJECT*` methods, which emit Markdown.

<a id="x-28MGL-PAX-3A-2AFORMAT-2A-20VARIABLE-29"></a>

- [variable] **\*FORMAT\***

    Bound by [`DOCUMENT`][432c] to its `FORMAT` argument, this allows Markdown
    output to depend on the output format.

<a id="x-28MGL-PAX-3AWITH-HEADING-20MGL-PAX-3AMACRO-29"></a>

- [macro] **WITH-HEADING** *(STREAM &KEY DREF LINK-TITLE-TO) &BODY BODY*

    Write a Markdown heading with the [`DOCTITLE`][e619] of `DREF` to `STREAM`.
    
    - `DREF` defaults to the definition for which documentation is
      currently being generated.
    
    - Nested `WITH-HEADING`s produce nested headings.
    
    - If [`*DOCUMENT-LINK-SECTIONS*`][1b28], generate anchors based on `DREF`.
    
    - `LINK-TITLE-TO` behaves like the `LINK-TITLE-TO` argument of
      [`DEFSECTION`][72b4].

<a id="x-28MGL-PAX-3ADOCTITLE-2A-20GENERIC-FUNCTION-29"></a>

- [generic-function] **DOCTITLE\*** *OBJECT*

    `DOCTITLE*` extends [`DOCTITLE`][e619] in the same way
    as [`DOCSTRING*`][9fd4] extends [`DOCSTRING`][affc].
    
    The default method returns `NIL`.
    
    This function is for extension only. Do not call it directly.

<a id="x-28MGL-PAX-3ADOCUMENTING-DEFINITION-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DOCUMENTING-DEFINITION** *(STREAM &KEY DREF PACKAGE READTABLE (ARGLIST NIL)) &BODY BODY*

    Write `DREF` to `STREAM` as described in
    [`*DOCUMENT-MARK-UP-SIGNATURES*`][8fb6], and establish `DREF` as a
     [Local Definition][9db9] for the processing of `BODY`.
    
    - `DREF` defaults to the definition for which documentation is
      currently being generated.
    
    - If `DREF` has a [`DOCTITLE`][e619], then it is [`PRINC`][676d]ed after the
      [`LOCATIVE-TYPE`][97ba] (see [Markdown in Titles][165c]). Else, `(DREF-NAME DREF)`
      is printed subject to [`*DOCUMENT-DOWNCASE-UPPERCASE-CODE*`][a5ee] but with
      all Markdown and [MathJax][a17d] markup escaped.
    
    - [`*PACKAGE*`][5ed1] and [`*READTABLE*`][b79a] are bound to `PACKAGE` and `READTABLE` for
      the duration of printing the `ARGLIST` and the processing of `BODY`.
      If either is `NIL`, then a default value is computed as described in
      [Package and Readtable][ab7e].
    
    - `ARGLIST`:
    
        - If it is not provided, then it defaults to (`ARGLIST` `DREF`).
    
        - If `NIL`, then it is not printed.
    
        - If it is a list, then it is must be a [lambda list][98ff] and
          is printed without the outermost parens and with the package
          names removed from the argument names.
    
        - If its is a string, then it must be valid Markdown.
    
    - It is not allowed to have [`WITH-HEADING`][80e8] within the [dynamic
      extent][36e9] of `BODY`.

<a id="x-28MGL-PAX-3AWITH-DISLOCATED-NAMES-20MGL-PAX-3AMACRO-29"></a>

- [macro] **WITH-DISLOCATED-NAMES** *NAMES &BODY BODY*

    For each name in `NAMES`, establish a [Local Definition][9db9].

<a id="x-28MGL-PAX-3ADOCUMENT-DOCSTRING-20FUNCTION-29"></a>

- [function] **DOCUMENT-DOCSTRING** *DOCSTRING STREAM &KEY (INDENTATION "    ") EXCLUDE-FIRST-LINE-P (PARAGRAPHP T)*

    Write `DOCSTRING` to `STREAM`, [sanitizing the Markdown][7bf5] from it, performing [Codification][f1ab] and
    [Linking][19e3], finally prefixing each line with `INDENTATION`. The prefix
    is not added to the first line if `EXCLUDE-FIRST-LINE-P`. If
    `PARAGRAPHP`, then add a newline before and after the output.

<a id="x-28MGL-PAX-3AESCAPE-MARKDOWN-20FUNCTION-29"></a>

- [function] **ESCAPE-MARKDOWN** *STRING &KEY (ESCAPE-INLINE T) (ESCAPE-MATHJAX T) (ESCAPE-HTML T) (ESCAPE-BLOCK T)*

    Backslash escape Markdown constructs in `STRING`.
    
    - If `ESCAPE-INLINE`, then escape the following characters:
    
            *_`[]\
    
    - If `ESCAPE-MATHJAX`, then escape `$` characters.
    
    - If `ESCAPE-HTML`, then escape the following characters:
    
            <&
    
    - If `ESCAPE-BLOCK`, then escape whatever is necessary to avoid
      starting a new Markdown block (e.g. a paragraph, heading, etc).

<a id="x-28MGL-PAX-3APRIN1-TO-MARKDOWN-20FUNCTION-29"></a>

- [function] **PRIN1-TO-MARKDOWN** *OBJECT &KEY (ESCAPE-INLINE T) (ESCAPE-MATHJAX T) (ESCAPE-HTML T) (ESCAPE-BLOCK T)*

    Like [`PRIN1-TO-STRING`][18e1], but bind [`*PRINT-CASE*`][443b] depending on
    [`*DOCUMENT-DOWNCASE-UPPERCASE-CODE*`][a5ee] and [`*FORMAT*`][3da8], and
    [`ESCAPE-MARKDOWN`][3026].

<a id="x-28MGL-PAX-3A-40SECTIONS-20MGL-PAX-3ASECTION-29"></a>

### 11.4 Sections

[`SECTION`][5fac] objects rarely need to be dissected since
[`DEFSECTION`][72b4] and [`DOCUMENT`][432c] cover most needs. However, it is plausible
that one wants to subclass them and maybe redefine how they are
presented.

<a id="x-28MGL-PAX-3ASECTION-20CLASS-29"></a>

- [class] **SECTION**

    [`DEFSECTION`][72b4] stores its `NAME`, `TITLE`, [`PACKAGE`][1d5a],
    [`READTABLE`][d646] and `ENTRIES` arguments in [`SECTION`][5fac]
    objects.

<a id="x-28MGL-PAX-3ASECTION-NAME-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>

- [reader] **SECTION-NAME** *[SECTION][5fac] (:NAME)*

    The name of the global variable whose value is
    this [`SECTION`][5fac] object.

<a id="x-28MGL-PAX-3ASECTION-PACKAGE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>

- [reader] **SECTION-PACKAGE** *[SECTION][5fac] (:PACKAGE)*

    [`*PACKAGE*`][5ed1] will be bound to this package when
    generating documentation for this section.

<a id="x-28MGL-PAX-3ASECTION-READTABLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>

- [reader] **SECTION-READTABLE** *[SECTION][5fac] (:READTABLE)*

    [`*READTABLE*`][b79a] will be bound to this when generating
    documentation for this section.

<a id="x-28MGL-PAX-3ASECTION-TITLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29"></a>

- [reader] **SECTION-TITLE** *[SECTION][5fac] (:TITLE)*

    A [title][090e] or `NIL`. Used in generated
    documentation (see [Markdown Output][dd29]) and is returned by [`DOCTITLE`][e619]
    for [`SECTION`][5fac] objects and `SECTION` [definition][2143]s.

<a id="x-28MGL-PAX-3ASECTION-LINK-TITLE-TO-20FUNCTION-29"></a>

- [function] **SECTION-LINK-TITLE-TO** *SECTION*

<a id="x-28MGL-PAX-3ASECTION-ENTRIES-20FUNCTION-29"></a>

- [function] **SECTION-ENTRIES** *SECTION*

    A list of Markdown docstrings and [`XREF`][1538]s in the order they
    occurred in [`DEFSECTION`][72b4].

<a id="x-28MGL-PAX-3A-40GLOSSARY-TERMS-20MGL-PAX-3ASECTION-29"></a>

### 11.5 Glossary Terms

[`GLOSSARY-TERM`][8251] objects rarely need to be dissected since
[`DEFINE-GLOSSARY-TERM`][8ece] and [`DOCUMENT`][432c] cover most needs. However, it is
plausible that one wants to subclass them and maybe redefine how
they are presented.

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-20CLASS-29"></a>

- [class] **GLOSSARY-TERM**

    See [`DEFINE-GLOSSARY-TERM`][8ece].

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-NAME-20-28MGL-PAX-3AREADER-20MGL-PAX-3AGLOSSARY-TERM-29-29"></a>

- [reader] **GLOSSARY-TERM-NAME** *[GLOSSARY-TERM][8251] (:NAME)*

    The name of the global variable whose value is
    this [`GLOSSARY-TERM`][8251] object.

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-TITLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3AGLOSSARY-TERM-29-29"></a>

- [reader] **GLOSSARY-TERM-TITLE** *[GLOSSARY-TERM][8251] (:TITLE)*

    A [title][090e] or `NIL`. Used in generated
    documentation (see [Markdown Output][dd29]) and is returned by [`DOCTITLE`][e619]
    for [`GLOSSARY-TERM`][8251] objects and `GLOSSARY-TERM` [definition][2143]s..

<a id="x-28MGL-PAX-3AGLOSSARY-TERM-URL-20-28MGL-PAX-3AREADER-20MGL-PAX-3AGLOSSARY-TERM-29-29"></a>

- [reader] **GLOSSARY-TERM-URL** *[GLOSSARY-TERM][8251] (:URL)*

    A string or `NIL`.

  [00d4]: dref/README.md#x-28MGL-PAX-3AACCESSOR-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:ACCESSOR MGL-PAX:LOCATIVE"
  [0165]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cic.htm "\"22.3.9.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [016d]: #x-28MGL-PAX-3A-40NAMES-IN-RAW-NAMES-20MGL-PAX-3ASECTION-29 "Names in Raw Names"
  [021a]: dref/README.md#x-28-22dref-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "\"dref\" ASDF/SYSTEM:SYSTEM"
  [02e3]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cib.htm "\"22.3.9.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [0317]: http://www.lispworks.com/documentation/HyperSpec/Body/t_pn.htm "PATHNAME (MGL-PAX:CLHS CLASS)"
  [0361]: #x-28MGL-PAX-3A-40SPECIFIC-LINK-20MGL-PAX-3ASECTION-29 "Specific Link"
  [040b]: http://www.lispworks.com/documentation/HyperSpec/Body/02_da.htm "\"2.4.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [051b]: http://www.lispworks.com/documentation/HyperSpec/Body/22_chb.htm "\"22.3.8.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [0617]: dref/README.md#x-28DREF-3AXREF-3D-20FUNCTION-29 "DREF:XREF= FUNCTION"
  [0684]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cac.htm "\"22.3.1.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [0702]: #x-28MGL-PAX-3A-40DOCUMENTABLE-20MGL-PAX-3ASECTION-29 "`DOCUMENTABLE`"
  [090c]: dref/README.md#x-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:STRUCTURE-ACCESSOR MGL-PAX:LOCATIVE"
  [090e]: #x-28MGL-PAX-3A-40TITLE-20MGL-PAX-3AGLOSSARY-TERM-29 "title"
  [0b3a]: dref/README.md#x-28MGL-PAX-3ALOCATIVE-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:LOCATIVE MGL-PAX:LOCATIVE"
  [0c4f]: http://www.lispworks.com/documentation/HyperSpec/Body/f_export.htm "EXPORT (MGL-PAX:CLHS FUNCTION)"
  [0c7e]: dref/README.md#x-28-22dref-2Ffull-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "\"dref/full\" ASDF/SYSTEM:SYSTEM"
  [0cac]: http://www.lispworks.com/documentation/HyperSpec/Body/22_caa.htm "\"22.3.1.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [0d07]: http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_2.htm "SYMBOL-NAME (MGL-PAX:CLHS FUNCTION)"
  [0d6e]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eval.htm "EVAL (MGL-PAX:CLHS FUNCTION)"
  [0db0]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cgf.htm "\"22.3.7.6\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [0ef0]: #x-28MGL-PAX-3A-2ADOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS* VARIABLE"
  [0f42]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dht.htm "\"2.4.8.20\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [0f52]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_t.htm#top_level_form "\"top level form\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [0fa3]: #x-28MGL-PAX-3A-40LOCATIVE-ALIASES-20MGL-PAX-3ASECTION-29 "Locative Aliases"
  [119e]: http://www.lispworks.com/documentation/HyperSpec/Body/t_fn.htm "FUNCTION (MGL-PAX:CLHS CLASS)"
  [11f1]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cfb.htm "\"22.3.6.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [1281]: #x-28MGL-PAX-3A-40PAX-WORLD-20MGL-PAX-3ASECTION-29 "PAX World"
  [130a]: dref/README.md#x-28DREF-EXT-3ALOCATIVE-TYPE-DIRECT-SUBS-20FUNCTION-29 "DREF-EXT:LOCATIVE-TYPE-DIRECT-SUBS FUNCTION"
  [1322]: https://help.github.com/articles/github-flavored-markdown#fenced-code-blocks "fenced code blocks"
  [13a9]: #x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-READMES-20FUNCTION-29 "MGL-PAX:UPDATE-ASDF-SYSTEM-READMES FUNCTION"
  [13c7]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_a.htm#atomic "\"atomic\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [142c]: http://www.lispworks.com/documentation/HyperSpec/Body/06_aba.htm "\"6.1.2.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [1538]: dref/README.md#x-28DREF-3AXREF-20CLASS-29 "DREF:XREF CLASS"
  [1539]: https://quicklisp.org/ "Quicklisp"
  [1567]: http://www.lispworks.com/documentation/HyperSpec/Body/22_ccb.htm "\"22.3.3.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [165c]: #x-28MGL-PAX-3A-40MARKDOWN-IN-TITLES-20MGL-PAX-3ASECTION-29 "Markdown in Titles"
  [172e]: dref/README.md#x-28METHOD-20MGL-PAX-3ALOCATIVE-29 "METHOD MGL-PAX:LOCATIVE"
  [1743]: https://emacs-w3m.github.io/info/emacs-w3m_10.html#Key-Binding "w3m's default key bindings"
  [17e0]: #x-28MGL-PAX-3A-2ADOCUMENT-URL-VERSIONS-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-URL-VERSIONS* VARIABLE"
  [1867]: http://www.lispworks.com/documentation/HyperSpec/Body/r_contin.htm "CONTINUE (MGL-PAX:CLHS RESTART)"
  [18e1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_to_.htm "PRIN1-TO-STRING (MGL-PAX:CLHS FUNCTION)"
  [1904]: https://github.com/3b/3bmd "3BMD"
  [1959]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cec.htm "\"22.3.5.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [19ad]: #x-28MGL-PAX-3A-40PDF-OUTPUT-20MGL-PAX-3ASECTION-29 "PDF Output"
  [19e3]: #x-28MGL-PAX-3A-40LINKING-20MGL-PAX-3ASECTION-29 "Linking"
  [1aed]: #x-28MGL-PAX-3AINSTALL-PAX-ELISP-20FUNCTION-29 "MGL-PAX:INSTALL-PAX-ELISP FUNCTION"
  [1b1b]: #x-28MGL-PAX-3A-40DOCUMENTATION-UTILITIES-20MGL-PAX-3ASECTION-29 "Utilities for Generating Documentation"
  [1b28]: #x-28MGL-PAX-3A-2ADOCUMENT-LINK-SECTIONS-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-LINK-SECTIONS* VARIABLE"
  [1d1d]: dref/README.md#x-28DREF-3A-40BASIC-LOCATIVE-TYPES-20MGL-PAX-3ASECTION-29 "Basic Locative Types"
  [1d5a]: http://www.lispworks.com/documentation/HyperSpec/Body/t_pkg.htm "PACKAGE (MGL-PAX:CLHS CLASS)"
  [1e36]: dref/README.md#x-28DREF-3ADREF-NAME-20-28MGL-PAX-3AREADER-20DREF-3ADREF-29-29 "DREF:DREF-NAME (MGL-PAX:READER DREF:DREF)"
  [1f37]: http://www.lispworks.com/documentation/HyperSpec/Body/t_class.htm "CLASS (MGL-PAX:CLHS CLASS)"
  [1f99]: http://www.lispworks.com/documentation/HyperSpec/Body/t_array.htm "ARRAY (MGL-PAX:CLHS CLASS)"
  [2038]: http://www.lispworks.com/documentation/HyperSpec/Body/t_stu_ob.htm "STRUCTURE-OBJECT (MGL-PAX:CLHS CLASS)"
  [2060]: dref/README.md#x-28CLASS-20MGL-PAX-3ALOCATIVE-29 "CLASS MGL-PAX:LOCATIVE"
  [2143]: dref/README.md#x-28DREF-3A-40DEFINITION-20MGL-PAX-3AGLOSSARY-TERM-29 "definition"
  [21f4]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_l.htm#loop_keyword "\"loop keyword\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [225d]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhn.htm "\"2.4.8.14\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [227d]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhi.htm "\"2.4.8.9\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [22c2]: #x-28MGL-PAX-3A-40LINKING-TO-SECTIONS-20MGL-PAX-3ASECTION-29 "Linking to Sections"
  [2352]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cfa.htm "\"22.3.6.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [238c]: #x-28MGL-PAX-3ATRANSCRIPTION-VALUES-CONSISTENCY-ERROR-20CONDITION-29 "MGL-PAX:TRANSCRIPTION-VALUES-CONSISTENCY-ERROR CONDITION"
  [2415]: README.md "PAX Manual"
  [2444]: dref/README.md#x-28DREF-EXT-3ALOCATIVE-ARGS-20FUNCTION-29 "DREF-EXT:LOCATIVE-ARGS FUNCTION"
  [2634]: #x-28MGL-PAX-3A-40OVERVIEW-OF-ESCAPING-20MGL-PAX-3ASECTION-29 "Overview of Escaping"
  [2826]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhj.htm "\"2.4.8.10\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [292a]: #x-28MGL-PAX-3A-40PAX-LOCATIVES-20MGL-PAX-3ASECTION-29 "PAX Locatives"
  [2c93]: #x-28MGL-PAX-3A-40GENERATING-DOCUMENTATION-20MGL-PAX-3ASECTION-29 "Generating Documentation"
  [2ca9]: #x-28MGL-PAX-3AOUTPUT-REFLINK-20FUNCTION-29 "MGL-PAX:OUTPUT-REFLINK FUNCTION"
  [2cd5]: #x-28MGL-PAX-3A-2ADOCUMENT-PANDOC-PDF-HEADER-INCLUDES-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-PANDOC-PDF-HEADER-INCLUDES* VARIABLE"
  [2d48]: #x-28MGL-PAX-3AWITH-DISLOCATED-NAMES-20MGL-PAX-3AMACRO-29 "MGL-PAX:WITH-DISLOCATED-NAMES MGL-PAX:MACRO"
  [2f21]: #x-28MGL-PAX-3A-40PRINTS-TO-AN-EQUIVALENT-STRING-20MGL-PAX-3AGLOSSARY-TERM-29 "prints to an equivalent string"
  [3026]: #x-28MGL-PAX-3AESCAPE-MARKDOWN-20FUNCTION-29 "MGL-PAX:ESCAPE-MARKDOWN FUNCTION"
  [3076]: https://github.com/redline6561/colorize/ "Colorize"
  [309c]: http://www.lispworks.com/documentation/HyperSpec/Body/02_df.htm "\"2.4.6\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [30ad]: dref/README.md#x-28DREF-3ALISP-LOCATIVE-TYPES-20FUNCTION-29 "DREF:LISP-LOCATIVE-TYPES FUNCTION"
  [31c5]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cea.htm "\"22.3.5.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [32da]: dref/README.md#x-28DREF-3ASOURCE-LOCATION-20FUNCTION-29 "DREF:SOURCE-LOCATION FUNCTION"
  [3386]: #x-28MGL-PAX-3A-40NAVIGATING-IN-EMACS-20MGL-PAX-3ASECTION-29 "Navigating Sources in Emacs"
  [342d]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhs.htm "\"2.4.8.19\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [3473]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_s.htm "FIND-SYMBOL (MGL-PAX:CLHS FUNCTION)"
  [34b8]: #x-28MGL-PAX-3A-40UNSPECIFIC-REFLINK-WITH-TEXT-20MGL-PAX-3ASECTION-29 "Unspecific Reflink with Text"
  [35a2]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#setf_expander "\"setf expander\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [36e1]: #x-28MGL-PAX-3A-40HTML-OUTPUT-20MGL-PAX-3ASECTION-29 "HTML Output"
  [36e9]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#dynamic_extent "\"dynamic extent\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [378f]: #x-28MGL-PAX-3A-40PARSING-20MGL-PAX-3ASECTION-29 "Parsing"
  [3808]: http://www.lispworks.com/documentation/HyperSpec/Body/f_terpri.htm "FRESH-LINE (MGL-PAX:CLHS FUNCTION)"
  [3831]: http://www.lispworks.com/documentation/HyperSpec/Body/v_sl_sls.htm "/// (MGL-PAX:CLHS VARIABLE)"
  [38de]: #x-28MGL-PAX-3A-40SPECIFIC-AUTOLINK-20MGL-PAX-3ASECTION-29 "Specific Autolink"
  [3942]: #x-28MGL-PAX-3A-40STABLE-PRINTED-LOCATIVE-20MGL-PAX-3AGLOSSARY-TERM-29 "stable printed locative"
  [3972]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_r.htm#reader_macro "\"reader macro\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [39df]: http://www.lispworks.com/documentation/HyperSpec/Body/m_w_std_.htm "WITH-STANDARD-IO-SYNTAX (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [3da8]: #x-28MGL-PAX-3A-2AFORMAT-2A-20VARIABLE-29 "MGL-PAX:*FORMAT* VARIABLE"
  [3e6e]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cbb.htm "\"22.3.2.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [3f2e]: http://www.lispworks.com/documentation/HyperSpec/Body/f_pr_obj.htm "PRINT-OBJECT (MGL-PAX:CLHS GENERIC-FUNCTION)"
  [3fa1]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cad.htm "\"22.3.1.4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [3fb5]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm "EQUAL (MGL-PAX:CLHS FUNCTION)"
  [407c]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_h.htm#home_package "\"home package\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [4143]: http://www.lispworks.com/documentation/HyperSpec/Body/f_stgeq_.htm "STRING= (MGL-PAX:CLHS FUNCTION)"
  [4317]: http://www.lispworks.com/documentation/HyperSpec/Body/f_cerror.htm "CERROR (MGL-PAX:CLHS FUNCTION)"
  [432c]: #x-28MGL-PAX-3ADOCUMENT-20FUNCTION-29 "MGL-PAX:DOCUMENT FUNCTION"
  [43bd]: dref/README.md#x-28DREF-3A-40REFERENCE-20MGL-PAX-3AGLOSSARY-TERM-29 "reference"
  [443b]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_cas.htm "*PRINT-CASE* (MGL-PAX:CLHS VARIABLE)"
  [4537]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pl_plp.htm "+++ (MGL-PAX:CLHS VARIABLE)"
  [460e]: #x-28MGL-PAX-3A-40M--2E-DEFAULTING-20MGL-PAX-3ASECTION-29 "`M-.` Defaulting"
  [4796]: dref/README.md#x-28LAMBDA-20MGL-PAX-3ALOCATIVE-29 "LAMBDA MGL-PAX:LOCATIVE"
  [479a]: http://www.lispworks.com/documentation/HyperSpec/Body/f_abortc.htm "ABORT (MGL-PAX:CLHS FUNCTION)"
  [4841]: http://www.lispworks.com/documentation/HyperSpec/Body/f_smp_cn.htm "SIMPLE-CONDITION-FORMAT-CONTROL (MGL-PAX:CLHS FUNCTION)"
  [48f1]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rdtabl.htm "READTABLE-CASE (MGL-PAX:CLHS FUNCTION)"
  [4bb8]: #x-28-22mgl-pax-2Fdocument-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "\"mgl-pax/document\" ASDF/SYSTEM:SYSTEM"
  [4c39]: #x-28MGL-PAX-3A-40TRANSCRIPT-CONSISTENCY-CHECKING-20MGL-PAX-3ASECTION-29 "Transcript Consistency Checking"
  [4dd7]: dref/README.md#x-28PACKAGE-20MGL-PAX-3ALOCATIVE-29 "PACKAGE MGL-PAX:LOCATIVE"
  [4e05]: #x-28MGL-PAX-3A-40UNSPECIFIC-REFLINK-20MGL-PAX-3ASECTION-29 "Unspecific Reflink"
  [5119]: #x-28MGL-PAX-3AGLOSSARY-TERM-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:GLOSSARY-TERM MGL-PAX:LOCATIVE"
  [51c3]: http://www.lispworks.com/documentation/HyperSpec/Body/t_method.htm "METHOD (MGL-PAX:CLHS CLASS)"
  [5225]: dref/README.md "DRef Manual"
  [5483]: http://www.lispworks.com/documentation/HyperSpec/Body/v__.htm "- (MGL-PAX:CLHS VARIABLE)"
  [548e]: dref/README.md#x-28DREF-EXT-3ADEFINE-LOCATIVE-ALIAS-20MGL-PAX-3AMACRO-29 "DREF-EXT:DEFINE-LOCATIVE-ALIAS MGL-PAX:MACRO"
  [54d8]: #x-28MGL-PAX-3A-40ADDING-NEW-LOCATIVES-20MGL-PAX-3ASECTION-29 "Adding New Locatives"
  [550b]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cgd.htm "\"22.3.7.4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [56ba]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dd.htm "\"2.4.4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [574a]: #x-28MGL-PAX-3A-40EXTENDING-DOCUMENT-20MGL-PAX-3ASECTION-29 "Extending `DOCUMENT`"
  [5825]: #x-28-22mgl-pax-2Ftranscribe-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "\"mgl-pax/transcribe\" ASDF/SYSTEM:SYSTEM"
  [5875]: dref/README.md#x-28GENERIC-FUNCTION-20MGL-PAX-3ALOCATIVE-29 "GENERIC-FUNCTION MGL-PAX:LOCATIVE"
  [587f]: #x-28MGL-PAX-3AMAKE-GIT-SOURCE-URI-FN-20FUNCTION-29 "MGL-PAX:MAKE-GIT-SOURCE-URI-FN FUNCTION"
  [5884]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_.htm "FIND-IF (MGL-PAX:CLHS FUNCTION)"
  [58f6]: #x-28GO-20MGL-PAX-3ALOCATIVE-29 "GO MGL-PAX:LOCATIVE"
  [59d9]: https://pandoc.org/ "Pandoc"
  [5a82]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm "EQ (MGL-PAX:CLHS FUNCTION)"
  [5b4d]: http://www.lispworks.com/documentation/HyperSpec/Body/22_ccd.htm "\"22.3.3.4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [5cd7]: #x-28MGL-PAX-3AINCLUDE-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:INCLUDE MGL-PAX:LOCATIVE"
  [5ed1]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pkg.htm "*PACKAGE* (MGL-PAX:CLHS VARIABLE)"
  [5fac]: #x-28MGL-PAX-3ASECTION-20CLASS-29 "MGL-PAX:SECTION CLASS"
  [5fc4]: dref/README.md#x-28DREF-3A-40NAME-20MGL-PAX-3AGLOSSARY-TERM-29 "name"
  [5fd4]: http://www.lispworks.com/documentation/HyperSpec/Body/t_eql.htm "EQL (MGL-PAX:CLHS TYPE)"
  [6067]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#destructuring_lambda_list "\"destructuring lambda list\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [60e4]: http://www.lispworks.com/documentation/HyperSpec/Body/22_chc.htm "\"22.3.8.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [6300]: #x-28MGL-PAX-3A-40TRANSCRIPTS-20MGL-PAX-3ASECTION-29 "Transcripts"
  [6334]: dref/README.md#x-28DREF-EXT-3ALOCATE-ERROR-20CONDITION-29 "DREF-EXT:LOCATE-ERROR CONDITION"
  [6384]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRIN1 (MGL-PAX:CLHS FUNCTION)"
  [63b4]: dref/README.md#x-28DREF-3ARESOLVE-20FUNCTION-29 "DREF:RESOLVE FUNCTION"
  [63ef]: http://www.lispworks.com/documentation/HyperSpec/Issues/iss009_w.htm "\"ISSUE:AREF-1D\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [63f3]: #x-28MGL-PAX-3ADEFINE-PACKAGE-20MGL-PAX-3AMACRO-29 "MGL-PAX:DEFINE-PACKAGE MGL-PAX:MACRO"
  [64be]: #x-28MGL-PAX-3AUNRESOLVABLE-REFLINK-20CONDITION-29 "MGL-PAX:UNRESOLVABLE-REFLINK CONDITION"
  [6547]: http://www.lispworks.com/documentation/HyperSpec/Body/f_open.htm "OPEN (MGL-PAX:CLHS FUNCTION)"
  [659d]: #x-28MGL-PAX-3A-40BROWSE-BY-LOCATIVE-TYPE-20MGL-PAX-3ASECTION-29 "Browse by Locative Types"
  [65b4]: dref/README.md#x-28DREF-3ADREF-APROPOS-20FUNCTION-29 "DREF:DREF-APROPOS FUNCTION"
  [65bc]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cae.htm "\"22.3.1.5\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [66c6]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*ERROR-OUTPUT* (MGL-PAX:CLHS VARIABLE)"
  [672f]: #x-28MGL-PAX-3ASECTION-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:SECTION MGL-PAX:LOCATIVE"
  [676d]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRINC (MGL-PAX:CLHS FUNCTION)"
  [6832]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defmet.htm "DEFMETHOD (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [685e]: #x-28MGL-PAX-3A-40INTRODUCTION-20MGL-PAX-3ASECTION-29 "Introduction"
  [6897]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cbc.htm "\"22.3.2.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [68c1]: https://daringfireball.net/projects/markdown/syntax#code "Markdown inline code"
  [68d2]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhh.htm "\"2.4.8.8\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [68fb]: dref/README.md#x-28DREF-EXT-3A-40EXTENDING-DREF-20MGL-PAX-3ASECTION-29 "Extending DRef"
  [698d]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dh.htm "\"2.4.8\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [6ab0]: #x-28MGL-PAX-3A-2ADOCUMENT-FANCY-HTML-NAVIGATION-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-FANCY-HTML-NAVIGATION* VARIABLE"
  [6af6]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PPRINT (MGL-PAX:CLHS FUNCTION)"
  [6b59]: #x-28MGL-PAX-3A-40TRANSCRIPT-DYNENV-20MGL-PAX-3ASECTION-29 "Controlling the Dynamic Environment"
  [6be7]: https://slime.common-lisp.dev/ "SLIME"
  [6c83]: dref/README.md#x-28VARIABLE-20MGL-PAX-3ALOCATIVE-29 "VARIABLE MGL-PAX:LOCATIVE"
  [6e18]: #x-28MGL-PAX-3A-40TRANSCRIPT-FINER-GRAINED-CONSISTENCY-CHECKS-20MGL-PAX-3ASECTION-29 "Finer-Grained Consistency Checks"
  [6f51]: http://www.lispworks.com/documentation/HyperSpec/Body/r_muffle.htm "MUFFLE-WARNING (MGL-PAX:CLHS RESTART)"
  [6fdb]: #x-28-22mgl-pax-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "\"mgl-pax\" ASDF/SYSTEM:SYSTEM"
  [7163]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhl.htm "\"2.4.8.12\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [72a7]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pl_plp.htm "+ (MGL-PAX:CLHS VARIABLE)"
  [72b4]: #x-28MGL-PAX-3ADEFSECTION-20MGL-PAX-3AMACRO-29 "MGL-PAX:DEFSECTION MGL-PAX:MACRO"
  [72cc]: #x-28MGL-PAX-3AENSURE-WEB-SERVER-20FUNCTION-29 "MGL-PAX:ENSURE-WEB-SERVER FUNCTION"
  [730f]: #x-28MGL-PAX-3A-2ADISCARD-DOCUMENTATION-P-2A-20VARIABLE-29 "MGL-PAX:*DISCARD-DOCUMENTATION-P* VARIABLE"
  [7439]: https://emacs-w3m.github.io/info/emacs-w3m.html "w3m"
  [7445]: #x-28MGL-PAX-3A-40INTERESTING-20MGL-PAX-3AGLOSSARY-TERM-29 "interesting"
  [7506]: dref/README.md#x-28READTABLE-20MGL-PAX-3ALOCATIVE-29 "READTABLE MGL-PAX:LOCATIVE"
  [76ab]: http://www.lispworks.com/documentation/HyperSpec/Body/22_ccc.htm "\"22.3.3.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [76df]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cbd.htm "\"22.3.2.4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [76ea]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cgb.htm "\"22.3.7.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [78d1]: http://www.lispworks.com/documentation/HyperSpec/Body/v__stst.htm "** (MGL-PAX:CLHS VARIABLE)"
  [7a4e]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#section "\"section\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [7a7f]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhc.htm "\"2.4.8.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [7ac8]: dref/README.md#x-28DREF-3A-40LOCATIVE-20MGL-PAX-3AGLOSSARY-TERM-29 "locative"
  [7b6f]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dha.htm "\"2.4.8.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [7bd6]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cab.htm "\"22.3.1.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [7bf5]: #x-28MGL-PAX-3A-40MARKDOWN-IN-DOCSTRINGS-20MGL-PAX-3ASECTION-29 "Markdown in Docstrings"
  [7c9f]: http://www.lispworks.com/documentation/HyperSpec/Body/d_type.htm "TYPE (MGL-PAX:CLHS DECLARATION)"
  [7cc3]: #x-28MGL-PAX-3A-40LINKING-TO-THE-HYPERSPEC-20MGL-PAX-3ASECTION-29 "Linking to the HyperSpec"
  [7d18]: http://c2.com/cgi/wiki?OnceAndOnlyOnce "OAOO"
  [7dc7]: #x-28MGL-PAX-3A-40DOCUMENT-RETURN-20MGL-PAX-3ASECTION-29 "Return Values"
  [7e92]: dref/README.md#x-28DREF-3ADREF-20FUNCTION-29 "DREF:DREF FUNCTION"
  [7eb5]: #x-28MGL-PAX-3A-40LINKABLE-20MGL-PAX-3AGLOSSARY-TERM-29 "linkable"
  [7f1f]: #x-28MGL-PAX-3ADOCUMENT-DOCSTRING-20FUNCTION-29 "MGL-PAX:DOCUMENT-DOCSTRING FUNCTION"
  [7f9a]: http://www.lispworks.com/documentation/HyperSpec/Body/m_deftp.htm "DEFTYPE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [80a8]: dref/README.md#x-28DREF-EXT-3ALOCATIVE-TYPE-DIRECT-SUPERS-20FUNCTION-29 "DREF-EXT:LOCATIVE-TYPE-DIRECT-SUPERS FUNCTION"
  [80e8]: #x-28MGL-PAX-3AWITH-HEADING-20MGL-PAX-3AMACRO-29 "MGL-PAX:WITH-HEADING MGL-PAX:MACRO"
  [8106]: #x-28MGL-PAX-3A-40M--2E-MINIBUFFER-SYNTAX-20MGL-PAX-3ASECTION-29 "`M-.` Minibuffer Syntax"
  [81b3]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhr.htm "\"2.4.8.18\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [81f7]: http://www.lispworks.com/documentation/HyperSpec/Body/s_fn.htm "FUNCTION (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [8251]: #x-28MGL-PAX-3AGLOSSARY-TERM-20CLASS-29 "MGL-PAX:GLOSSARY-TERM CLASS"
  [8269]: #x-28MGL-PAX-3ADOCUMENT-OBJECT-2A-20GENERIC-FUNCTION-29 "MGL-PAX:DOCUMENT-OBJECT* GENERIC-FUNCTION"
  [826b]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dg.htm "\"2.4.7\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [82f7]: http://www.lispworks.com/documentation/HyperSpec/Body/v_rd_eva.htm "*READ-EVAL* (MGL-PAX:CLHS VARIABLE)"
  [83d5]: #x-28MGL-PAX-3A-40BROWSING-WITH-W3M-20MGL-PAX-3ASECTION-29 "Browsing with w3m"
  [83e1]: http://www.lispworks.com/documentation/HyperSpec/Body/e_cnd.htm "CONDITION (MGL-PAX:CLHS CONDITION)"
  [8423]: #x-28MGL-PAX-3A-40TRANSCRIPT-UTILITIES-FOR-CONSISTENCY-CHECKING-20MGL-PAX-3ASECTION-29 "Utilities for Consistency Checking"
  [8492]: #x-28MGL-PAX-3ATRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR-20CONDITION-29 "MGL-PAX:TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR CONDITION"
  [84f2]: https://pandoc.org/MANUAL.html#extension-yaml_metadata_block "Pandoc YAML metadata block"
  [8541]: #x-28MGL-PAX-3A-40EMACS-SETUP-20MGL-PAX-3ASECTION-29 "Emacs Setup"
  [8710]: #x-28MGL-PAX-3AARGUMENT-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:ARGUMENT MGL-PAX:LOCATIVE"
  [875e]: #x-28MGL-PAX-3A-2ADOCUMENT-LINK-TO-HYPERSPEC-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-LINK-TO-HYPERSPEC* VARIABLE"
  [876d]: http://www.lispworks.com/documentation/HyperSpec/Body/f_ensu_1.htm "ENSURE-DIRECTORIES-EXIST (MGL-PAX:CLHS FUNCTION)"
  [886c]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cge.htm "\"22.3.7.5\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [88a7]: #x-28MGL-PAX-3ASECTION-READTABLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29 "MGL-PAX:SECTION-READTABLE (MGL-PAX:READER MGL-PAX:SECTION)"
  [88cf]: #x-28MGL-PAX-3A-40NAME-20MGL-PAX-3AGLOSSARY-TERM-29 "name"
  [8a58]: #x-28MGL-PAX-3A-40SECTIONS-20MGL-PAX-3ASECTION-29 "Sections"
  [8a5e]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhb.htm "\"2.4.8.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [8aca]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_rda.htm "*PRINT-READABLY* (MGL-PAX:CLHS VARIABLE)"
  [8c00]: https://daringfireball.net/projects/markdown/syntax#link "Markdown reference link"
  [8d9b]: #x-28MGL-PAX-3A-40OUTPUT-FORMATS-20MGL-PAX-3ASECTION-29 "Output Formats"
  [8e71]: #x-28MGL-PAX-3A-40UNSPECIFIC-LINK-20MGL-PAX-3ASECTION-29 "Unspecific Link"
  [8e92]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dc.htm "\"2.4.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [8ece]: #x-28MGL-PAX-3ADEFINE-GLOSSARY-TERM-20MGL-PAX-3AMACRO-29 "MGL-PAX:DEFINE-GLOSSARY-TERM MGL-PAX:MACRO"
  [8f19]: dref/README.md#x-28DREF-3ALOCATE-20FUNCTION-29 "DREF:LOCATE FUNCTION"
  [8f46]: http://www.lispworks.com/documentation/HyperSpec/Body/f_import.htm "IMPORT (MGL-PAX:CLHS FUNCTION)"
  [8fb6]: #x-28MGL-PAX-3A-2ADOCUMENT-MARK-UP-SIGNATURES-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-MARK-UP-SIGNATURES* VARIABLE"
  [90fa]: #x-28MGL-PAX-3A-2ADOCUMENT-HTML-DEFAULT-STYLE-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-HTML-DEFAULT-STYLE* VARIABLE"
  [9172]: http://www.lispworks.com/documentation/HyperSpec/Body/t_t.htm "T (MGL-PAX:CLHS CLASS)"
  [935f]: http://www.lispworks.com/documentation/HyperSpec/Issues/iss045.htm "\"SUMMARY:CHARACTER-PROPOSAL:2-6-5\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [9439]: http://www.lispworks.com/documentation/HyperSpec/Body/m_pr_unr.htm "PRINT-UNREADABLE-OBJECT (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [943a]: dref/README.md#x-28DREF-3APSEUDO-20DREF-3ADTYPE-29 "DREF:PSEUDO DREF:DTYPE"
  [945b]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cgc.htm "\"22.3.7.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [9478]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhv.htm "\"2.4.8.22\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [94c7]: #x-28MGL-PAX-3A-40BASICS-20MGL-PAX-3ASECTION-29 "Basics"
  [9590]: http://www.lispworks.com/documentation/HyperSpec/Body/v_stst.htm "* (MGL-PAX:CLHS VARIABLE)"
  [97ba]: dref/README.md#x-28DREF-EXT-3ALOCATIVE-TYPE-20FUNCTION-29 "DREF-EXT:LOCATIVE-TYPE FUNCTION"
  [98ff]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_l.htm#lambda_list "\"lambda list\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [9927]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cba.htm "\"22.3.2.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [99b0]: dref/README.md#x-28DREF-3ALOCATIVE-TYPES-20FUNCTION-29 "DREF:LOCATIVE-TYPES FUNCTION"
  [99b05]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#setf_function "\"setf function\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [9b15]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_a.htm#accessible "\"accessible\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [9b43]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defpkg.htm "DEFPACKAGE (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [9c7d]: #x-28MGL-PAX-3A-40PAGES-20MGL-PAX-3ASECTION-29 "`PAGES`"
  [9d50]: #x-28MGL-PAX-3A-40PAX-LIVE-HOME-PAGE-20MGL-PAX-3ASECTION-29 "PAX Live Home Page"
  [9db9]: #x-28MGL-PAX-3A-40LOCAL-DEFINITION-20MGL-PAX-3ASECTION-29 "Local Definition"
  [9dbc]: #x-28MGL-PAX-3A-40TRANSCRIPT-API-20MGL-PAX-3ASECTION-29 "Transcript API"
  [9f2f]: http://www.lispworks.com/documentation/HyperSpec/Body/v_sl_sls.htm "/ (MGL-PAX:CLHS VARIABLE)"
  [9fd4]: dref/README.md#x-28DREF-EXT-3ADOCSTRING-2A-20GENERIC-FUNCTION-29 "DREF-EXT:DOCSTRING* GENERIC-FUNCTION"
  [a11d]: dref/README.md#x-28DREF-3A-40LOCATIVE-TYPE-20MGL-PAX-3AGLOSSARY-TERM-29 "locative type"
  [a17d]: #x-28MGL-PAX-3A-40MATHJAX-20MGL-PAX-3ASECTION-29 "MathJax"
  [a249]: #x-28MGL-PAX-3ATRANSCRIPTION-CONSISTENCY-ERROR-20CONDITION-29 "MGL-PAX:TRANSCRIPTION-CONSISTENCY-ERROR CONDITION"
  [a270]: #x-28MGL-PAX-3A-40ESCAPING-AUTOLINKING-20MGL-PAX-3ASECTION-29 "Escaping Autolinking"
  [a317]: https://daringfireball.net/projects/markdown/ "Markdown"
  [a412]: #x-28MGL-PAX-3ADOCUMENTING-DEFINITION-20MGL-PAX-3AMACRO-29 "MGL-PAX:DOCUMENTING-DEFINITION MGL-PAX:MACRO"
  [a459]: dref/README.md#x-28DREF-3A-40DTYPES-20MGL-PAX-3ASECTION-29 "`DTYPE`s"
  [a595]: #x-28MGL-PAX-3A-40BROWSING-LIVE-DOCUMENTATION-20MGL-PAX-3ASECTION-29 "Browsing Live Documentation"
  [a5b1]: #x-28MGL-PAX-3ASECTION-PACKAGE-20-28MGL-PAX-3AREADER-20MGL-PAX-3ASECTION-29-29 "MGL-PAX:SECTION-PACKAGE (MGL-PAX:READER MGL-PAX:SECTION)"
  [a5ee]: #x-28MGL-PAX-3A-2ADOCUMENT-DOWNCASE-UPPERCASE-CODE-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-DOWNCASE-UPPERCASE-CODE* VARIABLE"
  [a843]: http://www.lispworks.com/documentation/HyperSpec/Body/t_std_ob.htm "STANDARD-OBJECT (MGL-PAX:CLHS CLASS)"
  [a8c5]: #x-28-22mgl-pax-2Fweb-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "\"mgl-pax/web\" ASDF/SYSTEM:SYSTEM"
  [a951]: dref/README.md#x-28MGL-PAX-3AUNKNOWN-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:UNKNOWN MGL-PAX:LOCATIVE"
  [ab38]: #x-28MGL-PAX-3A-40PARSING-LOCATIVES-20MGL-PAX-3ASECTION-29 "Parsing Locatives"
  [ab7e]: #x-28MGL-PAX-3A-40PACKAGE-AND-READTABLE-20MGL-PAX-3ASECTION-29 "Package and Readtable"
  [ac30]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cha.htm "\"22.3.8.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [ac5e]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhe.htm "\"2.4.8.5\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [ad78]: http://www.lispworks.com/documentation/HyperSpec/Body/f_format.htm "FORMAT (MGL-PAX:CLHS FUNCTION)"
  [ad80]: dref/README.md#x-28DREF-3A-40INTRODUCTION-20MGL-PAX-3ASECTION-29 "Introduction"
  [adf2]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhd.htm "\"2.4.8.4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [aeb6]: http://www.lispworks.com/documentation/HyperSpec/Body/a_fn.htm "FUNCTION MGL-PAX:CLHS"
  [af78]: #x-28MGL-PAX-3AGLOSSARY-TERM-TITLE-20-28MGL-PAX-3AREADER-20MGL-PAX-3AGLOSSARY-TERM-29-29 "MGL-PAX:GLOSSARY-TERM-TITLE (MGL-PAX:READER MGL-PAX:GLOSSARY-TERM)"
  [affc]: dref/README.md#x-28MGL-PAX-3ADOCSTRING-20FUNCTION-29 "MGL-PAX:DOCSTRING FUNCTION"
  [b033]: #x-28MGL-PAX-3A-40ASDF-SYSTEMS-AND-RELATED-PACKAGES-20MGL-PAX-3ASECTION-29 "`ASDF:SYSTEM`s and Related `PACKAGE`s"
  [b18e]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_p.htm#property_list "\"property list\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [b2e4]: #x-28MGL-PAX-3A-40FILTERING-LINKS-20MGL-PAX-3ASECTION-29 "Filtering Links"
  [b39f]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cdb.htm "\"22.3.4.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [b4f0]: http://www.lispworks.com/documentation/HyperSpec/Body/f_intern.htm "INTERN (MGL-PAX:CLHS FUNCTION)"
  [b6c4]: dref/README.md#x-28DREF-EXT-3ADEFINE-LOCATIVE-TYPE-20MGL-PAX-3AMACRO-29 "DREF-EXT:DEFINE-LOCATIVE-TYPE MGL-PAX:MACRO"
  [b79a]: http://www.lispworks.com/documentation/HyperSpec/Body/v_rdtabl.htm "*READTABLE* (MGL-PAX:CLHS VARIABLE)"
  [b7fc]: #x-28MGL-PAX-3A-40APROPOS-20MGL-PAX-3ASECTION-29 "Apropos"
  [b81d]: #x-28MGL-PAX-3ATRANSCRIPTION-ERROR-20CONDITION-29 "MGL-PAX:TRANSCRIPTION-ERROR CONDITION"
  [b89a]: #x-28MGL-PAX-3A-40CODIFIABLE-20MGL-PAX-3AGLOSSARY-TERM-29 "codifiable"
  [b8b4]: http://www.lispworks.com/documentation/HyperSpec/Body/f_abortc.htm "MUFFLE-WARNING (MGL-PAX:CLHS FUNCTION)"
  [b93c]: http://www.lispworks.com/documentation/HyperSpec/Body/t_string.htm "STRING (MGL-PAX:CLHS CLASS)"
  [ba62]: dref/README.md#x-28FUNCTION-20MGL-PAX-3ALOCATIVE-29 "FUNCTION MGL-PAX:LOCATIVE"
  [ba90]: #x-28MGL-PAX-3A-40LINKS-AND-SYSTEMS-20MGL-PAX-3ASECTION-29 "Links and Systems"
  [bb12]: #x-28MGL-PAX-3AUPDATE-ASDF-SYSTEM-HTML-DOCS-20FUNCTION-29 "MGL-PAX:UPDATE-ASDF-SYSTEM-HTML-DOCS FUNCTION"
  [bc83]: #x-28MGL-PAX-3A-40MARKDOWN-SYNTAX-HIGHLIGHTING-20MGL-PAX-3ASECTION-29 "Syntax Highlighting"
  [bcb6]: http://www.lispworks.com/documentation/HyperSpec/Body/e_warnin.htm "WARNING (MGL-PAX:CLHS CONDITION)"
  [bdd5]: #x-28MGL-PAX-3A-40HOME-SECTION-20MGL-PAX-3ASECTION-29 "Home Section"
  [bdd6]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cga.htm "\"22.3.7.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [bf38]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cfc.htm "\"22.3.6.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [bfaa]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhk.htm "\"2.4.8.11\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [c097]: dref/README.md#x-28ASDF-2FSYSTEM-3ASYSTEM-20MGL-PAX-3ALOCATIVE-29 "ASDF/SYSTEM:SYSTEM MGL-PAX:LOCATIVE"
  [c0d2]: #x-28MGL-PAX-3A-40LINK-FORMAT-20MGL-PAX-3ASECTION-29 "Link Format"
  [c2d3]: #x-28MGL-PAX-3A-40MARKDOWN-SUPPORT-20MGL-PAX-3ASECTION-29 "Markdown Support"
  [c2fd]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_i.htm#implicit_progn "\"implicit progn\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [c340]: dref/README.md#x-28DREF-3APSEUDO-LOCATIVE-TYPES-20FUNCTION-29 "DREF:PSEUDO-LOCATIVE-TYPES FUNCTION"
  [c34e]: #x-28MGL-PAX-3ADOCTITLE-2A-20GENERIC-FUNCTION-29 "MGL-PAX:DOCTITLE* GENERIC-FUNCTION"
  [c434]: #x-28MGL-PAX-3A-40BROWSING-WITH-OTHER-BROWSERS-20MGL-PAX-3ASECTION-29 "Browsing with Other Browsers"
  [c479]: dref/README.md#x-28CONDITION-20MGL-PAX-3ALOCATIVE-29 "CONDITION MGL-PAX:LOCATIVE"
  [c4a3]: http://www.lispworks.com/documentation/HyperSpec/Body/f_sin_c.htm "COS (MGL-PAX:CLHS FUNCTION)"
  [c4ce]: #x-28MGL-PAX-3A-40EXTENSION-API-20MGL-PAX-3ASECTION-29 "Writing Extensions"
  [c5ae]: http://www.lispworks.com/documentation/HyperSpec/Body/f_docume.htm "DOCUMENTATION (MGL-PAX:CLHS GENERIC-FUNCTION)"
  [c624]: https://daringfireball.net/projects/markdown/syntax#em "Markdown emphasis"
  [c818]: #x-28MGL-PAX-3AOUTPUT-LABEL-20FUNCTION-29 "MGL-PAX:OUTPUT-LABEL FUNCTION"
  [c819]: dref/README.md#x-28MGL-PAX-3ACONSTANT-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:CONSTANT MGL-PAX:LOCATIVE"
  [c879]: #x-28MGL-PAX-3A-40PLAIN-OUTPUT-20MGL-PAX-3ASECTION-29 "Plain Output"
  [c930]: #x-28MGL-PAX-3AEXPORTABLE-LOCATIVE-TYPE-P-20GENERIC-FUNCTION-29 "MGL-PAX:EXPORTABLE-LOCATIVE-TYPE-P GENERIC-FUNCTION"
  [c93e]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhg.htm "\"2.4.8.7\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [cae2]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cca.htm "\"22.3.3.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [cb15]: http://common-lisp.net/project/slime/doc/html/Finding-definitions.html#Finding-definitions "`M-.`"
  [cbc4]: #x-28MGL-PAX-3A-40REFLINK-20MGL-PAX-3ASECTION-29 "Reflink"
  [cc04]: dref/README.md#x-28MGL-PAX-3AREADER-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:READER MGL-PAX:LOCATIVE"
  [cd66]: http://www.lispworks.com/documentation/HyperSpec/Body/02_de.htm "\"2.4.5\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [cda7]: dref/README.md#x-28DREF-3AXREF-20FUNCTION-29 "DREF:XREF FUNCTION"
  [ce75]: #x-28MGL-PAX-3ADOCSTRING-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:DOCSTRING MGL-PAX:LOCATIVE"
  [cfab]: #x-28MGL-PAX-3A-40EMACS-KEYS-20MGL-PAX-3ASECTION-29 "Setting up Keys"
  [d162]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR (MGL-PAX:CLHS CONDITION)"
  [d1ca]: #x-28MGL-PAX-3A-40DOCUMENT-IMPLEMENTATION-NOTES-20MGL-PAX-3ASECTION-29 "Documentation Generation Implementation Notes"
  [d1dc]: #x-28MGL-PAX-3A-40GLOSSARY-TERMS-20MGL-PAX-3ASECTION-29 "Glossary Terms"
  [d273]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#format_directive "\"format directive\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [d296]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cia.htm "\"22.3.9.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [d3b3]: dref/README.md#x-28DREF-EXT-3ARESOLVE-2A-20GENERIC-FUNCTION-29 "DREF-EXT:RESOLVE* GENERIC-FUNCTION"
  [d3fc]: #x-28MGL-PAX-3A-40EMACS-LOADING-20MGL-PAX-3ASECTION-29 "Loading PAX"
  [d3fc5]: https://github.com/smithzvk/pythonic-string-reader "Pythonic String Reader"
  [d451]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRINT (MGL-PAX:CLHS FUNCTION)"
  [d4a9]: #x-28MGL-PAX-3A-40EMACS-FUNCTIONALITY-20MGL-PAX-3ASECTION-29 "Functionality Provided"
  [d4e7]: #x-28MGL-PAX-3A-2ADOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL* VARIABLE"
  [d534]: https://daringfireball.net/projects/markdown/syntax#em "Markdown image"
  [d5a2]: http://www.lispworks.com/documentation/HyperSpec/Body/f_car_c.htm "CAR (MGL-PAX:CLHS FUNCTION)"
  [d5a9]: http://www.lispworks.com/documentation/HyperSpec/Body/t_stream.htm "STREAM (MGL-PAX:CLHS CLASS)"
  [d5e1]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhq.htm "\"2.4.8.17\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [d646]: http://www.lispworks.com/documentation/HyperSpec/Body/t_rdtabl.htm "READTABLE (MGL-PAX:CLHS CLASS)"
  [d7b0]: #x-28MGL-PAX-3A-40WORD-20MGL-PAX-3AGLOSSARY-TERM-29 "word"
  [d813]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_fro.htm "READ-FROM-STRING (MGL-PAX:CLHS FUNCTION)"
  [d83a]: dref/README.md#x-28SETF-20MGL-PAX-3ALOCATIVE-29 "SETF MGL-PAX:LOCATIVE"
  [d850]: #x-28MGL-PAX-3ASECTION-ENTRIES-20FUNCTION-29 "MGL-PAX:SECTION-ENTRIES FUNCTION"
  [d930]: dref/README.md#x-28DREF-3ADREF-20CLASS-29 "DREF:DREF CLASS"
  [d9ee]: #x-28MGL-PAX-3A-2ADOCUMENT-LINK-CODE-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-LINK-CODE* VARIABLE"
  [da14]: http://www.lispworks.com/documentation/HyperSpec/Body/f_smp_cn.htm "SIMPLE-CONDITION-FORMAT-ARGUMENTS (MGL-PAX:CLHS FUNCTION)"
  [da65]: dref/README.md#x-28STRUCTURE-20MGL-PAX-3ALOCATIVE-29 "STRUCTURE MGL-PAX:LOCATIVE"
  [dae6]: http://www.lispworks.com/documentation/HyperSpec/Body/f_string.htm "STRING (MGL-PAX:CLHS FUNCTION)"
  [db03]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eql.htm "EQL (MGL-PAX:CLHS FUNCTION)"
  [db38]: http://www.lispworks.com/documentation/HyperSpec/Body/22_ced.htm "\"22.3.5.4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [db68]: http://www.lispworks.com/documentation/HyperSpec/Body/f_pkg_na.htm "PACKAGE-NAME (MGL-PAX:CLHS FUNCTION)"
  [dc0a]: #x-28MGL-PAX-3A-40DOCUMENT-FUNCTION-20MGL-PAX-3ASECTION-29 "The `DOCUMENT` Function"
  [dd03]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhp.htm "\"2.4.8.16\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [dd29]: #x-28MGL-PAX-3A-40MARKDOWN-OUTPUT-20MGL-PAX-3ASECTION-29 "Markdown Output"
  [dd37]: #x-28MGL-PAX-3A-2ADOCUMENT-PANDOC-PROGRAM-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-PANDOC-PROGRAM* VARIABLE"
  [dded]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhm.htm "\"2.4.8.13\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [dff6]: #x-28MGL-PAX-3A-40GITHUB-WORKFLOW-20MGL-PAX-3ASECTION-29 "GitHub Workflow"
  [e016]: http://www.lispworks.com/documentation/HyperSpec/Body/06_aab.htm "\"6.1.1.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [e196]: dref/README.md#x-28DREF-3ADEFINITIONS-20FUNCTION-29 "DREF:DEFINITIONS FUNCTION"
  [e216]: #x-28MGL-PAX-3A-2ADOCUMENT-HTML-TOP-BLOCKS-OF-LINKS-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS* VARIABLE"
  [e2a4]: #x-28MGL-PAX-3A-40UNSPECIFIC-AUTOLINK-20MGL-PAX-3ASECTION-29 "Unspecific Autolink"
  [e2ae]: #x-28MGL-PAX-3ANOTE-20MGL-PAX-3AMACRO-29 "MGL-PAX:NOTE MGL-PAX:MACRO"
  [e391]: #x-28MGL-PAX-3ADISLOCATED-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:DISLOCATED MGL-PAX:LOCATIVE"
  [e433]: http://www.lispworks.com/documentation/HyperSpec/Body/v_sl_sls.htm "// (MGL-PAX:CLHS VARIABLE)"
  [e43c]: http://www.lispworks.com/documentation/HyperSpec/Body/02_db.htm "\"2.4.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [e442]: http://www.lispworks.com/documentation/HyperSpec/Body/03_d.htm "\"3.4\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [e444]: #x-28MGL-PAX-3A-40M--2E-COMPLETION-20MGL-PAX-3ASECTION-29 "`M-.` Completion"
  [e51f]: #x-28MGL-PAX-3AEXPORTABLE-REFERENCE-P-20GENERIC-FUNCTION-29 "MGL-PAX:EXPORTABLE-REFERENCE-P GENERIC-FUNCTION"
  [e548]: dref/README.md#x-28MGL-PAX-3AWRITER-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:WRITER MGL-PAX:LOCATIVE"
  [e5ab]: http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_3.htm "SYMBOL-PACKAGE (MGL-PAX:CLHS FUNCTION)"
  [e5af]: http://www.lispworks.com/documentation/HyperSpec/Body/t_symbol.htm "SYMBOL (MGL-PAX:CLHS CLASS)"
  [e619]: #x-28MGL-PAX-3ADOCTITLE-20FUNCTION-29 "MGL-PAX:DOCTITLE FUNCTION"
  [e65d]: #x-28MGL-PAX-3A-40PARSING-NAMES-20MGL-PAX-3ASECTION-29 "Parsing Names"
  [e6bd]: dref/README.md#x-28DREF-3AARGLIST-20FUNCTION-29 "DREF:ARGLIST FUNCTION"
  [e6d3]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cdc.htm "\"22.3.4.3\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [e7ee]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*STANDARD-OUTPUT* (MGL-PAX:CLHS VARIABLE)"
  [ea37]: http://www.lispworks.com/documentation/HyperSpec/Body/v__stst.htm "*** (MGL-PAX:CLHS VARIABLE)"
  [ebd3]: #x-28MGL-PAX-3A-2ATRANSCRIBE-SYNTAXES-2A-20VARIABLE-29 "MGL-PAX:*TRANSCRIBE-SYNTAXES* VARIABLE"
  [ec7a]: #x-28MGL-PAX-3A-40AUTOLINK-20MGL-PAX-3ASECTION-29 "Autolink"
  [ed46]: #x-28MGL-PAX-3A-40M--2E-PROMPTING-20MGL-PAX-3ASECTION-29 "`M-.` Prompting"
  [ed5f]: #x-28MGL-PAX-3ACLHS-20MGL-PAX-3ALOCATIVE-29 "MGL-PAX:CLHS MGL-PAX:LOCATIVE"
  [ed9f]: http://www.lispworks.com/documentation/HyperSpec/Body/22_ceb.htm "\"22.3.5.2\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [ee51]: #x-28MGL-PAX-3AUPDATE-PAX-WORLD-20FUNCTION-29 "MGL-PAX:UPDATE-PAX-WORLD FUNCTION"
  [ee8b]: #x-28MGL-PAX-3A-2ADOCUMENT-PANDOC-PDF-METADATA-BLOCK-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-PANDOC-PDF-METADATA-BLOCK* VARIABLE"
  [f0d5]: #x-28MGL-PAX-3A-40RAW-NAMES-IN-WORDS-20MGL-PAX-3ASECTION-29 "Raw Names in Words"
  [f12d]: #x-28MGL-PAX-3A-2ADOCUMENT-MAX-NUMBERING-LEVEL-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-MAX-NUMBERING-LEVEL* VARIABLE"
  [f155]: #x-28-22mgl-pax-2Fnavigate-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "\"mgl-pax/navigate\" ASDF/SYSTEM:SYSTEM"
  [f1ab]: #x-28MGL-PAX-3A-40CODIFICATION-20MGL-PAX-3ASECTION-29 "Codification"
  [f1f0]: #x-28MGL-PAX-3ATRANSCRIBE-20FUNCTION-29 "MGL-PAX:TRANSCRIBE FUNCTION"
  [f25f]: #x-28MGL-PAX-3A-2ADOCUMENT-UPPERCASE-IS-CODE-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-UPPERCASE-IS-CODE* VARIABLE"
  [f275]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cda.htm "\"22.3.4.1\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [f2f5]: http://www.lispworks.com/documentation/HyperSpec/Body/e_smp_cn.htm "SIMPLE-CONDITION (MGL-PAX:CLHS CONDITION)"
  [f3f4]: #x-28MGL-PAX-3A-40EMACS-QUICKLISP-20MGL-PAX-3ASECTION-29 "Installing from Quicklisp"
  [f4fd]: #x-28MGL-PAX-3AREGISTER-DOC-IN-PAX-WORLD-20FUNCTION-29 "MGL-PAX:REGISTER-DOC-IN-PAX-WORLD FUNCTION"
  [f585]: #x-28MGL-PAX-3A-2ADOCUMENT-HYPERSPEC-ROOT-2A-20VARIABLE-29 "MGL-PAX:*DOCUMENT-HYPERSPEC-ROOT* VARIABLE"
  [f5af]: #x-28MGL-PAX-3A-40RAW-NAME-20MGL-PAX-3AGLOSSARY-TERM-29 "raw name"
  [f5bd]: #x-28MGL-PAX-3A-40TRANSCRIBING-WITH-EMACS-20MGL-PAX-3ASECTION-29 "Transcribing with Emacs"
  [f74b]: #x-28MGL-PAX-3A-40BACKGROUND-20MGL-PAX-3ASECTION-29 "Background"
  [f7a5]: #x-28MGL-PAX-3A-40SPECIFIC-REFLINK-20MGL-PAX-3ASECTION-29 "Specific Reflink"
  [f7e6]: #x-28MGL-PAX-3A-40DUMMY-OUTPUT-20MGL-PAX-3ASECTION-29 "Dummy Output"
  [f83b]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_l.htm#lisp_read-eval-print_loop "\"Lisp read-eval-print loop\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)"
  [f9fa]: http://www.lispworks.com/documentation/HyperSpec/Body/22_cbe.htm "\"22.3.2.5\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [fa43]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dho.htm "\"2.4.8.15\" (MGL-PAX:CLHS MGL-PAX:SECTION)"
  [fb17]: #x-28MGL-PAX-3A-40SPECIFIC-REFLINK-WITH-TEXT-20MGL-PAX-3ASECTION-29 "Specific Reflink with Text"
  [fbb1]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pl_plp.htm "++ (MGL-PAX:CLHS VARIABLE)"
  [fe21]: http://www.lispworks.com/documentation/HyperSpec/Body/v_t.htm "T (MGL-PAX:CLHS MGL-PAX:CONSTANT)"
  [fe58]: http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_rd.htm "READ (MGL-PAX:CLHS FUNCTION)"
  [ff58]: #x-28MGL-PAX-3A-40PUBLIC-SUPERCLASSES-20MGL-PAX-3AGLOSSARY-TERM-29 "public superclasses"
  [ff76]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_esc.htm "*PRINT-ESCAPE* (MGL-PAX:CLHS VARIABLE)"
  [ffd7]: http://www.lispworks.com/documentation/HyperSpec/Body/02_dhf.htm "\"2.4.8.6\" (MGL-PAX:CLHS MGL-PAX:SECTION)"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
