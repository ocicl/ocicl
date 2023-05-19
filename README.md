# ocicl
> An ASDF system distribution and management tool for Common Lisp

What is it?
------------
``ocicl`` is a modern alternative to quicklisp.  It is modern in the sense that:
* all software is [bundled as OCI-compliant artifacts](https://oras.land/) and distributed from an OCI-compliant registry (the github container registry).
* all software is distributed over secure (TLS) connections.
* [sigstore](https://www.sigstore.dev/) tooling is used ensure the integrity and authenticity of all software bundles.
* all software bundles are project-local, making it easy to lock specific versions to your own projects.
* all software bundles are built and published transparently using hosted CI infrastructure ([github actions](https://github.com/ocicl/ocicl-action)).

``ocicl`` rhymes with
"[ossicle](https://en.wikipedia.org/wiki/Ossicles)", a tiny bone
embedded in your middle ear.  Like the ossicles in your ear, the
``ocicle-runtime`` is a tiny library that is embedded in your lisp
image.  It is responsible for finding and loading
[ASDF](https://asdf.common-lisp.dev/) systems that you manage with the
``ocicl`` command line tool.

``ocicl`` is under active development, and may not work for you.

Quick Start
------------

Install ``ocicl`` by running ``make`` in the source directory.  This
will build the ``ocicl`` binary and install it in ``${DESTDIR}/bin``.
It will also install a helper program called ``ocicl-oras``.  The
default value for ``DESTDIR`` is ``${HOME}/.local/bin``, but you can
change it at install time like so:
```
$ DESTDIR=\usr\local make install
```
Make sure the ``${DESTDIR}\bin`` directory is on your path.

Now run ``ocicl setup``.  This is a mandatory step that installs the
``ocicl-runtime`` library, and suggests configurations for your
``${HOME}/.sbclrc`` file.

```
$ ocicl setup
Add the following to your ${HOME}/.sbclrc file:

#-ocicl
(when (probe-file #P"/home/green/.local/share/ocicl/ocicl-runtime.lisp")
  (load #P"/home/green/.local/share/ocicl/ocicl-runtime.lisp"))
```

The default behavior for the runtime is to invoke ``ocicl`` when ASDF
tries to load a system that it can't find.

Try running this:
```
$ sbcl --eval "(asdf:load-system :str)"
```

Now look at your current directory.  You should see a directory called
``systems`` and a file called ``systems.csv``.  The ``systems``
directory contains the code you just downloaded, and ``systems.csv``
contains a mapping of system names to OCI artifacts and ``.asd``
files.

The next time you try to load ``str``, ASDF will load the code that
you've already downloaded and compiled.

Now try deleting the ``systems`` directory, and loading ``str`` again
as above.  ``ocicl`` will download the exact version specified in the
``systems.csv`` file.  The idea here is that you could commit your
``system.csv`` file with your source code, but not the ``systems``
directory, and you would always run the versions you've locked to in
that file.

Now let's try the ``ocicl`` command line tool.

```
$ ocicl help
ocicl - copyright (C) 2023 Anthony Green <green@moxielogic.com>

Usage: ocicl [-h|--help] [-v|--verbose] command

Available options:
  -h, --help               print this help text
  -v, --verbose            produce verbose output

Choose from the following ocicl commands:

   install [SYSTEM[:VERSION]]...       Install systems
   latest [SYSTEM]...                  Install latest version of systems
   list SYSTEM...                      List available system versions
   setup                               Mandatory ocicl configuration
   version                             Show the ocicl version information

Distributed under the terms of the MIT License
```

If we again delete the ``systems`` directory, running ``ocicl
install`` will download all of the systems specified in your
``systems.csv`` file.

```
$ ocicl install
; downloading ghcr.io/ocicl/str@sha256:0903b59c33d3026ac55a6f4b25a79094d08e3110758d8ae728bf4188db659313
; downloading ghcr.io/ocicl/cl-ppcre@sha256:5274824d397fa197d5c7790344ace27f2a30fc34c6cadb0a9fcce7d1e4052486
; downloading ghcr.io/ocicl/cl-unicode@sha256:b61ac07aed06c926720e6a4c155fd0c9411b01a05ee7ebba55fca7df491880e5
; downloading ghcr.io/ocicl/flexi-streams@sha256:091df0cda6006b19aa206b022bb6d06fd9d5e5787b6152b9f0ae6846926ac5e0
; downloading ghcr.io/ocicl/trivial-gray-streams@sha256:e82a60fdccc33916f26b60a3af63ee110f0b364cc2af59eee4be86256e8ea2b6
; downloading ghcr.io/ocicl/cl-change-case@sha256:61791ee49f0160adad694eedbe8804fe9bcebad54336b0fbb8ce1a82091e20fa
```

You can download additional systems like so:
```
$ ocicl install trivial-garbage
; downloading trivial-garbage
; downloaded ghcr.io/ocicl/trivial-garbage@sha256:1eaadc3a546aaad7b452197663d9baece7a7e11beac6beb7db5b5faf4e74d541
; compiling file "/home/green/test/systems/trivial-garbage-20230511-b3af9c0/trivial-garbage.lisp" (written 19 MAY 2023 08:34:59 AM):

; wrote /home/green/.cache/common-lisp/sbcl-2.3.2-linux-x64/home/green/test/systems/trivial-garbage-20230511-b3af9c0/trivial-garbage-tmpMQBOWYP9.fasl
; compilation finished in 0:00:00.007
```

This downloads the latest version of trivial-garbage, which is the OCI
image with the ``latest`` tag, and is equivalent to ``ocicl install
trivial-garbage:latest``.

To see what other versions of a package are available, run ``ocicl list trivial-garbage``
```
$ ocicl list trivial-garbage
trivial-garbage:
 latest
 20230511-b3af9c0
```

Here we only have one version, 20230511-b3af9c0, which also has the
``latest`` tag.  Many lisp libraries are built from git sources
without release tags.  In this case, the version label represents the
build date and the git commit hash (b3af9c0).

To install any specific version of a system, just use the appropriate
version label in your ``ocicl install`` command.

To update all systems in your ``systems.csv`` file to the latest
version, run ``ocicl latest``.

Troubleshooting
---------------

Setting ``ocicl-runtime:*verbose*`` to ``t`` will output useful and interesting log info.

Author and License
-------------------

``ocicl`` was written by [Anthony Green](https://github.com/atgreen),
and is distributed under the terms of the MIT license.

This software includes Lisp source code files written by Zachary
Beane, Mark Karpov, and PMSF IT Consulting Pierre R. Mai.  See the
ocicl source files for details.
