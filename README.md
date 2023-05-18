# ocicl
> An ASDF system distribution and management tool for Common Lisp

What is it?
------------
``ocicl`` is a modern alternative to quicklisp.  It is modern in the sense that:
* all software is distributed over secure (TLS) connections.
* all software is bundled as OCI-compliant artifacts and distributed from an OCI-compliant registry (the github container registry).
* sigstore tooling is used ensure the integrity and authenticity of all software bundles.
* all software bundles are project-local, making it easy to lock specific versions to your own projects.
* all software bundles are built and published transparently using hosted CI infrastructure (github actions).

``ocicl`` rhymes with "ossicle", a tiny bone embedded in your middle
ear.  Like the ossicles in your ear, the ``ocicle-runtime`` is a tiny
library that is embedded in your lisp image.  It is responsible for
finding and loading ASDF systems that you manage with the ``ocicl``
command line tool.

``ocicl`` is under active development, and may not work for you.

Quick Start
------------

TBD

Author and License
-------------------

``ocicl`` was written by [Anthony Green](https://github.com/atgreen),
and is distributed under the terms of the MIT license.

This software includes Lisp source code files written by Zachary
Beane, Mark Karpov, and PMSF IT Consulting Pierre R. Mai.  See the
ocicl source files for details.
