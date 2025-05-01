-*- markdown -*-

* Compilation IOlib requires a C library named LibFixPOSIX -
https://github.com/sionescu/libfixposix - and its headers in order to
compile. LibFixPOSIX can either be installed manually, for which there
are instructions in the source code, or through distro repositories.

As of Debian 9.0 Stable, the libfixposix package uses code from 2011,
which is incompatible with the current IOlib and will cause a SEGFAULT
on load. Don't use it.

* Tests
 Some of the socket tests require an echo server, the default being
one on the Internet. If you can't use that, set
iolib-tests:*echo-address* and iolib-test:*echo-port* appropriately to
point the echo tests somewhere else.

* Generating documentation
 To generate the documentation, use this patched version of
texinfo-docstrings: http://gitorious.org/iolib/texinfo-docstrings
Then make sure that IOLib's .asd files and texinfo-docstrings.asd can
be loaded and run GNU make inside doc/ ; you'll then find the
generated docs under manual/. This procedure has only been tested
with SBCL.
