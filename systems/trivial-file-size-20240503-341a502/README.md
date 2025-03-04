# trivial-file-size

This library exports a single function, `file-size-in-octets`. It
returns the size of a file in bytes, using system calls when possible.

The canonical way to determine the size of a file in bytes, using
Common Lisp, is to open the file with an element type of
`(unsigned-byte 8)` and then calculate the length of the stream. This
is less than ideal. In most cases it would be better to get the size
of the file from its metadata, using a system call.

This is a problem I have run into several times in several different
projects. I want it *solved*, once and for all.

At the moment, getting the file size from metadata is supported for
the following Lisps:

- SBCL
- CMUCL
- Clozure CL
- CLISP
- Allegro CL
- ABCL
- GCL
- LispWorks
- ECL (on Unix only)

For other Lisps and platforms, we fall back to opening the file and
calling `file-length` on the stream.

This library is as much a call to arms as it is a resource. If you
know how to stat a file on *your* Common Lisp implementation, on
*your* platform, please make a pull request -- or just open an issue
to point me to the right documentation, and I'll do all the work.
