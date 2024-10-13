This project provides functionality to read and write a variety of tar archive
formats. Currently supported are the POSIX ustar, PAX (ustar with a few new
entry types), GNU, and v7 (very old) formats.

This project is rather low level and is focused exclusively on reading and
writing physical tar file entries using streams. Therefore, it contains no
functionality to automatically build archives from a set of files on the
filesystem or write the contents of a file to the filesystem. Additionally,
there are no smarts that read multiple physical entries and combine them into a
single logical entry (e.g., with PAX extended headers or GNU long link/path
name support). This approach is taken to both improve portability (the CL spec
has no facility for things like detecting symlinks or modifying/reading file
owners, etc.) and properly separate concerns.

For a higher level library that reads and writes logical entries, as well as
includes file system integration,
see [cl-tar](https://gitlab.common-lisp.net/cl-tar/cl-tar).

This project is a fork
of [Nathan Froyd's archive library](https://github.com/froydnj/archive). Much
code remains, but the non-portable bits have been stripped, better support for
multiple archive types (and autodetection) has been added, better blocking
support added (tar readers/writers are supposed to read/write in multiples of
512 bytes), cpio support removed, and a test suite added, along with other
miscellaneous fixes and improvements.

One major user visible difference between this library and the original archive
is that there is no need to discard entries when you are finished with them and
support has been added for seeking within a stream (using `FILE-POSITION` under
the hood). This means you can do something like iterate over all entries in one
go and then get a stream containing the contents of an arbitrary entry.

However, care must be taken if you hop around in a tar file as it will only
work if the underlying stream containing the tar archive is
seekable. Typically, streams backed by files **are** seekable, but all other
input streams (including stdin) **are not**. Therefore if there is any chance
your code that uses `tar-file` will read files from non-seekable streams, you
should process entries sequentially and completely before moving on to the next
one. Alternatively, you can set the blocking factor high enough that the entire
file is read into memory at once (yikes!).
