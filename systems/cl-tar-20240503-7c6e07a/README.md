This project provides a high level interface for interacting with tar
archives. It consists of several systems that provide different levels of
functionality.

**NOTE**: In order to load `tar/create` on Windows you need a version of osicat
with commits from [this PR](https://github.com/osicat/osicat/pull/57).

## Quickstart

If you want to extract a tar archive, without caring about preserving all the
metadata, run the following:

```common-lisp
(asdf:load-system "tar/simple-extract")

(tar:with-open-archive (a "/path/to/file.tar")
  (tar-simple-extract:simple-extract-archive a :directory "/path/to/extraction/point/"))
```

If you want to extract a tar archive, attempting to preserve symbolic links and
as much metadata as possible, evaluate the following:

```common-lisp
(asdf:load-system "tar/extract")

(tar:with-open-archive (a "/path/to/file.tar")
  (tar-extract:extract-archive a :directory "/path/to/extraction/point/"))
```

If you want to create a tar archive, attempting to preserve symbolic links and
as much metadata as possible, evaluate the following:

```common-lisp
(asdf:load-system "tar/create")

(let ((*default-pathname-defaults* #p"/path/to/parent/"))
  (tar:with-open-archive (a "/path/to/file.tar" :direction :output)
    (tar-create:create-archive a '("directory/") :recursep t)))
```

## Systems
### tar

The `tar` system is a thin layer on top of
the [`tar-file`](https://gitlab.common-lisp.net/cl-tar/cl-tar-file)
system. While `tar-file` is focused on reading and writing physical entries,
`tar` places an emphasis on reading and writing logical entries.

The practical effect of this is that when using `tar`, any single object stored
in a tar archive (regular file, symlink, etc.) is always represented as a
single entry (`tar:file-entry`, `tar:symbolic-link-entry`, etc.). This is in
contrast to `tar-file` where a regular file with a long name could be either
unrepresentable, a single `tar-file:file-entry` with a ustar header, a
`tar-file:pax-extended-attributes-entry` followed by a `tar-file:file-entry`,
or a `tar-file:gnu-long-name-entry` followed by a `tar-file:file-entry`. `tar`
takes care of generating and interpreting the correct sequence of physical
entries, depending on the type of the archive being used.

While this system is useful for reading and writing tar archives, its primary
purpose is for the inspection of archives and creating archives whose content
does not come directly from the file system. For file system integration, see
the remaining systems.

### tar/simple-extract

The `tar/simple-extract` system provides functionality to extract a tar archive
to your filesystem. Unfortunately, faithfully extracting tar files onto the
filesystem is a complex task and is impossible to perform using only the
functionality mandated by the Common Lisp specification.

Therefore, this system does not try to faithfully reproduce the contents of the
tar file. This means that it should work on any CL implementation that `tar`
and `tar-file` does, but the cost is there is information loss.

This system does not support extracting the following entry types:

+ Symbolic links. They can be skipped or dereferenced.
+ Hard links. They can be skipped or dereferenced.
+ Character devices. They can be skipped.
+ Block devices. They can be skipped.
+ FIFO. They can be skipped.

Additionally, metadata such as file owner, permissions, and modification time
are not set.

It is recommended that this extraction method is used only to extract an
archive to an empty folder. If that is done with default settings, the
extraction process should be fairly safe and predictable. Otherwise, you run
the risk of existing symlinks being followed and overwriting arbitrary files on
your machine.

### tar/extract

The `tar/extract` system attempts to provide full extraction functionality. As
such, it is a much more complex beast and likely does not work on all
implementation/OS combinations. Patches are always welcome to make it more
portable.

This system does not support extracting the following entry types on Windows:

+ Symbolic links. They can be skipped or dereferenced.
+ Hard links. They can be skipped or dereferenced.
+ Character devices. They can be skipped.
+ Block devices. They can be skipped.
+ FIFO. They can be skipped.

While it is *possible* to create symbolic links on Windows, it requires special
user permissions and many Windows applications are not designed with symlinks
in mind. This makes it both very unlikely that an arbitrary user can create
symlinks and very likely that the creation of symlinks would pose a risk to
other applications running on the same machine. Hard link support is also
possible, but I've rarely seen it and need to do more research before
attempting to support it.

While there is much less information loss when extracting an archive using this
system, that comes at the cost of increased security concerns when given an
untrusted archive as input. For example, a typical attack vector is to write a
symlink that points to an arbitrary file on your system and then using that
symlink to modify the target.

The goal of this system is to provide default extraction options so that it is
safe to extract an untrusted archive into an empty directory and have no way
for the extraction process to modify any file that exists outside of that
directory. Bug reports and patches are always welcome if you find this goal is
not met.

The safety goal is paramount, and as such, the performance of this system is
likely not the best. A primary reason is that we want to be robust to the
different file systems that are out there (e.g., case insensitive or unicode
normalizing), so we (currently) do not rely on caches to determine if a path is
safe to write to. The result is that there are many syscalls that happen during
extraction. There are probably ways this can be improved and patches are always
welcome.

### tar/create

The `tar/create` system produces a tar file from your file system that
faithfully reproduces all the metadata. It is a fairly complex beast and might
not work on all implementation/OS combinations. Patches are always welcome to
make it more portable.

See the docstring for `tar-create:create-archive` to understand its options.

## FAQ

### Can I create/extract compressed tar files?

Yes! It supports transparent gzip comporession. On input streams, the first few
bytes are examined for magic bytes. On output streams, the pathname is used as
a hint. You can override this behavior using the `:compression` argument to
`with-open-archive` or `open-archive`.
