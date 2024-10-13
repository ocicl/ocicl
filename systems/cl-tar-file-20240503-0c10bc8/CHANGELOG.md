## v0.2.1 - February 08, 2022

+ Testing updated CI pipelines.

## v0.2.0 - January 03, 2022

+ Added transparent gzip compression/decompression support using salza2 and
  chipz.

## v0.1.4 - January 03, 2022

+ Remove check on stream element type. On at least SBCL and ECL,
  `*standard-input*` and `*standard-output*` have element type `'character`,
  but also allow you to use binary stream functions on them.

## v0.1.3 - January 02, 2022

+ Fix bug with determining type of entries in v7 archives.
+ If given a gray stream that does not implement `stream-element-type`, assume
  it is `(unsigned-byte 8)`.

## v0.1.2 - September 25, 2021

+ Fix bug where strings read from headers were not bounded to the length of the
  field.
+ Fix bug when using FILE-POSITION to jump across block boundaries on
  BLOCKED-STREAMs.

## v0.1.1 - September 23, 2021

+ Robustify against streams that error when you call FILE-POSITION.

## v0.1.0 - September 22, 2021

First release!
