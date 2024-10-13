## v0.2.2 - January 30, 2021

+ Bug fix: Remove two systems `tar/simple-create` and `tar/simple-create-test`
  that were accidentally committed.

## v0.2.1 - January 12, 2021

+ Bug fix: `nix:futime` was previously defined by OSICAT. But that was an error
  and the latest osicat no longer defines it. Move the implementation of it
  locally until extraction functions can be written that use Win32 APIs
  directly.
  
## v0.2.0 - January 04, 2021

+ All slots on ENTRY now have a default value. Use NIL for "not provided"
  instead of keeping the slot unbound.
+ System names changed to all start with `tar/`.
+ Added system to create tar archives from the filesystem.
+ Added transparent gzip support.
+ Added command line interface and binary build.

## v0.1.1 - September 23, 2021

+ Fix bug where extracting an entry with fewer components than are stripped
  causes an error.
+ Fix bug where extracting files with non-ASCII characters would error. (I
  thought char was unsigned by default in C, but CFFI treats it as signed.
+ Fix bug reading archives with a PAX-GLOBAL-ATTRIBUTES-ENTRY. Currently, just
  ignore it.

## v0.1.0 - September 22, 2021

First release!
