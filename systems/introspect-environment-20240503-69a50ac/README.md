This library is intended to allow a bit more compile-time introspection of environments in CL.

Quite a bit of information is available at the time a macro or compiler-macro runs; inlining info, type declarations, that sort of thing. This information is all standard - any CL program can (declare (integer x)) and such.

This info ought to be accessible through the standard &environment parameters, but it is not. Several implementations keep the information for their own purposes but do not make it available to user programs, because there is no standard mechanism to do so.

This library uses implementation-specific hooks to make information available to users. This is currently supported on SBCL, CCL, and CMUCL. Other implementations have implementations of the functions that do as much as they can and/or provide reasonable defaults. typexpand and typexpand-1 are available for ABCL, Allegro, CLISP, ECL, and Lispworks, though the second return value may be off.
