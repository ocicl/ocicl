# ocicl 2.19.1 Release Notes

**Release Date:** September 2026

## Summary

Patch release fixing an installation failure on source trees that contain a symbolic link to a directory. Eclector is the reported case, which also blocked downstream systems such as Coalton.

## Bug Fixes

- **Installing a system whose tree links to a directory no longer fails.** ocicl extracted package tarballs with a library routine that turns a link entry into a plain file by copying the bytes of whatever it names. That works for a link to a file and fails for a link to a directory, and because the copy happened in a deferred pass with no per-link recovery, a single such link aborted the whole extraction. Eclector ships `documentation/presentation-slides` pointing at a slides directory, so `ocicl install eclector` reported `couldn't read from #<SB-SYS:FD-STREAM ...>: Is a directory` and installed nothing. ocicl now extracts link entries itself: links to files and hard links are still materialized with their target's contents, chains of links still resolve, and links to directories — along with broken or circular links — are skipped instead of failing the install. Pass `--verbose` to see which links were skipped. (Reported as [Eclector issue #89](https://github.com/s-expressionists/Eclector/issues/89).)

## Security

- **Links can no longer reach outside the staging directory.** The previous extraction path merged a link entry's raw target name and copied it with no containment check, so a crafted or compromised package could name an absolute path, a `..` path, or a `~`-prefixed path and have that file copied out of the user's filesystem and into the installed system tree. A link target is now dereferenced only when its resolved path lies inside the directory being extracted.

## Breaking Changes

None.

## Upgrade Notes

Drop-in replacement for 2.19.0.

## Installation

Download the appropriate package for your system from the [releases page](https://github.com/ocicl/ocicl/releases/tag/v2.19.1).
