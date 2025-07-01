# version-string

`version-string` produces version strings derived from your system's
ASDF version and state extracted from git.

To use it, be sure to set a `:version` in your project's ASDF system definition.
Now, do this:

```
(version-string:define-version-parameter +version+ :MY-SYSTEM)
```

This will define the parameter `+version+` and set it to a string
representing the ASDF version for `:MY-SYSTEM` augmented by
information extracted from git.

The version string logic works thusly:

- If there's a git tag pointing to the current commit, it uses that
  tag as the version (e.g., `v1.2.3` or `v1.2.3+dirty`)
- If no tag but git hash available, it uses the base version from .asd
  with git hash (e.g., `1.0.0-g1a2b3c4` or `1.0.0-g1a2b3c4+dirty`)
- If git isn't available, it falls back to the base version from the
  .asd file (and `0.0.0` if no `:version` was provided).

## Author and License

`cl-version-string` was written by Anthony Green and is distributed
under the terms of the MIT license.
