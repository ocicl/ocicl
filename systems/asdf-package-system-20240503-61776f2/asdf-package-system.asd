(defsystem "asdf-package-system"
  :description "Backward-compatible stub for ASDF 3.1's package-inferred-system feature"
  :long-description "ASDF 3.1 now include package-inferred-system which supports
quick-build and faslpath style of systems whereby each file starts with a defpackage form,
from which dependencies are deduced. This stub is present for backward compatibility with
systems that were trying to use this feature with ASDF 3.0"
  :depends-on ((:version :asdf "3.1.2"))
  :licence "MIT"
  :version "3.1.2")
