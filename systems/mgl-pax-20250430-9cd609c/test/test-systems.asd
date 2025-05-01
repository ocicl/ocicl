;;; These must be in a separate file as ASDF:FIND-SYSTEM reloads it.

(in-package :mgl-pax-test)

(asdf:defsystem interned-asdf-system-name)
(asdf:defsystem #:non-interned-asdf-system-name)
