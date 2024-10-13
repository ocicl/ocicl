(require "asdf")

(asdf:load-system :tar-file)

(asdf:load-system :40ants-doc-full)

(40ants-doc/builder:update-asdf-system-docs tar-file::@manual :tar-file)
