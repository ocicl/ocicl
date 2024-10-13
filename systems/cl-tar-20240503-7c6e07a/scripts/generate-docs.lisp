(require "asdf")

(asdf:load-system :tar/docs)

(asdf:load-system :40ants-doc-full)

(40ants-doc/builder:update-asdf-system-docs tar-docs::@manual :tar)
