(in-package :cl-user)

(require "asdf")

(asdf:load-system "asdf-release-ops")

(asdf:operate 'asdf-release-ops:dynamic-release-archive-op :tar-cli)
