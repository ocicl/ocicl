;;;; System
;;;;
;;;; This software is part of cl-tar. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:tar-cli-asdf)

(defclass tar-cli-system (ops:release-system)
  ())

(defmethod ops::archive-base-name ((s tar-cli-system))
  "cl-tar")
