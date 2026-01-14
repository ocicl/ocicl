;;;; package.lisp --- Package definition for the trivial-with-current-source-form system.
;;;;
;;;; Copyright (C) 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:trivial-with-current-source-form
  (:use
   #:cl)

  (:export
   #:with-current-source-form))
