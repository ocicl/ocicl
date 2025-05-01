;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Main test suite definition.
;;;

(in-package :iolib/tests)

(def-suite :iolib
  :description "Main test suite for IOLib.")

(def-suite :iolib/base :in :iolib)

(def-suite :iolib/pathnames :in :iolib)

(def-suite :iolib/multiplex :in :iolib)

(def-suite :iolib/streams :in :iolib)

(def-suite :iolib/sockets :in :iolib)
