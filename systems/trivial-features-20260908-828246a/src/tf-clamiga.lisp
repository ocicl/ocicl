;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; tf-clamiga.lisp --- CL-Amiga trivial-features implementation.
;;;
;;; Intentionally empty: the CL-Amiga runtime pushes all relevant
;;; features itself at startup: OS (:AMIGAOS / :UNIX + :DARWIN/:LINUX),
;;; CPU (:M68K / :PPC / :X86-64 / :ARM64), endianness (:BIG-ENDIAN or
;;; :LITTLE-ENDIAN), and word size (:32-BIT or :64-BIT).  There is
;;; nothing to normalize here.
