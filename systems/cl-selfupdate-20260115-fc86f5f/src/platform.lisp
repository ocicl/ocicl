;;; platform.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cl-selfupdate)

(defun detect-os ()
  "Detect the operating system. Returns a keyword like :linux, :darwin, :windows."
  (let ((features *features*))
    (cond
      ((or (member :linux features)
           (member :linux-target features))
       :linux)
      ((or (member :darwin features)
           (member :macos features)
           (member :macosx features))
       :darwin)
      ((or (member :windows features)
           (member :win32 features)
           (member :win64 features))
       :windows)
      ((member :freebsd features)
       :freebsd)
      ((member :openbsd features)
       :openbsd)
      ((member :netbsd features)
       :netbsd)
      (t :unknown))))

(defun detect-arch ()
  "Detect the CPU architecture. Returns a keyword like :amd64, :arm64, :x86."
  (let ((features *features*))
    (cond
      ((or (member :x86-64 features)
           (member :amd64 features)
           (member :x64 features))
       :amd64)
      ((or (member :arm64 features)
           (member :aarch64 features))
       :arm64)
      ((or (member :x86 features)
           (member :i686 features)
           (member :i386 features))
       :386)
      ((member :arm features)
       :arm)
      ((member :ppc64 features)
       :ppc64)
      ((member :ppc features)
       :ppc)
      ((member :riscv64 features)
       :riscv64)
      (t :unknown))))

(defun detect-platform ()
  "Returns a cons of (os . arch) for the current platform."
  (cons (detect-os) (detect-arch)))

(defun os-name-string (os)
  "Convert OS keyword to GitHub release naming convention string."
  (case os
    (:linux "linux")
    (:darwin "darwin")
    (:windows "windows")
    (:freebsd "freebsd")
    (:openbsd "openbsd")
    (:netbsd "netbsd")
    (t (string-downcase (symbol-name os)))))

(defun arch-name-string (arch)
  "Convert arch keyword to GitHub release naming convention string."
  (case arch
    (:amd64 "amd64")
    (:arm64 "arm64")
    (:386 "386")
    (:arm "arm")
    (:ppc64 "ppc64")
    (:riscv64 "riscv64")
    (t (string-downcase (symbol-name arch)))))

(defun platform-suffixes ()
  "Generate list of possible platform suffix patterns for asset matching.
Returns strings like 'linux_amd64', 'linux-amd64', etc."
  (let* ((os (os-name-string (detect-os)))
         (arch (arch-name-string (detect-arch)))
         (separators '("_" "-"))
         (suffixes '()))
    (dolist (sep separators)
      (push (format nil "~A~A~A" os sep arch) suffixes)
      (push (format nil "~A~A~A" arch sep os) suffixes))
    (nreverse suffixes)))
