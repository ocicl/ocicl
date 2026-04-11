;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-cp1257.lisp --- Implementation of the CP1257 character encoding.
;;;
;;; Copyright (C) 2025, Wojciech S. Gac <wojciech.s.gac@gmail.com>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:babel-encodings)

(define-constant +cp1257-to-unicode+
    #(#x20ac #xfffd #x201a #xfffd #x201e #x2026 #x2020 #x2021
      #xfffd #x2030 #xfffd #x2039 #xfffd #x00a8 #x02c7 #x00b8
      #xfffd #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
      #xfffd #x2122 #xfffd #x203a #xfffd #x00af #x02db #xfffd
      #x00a0 #xfffd #x00a2 #x00a3 #x00a4 #xfffd #x00a6 #x00a7
      #x00d8 #x00a9 #x0156 #x00ab #x00ac #x00ad #x00ae #x00c6
      #x00b0 #x00b1 #x00b2 #x00b3 #x00b4 #x00b5 #x00b6 #x00b7
      #x00f8 #x00b9 #x0157 #x00bb #x00bc #x00bd #x00be #x00e6
      #x0104 #x012e #x0100 #x0106 #x00c4 #x00c5 #x0118 #x0112
      #x010c #x00c9 #x0179 #x0116 #x0122 #x0136 #x012a #x013b
      #x0160 #x0143 #x0145 #x00d3 #x014c #x00d5 #x00d6 #x00d7
      #x0172 #x0141 #x015a #x016a #x00dc #x017b #x017d #x00df
      #x0105 #x012f #x0101 #x0107 #x00e4 #x00e5 #x0119 #x0113
      #x010d #x00e9 #x017a #x0117 #x0123 #x0137 #x012b #x013c
      #x0161 #x0144 #x0146 #x00f3 #x014d #x00f5 #x00f6 #x00f7
      #x0173 #x0142 #x015b #x016b #x00fc #x017c #x017e #x02d9)
  :test #'equalp)

(define-unibyte-decoder :cp1257 (octet)
  (if (and (>= octet #x80) (<= octet #xff))
      (svref +cp1257-to-unicode+
             (the ub8 (- octet #x80)))
      octet))

(define-constant +unicode-to-cp1257+
    (loop
      with h = (make-hash-table)
      for code from #x80
      for unicode across +cp1257-to-unicode+
      unless (= unicode #xfffd)
        do (setf (gethash unicode h) code)
      finally (return h))
  :test #'equalp)

(define-unibyte-encoder :cp1257 (code)
  (cond ((< code #x80) code)
        ((gethash code +unicode-to-cp1257+))
        (t  (handle-error))))

(define-character-encoding :cp1257
    "A 8-bit, fixed-width character encoding used by Windows for Estonian, Latvian and Lithuanian."
  :aliases '(:windows-1257)
  :literal-char-code-limit 256
  :codespace `((#x00 #x80)
               ,+unicode-to-cp1257+))
