;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-cp1250.lisp --- Implementation of the CP1250 character encoding.
;;;
;;; Copyright (C) 2025, Bartosz Knapik <knapikbartek@gmail.com>
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

(define-constant +cp1250-to-unicode+
   #(#x20ac #xfffd #x201a #xfffd #x201e #x2026 #x2020 #x2021
     #xfffd #x2030 #x0160 #x2039 #x015a #x0164 #x017d #x0179
     #xfffd #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
     #xfffd #x2122 #x0161 #x203a #x015b #x0165 #x017e #x017a
     #x00a0 #x02c7 #x02d8 #x0141 #x00a4 #x0104 #x00a6 #x00a7
     #x00a8 #x00a9 #x015e #x00ab #x00ac #x00ad #x00ae #x017b
     #x00b0 #x00b1 #x02db #x0142 #x00b4 #x00b5 #x00b6 #x00b7
     #x00b8 #x0105 #x015f #x00bb #x013d #x02dd #x013e #x017c
     #x0154 #x00c1 #x00c2 #x0102 #x00c4 #x0139 #x0106 #x00c7
     #x010c #x00c9 #x0118 #x00cb #x011a #x00cd #x00ce #x010e
     #x0110 #x0143 #x0147 #x00d3 #x00d4 #x0150 #x00d6 #x00d7
     #x0158 #x016e #x00da #x0170 #x00dc #x00dd #x0162 #x00df
     #x0155 #x00e1 #x00e2 #x0103 #x00e4 #x013a #x0107 #x00e7
     #x010d #x00e9 #x0119 #x00eb #x011b #x00ed #x00ee #x010f
     #x0111 #x0144 #x0148 #x00f3 #x00f4 #x0151 #x00f6 #x00f7
     #x0159 #x016f #x00fa #x0171 #x00fc #x00fd #x0163 #x02d9)
  :test #'equalp)

(define-unibyte-decoder :cp1250 (octet)
  (if (and (>= octet #x80) (<= octet #xff))
      (svref +cp1250-to-unicode+
             (the ub8 (- octet #x80)))
      octet))

(define-constant +unicode-to-cp1250+
    (loop
      with h = (make-hash-table)
      for code from #x80
      for unicode across +cp1250-to-unicode+
      unless (= unicode #xfffd)
        do (setf (gethash unicode h) code)
      finally (return h))
  :test #'equalp)

(define-unibyte-encoder :cp1250 (code)
  (cond ((< code #x80) code)
        ((gethash code +unicode-to-cp1250+))
        (t (handle-error))))

(define-character-encoding :cp1250
    "An 8-bit, fixed-width character encoding used by Windows for Central
     European languages, including Czech, Polish, Slovak, Hungarian, and others."
  :aliases '(:windows-1250)
  :literal-char-code-limit 256
  :codespace `((#x00 #x80)
               ,+unicode-to-cp1250+))
