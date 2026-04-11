;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-cp1258.lisp --- Implementation of the CP1258 character encoding.
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

(define-constant +cp1258-to-unicode+
   #(#x20ac #xfffd #x201a #x0192 #x201e #x2026 #x2020 #x2021
     #x02c6 #x2030 #xfffd #x2039 #x0152 #xfffd #xfffd #xfffd
     #xfffd #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
     #x02dc #x2122 #xfffd #x203a #x0153 #xfffd #xfffd #x0178
     #x00a0 #x00a1 #x00a2 #x00a3 #x00a4 #x00a5 #x00a6 #x00a7
     #x00a8 #x00a9 #x00aa #x00ab #x00ac #x00ad #x00ae #x00af
     #x00b0 #x00b1 #x00b2 #x00b3 #x00b4 #x00b5 #x00b6 #x00b7
     #x00b8 #x00b9 #x00ba #x00bb #x00bc #x00bd #x00be #x00bf
     #x00c0 #x00c1 #x00c2 #x0102 #x00c4 #x00c5 #x00c6 #x00c7
     #x00c8 #x00c9 #x00ca #x00cb #x0300 #x00cd #x00ce #x00cf
     #x0110 #x00d1 #x0309 #x00d3 #x00d4 #x01a0 #x00d6 #x00d7
     #x00d8 #x00d9 #x00da #x00db #x00dc #x01af #x0303 #x00df
     #x00e0 #x00e1 #x00e2 #x0103 #x00e4 #x00e5 #x00e6 #x00e7
     #x00e8 #x00e9 #x00ea #x00eb #x0301 #x00ed #x00ee #x00ef
     #x0111 #x00f1 #x0323 #x00f3 #x00f4 #x01a1 #x00f6 #x00f7
     #x00f8 #x00f9 #x00fa #x00fb #x00fc #x01b0 #x20ab #x00ff)
  :test #'equalp)

(define-unibyte-decoder :cp1258 (octet)
  (if (and (>= octet #x80) (<= octet #xff))
      (svref +cp1258-to-unicode+
             (the ub8 (- octet #x80)))
      octet))

(define-constant +unicode-to-cp1258+
    (loop
      with h = (make-hash-table)
      for code from #x80
      for unicode across +cp1258-to-unicode+
      unless (= unicode #xfffd)
        do (setf (gethash unicode h) code)
      finally (return h))
  :test #'equalp)

(define-unibyte-encoder :cp1258 (code)
  (cond ((< code #x80) code)
        ((gethash code +unicode-to-cp1258+))
        (t (handle-error))))

(define-character-encoding :cp1258
    "An 8-bit, fixed-width character encoding used by Windows for Vietnamese language"
  :aliases '(:windows-1258)
  :literal-char-code-limit 256
  :codespace `((#x00 #x80)
               ,+unicode-to-cp1258+))

