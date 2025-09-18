;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-cp1254.lisp --- Implementation of the CP1254 character encoding.
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

(define-constant +cp1254-to-unicode+
    #(;; #x80
      #x20ac #xfffd #x201a #x0192 #x201e #x2026 #x2020 #x2021
      #x02c6 #x2030 #x0160 #x2039 #x0152 #xfffd #xfffd #xfffd
      ;; #x90
      #xfffd #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
      #x02dc #x2122 #x0161 #x203a #x0153 #xfffd #xfffd #x0178
      ;; #xa0
      #x00a0 #x00a1 #x00a2 #x00a3 #x00a4 #x00a5 #x00a6 #x00a7
      #x00a8 #x00a9 #x00aa #x00ab #x00ac #x00ad #x00ae #x00af
      ;; #xb0
      #x00b0 #x00b1 #x00b2 #x00b3 #x00b4 #x00b5 #x00b6 #x00b7
      #x00b8 #x00b9 #x00ba #x00bb #x00bc #x00bd #x00be #x00bf
      ;; #xc0
      #x00c0 #x00c1 #x00c2 #x00c3 #x00c4 #x00c5 #x00c6 #x00c7
      #x00c8 #x00c9 #x00ca #x00cb #x00cc #x00cd #x00ce #x00cf
      ;; #xd0
      #x011e #x00d1 #x00d2 #x00d3 #x00d4 #x00d5 #x00d6 #x00d7
      #x00d8 #x00d9 #x00da #x00db #x00dc #x0130 #x015e #x00df
      ;; #xe0
      #x00e0 #x00e1 #x00e2 #x00e3 #x00e4 #x00e5 #x00e6 #x00e7
      #x00e8 #x00e9 #x00ea #x00eb #x00ec #x00ed #x00ee #x00ef
      ;; #xf0
      #x011f #x00f1 #x00f2 #x00f3 #x00f4 #x00f5 #x00f6 #x00f7
      #x00f8 #x00f9 #x00fa #x00fb #x00fc #x0131 #x015f #x00ff)
  :test #'equalp)

(define-unibyte-decoder :cp1254 (octet)
  (if (< octet #x80)
      octet
      (svref +cp1254-to-unicode+ (the ub8 (- octet #x80)))))

(define-constant +unicode-to-cp1254+
    (loop
      with h = (make-hash-table)
      for code from #x80
      for unicode across +cp1254-to-unicode+
      unless (= unicode #xfffd)
        do (setf (gethash unicode h) code)
      finally (return h))
  :test #'equalp)

(define-unibyte-encoder :cp1254 (code)
  (cond ((< code #x80) code)
        ((gethash code +unicode-to-cp1254+))
        (t (handle-error))))

(define-character-encoding :cp1254
    "An 8-bit, fixed-width character Turkish encoding from Windows."
  :aliases '(:windows-1254)
  :literal-char-code-limit 256
  :codespace `((#x00 #x80)
               ,+unicode-to-cp1254+))
