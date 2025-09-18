;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-cp1256.lisp --- Implementation of the CP1256 character encoding.
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

(define-constant +cp1256-to-unicode+
   #(#x20ac #x067e #x201a #x0192 #x201e #x2026 #x2020 #x2021
     #x02c6 #x2030 #x0679 #x2039 #x0152 #x0686 #x0698 #x0688
     #x06af #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
     #x06a9 #x2122 #x0691 #x203a #x0153 #x200c #x200d #x06ba
     #x00a0 #x060c #x00a2 #x00a3 #x00a4 #x00a5 #x00a6 #x00a7
     #x00a8 #x00a9 #x06be #x00ab #x00ac #x00ad #x00ae #x00af
     #x00b0 #x00b1 #x00b2 #x00b3 #x00b4 #x00b5 #x00b6 #x00b7
     #x00b8 #x00b9 #x061b #x00bb #x00bc #x00bd #x00be #x061f
     #x06c1 #x0621 #x0622 #x0623 #x0624 #x0625 #x0626 #x0627
     #x0628 #x0629 #x062a #x062b #x062c #x062d #x062e #x062f
     #x0630 #x0631 #x0632 #x0633 #x0634 #x0635 #x0636 #x00d7
     #x0637 #x0638 #x0639 #x063a #x0640 #x0641 #x0642 #x0643
     #x00e0 #x0644 #x00e2 #x0645 #x0646 #x0647 #x0648 #x00e7
     #x00e8 #x00e9 #x00ea #x00eb #x0649 #x064a #x00ee #x00ef
     #x064b #x064c #x064d #x064e #x00f4 #x064f #x0650 #x00f7
     #x0651 #x00f9 #x0652 #x00fb #x00fc #x200e #x200f #x06d2)
  :test #'equalp)

(define-unibyte-decoder :cp1256 (octet)
  (if (and (>= octet #x80) (<= octet #xff))
      (svref +cp1256-to-unicode+
             (the ub8 (- octet #x80)))
      octet))

(define-constant +unicode-to-cp1256+
    (loop
      with h = (make-hash-table)
      for code from #x80
      for unicode across +cp1256-to-unicode+
      unless (= unicode #xfffd)
        do (setf (gethash unicode h) code)
      finally (return h))
  :test #'equalp)

(define-unibyte-encoder :cp1256 (code)
  (cond ((< code #x80) code)
        ((gethash code +unicode-to-cp1256+))
        (t (handle-error))))

(define-character-encoding :cp1256
    "An 8-bit, fixed-width character encoding used by Windows for Arabic language"
  :aliases '(:windows-1256)
  :literal-char-code-limit 256
  :codespace `((#x00 #x80)
               ,+unicode-to-cp1256+))
