;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-cp1255.lisp --- Implementation of the CP1255 character encoding.
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

(define-constant +cp1255-to-unicode+
    #(#x20ac #xfffd #x201a #x0192 #x201e #x2026 #x2020 #x2021
      #x02c6 #x2030 #xfffd #x2039 #xfffd #xfffd #xfffd #xfffd
      #xfffd #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
      #x02dc #x2122 #xfffd #x203a #xfffd #xfffd #xfffd #xfffd
      #x00a0 #x00a1 #x00a2 #x00a3 #x20aa #x00a5 #x00a6 #x00a7
      #x00a8 #x00a9 #x00d7 #x00ab #x00ac #x00ad #x00ae #x00af
      #x00b0 #x00b1 #x00b2 #x00b3 #x00b4 #x00b5 #x00b6 #x00b7
      #x00b8 #x00b9 #x00f7 #x00bb #x00bc #x00bd #x00be #x00bf
      #x05b0 #x05b1 #x05b2 #x05b3 #x05b4 #x05b5 #x05b6 #x05b7
      #x05b8 #x05b9 #xfffd #x05bb #x05bc #x05bd #x05be #x05bf
      #x05c0 #x05c1 #x05c2 #x05c3 #x05f0 #x05f1 #x05f2 #x05f3
      #x05f4 #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
      #x05d0 #x05d1 #x05d2 #x05d3 #x05d4 #x05d5 #x05d6 #x05d7
      #x05d8 #x05d9 #x05da #x05db #x05dc #x05dd #x05de #x05df
      #x05e0 #x05e1 #x05e2 #x05e3 #x05e4 #x05e5 #x05e6 #x05e7
      #x05e8 #x05e9 #x05ea #xfffd #xfffd #x200e #x200f #xfffd)
  :test #'equalp)

(define-unibyte-decoder :cp1255 (octet)
  (if (and (>= octet #x80) (<= octet #xff))
      (svref +cp1255-to-unicode+
             (the ub8 (- octet #x80)))
      octet))

(define-constant +unicode-to-cp1255+
    (loop
      with h = (make-hash-table)
      for code from #x80
      for unicode across +cp1255-to-unicode+
      unless (= unicode #xfffd)
        do (setf (gethash unicode h) code)
      finally (return h))
  :test #'equalp)

(define-unibyte-encoder :cp1255 (code)
  (cond ((< code #x80) code)
        ((gethash code +unicode-to-cp1255+))
        (t  (handle-error))))

(define-character-encoding :cp1255
    "A 8-bit, fixed-width character encoding used by Windows for Hebrew"
  :aliases '(:windows-1255)
  :literal-char-code-limit 256
  :codespace `((#x00 #x80)
               ,+unicode-to-cp1255+))
