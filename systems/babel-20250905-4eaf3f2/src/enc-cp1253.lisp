;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-cp1253.lisp --- Implementation of the CP1253 character encoding.
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

(define-constant +cp1253-to-unicode+
    #(#x20ac #xfffd #x201a #x0192 #x201e #x2026 #x2020 #x2021
      #xfffd #x2030 #xfffd #x2039 #xfffd #xfffd #xfffd #xfffd
      #xfffd #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
      #xfffd #x2122 #xfffd #x203a #xfffd #xfffd #xfffd #xfffd
      #x00a0 #x0385 #x0386 #x00a3 #x00a4 #x00a5 #x00a6 #x00a7
      #x00a8 #x00a9 #xfffd #x00ab #x00ac #x00ad #x00ae #x2015
      #x00b0 #x00b1 #x00b2 #x00b3 #x0384 #x00b5 #x00b6 #x00b7
      #x0388 #x0389 #x038a #x00bb #x038c #x00bd #x038e #x038f
      #x0390 #x0391 #x0392 #x0393 #x0394 #x0395 #x0396 #x0397
      #x0398 #x0399 #x039a #x039b #x039c #x039d #x039e #x039f
      #x03a0 #x03a1 #xfffd #x03a3 #x03a4 #x03a5 #x03a6 #x03a7
      #x03a8 #x03a9 #x03aa #x03ab #x03ac #x03ad #x03ae #x03af
      #x03b0 #x03b1 #x03b2 #x03b3 #x03b4 #x03b5 #x03b6 #x03b7
      #x03b8 #x03b9 #x03ba #x03bb #x03bc #x03bd #x03be #x03bf
      #x03c0 #x03c1 #x03c2 #x03c3 #x03c4 #x03c5 #x03c6 #x03c7
      #x03c8 #x03c9 #x03ca #x03cb #x03cc #x03cd #x03ce #xfffd)
  :test #'equalp)

(define-unibyte-decoder :cp1253 (octet)
  (if (and (>= octet #x80) (<= octet #xff))
      (svref +cp1253-to-unicode+
             (the ub8 (- octet #x80)))
      octet))

(define-constant +unicode-to-cp1253+
    (loop
      with h = (make-hash-table)
      for code from #x80
      for unicode across +cp1253-to-unicode+
      unless (= unicode #xfffd)
        do (setf (gethash unicode h) code)
      finally (return h))
  :test #'equalp)

(define-unibyte-encoder :cp1253 (code)
  (cond ((< code #x80) code)
        ((gethash code +unicode-to-cp1253+))
        (t  (handle-error))))

(define-character-encoding :cp1253
    "A 8-bit, fixed-width character encoding used by Windows for modern
Greek."
  :aliases '(:windows-1253)
  :literal-char-code-limit 256
  :codespace `((#x00 #x80)
               ,+unicode-to-cp1253+))
