;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-cp1252.lisp --- Implementation of the CP1252 character encoding.
;;;
;;; Copyright (C) 2011, Nicolas Martyanoff
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

(define-constant +unicode-0152-017e-cp1252+
    #(#x8c #x9c  nil  nil  nil  nil  nil  nil
       nil  nil  nil  nil  nil  nil #x8a #x9a
       nil  nil  nil  nil  nil  nil  nil  nil
       nil  nil  nil  nil  nil  nil  nil  nil
       nil  nil  nil  nil  nil  nil #x9f  nil
       nil  nil  nil #x8e #x9e)
  :test #'equalp)

(define-constant +unicode-2013-203a-cp1252+
    #(#x96 #x97  nil  nil  nil #x91 #x92 #x82
       nil #x93 #x94 #x84  nil #x86 #x87 #x95
       nil  nil  nil #x85  nil  nil  nil  nil
       nil  nil  nil  nil  nil #x89  nil  nil
       nil  nil  nil  nil  nil  nil #x8b #x9b)
  :test #'equalp)

(define-constant +cp1252-to-unicode+
    #(#x20ac #x0081 #x201a #x0192 #x201e #x2026 #x2020 #x2021
      #x02c6 #x2030 #x0160 #x2039 #x0152 #x008d #x017d #x008f
      #x0090 #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
      #x02dc #x2122 #x0161 #x203a #x0153 #x009d #x017e #x0178)
  :test #'equalp)

(define-character-encoding :cp1252
    "An 8-bit, fixed-width character encoding used by Windows for Western
     European languages."
  :aliases '(:windows-1252)
  :literal-char-code-limit #x80
  :codespace `((0 #x80) (#xa0 #xff) (#x0152 #x017e ,+unicode-0152-017e-cp1252+)
               #x0192 #x02c6 #x02dc (#x2013 #x203a ,+unicode-2013-203a-cp1252+)
               #x20ac #x2122))

(define-unibyte-decoder :cp1252 (octet)
  (if (<= #x80 octet #x9f)
      (svref +cp1252-to-unicode+ (the ub8 (- octet #x80)))
      octet))

(define-unibyte-encoder :cp1252 (code)
  (cond
    ((or (< code #x80) (<= #xa0 code #xff))
     code)
    ((<= #x0152 code #x017e)
     (or (svref +unicode-0152-017e-cp1252+ (the ub8 (- code #x0152)))
         (handle-error)))
    ((= code #x0192) #x83)
    ((= code #x02c6) #x88)
    ((= code #x02dc) #x89)
    ((<= #x2013 code #x203a)
     (or (svref +unicode-2013-203a-cp1252+ (the ub8 (- code #x2013)))
         (handle-error)))
    ((= code #x20ac) #x80)
    ((= code #x2122) #x99)
    (t (handle-error))))
