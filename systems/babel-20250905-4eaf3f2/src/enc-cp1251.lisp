;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-cp1251.lisp --- Implementation of the CP1251 character encoding.
;;;
;;; Copyright (C) 2009, Andrey Moskvitin
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

(define-constant +cp1251-to-unicode+
    #(;; #x80
      #x0402 #x0403 #x201a #x0453 #x201e #x2026 #x2020 #x2021
      #x20ac #x2030 #x0409 #x2039 #x040a #x040c #x040b #x040f
      ;; #x90
      #x0452 #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
      #xfffd #x2122 #x0459 #x203a #x045a #x045c #x045b #x045f
      ;; #xa0
      #x00a0 #x040e #x045e #x0408 #x00a4 #x0490 #x00a6 #x00a7
      #x0401 #x00a9 #x0404 #x00ab #x00ac #x00ad #x00ae #x0407
      ;; #xb0
      #x00b0 #x00b1 #x0406 #x0456 #x0491 #x00b5 #x00b6 #x00b7
      #x0451 #x2116 #x0454 #x00bb #x0458 #x0405 #x0455 #x0457
      ;; #xc0
      #x0410 #x0411 #x0412 #x0413 #x0414 #x0415 #x0416 #x0417
      #x0418 #x0419 #x041a #x041b #x041c #x041d #x041e #x041f
      ;; #xd0
      #x0420 #x0421 #x0422 #x0423 #x0424 #x0425 #x0426 #x0427
      #x0428 #x0429 #x042a #x042b #x042c #x042d #x042e #x042f
      ;; #xe0
      #x0430 #x0431 #x0432 #x0433 #x0434 #x0435 #x0436 #x0437
      #x0438 #x0439 #x043a #x043b #x043c #x043d #x043e #x043f
      ;; #xf0
      #x0440 #x0441 #x0442 #x0443 #x0444 #x0445 #x0446 #x0447
      #x0448 #x0449 #x044a #x044b #x044c #x044d #x044e #x044f)
  :test #'equalp)

(define-unibyte-decoder :cp1251 (octet)
  (if (< octet #x80)
      octet
      (svref +cp1251-to-unicode+ (the ub8 (- octet #x80)))))

(define-constant +unicode-a0-bf-to-cp1251+
    #(#xa0  nil  nil  nil #xa4  nil #xa6 #xa7  ; #xa0-#xa7
       nil #xa9  nil #xab #xac #xad #xae  nil  ; #xa8-#xaf
      #xb0 #xb1  nil  nil  nil #xb5 #xb6 #xb7  ; #xb0-#xb7
       nil  nil  nil #xbb  nil  nil  nil  nil) ; #xb8-#xbf
  :test #'equalp)

(define-constant +unicode-0-97-to-cp1251+
    #( nil #xa8 #x80 #x81 #xaa #xbd #xb2 #xaf  ; #x00-#x07
      #xa3 #x8a #x8c #x8e #x8d  nil #xa1 #x8f  ; #x08-#x0f
      #xc0 #xc1 #xc2 #xc3 #xc4 #xc5 #xc6 #xc7  ; #x10-#x17
      #xc8 #xc9 #xca #xcb #xcc #xcd #xce #xcf  ; #x18-#x1f
      #xd0 #xd1 #xd2 #xd3 #xd4 #xd5 #xd6 #xd7  ; #x20-#x27
      #xd8 #xd9 #xda #xdb #xdc #xdd #xde #xdf  ; #x28-#x2f
      #xe0 #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 #xe7  ; #x30-#x37
      #xe8 #xe9 #xea #xeb #xec #xed #xee #xef  ; #x38-#x3f
      #xf0 #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7  ; #x40-#x47
      #xf8 #xf9 #xfa #xfb #xfc #xfd #xfe #xff  ; #x48-#x4f
       nil #xb8 #x90 #x83 #xba #xbe #xb3 #xbf  ; #x50-#x57
      #xbc #x9a #x9c #x9e #x9d  nil #xa2 #x9f  ; #x58-#x5f
       nil  nil  nil  nil  nil  nil  nil  nil  ; #x60-#x67
       nil  nil  nil  nil  nil  nil  nil  nil  ; #x68-#x6f
       nil  nil  nil  nil  nil  nil  nil  nil  ; #x70-#x77
       nil  nil  nil  nil  nil  nil  nil  nil  ; #x78-#x7f
       nil  nil  nil  nil  nil  nil  nil  nil  ; #x80-#x87
       nil  nil  nil  nil  nil  nil  nil  nil  ; #x88-#x8f
      #xa5 #xb4  nil  nil  nil  nil  nil  nil) ; #x90-#x97
  :test #'equalp)

(define-constant +unicode-10-3f-to-cp1251+
    #( nil  nil  nil #x96 #x97  nil  nil  nil  ; #x10-#x17
      #x91 #x92 #x82  nil #x93 #x94 #x84  nil  ; #x18-#x1f
      #x86 #x87 #x95  nil  nil  nil #x85  nil  ; #x20-#x27
       nil  nil  nil  nil  nil  nil  nil  nil  ; #x28-#x2f
      #x89  nil  nil  nil  nil  nil  nil  nil  ; #x30-#x37
       nil #x8b #x9b  nil  nil  nil  nil  nil) ; #x38-#x3f
  :test #'equalp)

(define-character-encoding :cp1251
    "An 8-bit, fixed-width character Russian encoding from Windows."
  :aliases '(:windows-1251)
  :literal-char-code-limit #x80
  :codespace `((#x0 #x80)
               (#xa0 #xc0 ,+unicode-a0-bf-to-cp1251+)
               (#x400 #x498 ,+unicode-0-97-to-cp1251+)
               (#x2010 #x2040 ,+unicode-10-3f-to-cp1251+)
               #x20ac #x2116 #x2122))

(define-unibyte-encoder :cp1251 (code)
  (cond
    ((< code #x80) code)
    ((<= #xa0 code #xbf)
     (or (svref +unicode-a0-bf-to-cp1251+ (the ub8 (- code #xa0)))
         (handle-error)))
    ((<= #x400 code #x497)
     (or (svref +unicode-0-97-to-cp1251+ (the ub8 (- code #x400)))
         (handle-error)))
    ((<= #x2010 code #x203f)
     (or (svref +unicode-10-3f-to-cp1251+ (the ub8 (- code #x2010)))
         (handle-error)))
    ((= code #x20ac) #x88)
    ((= code #x2116) #xb9)
    ((= code #x2122) #x99)
    (t (handle-error))))
