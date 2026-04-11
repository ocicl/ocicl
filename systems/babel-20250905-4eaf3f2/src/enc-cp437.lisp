;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-cp437.lisp --- Implementation of the IBM Code Page 437
;;;
;;; Copyright (C) 2020, Nicolas Hafner  <shinmera@tymoon.eu>
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

(define-character-encoding :cp437
    "An 8-bit, fixed-width character encoding from IBM."
  :aliases '(:oem-us :oem-437 :pc-8 :dos-latin-us)
  :literal-char-code-limit #xFF)

(define-constant +cp437-to-unicode+
    #(#x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
      #x0008 #x0009 #x000a #x000b #x000c #x000d #x000e #x000f
      #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0016 #x0017
      #x0018 #x0019 #x001a #x001b #x001c #x001d #x001e #x001f
      #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
      #x0028 #x0029 #x002a #x002b #x002c #x002d #x002e #x002f
      #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
      #x0038 #x0039 #x003a #x003b #x003c #x003d #x003e #x003f
      #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
      #x0048 #x0049 #x004a #x004b #x004c #x004d #x004e #x004f
      #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
      #x0058 #x0059 #x005a #x005b #x005c #x005d #x005e #x005f
      #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
      #x0068 #x0069 #x006a #x006b #x006c #x006d #x006e #x006f
      #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
      #x0078 #x0079 #x007a #x007b #x007c #x007d #x007e #x007f
      #x00c7 #x00fc #x00e9 #x00e2 #x00e4 #x00e0 #x00e5 #x00e7
      #x00ea #x00eb #x00e8 #x00ef #x00ee #x00ec #x00c4 #x00c5
      #x00c9 #x00e6 #x00c6 #x00f4 #x00f6 #x00f2 #x00fb #x00f9
      #x00ff #x00d6 #x00dc #x00a2 #x00a3 #x00a5 #x20a7 #x0192
      #x00e1 #x00ed #x00f3 #x00fa #x00f1 #x00d1 #x00aa #x00ba
      #x00bf #x2310 #x00ac #x00bd #x00bc #x00a1 #x00ab #x00bb
      #x2591 #x2592 #x2593 #x2502 #x2524 #x2561 #x2562 #x2556
      #x2555 #x2563 #x2551 #x2557 #x255d #x255c #x255b #x2510
      #x2514 #x2534 #x252c #x251c #x2500 #x253c #x255e #x255f
      #x255a #x2554 #x2569 #x2566 #x2560 #x2550 #x256c #x2567
      #x2568 #x2564 #x2565 #x2559 #x2558 #x2552 #x2553 #x256b
      #x256a #x2518 #x250c #x2588 #x2584 #x258c #x2590 #x2580
      #x03b1 #x00df #x0393 #x03c0 #x03a3 #x03c3 #x00b5 #x03c4
      #x03a6 #x0398 #x03a9 #x03b4 #x221e #x03c6 #x03b5 #x2229
      #x2261 #x00b1 #x2265 #x2264 #x2320 #x2321 #x00f7 #x2248
      #x00b0 #x2219 #x00b7 #x221a #x207f #x00b2 #x25a0 #x00a0)
  :test #'equalp)

(define-unibyte-decoder :cp437 (octet)
  (svref +cp437-to-unicode+ octet))

(define-unibyte-encoder :cp437 (code)
  (if (<= code 127)
      code
      ;; Adjacent code point groups are too small and too many to be
      ;; worth tabulating this, so we just use a case.
      (case code
        (#xA0 #xFF)
        (#xA1 #xAD)
        (#xA2 #x9B)
        (#xA3 #x9C)
        (#xA5 #x9D)
        (#xAA #xA6)
        (#xAB #xAE)
        (#xAC #xAA)
        (#xB0 #xF8)
        (#xB1 #xF1)
        (#xB2 #xFD)
        (#xB5 #xE6)
        (#xB7 #xFA)
        (#xBA #xA7)
        (#xBB #xAF)
        (#xBC #xAC)
        (#xBD #xAB)
        (#xBF #xA8)
        (#xC4 #x8E)
        (#xC5 #x8F)
        (#xC6 #x92)
        (#xC7 #x80)
        (#xC9 #x90)
        (#xD1 #xA5)
        (#xD6 #x99)
        (#xDC #x9A)
        (#xDF #xE1)
        (#xE0 #x85)
        (#xE1 #xA0)
        (#xE2 #x83)
        (#xE4 #x84)
        (#xE5 #x86)
        (#xE6 #x91)
        (#xE7 #x87)
        (#xE8 #x8A)
        (#xE9 #x82)
        (#xEA #x88)
        (#xEB #x89)
        (#xEC #x8D)
        (#xED #xA1)
        (#xEE #x8C)
        (#xEF #x8B)
        (#xF1 #xA4)
        (#xF2 #x95)
        (#xF3 #xA2)
        (#xF4 #x93)
        (#xF6 #x94)
        (#xF7 #xF6)
        (#xF9 #x97)
        (#xFA #xA3)
        (#xFB #x96)
        (#xFC #x81)
        (#xFF #x98)
        (#x192 #x9F)
        (#x393 #xE2)
        (#x398 #xE9)
        (#x3A3 #xE4)
        (#x3A6 #xE8)
        (#x3A9 #xEA)
        (#x3B1 #xE0)
        (#x3B4 #xEB)
        (#x3B5 #xEE)
        (#x3C0 #xE3)
        (#x3C3 #xE5)
        (#x3C4 #xE7)
        (#x3C6 #xED)
        (#x207F #xFC)
        (#x20A7 #x9E)
        (#x2219 #xF9)
        (#x221A #xFB)
        (#x221E #xEC)
        (#x2229 #xEF)
        (#x2248 #xF7)
        (#x2261 #xF0)
        (#x2264 #xF3)
        (#x2265 #xF2)
        (#x2310 #xA9)
        (#x2320 #xF4)
        (#x2321 #xF5)
        (#x2500 #xC4)
        (#x2502 #xB3)
        (#x250C #xDA)
        (#x2510 #xBF)
        (#x2514 #xC0)
        (#x2518 #xD9)
        (#x251C #xC3)
        (#x2524 #xB4)
        (#x252C #xC2)
        (#x2534 #xC1)
        (#x253C #xC5)
        (#x2550 #xCD)
        (#x2551 #xBA)
        (#x2552 #xD5)
        (#x2553 #xD6)
        (#x2554 #xC9)
        (#x2555 #xB8)
        (#x2556 #xB7)
        (#x2557 #xBB)
        (#x2558 #xD4)
        (#x2559 #xD3)
        (#x255A #xC8)
        (#x255B #xBE)
        (#x255C #xBD)
        (#x255D #xBC)
        (#x255E #xC6)
        (#x255F #xC7)
        (#x2560 #xCC)
        (#x2561 #xB5)
        (#x2562 #xB6)
        (#x2563 #xB9)
        (#x2564 #xD1)
        (#x2565 #xD2)
        (#x2566 #xCB)
        (#x2567 #xCF)
        (#x2568 #xD0)
        (#x2569 #xCA)
        (#x256A #xD8)
        (#x256B #xD7)
        (#x256C #xCE)
        (#x2580 #xDF)
        (#x2584 #xDC)
        (#x2588 #xDB)
        (#x258C #xDD)
        (#x2590 #xDE)
        (#x2591 #xB0)
        (#x2592 #xB1)
        (#x2593 #xB2)
        (#x25A0 #xFE)
        (t (handle-error)))))
