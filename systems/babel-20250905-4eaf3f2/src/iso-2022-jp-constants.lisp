;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; iso-2022-jp-constants.lisp --- Implementation of the ISO-2022-JP character encoding.
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

(define-constant +jis-x-0208-to-uni-page21+
   #(;; #x21
     #x3000 #x3001 #x3002 #xff0c #xff0e #x30fb #xff1a #xff1b
     #xff1f #xff01 #x309b #x309c #x00b4 #xff40 #x00a8 #xff3e
     #xffe3 #xff3f #x30fd #x30fe #x309d #x309e #x3003 #x4edd
     #x3005 #x3006 #x3007 #x30fc #x2015 #x2010 #xff0f #xff3c
     #x301c #x2016 #xff5c #x2026 #x2025 #x2018 #x2019 #x201c
     #x201d #xff08 #xff09 #x3014 #x3015 #xff3b #xff3d #xff5b
     #xff5d #x3008 #x3009 #x300a #x300b #x300c #x300d #x300e
     #x300f #x3010 #x3011 #xff0b #x2212 #x00b1 #x00d7 #x00f7
     #xff1d #x2260 #xff1c #xff1e #x2266 #x2267 #x221e #x2234
     #x2642 #x2640 #x00b0 #x2032 #x2033 #x2103 #xffe5 #xff04
     #x00a2 #x00a3 #xff05 #xff03 #xff06 #xff0a #xff20 #x00a7
     #x2606 #x2605 #x25cb #x25cf #x25ce #x25c7 
     ;; #x22
     #x25c6 #x25a1 #x25a0 #x25b3 #x25b2 #x25bd #x25bc #x203b
     #x3012 #x2192 #x2190 #x2191 #x2193 #x3013 #xfffd #xfffd
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #xfffd #x2208 #x220b #x2286 #x2287 #x2282 #x2283 #x222a
     #x2229 #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #xfffd #x2227 #x2228 #x00ac #x21d2 #x21d4 #x2200 #x2203
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #xfffd #xfffd #xfffd #x2220 #x22a5 #x2312 #x2202 #x2207
     #x2261 #x2252 #x226a #x226b #x221a #x223d #x221d #x2235
     #x222b #x222c #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #xfffd #x212b #x2030 #x266f #x266d #x266a #x2020 #x2021
     #x00b6 #xfffd #xfffd #xfffd #xfffd #x25ef 
     ;; #x23
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xff10
     #xff11 #xff12 #xff13 #xff14 #xff15 #xff16 #xff17 #xff18
     #xff19 #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #xff21 #xff22 #xff23 #xff24 #xff25 #xff26 #xff27 #xff28
     #xff29 #xff2a #xff2b #xff2c #xff2d #xff2e #xff2f #xff30
     #xff31 #xff32 #xff33 #xff34 #xff35 #xff36 #xff37 #xff38
     #xff39 #xff3a #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #xff41 #xff42 #xff43 #xff44 #xff45 #xff46 #xff47 #xff48
     #xff49 #xff4a #xff4b #xff4c #xff4d #xff4e #xff4f #xff50
     #xff51 #xff52 #xff53 #xff54 #xff55 #xff56 #xff57 #xff58
     #xff59 #xff5a #xfffd #xfffd #xfffd #xfffd 
     ;; #x24
     #x3041 #x3042 #x3043 #x3044 #x3045 #x3046 #x3047 #x3048
     #x3049 #x304a #x304b #x304c #x304d #x304e #x304f #x3050
     #x3051 #x3052 #x3053 #x3054 #x3055 #x3056 #x3057 #x3058
     #x3059 #x305a #x305b #x305c #x305d #x305e #x305f #x3060
     #x3061 #x3062 #x3063 #x3064 #x3065 #x3066 #x3067 #x3068
     #x3069 #x306a #x306b #x306c #x306d #x306e #x306f #x3070
     #x3071 #x3072 #x3073 #x3074 #x3075 #x3076 #x3077 #x3078
     #x3079 #x307a #x307b #x307c #x307d #x307e #x307f #x3080
     #x3081 #x3082 #x3083 #x3084 #x3085 #x3086 #x3087 #x3088
     #x3089 #x308a #x308b #x308c #x308d #x308e #x308f #x3090
     #x3091 #x3092 #x3093 #xfffd #xfffd #xfffd #xfffd #xfffd
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd 
     ;; #x25
     #x30a1 #x30a2 #x30a3 #x30a4 #x30a5 #x30a6 #x30a7 #x30a8
     #x30a9 #x30aa #x30ab #x30ac #x30ad #x30ae #x30af #x30b0
     #x30b1 #x30b2 #x30b3 #x30b4 #x30b5 #x30b6 #x30b7 #x30b8
     #x30b9 #x30ba #x30bb #x30bc #x30bd #x30be #x30bf #x30c0
     #x30c1 #x30c2 #x30c3 #x30c4 #x30c5 #x30c6 #x30c7 #x30c8
     #x30c9 #x30ca #x30cb #x30cc #x30cd #x30ce #x30cf #x30d0
     #x30d1 #x30d2 #x30d3 #x30d4 #x30d5 #x30d6 #x30d7 #x30d8
     #x30d9 #x30da #x30db #x30dc #x30dd #x30de #x30df #x30e0
     #x30e1 #x30e2 #x30e3 #x30e4 #x30e5 #x30e6 #x30e7 #x30e8
     #x30e9 #x30ea #x30eb #x30ec #x30ed #x30ee #x30ef #x30f0
     #x30f1 #x30f2 #x30f3 #x30f4 #x30f5 #x30f6 #xfffd #xfffd
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd 
     ;; #x26
     #x0391 #x0392 #x0393 #x0394 #x0395 #x0396 #x0397 #x0398
     #x0399 #x039a #x039b #x039c #x039d #x039e #x039f #x03a0
     #x03a1 #x03a3 #x03a4 #x03a5 #x03a6 #x03a7 #x03a8 #x03a9
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #x03b1 #x03b2 #x03b3 #x03b4 #x03b5 #x03b6 #x03b7 #x03b8
     #x03b9 #x03ba #x03bb #x03bc #x03bd #x03be #x03bf #x03c0
     #x03c1 #x03c3 #x03c4 #x03c5 #x03c6 #x03c7 #x03c8 #x03c9
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd 
     ;; #x27
     #x0410 #x0411 #x0412 #x0413 #x0414 #x0415 #x0401 #x0416
     #x0417 #x0418 #x0419 #x041a #x041b #x041c #x041d #x041e
     #x041f #x0420 #x0421 #x0422 #x0423 #x0424 #x0425 #x0426
     #x0427 #x0428 #x0429 #x042a #x042b #x042c #x042d #x042e
     #x042f #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #x0430 #x0431 #x0432 #x0433 #x0434 #x0435 #x0451 #x0436
     #x0437 #x0438 #x0439 #x043a #x043b #x043c #x043d #x043e
     #x043f #x0440 #x0441 #x0442 #x0443 #x0444 #x0445 #x0446
     #x0447 #x0448 #x0449 #x044a #x044b #x044c #x044d #x044e
     #x044f #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd 
     ;; #x28
     #x2500 #x2502 #x250c #x2510 #x2518 #x2514 #x251c #x252c
     #x2524 #x2534 #x253c #x2501 #x2503 #x250f #x2513 #x251b
     #x2517 #x2523 #x2533 #x252b #x253b #x254b #x2520 #x252f
     #x2528 #x2537 #x253f #x251d #x2530 #x2525 #x2538 #x2542)
  :test #'equalp)

(define-constant +jis-x-0208-to-uni-page30+
   #(;; #x30
     #x4e9c #x5516 #x5a03 #x963f #x54c0 #x611b #x6328 #x59f6
     #x9022 #x8475 #x831c #x7a50 #x60aa #x63e1 #x6e25 #x65ed
     #x8466 #x82a6 #x9bf5 #x6893 #x5727 #x65a1 #x6271 #x5b9b
     #x59d0 #x867b #x98f4 #x7d62 #x7dbe #x9b8e #x6216 #x7c9f
     #x88b7 #x5b89 #x5eb5 #x6309 #x6697 #x6848 #x95c7 #x978d
     #x674f #x4ee5 #x4f0a #x4f4d #x4f9d #x5049 #x56f2 #x5937
     #x59d4 #x5a01 #x5c09 #x60df #x610f #x6170 #x6613 #x6905
     #x70ba #x754f #x7570 #x79fb #x7dad #x7def #x80c3 #x840e
     #x8863 #x8b02 #x9055 #x907a #x533b #x4e95 #x4ea5 #x57df
     #x80b2 #x90c1 #x78ef #x4e00 #x58f1 #x6ea2 #x9038 #x7a32
     #x8328 #x828b #x9c2f #x5141 #x5370 #x54bd #x54e1 #x56e0
     #x59fb #x5f15 #x98f2 #x6deb #x80e4 #x852d 
     ;; #x31
     #x9662 #x9670 #x96a0 #x97fb #x540b #x53f3 #x5b87 #x70cf
     #x7fbd #x8fc2 #x96e8 #x536f #x9d5c #x7aba #x4e11 #x7893
     #x81fc #x6e26 #x5618 #x5504 #x6b1d #x851a #x9c3b #x59e5
     #x53a9 #x6d66 #x74dc #x958f #x5642 #x4e91 #x904b #x96f2
     #x834f #x990c #x53e1 #x55b6 #x5b30 #x5f71 #x6620 #x66f3
     #x6804 #x6c38 #x6cf3 #x6d29 #x745b #x76c8 #x7a4e #x9834
     #x82f1 #x885b #x8a60 #x92ed #x6db2 #x75ab #x76ca #x99c5
     #x60a6 #x8b01 #x8d8a #x95b2 #x698e #x53ad #x5186 #x5712
     #x5830 #x5944 #x5bb4 #x5ef6 #x6028 #x63a9 #x63f4 #x6cbf
     #x6f14 #x708e #x7114 #x7159 #x71d5 #x733f #x7e01 #x8276
     #x82d1 #x8597 #x9060 #x925b #x9d1b #x5869 #x65bc #x6c5a
     #x7525 #x51f9 #x592e #x5965 #x5f80 #x5fdc 
     ;; #x32
     #x62bc #x65fa #x6a2a #x6b27 #x6bb4 #x738b #x7fc1 #x8956
     #x9d2c #x9d0e #x9ec4 #x5ca1 #x6c96 #x837b #x5104 #x5c4b
     #x61b6 #x81c6 #x6876 #x7261 #x4e59 #x4ffa #x5378 #x6069
     #x6e29 #x7a4f #x97f3 #x4e0b #x5316 #x4eee #x4f55 #x4f3d
     #x4fa1 #x4f73 #x52a0 #x53ef #x5609 #x590f #x5ac1 #x5bb6
     #x5be1 #x79d1 #x6687 #x679c #x67b6 #x6b4c #x6cb3 #x706b
     #x73c2 #x798d #x79be #x7a3c #x7b87 #x82b1 #x82db #x8304
     #x8377 #x83ef #x83d3 #x8766 #x8ab2 #x5629 #x8ca8 #x8fe6
     #x904e #x971e #x868a #x4fc4 #x5ce8 #x6211 #x7259 #x753b
     #x81e5 #x82bd #x86fe #x8cc0 #x96c5 #x9913 #x99d5 #x4ecb
     #x4f1a #x89e3 #x56de #x584a #x58ca #x5efb #x5feb #x602a
     #x6094 #x6062 #x61d0 #x6212 #x62d0 #x6539 
     ;; #x33
     #x9b41 #x6666 #x68b0 #x6d77 #x7070 #x754c #x7686 #x7d75
     #x82a5 #x87f9 #x958b #x968e #x8c9d #x51f1 #x52be #x5916
     #x54b3 #x5bb3 #x5d16 #x6168 #x6982 #x6daf #x788d #x84cb
     #x8857 #x8a72 #x93a7 #x9ab8 #x6d6c #x99a8 #x86d9 #x57a3
     #x67ff #x86ce #x920e #x5283 #x5687 #x5404 #x5ed3 #x62e1
     #x64b9 #x683c #x6838 #x6bbb #x7372 #x78ba #x7a6b #x899a
     #x89d2 #x8d6b #x8f03 #x90ed #x95a3 #x9694 #x9769 #x5b66
     #x5cb3 #x697d #x984d #x984e #x639b #x7b20 #x6a2b #x6a7f
     #x68b6 #x9c0d #x6f5f #x5272 #x559d #x6070 #x62ec #x6d3b
     #x6e07 #x6ed1 #x845b #x8910 #x8f44 #x4e14 #x9c39 #x53f6
     #x691b #x6a3a #x9784 #x682a #x515c #x7ac3 #x84b2 #x91dc
     #x938c #x565b #x9d28 #x6822 #x8305 #x8431 
     ;; #x34
     #x7ca5 #x5208 #x82c5 #x74e6 #x4e7e #x4f83 #x51a0 #x5bd2
     #x520a #x52d8 #x52e7 #x5dfb #x559a #x582a #x59e6 #x5b8c
     #x5b98 #x5bdb #x5e72 #x5e79 #x60a3 #x611f #x6163 #x61be
     #x63db #x6562 #x67d1 #x6853 #x68fa #x6b3e #x6b53 #x6c57
     #x6f22 #x6f97 #x6f45 #x74b0 #x7518 #x76e3 #x770b #x7aff
     #x7ba1 #x7c21 #x7de9 #x7f36 #x7ff0 #x809d #x8266 #x839e
     #x89b3 #x8acc #x8cab #x9084 #x9451 #x9593 #x9591 #x95a2
     #x9665 #x97d3 #x9928 #x8218 #x4e38 #x542b #x5cb8 #x5dcc
     #x73a9 #x764c #x773c #x5ca9 #x7feb #x8d0b #x96c1 #x9811
     #x9854 #x9858 #x4f01 #x4f0e #x5371 #x559c #x5668 #x57fa
     #x5947 #x5b09 #x5bc4 #x5c90 #x5e0c #x5e7e #x5fcc #x63ee
     #x673a #x65d7 #x65e2 #x671f #x68cb #x68c4 
     ;; #x35
     #x6a5f #x5e30 #x6bc5 #x6c17 #x6c7d #x757f #x7948 #x5b63
     #x7a00 #x7d00 #x5fbd #x898f #x8a18 #x8cb4 #x8d77 #x8ecc
     #x8f1d #x98e2 #x9a0e #x9b3c #x4e80 #x507d #x5100 #x5993
     #x5b9c #x622f #x6280 #x64ec #x6b3a #x72a0 #x7591 #x7947
     #x7fa9 #x87fb #x8abc #x8b70 #x63ac #x83ca #x97a0 #x5409
     #x5403 #x55ab #x6854 #x6a58 #x8a70 #x7827 #x6775 #x9ecd
     #x5374 #x5ba2 #x811a #x8650 #x9006 #x4e18 #x4e45 #x4ec7
     #x4f11 #x53ca #x5438 #x5bae #x5f13 #x6025 #x6551 #x673d
     #x6c42 #x6c72 #x6ce3 #x7078 #x7403 #x7a76 #x7aae #x7b08
     #x7d1a #x7cfe #x7d66 #x65e7 #x725b #x53bb #x5c45 #x5de8
     #x62d2 #x62e0 #x6319 #x6e20 #x865a #x8a31 #x8ddd #x92f8
     #x6f01 #x79a6 #x9b5a #x4ea8 #x4eab #x4eac 
     ;; #x36
     #x4f9b #x4fa0 #x50d1 #x5147 #x7af6 #x5171 #x51f6 #x5354
     #x5321 #x537f #x53eb #x55ac #x5883 #x5ce1 #x5f37 #x5f4a
     #x602f #x6050 #x606d #x631f #x6559 #x6a4b #x6cc1 #x72c2
     #x72ed #x77ef #x80f8 #x8105 #x8208 #x854e #x90f7 #x93e1
     #x97ff #x9957 #x9a5a #x4ef0 #x51dd #x5c2d #x6681 #x696d
     #x5c40 #x66f2 #x6975 #x7389 #x6850 #x7c81 #x50c5 #x52e4
     #x5747 #x5dfe #x9326 #x65a4 #x6b23 #x6b3d #x7434 #x7981
     #x79bd #x7b4b #x7dca #x82b9 #x83cc #x887f #x895f #x8b39
     #x8fd1 #x91d1 #x541f #x9280 #x4e5d #x5036 #x53e5 #x533a
     #x72d7 #x7396 #x77e9 #x82e6 #x8eaf #x99c6 #x99c8 #x99d2
     #x5177 #x611a #x865e #x55b0 #x7a7a #x5076 #x5bd3 #x9047
     #x9685 #x4e32 #x6adb #x91e7 #x5c51 #x5c48 
     ;; #x37
     #x6398 #x7a9f #x6c93 #x9774 #x8f61 #x7aaa #x718a #x9688
     #x7c82 #x6817 #x7e70 #x6851 #x936c #x52f2 #x541b #x85ab
     #x8a13 #x7fa4 #x8ecd #x90e1 #x5366 #x8888 #x7941 #x4fc2
     #x50be #x5211 #x5144 #x5553 #x572d #x73ea #x578b #x5951
     #x5f62 #x5f84 #x6075 #x6176 #x6167 #x61a9 #x63b2 #x643a
     #x656c #x666f #x6842 #x6e13 #x7566 #x7a3d #x7cfb #x7d4c
     #x7d99 #x7e4b #x7f6b #x830e #x834a #x86cd #x8a08 #x8a63
     #x8b66 #x8efd #x981a #x9d8f #x82b8 #x8fce #x9be8 #x5287
     #x621f #x6483 #x6fc0 #x9699 #x6841 #x5091 #x6b20 #x6c7a
     #x6f54 #x7a74 #x7d50 #x8840 #x8a23 #x6708 #x4ef6 #x5039
     #x5026 #x5065 #x517c #x5238 #x5263 #x55a7 #x570f #x5805
     #x5acc #x5efa #x61b2 #x61f8 #x62f3 #x6372 
     ;; #x38
     #x691c #x6a29 #x727d #x72ac #x732e #x7814 #x786f #x7d79
     #x770c #x80a9 #x898b #x8b19 #x8ce2 #x8ed2 #x9063 #x9375
     #x967a #x9855 #x9a13 #x9e78 #x5143 #x539f #x53b3 #x5e7b
     #x5f26 #x6e1b #x6e90 #x7384 #x73fe #x7d43 #x8237 #x8a00
     #x8afa #x9650 #x4e4e #x500b #x53e4 #x547c #x56fa #x59d1
     #x5b64 #x5df1 #x5eab #x5f27 #x6238 #x6545 #x67af #x6e56
     #x72d0 #x7cca #x88b4 #x80a1 #x80e1 #x83f0 #x864e #x8a87
     #x8de8 #x9237 #x96c7 #x9867 #x9f13 #x4e94 #x4e92 #x4f0d
     #x5348 #x5449 #x543e #x5a2f #x5f8c #x5fa1 #x609f #x68a7
     #x6a8e #x745a #x7881 #x8a9e #x8aa4 #x8b77 #x9190 #x4e5e
     #x9bc9 #x4ea4 #x4f7c #x4faf #x5019 #x5016 #x5149 #x516c
     #x529f #x52b9 #x52fe #x539a #x53e3 #x5411 
     ;; #x39
     #x540e #x5589 #x5751 #x57a2 #x597d #x5b54 #x5b5d #x5b8f
     #x5de5 #x5de7 #x5df7 #x5e78 #x5e83 #x5e9a #x5eb7 #x5f18
     #x6052 #x614c #x6297 #x62d8 #x63a7 #x653b #x6602 #x6643
     #x66f4 #x676d #x6821 #x6897 #x69cb #x6c5f #x6d2a #x6d69
     #x6e2f #x6e9d #x7532 #x7687 #x786c #x7a3f #x7ce0 #x7d05
     #x7d18 #x7d5e #x7db1 #x8015 #x8003 #x80af #x80b1 #x8154
     #x818f #x822a #x8352 #x884c #x8861 #x8b1b #x8ca2 #x8cfc
     #x90ca #x9175 #x9271 #x783f #x92fc #x95a4 #x964d #x9805
     #x9999 #x9ad8 #x9d3b #x525b #x52ab #x53f7 #x5408 #x58d5
     #x62f7 #x6fe0 #x8c6a #x8f5f #x9eb9 #x514b #x523b #x544a
     #x56fd #x7a40 #x9177 #x9d60 #x9ed2 #x7344 #x6f09 #x8170
     #x7511 #x5ffd #x60da #x9aa8 #x72db #x8fbc 
     ;; #x3a
     #x6b64 #x9803 #x4eca #x56f0 #x5764 #x58be #x5a5a #x6068
     #x61c7 #x660f #x6606 #x6839 #x68b1 #x6df7 #x75d5 #x7d3a
     #x826e #x9b42 #x4e9b #x4f50 #x53c9 #x5506 #x5d6f #x5de6
     #x5dee #x67fb #x6c99 #x7473 #x7802 #x8a50 #x9396 #x88df
     #x5750 #x5ea7 #x632b #x50b5 #x50ac #x518d #x6700 #x54c9
     #x585e #x59bb #x5bb0 #x5f69 #x624d #x63a1 #x683d #x6b73
     #x6e08 #x707d #x91c7 #x7280 #x7815 #x7826 #x796d #x658e
     #x7d30 #x83dc #x88c1 #x8f09 #x969b #x5264 #x5728 #x6750
     #x7f6a #x8ca1 #x51b4 #x5742 #x962a #x583a #x698a #x80b4
     #x54b2 #x5d0e #x57fc #x7895 #x9dfa #x4f5c #x524a #x548b
     #x643e #x6628 #x6714 #x67f5 #x7a84 #x7b56 #x7d22 #x932f
     #x685c #x9bad #x7b39 #x5319 #x518a #x5237 
     ;; #x3b
     #x5bdf #x62f6 #x64ae #x64e6 #x672d #x6bba #x85a9 #x96d1
     #x7690 #x9bd6 #x634c #x9306 #x9bab #x76bf #x6652 #x4e09
     #x5098 #x53c2 #x5c71 #x60e8 #x6492 #x6563 #x685f #x71e6
     #x73ca #x7523 #x7b97 #x7e82 #x8695 #x8b83 #x8cdb #x9178
     #x9910 #x65ac #x66ab #x6b8b #x4ed5 #x4ed4 #x4f3a #x4f7f
     #x523a #x53f8 #x53f2 #x55e3 #x56db #x58eb #x59cb #x59c9
     #x59ff #x5b50 #x5c4d #x5e02 #x5e2b #x5fd7 #x601d #x6307
     #x652f #x5b5c #x65af #x65bd #x65e8 #x679d #x6b62 #x6b7b
     #x6c0f #x7345 #x7949 #x79c1 #x7cf8 #x7d19 #x7d2b #x80a2
     #x8102 #x81f3 #x8996 #x8a5e #x8a69 #x8a66 #x8a8c #x8aee
     #x8cc7 #x8cdc #x96cc #x98fc #x6b6f #x4e8b #x4f3c #x4f8d
     #x5150 #x5b57 #x5bfa #x6148 #x6301 #x6642 
     ;; #x3c
     #x6b21 #x6ecb #x6cbb #x723e #x74bd #x75d4 #x78c1 #x793a
     #x800c #x8033 #x81ea #x8494 #x8f9e #x6c50 #x9e7f #x5f0f
     #x8b58 #x9d2b #x7afa #x8ef8 #x5b8d #x96eb #x4e03 #x53f1
     #x57f7 #x5931 #x5ac9 #x5ba4 #x6089 #x6e7f #x6f06 #x75be
     #x8cea #x5b9f #x8500 #x7be0 #x5072 #x67f4 #x829d #x5c61
     #x854a #x7e1e #x820e #x5199 #x5c04 #x6368 #x8d66 #x659c
     #x716e #x793e #x7d17 #x8005 #x8b1d #x8eca #x906e #x86c7
     #x90aa #x501f #x52fa #x5c3a #x6753 #x707c #x7235 #x914c
     #x91c8 #x932b #x82e5 #x5bc2 #x5f31 #x60f9 #x4e3b #x53d6
     #x5b88 #x624b #x6731 #x6b8a #x72e9 #x73e0 #x7a2e #x816b
     #x8da3 #x9152 #x9996 #x5112 #x53d7 #x546a #x5bff #x6388
     #x6a39 #x7dac #x9700 #x56da #x53ce #x5468 
     ;; #x3d
     #x5b97 #x5c31 #x5dde #x4fee #x6101 #x62fe #x6d32 #x79c0
     #x79cb #x7d42 #x7e4d #x7fd2 #x81ed #x821f #x8490 #x8846
     #x8972 #x8b90 #x8e74 #x8f2f #x9031 #x914b #x916c #x96c6
     #x919c #x4ec0 #x4f4f #x5145 #x5341 #x5f93 #x620e #x67d4
     #x6c41 #x6e0b #x7363 #x7e26 #x91cd #x9283 #x53d4 #x5919
     #x5bbf #x6dd1 #x795d #x7e2e #x7c9b #x587e #x719f #x51fa
     #x8853 #x8ff0 #x4fca #x5cfb #x6625 #x77ac #x7ae3 #x821c
     #x99ff #x51c6 #x5faa #x65ec #x696f #x6b89 #x6df3 #x6e96
     #x6f64 #x76fe #x7d14 #x5de1 #x9075 #x9187 #x9806 #x51e6
     #x521d #x6240 #x6691 #x66d9 #x6e1a #x5eb6 #x7dd2 #x7f72
     #x66f8 #x85af #x85f7 #x8af8 #x52a9 #x53d9 #x5973 #x5e8f
     #x5f90 #x6055 #x92e4 #x9664 #x50b7 #x511f 
     ;; #x3e
     #x52dd #x5320 #x5347 #x53ec #x54e8 #x5546 #x5531 #x5617
     #x5968 #x59be #x5a3c #x5bb5 #x5c06 #x5c0f #x5c11 #x5c1a
     #x5e84 #x5e8a #x5ee0 #x5f70 #x627f #x6284 #x62db #x638c
     #x6377 #x6607 #x660c #x662d #x6676 #x677e #x68a2 #x6a1f
     #x6a35 #x6cbc #x6d88 #x6e09 #x6e58 #x713c #x7126 #x7167
     #x75c7 #x7701 #x785d #x7901 #x7965 #x79f0 #x7ae0 #x7b11
     #x7ca7 #x7d39 #x8096 #x83d6 #x848b #x8549 #x885d #x88f3
     #x8a1f #x8a3c #x8a54 #x8a73 #x8c61 #x8cde #x91a4 #x9266
     #x937e #x9418 #x969c #x9798 #x4e0a #x4e08 #x4e1e #x4e57
     #x5197 #x5270 #x57ce #x5834 #x58cc #x5b22 #x5e38 #x60c5
     #x64fe #x6761 #x6756 #x6d44 #x72b6 #x7573 #x7a63 #x84b8
     #x8b72 #x91b8 #x9320 #x5631 #x57f4 #x98fe 
     ;; #x3f
     #x62ed #x690d #x6b96 #x71ed #x7e54 #x8077 #x8272 #x89e6
     #x98df #x8755 #x8fb1 #x5c3b #x4f38 #x4fe1 #x4fb5 #x5507
     #x5a20 #x5bdd #x5be9 #x5fc3 #x614e #x632f #x65b0 #x664b
     #x68ee #x699b #x6d78 #x6df1 #x7533 #x75b9 #x771f #x795e
     #x79e6 #x7d33 #x81e3 #x82af #x85aa #x89aa #x8a3a #x8eab
     #x8f9b #x9032 #x91dd #x9707 #x4eba #x4ec1 #x5203 #x5875
     #x58ec #x5c0b #x751a #x5c3d #x814e #x8a0a #x8fc5 #x9663
     #x976d #x7b25 #x8acf #x9808 #x9162 #x56f3 #x53a8 #x9017
     #x5439 #x5782 #x5e25 #x63a8 #x6c34 #x708a #x7761 #x7c8b
     #x7fe0 #x8870 #x9042 #x9154 #x9310 #x9318 #x968f #x745e
     #x9ac4 #x5d07 #x5d69 #x6570 #x67a2 #x8da8 #x96db #x636e
     #x6749 #x6919 #x83c5 #x9817 #x96c0 #x88fe 
     ;; #x40
     #x6f84 #x647a #x5bf8 #x4e16 #x702c #x755d #x662f #x51c4
     #x5236 #x52e2 #x59d3 #x5f81 #x6027 #x6210 #x653f #x6574
     #x661f #x6674 #x68f2 #x6816 #x6b63 #x6e05 #x7272 #x751f
     #x76db #x7cbe #x8056 #x58f0 #x88fd #x897f #x8aa0 #x8a93
     #x8acb #x901d #x9192 #x9752 #x9759 #x6589 #x7a0e #x8106
     #x96bb #x5e2d #x60dc #x621a #x65a5 #x6614 #x6790 #x77f3
     #x7a4d #x7c4d #x7e3e #x810a #x8cac #x8d64 #x8de1 #x8e5f
     #x78a9 #x5207 #x62d9 #x63a5 #x6442 #x6298 #x8a2d #x7a83
     #x7bc0 #x8aac #x96ea #x7d76 #x820c #x8749 #x4ed9 #x5148
     #x5343 #x5360 #x5ba3 #x5c02 #x5c16 #x5ddd #x6226 #x6247
     #x64b0 #x6813 #x6834 #x6cc9 #x6d45 #x6d17 #x67d3 #x6f5c
     #x714e #x717d #x65cb #x7a7f #x7bad #x7dda 
     ;; #x41
     #x7e4a #x7fa8 #x817a #x821b #x8239 #x85a6 #x8a6e #x8cce
     #x8df5 #x9078 #x9077 #x92ad #x9291 #x9583 #x9bae #x524d
     #x5584 #x6f38 #x7136 #x5168 #x7985 #x7e55 #x81b3 #x7cce
     #x564c #x5851 #x5ca8 #x63aa #x66fe #x66fd #x695a #x72d9
     #x758f #x758e #x790e #x7956 #x79df #x7c97 #x7d20 #x7d44
     #x8607 #x8a34 #x963b #x9061 #x9f20 #x50e7 #x5275 #x53cc
     #x53e2 #x5009 #x55aa #x58ee #x594f #x723d #x5b8b #x5c64
     #x531d #x60e3 #x60f3 #x635c #x6383 #x633f #x63bb #x64cd
     #x65e9 #x66f9 #x5de3 #x69cd #x69fd #x6f15 #x71e5 #x4e89
     #x75e9 #x76f8 #x7a93 #x7cdf #x7dcf #x7d9c #x8061 #x8349
     #x8358 #x846c #x84bc #x85fb #x88c5 #x8d70 #x9001 #x906d
     #x9397 #x971c #x9a12 #x50cf #x5897 #x618e 
     ;; #x42
     #x81d3 #x8535 #x8d08 #x9020 #x4fc3 #x5074 #x5247 #x5373
     #x606f #x6349 #x675f #x6e2c #x8db3 #x901f #x4fd7 #x5c5e
     #x8cca #x65cf #x7d9a #x5352 #x8896 #x5176 #x63c3 #x5b58
     #x5b6b #x5c0a #x640d #x6751 #x905c #x4ed6 #x591a #x592a
     #x6c70 #x8a51 #x553e #x5815 #x59a5 #x60f0 #x6253 #x67c1
     #x8235 #x6955 #x9640 #x99c4 #x9a28 #x4f53 #x5806 #x5bfe
     #x8010 #x5cb1 #x5e2f #x5f85 #x6020 #x614b #x6234 #x66ff
     #x6cf0 #x6ede #x80ce #x817f #x82d4 #x888b #x8cb8 #x9000
     #x902e #x968a #x9edb #x9bdb #x4ee3 #x53f0 #x5927 #x7b2c
     #x918d #x984c #x9df9 #x6edd #x7027 #x5353 #x5544 #x5b85
     #x6258 #x629e #x62d3 #x6ca2 #x6fef #x7422 #x8a17 #x9438
     #x6fc1 #x8afe #x8338 #x51e7 #x86f8 #x53ea 
     ;; #x43
     #x53e9 #x4f46 #x9054 #x8fb0 #x596a #x8131 #x5dfd #x7aea
     #x8fbf #x68da #x8c37 #x72f8 #x9c48 #x6a3d #x8ab0 #x4e39
     #x5358 #x5606 #x5766 #x62c5 #x63a2 #x65e6 #x6b4e #x6de1
     #x6e5b #x70ad #x77ed #x7aef #x7baa #x7dbb #x803d #x80c6
     #x86cb #x8a95 #x935b #x56e3 #x58c7 #x5f3e #x65ad #x6696
     #x6a80 #x6bb5 #x7537 #x8ac7 #x5024 #x77e5 #x5730 #x5f1b
     #x6065 #x667a #x6c60 #x75f4 #x7a1a #x7f6e #x81f4 #x8718
     #x9045 #x99b3 #x7bc9 #x755c #x7af9 #x7b51 #x84c4 #x9010
     #x79e9 #x7a92 #x8336 #x5ae1 #x7740 #x4e2d #x4ef2 #x5b99
     #x5fe0 #x62bd #x663c #x67f1 #x6ce8 #x866b #x8877 #x8a3b
     #x914e #x92f3 #x99d0 #x6a17 #x7026 #x732a #x82e7 #x8457
     #x8caf #x4e01 #x5146 #x51cb #x558b #x5bf5 
     ;; #x44
     #x5e16 #x5e33 #x5e81 #x5f14 #x5f35 #x5f6b #x5fb4 #x61f2
     #x6311 #x66a2 #x671d #x6f6e #x7252 #x753a #x773a #x8074
     #x8139 #x8178 #x8776 #x8abf #x8adc #x8d85 #x8df3 #x929a
     #x9577 #x9802 #x9ce5 #x52c5 #x6357 #x76f4 #x6715 #x6c88
     #x73cd #x8cc3 #x93ae #x9673 #x6d25 #x589c #x690e #x69cc
     #x8ffd #x939a #x75db #x901a #x585a #x6802 #x63b4 #x69fb
     #x4f43 #x6f2c #x67d8 #x8fbb #x8526 #x7db4 #x9354 #x693f
     #x6f70 #x576a #x58f7 #x5b2c #x7d2c #x722a #x540a #x91e3
     #x9db4 #x4ead #x4f4e #x505c #x5075 #x5243 #x8c9e #x5448
     #x5824 #x5b9a #x5e1d #x5e95 #x5ead #x5ef7 #x5f1f #x608c
     #x62b5 #x633a #x63d0 #x68af #x6c40 #x7887 #x798e #x7a0b
     #x7de0 #x8247 #x8a02 #x8ae6 #x8e44 #x9013 
     ;; #x45
     #x90b8 #x912d #x91d8 #x9f0e #x6ce5 #x6458 #x64e2 #x6575
     #x6ef4 #x7684 #x7b1b #x9069 #x93d1 #x6eba #x54f2 #x5fb9
     #x64a4 #x8f4d #x8fed #x9244 #x5178 #x586b #x5929 #x5c55
     #x5e97 #x6dfb #x7e8f #x751c #x8cbc #x8ee2 #x985b #x70b9
     #x4f1d #x6bbf #x6fb1 #x7530 #x96fb #x514e #x5410 #x5835
     #x5857 #x59ac #x5c60 #x5f92 #x6597 #x675c #x6e21 #x767b
     #x83df #x8ced #x9014 #x90fd #x934d #x7825 #x783a #x52aa
     #x5ea6 #x571f #x5974 #x6012 #x5012 #x515a #x51ac #x51cd
     #x5200 #x5510 #x5854 #x5858 #x5957 #x5b95 #x5cf6 #x5d8b
     #x60bc #x6295 #x642d #x6771 #x6843 #x68bc #x68df #x76d7
     #x6dd8 #x6e6f #x6d9b #x706f #x71c8 #x5f53 #x75d8 #x7977
     #x7b49 #x7b54 #x7b52 #x7cd6 #x7d71 #x5230 
     ;; #x46
     #x8463 #x8569 #x85e4 #x8a0e #x8b04 #x8c46 #x8e0f #x9003
     #x900f #x9419 #x9676 #x982d #x9a30 #x95d8 #x50cd #x52d5
     #x540c #x5802 #x5c0e #x61a7 #x649e #x6d1e #x77b3 #x7ae5
     #x80f4 #x8404 #x9053 #x9285 #x5ce0 #x9d07 #x533f #x5f97
     #x5fb3 #x6d9c #x7279 #x7763 #x79bf #x7be4 #x6bd2 #x72ec
     #x8aad #x6803 #x6a61 #x51f8 #x7a81 #x6934 #x5c4a #x9cf6
     #x82eb #x5bc5 #x9149 #x701e #x5678 #x5c6f #x60c7 #x6566
     #x6c8c #x8c5a #x9041 #x9813 #x5451 #x66c7 #x920d #x5948
     #x90a3 #x5185 #x4e4d #x51ea #x8599 #x8b0e #x7058 #x637a
     #x934b #x6962 #x99b4 #x7e04 #x7577 #x5357 #x6960 #x8edf
     #x96e3 #x6c5d #x4e8c #x5c3c #x5f10 #x8fe9 #x5302 #x8cd1
     #x8089 #x8679 #x5eff #x65e5 #x4e73 #x5165 
     ;; #x47
     #x5982 #x5c3f #x97ee #x4efb #x598a #x5fcd #x8a8d #x6fe1
     #x79b0 #x7962 #x5be7 #x8471 #x732b #x71b1 #x5e74 #x5ff5
     #x637b #x649a #x71c3 #x7c98 #x4e43 #x5efc #x4e4b #x57dc
     #x56a2 #x60a9 #x6fc3 #x7d0d #x80fd #x8133 #x81bf #x8fb2
     #x8997 #x86a4 #x5df4 #x628a #x64ad #x8987 #x6777 #x6ce2
     #x6d3e #x7436 #x7834 #x5a46 #x7f75 #x82ad #x99ac #x4ff3
     #x5ec3 #x62dd #x6392 #x6557 #x676f #x76c3 #x724c #x80cc
     #x80ba #x8f29 #x914d #x500d #x57f9 #x5a92 #x6885 #x6973
     #x7164 #x72fd #x8cb7 #x58f2 #x8ce0 #x966a #x9019 #x877f
     #x79e4 #x77e7 #x8429 #x4f2f #x5265 #x535a #x62cd #x67cf
     #x6cca #x767d #x7b94 #x7c95 #x8236 #x8584 #x8feb #x66dd
     #x6f20 #x7206 #x7e1b #x83ab #x99c1 #x9ea6 
     ;; #x48
     #x51fd #x7bb1 #x7872 #x7bb8 #x8087 #x7b48 #x6ae8 #x5e61
     #x808c #x7551 #x7560 #x516b #x9262 #x6e8c #x767a #x9197
     #x9aea #x4f10 #x7f70 #x629c #x7b4f #x95a5 #x9ce9 #x567a
     #x5859 #x86e4 #x96bc #x4f34 #x5224 #x534a #x53cd #x53db
     #x5e06 #x642c #x6591 #x677f #x6c3e #x6c4e #x7248 #x72af
     #x73ed #x7554 #x7e41 #x822c #x85e9 #x8ca9 #x7bc4 #x91c6
     #x7169 #x9812 #x98ef #x633d #x6669 #x756a #x76e4 #x78d0
     #x8543 #x86ee #x532a #x5351 #x5426 #x5983 #x5e87 #x5f7c
     #x60b2 #x6249 #x6279 #x62ab #x6590 #x6bd4 #x6ccc #x75b2
     #x76ae #x7891 #x79d8 #x7dcb #x7f77 #x80a5 #x88ab #x8ab9
     #x8cbb #x907f #x975e #x98db #x6a0b #x7c38 #x5099 #x5c3e
     #x5fae #x6787 #x6bd8 #x7435 #x7709 #x7f8e 
     ;; #x49
     #x9f3b #x67ca #x7a17 #x5339 #x758b #x9aed #x5f66 #x819d
     #x83f1 #x8098 #x5f3c #x5fc5 #x7562 #x7b46 #x903c #x6867
     #x59eb #x5a9b #x7d10 #x767e #x8b2c #x4ff5 #x5f6a #x6a19
     #x6c37 #x6f02 #x74e2 #x7968 #x8868 #x8a55 #x8c79 #x5edf
     #x63cf #x75c5 #x79d2 #x82d7 #x9328 #x92f2 #x849c #x86ed
     #x9c2d #x54c1 #x5f6c #x658c #x6d5c #x7015 #x8ca7 #x8cd3
     #x983b #x654f #x74f6 #x4e0d #x4ed8 #x57e0 #x592b #x5a66
     #x5bcc #x51a8 #x5e03 #x5e9c #x6016 #x6276 #x6577 #x65a7
     #x666e #x6d6e #x7236 #x7b26 #x8150 #x819a #x8299 #x8b5c
     #x8ca0 #x8ce6 #x8d74 #x961c #x9644 #x4fae #x64ab #x6b66
     #x821e #x8461 #x856a #x90e8 #x5c01 #x6953 #x98a8 #x847a
     #x8557 #x4f0f #x526f #x5fa9 #x5e45 #x670d 
     ;; #x4a
     #x798f #x8179 #x8907 #x8986 #x6df5 #x5f17 #x6255 #x6cb8
     #x4ecf #x7269 #x9b92 #x5206 #x543b #x5674 #x58b3 #x61a4
     #x626e #x711a #x596e #x7c89 #x7cde #x7d1b #x96f0 #x6587
     #x805e #x4e19 #x4f75 #x5175 #x5840 #x5e63 #x5e73 #x5f0a
     #x67c4 #x4e26 #x853d #x9589 #x965b #x7c73 #x9801 #x50fb
     #x58c1 #x7656 #x78a7 #x5225 #x77a5 #x8511 #x7b86 #x504f
     #x5909 #x7247 #x7bc7 #x7de8 #x8fba #x8fd4 #x904d #x4fbf
     #x52c9 #x5a29 #x5f01 #x97ad #x4fdd #x8217 #x92ea #x5703
     #x6355 #x6b69 #x752b #x88dc #x8f14 #x7a42 #x52df #x5893
     #x6155 #x620a #x66ae #x6bcd #x7c3f #x83e9 #x5023 #x4ff8
     #x5305 #x5446 #x5831 #x5949 #x5b9d #x5cf0 #x5cef #x5d29
     #x5e96 #x62b1 #x6367 #x653e #x65b9 #x670b 
     ;; #x4b
     #x6cd5 #x6ce1 #x70f9 #x7832 #x7e2b #x80de #x82b3 #x840c
     #x84ec #x8702 #x8912 #x8a2a #x8c4a #x90a6 #x92d2 #x98fd
     #x9cf3 #x9d6c #x4e4f #x4ea1 #x508d #x5256 #x574a #x59a8
     #x5e3d #x5fd8 #x5fd9 #x623f #x66b4 #x671b #x67d0 #x68d2
     #x5192 #x7d21 #x80aa #x81a8 #x8b00 #x8c8c #x8cbf #x927e
     #x9632 #x5420 #x982c #x5317 #x50d5 #x535c #x58a8 #x64b2
     #x6734 #x7267 #x7766 #x7a46 #x91e6 #x52c3 #x6ca1 #x6b86
     #x5800 #x5e4c #x5954 #x672c #x7ffb #x51e1 #x76c6 #x6469
     #x78e8 #x9b54 #x9ebb #x57cb #x59b9 #x6627 #x679a #x6bce
     #x54e9 #x69d9 #x5e55 #x819c #x6795 #x9baa #x67fe #x9c52
     #x685d #x4ea6 #x4fe3 #x53c8 #x62b9 #x672b #x6cab #x8fc4
     #x4fad #x7e6d #x9ebf #x4e07 #x6162 #x6e80 
     ;; #x4c
     #x6f2b #x8513 #x5473 #x672a #x9b45 #x5df3 #x7b95 #x5cac
     #x5bc6 #x871c #x6e4a #x84d1 #x7a14 #x8108 #x5999 #x7c8d
     #x6c11 #x7720 #x52d9 #x5922 #x7121 #x725f #x77db #x9727
     #x9d61 #x690b #x5a7f #x5a18 #x51a5 #x540d #x547d #x660e
     #x76df #x8ff7 #x9298 #x9cf4 #x59ea #x725d #x6ec5 #x514d
     #x68c9 #x7dbf #x7dec #x9762 #x9eba #x6478 #x6a21 #x8302
     #x5984 #x5b5f #x6bdb #x731b #x76f2 #x7db2 #x8017 #x8499
     #x5132 #x6728 #x9ed9 #x76ee #x6762 #x52ff #x9905 #x5c24
     #x623b #x7c7e #x8cb0 #x554f #x60b6 #x7d0b #x9580 #x5301
     #x4e5f #x51b6 #x591c #x723a #x8036 #x91ce #x5f25 #x77e2
     #x5384 #x5f79 #x7d04 #x85ac #x8a33 #x8e8d #x9756 #x67f3
     #x85ae #x9453 #x6109 #x6108 #x6cb9 #x7652 
     ;; #x4d
     #x8aed #x8f38 #x552f #x4f51 #x512a #x52c7 #x53cb #x5ba5
     #x5e7d #x60a0 #x6182 #x63d6 #x6709 #x67da #x6e67 #x6d8c
     #x7336 #x7337 #x7531 #x7950 #x88d5 #x8a98 #x904a #x9091
     #x90f5 #x96c4 #x878d #x5915 #x4e88 #x4f59 #x4e0e #x8a89
     #x8f3f #x9810 #x50ad #x5e7c #x5996 #x5bb9 #x5eb8 #x63da
     #x63fa #x64c1 #x66dc #x694a #x69d8 #x6d0b #x6eb6 #x7194
     #x7528 #x7aaf #x7f8a #x8000 #x8449 #x84c9 #x8981 #x8b21
     #x8e0a #x9065 #x967d #x990a #x617e #x6291 #x6b32 #x6c83
     #x6d74 #x7fcc #x7ffc #x6dc0 #x7f85 #x87ba #x88f8 #x6765
     #x83b1 #x983c #x96f7 #x6d1b #x7d61 #x843d #x916a #x4e71
     #x5375 #x5d50 #x6b04 #x6feb #x85cd #x862d #x89a7 #x5229
     #x540f #x5c65 #x674e #x68a8 #x7406 #x7483 
     ;; #x4e
     #x75e2 #x88cf #x88e1 #x91cc #x96e2 #x9678 #x5f8b #x7387
     #x7acb #x844e #x63a0 #x7565 #x5289 #x6d41 #x6e9c #x7409
     #x7559 #x786b #x7c92 #x9686 #x7adc #x9f8d #x4fb6 #x616e
     #x65c5 #x865c #x4e86 #x4eae #x50da #x4e21 #x51cc #x5bee
     #x6599 #x6881 #x6dbc #x731f #x7642 #x77ad #x7a1c #x7ce7
     #x826f #x8ad2 #x907c #x91cf #x9675 #x9818 #x529b #x7dd1
     #x502b #x5398 #x6797 #x6dcb #x71d0 #x7433 #x81e8 #x8f2a
     #x96a3 #x9c57 #x9e9f #x7460 #x5841 #x6d99 #x7d2f #x985e
     #x4ee4 #x4f36 #x4f8b #x51b7 #x52b1 #x5dba #x601c #x73b2
     #x793c #x82d3 #x9234 #x96b7 #x96f6 #x970a #x9e97 #x9f62
     #x66a6 #x6b74 #x5217 #x52a3 #x70c8 #x88c2 #x5ec9 #x604b
     #x6190 #x6f23 #x7149 #x7c3e #x7df4 #x806f 
     ;; #x4f
     #x84ee #x9023 #x932c #x5442 #x9b6f #x6ad3 #x7089 #x8cc2
     #x8def #x9732 #x52b4 #x5a41 #x5eca #x5f04 #x6717 #x697c
     #x6994 #x6d6a #x6f0f #x7262 #x72fc #x7bed #x8001 #x807e
     #x874b #x90ce #x516d #x9e93 #x7984 #x808b #x9332 #x8ad6
     #x502d #x548c #x8a71 #x6b6a #x8cc4 #x8107 #x60d1 #x67a0
     #x9df2 #x4e99 #x4e98 #x9c10 #x8a6b #x85c1 #x8568 #x6900
     #x6e7e #x7897 #x8155 #xfffd #xfffd #xfffd #xfffd #xfffd
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
     #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd 
     ;; #x50
     #x5f0c #x4e10 #x4e15 #x4e2a #x4e31 #x4e36 #x4e3c #x4e3f
     #x4e42 #x4e56 #x4e58 #x4e82 #x4e85 #x8c6b #x4e8a #x8212
     #x5f0d #x4e8e #x4e9e #x4e9f #x4ea0 #x4ea2 #x4eb0 #x4eb3
     #x4eb6 #x4ece #x4ecd #x4ec4 #x4ec6 #x4ec2 #x4ed7 #x4ede
     #x4eed #x4edf #x4ef7 #x4f09 #x4f5a #x4f30 #x4f5b #x4f5d
     #x4f57 #x4f47 #x4f76 #x4f88 #x4f8f #x4f98 #x4f7b #x4f69
     #x4f70 #x4f91 #x4f6f #x4f86 #x4f96 #x5118 #x4fd4 #x4fdf
     #x4fce #x4fd8 #x4fdb #x4fd1 #x4fda #x4fd0 #x4fe4 #x4fe5
     #x501a #x5028 #x5014 #x502a #x5025 #x5005 #x4f1c #x4ff6
     #x5021 #x5029 #x502c #x4ffe #x4fef #x5011 #x5006 #x5043
     #x5047 #x6703 #x5055 #x5050 #x5048 #x505a #x5056 #x506c
     #x5078 #x5080 #x509a #x5085 #x50b4 #x50b2 
     ;; #x51
     #x50c9 #x50ca #x50b3 #x50c2 #x50d6 #x50de #x50e5 #x50ed
     #x50e3 #x50ee #x50f9 #x50f5 #x5109 #x5101 #x5102 #x5116
     #x5115 #x5114 #x511a #x5121 #x513a #x5137 #x513c #x513b
     #x513f #x5140 #x5152 #x514c #x5154 #x5162 #x7af8 #x5169
     #x516a #x516e #x5180 #x5182 #x56d8 #x518c #x5189 #x518f
     #x5191 #x5193 #x5195 #x5196 #x51a4 #x51a6 #x51a2 #x51a9
     #x51aa #x51ab #x51b3 #x51b1 #x51b2 #x51b0 #x51b5 #x51bd
     #x51c5 #x51c9 #x51db #x51e0 #x8655 #x51e9 #x51ed #x51f0
     #x51f5 #x51fe #x5204 #x520b #x5214 #x520e #x5227 #x522a
     #x522e #x5233 #x5239 #x524f #x5244 #x524b #x524c #x525e
     #x5254 #x526a #x5274 #x5269 #x5273 #x527f #x527d #x528d
     #x5294 #x5292 #x5271 #x5288 #x5291 #x8fa8 
     ;; #x52
     #x8fa7 #x52ac #x52ad #x52bc #x52b5 #x52c1 #x52cd #x52d7
     #x52de #x52e3 #x52e6 #x98ed #x52e0 #x52f3 #x52f5 #x52f8
     #x52f9 #x5306 #x5308 #x7538 #x530d #x5310 #x530f #x5315
     #x531a #x5323 #x532f #x5331 #x5333 #x5338 #x5340 #x5346
     #x5345 #x4e17 #x5349 #x534d #x51d6 #x535e #x5369 #x536e
     #x5918 #x537b #x5377 #x5382 #x5396 #x53a0 #x53a6 #x53a5
     #x53ae #x53b0 #x53b6 #x53c3 #x7c12 #x96d9 #x53df #x66fc
     #x71ee #x53ee #x53e8 #x53ed #x53fa #x5401 #x543d #x5440
     #x542c #x542d #x543c #x542e #x5436 #x5429 #x541d #x544e
     #x548f #x5475 #x548e #x545f #x5471 #x5477 #x5470 #x5492
     #x547b #x5480 #x5476 #x5484 #x5490 #x5486 #x54c7 #x54a2
     #x54b8 #x54a5 #x54ac #x54c4 #x54c8 #x54a8 
     ;; #x53
     #x54ab #x54c2 #x54a4 #x54be #x54bc #x54d8 #x54e5 #x54e6
     #x550f #x5514 #x54fd #x54ee #x54ed #x54fa #x54e2 #x5539
     #x5540 #x5563 #x554c #x552e #x555c #x5545 #x5556 #x5557
     #x5538 #x5533 #x555d #x5599 #x5580 #x54af #x558a #x559f
     #x557b #x557e #x5598 #x559e #x55ae #x557c #x5583 #x55a9
     #x5587 #x55a8 #x55da #x55c5 #x55df #x55c4 #x55dc #x55e4
     #x55d4 #x5614 #x55f7 #x5616 #x55fe #x55fd #x561b #x55f9
     #x564e #x5650 #x71df #x5634 #x5636 #x5632 #x5638 #x566b
     #x5664 #x562f #x566c #x566a #x5686 #x5680 #x568a #x56a0
     #x5694 #x568f #x56a5 #x56ae #x56b6 #x56b4 #x56c2 #x56bc
     #x56c1 #x56c3 #x56c0 #x56c8 #x56ce #x56d1 #x56d3 #x56d7
     #x56ee #x56f9 #x5700 #x56ff #x5704 #x5709 
     ;; #x54
     #x5708 #x570b #x570d #x5713 #x5718 #x5716 #x55c7 #x571c
     #x5726 #x5737 #x5738 #x574e #x573b #x5740 #x574f #x5769
     #x57c0 #x5788 #x5761 #x577f #x5789 #x5793 #x57a0 #x57b3
     #x57a4 #x57aa #x57b0 #x57c3 #x57c6 #x57d4 #x57d2 #x57d3
     #x580a #x57d6 #x57e3 #x580b #x5819 #x581d #x5872 #x5821
     #x5862 #x584b #x5870 #x6bc0 #x5852 #x583d #x5879 #x5885
     #x58b9 #x589f #x58ab #x58ba #x58de #x58bb #x58b8 #x58ae
     #x58c5 #x58d3 #x58d1 #x58d7 #x58d9 #x58d8 #x58e5 #x58dc
     #x58e4 #x58df #x58ef #x58fa #x58f9 #x58fb #x58fc #x58fd
     #x5902 #x590a #x5910 #x591b #x68a6 #x5925 #x592c #x592d
     #x5932 #x5938 #x593e #x7ad2 #x5955 #x5950 #x594e #x595a
     #x5958 #x5962 #x5960 #x5967 #x596c #x5969 
     ;; #x55
     #x5978 #x5981 #x599d #x4f5e #x4fab #x59a3 #x59b2 #x59c6
     #x59e8 #x59dc #x598d #x59d9 #x59da #x5a25 #x5a1f #x5a11
     #x5a1c #x5a09 #x5a1a #x5a40 #x5a6c #x5a49 #x5a35 #x5a36
     #x5a62 #x5a6a #x5a9a #x5abc #x5abe #x5acb #x5ac2 #x5abd
     #x5ae3 #x5ad7 #x5ae6 #x5ae9 #x5ad6 #x5afa #x5afb #x5b0c
     #x5b0b #x5b16 #x5b32 #x5ad0 #x5b2a #x5b36 #x5b3e #x5b43
     #x5b45 #x5b40 #x5b51 #x5b55 #x5b5a #x5b5b #x5b65 #x5b69
     #x5b70 #x5b73 #x5b75 #x5b78 #x6588 #x5b7a #x5b80 #x5b83
     #x5ba6 #x5bb8 #x5bc3 #x5bc7 #x5bc9 #x5bd4 #x5bd0 #x5be4
     #x5be6 #x5be2 #x5bde #x5be5 #x5beb #x5bf0 #x5bf6 #x5bf3
     #x5c05 #x5c07 #x5c08 #x5c0d #x5c13 #x5c20 #x5c22 #x5c28
     #x5c38 #x5c39 #x5c41 #x5c46 #x5c4e #x5c53 
     ;; #x56
     #x5c50 #x5c4f #x5b71 #x5c6c #x5c6e #x4e62 #x5c76 #x5c79
     #x5c8c #x5c91 #x5c94 #x599b #x5cab #x5cbb #x5cb6 #x5cbc
     #x5cb7 #x5cc5 #x5cbe #x5cc7 #x5cd9 #x5ce9 #x5cfd #x5cfa
     #x5ced #x5d8c #x5cea #x5d0b #x5d15 #x5d17 #x5d5c #x5d1f
     #x5d1b #x5d11 #x5d14 #x5d22 #x5d1a #x5d19 #x5d18 #x5d4c
     #x5d52 #x5d4e #x5d4b #x5d6c #x5d73 #x5d76 #x5d87 #x5d84
     #x5d82 #x5da2 #x5d9d #x5dac #x5dae #x5dbd #x5d90 #x5db7
     #x5dbc #x5dc9 #x5dcd #x5dd3 #x5dd2 #x5dd6 #x5ddb #x5deb
     #x5df2 #x5df5 #x5e0b #x5e1a #x5e19 #x5e11 #x5e1b #x5e36
     #x5e37 #x5e44 #x5e43 #x5e40 #x5e4e #x5e57 #x5e54 #x5e5f
     #x5e62 #x5e64 #x5e47 #x5e75 #x5e76 #x5e7a #x9ebc #x5e7f
     #x5ea0 #x5ec1 #x5ec2 #x5ec8 #x5ed0 #x5ecf 
     ;; #x57
     #x5ed6 #x5ee3 #x5edd #x5eda #x5edb #x5ee2 #x5ee1 #x5ee8
     #x5ee9 #x5eec #x5ef1 #x5ef3 #x5ef0 #x5ef4 #x5ef8 #x5efe
     #x5f03 #x5f09 #x5f5d #x5f5c #x5f0b #x5f11 #x5f16 #x5f29
     #x5f2d #x5f38 #x5f41 #x5f48 #x5f4c #x5f4e #x5f2f #x5f51
     #x5f56 #x5f57 #x5f59 #x5f61 #x5f6d #x5f73 #x5f77 #x5f83
     #x5f82 #x5f7f #x5f8a #x5f88 #x5f91 #x5f87 #x5f9e #x5f99
     #x5f98 #x5fa0 #x5fa8 #x5fad #x5fbc #x5fd6 #x5ffb #x5fe4
     #x5ff8 #x5ff1 #x5fdd #x60b3 #x5fff #x6021 #x6060 #x6019
     #x6010 #x6029 #x600e #x6031 #x601b #x6015 #x602b #x6026
     #x600f #x603a #x605a #x6041 #x606a #x6077 #x605f #x604a
     #x6046 #x604d #x6063 #x6043 #x6064 #x6042 #x606c #x606b
     #x6059 #x6081 #x608d #x60e7 #x6083 #x609a 
     ;; #x58
     #x6084 #x609b #x6096 #x6097 #x6092 #x60a7 #x608b #x60e1
     #x60b8 #x60e0 #x60d3 #x60b4 #x5ff0 #x60bd #x60c6 #x60b5
     #x60d8 #x614d #x6115 #x6106 #x60f6 #x60f7 #x6100 #x60f4
     #x60fa #x6103 #x6121 #x60fb #x60f1 #x610d #x610e #x6147
     #x613e #x6128 #x6127 #x614a #x613f #x613c #x612c #x6134
     #x613d #x6142 #x6144 #x6173 #x6177 #x6158 #x6159 #x615a
     #x616b #x6174 #x616f #x6165 #x6171 #x615f #x615d #x6153
     #x6175 #x6199 #x6196 #x6187 #x61ac #x6194 #x619a #x618a
     #x6191 #x61ab #x61ae #x61cc #x61ca #x61c9 #x61f7 #x61c8
     #x61c3 #x61c6 #x61ba #x61cb #x7f79 #x61cd #x61e6 #x61e3
     #x61f6 #x61fa #x61f4 #x61ff #x61fd #x61fc #x61fe #x6200
     #x6208 #x6209 #x620d #x620c #x6214 #x621b 
     ;; #x59
     #x621e #x6221 #x622a #x622e #x6230 #x6232 #x6233 #x6241
     #x624e #x625e #x6263 #x625b #x6260 #x6268 #x627c #x6282
     #x6289 #x627e #x6292 #x6293 #x6296 #x62d4 #x6283 #x6294
     #x62d7 #x62d1 #x62bb #x62cf #x62ff #x62c6 #x64d4 #x62c8
     #x62dc #x62cc #x62ca #x62c2 #x62c7 #x629b #x62c9 #x630c
     #x62ee #x62f1 #x6327 #x6302 #x6308 #x62ef #x62f5 #x6350
     #x633e #x634d #x641c #x634f #x6396 #x638e #x6380 #x63ab
     #x6376 #x63a3 #x638f #x6389 #x639f #x63b5 #x636b #x6369
     #x63be #x63e9 #x63c0 #x63c6 #x63e3 #x63c9 #x63d2 #x63f6
     #x63c4 #x6416 #x6434 #x6406 #x6413 #x6426 #x6436 #x651d
     #x6417 #x6428 #x640f #x6467 #x646f #x6476 #x644e #x652a
     #x6495 #x6493 #x64a5 #x64a9 #x6488 #x64bc 
     ;; #x5a
     #x64da #x64d2 #x64c5 #x64c7 #x64bb #x64d8 #x64c2 #x64f1
     #x64e7 #x8209 #x64e0 #x64e1 #x62ac #x64e3 #x64ef #x652c
     #x64f6 #x64f4 #x64f2 #x64fa #x6500 #x64fd #x6518 #x651c
     #x6505 #x6524 #x6523 #x652b #x6534 #x6535 #x6537 #x6536
     #x6538 #x754b #x6548 #x6556 #x6555 #x654d #x6558 #x655e
     #x655d #x6572 #x6578 #x6582 #x6583 #x8b8a #x659b #x659f
     #x65ab #x65b7 #x65c3 #x65c6 #x65c1 #x65c4 #x65cc #x65d2
     #x65db #x65d9 #x65e0 #x65e1 #x65f1 #x6772 #x660a #x6603
     #x65fb #x6773 #x6635 #x6636 #x6634 #x661c #x664f #x6644
     #x6649 #x6641 #x665e #x665d #x6664 #x6667 #x6668 #x665f
     #x6662 #x6670 #x6683 #x6688 #x668e #x6689 #x6684 #x6698
     #x669d #x66c1 #x66b9 #x66c9 #x66be #x66bc 
     ;; #x5b
     #x66c4 #x66b8 #x66d6 #x66da #x66e0 #x663f #x66e6 #x66e9
     #x66f0 #x66f5 #x66f7 #x670f #x6716 #x671e #x6726 #x6727
     #x9738 #x672e #x673f #x6736 #x6741 #x6738 #x6737 #x6746
     #x675e #x6760 #x6759 #x6763 #x6764 #x6789 #x6770 #x67a9
     #x677c #x676a #x678c #x678b #x67a6 #x67a1 #x6785 #x67b7
     #x67ef #x67b4 #x67ec #x67b3 #x67e9 #x67b8 #x67e4 #x67de
     #x67dd #x67e2 #x67ee #x67b9 #x67ce #x67c6 #x67e7 #x6a9c
     #x681e #x6846 #x6829 #x6840 #x684d #x6832 #x684e #x68b3
     #x682b #x6859 #x6863 #x6877 #x687f #x689f #x688f #x68ad
     #x6894 #x689d #x689b #x6883 #x6aae #x68b9 #x6874 #x68b5
     #x68a0 #x68ba #x690f #x688d #x687e #x6901 #x68ca #x6908
     #x68d8 #x6922 #x6926 #x68e1 #x690c #x68cd 
     ;; #x5c
     #x68d4 #x68e7 #x68d5 #x6936 #x6912 #x6904 #x68d7 #x68e3
     #x6925 #x68f9 #x68e0 #x68ef #x6928 #x692a #x691a #x6923
     #x6921 #x68c6 #x6979 #x6977 #x695c #x6978 #x696b #x6954
     #x697e #x696e #x6939 #x6974 #x693d #x6959 #x6930 #x6961
     #x695e #x695d #x6981 #x696a #x69b2 #x69ae #x69d0 #x69bf
     #x69c1 #x69d3 #x69be #x69ce #x5be8 #x69ca #x69dd #x69bb
     #x69c3 #x69a7 #x6a2e #x6991 #x69a0 #x699c #x6995 #x69b4
     #x69de #x69e8 #x6a02 #x6a1b #x69ff #x6b0a #x69f9 #x69f2
     #x69e7 #x6a05 #x69b1 #x6a1e #x69ed #x6a14 #x69eb #x6a0a
     #x6a12 #x6ac1 #x6a23 #x6a13 #x6a44 #x6a0c #x6a72 #x6a36
     #x6a78 #x6a47 #x6a62 #x6a59 #x6a66 #x6a48 #x6a38 #x6a22
     #x6a90 #x6a8d #x6aa0 #x6a84 #x6aa2 #x6aa3 
     ;; #x5d
     #x6a97 #x8617 #x6abb #x6ac3 #x6ac2 #x6ab8 #x6ab3 #x6aac
     #x6ade #x6ad1 #x6adf #x6aaa #x6ada #x6aea #x6afb #x6b05
     #x8616 #x6afa #x6b12 #x6b16 #x9b31 #x6b1f #x6b38 #x6b37
     #x76dc #x6b39 #x98ee #x6b47 #x6b43 #x6b49 #x6b50 #x6b59
     #x6b54 #x6b5b #x6b5f #x6b61 #x6b78 #x6b79 #x6b7f #x6b80
     #x6b84 #x6b83 #x6b8d #x6b98 #x6b95 #x6b9e #x6ba4 #x6baa
     #x6bab #x6baf #x6bb2 #x6bb1 #x6bb3 #x6bb7 #x6bbc #x6bc6
     #x6bcb #x6bd3 #x6bdf #x6bec #x6beb #x6bf3 #x6bef #x9ebe
     #x6c08 #x6c13 #x6c14 #x6c1b #x6c24 #x6c23 #x6c5e #x6c55
     #x6c62 #x6c6a #x6c82 #x6c8d #x6c9a #x6c81 #x6c9b #x6c7e
     #x6c68 #x6c73 #x6c92 #x6c90 #x6cc4 #x6cf1 #x6cd3 #x6cbd
     #x6cd7 #x6cc5 #x6cdd #x6cae #x6cb1 #x6cbe 
     ;; #x5e
     #x6cba #x6cdb #x6cef #x6cd9 #x6cea #x6d1f #x884d #x6d36
     #x6d2b #x6d3d #x6d38 #x6d19 #x6d35 #x6d33 #x6d12 #x6d0c
     #x6d63 #x6d93 #x6d64 #x6d5a #x6d79 #x6d59 #x6d8e #x6d95
     #x6fe4 #x6d85 #x6df9 #x6e15 #x6e0a #x6db5 #x6dc7 #x6de6
     #x6db8 #x6dc6 #x6dec #x6dde #x6dcc #x6de8 #x6dd2 #x6dc5
     #x6dfa #x6dd9 #x6de4 #x6dd5 #x6dea #x6dee #x6e2d #x6e6e
     #x6e2e #x6e19 #x6e72 #x6e5f #x6e3e #x6e23 #x6e6b #x6e2b
     #x6e76 #x6e4d #x6e1f #x6e43 #x6e3a #x6e4e #x6e24 #x6eff
     #x6e1d #x6e38 #x6e82 #x6eaa #x6e98 #x6ec9 #x6eb7 #x6ed3
     #x6ebd #x6eaf #x6ec4 #x6eb2 #x6ed4 #x6ed5 #x6e8f #x6ea5
     #x6ec2 #x6e9f #x6f41 #x6f11 #x704c #x6eec #x6ef8 #x6efe
     #x6f3f #x6ef2 #x6f31 #x6eef #x6f32 #x6ecc 
     ;; #x5f
     #x6f3e #x6f13 #x6ef7 #x6f86 #x6f7a #x6f78 #x6f81 #x6f80
     #x6f6f #x6f5b #x6ff3 #x6f6d #x6f82 #x6f7c #x6f58 #x6f8e
     #x6f91 #x6fc2 #x6f66 #x6fb3 #x6fa3 #x6fa1 #x6fa4 #x6fb9
     #x6fc6 #x6faa #x6fdf #x6fd5 #x6fec #x6fd4 #x6fd8 #x6ff1
     #x6fee #x6fdb #x7009 #x700b #x6ffa #x7011 #x7001 #x700f
     #x6ffe #x701b #x701a #x6f74 #x701d #x7018 #x701f #x7030
     #x703e #x7032 #x7051 #x7063 #x7099 #x7092 #x70af #x70f1
     #x70ac #x70b8 #x70b3 #x70ae #x70df #x70cb #x70dd #x70d9
     #x7109 #x70fd #x711c #x7119 #x7165 #x7155 #x7188 #x7166
     #x7162 #x714c #x7156 #x716c #x718f #x71fb #x7184 #x7195
     #x71a8 #x71ac #x71d7 #x71b9 #x71be #x71d2 #x71c9 #x71d4
     #x71ce #x71e0 #x71ec #x71e7 #x71f5 #x71fc 
     ;; #x60
     #x71f9 #x71ff #x720d #x7210 #x721b #x7228 #x722d #x722c
     #x7230 #x7232 #x723b #x723c #x723f #x7240 #x7246 #x724b
     #x7258 #x7274 #x727e #x7282 #x7281 #x7287 #x7292 #x7296
     #x72a2 #x72a7 #x72b9 #x72b2 #x72c3 #x72c6 #x72c4 #x72ce
     #x72d2 #x72e2 #x72e0 #x72e1 #x72f9 #x72f7 #x500f #x7317
     #x730a #x731c #x7316 #x731d #x7334 #x732f #x7329 #x7325
     #x733e #x734e #x734f #x9ed8 #x7357 #x736a #x7368 #x7370
     #x7378 #x7375 #x737b #x737a #x73c8 #x73b3 #x73ce #x73bb
     #x73c0 #x73e5 #x73ee #x73de #x74a2 #x7405 #x746f #x7425
     #x73f8 #x7432 #x743a #x7455 #x743f #x745f #x7459 #x7441
     #x745c #x7469 #x7470 #x7463 #x746a #x7476 #x747e #x748b
     #x749e #x74a7 #x74ca #x74cf #x74d4 #x73f1 
     ;; #x61
     #x74e0 #x74e3 #x74e7 #x74e9 #x74ee #x74f2 #x74f0 #x74f1
     #x74f8 #x74f7 #x7504 #x7503 #x7505 #x750c #x750e #x750d
     #x7515 #x7513 #x751e #x7526 #x752c #x753c #x7544 #x754d
     #x754a #x7549 #x755b #x7546 #x755a #x7569 #x7564 #x7567
     #x756b #x756d #x7578 #x7576 #x7586 #x7587 #x7574 #x758a
     #x7589 #x7582 #x7594 #x759a #x759d #x75a5 #x75a3 #x75c2
     #x75b3 #x75c3 #x75b5 #x75bd #x75b8 #x75bc #x75b1 #x75cd
     #x75ca #x75d2 #x75d9 #x75e3 #x75de #x75fe #x75ff #x75fc
     #x7601 #x75f0 #x75fa #x75f2 #x75f3 #x760b #x760d #x7609
     #x761f #x7627 #x7620 #x7621 #x7622 #x7624 #x7634 #x7630
     #x763b #x7647 #x7648 #x7646 #x765c #x7658 #x7661 #x7662
     #x7668 #x7669 #x766a #x7667 #x766c #x7670 
     ;; #x62
     #x7672 #x7676 #x7678 #x767c #x7680 #x7683 #x7688 #x768b
     #x768e #x7696 #x7693 #x7699 #x769a #x76b0 #x76b4 #x76b8
     #x76b9 #x76ba #x76c2 #x76cd #x76d6 #x76d2 #x76de #x76e1
     #x76e5 #x76e7 #x76ea #x862f #x76fb #x7708 #x7707 #x7704
     #x7729 #x7724 #x771e #x7725 #x7726 #x771b #x7737 #x7738
     #x7747 #x775a #x7768 #x776b #x775b #x7765 #x777f #x777e
     #x7779 #x778e #x778b #x7791 #x77a0 #x779e #x77b0 #x77b6
     #x77b9 #x77bf #x77bc #x77bd #x77bb #x77c7 #x77cd #x77d7
     #x77da #x77dc #x77e3 #x77ee #x77fc #x780c #x7812 #x7926
     #x7820 #x792a #x7845 #x788e #x7874 #x7886 #x787c #x789a
     #x788c #x78a3 #x78b5 #x78aa #x78af #x78d1 #x78c6 #x78cb
     #x78d4 #x78be #x78bc #x78c5 #x78ca #x78ec 
     ;; #x63
     #x78e7 #x78da #x78fd #x78f4 #x7907 #x7912 #x7911 #x7919
     #x792c #x792b #x7940 #x7960 #x7957 #x795f #x795a #x7955
     #x7953 #x797a #x797f #x798a #x799d #x79a7 #x9f4b #x79aa
     #x79ae #x79b3 #x79b9 #x79ba #x79c9 #x79d5 #x79e7 #x79ec
     #x79e1 #x79e3 #x7a08 #x7a0d #x7a18 #x7a19 #x7a20 #x7a1f
     #x7980 #x7a31 #x7a3b #x7a3e #x7a37 #x7a43 #x7a57 #x7a49
     #x7a61 #x7a62 #x7a69 #x9f9d #x7a70 #x7a79 #x7a7d #x7a88
     #x7a97 #x7a95 #x7a98 #x7a96 #x7aa9 #x7ac8 #x7ab0 #x7ab6
     #x7ac5 #x7ac4 #x7abf #x9083 #x7ac7 #x7aca #x7acd #x7acf
     #x7ad5 #x7ad3 #x7ad9 #x7ada #x7add #x7ae1 #x7ae2 #x7ae6
     #x7aed #x7af0 #x7b02 #x7b0f #x7b0a #x7b06 #x7b33 #x7b18
     #x7b19 #x7b1e #x7b35 #x7b28 #x7b36 #x7b50 
     ;; #x64
     #x7b7a #x7b04 #x7b4d #x7b0b #x7b4c #x7b45 #x7b75 #x7b65
     #x7b74 #x7b67 #x7b70 #x7b71 #x7b6c #x7b6e #x7b9d #x7b98
     #x7b9f #x7b8d #x7b9c #x7b9a #x7b8b #x7b92 #x7b8f #x7b5d
     #x7b99 #x7bcb #x7bc1 #x7bcc #x7bcf #x7bb4 #x7bc6 #x7bdd
     #x7be9 #x7c11 #x7c14 #x7be6 #x7be5 #x7c60 #x7c00 #x7c07
     #x7c13 #x7bf3 #x7bf7 #x7c17 #x7c0d #x7bf6 #x7c23 #x7c27
     #x7c2a #x7c1f #x7c37 #x7c2b #x7c3d #x7c4c #x7c43 #x7c54
     #x7c4f #x7c40 #x7c50 #x7c58 #x7c5f #x7c64 #x7c56 #x7c65
     #x7c6c #x7c75 #x7c83 #x7c90 #x7ca4 #x7cad #x7ca2 #x7cab
     #x7ca1 #x7ca8 #x7cb3 #x7cb2 #x7cb1 #x7cae #x7cb9 #x7cbd
     #x7cc0 #x7cc5 #x7cc2 #x7cd8 #x7cd2 #x7cdc #x7ce2 #x9b3b
     #x7cef #x7cf2 #x7cf4 #x7cf6 #x7cfa #x7d06 
     ;; #x65
     #x7d02 #x7d1c #x7d15 #x7d0a #x7d45 #x7d4b #x7d2e #x7d32
     #x7d3f #x7d35 #x7d46 #x7d73 #x7d56 #x7d4e #x7d72 #x7d68
     #x7d6e #x7d4f #x7d63 #x7d93 #x7d89 #x7d5b #x7d8f #x7d7d
     #x7d9b #x7dba #x7dae #x7da3 #x7db5 #x7dc7 #x7dbd #x7dab
     #x7e3d #x7da2 #x7daf #x7ddc #x7db8 #x7d9f #x7db0 #x7dd8
     #x7ddd #x7de4 #x7dde #x7dfb #x7df2 #x7de1 #x7e05 #x7e0a
     #x7e23 #x7e21 #x7e12 #x7e31 #x7e1f #x7e09 #x7e0b #x7e22
     #x7e46 #x7e66 #x7e3b #x7e35 #x7e39 #x7e43 #x7e37 #x7e32
     #x7e3a #x7e67 #x7e5d #x7e56 #x7e5e #x7e59 #x7e5a #x7e79
     #x7e6a #x7e69 #x7e7c #x7e7b #x7e83 #x7dd5 #x7e7d #x8fae
     #x7e7f #x7e88 #x7e89 #x7e8c #x7e92 #x7e90 #x7e93 #x7e94
     #x7e96 #x7e8e #x7e9b #x7e9c #x7f38 #x7f3a 
     ;; #x66
     #x7f45 #x7f4c #x7f4d #x7f4e #x7f50 #x7f51 #x7f55 #x7f54
     #x7f58 #x7f5f #x7f60 #x7f68 #x7f69 #x7f67 #x7f78 #x7f82
     #x7f86 #x7f83 #x7f88 #x7f87 #x7f8c #x7f94 #x7f9e #x7f9d
     #x7f9a #x7fa3 #x7faf #x7fb2 #x7fb9 #x7fae #x7fb6 #x7fb8
     #x8b71 #x7fc5 #x7fc6 #x7fca #x7fd5 #x7fd4 #x7fe1 #x7fe6
     #x7fe9 #x7ff3 #x7ff9 #x98dc #x8006 #x8004 #x800b #x8012
     #x8018 #x8019 #x801c #x8021 #x8028 #x803f #x803b #x804a
     #x8046 #x8052 #x8058 #x805a #x805f #x8062 #x8068 #x8073
     #x8072 #x8070 #x8076 #x8079 #x807d #x807f #x8084 #x8086
     #x8085 #x809b #x8093 #x809a #x80ad #x5190 #x80ac #x80db
     #x80e5 #x80d9 #x80dd #x80c4 #x80da #x80d6 #x8109 #x80ef
     #x80f1 #x811b #x8129 #x8123 #x812f #x814b 
     ;; #x67
     #x968b #x8146 #x813e #x8153 #x8151 #x80fc #x8171 #x816e
     #x8165 #x8166 #x8174 #x8183 #x8188 #x818a #x8180 #x8182
     #x81a0 #x8195 #x81a4 #x81a3 #x815f #x8193 #x81a9 #x81b0
     #x81b5 #x81be #x81b8 #x81bd #x81c0 #x81c2 #x81ba #x81c9
     #x81cd #x81d1 #x81d9 #x81d8 #x81c8 #x81da #x81df #x81e0
     #x81e7 #x81fa #x81fb #x81fe #x8201 #x8202 #x8205 #x8207
     #x820a #x820d #x8210 #x8216 #x8229 #x822b #x8238 #x8233
     #x8240 #x8259 #x8258 #x825d #x825a #x825f #x8264 #x8262
     #x8268 #x826a #x826b #x822e #x8271 #x8277 #x8278 #x827e
     #x828d #x8292 #x82ab #x829f #x82bb #x82ac #x82e1 #x82e3
     #x82df #x82d2 #x82f4 #x82f3 #x82fa #x8393 #x8303 #x82fb
     #x82f9 #x82de #x8306 #x82dc #x8309 #x82d9 
     ;; #x68
     #x8335 #x8334 #x8316 #x8332 #x8331 #x8340 #x8339 #x8350
     #x8345 #x832f #x832b #x8317 #x8318 #x8385 #x839a #x83aa
     #x839f #x83a2 #x8396 #x8323 #x838e #x8387 #x838a #x837c
     #x83b5 #x8373 #x8375 #x83a0 #x8389 #x83a8 #x83f4 #x8413
     #x83eb #x83ce #x83fd #x8403 #x83d8 #x840b #x83c1 #x83f7
     #x8407 #x83e0 #x83f2 #x840d #x8422 #x8420 #x83bd #x8438
     #x8506 #x83fb #x846d #x842a #x843c #x855a #x8484 #x8477
     #x846b #x84ad #x846e #x8482 #x8469 #x8446 #x842c #x846f
     #x8479 #x8435 #x84ca #x8462 #x84b9 #x84bf #x849f #x84d9
     #x84cd #x84bb #x84da #x84d0 #x84c1 #x84c6 #x84d6 #x84a1
     #x8521 #x84ff #x84f4 #x8517 #x8518 #x852c #x851f #x8515
     #x8514 #x84fc #x8540 #x8563 #x8558 #x8548 
     ;; #x69
     #x8541 #x8602 #x854b #x8555 #x8580 #x85a4 #x8588 #x8591
     #x858a #x85a8 #x856d #x8594 #x859b #x85ea #x8587 #x859c
     #x8577 #x857e #x8590 #x85c9 #x85ba #x85cf #x85b9 #x85d0
     #x85d5 #x85dd #x85e5 #x85dc #x85f9 #x860a #x8613 #x860b
     #x85fe #x85fa #x8606 #x8622 #x861a #x8630 #x863f #x864d
     #x4e55 #x8654 #x865f #x8667 #x8671 #x8693 #x86a3 #x86a9
     #x86aa #x868b #x868c #x86b6 #x86af #x86c4 #x86c6 #x86b0
     #x86c9 #x8823 #x86ab #x86d4 #x86de #x86e9 #x86ec #x86df
     #x86db #x86ef #x8712 #x8706 #x8708 #x8700 #x8703 #x86fb
     #x8711 #x8709 #x870d #x86f9 #x870a #x8734 #x873f #x8737
     #x873b #x8725 #x8729 #x871a #x8760 #x875f #x8778 #x874c
     #x874e #x8774 #x8757 #x8768 #x876e #x8759 
     ;; #x6a
     #x8753 #x8763 #x876a #x8805 #x87a2 #x879f #x8782 #x87af
     #x87cb #x87bd #x87c0 #x87d0 #x96d6 #x87ab #x87c4 #x87b3
     #x87c7 #x87c6 #x87bb #x87ef #x87f2 #x87e0 #x880f #x880d
     #x87fe #x87f6 #x87f7 #x880e #x87d2 #x8811 #x8816 #x8815
     #x8822 #x8821 #x8831 #x8836 #x8839 #x8827 #x883b #x8844
     #x8842 #x8852 #x8859 #x885e #x8862 #x886b #x8881 #x887e
     #x889e #x8875 #x887d #x88b5 #x8872 #x8882 #x8897 #x8892
     #x88ae #x8899 #x88a2 #x888d #x88a4 #x88b0 #x88bf #x88b1
     #x88c3 #x88c4 #x88d4 #x88d8 #x88d9 #x88dd #x88f9 #x8902
     #x88fc #x88f4 #x88e8 #x88f2 #x8904 #x890c #x890a #x8913
     #x8943 #x891e #x8925 #x892a #x892b #x8941 #x8944 #x893b
     #x8936 #x8938 #x894c #x891d #x8960 #x895e 
     ;; #x6b
     #x8966 #x8964 #x896d #x896a #x896f #x8974 #x8977 #x897e
     #x8983 #x8988 #x898a #x8993 #x8998 #x89a1 #x89a9 #x89a6
     #x89ac #x89af #x89b2 #x89ba #x89bd #x89bf #x89c0 #x89da
     #x89dc #x89dd #x89e7 #x89f4 #x89f8 #x8a03 #x8a16 #x8a10
     #x8a0c #x8a1b #x8a1d #x8a25 #x8a36 #x8a41 #x8a5b #x8a52
     #x8a46 #x8a48 #x8a7c #x8a6d #x8a6c #x8a62 #x8a85 #x8a82
     #x8a84 #x8aa8 #x8aa1 #x8a91 #x8aa5 #x8aa6 #x8a9a #x8aa3
     #x8ac4 #x8acd #x8ac2 #x8ada #x8aeb #x8af3 #x8ae7 #x8ae4
     #x8af1 #x8b14 #x8ae0 #x8ae2 #x8af7 #x8ade #x8adb #x8b0c
     #x8b07 #x8b1a #x8ae1 #x8b16 #x8b10 #x8b17 #x8b20 #x8b33
     #x97ab #x8b26 #x8b2b #x8b3e #x8b28 #x8b41 #x8b4c #x8b4f
     #x8b4e #x8b49 #x8b56 #x8b5b #x8b5a #x8b6b 
     ;; #x6c
     #x8b5f #x8b6c #x8b6f #x8b74 #x8b7d #x8b80 #x8b8c #x8b8e
     #x8b92 #x8b93 #x8b96 #x8b99 #x8b9a #x8c3a #x8c41 #x8c3f
     #x8c48 #x8c4c #x8c4e #x8c50 #x8c55 #x8c62 #x8c6c #x8c78
     #x8c7a #x8c82 #x8c89 #x8c85 #x8c8a #x8c8d #x8c8e #x8c94
     #x8c7c #x8c98 #x621d #x8cad #x8caa #x8cbd #x8cb2 #x8cb3
     #x8cae #x8cb6 #x8cc8 #x8cc1 #x8ce4 #x8ce3 #x8cda #x8cfd
     #x8cfa #x8cfb #x8d04 #x8d05 #x8d0a #x8d07 #x8d0f #x8d0d
     #x8d10 #x9f4e #x8d13 #x8ccd #x8d14 #x8d16 #x8d67 #x8d6d
     #x8d71 #x8d73 #x8d81 #x8d99 #x8dc2 #x8dbe #x8dba #x8dcf
     #x8dda #x8dd6 #x8dcc #x8ddb #x8dcb #x8dea #x8deb #x8ddf
     #x8de3 #x8dfc #x8e08 #x8e09 #x8dff #x8e1d #x8e1e #x8e10
     #x8e1f #x8e42 #x8e35 #x8e30 #x8e34 #x8e4a 
     ;; #x6d
     #x8e47 #x8e49 #x8e4c #x8e50 #x8e48 #x8e59 #x8e64 #x8e60
     #x8e2a #x8e63 #x8e55 #x8e76 #x8e72 #x8e7c #x8e81 #x8e87
     #x8e85 #x8e84 #x8e8b #x8e8a #x8e93 #x8e91 #x8e94 #x8e99
     #x8eaa #x8ea1 #x8eac #x8eb0 #x8ec6 #x8eb1 #x8ebe #x8ec5
     #x8ec8 #x8ecb #x8edb #x8ee3 #x8efc #x8efb #x8eeb #x8efe
     #x8f0a #x8f05 #x8f15 #x8f12 #x8f19 #x8f13 #x8f1c #x8f1f
     #x8f1b #x8f0c #x8f26 #x8f33 #x8f3b #x8f39 #x8f45 #x8f42
     #x8f3e #x8f4c #x8f49 #x8f46 #x8f4e #x8f57 #x8f5c #x8f62
     #x8f63 #x8f64 #x8f9c #x8f9f #x8fa3 #x8fad #x8faf #x8fb7
     #x8fda #x8fe5 #x8fe2 #x8fea #x8fef #x9087 #x8ff4 #x9005
     #x8ff9 #x8ffa #x9011 #x9015 #x9021 #x900d #x901e #x9016
     #x900b #x9027 #x9036 #x9035 #x9039 #x8ff8 
     ;; #x6e
     #x904f #x9050 #x9051 #x9052 #x900e #x9049 #x903e #x9056
     #x9058 #x905e #x9068 #x906f #x9076 #x96a8 #x9072 #x9082
     #x907d #x9081 #x9080 #x908a #x9089 #x908f #x90a8 #x90af
     #x90b1 #x90b5 #x90e2 #x90e4 #x6248 #x90db #x9102 #x9112
     #x9119 #x9132 #x9130 #x914a #x9156 #x9158 #x9163 #x9165
     #x9169 #x9173 #x9172 #x918b #x9189 #x9182 #x91a2 #x91ab
     #x91af #x91aa #x91b5 #x91b4 #x91ba #x91c0 #x91c1 #x91c9
     #x91cb #x91d0 #x91d6 #x91df #x91e1 #x91db #x91fc #x91f5
     #x91f6 #x921e #x91ff #x9214 #x922c #x9215 #x9211 #x925e
     #x9257 #x9245 #x9249 #x9264 #x9248 #x9295 #x923f #x924b
     #x9250 #x929c #x9296 #x9293 #x929b #x925a #x92cf #x92b9
     #x92b7 #x92e9 #x930f #x92fa #x9344 #x932e 
     ;; #x6f
     #x9319 #x9322 #x931a #x9323 #x933a #x9335 #x933b #x935c
     #x9360 #x937c #x936e #x9356 #x93b0 #x93ac #x93ad #x9394
     #x93b9 #x93d6 #x93d7 #x93e8 #x93e5 #x93d8 #x93c3 #x93dd
     #x93d0 #x93c8 #x93e4 #x941a #x9414 #x9413 #x9403 #x9407
     #x9410 #x9436 #x942b #x9435 #x9421 #x943a #x9441 #x9452
     #x9444 #x945b #x9460 #x9462 #x945e #x946a #x9229 #x9470
     #x9475 #x9477 #x947d #x945a #x947c #x947e #x9481 #x947f
     #x9582 #x9587 #x958a #x9594 #x9596 #x9598 #x9599 #x95a0
     #x95a8 #x95a7 #x95ad #x95bc #x95bb #x95b9 #x95be #x95ca
     #x6ff6 #x95c3 #x95cd #x95cc #x95d5 #x95d4 #x95d6 #x95dc
     #x95e1 #x95e5 #x95e2 #x9621 #x9628 #x962e #x962f #x9642
     #x964c #x964f #x964b #x9677 #x965c #x965e 
     ;; #x70
     #x965d #x965f #x9666 #x9672 #x966c #x968d #x9698 #x9695
     #x9697 #x96aa #x96a7 #x96b1 #x96b2 #x96b0 #x96b4 #x96b6
     #x96b8 #x96b9 #x96ce #x96cb #x96c9 #x96cd #x894d #x96dc
     #x970d #x96d5 #x96f9 #x9704 #x9706 #x9708 #x9713 #x970e
     #x9711 #x970f #x9716 #x9719 #x9724 #x972a #x9730 #x9739
     #x973d #x973e #x9744 #x9746 #x9748 #x9742 #x9749 #x975c
     #x9760 #x9764 #x9766 #x9768 #x52d2 #x976b #x9771 #x9779
     #x9785 #x977c #x9781 #x977a #x9786 #x978b #x978f #x9790
     #x979c #x97a8 #x97a6 #x97a3 #x97b3 #x97b4 #x97c3 #x97c6
     #x97c8 #x97cb #x97dc #x97ed #x9f4f #x97f2 #x7adf #x97f6
     #x97f5 #x980f #x980c #x9838 #x9824 #x9821 #x9837 #x983d
     #x9846 #x984f #x984b #x986b #x986f #x9870 
     ;; #x71
     #x9871 #x9874 #x9873 #x98aa #x98af #x98b1 #x98b6 #x98c4
     #x98c3 #x98c6 #x98e9 #x98eb #x9903 #x9909 #x9912 #x9914
     #x9918 #x9921 #x991d #x991e #x9924 #x9920 #x992c #x992e
     #x993d #x993e #x9942 #x9949 #x9945 #x9950 #x994b #x9951
     #x9952 #x994c #x9955 #x9997 #x9998 #x99a5 #x99ad #x99ae
     #x99bc #x99df #x99db #x99dd #x99d8 #x99d1 #x99ed #x99ee
     #x99f1 #x99f2 #x99fb #x99f8 #x9a01 #x9a0f #x9a05 #x99e2
     #x9a19 #x9a2b #x9a37 #x9a45 #x9a42 #x9a40 #x9a43 #x9a3e
     #x9a55 #x9a4d #x9a5b #x9a57 #x9a5f #x9a62 #x9a65 #x9a64
     #x9a69 #x9a6b #x9a6a #x9aad #x9ab0 #x9abc #x9ac0 #x9acf
     #x9ad1 #x9ad3 #x9ad4 #x9ade #x9adf #x9ae2 #x9ae3 #x9ae6
     #x9aef #x9aeb #x9aee #x9af4 #x9af1 #x9af7 
     ;; #x72
     #x9afb #x9b06 #x9b18 #x9b1a #x9b1f #x9b22 #x9b23 #x9b25
     #x9b27 #x9b28 #x9b29 #x9b2a #x9b2e #x9b2f #x9b32 #x9b44
     #x9b43 #x9b4f #x9b4d #x9b4e #x9b51 #x9b58 #x9b74 #x9b93
     #x9b83 #x9b91 #x9b96 #x9b97 #x9b9f #x9ba0 #x9ba8 #x9bb4
     #x9bc0 #x9bca #x9bb9 #x9bc6 #x9bcf #x9bd1 #x9bd2 #x9be3
     #x9be2 #x9be4 #x9bd4 #x9be1 #x9c3a #x9bf2 #x9bf1 #x9bf0
     #x9c15 #x9c14 #x9c09 #x9c13 #x9c0c #x9c06 #x9c08 #x9c12
     #x9c0a #x9c04 #x9c2e #x9c1b #x9c25 #x9c24 #x9c21 #x9c30
     #x9c47 #x9c32 #x9c46 #x9c3e #x9c5a #x9c60 #x9c67 #x9c76
     #x9c78 #x9ce7 #x9cec #x9cf0 #x9d09 #x9d08 #x9ceb #x9d03
     #x9d06 #x9d2a #x9d26 #x9daf #x9d23 #x9d1f #x9d44 #x9d15
     #x9d12 #x9d41 #x9d3f #x9d3e #x9d46 #x9d48 
     ;; #x73
     #x9d5d #x9d5e #x9d64 #x9d51 #x9d50 #x9d59 #x9d72 #x9d89
     #x9d87 #x9dab #x9d6f #x9d7a #x9d9a #x9da4 #x9da9 #x9db2
     #x9dc4 #x9dc1 #x9dbb #x9db8 #x9dba #x9dc6 #x9dcf #x9dc2
     #x9dd9 #x9dd3 #x9df8 #x9de6 #x9ded #x9def #x9dfd #x9e1a
     #x9e1b #x9e1e #x9e75 #x9e79 #x9e7d #x9e81 #x9e88 #x9e8b
     #x9e8c #x9e92 #x9e95 #x9e91 #x9e9d #x9ea5 #x9ea9 #x9eb8
     #x9eaa #x9ead #x9761 #x9ecc #x9ece #x9ecf #x9ed0 #x9ed4
     #x9edc #x9ede #x9edd #x9ee0 #x9ee5 #x9ee8 #x9eef #x9ef4
     #x9ef6 #x9ef7 #x9ef9 #x9efb #x9efc #x9efd #x9f07 #x9f08
     #x76b7 #x9f15 #x9f21 #x9f2c #x9f3e #x9f4a #x9f52 #x9f54
     #x9f63 #x9f5f #x9f60 #x9f61 #x9f66 #x9f67 #x9f6c #x9f6a
     #x9f77 #x9f72 #x9f76 #x9f95 #x9f9c #x9fa0 
     ;; #x74
     #x582f #x69c7 #x9059 #x7464 #x51dc #x7199)
  :test #'equalp)

(define-constant +unicode-to-jis-x-0208+
    (let ((h (make-hash-table)))
      (flet ((flip-mapping (unicodes b1-start)
               (loop
                 with b1 = b1-start
                 with b2 = #x21
                 for unicode across unicodes
                 do
                    (unless (= unicode #xfffd)
                      (setf (gethash unicode h) (logior (ash b1 8) b2))) 
                    (incf b2)
                    (when (= b2 #x7f)
                      (incf b1)
                      (setf b2 #x21)))))
        (flip-mapping +jis-x-0208-to-uni-page21+ #x21)
        (flip-mapping +jis-x-0208-to-uni-page30+ #x30)
        h))
  :test #'equalp)
