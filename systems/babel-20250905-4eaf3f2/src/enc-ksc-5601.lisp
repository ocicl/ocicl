;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-ksc-5601.lisp --- Implementation of the KS C 5601 character encoding.
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

(define-constant +ksc-5601-21-2c-to-unicode+
    #(
      ;; #x21
      #x3000 #x3001 #x3002 #x00b7 #x2025 #x2026 #x00a8 #x3003
      #x00ad #x2015 #x2225 #xff3c #x223c #x2018 #x2019 #x201c
      #x201d #x3014 #x3015 #x3008 #x3009 #x300a #x300b #x300c
      #x300d #x300e #x300f #x3010 #x3011 #x00b1 #x00d7 #x00f7
      #x2260 #x2264 #x2265 #x221e #x2234 #x00b0 #x2032 #x2033
      #x2103 #x212b #xffe0 #xffe1 #xffe5 #x2642 #x2640 #x2220
      #x22a5 #x2312 #x2202 #x2207 #x2261 #x2252 #x00a7 #x203b
      #x2606 #x2605 #x25cb #x25cf #x25ce #x25c7 #x25c6 #x25a1
      #x25a0 #x25b3 #x25b2 #x25bd #x25bc #x2192 #x2190 #x2191
      #x2193 #x2194 #x3013 #x226a #x226b #x221a #x223d #x221d
      #x2235 #x222b #x222c #x2208 #x220b #x2286 #x2287 #x2282
      #x2283 #x222a #x2229 #x2227 #x2228 #xffe2
      ;; #x22
      #x21d2 #x21d4 #x2200 #x2203 #x00b4 #xff5e #x02c7 #x02d8
      #x02dd #x02da #x02d9 #x00b8 #x02db #x00a1 #x00bf #x02d0
      #x222e #x2211 #x220f #x00a4 #x2109 #x2030 #x25c1 #x25c0
      #x25b7 #x25b6 #x2664 #x2660 #x2661 #x2665 #x2667 #x2663
      #x2299 #x25c8 #x25a3 #x25d0 #x25d1 #x2592 #x25a4 #x25a5
      #x25a8 #x25a7 #x25a6 #x25a9 #x2668 #x260f #x260e #x261c
      #x261e #x00b6 #x2020 #x2021 #x2195 #x2197 #x2199 #x2196
      #x2198 #x266d #x2669 #x266a #x266c #x327f #x321c #x2116
      #x33c7 #x2122 #x33c2 #x33d8 #x2121 #x20ac #x00ae #x327e
      #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
      #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
      #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
      ;; #x23
      #xff01 #xff02 #xff03 #xff04 #xff05 #xff06 #xff07 #xff08
      #xff09 #xff0a #xff0b #xff0c #xff0d #xff0e #xff0f #xff10
      #xff11 #xff12 #xff13 #xff14 #xff15 #xff16 #xff17 #xff18
      #xff19 #xff1a #xff1b #xff1c #xff1d #xff1e #xff1f #xff20
      #xff21 #xff22 #xff23 #xff24 #xff25 #xff26 #xff27 #xff28
      #xff29 #xff2a #xff2b #xff2c #xff2d #xff2e #xff2f #xff30
      #xff31 #xff32 #xff33 #xff34 #xff35 #xff36 #xff37 #xff38
      #xff39 #xff3a #xff3b #xffe6 #xff3d #xff3e #xff3f #xff40
      #xff41 #xff42 #xff43 #xff44 #xff45 #xff46 #xff47 #xff48
      #xff49 #xff4a #xff4b #xff4c #xff4d #xff4e #xff4f #xff50
      #xff51 #xff52 #xff53 #xff54 #xff55 #xff56 #xff57 #xff58
      #xff59 #xff5a #xff5b #xff5c #xff5d #xffe3
      ;; #x24
      #x3131 #x3132 #x3133 #x3134 #x3135 #x3136 #x3137 #x3138
      #x3139 #x313a #x313b #x313c #x313d #x313e #x313f #x3140
      #x3141 #x3142 #x3143 #x3144 #x3145 #x3146 #x3147 #x3148
      #x3149 #x314a #x314b #x314c #x314d #x314e #x314f #x3150
      #x3151 #x3152 #x3153 #x3154 #x3155 #x3156 #x3157 #x3158
      #x3159 #x315a #x315b #x315c #x315d #x315e #x315f #x3160
      #x3161 #x3162 #x3163 #x3164 #x3165 #x3166 #x3167 #x3168
      #x3169 #x316a #x316b #x316c #x316d #x316e #x316f #x3170
      #x3171 #x3172 #x3173 #x3174 #x3175 #x3176 #x3177 #x3178
      #x3179 #x317a #x317b #x317c #x317d #x317e #x317f #x3180
      #x3181 #x3182 #x3183 #x3184 #x3185 #x3186 #x3187 #x3188
      #x3189 #x318a #x318b #x318c #x318d #x318e
      ;; #x25
      #x2170 #x2171 #x2172 #x2173 #x2174 #x2175 #x2176 #x2177
      #x2178 #x2179 #xfffd #xfffd #xfffd #xfffd #xfffd #x2160
      #x2161 #x2162 #x2163 #x2164 #x2165 #x2166 #x2167 #x2168
      #x2169 #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
      #x0391 #x0392 #x0393 #x0394 #x0395 #x0396 #x0397 #x0398
      #x0399 #x039a #x039b #x039c #x039d #x039e #x039f #x03a0
      #x03a1 #x03a3 #x03a4 #x03a5 #x03a6 #x03a7 #x03a8 #x03a9
      #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
      #x03b1 #x03b2 #x03b3 #x03b4 #x03b5 #x03b6 #x03b7 #x03b8
      #x03b9 #x03ba #x03bb #x03bc #x03bd #x03be #x03bf #x03c0
      #x03c1 #x03c3 #x03c4 #x03c5 #x03c6 #x03c7 #x03c8 #x03c9
      #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
      ;; #x26
      #x2500 #x2502 #x250c #x2510 #x2518 #x2514 #x251c #x252c
      #x2524 #x2534 #x253c #x2501 #x2503 #x250f #x2513 #x251b
      #x2517 #x2523 #x2533 #x252b #x253b #x254b #x2520 #x252f
      #x2528 #x2537 #x253f #x251d #x2530 #x2525 #x2538 #x2542
      #x2512 #x2511 #x251a #x2519 #x2516 #x2515 #x250e #x250d
      #x251e #x251f #x2521 #x2522 #x2526 #x2527 #x2529 #x252a
      #x252d #x252e #x2531 #x2532 #x2535 #x2536 #x2539 #x253a
      #x253d #x253e #x2540 #x2541 #x2543 #x2544 #x2545 #x2546
      #x2547 #x2548 #x2549 #x254a #xfffd #xfffd #xfffd #xfffd
      #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
      #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
      #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
      ;; #x27
      #x3395 #x3396 #x3397 #x2113 #x3398 #x33c4 #x33a3 #x33a4
      #x33a5 #x33a6 #x3399 #x339a #x339b #x339c #x339d #x339e
      #x339f #x33a0 #x33a1 #x33a2 #x33ca #x338d #x338e #x338f
      #x33cf #x3388 #x3389 #x33c8 #x33a7 #x33a8 #x33b0 #x33b1
      #x33b2 #x33b3 #x33b4 #x33b5 #x33b6 #x33b7 #x33b8 #x33b9
      #x3380 #x3381 #x3382 #x3383 #x3384 #x33ba #x33bb #x33bc
      #x33bd #x33be #x33bf #x3390 #x3391 #x3392 #x3393 #x3394
      #x2126 #x33c0 #x33c1 #x338a #x338b #x338c #x33d6 #x33c5
      #x33ad #x33ae #x33af #x33db #x33a9 #x33aa #x33ab #x33ac
      #x33dd #x33d0 #x33d3 #x33c3 #x33c9 #x33dc #x33c6 #xfffd
      #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
      #xfffd #xfffd #xfffd #xfffd #xfffd #xfffd
      ;; #x28
      #x00c6 #x00d0 #x00aa #x0126 #xfffd #x0132 #xfffd #x013f
      #x0141 #x00d8 #x0152 #x00ba #x00de #x0166 #x014a #xfffd
      #x3260 #x3261 #x3262 #x3263 #x3264 #x3265 #x3266 #x3267
      #x3268 #x3269 #x326a #x326b #x326c #x326d #x326e #x326f
      #x3270 #x3271 #x3272 #x3273 #x3274 #x3275 #x3276 #x3277
      #x3278 #x3279 #x327a #x327b #x24d0 #x24d1 #x24d2 #x24d3
      #x24d4 #x24d5 #x24d6 #x24d7 #x24d8 #x24d9 #x24da #x24db
      #x24dc #x24dd #x24de #x24df #x24e0 #x24e1 #x24e2 #x24e3
      #x24e4 #x24e5 #x24e6 #x24e7 #x24e8 #x24e9 #x2460 #x2461
      #x2462 #x2463 #x2464 #x2465 #x2466 #x2467 #x2468 #x2469
      #x246a #x246b #x246c #x246d #x246e #x00bd #x2153 #x2154
      #x00bc #x00be #x215b #x215c #x215d #x215e
      ;; #x29
      #x00e6 #x0111 #x00f0 #x0127 #x0131 #x0133 #x0138 #x0140
      #x0142 #x00f8 #x0153 #x00df #x00fe #x0167 #x014b #x0149
      #x3200 #x3201 #x3202 #x3203 #x3204 #x3205 #x3206 #x3207
      #x3208 #x3209 #x320a #x320b #x320c #x320d #x320e #x320f
      #x3210 #x3211 #x3212 #x3213 #x3214 #x3215 #x3216 #x3217
      #x3218 #x3219 #x321a #x321b #x249c #x249d #x249e #x249f
      #x24a0 #x24a1 #x24a2 #x24a3 #x24a4 #x24a5 #x24a6 #x24a7
      #x24a8 #x24a9 #x24aa #x24ab #x24ac #x24ad #x24ae #x24af
      #x24b0 #x24b1 #x24b2 #x24b3 #x24b4 #x24b5 #x2474 #x2475
      #x2476 #x2477 #x2478 #x2479 #x247a #x247b #x247c #x247d
      #x247e #x247f #x2480 #x2481 #x2482 #x00b9 #x00b2 #x00b3
      #x2074 #x207f #x2081 #x2082 #x2083 #x2084
      ;; #x2a
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
      ;; #x2b
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
      ;; #x2c
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
      )
  :test #'equalp)

(define-constant +ksc-5601-30-48-to-unicode+
    #(
      ;; #x30
      #xac00 #xac01 #xac04 #xac07 #xac08 #xac09 #xac0a #xac10
      #xac11 #xac12 #xac13 #xac14 #xac15 #xac16 #xac17 #xac19
      #xac1a #xac1b #xac1c #xac1d #xac20 #xac24 #xac2c #xac2d
      #xac2f #xac30 #xac31 #xac38 #xac39 #xac3c #xac40 #xac4b
      #xac4d #xac54 #xac58 #xac5c #xac70 #xac71 #xac74 #xac77
      #xac78 #xac7a #xac80 #xac81 #xac83 #xac84 #xac85 #xac86
      #xac89 #xac8a #xac8b #xac8c #xac90 #xac94 #xac9c #xac9d
      #xac9f #xaca0 #xaca1 #xaca8 #xaca9 #xacaa #xacac #xacaf
      #xacb0 #xacb8 #xacb9 #xacbb #xacbc #xacbd #xacc1 #xacc4
      #xacc8 #xaccc #xacd5 #xacd7 #xace0 #xace1 #xace4 #xace7
      #xace8 #xacea #xacec #xacef #xacf0 #xacf1 #xacf3 #xacf5
      #xacf6 #xacfc #xacfd #xad00 #xad04 #xad06
      ;; #x31
      #xad0c #xad0d #xad0f #xad11 #xad18 #xad1c #xad20 #xad29
      #xad2c #xad2d #xad34 #xad35 #xad38 #xad3c #xad44 #xad45
      #xad47 #xad49 #xad50 #xad54 #xad58 #xad61 #xad63 #xad6c
      #xad6d #xad70 #xad73 #xad74 #xad75 #xad76 #xad7b #xad7c
      #xad7d #xad7f #xad81 #xad82 #xad88 #xad89 #xad8c #xad90
      #xad9c #xad9d #xada4 #xadb7 #xadc0 #xadc1 #xadc4 #xadc8
      #xadd0 #xadd1 #xadd3 #xaddc #xade0 #xade4 #xadf8 #xadf9
      #xadfc #xadff #xae00 #xae01 #xae08 #xae09 #xae0b #xae0d
      #xae14 #xae30 #xae31 #xae34 #xae37 #xae38 #xae3a #xae40
      #xae41 #xae43 #xae45 #xae46 #xae4a #xae4c #xae4d #xae4e
      #xae50 #xae54 #xae56 #xae5c #xae5d #xae5f #xae60 #xae61
      #xae65 #xae68 #xae69 #xae6c #xae70 #xae78
      ;; #x32
      #xae79 #xae7b #xae7c #xae7d #xae84 #xae85 #xae8c #xaebc
      #xaebd #xaebe #xaec0 #xaec4 #xaecc #xaecd #xaecf #xaed0
      #xaed1 #xaed8 #xaed9 #xaedc #xaee8 #xaeeb #xaeed #xaef4
      #xaef8 #xaefc #xaf07 #xaf08 #xaf0d #xaf10 #xaf2c #xaf2d
      #xaf30 #xaf32 #xaf34 #xaf3c #xaf3d #xaf3f #xaf41 #xaf42
      #xaf43 #xaf48 #xaf49 #xaf50 #xaf5c #xaf5d #xaf64 #xaf65
      #xaf79 #xaf80 #xaf84 #xaf88 #xaf90 #xaf91 #xaf95 #xaf9c
      #xafb8 #xafb9 #xafbc #xafc0 #xafc7 #xafc8 #xafc9 #xafcb
      #xafcd #xafce #xafd4 #xafdc #xafe8 #xafe9 #xaff0 #xaff1
      #xaff4 #xaff8 #xb000 #xb001 #xb004 #xb00c #xb010 #xb014
      #xb01c #xb01d #xb028 #xb044 #xb045 #xb048 #xb04a #xb04c
      #xb04e #xb053 #xb054 #xb055 #xb057 #xb059
      ;; #x33
      #xb05d #xb07c #xb07d #xb080 #xb084 #xb08c #xb08d #xb08f
      #xb091 #xb098 #xb099 #xb09a #xb09c #xb09f #xb0a0 #xb0a1
      #xb0a2 #xb0a8 #xb0a9 #xb0ab #xb0ac #xb0ad #xb0ae #xb0af
      #xb0b1 #xb0b3 #xb0b4 #xb0b5 #xb0b8 #xb0bc #xb0c4 #xb0c5
      #xb0c7 #xb0c8 #xb0c9 #xb0d0 #xb0d1 #xb0d4 #xb0d8 #xb0e0
      #xb0e5 #xb108 #xb109 #xb10b #xb10c #xb110 #xb112 #xb113
      #xb118 #xb119 #xb11b #xb11c #xb11d #xb123 #xb124 #xb125
      #xb128 #xb12c #xb134 #xb135 #xb137 #xb138 #xb139 #xb140
      #xb141 #xb144 #xb148 #xb150 #xb151 #xb154 #xb155 #xb158
      #xb15c #xb160 #xb178 #xb179 #xb17c #xb180 #xb182 #xb188
      #xb189 #xb18b #xb18d #xb192 #xb193 #xb194 #xb198 #xb19c
      #xb1a8 #xb1cc #xb1d0 #xb1d4 #xb1dc #xb1dd
      ;; #x34
      #xb1df #xb1e8 #xb1e9 #xb1ec #xb1f0 #xb1f9 #xb1fb #xb1fd
      #xb204 #xb205 #xb208 #xb20b #xb20c #xb214 #xb215 #xb217
      #xb219 #xb220 #xb234 #xb23c #xb258 #xb25c #xb260 #xb268
      #xb269 #xb274 #xb275 #xb27c #xb284 #xb285 #xb289 #xb290
      #xb291 #xb294 #xb298 #xb299 #xb29a #xb2a0 #xb2a1 #xb2a3
      #xb2a5 #xb2a6 #xb2aa #xb2ac #xb2b0 #xb2b4 #xb2c8 #xb2c9
      #xb2cc #xb2d0 #xb2d2 #xb2d8 #xb2d9 #xb2db #xb2dd #xb2e2
      #xb2e4 #xb2e5 #xb2e6 #xb2e8 #xb2eb #xb2ec #xb2ed #xb2ee
      #xb2ef #xb2f3 #xb2f4 #xb2f5 #xb2f7 #xb2f8 #xb2f9 #xb2fa
      #xb2fb #xb2ff #xb300 #xb301 #xb304 #xb308 #xb310 #xb311
      #xb313 #xb314 #xb315 #xb31c #xb354 #xb355 #xb356 #xb358
      #xb35b #xb35c #xb35e #xb35f #xb364 #xb365
      ;; #x35
      #xb367 #xb369 #xb36b #xb36e #xb370 #xb371 #xb374 #xb378
      #xb380 #xb381 #xb383 #xb384 #xb385 #xb38c #xb390 #xb394
      #xb3a0 #xb3a1 #xb3a8 #xb3ac #xb3c4 #xb3c5 #xb3c8 #xb3cb
      #xb3cc #xb3ce #xb3d0 #xb3d4 #xb3d5 #xb3d7 #xb3d9 #xb3db
      #xb3dd #xb3e0 #xb3e4 #xb3e8 #xb3fc #xb410 #xb418 #xb41c
      #xb420 #xb428 #xb429 #xb42b #xb434 #xb450 #xb451 #xb454
      #xb458 #xb460 #xb461 #xb463 #xb465 #xb46c #xb480 #xb488
      #xb49d #xb4a4 #xb4a8 #xb4ac #xb4b5 #xb4b7 #xb4b9 #xb4c0
      #xb4c4 #xb4c8 #xb4d0 #xb4d5 #xb4dc #xb4dd #xb4e0 #xb4e3
      #xb4e4 #xb4e6 #xb4ec #xb4ed #xb4ef #xb4f1 #xb4f8 #xb514
      #xb515 #xb518 #xb51b #xb51c #xb524 #xb525 #xb527 #xb528
      #xb529 #xb52a #xb530 #xb531 #xb534 #xb538
      ;; #x36
      #xb540 #xb541 #xb543 #xb544 #xb545 #xb54b #xb54c #xb54d
      #xb550 #xb554 #xb55c #xb55d #xb55f #xb560 #xb561 #xb5a0
      #xb5a1 #xb5a4 #xb5a8 #xb5aa #xb5ab #xb5b0 #xb5b1 #xb5b3
      #xb5b4 #xb5b5 #xb5bb #xb5bc #xb5bd #xb5c0 #xb5c4 #xb5cc
      #xb5cd #xb5cf #xb5d0 #xb5d1 #xb5d8 #xb5ec #xb610 #xb611
      #xb614 #xb618 #xb625 #xb62c #xb634 #xb648 #xb664 #xb668
      #xb69c #xb69d #xb6a0 #xb6a4 #xb6ab #xb6ac #xb6b1 #xb6d4
      #xb6f0 #xb6f4 #xb6f8 #xb700 #xb701 #xb705 #xb728 #xb729
      #xb72c #xb72f #xb730 #xb738 #xb739 #xb73b #xb744 #xb748
      #xb74c #xb754 #xb755 #xb760 #xb764 #xb768 #xb770 #xb771
      #xb773 #xb775 #xb77c #xb77d #xb780 #xb784 #xb78c #xb78d
      #xb78f #xb790 #xb791 #xb792 #xb796 #xb797
      ;; #x37
      #xb798 #xb799 #xb79c #xb7a0 #xb7a8 #xb7a9 #xb7ab #xb7ac
      #xb7ad #xb7b4 #xb7b5 #xb7b8 #xb7c7 #xb7c9 #xb7ec #xb7ed
      #xb7f0 #xb7f4 #xb7fc #xb7fd #xb7ff #xb800 #xb801 #xb807
      #xb808 #xb809 #xb80c #xb810 #xb818 #xb819 #xb81b #xb81d
      #xb824 #xb825 #xb828 #xb82c #xb834 #xb835 #xb837 #xb838
      #xb839 #xb840 #xb844 #xb851 #xb853 #xb85c #xb85d #xb860
      #xb864 #xb86c #xb86d #xb86f #xb871 #xb878 #xb87c #xb88d
      #xb8a8 #xb8b0 #xb8b4 #xb8b8 #xb8c0 #xb8c1 #xb8c3 #xb8c5
      #xb8cc #xb8d0 #xb8d4 #xb8dd #xb8df #xb8e1 #xb8e8 #xb8e9
      #xb8ec #xb8f0 #xb8f8 #xb8f9 #xb8fb #xb8fd #xb904 #xb918
      #xb920 #xb93c #xb93d #xb940 #xb944 #xb94c #xb94f #xb951
      #xb958 #xb959 #xb95c #xb960 #xb968 #xb969
      ;; #x38
      #xb96b #xb96d #xb974 #xb975 #xb978 #xb97c #xb984 #xb985
      #xb987 #xb989 #xb98a #xb98d #xb98e #xb9ac #xb9ad #xb9b0
      #xb9b4 #xb9bc #xb9bd #xb9bf #xb9c1 #xb9c8 #xb9c9 #xb9cc
      #xb9ce #xb9cf #xb9d0 #xb9d1 #xb9d2 #xb9d8 #xb9d9 #xb9db
      #xb9dd #xb9de #xb9e1 #xb9e3 #xb9e4 #xb9e5 #xb9e8 #xb9ec
      #xb9f4 #xb9f5 #xb9f7 #xb9f8 #xb9f9 #xb9fa #xba00 #xba01
      #xba08 #xba15 #xba38 #xba39 #xba3c #xba40 #xba42 #xba48
      #xba49 #xba4b #xba4d #xba4e #xba53 #xba54 #xba55 #xba58
      #xba5c #xba64 #xba65 #xba67 #xba68 #xba69 #xba70 #xba71
      #xba74 #xba78 #xba83 #xba84 #xba85 #xba87 #xba8c #xbaa8
      #xbaa9 #xbaab #xbaac #xbab0 #xbab2 #xbab8 #xbab9 #xbabb
      #xbabd #xbac4 #xbac8 #xbad8 #xbad9 #xbafc
      ;; #x39
      #xbb00 #xbb04 #xbb0d #xbb0f #xbb11 #xbb18 #xbb1c #xbb20
      #xbb29 #xbb2b #xbb34 #xbb35 #xbb36 #xbb38 #xbb3b #xbb3c
      #xbb3d #xbb3e #xbb44 #xbb45 #xbb47 #xbb49 #xbb4d #xbb4f
      #xbb50 #xbb54 #xbb58 #xbb61 #xbb63 #xbb6c #xbb88 #xbb8c
      #xbb90 #xbba4 #xbba8 #xbbac #xbbb4 #xbbb7 #xbbc0 #xbbc4
      #xbbc8 #xbbd0 #xbbd3 #xbbf8 #xbbf9 #xbbfc #xbbff #xbc00
      #xbc02 #xbc08 #xbc09 #xbc0b #xbc0c #xbc0d #xbc0f #xbc11
      #xbc14 #xbc15 #xbc16 #xbc17 #xbc18 #xbc1b #xbc1c #xbc1d
      #xbc1e #xbc1f #xbc24 #xbc25 #xbc27 #xbc29 #xbc2d #xbc30
      #xbc31 #xbc34 #xbc38 #xbc40 #xbc41 #xbc43 #xbc44 #xbc45
      #xbc49 #xbc4c #xbc4d #xbc50 #xbc5d #xbc84 #xbc85 #xbc88
      #xbc8b #xbc8c #xbc8e #xbc94 #xbc95 #xbc97
      ;; #x3a
      #xbc99 #xbc9a #xbca0 #xbca1 #xbca4 #xbca7 #xbca8 #xbcb0
      #xbcb1 #xbcb3 #xbcb4 #xbcb5 #xbcbc #xbcbd #xbcc0 #xbcc4
      #xbccd #xbccf #xbcd0 #xbcd1 #xbcd5 #xbcd8 #xbcdc #xbcf4
      #xbcf5 #xbcf6 #xbcf8 #xbcfc #xbd04 #xbd05 #xbd07 #xbd09
      #xbd10 #xbd14 #xbd24 #xbd2c #xbd40 #xbd48 #xbd49 #xbd4c
      #xbd50 #xbd58 #xbd59 #xbd64 #xbd68 #xbd80 #xbd81 #xbd84
      #xbd87 #xbd88 #xbd89 #xbd8a #xbd90 #xbd91 #xbd93 #xbd95
      #xbd99 #xbd9a #xbd9c #xbda4 #xbdb0 #xbdb8 #xbdd4 #xbdd5
      #xbdd8 #xbddc #xbde9 #xbdf0 #xbdf4 #xbdf8 #xbe00 #xbe03
      #xbe05 #xbe0c #xbe0d #xbe10 #xbe14 #xbe1c #xbe1d #xbe1f
      #xbe44 #xbe45 #xbe48 #xbe4c #xbe4e #xbe54 #xbe55 #xbe57
      #xbe59 #xbe5a #xbe5b #xbe60 #xbe61 #xbe64
      ;; #x3b
      #xbe68 #xbe6a #xbe70 #xbe71 #xbe73 #xbe74 #xbe75 #xbe7b
      #xbe7c #xbe7d #xbe80 #xbe84 #xbe8c #xbe8d #xbe8f #xbe90
      #xbe91 #xbe98 #xbe99 #xbea8 #xbed0 #xbed1 #xbed4 #xbed7
      #xbed8 #xbee0 #xbee3 #xbee4 #xbee5 #xbeec #xbf01 #xbf08
      #xbf09 #xbf18 #xbf19 #xbf1b #xbf1c #xbf1d #xbf40 #xbf41
      #xbf44 #xbf48 #xbf50 #xbf51 #xbf55 #xbf94 #xbfb0 #xbfc5
      #xbfcc #xbfcd #xbfd0 #xbfd4 #xbfdc #xbfdf #xbfe1 #xc03c
      #xc051 #xc058 #xc05c #xc060 #xc068 #xc069 #xc090 #xc091
      #xc094 #xc098 #xc0a0 #xc0a1 #xc0a3 #xc0a5 #xc0ac #xc0ad
      #xc0af #xc0b0 #xc0b3 #xc0b4 #xc0b5 #xc0b6 #xc0bc #xc0bd
      #xc0bf #xc0c0 #xc0c1 #xc0c5 #xc0c8 #xc0c9 #xc0cc #xc0d0
      #xc0d8 #xc0d9 #xc0db #xc0dc #xc0dd #xc0e4
      ;; #x3c
      #xc0e5 #xc0e8 #xc0ec #xc0f4 #xc0f5 #xc0f7 #xc0f9 #xc100
      #xc104 #xc108 #xc110 #xc115 #xc11c #xc11d #xc11e #xc11f
      #xc120 #xc123 #xc124 #xc126 #xc127 #xc12c #xc12d #xc12f
      #xc130 #xc131 #xc136 #xc138 #xc139 #xc13c #xc140 #xc148
      #xc149 #xc14b #xc14c #xc14d #xc154 #xc155 #xc158 #xc15c
      #xc164 #xc165 #xc167 #xc168 #xc169 #xc170 #xc174 #xc178
      #xc185 #xc18c #xc18d #xc18e #xc190 #xc194 #xc196 #xc19c
      #xc19d #xc19f #xc1a1 #xc1a5 #xc1a8 #xc1a9 #xc1ac #xc1b0
      #xc1bd #xc1c4 #xc1c8 #xc1cc #xc1d4 #xc1d7 #xc1d8 #xc1e0
      #xc1e4 #xc1e8 #xc1f0 #xc1f1 #xc1f3 #xc1fc #xc1fd #xc200
      #xc204 #xc20c #xc20d #xc20f #xc211 #xc218 #xc219 #xc21c
      #xc21f #xc220 #xc228 #xc229 #xc22b #xc22d
      ;; #x3d
      #xc22f #xc231 #xc232 #xc234 #xc248 #xc250 #xc251 #xc254
      #xc258 #xc260 #xc265 #xc26c #xc26d #xc270 #xc274 #xc27c
      #xc27d #xc27f #xc281 #xc288 #xc289 #xc290 #xc298 #xc29b
      #xc29d #xc2a4 #xc2a5 #xc2a8 #xc2ac #xc2ad #xc2b4 #xc2b5
      #xc2b7 #xc2b9 #xc2dc #xc2dd #xc2e0 #xc2e3 #xc2e4 #xc2eb
      #xc2ec #xc2ed #xc2ef #xc2f1 #xc2f6 #xc2f8 #xc2f9 #xc2fb
      #xc2fc #xc300 #xc308 #xc309 #xc30c #xc30d #xc313 #xc314
      #xc315 #xc318 #xc31c #xc324 #xc325 #xc328 #xc329 #xc345
      #xc368 #xc369 #xc36c #xc370 #xc372 #xc378 #xc379 #xc37c
      #xc37d #xc384 #xc388 #xc38c #xc3c0 #xc3d8 #xc3d9 #xc3dc
      #xc3df #xc3e0 #xc3e2 #xc3e8 #xc3e9 #xc3ed #xc3f4 #xc3f5
      #xc3f8 #xc408 #xc410 #xc424 #xc42c #xc430
      ;; #x3e
      #xc434 #xc43c #xc43d #xc448 #xc464 #xc465 #xc468 #xc46c
      #xc474 #xc475 #xc479 #xc480 #xc494 #xc49c #xc4b8 #xc4bc
      #xc4e9 #xc4f0 #xc4f1 #xc4f4 #xc4f8 #xc4fa #xc4ff #xc500
      #xc501 #xc50c #xc510 #xc514 #xc51c #xc528 #xc529 #xc52c
      #xc530 #xc538 #xc539 #xc53b #xc53d #xc544 #xc545 #xc548
      #xc549 #xc54a #xc54c #xc54d #xc54e #xc553 #xc554 #xc555
      #xc557 #xc558 #xc559 #xc55d #xc55e #xc560 #xc561 #xc564
      #xc568 #xc570 #xc571 #xc573 #xc574 #xc575 #xc57c #xc57d
      #xc580 #xc584 #xc587 #xc58c #xc58d #xc58f #xc591 #xc595
      #xc597 #xc598 #xc59c #xc5a0 #xc5a9 #xc5b4 #xc5b5 #xc5b8
      #xc5b9 #xc5bb #xc5bc #xc5bd #xc5be #xc5c4 #xc5c5 #xc5c6
      #xc5c7 #xc5c8 #xc5c9 #xc5ca #xc5cc #xc5ce
      ;; #x3f
      #xc5d0 #xc5d1 #xc5d4 #xc5d8 #xc5e0 #xc5e1 #xc5e3 #xc5e5
      #xc5ec #xc5ed #xc5ee #xc5f0 #xc5f4 #xc5f6 #xc5f7 #xc5fc
      #xc5fd #xc5fe #xc5ff #xc600 #xc601 #xc605 #xc606 #xc607
      #xc608 #xc60c #xc610 #xc618 #xc619 #xc61b #xc61c #xc624
      #xc625 #xc628 #xc62c #xc62d #xc62e #xc630 #xc633 #xc634
      #xc635 #xc637 #xc639 #xc63b #xc640 #xc641 #xc644 #xc648
      #xc650 #xc651 #xc653 #xc654 #xc655 #xc65c #xc65d #xc660
      #xc66c #xc66f #xc671 #xc678 #xc679 #xc67c #xc680 #xc688
      #xc689 #xc68b #xc68d #xc694 #xc695 #xc698 #xc69c #xc6a4
      #xc6a5 #xc6a7 #xc6a9 #xc6b0 #xc6b1 #xc6b4 #xc6b8 #xc6b9
      #xc6ba #xc6c0 #xc6c1 #xc6c3 #xc6c5 #xc6cc #xc6cd #xc6d0
      #xc6d4 #xc6dc #xc6dd #xc6e0 #xc6e1 #xc6e8
      ;; #x40
      #xc6e9 #xc6ec #xc6f0 #xc6f8 #xc6f9 #xc6fd #xc704 #xc705
      #xc708 #xc70c #xc714 #xc715 #xc717 #xc719 #xc720 #xc721
      #xc724 #xc728 #xc730 #xc731 #xc733 #xc735 #xc737 #xc73c
      #xc73d #xc740 #xc744 #xc74a #xc74c #xc74d #xc74f #xc751
      #xc752 #xc753 #xc754 #xc755 #xc756 #xc757 #xc758 #xc75c
      #xc760 #xc768 #xc76b #xc774 #xc775 #xc778 #xc77c #xc77d
      #xc77e #xc783 #xc784 #xc785 #xc787 #xc788 #xc789 #xc78a
      #xc78e #xc790 #xc791 #xc794 #xc796 #xc797 #xc798 #xc79a
      #xc7a0 #xc7a1 #xc7a3 #xc7a4 #xc7a5 #xc7a6 #xc7ac #xc7ad
      #xc7b0 #xc7b4 #xc7bc #xc7bd #xc7bf #xc7c0 #xc7c1 #xc7c8
      #xc7c9 #xc7cc #xc7ce #xc7d0 #xc7d8 #xc7dd #xc7e4 #xc7e8
      #xc7ec #xc800 #xc801 #xc804 #xc808 #xc80a
      ;; #x41
      #xc810 #xc811 #xc813 #xc815 #xc816 #xc81c #xc81d #xc820
      #xc824 #xc82c #xc82d #xc82f #xc831 #xc838 #xc83c #xc840
      #xc848 #xc849 #xc84c #xc84d #xc854 #xc870 #xc871 #xc874
      #xc878 #xc87a #xc880 #xc881 #xc883 #xc885 #xc886 #xc887
      #xc88b #xc88c #xc88d #xc894 #xc89d #xc89f #xc8a1 #xc8a8
      #xc8bc #xc8bd #xc8c4 #xc8c8 #xc8cc #xc8d4 #xc8d5 #xc8d7
      #xc8d9 #xc8e0 #xc8e1 #xc8e4 #xc8f5 #xc8fc #xc8fd #xc900
      #xc904 #xc905 #xc906 #xc90c #xc90d #xc90f #xc911 #xc918
      #xc92c #xc934 #xc950 #xc951 #xc954 #xc958 #xc960 #xc961
      #xc963 #xc96c #xc970 #xc974 #xc97c #xc988 #xc989 #xc98c
      #xc990 #xc998 #xc999 #xc99b #xc99d #xc9c0 #xc9c1 #xc9c4
      #xc9c7 #xc9c8 #xc9ca #xc9d0 #xc9d1 #xc9d3
      ;; #x42
      #xc9d5 #xc9d6 #xc9d9 #xc9da #xc9dc #xc9dd #xc9e0 #xc9e2
      #xc9e4 #xc9e7 #xc9ec #xc9ed #xc9ef #xc9f0 #xc9f1 #xc9f8
      #xc9f9 #xc9fc #xca00 #xca08 #xca09 #xca0b #xca0c #xca0d
      #xca14 #xca18 #xca29 #xca4c #xca4d #xca50 #xca54 #xca5c
      #xca5d #xca5f #xca60 #xca61 #xca68 #xca7d #xca84 #xca98
      #xcabc #xcabd #xcac0 #xcac4 #xcacc #xcacd #xcacf #xcad1
      #xcad3 #xcad8 #xcad9 #xcae0 #xcaec #xcaf4 #xcb08 #xcb10
      #xcb14 #xcb18 #xcb20 #xcb21 #xcb41 #xcb48 #xcb49 #xcb4c
      #xcb50 #xcb58 #xcb59 #xcb5d #xcb64 #xcb78 #xcb79 #xcb9c
      #xcbb8 #xcbd4 #xcbe4 #xcbe7 #xcbe9 #xcc0c #xcc0d #xcc10
      #xcc14 #xcc1c #xcc1d #xcc21 #xcc22 #xcc27 #xcc28 #xcc29
      #xcc2c #xcc2e #xcc30 #xcc38 #xcc39 #xcc3b
      ;; #x43
      #xcc3c #xcc3d #xcc3e #xcc44 #xcc45 #xcc48 #xcc4c #xcc54
      #xcc55 #xcc57 #xcc58 #xcc59 #xcc60 #xcc64 #xcc66 #xcc68
      #xcc70 #xcc75 #xcc98 #xcc99 #xcc9c #xcca0 #xcca8 #xcca9
      #xccab #xccac #xccad #xccb4 #xccb5 #xccb8 #xccbc #xccc4
      #xccc5 #xccc7 #xccc9 #xccd0 #xccd4 #xcce4 #xccec #xccf0
      #xcd01 #xcd08 #xcd09 #xcd0c #xcd10 #xcd18 #xcd19 #xcd1b
      #xcd1d #xcd24 #xcd28 #xcd2c #xcd39 #xcd5c #xcd60 #xcd64
      #xcd6c #xcd6d #xcd6f #xcd71 #xcd78 #xcd88 #xcd94 #xcd95
      #xcd98 #xcd9c #xcda4 #xcda5 #xcda7 #xcda9 #xcdb0 #xcdc4
      #xcdcc #xcdd0 #xcde8 #xcdec #xcdf0 #xcdf8 #xcdf9 #xcdfb
      #xcdfd #xce04 #xce08 #xce0c #xce14 #xce19 #xce20 #xce21
      #xce24 #xce28 #xce30 #xce31 #xce33 #xce35
      ;; #x44
      #xce58 #xce59 #xce5c #xce5f #xce60 #xce61 #xce68 #xce69
      #xce6b #xce6d #xce74 #xce75 #xce78 #xce7c #xce84 #xce85
      #xce87 #xce89 #xce90 #xce91 #xce94 #xce98 #xcea0 #xcea1
      #xcea3 #xcea4 #xcea5 #xceac #xcead #xcec1 #xcee4 #xcee5
      #xcee8 #xceeb #xceec #xcef4 #xcef5 #xcef7 #xcef8 #xcef9
      #xcf00 #xcf01 #xcf04 #xcf08 #xcf10 #xcf11 #xcf13 #xcf15
      #xcf1c #xcf20 #xcf24 #xcf2c #xcf2d #xcf2f #xcf30 #xcf31
      #xcf38 #xcf54 #xcf55 #xcf58 #xcf5c #xcf64 #xcf65 #xcf67
      #xcf69 #xcf70 #xcf71 #xcf74 #xcf78 #xcf80 #xcf85 #xcf8c
      #xcfa1 #xcfa8 #xcfb0 #xcfc4 #xcfe0 #xcfe1 #xcfe4 #xcfe8
      #xcff0 #xcff1 #xcff3 #xcff5 #xcffc #xd000 #xd004 #xd011
      #xd018 #xd02d #xd034 #xd035 #xd038 #xd03c
      ;; #x45
      #xd044 #xd045 #xd047 #xd049 #xd050 #xd054 #xd058 #xd060
      #xd06c #xd06d #xd070 #xd074 #xd07c #xd07d #xd081 #xd0a4
      #xd0a5 #xd0a8 #xd0ac #xd0b4 #xd0b5 #xd0b7 #xd0b9 #xd0c0
      #xd0c1 #xd0c4 #xd0c8 #xd0c9 #xd0d0 #xd0d1 #xd0d3 #xd0d4
      #xd0d5 #xd0dc #xd0dd #xd0e0 #xd0e4 #xd0ec #xd0ed #xd0ef
      #xd0f0 #xd0f1 #xd0f8 #xd10d #xd130 #xd131 #xd134 #xd138
      #xd13a #xd140 #xd141 #xd143 #xd144 #xd145 #xd14c #xd14d
      #xd150 #xd154 #xd15c #xd15d #xd15f #xd161 #xd168 #xd16c
      #xd17c #xd184 #xd188 #xd1a0 #xd1a1 #xd1a4 #xd1a8 #xd1b0
      #xd1b1 #xd1b3 #xd1b5 #xd1ba #xd1bc #xd1c0 #xd1d8 #xd1f4
      #xd1f8 #xd207 #xd209 #xd210 #xd22c #xd22d #xd230 #xd234
      #xd23c #xd23d #xd23f #xd241 #xd248 #xd25c
      ;; #x46
      #xd264 #xd280 #xd281 #xd284 #xd288 #xd290 #xd291 #xd295
      #xd29c #xd2a0 #xd2a4 #xd2ac #xd2b1 #xd2b8 #xd2b9 #xd2bc
      #xd2bf #xd2c0 #xd2c2 #xd2c8 #xd2c9 #xd2cb #xd2d4 #xd2d8
      #xd2dc #xd2e4 #xd2e5 #xd2f0 #xd2f1 #xd2f4 #xd2f8 #xd300
      #xd301 #xd303 #xd305 #xd30c #xd30d #xd30e #xd310 #xd314
      #xd316 #xd31c #xd31d #xd31f #xd320 #xd321 #xd325 #xd328
      #xd329 #xd32c #xd330 #xd338 #xd339 #xd33b #xd33c #xd33d
      #xd344 #xd345 #xd37c #xd37d #xd380 #xd384 #xd38c #xd38d
      #xd38f #xd390 #xd391 #xd398 #xd399 #xd39c #xd3a0 #xd3a8
      #xd3a9 #xd3ab #xd3ad #xd3b4 #xd3b8 #xd3bc #xd3c4 #xd3c5
      #xd3c8 #xd3c9 #xd3d0 #xd3d8 #xd3e1 #xd3e3 #xd3ec #xd3ed
      #xd3f0 #xd3f4 #xd3fc #xd3fd #xd3ff #xd401
      ;; #x47
      #xd408 #xd41d #xd440 #xd444 #xd45c #xd460 #xd464 #xd46d
      #xd46f #xd478 #xd479 #xd47c #xd47f #xd480 #xd482 #xd488
      #xd489 #xd48b #xd48d #xd494 #xd4a9 #xd4cc #xd4d0 #xd4d4
      #xd4dc #xd4df #xd4e8 #xd4ec #xd4f0 #xd4f8 #xd4fb #xd4fd
      #xd504 #xd508 #xd50c #xd514 #xd515 #xd517 #xd53c #xd53d
      #xd540 #xd544 #xd54c #xd54d #xd54f #xd551 #xd558 #xd559
      #xd55c #xd560 #xd565 #xd568 #xd569 #xd56b #xd56d #xd574
      #xd575 #xd578 #xd57c #xd584 #xd585 #xd587 #xd588 #xd589
      #xd590 #xd5a5 #xd5c8 #xd5c9 #xd5cc #xd5d0 #xd5d2 #xd5d8
      #xd5d9 #xd5db #xd5dd #xd5e4 #xd5e5 #xd5e8 #xd5ec #xd5f4
      #xd5f5 #xd5f7 #xd5f9 #xd600 #xd601 #xd604 #xd608 #xd610
      #xd611 #xd613 #xd614 #xd615 #xd61c #xd620
      ;; #x48
      #xd624 #xd62d #xd638 #xd639 #xd63c #xd640 #xd645 #xd648
      #xd649 #xd64b #xd64d #xd651 #xd654 #xd655 #xd658 #xd65c
      #xd667 #xd669 #xd670 #xd671 #xd674 #xd683 #xd685 #xd68c
      #xd68d #xd690 #xd694 #xd69d #xd69f #xd6a1 #xd6a8 #xd6ac
      #xd6b0 #xd6b9 #xd6bb #xd6c4 #xd6c5 #xd6c8 #xd6cc #xd6d1
      #xd6d4 #xd6d7 #xd6d9 #xd6e0 #xd6e4 #xd6e8 #xd6f0 #xd6f5
      #xd6fc #xd6fd #xd700 #xd704 #xd711 #xd718 #xd719 #xd71c
      #xd720 #xd728 #xd729 #xd72b #xd72d #xd734 #xd735 #xd738
      #xd73c #xd744 #xd747 #xd749 #xd750 #xd751 #xd754 #xd756
      #xd757 #xd758 #xd759 #xd760 #xd761 #xd763 #xd765 #xd769
      #xd76c #xd770 #xd774 #xd77c #xd77d #xd781 #xd788 #xd789
      #xd78c #xd790 #xd798 #xd799 #xd79b #xd79d
      )
  :test #'equalp)

(define-constant +ksc-5601-4a-7d-to-unicode+
    #(
      ;; #x4a
      #x4f3d #x4f73 #x5047 #x50f9 #x52a0 #x53ef #x5475 #x54e5
      #x5609 #x5ac1 #x5bb6 #x6687 #x67b6 #x67b7 #x67ef #x6b4c
      #x73c2 #x75c2 #x7a3c #x82db #x8304 #x8857 #x8888 #x8a36
      #x8cc8 #x8dcf #x8efb #x8fe6 #x99d5 #x523b #x5374 #x5404
      #x606a #x6164 #x6bbc #x73cf #x811a #x89ba #x89d2 #x95a3
      #x4f83 #x520a #x58be #x5978 #x59e6 #x5e72 #x5e79 #x61c7
      #x63c0 #x6746 #x67ec #x687f #x6f97 #x764e #x770b #x78f5
      #x7a08 #x7aff #x7c21 #x809d #x826e #x8271 #x8aeb #x9593
      #x4e6b #x559d #x66f7 #x6e34 #x78a3 #x7aed #x845b #x8910
      #x874e #x97a8 #x52d8 #x574e #x582a #x5d4c #x611f #x61be
      #x6221 #x6562 #x67d1 #x6a44 #x6e1b #x7518 #x75b3 #x76e3
      #x77b0 #x7d3a #x90af #x9451 #x9452 #x9f95
      ;; #x4b
      #x5323 #x5cac #x7532 #x80db #x9240 #x9598 #x525b #x5808
      #x59dc #x5ca1 #x5d17 #x5eb7 #x5f3a #x5f4a #x6177 #x6c5f
      #x757a #x7586 #x7ce0 #x7d73 #x7db1 #x7f8c #x8154 #x8221
      #x8591 #x8941 #x8b1b #x92fc #x964d #x9c47 #x4ecb #x4ef7
      #x500b #x51f1 #x584f #x6137 #x613e #x6168 #x6539 #x69ea
      #x6f11 #x75a5 #x7686 #x76d6 #x7b87 #x82a5 #x84cb #xf900
      #x93a7 #x958b #x5580 #x5ba2 #x5751 #xf901 #x7cb3 #x7fb9
      #x91b5 #x5028 #x53bb #x5c45 #x5de8 #x62d2 #x636e #x64da
      #x64e7 #x6e20 #x70ac #x795b #x8ddd #x8e1e #xf902 #x907d
      #x9245 #x92f8 #x4e7e #x4ef6 #x5065 #x5dfe #x5efa #x6106
      #x6957 #x8171 #x8654 #x8e47 #x9375 #x9a2b #x4e5e #x5091
      #x6770 #x6840 #x5109 #x528d #x5292 #x6aa2
      ;; #x4c
      #x77bc #x9210 #x9ed4 #x52ab #x602f #x8ff2 #x5048 #x61a9
      #x63ed #x64ca #x683c #x6a84 #x6fc0 #x8188 #x89a1 #x9694
      #x5805 #x727d #x72ac #x7504 #x7d79 #x7e6d #x80a9 #x898b
      #x8b74 #x9063 #x9d51 #x6289 #x6c7a #x6f54 #x7d50 #x7f3a
      #x8a23 #x517c #x614a #x7b9d #x8b19 #x9257 #x938c #x4eac
      #x4fd3 #x501e #x50be #x5106 #x52c1 #x52cd #x537f #x5770
      #x5883 #x5e9a #x5f91 #x6176 #x61ac #x64ce #x656c #x666f
      #x66bb #x66f4 #x6897 #x6d87 #x7085 #x70f1 #x749f #x74a5
      #x74ca #x75d9 #x786c #x78ec #x7adf #x7af6 #x7d45 #x7d93
      #x8015 #x803f #x811b #x8396 #x8b66 #x8f15 #x9015 #x93e1
      #x9803 #x9838 #x9a5a #x9be8 #x4fc2 #x5553 #x583a #x5951
      #x5b63 #x5c46 #x60b8 #x6212 #x6842 #x68b0
      ;; #x4d
      #x68e8 #x6eaa #x754c #x7678 #x78ce #x7a3d #x7cfb #x7e6b
      #x7e7c #x8a08 #x8aa1 #x8c3f #x968e #x9dc4 #x53e4 #x53e9
      #x544a #x5471 #x56fa #x59d1 #x5b64 #x5c3b #x5eab #x62f7
      #x6537 #x6545 #x6572 #x66a0 #x67af #x69c1 #x6cbd #x75fc
      #x7690 #x777e #x7a3f #x7f94 #x8003 #x80a1 #x818f #x82e6
      #x82fd #x83f0 #x85c1 #x8831 #x88b4 #x8aa5 #xf903 #x8f9c
      #x932e #x96c7 #x9867 #x9ad8 #x9f13 #x54ed #x659b #x66f2
      #x688f #x7a40 #x8c37 #x9d60 #x56f0 #x5764 #x5d11 #x6606
      #x68b1 #x68cd #x6efe #x7428 #x889e #x9be4 #x6c68 #xf904
      #x9aa8 #x4f9b #x516c #x5171 #x529f #x5b54 #x5de5 #x6050
      #x606d #x62f1 #x63a7 #x653b #x73d9 #x7a7a #x86a3 #x8ca2
      #x978f #x4e32 #x5be1 #x6208 #x679c #x74dc
      ;; #x4e
      #x79d1 #x83d3 #x8a87 #x8ab2 #x8de8 #x904e #x934b #x9846
      #x5ed3 #x69e8 #x85ff #x90ed #xf905 #x51a0 #x5b98 #x5bec
      #x6163 #x68fa #x6b3e #x704c #x742f #x74d8 #x7ba1 #x7f50
      #x83c5 #x89c0 #x8cab #x95dc #x9928 #x522e #x605d #x62ec
      #x9002 #x4f8a #x5149 #x5321 #x58d9 #x5ee3 #x66e0 #x6d38
      #x709a #x72c2 #x73d6 #x7b50 #x80f1 #x945b #x5366 #x639b
      #x7f6b #x4e56 #x5080 #x584a #x58de #x602a #x6127 #x62d0
      #x69d0 #x9b41 #x5b8f #x7d18 #x80b1 #x8f5f #x4ea4 #x50d1
      #x54ac #x55ac #x5b0c #x5da0 #x5de7 #x652a #x654e #x6821
      #x6a4b #x72e1 #x768e #x77ef #x7d5e #x7ff9 #x81a0 #x854e
      #x86df #x8f03 #x8f4e #x90ca #x9903 #x9a55 #x9bab #x4e18
      #x4e45 #x4e5d #x4ec7 #x4ff1 #x5177 #x52fe
      ;; #x4f
      #x5340 #x53e3 #x53e5 #x548e #x5614 #x5775 #x57a2 #x5bc7
      #x5d87 #x5ed0 #x61fc #x62d8 #x6551 #x67b8 #x67e9 #x69cb
      #x6b50 #x6bc6 #x6bec #x6c42 #x6e9d #x7078 #x72d7 #x7396
      #x7403 #x77bf #x77e9 #x7a76 #x7d7f #x8009 #x81fc #x8205
      #x820a #x82df #x8862 #x8b33 #x8cfc #x8ec0 #x9011 #x90b1
      #x9264 #x92b6 #x99d2 #x9a45 #x9ce9 #x9dd7 #x9f9c #x570b
      #x5c40 #x83ca #x97a0 #x97ab #x9eb4 #x541b #x7a98 #x7fa4
      #x88d9 #x8ecd #x90e1 #x5800 #x5c48 #x6398 #x7a9f #x5bae
      #x5f13 #x7a79 #x7aae #x828e #x8eac #x5026 #x5238 #x52f8
      #x5377 #x5708 #x62f3 #x6372 #x6b0a #x6dc3 #x7737 #x53a5
      #x7357 #x8568 #x8e76 #x95d5 #x673a #x6ac3 #x6f70 #x8a6d
      #x8ecc #x994b #xf906 #x6677 #x6b78 #x8cb4
      ;; #x50
      #x9b3c #xf907 #x53eb #x572d #x594e #x63c6 #x69fb #x73ea
      #x7845 #x7aba #x7ac5 #x7cfe #x8475 #x898f #x8d73 #x9035
      #x95a8 #x52fb #x5747 #x7547 #x7b60 #x83cc #x921e #xf908
      #x6a58 #x514b #x524b #x5287 #x621f #x68d8 #x6975 #x9699
      #x50c5 #x52a4 #x52e4 #x61c3 #x65a4 #x6839 #x69ff #x747e
      #x7b4b #x82b9 #x83eb #x89b2 #x8b39 #x8fd1 #x9949 #xf909
      #x4eca #x5997 #x64d2 #x6611 #x6a8e #x7434 #x7981 #x79bd
      #x82a9 #x887e #x887f #x895f #xf90a #x9326 #x4f0b #x53ca
      #x6025 #x6271 #x6c72 #x7d1a #x7d66 #x4e98 #x5162 #x77dc
      #x80af #x4f01 #x4f0e #x5176 #x5180 #x55dc #x5668 #x573b
      #x57fa #x57fc #x5914 #x5947 #x5993 #x5bc4 #x5c90 #x5d0e
      #x5df1 #x5e7e #x5fcc #x6280 #x65d7 #x65e3
      ;; #x51
      #x671e #x671f #x675e #x68cb #x68c4 #x6a5f #x6b3a #x6c23
      #x6c7d #x6c82 #x6dc7 #x7398 #x7426 #x742a #x7482 #x74a3
      #x7578 #x757f #x7881 #x78ef #x7941 #x7947 #x7948 #x797a
      #x7b95 #x7d00 #x7dba #x7f88 #x8006 #x802d #x808c #x8a18
      #x8b4f #x8c48 #x8d77 #x9321 #x9324 #x98e2 #x9951 #x9a0e
      #x9a0f #x9a65 #x9e92 #x7dca #x4f76 #x5409 #x62ee #x6854
      #x91d1 #x55ab #x513a #xf90b #xf90c #x5a1c #x61e6 #xf90d
      #x62cf #x62ff #xf90e #xf90f #xf910 #xf911 #xf912 #xf913
      #x90a3 #xf914 #xf915 #xf916 #xf917 #xf918 #x8afe #xf919
      #xf91a #xf91b #xf91c #x6696 #xf91d #x7156 #xf91e #xf91f
      #x96e3 #xf920 #x634f #x637a #x5357 #xf921 #x678f #x6960
      #x6e73 #xf922 #x7537 #xf923 #xf924 #xf925
      ;; #x52
      #x7d0d #xf926 #xf927 #x8872 #x56ca #x5a18 #xf928 #xf929
      #xf92a #xf92b #xf92c #x4e43 #xf92d #x5167 #x5948 #x67f0
      #x8010 #xf92e #x5973 #x5e74 #x649a #x79ca #x5ff5 #x606c
      #x62c8 #x637b #x5be7 #x5bd7 #x52aa #xf92f #x5974 #x5f29
      #x6012 #xf930 #xf931 #xf932 #x7459 #xf933 #xf934 #xf935
      #xf936 #xf937 #xf938 #x99d1 #xf939 #xf93a #xf93b #xf93c
      #xf93d #xf93e #xf93f #xf940 #xf941 #xf942 #xf943 #x6fc3
      #xf944 #xf945 #x81bf #x8fb2 #x60f1 #xf946 #xf947 #x8166
      #xf948 #xf949 #x5c3f #xf94a #xf94b #xf94c #xf94d #xf94e
      #xf94f #xf950 #xf951 #x5ae9 #x8a25 #x677b #x7d10 #xf952
      #xf953 #xf954 #xf955 #xf956 #xf957 #x80fd #xf958 #xf959
      #x5c3c #x6ce5 #x533f #x6eba #x591a #x8336
      ;; #x53
      #x4e39 #x4eb6 #x4f46 #x55ae #x5718 #x58c7 #x5f56 #x65b7
      #x65e6 #x6a80 #x6bb5 #x6e4d #x77ed #x7aef #x7c1e #x7dde
      #x86cb #x8892 #x9132 #x935b #x64bb #x6fbe #x737a #x75b8
      #x9054 #x5556 #x574d #x61ba #x64d4 #x66c7 #x6de1 #x6e5b
      #x6f6d #x6fb9 #x75f0 #x8043 #x81bd #x8541 #x8983 #x8ac7
      #x8b5a #x931f #x6c93 #x7553 #x7b54 #x8e0f #x905d #x5510
      #x5802 #x5858 #x5e62 #x6207 #x649e #x68e0 #x7576 #x7cd6
      #x87b3 #x9ee8 #x4ee3 #x5788 #x576e #x5927 #x5c0d #x5cb1
      #x5e36 #x5f85 #x6234 #x64e1 #x73b3 #x81fa #x888b #x8cb8
      #x968a #x9edb #x5b85 #x5fb7 #x60b3 #x5012 #x5200 #x5230
      #x5716 #x5835 #x5857 #x5c0e #x5c60 #x5cf6 #x5d8b #x5ea6
      #x5f92 #x60bc #x6311 #x6389 #x6417 #x6843
      ;; #x54
      #x68f9 #x6ac2 #x6dd8 #x6e21 #x6ed4 #x6fe4 #x71fe #x76dc
      #x7779 #x79b1 #x7a3b #x8404 #x89a9 #x8ced #x8df3 #x8e48
      #x9003 #x9014 #x9053 #x90fd #x934d #x9676 #x97dc #x6bd2
      #x7006 #x7258 #x72a2 #x7368 #x7763 #x79bf #x7be4 #x7e9b
      #x8b80 #x58a9 #x60c7 #x6566 #x65fd #x66be #x6c8c #x711e
      #x71c9 #x8c5a #x9813 #x4e6d #x7a81 #x4edd #x51ac #x51cd
      #x52d5 #x540c #x61a7 #x6771 #x6850 #x68df #x6d1e #x6f7c
      #x75bc #x77b3 #x7ae5 #x80f4 #x8463 #x9285 #x515c #x6597
      #x675c #x6793 #x75d8 #x7ac7 #x8373 #xf95a #x8c46 #x9017
      #x982d #x5c6f #x81c0 #x829a #x9041 #x906f #x920d #x5f97
      #x5d9d #x6a59 #x71c8 #x767b #x7b49 #x85e4 #x8b04 #x9127
      #x9a30 #x5587 #x61f6 #xf95b #x7669 #x7f85
      ;; #x55
      #x863f #x87ba #x88f8 #x908f #xf95c #x6d1b #x70d9 #x73de
      #x7d61 #x843d #xf95d #x916a #x99f1 #xf95e #x4e82 #x5375
      #x6b04 #x6b12 #x703e #x721b #x862d #x9e1e #x524c #x8fa3
      #x5d50 #x64e5 #x652c #x6b16 #x6feb #x7c43 #x7e9c #x85cd
      #x8964 #x89bd #x62c9 #x81d8 #x881f #x5eca #x6717 #x6d6a
      #x72fc #x7405 #x746f #x8782 #x90de #x4f86 #x5d0d #x5fa0
      #x840a #x51b7 #x63a0 #x7565 #x4eae #x5006 #x5169 #x51c9
      #x6881 #x6a11 #x7cae #x7cb1 #x7ce7 #x826f #x8ad2 #x8f1b
      #x91cf #x4fb6 #x5137 #x52f5 #x5442 #x5eec #x616e #x623e
      #x65c5 #x6ada #x6ffe #x792a #x85dc #x8823 #x95ad #x9a62
      #x9a6a #x9e97 #x9ece #x529b #x66c6 #x6b77 #x701d #x792b
      #x8f62 #x9742 #x6190 #x6200 #x6523 #x6f23
      ;; #x56
      #x7149 #x7489 #x7df4 #x806f #x84ee #x8f26 #x9023 #x934a
      #x51bd #x5217 #x52a3 #x6d0c #x70c8 #x88c2 #x5ec9 #x6582
      #x6bae #x6fc2 #x7c3e #x7375 #x4ee4 #x4f36 #x56f9 #xf95f
      #x5cba #x5dba #x601c #x73b2 #x7b2d #x7f9a #x7fce #x8046
      #x901e #x9234 #x96f6 #x9748 #x9818 #x9f61 #x4f8b #x6fa7
      #x79ae #x91b4 #x96b7 #x52de #xf960 #x6488 #x64c4 #x6ad3
      #x6f5e #x7018 #x7210 #x76e7 #x8001 #x8606 #x865c #x8def
      #x8f05 #x9732 #x9b6f #x9dfa #x9e75 #x788c #x797f #x7da0
      #x83c9 #x9304 #x9e7f #x9e93 #x8ad6 #x58df #x5f04 #x6727
      #x7027 #x74cf #x7c60 #x807e #x5121 #x7028 #x7262 #x78ca
      #x8cc2 #x8cda #x8cf4 #x96f7 #x4e86 #x50da #x5bee #x5ed6
      #x6599 #x71ce #x7642 #x77ad #x804a #x84fc
      ;; #x57
      #x907c #x9b27 #x9f8d #x58d8 #x5a41 #x5c62 #x6a13 #x6dda
      #x6f0f #x763b #x7d2f #x7e37 #x851e #x8938 #x93e4 #x964b
      #x5289 #x65d2 #x67f3 #x69b4 #x6d41 #x6e9c #x700f #x7409
      #x7460 #x7559 #x7624 #x786b #x8b2c #x985e #x516d #x622e
      #x9678 #x4f96 #x502b #x5d19 #x6dea #x7db8 #x8f2a #x5f8b
      #x6144 #x6817 #xf961 #x9686 #x52d2 #x808b #x51dc #x51cc
      #x695e #x7a1c #x7dbe #x83f1 #x9675 #x4fda #x5229 #x5398
      #x540f #x550e #x5c65 #x60a7 #x674e #x68a8 #x6d6c #x7281
      #x72f8 #x7406 #x7483 #xf962 #x75e2 #x7c6c #x7f79 #x7fb8
      #x8389 #x88cf #x88e1 #x91cc #x91d0 #x96e2 #x9bc9 #x541d
      #x6f7e #x71d0 #x7498 #x85fa #x8eaa #x96a3 #x9c57 #x9e9f
      #x6797 #x6dcb #x7433 #x81e8 #x9716 #x782c
      ;; #x58
      #x7acb #x7b20 #x7c92 #x6469 #x746a #x75f2 #x78bc #x78e8
      #x99ac #x9b54 #x9ebb #x5bde #x5e55 #x6f20 #x819c #x83ab
      #x9088 #x4e07 #x534d #x5a29 #x5dd2 #x5f4e #x6162 #x633d
      #x6669 #x66fc #x6eff #x6f2b #x7063 #x779e #x842c #x8513
      #x883b #x8f13 #x9945 #x9c3b #x551c #x62b9 #x672b #x6cab
      #x8309 #x896a #x977a #x4ea1 #x5984 #x5fd8 #x5fd9 #x671b
      #x7db2 #x7f54 #x8292 #x832b #x83bd #x8f1e #x9099 #x57cb
      #x59b9 #x5a92 #x5bd0 #x6627 #x679a #x6885 #x6bcf #x7164
      #x7f75 #x8cb7 #x8ce3 #x9081 #x9b45 #x8108 #x8c8a #x964c
      #x9a40 #x9ea5 #x5b5f #x6c13 #x731b #x76f2 #x76df #x840c
      #x51aa #x8993 #x514d #x5195 #x52c9 #x68c9 #x6c94 #x7704
      #x7720 #x7dbf #x7dec #x9762 #x9eb5 #x6ec5
      ;; #x59
      #x8511 #x51a5 #x540d #x547d #x660e #x669d #x6927 #x6e9f
      #x76bf #x7791 #x8317 #x84c2 #x879f #x9169 #x9298 #x9cf4
      #x8882 #x4fae #x5192 #x52df #x59c6 #x5e3d #x6155 #x6478
      #x6479 #x66ae #x67d0 #x6a21 #x6bcd #x6bdb #x725f #x7261
      #x7441 #x7738 #x77db #x8017 #x82bc #x8305 #x8b00 #x8b28
      #x8c8c #x6728 #x6c90 #x7267 #x76ee #x7766 #x7a46 #x9da9
      #x6b7f #x6c92 #x5922 #x6726 #x8499 #x536f #x5893 #x5999
      #x5edf #x63cf #x6634 #x6773 #x6e3a #x732b #x7ad7 #x82d7
      #x9328 #x52d9 #x5deb #x61ae #x61cb #x620a #x62c7 #x64ab
      #x65e0 #x6959 #x6b66 #x6bcb #x7121 #x73f7 #x755d #x7e46
      #x821e #x8302 #x856a #x8aa3 #x8cbf #x9727 #x9d61 #x58a8
      #x9ed8 #x5011 #x520e #x543b #x554f #x6587
      ;; #x5a
      #x6c76 #x7d0a #x7d0b #x805e #x868a #x9580 #x96ef #x52ff
      #x6c95 #x7269 #x5473 #x5a9a #x5c3e #x5d4b #x5f4c #x5fae
      #x672a #x68b6 #x6963 #x6e3c #x6e44 #x7709 #x7c73 #x7f8e
      #x8587 #x8b0e #x8ff7 #x9761 #x9ef4 #x5cb7 #x60b6 #x610d
      #x61ab #x654f #x65fb #x65fc #x6c11 #x6cef #x739f #x73c9
      #x7de1 #x9594 #x5bc6 #x871c #x8b10 #x525d #x535a #x62cd
      #x640f #x64b2 #x6734 #x6a38 #x6cca #x73c0 #x749e #x7b94
      #x7c95 #x7e1b #x818a #x8236 #x8584 #x8feb #x96f9 #x99c1
      #x4f34 #x534a #x53cd #x53db #x62cc #x642c #x6500 #x6591
      #x69c3 #x6cee #x6f58 #x73ed #x7554 #x7622 #x76e4 #x76fc
      #x78d0 #x78fb #x792c #x7d46 #x822c #x87e0 #x8fd4 #x9812
      #x98ef #x52c3 #x62d4 #x64a5 #x6e24 #x6f51
      ;; #x5b
      #x767c #x8dcb #x91b1 #x9262 #x9aee #x9b43 #x5023 #x508d
      #x574a #x59a8 #x5c28 #x5e47 #x5f77 #x623f #x653e #x65b9
      #x65c1 #x6609 #x678b #x699c #x6ec2 #x78c5 #x7d21 #x80aa
      #x8180 #x822b #x82b3 #x84a1 #x868c #x8a2a #x8b17 #x90a6
      #x9632 #x9f90 #x500d #x4ff3 #xf963 #x57f9 #x5f98 #x62dc
      #x6392 #x676f #x6e43 #x7119 #x76c3 #x80cc #x80da #x88f4
      #x88f5 #x8919 #x8ce0 #x8f29 #x914d #x966a #x4f2f #x4f70
      #x5e1b #x67cf #x6822 #x767d #x767e #x9b44 #x5e61 #x6a0a
      #x7169 #x71d4 #x756a #xf964 #x7e41 #x8543 #x85e9 #x98dc
      #x4f10 #x7b4f #x7f70 #x95a5 #x51e1 #x5e06 #x68b5 #x6c3e
      #x6c4e #x6cdb #x72af #x7bc4 #x8303 #x6cd5 #x743a #x50fb
      #x5288 #x58c1 #x64d8 #x6a97 #x74a7 #x7656
      ;; #x5c
      #x78a7 #x8617 #x95e2 #x9739 #xf965 #x535e #x5f01 #x8b8a
      #x8fa8 #x8faf #x908a #x5225 #x77a5 #x9c49 #x9f08 #x4e19
      #x5002 #x5175 #x5c5b #x5e77 #x661e #x663a #x67c4 #x68c5
      #x70b3 #x7501 #x75c5 #x79c9 #x7add #x8f27 #x9920 #x9a08
      #x4fdd #x5821 #x5831 #x5bf6 #x666e #x6b65 #x6d11 #x6e7a
      #x6f7d #x73e4 #x752b #x83e9 #x88dc #x8913 #x8b5c #x8f14
      #x4f0f #x50d5 #x5310 #x535c #x5b93 #x5fa9 #x670d #x798f
      #x8179 #x832f #x8514 #x8907 #x8986 #x8f39 #x8f3b #x99a5
      #x9c12 #x672c #x4e76 #x4ff8 #x5949 #x5c01 #x5cef #x5cf0
      #x6367 #x68d2 #x70fd #x71a2 #x742b #x7e2b #x84ec #x8702
      #x9022 #x92d2 #x9cf3 #x4e0d #x4ed8 #x4fef #x5085 #x5256
      #x526f #x5426 #x5490 #x57e0 #x592b #x5a66
      ;; #x5d
      #x5b5a #x5b75 #x5bcc #x5e9c #xf966 #x6276 #x6577 #x65a7
      #x6d6e #x6ea5 #x7236 #x7b26 #x7c3f #x7f36 #x8150 #x8151
      #x819a #x8240 #x8299 #x83a9 #x8a03 #x8ca0 #x8ce6 #x8cfb
      #x8d74 #x8dba #x90e8 #x91dc #x961c #x9644 #x99d9 #x9ce7
      #x5317 #x5206 #x5429 #x5674 #x58b3 #x5954 #x596e #x5fff
      #x61a4 #x626e #x6610 #x6c7e #x711a #x76c6 #x7c89 #x7cde
      #x7d1b #x82ac #x8cc1 #x96f0 #xf967 #x4f5b #x5f17 #x5f7f
      #x62c2 #x5d29 #x670b #x68da #x787c #x7e43 #x9d6c #x4e15
      #x5099 #x5315 #x532a #x5351 #x5983 #x5a62 #x5e87 #x60b2
      #x618a #x6249 #x6279 #x6590 #x6787 #x69a7 #x6bd4 #x6bd6
      #x6bd7 #x6bd8 #x6cb8 #xf968 #x7435 #x75fa #x7812 #x7891
      #x79d5 #x79d8 #x7c83 #x7dcb #x7fe1 #x80a5
      ;; #x5e
      #x813e #x81c2 #x83f2 #x871a #x88e8 #x8ab9 #x8b6c #x8cbb
      #x9119 #x975e #x98db #x9f3b #x56ac #x5b2a #x5f6c #x658c
      #x6ab3 #x6baf #x6d5c #x6ff1 #x7015 #x725d #x73ad #x8ca7
      #x8cd3 #x983b #x6191 #x6c37 #x8058 #x9a01 #x4e4d #x4e8b
      #x4e9b #x4ed5 #x4f3a #x4f3c #x4f7f #x4fdf #x50ff #x53f2
      #x53f8 #x5506 #x55e3 #x56db #x58eb #x5962 #x5a11 #x5beb
      #x5bfa #x5c04 #x5df3 #x5e2b #x5f99 #x601d #x6368 #x659c
      #x65af #x67f6 #x67fb #x68ad #x6b7b #x6c99 #x6cd7 #x6e23
      #x7009 #x7345 #x7802 #x793e #x7940 #x7960 #x79c1 #x7be9
      #x7d17 #x7d72 #x8086 #x820d #x838e #x84d1 #x86c7 #x88df
      #x8a50 #x8a5e #x8b1d #x8cdc #x8d66 #x8fad #x90aa #x98fc
      #x99df #x9e9d #x524a #xf969 #x6714 #xf96a
      ;; #x5f
      #x5098 #x522a #x5c71 #x6563 #x6c55 #x73ca #x7523 #x759d
      #x7b97 #x849c #x9178 #x9730 #x4e77 #x6492 #x6bba #x715e
      #x85a9 #x4e09 #xf96b #x6749 #x68ee #x6e17 #x829f #x8518
      #x886b #x63f7 #x6f81 #x9212 #x98af #x4e0a #x50b7 #x50cf
      #x511f #x5546 #x55aa #x5617 #x5b40 #x5c19 #x5ce0 #x5e38
      #x5e8a #x5ea0 #x5ec2 #x60f3 #x6851 #x6a61 #x6e58 #x723d
      #x7240 #x72c0 #x76f8 #x7965 #x7bb1 #x7fd4 #x88f3 #x89f4
      #x8a73 #x8c61 #x8cde #x971c #x585e #x74bd #x8cfd #x55c7
      #xf96c #x7a61 #x7d22 #x8272 #x7272 #x751f #x7525 #xf96d
      #x7b19 #x5885 #x58fb #x5dbc #x5e8f #x5eb6 #x5f90 #x6055
      #x6292 #x637f #x654d #x6691 #x66d9 #x66f8 #x6816 #x68f2
      #x7280 #x745e #x7b6e #x7d6e #x7dd6 #x7f72
      ;; #x60
      #x80e5 #x8212 #x85af #x897f #x8a93 #x901d #x92e4 #x9ecd
      #x9f20 #x5915 #x596d #x5e2d #x60dc #x6614 #x6673 #x6790
      #x6c50 #x6dc5 #x6f5f #x77f3 #x78a9 #x84c6 #x91cb #x932b
      #x4ed9 #x50ca #x5148 #x5584 #x5b0b #x5ba3 #x6247 #x657e
      #x65cb #x6e32 #x717d #x7401 #x7444 #x7487 #x74bf #x766c
      #x79aa #x7dda #x7e55 #x7fa8 #x817a #x81b3 #x8239 #x861a
      #x87ec #x8a75 #x8de3 #x9078 #x9291 #x9425 #x994d #x9bae
      #x5368 #x5c51 #x6954 #x6cc4 #x6d29 #x6e2b #x820c #x859b
      #x893b #x8a2d #x8aaa #x96ea #x9f67 #x5261 #x66b9 #x6bb2
      #x7e96 #x87fe #x8d0d #x9583 #x965d #x651d #x6d89 #x71ee
      #xf96e #x57ce #x59d3 #x5bac #x6027 #x60fa #x6210 #x661f
      #x665f #x7329 #x73f9 #x76db #x7701 #x7b6c
      ;; #x61
      #x8056 #x8072 #x8165 #x8aa0 #x9192 #x4e16 #x52e2 #x6b72
      #x6d17 #x7a05 #x7b39 #x7d30 #xf96f #x8cb0 #x53ec #x562f
      #x5851 #x5bb5 #x5c0f #x5c11 #x5de2 #x6240 #x6383 #x6414
      #x662d #x68b3 #x6cbc #x6d88 #x6eaf #x701f #x70a4 #x71d2
      #x7526 #x758f #x758e #x7619 #x7b11 #x7be0 #x7c2b #x7d20
      #x7d39 #x852c #x856d #x8607 #x8a34 #x900d #x9061 #x90b5
      #x92b7 #x97f6 #x9a37 #x4fd7 #x5c6c #x675f #x6d91 #x7c9f
      #x7e8c #x8b16 #x8d16 #x901f #x5b6b #x5dfd #x640d #x84c0
      #x905c #x98e1 #x7387 #x5b8b #x609a #x677e #x6dde #x8a1f
      #x8aa6 #x9001 #x980c #x5237 #xf970 #x7051 #x788e #x9396
      #x8870 #x91d7 #x4fee #x53d7 #x55fd #x56da #x5782 #x58fd
      #x5ac2 #x5b88 #x5cab #x5cc0 #x5e25 #x6101
      ;; #x62
      #x620d #x624b #x6388 #x641c #x6536 #x6578 #x6a39 #x6b8a
      #x6c34 #x6d19 #x6f31 #x71e7 #x72e9 #x7378 #x7407 #x74b2
      #x7626 #x7761 #x79c0 #x7a57 #x7aea #x7cb9 #x7d8f #x7dac
      #x7e61 #x7f9e #x8129 #x8331 #x8490 #x84da #x85ea #x8896
      #x8ab0 #x8b90 #x8f38 #x9042 #x9083 #x916c #x9296 #x92b9
      #x968b #x96a7 #x96a8 #x96d6 #x9700 #x9808 #x9996 #x9ad3
      #x9b1a #x53d4 #x587e #x5919 #x5b70 #x5bbf #x6dd1 #x6f5a
      #x719f #x7421 #x74b9 #x8085 #x83fd #x5de1 #x5f87 #x5faa
      #x6042 #x65ec #x6812 #x696f #x6a53 #x6b89 #x6d35 #x6df3
      #x73e3 #x76fe #x77ac #x7b4d #x7d14 #x8123 #x821c #x8340
      #x84f4 #x8563 #x8a62 #x8ac4 #x9187 #x931e #x9806 #x99b4
      #x620c #x8853 #x8ff0 #x9265 #x5d07 #x5d27
      ;; #x63
      #x5d69 #x745f #x819d #x8768 #x6fd5 #x62fe #x7fd2 #x8936
      #x8972 #x4e1e #x4e58 #x50e7 #x52dd #x5347 #x627f #x6607
      #x7e69 #x8805 #x965e #x4f8d #x5319 #x5636 #x59cb #x5aa4
      #x5c38 #x5c4e #x5c4d #x5e02 #x5f11 #x6043 #x65bd #x662f
      #x6642 #x67be #x67f4 #x731c #x77e2 #x793a #x7fc5 #x8494
      #x84cd #x8996 #x8a66 #x8a69 #x8ae1 #x8c55 #x8c7a #x57f4
      #x5bd4 #x5f0f #x606f #x62ed #x690d #x6b96 #x6e5c #x7184
      #x7bd2 #x8755 #x8b58 #x8efe #x98df #x98fe #x4f38 #x4f81
      #x4fe1 #x547b #x5a20 #x5bb8 #x613c #x65b0 #x6668 #x71fc
      #x7533 #x795e #x7d33 #x814e #x81e3 #x8398 #x85aa #x85ce
      #x8703 #x8a0a #x8eab #x8f9b #xf971 #x8fc5 #x5931 #x5ba4
      #x5be6 #x6089 #x5be9 #x5c0b #x5fc3 #x6c81
      ;; #x64
      #xf972 #x6df1 #x700b #x751a #x82af #x8af6 #x4ec0 #x5341
      #xf973 #x96d9 #x6c0f #x4e9e #x4fc4 #x5152 #x555e #x5a25
      #x5ce8 #x6211 #x7259 #x82bd #x83aa #x86fe #x8859 #x8a1d
      #x963f #x96c5 #x9913 #x9d09 #x9d5d #x580a #x5cb3 #x5dbd
      #x5e44 #x60e1 #x6115 #x63e1 #x6a02 #x6e25 #x9102 #x9354
      #x984e #x9c10 #x9f77 #x5b89 #x5cb8 #x6309 #x664f #x6848
      #x773c #x96c1 #x978d #x9854 #x9b9f #x65a1 #x8b01 #x8ecb
      #x95bc #x5535 #x5ca9 #x5dd6 #x5eb5 #x6697 #x764c #x83f4
      #x95c7 #x58d3 #x62bc #x72ce #x9d28 #x4ef0 #x592e #x600f
      #x663b #x6b83 #x79e7 #x9d26 #x5393 #x54c0 #x57c3 #x5d16
      #x611b #x66d6 #x6daf #x788d #x827e #x9698 #x9744 #x5384
      #x627c #x6396 #x6db2 #x7e0a #x814b #x984d
      ;; #x65
      #x6afb #x7f4c #x9daf #x9e1a #x4e5f #x503b #x51b6 #x591c
      #x60f9 #x63f6 #x6930 #x723a #x8036 #xf974 #x91ce #x5f31
      #xf975 #xf976 #x7d04 #x82e5 #x846f #x84bb #x85e5 #x8e8d
      #xf977 #x4f6f #xf978 #xf979 #x58e4 #x5b43 #x6059 #x63da
      #x6518 #x656d #x6698 #xf97a #x694a #x6a23 #x6d0b #x7001
      #x716c #x75d2 #x760d #x79b3 #x7a70 #xf97b #x7f8a #xf97c
      #x8944 #xf97d #x8b93 #x91c0 #x967d #xf97e #x990a #x5704
      #x5fa1 #x65bc #x6f01 #x7600 #x79a6 #x8a9e #x99ad #x9b5a
      #x9f6c #x5104 #x61b6 #x6291 #x6a8d #x81c6 #x5043 #x5830
      #x5f66 #x7109 #x8a00 #x8afa #x5b7c #x8616 #x4ffa #x513c
      #x56b4 #x5944 #x63a9 #x6df9 #x5daa #x696d #x5186 #x4e88
      #x4f59 #xf97f #xf980 #xf981 #x5982 #xf982
      ;; #x66
      #xf983 #x6b5f #x6c5d #xf984 #x74b5 #x7916 #xf985 #x8207
      #x8245 #x8339 #x8f3f #x8f5d #xf986 #x9918 #xf987 #xf988
      #xf989 #x4ea6 #xf98a #x57df #x5f79 #x6613 #xf98b #xf98c
      #x75ab #x7e79 #x8b6f #xf98d #x9006 #x9a5b #x56a5 #x5827
      #x59f8 #x5a1f #x5bb4 #xf98e #x5ef6 #xf98f #xf990 #x6350
      #x633b #xf991 #x693d #x6c87 #x6cbf #x6d8e #x6d93 #x6df5
      #x6f14 #xf992 #x70df #x7136 #x7159 #xf993 #x71c3 #x71d5
      #xf994 #x784f #x786f #xf995 #x7b75 #x7de3 #xf996 #x7e2f
      #xf997 #x884d #x8edf #xf998 #xf999 #xf99a #x925b #xf99b
      #x9cf6 #xf99c #xf99d #xf99e #x6085 #x6d85 #xf99f #x71b1
      #xf9a0 #xf9a1 #x95b1 #x53ad #xf9a2 #xf9a3 #xf9a4 #x67d3
      #xf9a5 #x708e #x7130 #x7430 #x8276 #x82d2
      ;; #x67
      #xf9a6 #x95bb #x9ae5 #x9e7d #x66c4 #xf9a7 #x71c1 #x8449
      #xf9a8 #xf9a9 #x584b #xf9aa #xf9ab #x5db8 #x5f71 #xf9ac
      #x6620 #x668e #x6979 #x69ae #x6c38 #x6cf3 #x6e36 #x6f41
      #x6fda #x701b #x702f #x7150 #x71df #x7370 #xf9ad #x745b
      #xf9ae #x74d4 #x76c8 #x7a4e #x7e93 #xf9af #xf9b0 #x82f1
      #x8a60 #x8fce #xf9b1 #x9348 #xf9b2 #x9719 #xf9b3 #xf9b4
      #x4e42 #x502a #xf9b5 #x5208 #x53e1 #x66f3 #x6c6d #x6fca
      #x730a #x777f #x7a62 #x82ae #x85dd #x8602 #xf9b6 #x88d4
      #x8a63 #x8b7d #x8c6b #xf9b7 #x92b3 #xf9b8 #x9713 #x9810
      #x4e94 #x4f0d #x4fc9 #x50b2 #x5348 #x543e #x5433 #x55da
      #x5862 #x58ba #x5967 #x5a1b #x5be4 #x609f #xf9b9 #x61ca
      #x6556 #x65ff #x6664 #x68a7 #x6c5a #x6fb3
      ;; #x68
      #x70cf #x71ac #x7352 #x7b7d #x8708 #x8aa4 #x9c32 #x9f07
      #x5c4b #x6c83 #x7344 #x7389 #x923a #x6eab #x7465 #x761f
      #x7a69 #x7e15 #x860a #x5140 #x58c5 #x64c1 #x74ee #x7515
      #x7670 #x7fc1 #x9095 #x96cd #x9954 #x6e26 #x74e6 #x7aa9
      #x7aaa #x81e5 #x86d9 #x8778 #x8a1b #x5a49 #x5b8c #x5b9b
      #x68a1 #x6900 #x6d63 #x73a9 #x7413 #x742c #x7897 #x7de9
      #x7feb #x8118 #x8155 #x839e #x8c4c #x962e #x9811 #x66f0
      #x5f80 #x65fa #x6789 #x6c6a #x738b #x502d #x5a03 #x6b6a
      #x77ee #x5916 #x5d6c #x5dcd #x7325 #x754f #xf9ba #xf9bb
      #x50e5 #x51f9 #x582f #x592d #x5996 #x59da #x5be5 #xf9bc
      #xf9bd #x5da2 #x62d7 #x6416 #x6493 #x64fe #xf9be #x66dc
      #xf9bf #x6a48 #xf9c0 #x71ff #x7464 #xf9c1
      ;; #x69
      #x7a88 #x7aaf #x7e47 #x7e5e #x8000 #x8170 #xf9c2 #x87ef
      #x8981 #x8b20 #x9059 #xf9c3 #x9080 #x9952 #x617e #x6b32
      #x6d74 #x7e1f #x8925 #x8fb1 #x4fd1 #x50ad #x5197 #x52c7
      #x57c7 #x5889 #x5bb9 #x5eb8 #x6142 #x6995 #x6d8c #x6e67
      #x6eb6 #x7194 #x7462 #x7528 #x752c #x8073 #x8338 #x84c9
      #x8e0a #x9394 #x93de #xf9c4 #x4e8e #x4f51 #x5076 #x512a
      #x53c8 #x53cb #x53f3 #x5b87 #x5bd3 #x5c24 #x611a #x6182
      #x65f4 #x725b #x7397 #x7440 #x76c2 #x7950 #x7991 #x79b9
      #x7d06 #x7fbd #x828b #x85d5 #x865e #x8fc2 #x9047 #x90f5
      #x91ea #x9685 #x96e8 #x96e9 #x52d6 #x5f67 #x65ed #x6631
      #x682f #x715c #x7a36 #x90c1 #x980a #x4e91 #xf9c5 #x6a52
      #x6b9e #x6f90 #x7189 #x8018 #x82b8 #x8553
      ;; #x6a
      #x904b #x9695 #x96f2 #x97fb #x851a #x9b31 #x4e90 #x718a
      #x96c4 #x5143 #x539f #x54e1 #x5713 #x5712 #x57a3 #x5a9b
      #x5ac4 #x5bc3 #x6028 #x613f #x63f4 #x6c85 #x6d39 #x6e72
      #x6e90 #x7230 #x733f #x7457 #x82d1 #x8881 #x8f45 #x9060
      #xf9c6 #x9662 #x9858 #x9d1b #x6708 #x8d8a #x925e #x4f4d
      #x5049 #x50de #x5371 #x570d #x59d4 #x5a01 #x5c09 #x6170
      #x6690 #x6e2d #x7232 #x744b #x7def #x80c3 #x840e #x8466
      #x853f #x875f #x885b #x8918 #x8b02 #x9055 #x97cb #x9b4f
      #x4e73 #x4f91 #x5112 #x516a #xf9c7 #x552f #x55a9 #x5b7a
      #x5ba5 #x5e7c #x5e7d #x5ebe #x60a0 #x60df #x6108 #x6109
      #x63c4 #x6538 #x6709 #xf9c8 #x67d4 #x67da #xf9c9 #x6961
      #x6962 #x6cb9 #x6d27 #xf9ca #x6e38 #xf9cb
      ;; #x6b
      #x6fe1 #x7336 #x7337 #xf9cc #x745c #x7531 #xf9cd #x7652
      #xf9ce #xf9cf #x7dad #x81fe #x8438 #x88d5 #x8a98 #x8adb
      #x8aed #x8e30 #x8e42 #x904a #x903e #x907a #x9149 #x91c9
      #x936e #xf9d0 #xf9d1 #x5809 #xf9d2 #x6bd3 #x8089 #x80b2
      #xf9d3 #xf9d4 #x5141 #x596b #x5c39 #xf9d5 #xf9d6 #x6f64
      #x73a7 #x80e4 #x8d07 #xf9d7 #x9217 #x958f #xf9d8 #xf9d9
      #xf9da #xf9db #x807f #x620e #x701c #x7d68 #x878d #xf9dc
      #x57a0 #x6069 #x6147 #x6bb7 #x8abe #x9280 #x96b1 #x4e59
      #x541f #x6deb #x852d #x9670 #x97f3 #x98ee #x63d6 #x6ce3
      #x9091 #x51dd #x61c9 #x81ba #x9df9 #x4f9d #x501a #x5100
      #x5b9c #x610f #x61ff #x64ec #x6905 #x6bc5 #x7591 #x77e3
      #x7fa9 #x8264 #x858f #x87fb #x8863 #x8abc
      ;; #x6c
      #x8b70 #x91ab #x4e8c #x4ee5 #x4f0a #xf9dd #xf9de #x5937
      #x59e8 #xf9df #x5df2 #x5f1b #x5f5b #x6021 #xf9e0 #xf9e1
      #xf9e2 #xf9e3 #x723e #x73e5 #xf9e4 #x7570 #x75cd #xf9e5
      #x79fb #xf9e6 #x800c #x8033 #x8084 #x82e1 #x8351 #xf9e7
      #xf9e8 #x8cbd #x8cb3 #x9087 #xf9e9 #xf9ea #x98f4 #x990c
      #xf9eb #xf9ec #x7037 #x76ca #x7fca #x7fcc #x7ffc #x8b1a
      #x4eba #x4ec1 #x5203 #x5370 #xf9ed #x54bd #x56e0 #x59fb
      #x5bc5 #x5f15 #x5fcd #x6e6e #xf9ee #xf9ef #x7d6a #x8335
      #xf9f0 #x8693 #x8a8d #xf9f1 #x976d #x9777 #xf9f2 #xf9f3
      #x4e00 #x4f5a #x4f7e #x58f9 #x65e5 #x6ea2 #x9038 #x93b0
      #x99b9 #x4efb #x58ec #x598a #x59d9 #x6041 #xf9f4 #xf9f5
      #x7a14 #xf9f6 #x834f #x8cc3 #x5165 #x5344
      ;; #x6d
      #xf9f7 #xf9f8 #xf9f9 #x4ecd #x5269 #x5b55 #x82bf #x4ed4
      #x523a #x54a8 #x59c9 #x59ff #x5b50 #x5b57 #x5b5c #x6063
      #x6148 #x6ecb #x7099 #x716e #x7386 #x74f7 #x75b5 #x78c1
      #x7d2b #x8005 #x81ea #x8328 #x8517 #x85c9 #x8aee #x8cc7
      #x96cc #x4f5c #x52fa #x56bc #x65ab #x6628 #x707c #x70b8
      #x7235 #x7dbd #x828d #x914c #x96c0 #x9d72 #x5b71 #x68e7
      #x6b98 #x6f7a #x76de #x5c91 #x66ab #x6f5b #x7bb4 #x7c2a
      #x8836 #x96dc #x4e08 #x4ed7 #x5320 #x5834 #x58bb #x58ef
      #x596c #x5c07 #x5e33 #x5e84 #x5f35 #x638c #x66b2 #x6756
      #x6a1f #x6aa3 #x6b0c #x6f3f #x7246 #xf9fa #x7350 #x748b
      #x7ae0 #x7ca7 #x8178 #x81df #x81e7 #x838a #x846c #x8523
      #x8594 #x85cf #x88dd #x8d13 #x91ac #x9577
      ;; #x6e
      #x969c #x518d #x54c9 #x5728 #x5bb0 #x624d #x6750 #x683d
      #x6893 #x6e3d #x6ed3 #x707d #x7e21 #x88c1 #x8ca1 #x8f09
      #x9f4b #x9f4e #x722d #x7b8f #x8acd #x931a #x4f47 #x4f4e
      #x5132 #x5480 #x59d0 #x5e95 #x62b5 #x6775 #x696e #x6a17
      #x6cae #x6e1a #x72d9 #x732a #x75bd #x7bb8 #x7d35 #x82e7
      #x83f9 #x8457 #x85f7 #x8a5b #x8caf #x8e87 #x9019 #x90b8
      #x96ce #x9f5f #x52e3 #x540a #x5ae1 #x5bc2 #x6458 #x6575
      #x6ef4 #x72c4 #xf9fb #x7684 #x7a4d #x7b1b #x7c4d #x7e3e
      #x7fdf #x837b #x8b2b #x8cca #x8d64 #x8de1 #x8e5f #x8fea
      #x8ff9 #x9069 #x93d1 #x4f43 #x4f7a #x50b3 #x5168 #x5178
      #x524d #x526a #x5861 #x587c #x5960 #x5c08 #x5c55 #x5edb
      #x609b #x6230 #x6813 #x6bbf #x6c08 #x6fb1
      ;; #x6f
      #x714e #x7420 #x7530 #x7538 #x7551 #x7672 #x7b4c #x7b8b
      #x7bad #x7bc6 #x7e8f #x8a6e #x8f3e #x8f49 #x923f #x9293
      #x9322 #x942b #x96fb #x985a #x986b #x991e #x5207 #x622a
      #x6298 #x6d59 #x7664 #x7aca #x7bc0 #x7d76 #x5360 #x5cbe
      #x5e97 #x6f38 #x70b9 #x7c98 #x9711 #x9b8e #x9ede #x63a5
      #x647a #x8776 #x4e01 #x4e95 #x4ead #x505c #x5075 #x5448
      #x59c3 #x5b9a #x5e40 #x5ead #x5ef7 #x5f81 #x60c5 #x633a
      #x653f #x6574 #x65cc #x6676 #x6678 #x67fe #x6968 #x6a89
      #x6b63 #x6c40 #x6dc0 #x6de8 #x6e1f #x6e5e #x701e #x70a1
      #x738e #x73fd #x753a #x775b #x7887 #x798e #x7a0b #x7a7d
      #x7cbe #x7d8e #x8247 #x8a02 #x8aea #x8c9e #x912d #x914a
      #x91d8 #x9266 #x92cc #x9320 #x9706 #x9756
      ;; #x70
      #x975c #x9802 #x9f0e #x5236 #x5291 #x557c #x5824 #x5e1d
      #x5f1f #x608c #x63d0 #x68af #x6fdf #x796d #x7b2c #x81cd
      #x85ba #x88fd #x8af8 #x8e44 #x918d #x9664 #x969b #x973d
      #x984c #x9f4a #x4fce #x5146 #x51cb #x52a9 #x5632 #x5f14
      #x5f6b #x63aa #x64cd #x65e9 #x6641 #x66fa #x66f9 #x671d
      #x689d #x68d7 #x69fd #x6f15 #x6f6e #x7167 #x71e5 #x722a
      #x74aa #x773a #x7956 #x795a #x79df #x7a20 #x7a95 #x7c97
      #x7cdf #x7d44 #x7e70 #x8087 #x85fb #x86a4 #x8a54 #x8abf
      #x8d99 #x8e81 #x9020 #x906d #x91e3 #x963b #x96d5 #x9ce5
      #x65cf #x7c07 #x8db3 #x93c3 #x5b58 #x5c0a #x5352 #x62d9
      #x731d #x5027 #x5b97 #x5f9e #x60b0 #x616b #x68d5 #x6dd9
      #x742e #x7a2e #x7d42 #x7d9c #x7e31 #x816b
      ;; #x71
      #x8e2a #x8e35 #x937e #x9418 #x4f50 #x5750 #x5de6 #x5ea7
      #x632b #x7f6a #x4e3b #x4f4f #x4f8f #x505a #x59dd #x80c4
      #x546a #x5468 #x55fe #x594f #x5b99 #x5dde #x5eda #x665d
      #x6731 #x67f1 #x682a #x6ce8 #x6d32 #x6e4a #x6f8d #x70b7
      #x73e0 #x7587 #x7c4c #x7d02 #x7d2c #x7da2 #x821f #x86db
      #x8a3b #x8a85 #x8d70 #x8e8a #x8f33 #x9031 #x914e #x9152
      #x9444 #x99d0 #x7af9 #x7ca5 #x4fca #x5101 #x51c6 #x57c8
      #x5bef #x5cfb #x6659 #x6a3d #x6d5a #x6e96 #x6fec #x710c
      #x756f #x7ae3 #x8822 #x9021 #x9075 #x96cb #x99ff #x8301
      #x4e2d #x4ef2 #x8846 #x91cd #x537d #x6adb #x696b #x6c41
      #x847a #x589e #x618e #x66fe #x62ef #x70dd #x7511 #x75c7
      #x7e52 #x84b8 #x8b49 #x8d08 #x4e4b #x53ea
      ;; #x72
      #x54ab #x5730 #x5740 #x5fd7 #x6301 #x6307 #x646f #x652f
      #x65e8 #x667a #x679d #x67b3 #x6b62 #x6c60 #x6c9a #x6f2c
      #x77e5 #x7825 #x7949 #x7957 #x7d19 #x80a2 #x8102 #x81f3
      #x829d #x82b7 #x8718 #x8a8c #xf9fc #x8d04 #x8dbe #x9072
      #x76f4 #x7a19 #x7a37 #x7e54 #x8077 #x5507 #x55d4 #x5875
      #x632f #x6422 #x6649 #x664b #x686d #x699b #x6b84 #x6d25
      #x6eb1 #x73cd #x7468 #x74a1 #x755b #x75b9 #x76e1 #x771e
      #x778b #x79e6 #x7e09 #x7e1d #x81fb #x852f #x8897 #x8a3a
      #x8cd1 #x8eeb #x8fb0 #x9032 #x93ad #x9663 #x9673 #x9707
      #x4f84 #x53f1 #x59ea #x5ac9 #x5e19 #x684e #x74c6 #x75be
      #x79e9 #x7a92 #x81a3 #x86ed #x8cea #x8dcc #x8fed #x659f
      #x6715 #xf9fd #x57f7 #x6f57 #x7ddd #x8f2f
      ;; #x73
      #x93f6 #x96c6 #x5fb5 #x61f2 #x6f84 #x4e14 #x4f98 #x501f
      #x53c9 #x55df #x5d6f #x5dee #x6b21 #x6b64 #x78cb #x7b9a
      #xf9fe #x8e49 #x8eca #x906e #x6349 #x643e #x7740 #x7a84
      #x932f #x947f #x9f6a #x64b0 #x6faf #x71e6 #x74a8 #x74da
      #x7ac4 #x7c12 #x7e82 #x7cb2 #x7e98 #x8b9a #x8d0a #x947d
      #x9910 #x994c #x5239 #x5bdf #x64e6 #x672d #x7d2e #x50ed
      #x53c3 #x5879 #x6158 #x6159 #x61fa #x65ac #x7ad9 #x8b92
      #x8b96 #x5009 #x5021 #x5275 #x5531 #x5a3c #x5ee0 #x5f70
      #x6134 #x655e #x660c #x6636 #x66a2 #x69cd #x6ec4 #x6f32
      #x7316 #x7621 #x7a93 #x8139 #x8259 #x83d6 #x84bc #x50b5
      #x57f0 #x5bc0 #x5be8 #x5f69 #x63a1 #x7826 #x7db5 #x83dc
      #x8521 #x91c7 #x91f5 #x518a #x67f5 #x7b56
      ;; #x74
      #x8cac #x51c4 #x59bb #x60bd #x8655 #x501c #xf9ff #x5254
      #x5c3a #x617d #x621a #x62d3 #x64f2 #x65a5 #x6ecc #x7620
      #x810a #x8e60 #x965f #x96bb #x4edf #x5343 #x5598 #x5929
      #x5ddd #x64c5 #x6cc9 #x6dfa #x7394 #x7a7f #x821b #x85a6
      #x8ce4 #x8e10 #x9077 #x91e7 #x95e1 #x9621 #x97c6 #x51f8
      #x54f2 #x5586 #x5fb9 #x64a4 #x6f88 #x7db4 #x8f1f #x8f4d
      #x9435 #x50c9 #x5c16 #x6cbe #x6dfb #x751b #x77bb #x7c3d
      #x7c64 #x8a79 #x8ac2 #x581e #x59be #x5e16 #x6377 #x7252
      #x758a #x776b #x8adc #x8cbc #x8f12 #x5ef3 #x6674 #x6df8
      #x807d #x83c1 #x8acb #x9751 #x9bd6 #xfa00 #x5243 #x66ff
      #x6d95 #x6eef #x7de0 #x8ae6 #x902e #x905e #x9ad4 #x521d
      #x527f #x54e8 #x6194 #x6284 #x62db #x68a2
      ;; #x75
      #x6912 #x695a #x6a35 #x7092 #x7126 #x785d #x7901 #x790e
      #x79d2 #x7a0d #x8096 #x8278 #x82d5 #x8349 #x8549 #x8c82
      #x8d85 #x9162 #x918b #x91ae #x4fc3 #x56d1 #x71ed #x77d7
      #x8700 #x89f8 #x5bf8 #x5fd6 #x6751 #x90a8 #x53e2 #x585a
      #x5bf5 #x60a4 #x6181 #x6460 #x7e3d #x8070 #x8525 #x9283
      #x64ae #x50ac #x5d14 #x6700 #x589c #x62bd #x63a8 #x690e
      #x6978 #x6a1e #x6e6b #x76ba #x79cb #x82bb #x8429 #x8acf
      #x8da8 #x8ffd #x9112 #x914b #x919c #x9310 #x9318 #x939a
      #x96db #x9a36 #x9c0d #x4e11 #x755c #x795d #x7afa #x7b51
      #x7bc9 #x7e2e #x84c4 #x8e59 #x8e74 #x8ef8 #x9010 #x6625
      #x693f #x7443 #x51fa #x672e #x9edc #x5145 #x5fe0 #x6c96
      #x87f2 #x885d #x8877 #x60b4 #x81b5 #x8403
      ;; #x76
      #x8d05 #x53d6 #x5439 #x5634 #x5a36 #x5c31 #x708a #x7fe0
      #x805a #x8106 #x81ed #x8da3 #x9189 #x9a5f #x9df2 #x5074
      #x4ec4 #x53a0 #x60fb #x6e2c #x5c64 #x4f88 #x5024 #x55e4
      #x5cd9 #x5e5f #x6065 #x6894 #x6cbb #x6dc4 #x71be #x75d4
      #x75f4 #x7661 #x7a1a #x7a49 #x7dc7 #x7dfb #x7f6e #x81f4
      #x86a9 #x8f1c #x96c9 #x99b3 #x9f52 #x5247 #x52c5 #x98ed
      #x89aa #x4e03 #x67d2 #x6f06 #x4fb5 #x5be2 #x6795 #x6c88
      #x6d78 #x741b #x7827 #x91dd #x937c #x87c4 #x79e4 #x7a31
      #x5feb #x4ed6 #x54a4 #x553e #x58ae #x59a5 #x60f0 #x6253
      #x62d6 #x6736 #x6955 #x8235 #x9640 #x99b1 #x99dd #x502c
      #x5353 #x5544 #x577c #xfa01 #x6258 #xfa02 #x64e2 #x666b
      #x67dd #x6fc1 #x6fef #x7422 #x7438 #x8a17
      ;; #x77
      #x9438 #x5451 #x5606 #x5766 #x5f48 #x619a #x6b4e #x7058
      #x70ad #x7dbb #x8a95 #x596a #x812b #x63a2 #x7708 #x803d
      #x8caa #x5854 #x642d #x69bb #x5b95 #x5e11 #x6e6f #xfa03
      #x8569 #x514c #x53f0 #x592a #x6020 #x614b #x6b86 #x6c70
      #x6cf0 #x7b1e #x80ce #x82d4 #x8dc6 #x90b0 #x98b1 #xfa04
      #x64c7 #x6fa4 #x6491 #x6504 #x514e #x5410 #x571f #x8a0e
      #x615f #x6876 #xfa05 #x75db #x7b52 #x7d71 #x901a #x5806
      #x69cc #x817f #x892a #x9000 #x9839 #x5078 #x5957 #x59ac
      #x6295 #x900f #x9b2a #x615d #x7279 #x95d6 #x5761 #x5a46
      #x5df4 #x628a #x64ad #x64fa #x6777 #x6ce2 #x6d3e #x722c
      #x7436 #x7834 #x7f77 #x82ad #x8ddb #x9817 #x5224 #x5742
      #x677f #x7248 #x74e3 #x8ca9 #x8fa6 #x9211
      ;; #x78
      #x962a #x516b #x53ed #x634c #x4f69 #x5504 #x6096 #x6557
      #x6c9b #x6d7f #x724c #x72fd #x7a17 #x8987 #x8c9d #x5f6d
      #x6f8e #x70f9 #x81a8 #x610e #x4fbf #x504f #x6241 #x7247
      #x7bc7 #x7de8 #x7fe9 #x904d #x97ad #x9a19 #x8cb6 #x576a
      #x5e73 #x67b0 #x840d #x8a55 #x5420 #x5b16 #x5e63 #x5ee2
      #x5f0a #x6583 #x80ba #x853d #x9589 #x965b #x4f48 #x5305
      #x530d #x530f #x5486 #x54fa #x5703 #x5e03 #x6016 #x629b
      #x62b1 #x6355 #xfa06 #x6ce1 #x6d66 #x75b1 #x7832 #x80de
      #x812f #x82de #x8461 #x84b2 #x888d #x8912 #x900b #x92ea
      #x98fd #x9b91 #x5e45 #x66b4 #x66dd #x7011 #x7206 #xfa07
      #x4ff5 #x527d #x5f6a #x6153 #x6753 #x6a19 #x6f02 #x74e2
      #x7968 #x8868 #x8c79 #x98c7 #x98c4 #x9a43
      ;; #x79
      #x54c1 #x7a1f #x6953 #x8af7 #x8c4a #x98a8 #x99ae #x5f7c
      #x62ab #x75b2 #x76ae #x88ab #x907f #x9642 #x5339 #x5f3c
      #x5fc5 #x6ccc #x73cc #x7562 #x758b #x7b46 #x82fe #x999d
      #x4e4f #x903c #x4e0b #x4f55 #x53a6 #x590f #x5ec8 #x6630
      #x6cb3 #x7455 #x8377 #x8766 #x8cc0 #x9050 #x971e #x9c15
      #x58d1 #x5b78 #x8650 #x8b14 #x9db4 #x5bd2 #x6068 #x608d
      #x65f1 #x6c57 #x6f22 #x6fa3 #x701a #x7f55 #x7ff0 #x9591
      #x9592 #x9650 #x97d3 #x5272 #x8f44 #x51fd #x542b #x54b8
      #x5563 #x558a #x6abb #x6db5 #x7dd8 #x8266 #x929c #x9677
      #x9e79 #x5408 #x54c8 #x76d2 #x86e4 #x95a4 #x95d4 #x965c
      #x4ea2 #x4f09 #x59ee #x5ae6 #x5df7 #x6052 #x6297 #x676d
      #x6841 #x6c86 #x6e2f #x7f38 #x809b #x822a
      ;; #x7a
      #xfa08 #xfa09 #x9805 #x4ea5 #x5055 #x54b3 #x5793 #x595a
      #x5b69 #x5bb3 #x61c8 #x6977 #x6d77 #x7023 #x87f9 #x89e3
      #x8a72 #x8ae7 #x9082 #x99ed #x9ab8 #x52be #x6838 #x5016
      #x5e78 #x674f #x8347 #x884c #x4eab #x5411 #x56ae #x73e6
      #x9115 #x97ff #x9909 #x9957 #x9999 #x5653 #x589f #x865b
      #x8a31 #x61b2 #x6af6 #x737b #x8ed2 #x6b47 #x96aa #x9a57
      #x5955 #x7200 #x8d6b #x9769 #x4fd4 #x5cf4 #x5f26 #x61f8
      #x665b #x6ceb #x70ab #x7384 #x73b9 #x73fe #x7729 #x774d
      #x7d43 #x7d62 #x7e23 #x8237 #x8852 #xfa0a #x8ce2 #x9249
      #x986f #x5b51 #x7a74 #x8840 #x9801 #x5acc #x4fe0 #x5354
      #x593e #x5cfd #x633e #x6d79 #x72f9 #x8105 #x8107 #x83a2
      #x92cf #x9830 #x4ea8 #x5144 #x5211 #x578b
      ;; #x7b
      #x5f62 #x6cc2 #x6ece #x7005 #x7050 #x70af #x7192 #x73e9
      #x7469 #x834a #x87a2 #x8861 #x9008 #x90a2 #x93a3 #x99a8
      #x516e #x5f57 #x60e0 #x6167 #x66b3 #x8559 #x8e4a #x91af
      #x978b #x4e4e #x4e92 #x547c #x58d5 #x58fa #x597d #x5cb5
      #x5f27 #x6236 #x6248 #x660a #x6667 #x6beb #x6d69 #x6dcf
      #x6e56 #x6ef8 #x6f94 #x6fe0 #x6fe9 #x705d #x72d0 #x7425
      #x745a #x74e0 #x7693 #x795c #x7cca #x7e1e #x80e1 #x82a6
      #x846b #x84bf #x864e #x865f #x8774 #x8b77 #x8c6a #x93ac
      #x9800 #x9865 #x60d1 #x6216 #x9177 #x5a5a #x660f #x6df7
      #x6e3e #x743f #x9b42 #x5ffd #x60da #x7b0f #x54c4 #x5f18
      #x6c5e #x6cd3 #x6d2a #x70d8 #x7d05 #x8679 #x8a0c #x9d3b
      #x5316 #x548c #x5b05 #x6a3a #x706b #x7575
      ;; #x7c
      #x798d #x79be #x82b1 #x83ef #x8a71 #x8b41 #x8ca8 #x9774
      #xfa0b #x64f4 #x652b #x78ba #x78bb #x7a6b #x4e38 #x559a
      #x5950 #x5ba6 #x5e7b #x60a3 #x63db #x6b61 #x6665 #x6853
      #x6e19 #x7165 #x74b0 #x7d08 #x9084 #x9a69 #x9c25 #x6d3b
      #x6ed1 #x733e #x8c41 #x95ca #x51f0 #x5e4c #x5fa8 #x604d
      #x60f6 #x6130 #x614c #x6643 #x6644 #x69a5 #x6cc1 #x6e5f
      #x6ec9 #x6f62 #x714c #x749c #x7687 #x7bc1 #x7c27 #x8352
      #x8757 #x9051 #x968d #x9ec3 #x532f #x56de #x5efb #x5f8a
      #x6062 #x6094 #x61f7 #x6666 #x6703 #x6a9c #x6dee #x6fae
      #x7070 #x736a #x7e6a #x81be #x8334 #x86d4 #x8aa8 #x8cc4
      #x5283 #x7372 #x5b96 #x6a6b #x9404 #x54ee #x5686 #x5b5d
      #x6548 #x6585 #x66c9 #x689f #x6d8d #x6dc6
      ;; #x7d
      #x723b #x80b4 #x9175 #x9a4d #x4faf #x5019 #x539a #x540e
      #x543c #x5589 #x55c5 #x5e3f #x5f8c #x673d #x7166 #x73dd
      #x9005 #x52db #x52f3 #x5864 #x58ce #x7104 #x718f #x71fb
      #x85b0 #x8a13 #x6688 #x85a8 #x55a7 #x6684 #x714a #x8431
      #x5349 #x5599 #x6bc1 #x5f59 #x5fbd #x63ee #x6689 #x7147
      #x8af1 #x8f1d #x9ebe #x4f11 #x643a #x70cb #x7566 #x8667
      #x6064 #x8b4e #x9df8 #x5147 #x51f6 #x5308 #x6d36 #x80f8
      #x9ed1 #x6615 #x6b23 #x7098 #x75d5 #x5403 #x5c79 #x7d07
      #x8a16 #x6b20 #x6b3d #x6b46 #x5438 #x6070 #x6d3d #x7fd5
      #x8208 #x50d6 #x51de #x559c #x566b #x56cd #x59ec #x5b09
      #x5e0c #x6199 #x6198 #x6231 #x665e #x66e6 #x7199 #x71b9
      #x71ba #x72a7 #x79a7 #x7a00 #x7fb2 #x8a70
      )
  :test #'equalp)

(define-constant +unicode-to-ksc-5601+
    (let ((h (make-hash-table)))
      (loop
        for b2 of-type ub8 from #x21 to #x7e
        do (loop
             for b1 of-type ub8 from #x21 to #x2c
             for unicode of-type code-point
               = (svref +ksc-5601-21-2c-to-unicode+
                        (+ (* 94 (- b1 #x21))
                           (- b2 #x21)))
             unless (= unicode #xfffd)
               do (setf (gethash unicode h)
                        (logior (ash b1 8) b2)))
        do (loop
             for b1 of-type ub8 from #x30 to #x48
             for unicode of-type code-point
               = (svref +ksc-5601-30-48-to-unicode+
                        (+ (* 94 (- b1 #x30))
                           (- b2 #x21)))
             unless (= unicode #xfffd)
               do (setf (gethash unicode h)
                        (logior (ash b1 8) b2)))
        do (loop
             for b1 of-type ub8 from #x4a to #x7d
             for unicode of-type code-point
               = (svref +ksc-5601-4a-7d-to-unicode+
                        (+ (* 94 (- b1 #x4a))
                           (- b2 #x21)))
             unless (= unicode #xfffd)
               do (setf (gethash unicode h)
                        (logior (ash b1 8) b2))))
      h)
  :test #'equalp)

(defun utf8-to-ksc-5601 (utf8)
  (gethash utf8 +unicode-to-ksc-5601+))

(defun ksc-5601-to-utf8 (ksc-5601)
  (let ((b1 (ldb (byte 8 0) ksc-5601))
        (b2 (ldb (byte 8 8) ksc-5601)))
    (cond
      ((and (<= #x21 b1 #x2c)
            (<= #x21 b2 #x7e))
       (svref +ksc-5601-21-2c-to-unicode+
              (+ (* 94 (- b1 #x21))
                 (- b2 #x21))))
      ((and (<= #x30 b1 #x48)
            (<= #x21 b2 #x7e))
       (svref +ksc-5601-30-48-to-unicode+
              (+ (* 94 (- b1 #x30))
                 (- b2 #x21))))
      ((and (<= #x4a b1 #x7d)
            (<= #x21 b2 #x7e))
       (svref +ksc-5601-4a-7d-to-unicode+
              (+ (* 94 (- b1 #x4a))
                 (- b2 #x21))))
      (t #xfffd))))

(define-character-encoding :ksc_5601
    "A 16-bit, fixed-width character encoding for the Korean language"
  :aliases '(:ks_c_5601 :ks_c_5601-1987 :ksc_5601-1987 :wansung)
  :code-unit-size 16
  :default-replacement #xfffd
  :codespace `(,+unicode-to-ksc-5601+))

(define-code-point-counter :ksc_5601 (getter type)
  `(named-lambda ksc-5601-code-point-counter (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (multiple-value-bind (count rem)
         (floor (- end start) 2)
       (cond
         ((and (plusp max) (> count max))
          (values max (the fixnum (+ start (* 2 max)))))
         (t
          (unless (zerop rem)
            (let ((vector (make-array 2 :fill-pointer 0)))
              (dotimes (i rem)
                (vector-push (,getter seq (+ i (- end rem))) vector))
              (decoding-error vector :ksc_5601 seq (the fixnum (- end rem)) nil
                              'end-of-input-in-character)
              (decf end rem)))
          (values count end))))))

(define-encoder :ksc_5601 (getter src-type setter dest-type)
  `(named-lambda ksc-5601-encoder (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (loop for i fixnum from start below end
           and di fixnum from d-start by 2
           for code of-type code-point = (,getter src i)
           for ksc-5601 of-type code-point = (utf8-to-ksc-5601 code)
           unless ksc-5601
             do (handle-error)
           do (,setter ksc-5601 dest di 2 :be)
           finally (return (the fixnum (- di d-start))))))

(define-decoder :ksc_5601 (getter src-type setter dest-type)
  `(named-lambda ksc-5601-decoder (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (loop for i fixnum from start below end by 2
           and di fixnum from d-start
           for ksc-5601 of-type code-point = (,getter src i 2)
           for code of-type code-point = (ksc-5601-to-utf8 ksc-5601)
           do (,setter code dest di)
           finally (return (the fixnum (- di d-start))))))
