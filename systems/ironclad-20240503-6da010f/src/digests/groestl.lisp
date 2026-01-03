;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; groestl.lisp -- implementation of the Grøstl hash function

(in-package :crypto)


;;;
;;; Parameters
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +groestl-rows+ 8)
  (defconstant +groestl-length-field-length+ +groestl-rows+)
  (defconstant +groestl-cols-512+ 8)
  (defconstant +groestl-cols-1024+ 16)
  (defconstant +groestl-size-512+ (* +groestl-rows+ +groestl-cols-512+))
  (defconstant +groestl-size-1024+ (* +groestl-rows+ +groestl-cols-1024+))
  (defconstant +groestl-rounds-512+ 10)
  (defconstant +groestl-rounds-1024+ 14)
  (defconst +groestl-table+
    (make-array 2048
                :element-type '(unsigned-byte 64)
                :initial-contents '(#xc632f4a5f497a5c6 #xf86f978497eb84f8
                                    #xee5eb099b0c799ee #xf67a8c8d8cf78df6
                                    #xffe8170d17e50dff #xd60adcbddcb7bdd6
                                    #xde16c8b1c8a7b1de #x916dfc54fc395491
                                    #x6090f050f0c05060 #x0207050305040302
                                    #xce2ee0a9e087a9ce #x56d1877d87ac7d56
                                    #xe7cc2b192bd519e7 #xb513a662a67162b5
                                    #x4d7c31e6319ae64d #xec59b59ab5c39aec
                                    #x8f40cf45cf05458f #x1fa3bc9dbc3e9d1f
                                    #x8949c040c0094089 #xfa68928792ef87fa
                                    #xefd03f153fc515ef #xb29426eb267febb2
                                    #x8ece40c94007c98e #xfbe61d0b1ded0bfb
                                    #x416e2fec2f82ec41 #xb31aa967a97d67b3
                                    #x5f431cfd1cbefd5f #x456025ea258aea45
                                    #x23f9dabfda46bf23 #x535102f702a6f753
                                    #xe445a196a1d396e4 #x9b76ed5bed2d5b9b
                                    #x75285dc25deac275 #xe1c5241c24d91ce1
                                    #x3dd4e9aee97aae3d #x4cf2be6abe986a4c
                                    #x6c82ee5aeed85a6c #x7ebdc341c3fc417e
                                    #xf5f3060206f102f5 #x8352d14fd11d4f83
                                    #x688ce45ce4d05c68 #x515607f407a2f451
                                    #xd18d5c345cb934d1 #xf9e1180818e908f9
                                    #xe24cae93aedf93e2 #xab3e9573954d73ab
                                    #x6297f553f5c45362 #x2a6b413f41543f2a
                                    #x081c140c14100c08 #x9563f652f6315295
                                    #x46e9af65af8c6546 #x9d7fe25ee2215e9d
                                    #x3048782878602830 #x37cff8a1f86ea137
                                    #x0a1b110f11140f0a #x2febc4b5c45eb52f
                                    #x0e151b091b1c090e #x247e5a365a483624
                                    #x1badb69bb6369b1b #xdf98473d47a53ddf
                                    #xcda76a266a8126cd #x4ef5bb69bb9c694e
                                    #x7f334ccd4cfecd7f #xea50ba9fbacf9fea
                                    #x123f2d1b2d241b12 #x1da4b99eb93a9e1d
                                    #x58c49c749cb07458 #x3446722e72682e34
                                    #x3641772d776c2d36 #xdc11cdb2cda3b2dc
                                    #xb49d29ee2973eeb4 #x5b4d16fb16b6fb5b
                                    #xa4a501f60153f6a4 #x76a1d74dd7ec4d76
                                    #xb714a361a37561b7 #x7d3449ce49face7d
                                    #x52df8d7b8da47b52 #xdd9f423e42a13edd
                                    #x5ecd937193bc715e #x13b1a297a2269713
                                    #xa6a204f50457f5a6 #xb901b868b86968b9
                                    #x0000000000000000 #xc1b5742c74992cc1
                                    #x40e0a060a0806040 #xe3c2211f21dd1fe3
                                    #x793a43c843f2c879 #xb69a2ced2c77edb6
                                    #xd40dd9bed9b3bed4 #x8d47ca46ca01468d
                                    #x671770d970ced967 #x72afdd4bdde44b72
                                    #x94ed79de7933de94 #x98ff67d4672bd498
                                    #xb09323e8237be8b0 #x855bde4ade114a85
                                    #xbb06bd6bbd6d6bbb #xc5bb7e2a7e912ac5
                                    #x4f7b34e5349ee54f #xedd73a163ac116ed
                                    #x86d254c55417c586 #x9af862d7622fd79a
                                    #x6699ff55ffcc5566 #x11b6a794a7229411
                                    #x8ac04acf4a0fcf8a #xe9d9301030c910e9
                                    #x040e0a060a080604 #xfe66988198e781fe
                                    #xa0ab0bf00b5bf0a0 #x78b4cc44ccf04478
                                    #x25f0d5bad54aba25 #x4b753ee33e96e34b
                                    #xa2ac0ef30e5ff3a2 #x5d4419fe19bafe5d
                                    #x80db5bc05b1bc080 #x0580858a850a8a05
                                    #x3fd3ecadec7ead3f #x21fedfbcdf42bc21
                                    #x70a8d848d8e04870 #xf1fd0c040cf904f1
                                    #x63197adf7ac6df63 #x772f58c158eec177
                                    #xaf309f759f4575af #x42e7a563a5846342
                                    #x2070503050403020 #xe5cb2e1a2ed11ae5
                                    #xfdef120e12e10efd #xbf08b76db7656dbf
                                    #x8155d44cd4194c81 #x18243c143c301418
                                    #x26795f355f4c3526 #xc3b2712f719d2fc3
                                    #xbe8638e13867e1be #x35c8fda2fd6aa235
                                    #x88c74fcc4f0bcc88 #x2e654b394b5c392e
                                    #x936af957f93d5793 #x55580df20daaf255
                                    #xfc619d829de382fc #x7ab3c947c9f4477a
                                    #xc827efacef8bacc8 #xba8832e7326fe7ba
                                    #x324f7d2b7d642b32 #xe642a495a4d795e6
                                    #xc03bfba0fb9ba0c0 #x19aab398b3329819
                                    #x9ef668d16827d19e #xa322817f815d7fa3
                                    #x44eeaa66aa886644 #x54d6827e82a87e54
                                    #x3bdde6abe676ab3b #x0b959e839e16830b
                                    #x8cc945ca4503ca8c #xc7bc7b297b9529c7
                                    #x6b056ed36ed6d36b #x286c443c44503c28
                                    #xa72c8b798b5579a7 #xbc813de23d63e2bc
                                    #x1631271d272c1d16 #xad379a769a4176ad
                                    #xdb964d3b4dad3bdb #x649efa56fac85664
                                    #x74a6d24ed2e84e74 #x1436221e22281e14
                                    #x92e476db763fdb92 #x0c121e0a1e180a0c
                                    #x48fcb46cb4906c48 #xb88f37e4376be4b8
                                    #x9f78e75de7255d9f #xbd0fb26eb2616ebd
                                    #x43692aef2a86ef43 #xc435f1a6f193a6c4
                                    #x39dae3a8e372a839 #x31c6f7a4f762a431
                                    #xd38a593759bd37d3 #xf274868b86ff8bf2
                                    #xd583563256b132d5 #x8b4ec543c50d438b
                                    #x6e85eb59ebdc596e #xda18c2b7c2afb7da
                                    #x018e8f8c8f028c01 #xb11dac64ac7964b1
                                    #x9cf16dd26d23d29c #x49723be03b92e049
                                    #xd81fc7b4c7abb4d8 #xacb915fa1543faac
                                    #xf3fa090709fd07f3 #xcfa06f256f8525cf
                                    #xca20eaafea8fafca #xf47d898e89f38ef4
                                    #x476720e9208ee947 #x1038281828201810
                                    #x6f0b64d564ded56f #xf073838883fb88f0
                                    #x4afbb16fb1946f4a #x5cca967296b8725c
                                    #x38546c246c702438 #x575f08f108aef157
                                    #x732152c752e6c773 #x9764f351f3355197
                                    #xcbae6523658d23cb #xa125847c84597ca1
                                    #xe857bf9cbfcb9ce8 #x3e5d6321637c213e
                                    #x96ea7cdd7c37dd96 #x611e7fdc7fc2dc61
                                    #x0d9c9186911a860d #x0f9b9485941e850f
                                    #xe04bab90abdb90e0 #x7cbac642c6f8427c
                                    #x712657c457e2c471 #xcc29e5aae583aacc
                                    #x90e373d8733bd890 #x06090f050f0c0506
                                    #xf7f4030103f501f7 #x1c2a36123638121c
                                    #xc23cfea3fe9fa3c2 #x6a8be15fe1d45f6a
                                    #xaebe10f91047f9ae #x69026bd06bd2d069
                                    #x17bfa891a82e9117 #x9971e858e8295899
                                    #x3a5369276974273a #x27f7d0b9d04eb927
                                    #xd991483848a938d9 #xebde351335cd13eb
                                    #x2be5ceb3ce56b32b #x2277553355443322
                                    #xd204d6bbd6bfbbd2 #xa9399070904970a9
                                    #x07878089800e8907 #x33c1f2a7f266a733
                                    #x2decc1b6c15ab62d #x3c5a66226678223c
                                    #x15b8ad92ad2a9215 #xc9a96020608920c9
                                    #x875cdb49db154987 #xaab01aff1a4fffaa
                                    #x50d8887888a07850 #xa52b8e7a8e517aa5
                                    #x03898a8f8a068f03 #x594a13f813b2f859
                                    #x09929b809b128009 #x1a2339173934171a
                                    #x651075da75cada65 #xd784533153b531d7
                                    #x84d551c65113c684 #xd003d3b8d3bbb8d0
                                    #x82dc5ec35e1fc382 #x29e2cbb0cb52b029
                                    #x5ac3997799b4775a #x1e2d3311333c111e
                                    #x7b3d46cb46f6cb7b #xa8b71ffc1f4bfca8
                                    #x6d0c61d661dad66d #x2c624e3a4e583a2c

                                    #xc6c632f4a5f497a5 #xf8f86f978497eb84
                                    #xeeee5eb099b0c799 #xf6f67a8c8d8cf78d
                                    #xffffe8170d17e50d #xd6d60adcbddcb7bd
                                    #xdede16c8b1c8a7b1 #x91916dfc54fc3954
                                    #x606090f050f0c050 #x0202070503050403
                                    #xcece2ee0a9e087a9 #x5656d1877d87ac7d
                                    #xe7e7cc2b192bd519 #xb5b513a662a67162
                                    #x4d4d7c31e6319ae6 #xecec59b59ab5c39a
                                    #x8f8f40cf45cf0545 #x1f1fa3bc9dbc3e9d
                                    #x898949c040c00940 #xfafa68928792ef87
                                    #xefefd03f153fc515 #xb2b29426eb267feb
                                    #x8e8ece40c94007c9 #xfbfbe61d0b1ded0b
                                    #x41416e2fec2f82ec #xb3b31aa967a97d67
                                    #x5f5f431cfd1cbefd #x45456025ea258aea
                                    #x2323f9dabfda46bf #x53535102f702a6f7
                                    #xe4e445a196a1d396 #x9b9b76ed5bed2d5b
                                    #x7575285dc25deac2 #xe1e1c5241c24d91c
                                    #x3d3dd4e9aee97aae #x4c4cf2be6abe986a
                                    #x6c6c82ee5aeed85a #x7e7ebdc341c3fc41
                                    #xf5f5f3060206f102 #x838352d14fd11d4f
                                    #x68688ce45ce4d05c #x51515607f407a2f4
                                    #xd1d18d5c345cb934 #xf9f9e1180818e908
                                    #xe2e24cae93aedf93 #xabab3e9573954d73
                                    #x626297f553f5c453 #x2a2a6b413f41543f
                                    #x08081c140c14100c #x959563f652f63152
                                    #x4646e9af65af8c65 #x9d9d7fe25ee2215e
                                    #x3030487828786028 #x3737cff8a1f86ea1
                                    #x0a0a1b110f11140f #x2f2febc4b5c45eb5
                                    #x0e0e151b091b1c09 #x24247e5a365a4836
                                    #x1b1badb69bb6369b #xdfdf98473d47a53d
                                    #xcdcda76a266a8126 #x4e4ef5bb69bb9c69
                                    #x7f7f334ccd4cfecd #xeaea50ba9fbacf9f
                                    #x12123f2d1b2d241b #x1d1da4b99eb93a9e
                                    #x5858c49c749cb074 #x343446722e72682e
                                    #x363641772d776c2d #xdcdc11cdb2cda3b2
                                    #xb4b49d29ee2973ee #x5b5b4d16fb16b6fb
                                    #xa4a4a501f60153f6 #x7676a1d74dd7ec4d
                                    #xb7b714a361a37561 #x7d7d3449ce49face
                                    #x5252df8d7b8da47b #xdddd9f423e42a13e
                                    #x5e5ecd937193bc71 #x1313b1a297a22697
                                    #xa6a6a204f50457f5 #xb9b901b868b86968
                                    #x0000000000000000 #xc1c1b5742c74992c
                                    #x4040e0a060a08060 #xe3e3c2211f21dd1f
                                    #x79793a43c843f2c8 #xb6b69a2ced2c77ed
                                    #xd4d40dd9bed9b3be #x8d8d47ca46ca0146
                                    #x67671770d970ced9 #x7272afdd4bdde44b
                                    #x9494ed79de7933de #x9898ff67d4672bd4
                                    #xb0b09323e8237be8 #x85855bde4ade114a
                                    #xbbbb06bd6bbd6d6b #xc5c5bb7e2a7e912a
                                    #x4f4f7b34e5349ee5 #xededd73a163ac116
                                    #x8686d254c55417c5 #x9a9af862d7622fd7
                                    #x666699ff55ffcc55 #x1111b6a794a72294
                                    #x8a8ac04acf4a0fcf #xe9e9d9301030c910
                                    #x04040e0a060a0806 #xfefe66988198e781
                                    #xa0a0ab0bf00b5bf0 #x7878b4cc44ccf044
                                    #x2525f0d5bad54aba #x4b4b753ee33e96e3
                                    #xa2a2ac0ef30e5ff3 #x5d5d4419fe19bafe
                                    #x8080db5bc05b1bc0 #x050580858a850a8a
                                    #x3f3fd3ecadec7ead #x2121fedfbcdf42bc
                                    #x7070a8d848d8e048 #xf1f1fd0c040cf904
                                    #x6363197adf7ac6df #x77772f58c158eec1
                                    #xafaf309f759f4575 #x4242e7a563a58463
                                    #x2020705030504030 #xe5e5cb2e1a2ed11a
                                    #xfdfdef120e12e10e #xbfbf08b76db7656d
                                    #x818155d44cd4194c #x1818243c143c3014
                                    #x2626795f355f4c35 #xc3c3b2712f719d2f
                                    #xbebe8638e13867e1 #x3535c8fda2fd6aa2
                                    #x8888c74fcc4f0bcc #x2e2e654b394b5c39
                                    #x93936af957f93d57 #x5555580df20daaf2
                                    #xfcfc619d829de382 #x7a7ab3c947c9f447
                                    #xc8c827efacef8bac #xbaba8832e7326fe7
                                    #x32324f7d2b7d642b #xe6e642a495a4d795
                                    #xc0c03bfba0fb9ba0 #x1919aab398b33298
                                    #x9e9ef668d16827d1 #xa3a322817f815d7f
                                    #x4444eeaa66aa8866 #x5454d6827e82a87e
                                    #x3b3bdde6abe676ab #x0b0b959e839e1683
                                    #x8c8cc945ca4503ca #xc7c7bc7b297b9529
                                    #x6b6b056ed36ed6d3 #x28286c443c44503c
                                    #xa7a72c8b798b5579 #xbcbc813de23d63e2
                                    #x161631271d272c1d #xadad379a769a4176
                                    #xdbdb964d3b4dad3b #x64649efa56fac856
                                    #x7474a6d24ed2e84e #x141436221e22281e
                                    #x9292e476db763fdb #x0c0c121e0a1e180a
                                    #x4848fcb46cb4906c #xb8b88f37e4376be4
                                    #x9f9f78e75de7255d #xbdbd0fb26eb2616e
                                    #x4343692aef2a86ef #xc4c435f1a6f193a6
                                    #x3939dae3a8e372a8 #x3131c6f7a4f762a4
                                    #xd3d38a593759bd37 #xf2f274868b86ff8b
                                    #xd5d583563256b132 #x8b8b4ec543c50d43
                                    #x6e6e85eb59ebdc59 #xdada18c2b7c2afb7
                                    #x01018e8f8c8f028c #xb1b11dac64ac7964
                                    #x9c9cf16dd26d23d2 #x4949723be03b92e0
                                    #xd8d81fc7b4c7abb4 #xacacb915fa1543fa
                                    #xf3f3fa090709fd07 #xcfcfa06f256f8525
                                    #xcaca20eaafea8faf #xf4f47d898e89f38e
                                    #x47476720e9208ee9 #x1010382818282018
                                    #x6f6f0b64d564ded5 #xf0f073838883fb88
                                    #x4a4afbb16fb1946f #x5c5cca967296b872
                                    #x3838546c246c7024 #x57575f08f108aef1
                                    #x73732152c752e6c7 #x979764f351f33551
                                    #xcbcbae6523658d23 #xa1a125847c84597c
                                    #xe8e857bf9cbfcb9c #x3e3e5d6321637c21
                                    #x9696ea7cdd7c37dd #x61611e7fdc7fc2dc
                                    #x0d0d9c9186911a86 #x0f0f9b9485941e85
                                    #xe0e04bab90abdb90 #x7c7cbac642c6f842
                                    #x71712657c457e2c4 #xcccc29e5aae583aa
                                    #x9090e373d8733bd8 #x0606090f050f0c05
                                    #xf7f7f4030103f501 #x1c1c2a3612363812
                                    #xc2c23cfea3fe9fa3 #x6a6a8be15fe1d45f
                                    #xaeaebe10f91047f9 #x6969026bd06bd2d0
                                    #x1717bfa891a82e91 #x999971e858e82958
                                    #x3a3a536927697427 #x2727f7d0b9d04eb9
                                    #xd9d991483848a938 #xebebde351335cd13
                                    #x2b2be5ceb3ce56b3 #x2222775533554433
                                    #xd2d204d6bbd6bfbb #xa9a9399070904970
                                    #x0707878089800e89 #x3333c1f2a7f266a7
                                    #x2d2decc1b6c15ab6 #x3c3c5a6622667822
                                    #x1515b8ad92ad2a92 #xc9c9a96020608920
                                    #x87875cdb49db1549 #xaaaab01aff1a4fff
                                    #x5050d8887888a078 #xa5a52b8e7a8e517a
                                    #x0303898a8f8a068f #x59594a13f813b2f8
                                    #x0909929b809b1280 #x1a1a233917393417
                                    #x65651075da75cada #xd7d784533153b531
                                    #x8484d551c65113c6 #xd0d003d3b8d3bbb8
                                    #x8282dc5ec35e1fc3 #x2929e2cbb0cb52b0
                                    #x5a5ac3997799b477 #x1e1e2d3311333c11
                                    #x7b7b3d46cb46f6cb #xa8a8b71ffc1f4bfc
                                    #x6d6d0c61d661dad6 #x2c2c624e3a4e583a
                                    
                                    #xa5c6c632f4a5f497 #x84f8f86f978497eb
                                    #x99eeee5eb099b0c7 #x8df6f67a8c8d8cf7
                                    #x0dffffe8170d17e5 #xbdd6d60adcbddcb7
                                    #xb1dede16c8b1c8a7 #x5491916dfc54fc39
                                    #x50606090f050f0c0 #x0302020705030504
                                    #xa9cece2ee0a9e087 #x7d5656d1877d87ac
                                    #x19e7e7cc2b192bd5 #x62b5b513a662a671
                                    #xe64d4d7c31e6319a #x9aecec59b59ab5c3
                                    #x458f8f40cf45cf05 #x9d1f1fa3bc9dbc3e
                                    #x40898949c040c009 #x87fafa68928792ef
                                    #x15efefd03f153fc5 #xebb2b29426eb267f
                                    #xc98e8ece40c94007 #x0bfbfbe61d0b1ded
                                    #xec41416e2fec2f82 #x67b3b31aa967a97d
                                    #xfd5f5f431cfd1cbe #xea45456025ea258a
                                    #xbf2323f9dabfda46 #xf753535102f702a6
                                    #x96e4e445a196a1d3 #x5b9b9b76ed5bed2d
                                    #xc27575285dc25dea #x1ce1e1c5241c24d9
                                    #xae3d3dd4e9aee97a #x6a4c4cf2be6abe98
                                    #x5a6c6c82ee5aeed8 #x417e7ebdc341c3fc
                                    #x02f5f5f3060206f1 #x4f838352d14fd11d
                                    #x5c68688ce45ce4d0 #xf451515607f407a2
                                    #x34d1d18d5c345cb9 #x08f9f9e1180818e9
                                    #x93e2e24cae93aedf #x73abab3e9573954d
                                    #x53626297f553f5c4 #x3f2a2a6b413f4154
                                    #x0c08081c140c1410 #x52959563f652f631
                                    #x654646e9af65af8c #x5e9d9d7fe25ee221
                                    #x2830304878287860 #xa13737cff8a1f86e
                                    #x0f0a0a1b110f1114 #xb52f2febc4b5c45e
                                    #x090e0e151b091b1c #x3624247e5a365a48
                                    #x9b1b1badb69bb636 #x3ddfdf98473d47a5
                                    #x26cdcda76a266a81 #x694e4ef5bb69bb9c
                                    #xcd7f7f334ccd4cfe #x9feaea50ba9fbacf
                                    #x1b12123f2d1b2d24 #x9e1d1da4b99eb93a
                                    #x745858c49c749cb0 #x2e343446722e7268
                                    #x2d363641772d776c #xb2dcdc11cdb2cda3
                                    #xeeb4b49d29ee2973 #xfb5b5b4d16fb16b6
                                    #xf6a4a4a501f60153 #x4d7676a1d74dd7ec
                                    #x61b7b714a361a375 #xce7d7d3449ce49fa
                                    #x7b5252df8d7b8da4 #x3edddd9f423e42a1
                                    #x715e5ecd937193bc #x971313b1a297a226
                                    #xf5a6a6a204f50457 #x68b9b901b868b869
                                    #x0000000000000000 #x2cc1c1b5742c7499
                                    #x604040e0a060a080 #x1fe3e3c2211f21dd
                                    #xc879793a43c843f2 #xedb6b69a2ced2c77
                                    #xbed4d40dd9bed9b3 #x468d8d47ca46ca01
                                    #xd967671770d970ce #x4b7272afdd4bdde4
                                    #xde9494ed79de7933 #xd49898ff67d4672b
                                    #xe8b0b09323e8237b #x4a85855bde4ade11
                                    #x6bbbbb06bd6bbd6d #x2ac5c5bb7e2a7e91
                                    #xe54f4f7b34e5349e #x16ededd73a163ac1
                                    #xc58686d254c55417 #xd79a9af862d7622f
                                    #x55666699ff55ffcc #x941111b6a794a722
                                    #xcf8a8ac04acf4a0f #x10e9e9d9301030c9
                                    #x0604040e0a060a08 #x81fefe66988198e7
                                    #xf0a0a0ab0bf00b5b #x447878b4cc44ccf0
                                    #xba2525f0d5bad54a #xe34b4b753ee33e96
                                    #xf3a2a2ac0ef30e5f #xfe5d5d4419fe19ba
                                    #xc08080db5bc05b1b #x8a050580858a850a
                                    #xad3f3fd3ecadec7e #xbc2121fedfbcdf42
                                    #x487070a8d848d8e0 #x04f1f1fd0c040cf9
                                    #xdf6363197adf7ac6 #xc177772f58c158ee
                                    #x75afaf309f759f45 #x634242e7a563a584
                                    #x3020207050305040 #x1ae5e5cb2e1a2ed1
                                    #x0efdfdef120e12e1 #x6dbfbf08b76db765
                                    #x4c818155d44cd419 #x141818243c143c30
                                    #x352626795f355f4c #x2fc3c3b2712f719d
                                    #xe1bebe8638e13867 #xa23535c8fda2fd6a
                                    #xcc8888c74fcc4f0b #x392e2e654b394b5c
                                    #x5793936af957f93d #xf25555580df20daa
                                    #x82fcfc619d829de3 #x477a7ab3c947c9f4
                                    #xacc8c827efacef8b #xe7baba8832e7326f
                                    #x2b32324f7d2b7d64 #x95e6e642a495a4d7
                                    #xa0c0c03bfba0fb9b #x981919aab398b332
                                    #xd19e9ef668d16827 #x7fa3a322817f815d
                                    #x664444eeaa66aa88 #x7e5454d6827e82a8
                                    #xab3b3bdde6abe676 #x830b0b959e839e16
                                    #xca8c8cc945ca4503 #x29c7c7bc7b297b95
                                    #xd36b6b056ed36ed6 #x3c28286c443c4450
                                    #x79a7a72c8b798b55 #xe2bcbc813de23d63
                                    #x1d161631271d272c #x76adad379a769a41
                                    #x3bdbdb964d3b4dad #x5664649efa56fac8
                                    #x4e7474a6d24ed2e8 #x1e141436221e2228
                                    #xdb9292e476db763f #x0a0c0c121e0a1e18
                                    #x6c4848fcb46cb490 #xe4b8b88f37e4376b
                                    #x5d9f9f78e75de725 #x6ebdbd0fb26eb261
                                    #xef4343692aef2a86 #xa6c4c435f1a6f193
                                    #xa83939dae3a8e372 #xa43131c6f7a4f762
                                    #x37d3d38a593759bd #x8bf2f274868b86ff
                                    #x32d5d583563256b1 #x438b8b4ec543c50d
                                    #x596e6e85eb59ebdc #xb7dada18c2b7c2af
                                    #x8c01018e8f8c8f02 #x64b1b11dac64ac79
                                    #xd29c9cf16dd26d23 #xe04949723be03b92
                                    #xb4d8d81fc7b4c7ab #xfaacacb915fa1543
                                    #x07f3f3fa090709fd #x25cfcfa06f256f85
                                    #xafcaca20eaafea8f #x8ef4f47d898e89f3
                                    #xe947476720e9208e #x1810103828182820
                                    #xd56f6f0b64d564de #x88f0f073838883fb
                                    #x6f4a4afbb16fb194 #x725c5cca967296b8
                                    #x243838546c246c70 #xf157575f08f108ae
                                    #xc773732152c752e6 #x51979764f351f335
                                    #x23cbcbae6523658d #x7ca1a125847c8459
                                    #x9ce8e857bf9cbfcb #x213e3e5d6321637c
                                    #xdd9696ea7cdd7c37 #xdc61611e7fdc7fc2
                                    #x860d0d9c9186911a #x850f0f9b9485941e
                                    #x90e0e04bab90abdb #x427c7cbac642c6f8
                                    #xc471712657c457e2 #xaacccc29e5aae583
                                    #xd89090e373d8733b #x050606090f050f0c
                                    #x01f7f7f4030103f5 #x121c1c2a36123638
                                    #xa3c2c23cfea3fe9f #x5f6a6a8be15fe1d4
                                    #xf9aeaebe10f91047 #xd06969026bd06bd2
                                    #x911717bfa891a82e #x58999971e858e829
                                    #x273a3a5369276974 #xb92727f7d0b9d04e
                                    #x38d9d991483848a9 #x13ebebde351335cd
                                    #xb32b2be5ceb3ce56 #x3322227755335544
                                    #xbbd2d204d6bbd6bf #x70a9a93990709049
                                    #x890707878089800e #xa73333c1f2a7f266
                                    #xb62d2decc1b6c15a #x223c3c5a66226678
                                    #x921515b8ad92ad2a #x20c9c9a960206089
                                    #x4987875cdb49db15 #xffaaaab01aff1a4f
                                    #x785050d8887888a0 #x7aa5a52b8e7a8e51
                                    #x8f0303898a8f8a06 #xf859594a13f813b2
                                    #x800909929b809b12 #x171a1a2339173934
                                    #xda65651075da75ca #x31d7d784533153b5
                                    #xc68484d551c65113 #xb8d0d003d3b8d3bb
                                    #xc38282dc5ec35e1f #xb02929e2cbb0cb52
                                    #x775a5ac3997799b4 #x111e1e2d3311333c
                                    #xcb7b7b3d46cb46f6 #xfca8a8b71ffc1f4b
                                    #xd66d6d0c61d661da #x3a2c2c624e3a4e58
                                    
                                    #x97a5c6c632f4a5f4 #xeb84f8f86f978497
                                    #xc799eeee5eb099b0 #xf78df6f67a8c8d8c
                                    #xe50dffffe8170d17 #xb7bdd6d60adcbddc
                                    #xa7b1dede16c8b1c8 #x395491916dfc54fc
                                    #xc050606090f050f0 #x0403020207050305
                                    #x87a9cece2ee0a9e0 #xac7d5656d1877d87
                                    #xd519e7e7cc2b192b #x7162b5b513a662a6
                                    #x9ae64d4d7c31e631 #xc39aecec59b59ab5
                                    #x05458f8f40cf45cf #x3e9d1f1fa3bc9dbc
                                    #x0940898949c040c0 #xef87fafa68928792
                                    #xc515efefd03f153f #x7febb2b29426eb26
                                    #x07c98e8ece40c940 #xed0bfbfbe61d0b1d
                                    #x82ec41416e2fec2f #x7d67b3b31aa967a9
                                    #xbefd5f5f431cfd1c #x8aea45456025ea25
                                    #x46bf2323f9dabfda #xa6f753535102f702
                                    #xd396e4e445a196a1 #x2d5b9b9b76ed5bed
                                    #xeac27575285dc25d #xd91ce1e1c5241c24
                                    #x7aae3d3dd4e9aee9 #x986a4c4cf2be6abe
                                    #xd85a6c6c82ee5aee #xfc417e7ebdc341c3
                                    #xf102f5f5f3060206 #x1d4f838352d14fd1
                                    #xd05c68688ce45ce4 #xa2f451515607f407
                                    #xb934d1d18d5c345c #xe908f9f9e1180818
                                    #xdf93e2e24cae93ae #x4d73abab3e957395
                                    #xc453626297f553f5 #x543f2a2a6b413f41
                                    #x100c08081c140c14 #x3152959563f652f6
                                    #x8c654646e9af65af #x215e9d9d7fe25ee2
                                    #x6028303048782878 #x6ea13737cff8a1f8
                                    #x140f0a0a1b110f11 #x5eb52f2febc4b5c4
                                    #x1c090e0e151b091b #x483624247e5a365a
                                    #x369b1b1badb69bb6 #xa53ddfdf98473d47
                                    #x8126cdcda76a266a #x9c694e4ef5bb69bb
                                    #xfecd7f7f334ccd4c #xcf9feaea50ba9fba
                                    #x241b12123f2d1b2d #x3a9e1d1da4b99eb9
                                    #xb0745858c49c749c #x682e343446722e72
                                    #x6c2d363641772d77 #xa3b2dcdc11cdb2cd
                                    #x73eeb4b49d29ee29 #xb6fb5b5b4d16fb16
                                    #x53f6a4a4a501f601 #xec4d7676a1d74dd7
                                    #x7561b7b714a361a3 #xface7d7d3449ce49
                                    #xa47b5252df8d7b8d #xa13edddd9f423e42
                                    #xbc715e5ecd937193 #x26971313b1a297a2
                                    #x57f5a6a6a204f504 #x6968b9b901b868b8
                                    #x0000000000000000 #x992cc1c1b5742c74
                                    #x80604040e0a060a0 #xdd1fe3e3c2211f21
                                    #xf2c879793a43c843 #x77edb6b69a2ced2c
                                    #xb3bed4d40dd9bed9 #x01468d8d47ca46ca
                                    #xced967671770d970 #xe44b7272afdd4bdd
                                    #x33de9494ed79de79 #x2bd49898ff67d467
                                    #x7be8b0b09323e823 #x114a85855bde4ade
                                    #x6d6bbbbb06bd6bbd #x912ac5c5bb7e2a7e
                                    #x9ee54f4f7b34e534 #xc116ededd73a163a
                                    #x17c58686d254c554 #x2fd79a9af862d762
                                    #xcc55666699ff55ff #x22941111b6a794a7
                                    #x0fcf8a8ac04acf4a #xc910e9e9d9301030
                                    #x080604040e0a060a #xe781fefe66988198
                                    #x5bf0a0a0ab0bf00b #xf0447878b4cc44cc
                                    #x4aba2525f0d5bad5 #x96e34b4b753ee33e
                                    #x5ff3a2a2ac0ef30e #xbafe5d5d4419fe19
                                    #x1bc08080db5bc05b #x0a8a050580858a85
                                    #x7ead3f3fd3ecadec #x42bc2121fedfbcdf
                                    #xe0487070a8d848d8 #xf904f1f1fd0c040c
                                    #xc6df6363197adf7a #xeec177772f58c158
                                    #x4575afaf309f759f #x84634242e7a563a5
                                    #x4030202070503050 #xd11ae5e5cb2e1a2e
                                    #xe10efdfdef120e12 #x656dbfbf08b76db7
                                    #x194c818155d44cd4 #x30141818243c143c
                                    #x4c352626795f355f #x9d2fc3c3b2712f71
                                    #x67e1bebe8638e138 #x6aa23535c8fda2fd
                                    #x0bcc8888c74fcc4f #x5c392e2e654b394b
                                    #x3d5793936af957f9 #xaaf25555580df20d
                                    #xe382fcfc619d829d #xf4477a7ab3c947c9
                                    #x8bacc8c827efacef #x6fe7baba8832e732
                                    #x642b32324f7d2b7d #xd795e6e642a495a4
                                    #x9ba0c0c03bfba0fb #x32981919aab398b3
                                    #x27d19e9ef668d168 #x5d7fa3a322817f81
                                    #x88664444eeaa66aa #xa87e5454d6827e82
                                    #x76ab3b3bdde6abe6 #x16830b0b959e839e
                                    #x03ca8c8cc945ca45 #x9529c7c7bc7b297b
                                    #xd6d36b6b056ed36e #x503c28286c443c44
                                    #x5579a7a72c8b798b #x63e2bcbc813de23d
                                    #x2c1d161631271d27 #x4176adad379a769a
                                    #xad3bdbdb964d3b4d #xc85664649efa56fa
                                    #xe84e7474a6d24ed2 #x281e141436221e22
                                    #x3fdb9292e476db76 #x180a0c0c121e0a1e
                                    #x906c4848fcb46cb4 #x6be4b8b88f37e437
                                    #x255d9f9f78e75de7 #x616ebdbd0fb26eb2
                                    #x86ef4343692aef2a #x93a6c4c435f1a6f1
                                    #x72a83939dae3a8e3 #x62a43131c6f7a4f7
                                    #xbd37d3d38a593759 #xff8bf2f274868b86
                                    #xb132d5d583563256 #x0d438b8b4ec543c5
                                    #xdc596e6e85eb59eb #xafb7dada18c2b7c2
                                    #x028c01018e8f8c8f #x7964b1b11dac64ac
                                    #x23d29c9cf16dd26d #x92e04949723be03b
                                    #xabb4d8d81fc7b4c7 #x43faacacb915fa15
                                    #xfd07f3f3fa090709 #x8525cfcfa06f256f
                                    #x8fafcaca20eaafea #xf38ef4f47d898e89
                                    #x8ee947476720e920 #x2018101038281828
                                    #xded56f6f0b64d564 #xfb88f0f073838883
                                    #x946f4a4afbb16fb1 #xb8725c5cca967296
                                    #x70243838546c246c #xaef157575f08f108
                                    #xe6c773732152c752 #x3551979764f351f3
                                    #x8d23cbcbae652365 #x597ca1a125847c84
                                    #xcb9ce8e857bf9cbf #x7c213e3e5d632163
                                    #x37dd9696ea7cdd7c #xc2dc61611e7fdc7f
                                    #x1a860d0d9c918691 #x1e850f0f9b948594
                                    #xdb90e0e04bab90ab #xf8427c7cbac642c6
                                    #xe2c471712657c457 #x83aacccc29e5aae5
                                    #x3bd89090e373d873 #x0c050606090f050f
                                    #xf501f7f7f4030103 #x38121c1c2a361236
                                    #x9fa3c2c23cfea3fe #xd45f6a6a8be15fe1
                                    #x47f9aeaebe10f910 #xd2d06969026bd06b
                                    #x2e911717bfa891a8 #x2958999971e858e8
                                    #x74273a3a53692769 #x4eb92727f7d0b9d0
                                    #xa938d9d991483848 #xcd13ebebde351335
                                    #x56b32b2be5ceb3ce #x4433222277553355
                                    #xbfbbd2d204d6bbd6 #x4970a9a939907090
                                    #x0e89070787808980 #x66a73333c1f2a7f2
                                    #x5ab62d2decc1b6c1 #x78223c3c5a662266
                                    #x2a921515b8ad92ad #x8920c9c9a9602060
                                    #x154987875cdb49db #x4fffaaaab01aff1a
                                    #xa0785050d8887888 #x517aa5a52b8e7a8e
                                    #x068f0303898a8f8a #xb2f859594a13f813
                                    #x12800909929b809b #x34171a1a23391739
                                    #xcada65651075da75 #xb531d7d784533153
                                    #x13c68484d551c651 #xbbb8d0d003d3b8d3
                                    #x1fc38282dc5ec35e #x52b02929e2cbb0cb
                                    #xb4775a5ac3997799 #x3c111e1e2d331133
                                    #xf6cb7b7b3d46cb46 #x4bfca8a8b71ffc1f
                                    #xdad66d6d0c61d661 #x583a2c2c624e3a4e
                                    
                                    #xf497a5c6c632f4a5 #x97eb84f8f86f9784
                                    #xb0c799eeee5eb099 #x8cf78df6f67a8c8d
                                    #x17e50dffffe8170d #xdcb7bdd6d60adcbd
                                    #xc8a7b1dede16c8b1 #xfc395491916dfc54
                                    #xf0c050606090f050 #x0504030202070503
                                    #xe087a9cece2ee0a9 #x87ac7d5656d1877d
                                    #x2bd519e7e7cc2b19 #xa67162b5b513a662
                                    #x319ae64d4d7c31e6 #xb5c39aecec59b59a
                                    #xcf05458f8f40cf45 #xbc3e9d1f1fa3bc9d
                                    #xc00940898949c040 #x92ef87fafa689287
                                    #x3fc515efefd03f15 #x267febb2b29426eb
                                    #x4007c98e8ece40c9 #x1ded0bfbfbe61d0b
                                    #x2f82ec41416e2fec #xa97d67b3b31aa967
                                    #x1cbefd5f5f431cfd #x258aea45456025ea
                                    #xda46bf2323f9dabf #x02a6f753535102f7
                                    #xa1d396e4e445a196 #xed2d5b9b9b76ed5b
                                    #x5deac27575285dc2 #x24d91ce1e1c5241c
                                    #xe97aae3d3dd4e9ae #xbe986a4c4cf2be6a
                                    #xeed85a6c6c82ee5a #xc3fc417e7ebdc341
                                    #x06f102f5f5f30602 #xd11d4f838352d14f
                                    #xe4d05c68688ce45c #x07a2f451515607f4
                                    #x5cb934d1d18d5c34 #x18e908f9f9e11808
                                    #xaedf93e2e24cae93 #x954d73abab3e9573
                                    #xf5c453626297f553 #x41543f2a2a6b413f
                                    #x14100c08081c140c #xf63152959563f652
                                    #xaf8c654646e9af65 #xe2215e9d9d7fe25e
                                    #x7860283030487828 #xf86ea13737cff8a1
                                    #x11140f0a0a1b110f #xc45eb52f2febc4b5
                                    #x1b1c090e0e151b09 #x5a483624247e5a36
                                    #xb6369b1b1badb69b #x47a53ddfdf98473d
                                    #x6a8126cdcda76a26 #xbb9c694e4ef5bb69
                                    #x4cfecd7f7f334ccd #xbacf9feaea50ba9f
                                    #x2d241b12123f2d1b #xb93a9e1d1da4b99e
                                    #x9cb0745858c49c74 #x72682e343446722e
                                    #x776c2d363641772d #xcda3b2dcdc11cdb2
                                    #x2973eeb4b49d29ee #x16b6fb5b5b4d16fb
                                    #x0153f6a4a4a501f6 #xd7ec4d7676a1d74d
                                    #xa37561b7b714a361 #x49face7d7d3449ce
                                    #x8da47b5252df8d7b #x42a13edddd9f423e
                                    #x93bc715e5ecd9371 #xa226971313b1a297
                                    #x0457f5a6a6a204f5 #xb86968b9b901b868
                                    #x0000000000000000 #x74992cc1c1b5742c
                                    #xa080604040e0a060 #x21dd1fe3e3c2211f
                                    #x43f2c879793a43c8 #x2c77edb6b69a2ced
                                    #xd9b3bed4d40dd9be #xca01468d8d47ca46
                                    #x70ced967671770d9 #xdde44b7272afdd4b
                                    #x7933de9494ed79de #x672bd49898ff67d4
                                    #x237be8b0b09323e8 #xde114a85855bde4a
                                    #xbd6d6bbbbb06bd6b #x7e912ac5c5bb7e2a
                                    #x349ee54f4f7b34e5 #x3ac116ededd73a16
                                    #x5417c58686d254c5 #x622fd79a9af862d7
                                    #xffcc55666699ff55 #xa722941111b6a794
                                    #x4a0fcf8a8ac04acf #x30c910e9e9d93010
                                    #x0a080604040e0a06 #x98e781fefe669881
                                    #x0b5bf0a0a0ab0bf0 #xccf0447878b4cc44
                                    #xd54aba2525f0d5ba #x3e96e34b4b753ee3
                                    #x0e5ff3a2a2ac0ef3 #x19bafe5d5d4419fe
                                    #x5b1bc08080db5bc0 #x850a8a050580858a
                                    #xec7ead3f3fd3ecad #xdf42bc2121fedfbc
                                    #xd8e0487070a8d848 #x0cf904f1f1fd0c04
                                    #x7ac6df6363197adf #x58eec177772f58c1
                                    #x9f4575afaf309f75 #xa584634242e7a563
                                    #x5040302020705030 #x2ed11ae5e5cb2e1a
                                    #x12e10efdfdef120e #xb7656dbfbf08b76d
                                    #xd4194c818155d44c #x3c30141818243c14
                                    #x5f4c352626795f35 #x719d2fc3c3b2712f
                                    #x3867e1bebe8638e1 #xfd6aa23535c8fda2
                                    #x4f0bcc8888c74fcc #x4b5c392e2e654b39
                                    #xf93d5793936af957 #x0daaf25555580df2
                                    #x9de382fcfc619d82 #xc9f4477a7ab3c947
                                    #xef8bacc8c827efac #x326fe7baba8832e7
                                    #x7d642b32324f7d2b #xa4d795e6e642a495
                                    #xfb9ba0c0c03bfba0 #xb332981919aab398
                                    #x6827d19e9ef668d1 #x815d7fa3a322817f
                                    #xaa88664444eeaa66 #x82a87e5454d6827e
                                    #xe676ab3b3bdde6ab #x9e16830b0b959e83
                                    #x4503ca8c8cc945ca #x7b9529c7c7bc7b29
                                    #x6ed6d36b6b056ed3 #x44503c28286c443c
                                    #x8b5579a7a72c8b79 #x3d63e2bcbc813de2
                                    #x272c1d161631271d #x9a4176adad379a76
                                    #x4dad3bdbdb964d3b #xfac85664649efa56
                                    #xd2e84e7474a6d24e #x22281e141436221e
                                    #x763fdb9292e476db #x1e180a0c0c121e0a
                                    #xb4906c4848fcb46c #x376be4b8b88f37e4
                                    #xe7255d9f9f78e75d #xb2616ebdbd0fb26e
                                    #x2a86ef4343692aef #xf193a6c4c435f1a6
                                    #xe372a83939dae3a8 #xf762a43131c6f7a4
                                    #x59bd37d3d38a5937 #x86ff8bf2f274868b
                                    #x56b132d5d5835632 #xc50d438b8b4ec543
                                    #xebdc596e6e85eb59 #xc2afb7dada18c2b7
                                    #x8f028c01018e8f8c #xac7964b1b11dac64
                                    #x6d23d29c9cf16dd2 #x3b92e04949723be0
                                    #xc7abb4d8d81fc7b4 #x1543faacacb915fa
                                    #x09fd07f3f3fa0907 #x6f8525cfcfa06f25
                                    #xea8fafcaca20eaaf #x89f38ef4f47d898e
                                    #x208ee947476720e9 #x2820181010382818
                                    #x64ded56f6f0b64d5 #x83fb88f0f0738388
                                    #xb1946f4a4afbb16f #x96b8725c5cca9672
                                    #x6c70243838546c24 #x08aef157575f08f1
                                    #x52e6c773732152c7 #xf33551979764f351
                                    #x658d23cbcbae6523 #x84597ca1a125847c
                                    #xbfcb9ce8e857bf9c #x637c213e3e5d6321
                                    #x7c37dd9696ea7cdd #x7fc2dc61611e7fdc
                                    #x911a860d0d9c9186 #x941e850f0f9b9485
                                    #xabdb90e0e04bab90 #xc6f8427c7cbac642
                                    #x57e2c471712657c4 #xe583aacccc29e5aa
                                    #x733bd89090e373d8 #x0f0c050606090f05
                                    #x03f501f7f7f40301 #x3638121c1c2a3612
                                    #xfe9fa3c2c23cfea3 #xe1d45f6a6a8be15f
                                    #x1047f9aeaebe10f9 #x6bd2d06969026bd0
                                    #xa82e911717bfa891 #xe82958999971e858
                                    #x6974273a3a536927 #xd04eb92727f7d0b9
                                    #x48a938d9d9914838 #x35cd13ebebde3513
                                    #xce56b32b2be5ceb3 #x5544332222775533
                                    #xd6bfbbd2d204d6bb #x904970a9a9399070
                                    #x800e890707878089 #xf266a73333c1f2a7
                                    #xc15ab62d2decc1b6 #x6678223c3c5a6622
                                    #xad2a921515b8ad92 #x608920c9c9a96020
                                    #xdb154987875cdb49 #x1a4fffaaaab01aff
                                    #x88a0785050d88878 #x8e517aa5a52b8e7a
                                    #x8a068f0303898a8f #x13b2f859594a13f8
                                    #x9b12800909929b80 #x3934171a1a233917
                                    #x75cada65651075da #x53b531d7d7845331
                                    #x5113c68484d551c6 #xd3bbb8d0d003d3b8
                                    #x5e1fc38282dc5ec3 #xcb52b02929e2cbb0
                                    #x99b4775a5ac39977 #x333c111e1e2d3311
                                    #x46f6cb7b7b3d46cb #x1f4bfca8a8b71ffc
                                    #x61dad66d6d0c61d6 #x4e583a2c2c624e3a
                                    
                                    #xa5f497a5c6c632f4 #x8497eb84f8f86f97
                                    #x99b0c799eeee5eb0 #x8d8cf78df6f67a8c
                                    #x0d17e50dffffe817 #xbddcb7bdd6d60adc
                                    #xb1c8a7b1dede16c8 #x54fc395491916dfc
                                    #x50f0c050606090f0 #x0305040302020705
                                    #xa9e087a9cece2ee0 #x7d87ac7d5656d187
                                    #x192bd519e7e7cc2b #x62a67162b5b513a6
                                    #xe6319ae64d4d7c31 #x9ab5c39aecec59b5
                                    #x45cf05458f8f40cf #x9dbc3e9d1f1fa3bc
                                    #x40c00940898949c0 #x8792ef87fafa6892
                                    #x153fc515efefd03f #xeb267febb2b29426
                                    #xc94007c98e8ece40 #x0b1ded0bfbfbe61d
                                    #xec2f82ec41416e2f #x67a97d67b3b31aa9
                                    #xfd1cbefd5f5f431c #xea258aea45456025
                                    #xbfda46bf2323f9da #xf702a6f753535102
                                    #x96a1d396e4e445a1 #x5bed2d5b9b9b76ed
                                    #xc25deac27575285d #x1c24d91ce1e1c524
                                    #xaee97aae3d3dd4e9 #x6abe986a4c4cf2be
                                    #x5aeed85a6c6c82ee #x41c3fc417e7ebdc3
                                    #x0206f102f5f5f306 #x4fd11d4f838352d1
                                    #x5ce4d05c68688ce4 #xf407a2f451515607
                                    #x345cb934d1d18d5c #x0818e908f9f9e118
                                    #x93aedf93e2e24cae #x73954d73abab3e95
                                    #x53f5c453626297f5 #x3f41543f2a2a6b41
                                    #x0c14100c08081c14 #x52f63152959563f6
                                    #x65af8c654646e9af #x5ee2215e9d9d7fe2
                                    #x2878602830304878 #xa1f86ea13737cff8
                                    #x0f11140f0a0a1b11 #xb5c45eb52f2febc4
                                    #x091b1c090e0e151b #x365a483624247e5a
                                    #x9bb6369b1b1badb6 #x3d47a53ddfdf9847
                                    #x266a8126cdcda76a #x69bb9c694e4ef5bb
                                    #xcd4cfecd7f7f334c #x9fbacf9feaea50ba
                                    #x1b2d241b12123f2d #x9eb93a9e1d1da4b9
                                    #x749cb0745858c49c #x2e72682e34344672
                                    #x2d776c2d36364177 #xb2cda3b2dcdc11cd
                                    #xee2973eeb4b49d29 #xfb16b6fb5b5b4d16
                                    #xf60153f6a4a4a501 #x4dd7ec4d7676a1d7
                                    #x61a37561b7b714a3 #xce49face7d7d3449
                                    #x7b8da47b5252df8d #x3e42a13edddd9f42
                                    #x7193bc715e5ecd93 #x97a226971313b1a2
                                    #xf50457f5a6a6a204 #x68b86968b9b901b8
                                    #x0000000000000000 #x2c74992cc1c1b574
                                    #x60a080604040e0a0 #x1f21dd1fe3e3c221
                                    #xc843f2c879793a43 #xed2c77edb6b69a2c
                                    #xbed9b3bed4d40dd9 #x46ca01468d8d47ca
                                    #xd970ced967671770 #x4bdde44b7272afdd
                                    #xde7933de9494ed79 #xd4672bd49898ff67
                                    #xe8237be8b0b09323 #x4ade114a85855bde
                                    #x6bbd6d6bbbbb06bd #x2a7e912ac5c5bb7e
                                    #xe5349ee54f4f7b34 #x163ac116ededd73a
                                    #xc55417c58686d254 #xd7622fd79a9af862
                                    #x55ffcc55666699ff #x94a722941111b6a7
                                    #xcf4a0fcf8a8ac04a #x1030c910e9e9d930
                                    #x060a080604040e0a #x8198e781fefe6698
                                    #xf00b5bf0a0a0ab0b #x44ccf0447878b4cc
                                    #xbad54aba2525f0d5 #xe33e96e34b4b753e
                                    #xf30e5ff3a2a2ac0e #xfe19bafe5d5d4419
                                    #xc05b1bc08080db5b #x8a850a8a05058085
                                    #xadec7ead3f3fd3ec #xbcdf42bc2121fedf
                                    #x48d8e0487070a8d8 #x040cf904f1f1fd0c
                                    #xdf7ac6df6363197a #xc158eec177772f58
                                    #x759f4575afaf309f #x63a584634242e7a5
                                    #x3050403020207050 #x1a2ed11ae5e5cb2e
                                    #x0e12e10efdfdef12 #x6db7656dbfbf08b7
                                    #x4cd4194c818155d4 #x143c30141818243c
                                    #x355f4c352626795f #x2f719d2fc3c3b271
                                    #xe13867e1bebe8638 #xa2fd6aa23535c8fd
                                    #xcc4f0bcc8888c74f #x394b5c392e2e654b
                                    #x57f93d5793936af9 #xf20daaf25555580d
                                    #x829de382fcfc619d #x47c9f4477a7ab3c9
                                    #xacef8bacc8c827ef #xe7326fe7baba8832
                                    #x2b7d642b32324f7d #x95a4d795e6e642a4
                                    #xa0fb9ba0c0c03bfb #x98b332981919aab3
                                    #xd16827d19e9ef668 #x7f815d7fa3a32281
                                    #x66aa88664444eeaa #x7e82a87e5454d682
                                    #xabe676ab3b3bdde6 #x839e16830b0b959e
                                    #xca4503ca8c8cc945 #x297b9529c7c7bc7b
                                    #xd36ed6d36b6b056e #x3c44503c28286c44
                                    #x798b5579a7a72c8b #xe23d63e2bcbc813d
                                    #x1d272c1d16163127 #x769a4176adad379a
                                    #x3b4dad3bdbdb964d #x56fac85664649efa
                                    #x4ed2e84e7474a6d2 #x1e22281e14143622
                                    #xdb763fdb9292e476 #x0a1e180a0c0c121e
                                    #x6cb4906c4848fcb4 #xe4376be4b8b88f37
                                    #x5de7255d9f9f78e7 #x6eb2616ebdbd0fb2
                                    #xef2a86ef4343692a #xa6f193a6c4c435f1
                                    #xa8e372a83939dae3 #xa4f762a43131c6f7
                                    #x3759bd37d3d38a59 #x8b86ff8bf2f27486
                                    #x3256b132d5d58356 #x43c50d438b8b4ec5
                                    #x59ebdc596e6e85eb #xb7c2afb7dada18c2
                                    #x8c8f028c01018e8f #x64ac7964b1b11dac
                                    #xd26d23d29c9cf16d #xe03b92e04949723b
                                    #xb4c7abb4d8d81fc7 #xfa1543faacacb915
                                    #x0709fd07f3f3fa09 #x256f8525cfcfa06f
                                    #xafea8fafcaca20ea #x8e89f38ef4f47d89
                                    #xe9208ee947476720 #x1828201810103828
                                    #xd564ded56f6f0b64 #x8883fb88f0f07383
                                    #x6fb1946f4a4afbb1 #x7296b8725c5cca96
                                    #x246c70243838546c #xf108aef157575f08
                                    #xc752e6c773732152 #x51f33551979764f3
                                    #x23658d23cbcbae65 #x7c84597ca1a12584
                                    #x9cbfcb9ce8e857bf #x21637c213e3e5d63
                                    #xdd7c37dd9696ea7c #xdc7fc2dc61611e7f
                                    #x86911a860d0d9c91 #x85941e850f0f9b94
                                    #x90abdb90e0e04bab #x42c6f8427c7cbac6
                                    #xc457e2c471712657 #xaae583aacccc29e5
                                    #xd8733bd89090e373 #x050f0c050606090f
                                    #x0103f501f7f7f403 #x123638121c1c2a36
                                    #xa3fe9fa3c2c23cfe #x5fe1d45f6a6a8be1
                                    #xf91047f9aeaebe10 #xd06bd2d06969026b
                                    #x91a82e911717bfa8 #x58e82958999971e8
                                    #x276974273a3a5369 #xb9d04eb92727f7d0
                                    #x3848a938d9d99148 #x1335cd13ebebde35
                                    #xb3ce56b32b2be5ce #x3355443322227755
                                    #xbbd6bfbbd2d204d6 #x70904970a9a93990
                                    #x89800e8907078780 #xa7f266a73333c1f2
                                    #xb6c15ab62d2decc1 #x226678223c3c5a66
                                    #x92ad2a921515b8ad #x20608920c9c9a960
                                    #x49db154987875cdb #xff1a4fffaaaab01a
                                    #x7888a0785050d888 #x7a8e517aa5a52b8e
                                    #x8f8a068f0303898a #xf813b2f859594a13
                                    #x809b12800909929b #x173934171a1a2339
                                    #xda75cada65651075 #x3153b531d7d78453
                                    #xc65113c68484d551 #xb8d3bbb8d0d003d3
                                    #xc35e1fc38282dc5e #xb0cb52b02929e2cb
                                    #x7799b4775a5ac399 #x11333c111e1e2d33
                                    #xcb46f6cb7b7b3d46 #xfc1f4bfca8a8b71f
                                    #xd661dad66d6d0c61 #x3a4e583a2c2c624e
                                    
                                    #xf4a5f497a5c6c632 #x978497eb84f8f86f
                                    #xb099b0c799eeee5e #x8c8d8cf78df6f67a
                                    #x170d17e50dffffe8 #xdcbddcb7bdd6d60a
                                    #xc8b1c8a7b1dede16 #xfc54fc395491916d
                                    #xf050f0c050606090 #x0503050403020207
                                    #xe0a9e087a9cece2e #x877d87ac7d5656d1
                                    #x2b192bd519e7e7cc #xa662a67162b5b513
                                    #x31e6319ae64d4d7c #xb59ab5c39aecec59
                                    #xcf45cf05458f8f40 #xbc9dbc3e9d1f1fa3
                                    #xc040c00940898949 #x928792ef87fafa68
                                    #x3f153fc515efefd0 #x26eb267febb2b294
                                    #x40c94007c98e8ece #x1d0b1ded0bfbfbe6
                                    #x2fec2f82ec41416e #xa967a97d67b3b31a
                                    #x1cfd1cbefd5f5f43 #x25ea258aea454560
                                    #xdabfda46bf2323f9 #x02f702a6f7535351
                                    #xa196a1d396e4e445 #xed5bed2d5b9b9b76
                                    #x5dc25deac2757528 #x241c24d91ce1e1c5
                                    #xe9aee97aae3d3dd4 #xbe6abe986a4c4cf2
                                    #xee5aeed85a6c6c82 #xc341c3fc417e7ebd
                                    #x060206f102f5f5f3 #xd14fd11d4f838352
                                    #xe45ce4d05c68688c #x07f407a2f4515156
                                    #x5c345cb934d1d18d #x180818e908f9f9e1
                                    #xae93aedf93e2e24c #x9573954d73abab3e
                                    #xf553f5c453626297 #x413f41543f2a2a6b
                                    #x140c14100c08081c #xf652f63152959563
                                    #xaf65af8c654646e9 #xe25ee2215e9d9d7f
                                    #x7828786028303048 #xf8a1f86ea13737cf
                                    #x110f11140f0a0a1b #xc4b5c45eb52f2feb
                                    #x1b091b1c090e0e15 #x5a365a483624247e
                                    #xb69bb6369b1b1bad #x473d47a53ddfdf98
                                    #x6a266a8126cdcda7 #xbb69bb9c694e4ef5
                                    #x4ccd4cfecd7f7f33 #xba9fbacf9feaea50
                                    #x2d1b2d241b12123f #xb99eb93a9e1d1da4
                                    #x9c749cb0745858c4 #x722e72682e343446
                                    #x772d776c2d363641 #xcdb2cda3b2dcdc11
                                    #x29ee2973eeb4b49d #x16fb16b6fb5b5b4d
                                    #x01f60153f6a4a4a5 #xd74dd7ec4d7676a1
                                    #xa361a37561b7b714 #x49ce49face7d7d34
                                    #x8d7b8da47b5252df #x423e42a13edddd9f
                                    #x937193bc715e5ecd #xa297a226971313b1
                                    #x04f50457f5a6a6a2 #xb868b86968b9b901
                                    #x0000000000000000 #x742c74992cc1c1b5
                                    #xa060a080604040e0 #x211f21dd1fe3e3c2
                                    #x43c843f2c879793a #x2ced2c77edb6b69a
                                    #xd9bed9b3bed4d40d #xca46ca01468d8d47
                                    #x70d970ced9676717 #xdd4bdde44b7272af
                                    #x79de7933de9494ed #x67d4672bd49898ff
                                    #x23e8237be8b0b093 #xde4ade114a85855b
                                    #xbd6bbd6d6bbbbb06 #x7e2a7e912ac5c5bb
                                    #x34e5349ee54f4f7b #x3a163ac116ededd7
                                    #x54c55417c58686d2 #x62d7622fd79a9af8
                                    #xff55ffcc55666699 #xa794a722941111b6
                                    #x4acf4a0fcf8a8ac0 #x301030c910e9e9d9
                                    #x0a060a080604040e #x988198e781fefe66
                                    #x0bf00b5bf0a0a0ab #xcc44ccf0447878b4
                                    #xd5bad54aba2525f0 #x3ee33e96e34b4b75
                                    #x0ef30e5ff3a2a2ac #x19fe19bafe5d5d44
                                    #x5bc05b1bc08080db #x858a850a8a050580
                                    #xecadec7ead3f3fd3 #xdfbcdf42bc2121fe
                                    #xd848d8e0487070a8 #x0c040cf904f1f1fd
                                    #x7adf7ac6df636319 #x58c158eec177772f
                                    #x9f759f4575afaf30 #xa563a584634242e7
                                    #x5030504030202070 #x2e1a2ed11ae5e5cb
                                    #x120e12e10efdfdef #xb76db7656dbfbf08
                                    #xd44cd4194c818155 #x3c143c3014181824
                                    #x5f355f4c35262679 #x712f719d2fc3c3b2
                                    #x38e13867e1bebe86 #xfda2fd6aa23535c8
                                    #x4fcc4f0bcc8888c7 #x4b394b5c392e2e65
                                    #xf957f93d5793936a #x0df20daaf2555558
                                    #x9d829de382fcfc61 #xc947c9f4477a7ab3
                                    #xefacef8bacc8c827 #x32e7326fe7baba88
                                    #x7d2b7d642b32324f #xa495a4d795e6e642
                                    #xfba0fb9ba0c0c03b #xb398b332981919aa
                                    #x68d16827d19e9ef6 #x817f815d7fa3a322
                                    #xaa66aa88664444ee #x827e82a87e5454d6
                                    #xe6abe676ab3b3bdd #x9e839e16830b0b95
                                    #x45ca4503ca8c8cc9 #x7b297b9529c7c7bc
                                    #x6ed36ed6d36b6b05 #x443c44503c28286c
                                    #x8b798b5579a7a72c #x3de23d63e2bcbc81
                                    #x271d272c1d161631 #x9a769a4176adad37
                                    #x4d3b4dad3bdbdb96 #xfa56fac85664649e
                                    #xd24ed2e84e7474a6 #x221e22281e141436
                                    #x76db763fdb9292e4 #x1e0a1e180a0c0c12
                                    #xb46cb4906c4848fc #x37e4376be4b8b88f
                                    #xe75de7255d9f9f78 #xb26eb2616ebdbd0f
                                    #x2aef2a86ef434369 #xf1a6f193a6c4c435
                                    #xe3a8e372a83939da #xf7a4f762a43131c6
                                    #x593759bd37d3d38a #x868b86ff8bf2f274
                                    #x563256b132d5d583 #xc543c50d438b8b4e
                                    #xeb59ebdc596e6e85 #xc2b7c2afb7dada18
                                    #x8f8c8f028c01018e #xac64ac7964b1b11d
                                    #x6dd26d23d29c9cf1 #x3be03b92e0494972
                                    #xc7b4c7abb4d8d81f #x15fa1543faacacb9
                                    #x090709fd07f3f3fa #x6f256f8525cfcfa0
                                    #xeaafea8fafcaca20 #x898e89f38ef4f47d
                                    #x20e9208ee9474767 #x2818282018101038
                                    #x64d564ded56f6f0b #x838883fb88f0f073
                                    #xb16fb1946f4a4afb #x967296b8725c5cca
                                    #x6c246c7024383854 #x08f108aef157575f
                                    #x52c752e6c7737321 #xf351f33551979764
                                    #x6523658d23cbcbae #x847c84597ca1a125
                                    #xbf9cbfcb9ce8e857 #x6321637c213e3e5d
                                    #x7cdd7c37dd9696ea #x7fdc7fc2dc61611e
                                    #x9186911a860d0d9c #x9485941e850f0f9b
                                    #xab90abdb90e0e04b #xc642c6f8427c7cba
                                    #x57c457e2c4717126 #xe5aae583aacccc29
                                    #x73d8733bd89090e3 #x0f050f0c05060609
                                    #x030103f501f7f7f4 #x36123638121c1c2a
                                    #xfea3fe9fa3c2c23c #xe15fe1d45f6a6a8b
                                    #x10f91047f9aeaebe #x6bd06bd2d0696902
                                    #xa891a82e911717bf #xe858e82958999971
                                    #x69276974273a3a53 #xd0b9d04eb92727f7
                                    #x483848a938d9d991 #x351335cd13ebebde
                                    #xceb3ce56b32b2be5 #x5533554433222277
                                    #xd6bbd6bfbbd2d204 #x9070904970a9a939
                                    #x8089800e89070787 #xf2a7f266a73333c1
                                    #xc1b6c15ab62d2dec #x66226678223c3c5a
                                    #xad92ad2a921515b8 #x6020608920c9c9a9
                                    #xdb49db154987875c #x1aff1a4fffaaaab0
                                    #x887888a0785050d8 #x8e7a8e517aa5a52b
                                    #x8a8f8a068f030389 #x13f813b2f859594a
                                    #x9b809b1280090992 #x39173934171a1a23
                                    #x75da75cada656510 #x533153b531d7d784
                                    #x51c65113c68484d5 #xd3b8d3bbb8d0d003
                                    #x5ec35e1fc38282dc #xcbb0cb52b02929e2
                                    #x997799b4775a5ac3 #x3311333c111e1e2d
                                    #x46cb46f6cb7b7b3d #x1ffc1f4bfca8a8b7
                                    #x61d661dad66d6d0c #x4e3a4e583a2c2c62
                                    
                                    #x32f4a5f497a5c6c6 #x6f978497eb84f8f8
                                    #x5eb099b0c799eeee #x7a8c8d8cf78df6f6
                                    #xe8170d17e50dffff #x0adcbddcb7bdd6d6
                                    #x16c8b1c8a7b1dede #x6dfc54fc39549191
                                    #x90f050f0c0506060 #x0705030504030202
                                    #x2ee0a9e087a9cece #xd1877d87ac7d5656
                                    #xcc2b192bd519e7e7 #x13a662a67162b5b5
                                    #x7c31e6319ae64d4d #x59b59ab5c39aecec
                                    #x40cf45cf05458f8f #xa3bc9dbc3e9d1f1f
                                    #x49c040c009408989 #x68928792ef87fafa
                                    #xd03f153fc515efef #x9426eb267febb2b2
                                    #xce40c94007c98e8e #xe61d0b1ded0bfbfb
                                    #x6e2fec2f82ec4141 #x1aa967a97d67b3b3
                                    #x431cfd1cbefd5f5f #x6025ea258aea4545
                                    #xf9dabfda46bf2323 #x5102f702a6f75353
                                    #x45a196a1d396e4e4 #x76ed5bed2d5b9b9b
                                    #x285dc25deac27575 #xc5241c24d91ce1e1
                                    #xd4e9aee97aae3d3d #xf2be6abe986a4c4c
                                    #x82ee5aeed85a6c6c #xbdc341c3fc417e7e
                                    #xf3060206f102f5f5 #x52d14fd11d4f8383
                                    #x8ce45ce4d05c6868 #x5607f407a2f45151
                                    #x8d5c345cb934d1d1 #xe1180818e908f9f9
                                    #x4cae93aedf93e2e2 #x3e9573954d73abab
                                    #x97f553f5c4536262 #x6b413f41543f2a2a
                                    #x1c140c14100c0808 #x63f652f631529595
                                    #xe9af65af8c654646 #x7fe25ee2215e9d9d
                                    #x4878287860283030 #xcff8a1f86ea13737
                                    #x1b110f11140f0a0a #xebc4b5c45eb52f2f
                                    #x151b091b1c090e0e #x7e5a365a48362424
                                    #xadb69bb6369b1b1b #x98473d47a53ddfdf
                                    #xa76a266a8126cdcd #xf5bb69bb9c694e4e
                                    #x334ccd4cfecd7f7f #x50ba9fbacf9feaea
                                    #x3f2d1b2d241b1212 #xa4b99eb93a9e1d1d
                                    #xc49c749cb0745858 #x46722e72682e3434
                                    #x41772d776c2d3636 #x11cdb2cda3b2dcdc
                                    #x9d29ee2973eeb4b4 #x4d16fb16b6fb5b5b
                                    #xa501f60153f6a4a4 #xa1d74dd7ec4d7676
                                    #x14a361a37561b7b7 #x3449ce49face7d7d
                                    #xdf8d7b8da47b5252 #x9f423e42a13edddd
                                    #xcd937193bc715e5e #xb1a297a226971313
                                    #xa204f50457f5a6a6 #x01b868b86968b9b9
                                    #x0000000000000000 #xb5742c74992cc1c1
                                    #xe0a060a080604040 #xc2211f21dd1fe3e3
                                    #x3a43c843f2c87979 #x9a2ced2c77edb6b6
                                    #x0dd9bed9b3bed4d4 #x47ca46ca01468d8d
                                    #x1770d970ced96767 #xafdd4bdde44b7272
                                    #xed79de7933de9494 #xff67d4672bd49898
                                    #x9323e8237be8b0b0 #x5bde4ade114a8585
                                    #x06bd6bbd6d6bbbbb #xbb7e2a7e912ac5c5
                                    #x7b34e5349ee54f4f #xd73a163ac116eded
                                    #xd254c55417c58686 #xf862d7622fd79a9a
                                    #x99ff55ffcc556666 #xb6a794a722941111
                                    #xc04acf4a0fcf8a8a #xd9301030c910e9e9
                                    #x0e0a060a08060404 #x66988198e781fefe
                                    #xab0bf00b5bf0a0a0 #xb4cc44ccf0447878
                                    #xf0d5bad54aba2525 #x753ee33e96e34b4b
                                    #xac0ef30e5ff3a2a2 #x4419fe19bafe5d5d
                                    #xdb5bc05b1bc08080 #x80858a850a8a0505
                                    #xd3ecadec7ead3f3f #xfedfbcdf42bc2121
                                    #xa8d848d8e0487070 #xfd0c040cf904f1f1
                                    #x197adf7ac6df6363 #x2f58c158eec17777
                                    #x309f759f4575afaf #xe7a563a584634242
                                    #x7050305040302020 #xcb2e1a2ed11ae5e5
                                    #xef120e12e10efdfd #x08b76db7656dbfbf
                                    #x55d44cd4194c8181 #x243c143c30141818
                                    #x795f355f4c352626 #xb2712f719d2fc3c3
                                    #x8638e13867e1bebe #xc8fda2fd6aa23535
                                    #xc74fcc4f0bcc8888 #x654b394b5c392e2e
                                    #x6af957f93d579393 #x580df20daaf25555
                                    #x619d829de382fcfc #xb3c947c9f4477a7a
                                    #x27efacef8bacc8c8 #x8832e7326fe7baba
                                    #x4f7d2b7d642b3232 #x42a495a4d795e6e6
                                    #x3bfba0fb9ba0c0c0 #xaab398b332981919
                                    #xf668d16827d19e9e #x22817f815d7fa3a3
                                    #xeeaa66aa88664444 #xd6827e82a87e5454
                                    #xdde6abe676ab3b3b #x959e839e16830b0b
                                    #xc945ca4503ca8c8c #xbc7b297b9529c7c7
                                    #x056ed36ed6d36b6b #x6c443c44503c2828
                                    #x2c8b798b5579a7a7 #x813de23d63e2bcbc
                                    #x31271d272c1d1616 #x379a769a4176adad
                                    #x964d3b4dad3bdbdb #x9efa56fac8566464
                                    #xa6d24ed2e84e7474 #x36221e22281e1414
                                    #xe476db763fdb9292 #x121e0a1e180a0c0c
                                    #xfcb46cb4906c4848 #x8f37e4376be4b8b8
                                    #x78e75de7255d9f9f #x0fb26eb2616ebdbd
                                    #x692aef2a86ef4343 #x35f1a6f193a6c4c4
                                    #xdae3a8e372a83939 #xc6f7a4f762a43131
                                    #x8a593759bd37d3d3 #x74868b86ff8bf2f2
                                    #x83563256b132d5d5 #x4ec543c50d438b8b
                                    #x85eb59ebdc596e6e #x18c2b7c2afb7dada
                                    #x8e8f8c8f028c0101 #x1dac64ac7964b1b1
                                    #xf16dd26d23d29c9c #x723be03b92e04949
                                    #x1fc7b4c7abb4d8d8 #xb915fa1543faacac
                                    #xfa090709fd07f3f3 #xa06f256f8525cfcf
                                    #x20eaafea8fafcaca #x7d898e89f38ef4f4
                                    #x6720e9208ee94747 #x3828182820181010
                                    #x0b64d564ded56f6f #x73838883fb88f0f0
                                    #xfbb16fb1946f4a4a #xca967296b8725c5c
                                    #x546c246c70243838 #x5f08f108aef15757
                                    #x2152c752e6c77373 #x64f351f335519797
                                    #xae6523658d23cbcb #x25847c84597ca1a1
                                    #x57bf9cbfcb9ce8e8 #x5d6321637c213e3e
                                    #xea7cdd7c37dd9696 #x1e7fdc7fc2dc6161
                                    #x9c9186911a860d0d #x9b9485941e850f0f
                                    #x4bab90abdb90e0e0 #xbac642c6f8427c7c
                                    #x2657c457e2c47171 #x29e5aae583aacccc
                                    #xe373d8733bd89090 #x090f050f0c050606
                                    #xf4030103f501f7f7 #x2a36123638121c1c
                                    #x3cfea3fe9fa3c2c2 #x8be15fe1d45f6a6a
                                    #xbe10f91047f9aeae #x026bd06bd2d06969
                                    #xbfa891a82e911717 #x71e858e829589999
                                    #x5369276974273a3a #xf7d0b9d04eb92727
                                    #x91483848a938d9d9 #xde351335cd13ebeb
                                    #xe5ceb3ce56b32b2b #x7755335544332222
                                    #x04d6bbd6bfbbd2d2 #x399070904970a9a9
                                    #x878089800e890707 #xc1f2a7f266a73333
                                    #xecc1b6c15ab62d2d #x5a66226678223c3c
                                    #xb8ad92ad2a921515 #xa96020608920c9c9
                                    #x5cdb49db15498787 #xb01aff1a4fffaaaa
                                    #xd8887888a0785050 #x2b8e7a8e517aa5a5
                                    #x898a8f8a068f0303 #x4a13f813b2f85959
                                    #x929b809b12800909 #x2339173934171a1a
                                    #x1075da75cada6565 #x84533153b531d7d7
                                    #xd551c65113c68484 #x03d3b8d3bbb8d0d0
                                    #xdc5ec35e1fc38282 #xe2cbb0cb52b02929
                                    #xc3997799b4775a5a #x2d3311333c111e1e
                                    #x3d46cb46f6cb7b7b #xb71ffc1f4bfca8a8
                                    #x0c61d661dad66d6d #x624e3a4e583a2c2c))))

(declaim (inline groestl-table)
         (ftype (function ((integer 0 2048)) (unsigned-byte 64)) groestl-table))
(defun groestl-table (i)
  (declare (type (integer 0 2048) i))
  (aref +groestl-table+ i))

;;;
;;; Rounds
;;;

(defmacro groestl-ext-byte (v n)
  `(logand (ash ,v (* 8 (- ,n 7))) #xff))

(defmacro groestl-column (x y i c0 c1 c2 c3 c4 c5 c6 c7)
  "Compute one new state column."
  `(setf (aref ,y ,i)
         (logxor (groestl-table (groestl-ext-byte (aref ,x ,c0) 0))
                 (groestl-table (+ 256 (groestl-ext-byte (aref ,x ,c1) 1)))
                 (groestl-table (+ 512 (groestl-ext-byte (aref ,x ,c2) 2)))
                 (groestl-table (+ 768 (groestl-ext-byte (aref ,x ,c3) 3)))
                 (groestl-table (+ 1024 (groestl-ext-byte (aref ,x ,c4) 4)))
                 (groestl-table (+ 1280 (groestl-ext-byte (aref ,x ,c5) 5)))
                 (groestl-table (+ 1536 (groestl-ext-byte (aref ,x ,c6) 6)))
                 (groestl-table (+ 1792 (groestl-ext-byte (aref ,x ,c7) 7))))))

(declaim (inline groestl-rnd512p)
         (ftype (function ((simple-array (unsigned-byte 64) (#.+groestl-cols-512+))
                           (simple-array (unsigned-byte 64) (#.+groestl-cols-512+))
                           (unsigned-byte 64)))
                groestl-end512p))
(defun groestl-rnd512p (x y r)
  "Compute a round in P (short variants)."
  (declare (type (simple-array (unsigned-byte 64) (#.+groestl-cols-512+)) x y)
           (type (unsigned-byte 64) r)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (setf (aref x 0) (logxor (aref x 0) #x0000000000000000 r)
        (aref x 1) (logxor (aref x 1) #x1000000000000000 r)
        (aref x 2) (logxor (aref x 2) #x2000000000000000 r)
        (aref x 3) (logxor (aref x 3) #x3000000000000000 r)
        (aref x 4) (logxor (aref x 4) #x4000000000000000 r)
        (aref x 5) (logxor (aref x 5) #x5000000000000000 r)
        (aref x 6) (logxor (aref x 6) #x6000000000000000 r)
        (aref x 7) (logxor (aref x 7) #x7000000000000000 r))
  (groestl-column x y 0 0 1 2 3 4 5 6 7)
  (groestl-column x y 1 1 2 3 4 5 6 7 0)
  (groestl-column x y 2 2 3 4 5 6 7 0 1)
  (groestl-column x y 3 3 4 5 6 7 0 1 2)
  (groestl-column x y 4 4 5 6 7 0 1 2 3)
  (groestl-column x y 5 5 6 7 0 1 2 3 4)
  (groestl-column x y 6 6 7 0 1 2 3 4 5)
  (groestl-column x y 7 7 0 1 2 3 4 5 6)
  (values))

(declaim (inline groestl-rnd512q)
         (ftype (function ((simple-array (unsigned-byte 64) (#.+groestl-cols-512+))
                           (simple-array (unsigned-byte 64) (#.+groestl-cols-512+))
                           (unsigned-byte 64)))
                groestl-end512q))
(defun groestl-rnd512q (x y r)
  "Compute a round in Q (short variants)."
  (declare (type (simple-array (unsigned-byte 64) (#.+groestl-cols-512+)) x y)
           (type (unsigned-byte 64) r)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (setf (aref x 0) (logxor (aref x 0) #xffffffffffffffff r)
        (aref x 1) (logxor (aref x 1) #xffffffffffffffef r)
        (aref x 2) (logxor (aref x 2) #xffffffffffffffdf r)
        (aref x 3) (logxor (aref x 3) #xffffffffffffffcf r)
        (aref x 4) (logxor (aref x 4) #xffffffffffffffbf r)
        (aref x 5) (logxor (aref x 5) #xffffffffffffffaf r)
        (aref x 6) (logxor (aref x 6) #xffffffffffffff9f r)
        (aref x 7) (logxor (aref x 7) #xffffffffffffff8f r))
  (groestl-column x y 0 1 3 5 7 0 2 4 6)
  (groestl-column x y 1 2 4 6 0 1 3 5 7)
  (groestl-column x y 2 3 5 7 1 2 4 6 0)
  (groestl-column x y 3 4 6 0 2 3 5 7 1)
  (groestl-column x y 4 5 7 1 3 4 6 0 2)
  (groestl-column x y 5 6 0 2 4 5 7 1 3)
  (groestl-column x y 6 7 1 3 5 6 0 2 4)
  (groestl-column x y 7 0 2 4 6 7 1 3 5)
  (values))

(declaim (inline groestl-rnd1024p)
         (ftype (function ((simple-array (unsigned-byte 64) (#.+groestl-cols-1024+))
                           (simple-array (unsigned-byte 64) (#.+groestl-cols-1024+))
                           (unsigned-byte 64)))
                groestl-end1024p))
(defun groestl-rnd1024p (x y r)
  "Compute a round in P (long variants)."
  (declare (type (simple-array (unsigned-byte 64) (#.+groestl-cols-1024+)) x y)
           (type (unsigned-byte 64) r)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (setf (aref x 0) (logxor (aref x 0) #x0000000000000000 r)
        (aref x 1) (logxor (aref x 1) #x1000000000000000 r)
        (aref x 2) (logxor (aref x 2) #x2000000000000000 r)
        (aref x 3) (logxor (aref x 3) #x3000000000000000 r)
        (aref x 4) (logxor (aref x 4) #x4000000000000000 r)
        (aref x 5) (logxor (aref x 5) #x5000000000000000 r)
        (aref x 6) (logxor (aref x 6) #x6000000000000000 r)
        (aref x 7) (logxor (aref x 7) #x7000000000000000 r)
        (aref x 8) (logxor (aref x 8) #x8000000000000000 r)
        (aref x 9) (logxor (aref x 9) #x9000000000000000 r)
        (aref x 10) (logxor (aref x 10) #xa000000000000000 r)
        (aref x 11) (logxor (aref x 11) #xb000000000000000 r)
        (aref x 12) (logxor (aref x 12) #xc000000000000000 r)
        (aref x 13) (logxor (aref x 13) #xd000000000000000 r)
        (aref x 14) (logxor (aref x 14) #xe000000000000000 r)
        (aref x 15) (logxor (aref x 15) #xf000000000000000 r))
  (groestl-column x y 15 15 0 1 2 3 4 5 10)
  (groestl-column x y 14 14 15 0 1 2 3 4 9)
  (groestl-column x y 13 13 14 15 0 1 2 3 8)
  (groestl-column x y 12 12 13 14 15 0 1 2 7)
  (groestl-column x y 11 11 12 13 14 15 0 1 6)
  (groestl-column x y 10 10 11 12 13 14 15 0 5)
  (groestl-column x y 9 9 10 11 12 13 14 15 4)
  (groestl-column x y 8 8 9 10 11 12 13 14 3)
  (groestl-column x y 7 7 8 9 10 11 12 13 2)
  (groestl-column x y 6 6 7 8 9 10 11 12 1)
  (groestl-column x y 5 5 6 7 8 9 10 11 0)
  (groestl-column x y 4 4 5 6 7 8 9 10 15)
  (groestl-column x y 3 3 4 5 6 7 8 9 14)
  (groestl-column x y 2 2 3 4 5 6 7 8 13)
  (groestl-column x y 1 1 2 3 4 5 6 7 12)
  (groestl-column x y 0 0 1 2 3 4 5 6 11)
  (values))

(declaim (inline groestl-rnd1024q)
         (ftype (function ((simple-array (unsigned-byte 64) (#.+groestl-cols-1024+))
                           (simple-array (unsigned-byte 64) (#.+groestl-cols-1024+))
                           (unsigned-byte 64)))
                groestl-end1024q))
(defun groestl-rnd1024q (x y r)
  "Compute a round in Q (long variants)."
  (declare (type (simple-array (unsigned-byte 64) (#.+groestl-cols-1024+)) x y)
           (type (unsigned-byte 64) r)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (setf (aref x 0) (logxor (aref x 0) #xffffffffffffffff r)
        (aref x 1) (logxor (aref x 1) #xffffffffffffffef r)
        (aref x 2) (logxor (aref x 2) #xffffffffffffffdf r)
        (aref x 3) (logxor (aref x 3) #xffffffffffffffcf r)
        (aref x 4) (logxor (aref x 4) #xffffffffffffffbf r)
        (aref x 5) (logxor (aref x 5) #xffffffffffffffaf r)
        (aref x 6) (logxor (aref x 6) #xffffffffffffff9f r)
        (aref x 7) (logxor (aref x 7) #xffffffffffffff8f r)
        (aref x 8) (logxor (aref x 8) #xffffffffffffff7f r)
        (aref x 9) (logxor (aref x 9) #xffffffffffffff6f r)
        (aref x 10) (logxor (aref x 10) #xffffffffffffff5f r)
        (aref x 11) (logxor (aref x 11) #xffffffffffffff4f r)
        (aref x 12) (logxor (aref x 12) #xffffffffffffff3f r)
        (aref x 13) (logxor (aref x 13) #xffffffffffffff2f r)
        (aref x 14) (logxor (aref x 14) #xffffffffffffff1f r)
        (aref x 15) (logxor (aref x 15) #xffffffffffffff0f r))
  (groestl-column x y 15 0 2 4 10 15 1 3 5)
  (groestl-column x y 14 15 1 3 9 14 0 2 4)
  (groestl-column x y 13 14 0 2 8 13 15 1 3)
  (groestl-column x y 12 13 15 1 7 12 14 0 2)
  (groestl-column x y 11 12 14 0 6 11 13 15 1)
  (groestl-column x y 10 11 13 15 5 10 12 14 0)
  (groestl-column x y 9 10 12 14 4 9 11 13 15)
  (groestl-column x y 8 9 11 13 3 8 10 12 14)
  (groestl-column x y 7 8 10 12 2 7 9 11 13)
  (groestl-column x y 6 7 9 11 1 6 8 10 12)
  (groestl-column x y 5 6 8 10 0 5 7 9 11)
  (groestl-column x y 4 5 7 9 15 4 6 8 10)
  (groestl-column x y 3 4 6 8 14 3 5 7 9)
  (groestl-column x y 2 3 5 7 13 2 4 6 8)
  (groestl-column x y 1 2 4 6 12 1 3 5 7)
  (groestl-column x y 0 1 3 5 11 0 2 4 6)
  (values))


;;;
;;; Compression
;;;

(defun groestl-f512 (state input start)
  "The compression function (short variants)."
  (declare (type (simple-array (unsigned-byte 64) (#.+groestl-cols-512+)) state)
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type (unsigned-byte 64) start)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((y (make-array #.+groestl-cols-512+ :element-type '(unsigned-byte 64)))
        (z (make-array #.+groestl-cols-512+ :element-type '(unsigned-byte 64)))
        (outq (make-array #.+groestl-cols-512+ :element-type '(unsigned-byte 64)))
        (inp (make-array #.+groestl-cols-512+ :element-type '(unsigned-byte 64))))
    (declare (type (simple-array (unsigned-byte 64) (#.+groestl-cols-512+)) y z outq inp)
             (dynamic-extent y z outq inp))

    (dotimes (i +groestl-cols-512+)
      (declare (type (integer 0 #.+groestl-cols-512+) i))
      (let ((n (ub64ref/be input (+ start (* 8 i)))))
        (declare (type (unsigned-byte 64) n))
        (setf (aref z i) n
              (aref inp i) (logxor (aref state i) n))))

    ;; Compute Q(m)
    (groestl-rnd512q z y 0)
    (loop for i from 1 below (1- +groestl-rounds-512+) by 2 do
      (groestl-rnd512q y z i)
      (groestl-rnd512q z y (1+ i)))
    (groestl-rnd512q y outq (1- +groestl-rounds-512+))

    ;; Compute P(h + m)
    (groestl-rnd512p inp z 0)
    (loop for i of-type fixnum from 1 below (1- +groestl-rounds-512+) by 2 do
      (groestl-rnd512p z y (ash i 56))
      (groestl-rnd512p y z (ash (1+ i) 56)))
    (groestl-rnd512p z y (ash (1- +groestl-rounds-512+) 56))

    ;; h' = h + Q(m) + P(h + m)
    (dotimes (i +groestl-cols-512+)
      (declare (type (integer 0 #.+groestl-cols-512+) i))
      (setf (aref state i) (logxor (aref state i) (aref outq i) (aref y i))))

    (values)))

(defun groestl-f1024 (state input start)
  "The compression function (long variants)."
  (declare (type (simple-array (unsigned-byte 64) (#.+groestl-cols-1024+)) state)
           (type (simple-array (unsigned-byte 8) (*)) input)
           (type (unsigned-byte 64) start)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((y (make-array #.+groestl-cols-1024+ :element-type '(unsigned-byte 64)))
        (z (make-array #.+groestl-cols-1024+ :element-type '(unsigned-byte 64)))
        (outq (make-array #.+groestl-cols-1024+ :element-type '(unsigned-byte 64)))
        (inp (make-array #.+groestl-cols-1024+ :element-type '(unsigned-byte 64))))
    (declare (type (simple-array (unsigned-byte 64) (#.+groestl-cols-1024+)) y z outq inp)
             (dynamic-extent y z outq inp))

    (dotimes (i +groestl-cols-1024+)
      (declare (type (integer 0 #.+groestl-cols-1024+) i))
      (let ((n (ub64ref/be input (+ start (* 8 i)))))
        (declare (type (unsigned-byte 64) n))
        (setf (aref z i) n
              (aref inp i) (logxor (aref state i) n))))

    ;; Compute Q(m)
    (groestl-rnd1024q z y 0)
    (loop for i from 1 below (1- +groestl-rounds-1024+) by 2 do
      (groestl-rnd1024q y z i)
      (groestl-rnd1024q z y (1+ i)))
    (groestl-rnd1024q y outq (1- +groestl-rounds-1024+))

    ;; Compute P(h + m)
    (groestl-rnd1024p inp z 0)
    (loop for i of-type fixnum from 1 below (1- +groestl-rounds-1024+) by 2 do
      (groestl-rnd1024p z y (ash i 56))
      (groestl-rnd1024p y z (ash (1+ i) 56)))
    (groestl-rnd1024p z y (ash (1- +groestl-rounds-1024+) 56))

    ;; h' = h + Q(m) + P(h + m)
    (dotimes (i +groestl-cols-1024+)
      (declare (type (integer 0 #.+groestl-cols-1024+) i))
      (setf (aref state i) (logxor (aref state i) (aref outq i) (aref y i))))

    (values)))


;;;
;;; Digest structures and functions
;;;

(defun groestl-make-initial-state (output-bit-length)
  (let ((state (make-array (if (<= output-bit-length 256)
                               #.+groestl-cols-512+
                               #.+groestl-cols-1024+)
                           :element-type '(unsigned-byte 64)
                           :initial-element 0)))
    (setf (aref state (1- (length state))) output-bit-length)
    state))

(defstruct (groestl
            (:constructor %make-groestl-digest nil)
            (:copier nil))
  (state (groestl-make-initial-state 512)
   :type (simple-array (unsigned-byte 64) (*)))
  (block-counter 0 :type (unsigned-byte 64))
  (buffer (make-array #.+groestl-size-1024+ :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  (buffer-index 0 :type (integer 0 #.+groestl-size-1024+)))

(defstruct (groestl/384
            (:include groestl)
            (:constructor %make-groestl/384-digest
                (&aux (state (groestl-make-initial-state 384))))
            (:copier nil)))

(defstruct (groestl/256
            (:include groestl)
            (:constructor %make-groestl/256-digest
                (&aux (state (groestl-make-initial-state 256))
                   (buffer (make-array #.+groestl-size-512+
                                       :element-type '(unsigned-byte 8)))))))

(defstruct (groestl/224
            (:include groestl)
            (:constructor %make-groestl/224-digest
                (&aux (state (groestl-make-initial-state 224))
                   (buffer (make-array #.+groestl-size-512+
                                       :element-type '(unsigned-byte 8)))))))

(defmethod reinitialize-instance ((state groestl) &rest initargs)
  (declare (ignore initargs))
  (setf (groestl-state state) (etypecase state
                                (groestl/224 (groestl-make-initial-state 224))
                                (groestl/256 (groestl-make-initial-state 256))
                                (groestl/384 (groestl-make-initial-state 384))
                                (groestl (groestl-make-initial-state 512)))
        (groestl-block-counter state) 0
        (groestl-buffer-index state) 0)
  state)

(defmethod copy-digest ((state groestl) &optional copy)
  (check-type copy (or null groestl))
  (let ((copy (if copy
                  copy
                  (etypecase state
                    (groestl/224 (%make-groestl/224-digest))
                    (groestl/256 (%make-groestl/256-digest))
                    (groestl/384 (%make-groestl/384-digest))
                    (groestl (%make-groestl-digest))))))
    (declare (type groestl copy))
    (replace (groestl-state copy) (groestl-state state))
    (replace (groestl-buffer copy) (groestl-buffer state))
    (setf (groestl-block-counter copy) (groestl-block-counter state)
          (groestl-buffer-index copy) (groestl-buffer-index state))
    copy))

(defun groestl-update (state input start end)
  (declare (type (simple-array (unsigned-byte 8) (*)) input)
           (type (unsigned-byte 64) start end)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let* ((groestl-state (groestl-state state))
         (buffer (groestl-buffer state))
         (buffer-index (groestl-buffer-index state))
         (block-counter (groestl-block-counter state))
         (block-size (length buffer))
         (transform (if (= block-size +groestl-size-512+)
                        #'groestl-f512
                        #'groestl-f1024))
         (length (- end start))
         (n 0))
    (declare (type (simple-array (unsigned-byte 64) (*)) groestl-state)
             (type (simple-array (unsigned-byte 8) (*)) buffer)
             (type (integer 0 #.+groestl-size-1024+) block-size buffer-index n)
             (type (unsigned-byte 64) length))

    ;; Try to fill the buffer with the new data
    (setf n (min length (- block-size buffer-index)))
    (replace buffer input :start1 buffer-index :start2 start :end2 (+ start n))
    (incf buffer-index n)
    (incf start n)
    (decf length n)

    ;; Process data in buffer
    (when (= buffer-index block-size)
      (funcall transform groestl-state buffer 0)
      (setf buffer-index 0)
      (incf block-counter))

    ;; Process data in message
    (loop until (< length block-size) do
      (funcall transform groestl-state input start)
      (incf block-counter)
      (incf start block-size)
      (decf length block-size))

    ;; Put remaining message data in buffer
    (when (plusp length)
      (replace buffer input :end1 length :start2 start)
      (setf buffer-index length))

    ;; Save the new state
    (setf (groestl-block-counter state) block-counter
          (groestl-buffer-index state) buffer-index)

    (values)))

(defun groestl-finalize (state digest digest-start)
  (let* ((digest-length (digest-length state))
         (groestl-state (groestl-state state))
         (buffer (groestl-buffer state))
         (buffer-index (groestl-buffer-index state))
         (block-counter (groestl-block-counter state))
         (block-size (length buffer))
         (transform (if (= block-size +groestl-size-512+)
                        #'groestl-f512
                        #'groestl-f1024)))
    (declare (type (simple-array (unsigned-byte 64) (*)) groestl-state)
             (type (simple-array (unsigned-byte 8) (*)) buffer)
             (type (integer 0 #.+groestl-size-1024+) block-size buffer-index)
             (optimize (speed 3) (space 0) (safety 0) (debug 0)))

    ;; Pad with 0s
    (setf (aref buffer buffer-index) #x80)
    (incf buffer-index)
    (when (> buffer-index (- block-size +groestl-length-field-length+))
      ;; Padding requires two blocks
      (fill buffer 0 :start buffer-index)
      (funcall transform groestl-state buffer 0)
      (incf block-counter)
      (setf buffer-index 0))
    (fill buffer 0 :start buffer-index)
    (setf buffer-index (- block-size +groestl-length-field-length+))

    ;; Length padding
    (incf block-counter)
    (setf (ub64ref/be buffer buffer-index) block-counter)
    (funcall transform groestl-state buffer 0)

    ;; Output transformation: h = P(h) + h
    (if (= block-size +groestl-size-512+)
        (let ((temp (copy-seq groestl-state))
              (y (make-array +groestl-cols-512+ :element-type '(unsigned-byte 64)))
              (z (make-array +groestl-cols-512+ :element-type '(unsigned-byte 64))))
          (declare (type (simple-array (unsigned-byte 64) (#.+groestl-cols-512+)) temp y z)
                   (dynamic-extent temp y z))
          (groestl-rnd512p temp z 0)
          (loop for i from 1 below (1- +groestl-rounds-512+) by 2 do
            (groestl-rnd512p z y (ash i 56))
            (groestl-rnd512p y z (ash (1+ i) 56)))
          (groestl-rnd512p z temp (ash (1- +groestl-rounds-512+) 56))
          (dotimes (i +groestl-cols-512+)
            (setf (aref groestl-state i) (logxor (aref groestl-state i)
                                                 (aref temp i)))))
        (let ((temp (copy-seq groestl-state))
              (y (make-array +groestl-cols-1024+ :element-type '(unsigned-byte 64)))
              (z (make-array +groestl-cols-1024+ :element-type '(unsigned-byte 64))))
          (declare (type (simple-array (unsigned-byte 64) (#.+groestl-cols-1024+)) temp y z)
                   (dynamic-extent temp y z))
          (groestl-rnd1024p temp y 0)
          (loop for i from 1 below (1- +groestl-rounds-1024+) by 2 do
            (groestl-rnd1024p y z (ash i 56))
            (groestl-rnd1024p z y (ash (1+ i) 56)))
          (groestl-rnd1024p y temp (ash (1- +groestl-rounds-1024+) 56))
          (dotimes (i +groestl-cols-1024+)
            (setf (aref groestl-state i) (logxor (aref groestl-state i)
                                                 (aref temp i))))))

    ;; Truncate the final hash value to generate the message digest
    (let ((output (make-array block-size :element-type '(unsigned-byte 8))))
      (dotimes (i (length groestl-state))
        (setf (ub64ref/be output (* i 8)) (aref groestl-state i)))
      (replace digest output :start1 digest-start :start2 (- block-size digest-length))
      digest)))

(define-digest-updater groestl
  (groestl-update state sequence start end))

(define-digest-finalizer ((groestl 64)
                          (groestl/384 48)
                          (groestl/256 32)
                          (groestl/224 28))
  (groestl-finalize state digest digest-start))

(defdigest groestl :digest-length 64 :block-length 128)
(defdigest groestl/384 :digest-length 48 :block-length 128)
(defdigest groestl/256 :digest-length 32 :block-length 64)
(defdigest groestl/224 :digest-length 28 :block-length 64)
