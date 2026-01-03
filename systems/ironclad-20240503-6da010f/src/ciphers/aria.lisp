;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; aria.lisp - implementation of the ARIA block cipher

(in-package :crypto)


(defconst +aria-s1+
  (make-array 256
              :element-type '(unsigned-byte 32)
              :initial-contents '(#x00636363 #x007c7c7c #x00777777 #x007b7b7b
                                  #x00f2f2f2 #x006b6b6b #x006f6f6f #x00c5c5c5
                                  #x00303030 #x00010101 #x00676767 #x002b2b2b
                                  #x00fefefe #x00d7d7d7 #x00ababab #x00767676
                                  #x00cacaca #x00828282 #x00c9c9c9 #x007d7d7d
                                  #x00fafafa #x00595959 #x00474747 #x00f0f0f0
                                  #x00adadad #x00d4d4d4 #x00a2a2a2 #x00afafaf
                                  #x009c9c9c #x00a4a4a4 #x00727272 #x00c0c0c0
                                  #x00b7b7b7 #x00fdfdfd #x00939393 #x00262626
                                  #x00363636 #x003f3f3f #x00f7f7f7 #x00cccccc
                                  #x00343434 #x00a5a5a5 #x00e5e5e5 #x00f1f1f1
                                  #x00717171 #x00d8d8d8 #x00313131 #x00151515
                                  #x00040404 #x00c7c7c7 #x00232323 #x00c3c3c3
                                  #x00181818 #x00969696 #x00050505 #x009a9a9a
                                  #x00070707 #x00121212 #x00808080 #x00e2e2e2
                                  #x00ebebeb #x00272727 #x00b2b2b2 #x00757575
                                  #x00090909 #x00838383 #x002c2c2c #x001a1a1a
                                  #x001b1b1b #x006e6e6e #x005a5a5a #x00a0a0a0
                                  #x00525252 #x003b3b3b #x00d6d6d6 #x00b3b3b3
                                  #x00292929 #x00e3e3e3 #x002f2f2f #x00848484
                                  #x00535353 #x00d1d1d1 #x00000000 #x00ededed
                                  #x00202020 #x00fcfcfc #x00b1b1b1 #x005b5b5b
                                  #x006a6a6a #x00cbcbcb #x00bebebe #x00393939
                                  #x004a4a4a #x004c4c4c #x00585858 #x00cfcfcf
                                  #x00d0d0d0 #x00efefef #x00aaaaaa #x00fbfbfb
                                  #x00434343 #x004d4d4d #x00333333 #x00858585
                                  #x00454545 #x00f9f9f9 #x00020202 #x007f7f7f
                                  #x00505050 #x003c3c3c #x009f9f9f #x00a8a8a8
                                  #x00515151 #x00a3a3a3 #x00404040 #x008f8f8f
                                  #x00929292 #x009d9d9d #x00383838 #x00f5f5f5
                                  #x00bcbcbc #x00b6b6b6 #x00dadada #x00212121
                                  #x00101010 #x00ffffff #x00f3f3f3 #x00d2d2d2
                                  #x00cdcdcd #x000c0c0c #x00131313 #x00ececec
                                  #x005f5f5f #x00979797 #x00444444 #x00171717
                                  #x00c4c4c4 #x00a7a7a7 #x007e7e7e #x003d3d3d
                                  #x00646464 #x005d5d5d #x00191919 #x00737373
                                  #x00606060 #x00818181 #x004f4f4f #x00dcdcdc
                                  #x00222222 #x002a2a2a #x00909090 #x00888888
                                  #x00464646 #x00eeeeee #x00b8b8b8 #x00141414
                                  #x00dedede #x005e5e5e #x000b0b0b #x00dbdbdb
                                  #x00e0e0e0 #x00323232 #x003a3a3a #x000a0a0a
                                  #x00494949 #x00060606 #x00242424 #x005c5c5c
                                  #x00c2c2c2 #x00d3d3d3 #x00acacac #x00626262
                                  #x00919191 #x00959595 #x00e4e4e4 #x00797979
                                  #x00e7e7e7 #x00c8c8c8 #x00373737 #x006d6d6d
                                  #x008d8d8d #x00d5d5d5 #x004e4e4e #x00a9a9a9
                                  #x006c6c6c #x00565656 #x00f4f4f4 #x00eaeaea
                                  #x00656565 #x007a7a7a #x00aeaeae #x00080808
                                  #x00bababa #x00787878 #x00252525 #x002e2e2e
                                  #x001c1c1c #x00a6a6a6 #x00b4b4b4 #x00c6c6c6
                                  #x00e8e8e8 #x00dddddd #x00747474 #x001f1f1f
                                  #x004b4b4b #x00bdbdbd #x008b8b8b #x008a8a8a
                                  #x00707070 #x003e3e3e #x00b5b5b5 #x00666666
                                  #x00484848 #x00030303 #x00f6f6f6 #x000e0e0e
                                  #x00616161 #x00353535 #x00575757 #x00b9b9b9
                                  #x00868686 #x00c1c1c1 #x001d1d1d #x009e9e9e
                                  #x00e1e1e1 #x00f8f8f8 #x00989898 #x00111111
                                  #x00696969 #x00d9d9d9 #x008e8e8e #x00949494
                                  #x009b9b9b #x001e1e1e #x00878787 #x00e9e9e9
                                  #x00cecece #x00555555 #x00282828 #x00dfdfdf
                                  #x008c8c8c #x00a1a1a1 #x00898989 #x000d0d0d
                                  #x00bfbfbf #x00e6e6e6 #x00424242 #x00686868
                                  #x00414141 #x00999999 #x002d2d2d #x000f0f0f
                                  #x00b0b0b0 #x00545454 #x00bbbbbb #x00161616)))

(defconst +aria-s2+
  (make-array 256
              :element-type '(unsigned-byte 32)
              :initial-contents '(#xe200e2e2 #x4e004e4e #x54005454 #xfc00fcfc
                                  #x94009494 #xc200c2c2 #x4a004a4a #xcc00cccc
                                  #x62006262 #x0d000d0d #x6a006a6a #x46004646
                                  #x3c003c3c #x4d004d4d #x8b008b8b #xd100d1d1
                                  #x5e005e5e #xfa00fafa #x64006464 #xcb00cbcb
                                  #xb400b4b4 #x97009797 #xbe00bebe #x2b002b2b
                                  #xbc00bcbc #x77007777 #x2e002e2e #x03000303
                                  #xd300d3d3 #x19001919 #x59005959 #xc100c1c1
                                  #x1d001d1d #x06000606 #x41004141 #x6b006b6b
                                  #x55005555 #xf000f0f0 #x99009999 #x69006969
                                  #xea00eaea #x9c009c9c #x18001818 #xae00aeae
                                  #x63006363 #xdf00dfdf #xe700e7e7 #xbb00bbbb
                                  #x00000000 #x73007373 #x66006666 #xfb00fbfb
                                  #x96009696 #x4c004c4c #x85008585 #xe400e4e4
                                  #x3a003a3a #x09000909 #x45004545 #xaa00aaaa
                                  #x0f000f0f #xee00eeee #x10001010 #xeb00ebeb
                                  #x2d002d2d #x7f007f7f #xf400f4f4 #x29002929
                                  #xac00acac #xcf00cfcf #xad00adad #x91009191
                                  #x8d008d8d #x78007878 #xc800c8c8 #x95009595
                                  #xf900f9f9 #x2f002f2f #xce00cece #xcd00cdcd
                                  #x08000808 #x7a007a7a #x88008888 #x38003838
                                  #x5c005c5c #x83008383 #x2a002a2a #x28002828
                                  #x47004747 #xdb00dbdb #xb800b8b8 #xc700c7c7
                                  #x93009393 #xa400a4a4 #x12001212 #x53005353
                                  #xff00ffff #x87008787 #x0e000e0e #x31003131
                                  #x36003636 #x21002121 #x58005858 #x48004848
                                  #x01000101 #x8e008e8e #x37003737 #x74007474
                                  #x32003232 #xca00caca #xe900e9e9 #xb100b1b1
                                  #xb700b7b7 #xab00abab #x0c000c0c #xd700d7d7
                                  #xc400c4c4 #x56005656 #x42004242 #x26002626
                                  #x07000707 #x98009898 #x60006060 #xd900d9d9
                                  #xb600b6b6 #xb900b9b9 #x11001111 #x40004040
                                  #xec00ecec #x20002020 #x8c008c8c #xbd00bdbd
                                  #xa000a0a0 #xc900c9c9 #x84008484 #x04000404
                                  #x49004949 #x23002323 #xf100f1f1 #x4f004f4f
                                  #x50005050 #x1f001f1f #x13001313 #xdc00dcdc
                                  #xd800d8d8 #xc000c0c0 #x9e009e9e #x57005757
                                  #xe300e3e3 #xc300c3c3 #x7b007b7b #x65006565
                                  #x3b003b3b #x02000202 #x8f008f8f #x3e003e3e
                                  #xe800e8e8 #x25002525 #x92009292 #xe500e5e5
                                  #x15001515 #xdd00dddd #xfd00fdfd #x17001717
                                  #xa900a9a9 #xbf00bfbf #xd400d4d4 #x9a009a9a
                                  #x7e007e7e #xc500c5c5 #x39003939 #x67006767
                                  #xfe00fefe #x76007676 #x9d009d9d #x43004343
                                  #xa700a7a7 #xe100e1e1 #xd000d0d0 #xf500f5f5
                                  #x68006868 #xf200f2f2 #x1b001b1b #x34003434
                                  #x70007070 #x05000505 #xa300a3a3 #x8a008a8a
                                  #xd500d5d5 #x79007979 #x86008686 #xa800a8a8
                                  #x30003030 #xc600c6c6 #x51005151 #x4b004b4b
                                  #x1e001e1e #xa600a6a6 #x27002727 #xf600f6f6
                                  #x35003535 #xd200d2d2 #x6e006e6e #x24002424
                                  #x16001616 #x82008282 #x5f005f5f #xda00dada
                                  #xe600e6e6 #x75007575 #xa200a2a2 #xef00efef
                                  #x2c002c2c #xb200b2b2 #x1c001c1c #x9f009f9f
                                  #x5d005d5d #x6f006f6f #x80008080 #x0a000a0a
                                  #x72007272 #x44004444 #x9b009b9b #x6c006c6c
                                  #x90009090 #x0b000b0b #x5b005b5b #x33003333
                                  #x7d007d7d #x5a005a5a #x52005252 #xf300f3f3
                                  #x61006161 #xa100a1a1 #xf700f7f7 #xb000b0b0
                                  #xd600d6d6 #x3f003f3f #x7c007c7c #x6d006d6d
                                  #xed00eded #x14001414 #xe000e0e0 #xa500a5a5
                                  #x3d003d3d #x22002222 #xb300b3b3 #xf800f8f8
                                  #x89008989 #xde00dede #x71007171 #x1a001a1a
                                  #xaf00afaf #xba00baba #xb500b5b5 #x81008181)))

(defconst +aria-x1+
  (make-array 256
              :element-type '(unsigned-byte 32)
              :initial-contents '(#x52520052 #x09090009 #x6a6a006a #xd5d500d5
                                  #x30300030 #x36360036 #xa5a500a5 #x38380038
                                  #xbfbf00bf #x40400040 #xa3a300a3 #x9e9e009e
                                  #x81810081 #xf3f300f3 #xd7d700d7 #xfbfb00fb
                                  #x7c7c007c #xe3e300e3 #x39390039 #x82820082
                                  #x9b9b009b #x2f2f002f #xffff00ff #x87870087
                                  #x34340034 #x8e8e008e #x43430043 #x44440044
                                  #xc4c400c4 #xdede00de #xe9e900e9 #xcbcb00cb
                                  #x54540054 #x7b7b007b #x94940094 #x32320032
                                  #xa6a600a6 #xc2c200c2 #x23230023 #x3d3d003d
                                  #xeeee00ee #x4c4c004c #x95950095 #x0b0b000b
                                  #x42420042 #xfafa00fa #xc3c300c3 #x4e4e004e
                                  #x08080008 #x2e2e002e #xa1a100a1 #x66660066
                                  #x28280028 #xd9d900d9 #x24240024 #xb2b200b2
                                  #x76760076 #x5b5b005b #xa2a200a2 #x49490049
                                  #x6d6d006d #x8b8b008b #xd1d100d1 #x25250025
                                  #x72720072 #xf8f800f8 #xf6f600f6 #x64640064
                                  #x86860086 #x68680068 #x98980098 #x16160016
                                  #xd4d400d4 #xa4a400a4 #x5c5c005c #xcccc00cc
                                  #x5d5d005d #x65650065 #xb6b600b6 #x92920092
                                  #x6c6c006c #x70700070 #x48480048 #x50500050
                                  #xfdfd00fd #xeded00ed #xb9b900b9 #xdada00da
                                  #x5e5e005e #x15150015 #x46460046 #x57570057
                                  #xa7a700a7 #x8d8d008d #x9d9d009d #x84840084
                                  #x90900090 #xd8d800d8 #xabab00ab #x00000000
                                  #x8c8c008c #xbcbc00bc #xd3d300d3 #x0a0a000a
                                  #xf7f700f7 #xe4e400e4 #x58580058 #x05050005
                                  #xb8b800b8 #xb3b300b3 #x45450045 #x06060006
                                  #xd0d000d0 #x2c2c002c #x1e1e001e #x8f8f008f
                                  #xcaca00ca #x3f3f003f #x0f0f000f #x02020002
                                  #xc1c100c1 #xafaf00af #xbdbd00bd #x03030003
                                  #x01010001 #x13130013 #x8a8a008a #x6b6b006b
                                  #x3a3a003a #x91910091 #x11110011 #x41410041
                                  #x4f4f004f #x67670067 #xdcdc00dc #xeaea00ea
                                  #x97970097 #xf2f200f2 #xcfcf00cf #xcece00ce
                                  #xf0f000f0 #xb4b400b4 #xe6e600e6 #x73730073
                                  #x96960096 #xacac00ac #x74740074 #x22220022
                                  #xe7e700e7 #xadad00ad #x35350035 #x85850085
                                  #xe2e200e2 #xf9f900f9 #x37370037 #xe8e800e8
                                  #x1c1c001c #x75750075 #xdfdf00df #x6e6e006e
                                  #x47470047 #xf1f100f1 #x1a1a001a #x71710071
                                  #x1d1d001d #x29290029 #xc5c500c5 #x89890089
                                  #x6f6f006f #xb7b700b7 #x62620062 #x0e0e000e
                                  #xaaaa00aa #x18180018 #xbebe00be #x1b1b001b
                                  #xfcfc00fc #x56560056 #x3e3e003e #x4b4b004b
                                  #xc6c600c6 #xd2d200d2 #x79790079 #x20200020
                                  #x9a9a009a #xdbdb00db #xc0c000c0 #xfefe00fe
                                  #x78780078 #xcdcd00cd #x5a5a005a #xf4f400f4
                                  #x1f1f001f #xdddd00dd #xa8a800a8 #x33330033
                                  #x88880088 #x07070007 #xc7c700c7 #x31310031
                                  #xb1b100b1 #x12120012 #x10100010 #x59590059
                                  #x27270027 #x80800080 #xecec00ec #x5f5f005f
                                  #x60600060 #x51510051 #x7f7f007f #xa9a900a9
                                  #x19190019 #xb5b500b5 #x4a4a004a #x0d0d000d
                                  #x2d2d002d #xe5e500e5 #x7a7a007a #x9f9f009f
                                  #x93930093 #xc9c900c9 #x9c9c009c #xefef00ef
                                  #xa0a000a0 #xe0e000e0 #x3b3b003b #x4d4d004d
                                  #xaeae00ae #x2a2a002a #xf5f500f5 #xb0b000b0
                                  #xc8c800c8 #xebeb00eb #xbbbb00bb #x3c3c003c
                                  #x83830083 #x53530053 #x99990099 #x61610061
                                  #x17170017 #x2b2b002b #x04040004 #x7e7e007e
                                  #xbaba00ba #x77770077 #xd6d600d6 #x26260026
                                  #xe1e100e1 #x69690069 #x14140014 #x63630063
                                  #x55550055 #x21210021 #x0c0c000c #x7d7d007d)))

(defconst +aria-x2+
  (make-array 256
              :element-type '(unsigned-byte 32)
              :initial-contents '(#x30303000 #x68686800 #x99999900 #x1b1b1b00
                                  #x87878700 #xb9b9b900 #x21212100 #x78787800
                                  #x50505000 #x39393900 #xdbdbdb00 #xe1e1e100
                                  #x72727200 #x09090900 #x62626200 #x3c3c3c00
                                  #x3e3e3e00 #x7e7e7e00 #x5e5e5e00 #x8e8e8e00
                                  #xf1f1f100 #xa0a0a000 #xcccccc00 #xa3a3a300
                                  #x2a2a2a00 #x1d1d1d00 #xfbfbfb00 #xb6b6b600
                                  #xd6d6d600 #x20202000 #xc4c4c400 #x8d8d8d00
                                  #x81818100 #x65656500 #xf5f5f500 #x89898900
                                  #xcbcbcb00 #x9d9d9d00 #x77777700 #xc6c6c600
                                  #x57575700 #x43434300 #x56565600 #x17171700
                                  #xd4d4d400 #x40404000 #x1a1a1a00 #x4d4d4d00
                                  #xc0c0c000 #x63636300 #x6c6c6c00 #xe3e3e300
                                  #xb7b7b700 #xc8c8c800 #x64646400 #x6a6a6a00
                                  #x53535300 #xaaaaaa00 #x38383800 #x98989800
                                  #x0c0c0c00 #xf4f4f400 #x9b9b9b00 #xededed00
                                  #x7f7f7f00 #x22222200 #x76767600 #xafafaf00
                                  #xdddddd00 #x3a3a3a00 #x0b0b0b00 #x58585800
                                  #x67676700 #x88888800 #x06060600 #xc3c3c300
                                  #x35353500 #x0d0d0d00 #x01010100 #x8b8b8b00
                                  #x8c8c8c00 #xc2c2c200 #xe6e6e600 #x5f5f5f00
                                  #x02020200 #x24242400 #x75757500 #x93939300
                                  #x66666600 #x1e1e1e00 #xe5e5e500 #xe2e2e200
                                  #x54545400 #xd8d8d800 #x10101000 #xcecece00
                                  #x7a7a7a00 #xe8e8e800 #x08080800 #x2c2c2c00
                                  #x12121200 #x97979700 #x32323200 #xababab00
                                  #xb4b4b400 #x27272700 #x0a0a0a00 #x23232300
                                  #xdfdfdf00 #xefefef00 #xcacaca00 #xd9d9d900
                                  #xb8b8b800 #xfafafa00 #xdcdcdc00 #x31313100
                                  #x6b6b6b00 #xd1d1d100 #xadadad00 #x19191900
                                  #x49494900 #xbdbdbd00 #x51515100 #x96969600
                                  #xeeeeee00 #xe4e4e400 #xa8a8a800 #x41414100
                                  #xdadada00 #xffffff00 #xcdcdcd00 #x55555500
                                  #x86868600 #x36363600 #xbebebe00 #x61616100
                                  #x52525200 #xf8f8f800 #xbbbbbb00 #x0e0e0e00
                                  #x82828200 #x48484800 #x69696900 #x9a9a9a00
                                  #xe0e0e000 #x47474700 #x9e9e9e00 #x5c5c5c00
                                  #x04040400 #x4b4b4b00 #x34343400 #x15151500
                                  #x79797900 #x26262600 #xa7a7a700 #xdedede00
                                  #x29292900 #xaeaeae00 #x92929200 #xd7d7d700
                                  #x84848400 #xe9e9e900 #xd2d2d200 #xbababa00
                                  #x5d5d5d00 #xf3f3f300 #xc5c5c500 #xb0b0b000
                                  #xbfbfbf00 #xa4a4a400 #x3b3b3b00 #x71717100
                                  #x44444400 #x46464600 #x2b2b2b00 #xfcfcfc00
                                  #xebebeb00 #x6f6f6f00 #xd5d5d500 #xf6f6f600
                                  #x14141400 #xfefefe00 #x7c7c7c00 #x70707000
                                  #x5a5a5a00 #x7d7d7d00 #xfdfdfd00 #x2f2f2f00
                                  #x18181800 #x83838300 #x16161600 #xa5a5a500
                                  #x91919100 #x1f1f1f00 #x05050500 #x95959500
                                  #x74747400 #xa9a9a900 #xc1c1c100 #x5b5b5b00
                                  #x4a4a4a00 #x85858500 #x6d6d6d00 #x13131300
                                  #x07070700 #x4f4f4f00 #x4e4e4e00 #x45454500
                                  #xb2b2b200 #x0f0f0f00 #xc9c9c900 #x1c1c1c00
                                  #xa6a6a600 #xbcbcbc00 #xececec00 #x73737300
                                  #x90909000 #x7b7b7b00 #xcfcfcf00 #x59595900
                                  #x8f8f8f00 #xa1a1a100 #xf9f9f900 #x2d2d2d00
                                  #xf2f2f200 #xb1b1b100 #x00000000 #x94949400
                                  #x37373700 #x9f9f9f00 #xd0d0d000 #x2e2e2e00
                                  #x9c9c9c00 #x6e6e6e00 #x28282800 #x3f3f3f00
                                  #x80808000 #xf0f0f000 #x3d3d3d00 #xd3d3d300
                                  #x25252500 #x8a8a8a00 #xb5b5b500 #xe7e7e700
                                  #x42424200 #xb3b3b300 #xc7c7c700 #xeaeaea00
                                  #xf7f7f700 #x4c4c4c00 #x11111100 #x33333300
                                  #x03030300 #xa2a2a200 #xacacac00 #x60606000)))

(defconst +aria-c1+
  (make-array 4
              :element-type '(unsigned-byte 32)
              :initial-contents '(#x517cc1b7 #x27220a94 #xfe13abe8 #xfa9a6ee0)))

(defconst +aria-c2+
  (make-array 4
              :element-type '(unsigned-byte 32)
              :initial-contents '(#x6db14acc #x9e21c820 #xff28b1d5 #xef5de2b0)))

(defconst +aria-c3+
  (make-array 4
              :element-type '(unsigned-byte 32)
              :initial-contents '(#xdb92371d #x2126e970 #x03249775 #x04e8c90e)))

(defmacro aria-brf (x y)
  `(logand (ash ,x ,(- (* 8 y))) 255))

(defmacro aria-kxl (x0 x1 x2 x3 keys n)
  `(setf ,x0 (logxor ,x0 (aref ,keys ,(* 4 n)))
         ,x1 (logxor ,x1 (aref ,keys ,(+ (* 4 n) 1)))
         ,x2 (logxor ,x2 (aref ,keys ,(+ (* 4 n) 2)))
         ,x3 (logxor ,x3 (aref ,keys ,(+ (* 4 n) 3)))))

(defmacro aria-sbl1-m (x0 x1 x2 x3)
  `(setf ,x0 (logxor (aref +aria-s1+ (aria-brf ,x0 3))
                     (aref +aria-s2+ (aria-brf ,x0 2))
                     (aref +aria-x1+ (aria-brf ,x0 1))
                     (aref +aria-x2+ (aria-brf ,x0 0)))
         ,x1 (logxor (aref +aria-s1+ (aria-brf ,x1 3))
                     (aref +aria-s2+ (aria-brf ,x1 2))
                     (aref +aria-x1+ (aria-brf ,x1 1))
                     (aref +aria-x2+ (aria-brf ,x1 0)))
         ,x2 (logxor (aref +aria-s1+ (aria-brf ,x2 3))
                     (aref +aria-s2+ (aria-brf ,x2 2))
                     (aref +aria-x1+ (aria-brf ,x2 1))
                     (aref +aria-x2+ (aria-brf ,x2 0)))
         ,x3 (logxor (aref +aria-s1+ (aria-brf ,x3 3))
                     (aref +aria-s2+ (aria-brf ,x3 2))
                     (aref +aria-x1+ (aria-brf ,x3 1))
                     (aref +aria-x2+ (aria-brf ,x3 0)))))

(defmacro aria-sbl2-m (x0 x1 x2 x3)
  `(setf ,x0 (logxor (aref +aria-x1+ (aria-brf ,x0 3))
                     (aref +aria-x2+ (aria-brf ,x0 2))
                     (aref +aria-s1+ (aria-brf ,x0 1))
                     (aref +aria-s2+ (aria-brf ,x0 0)))
         ,x1 (logxor (aref +aria-x1+ (aria-brf ,x1 3))
                     (aref +aria-x2+ (aria-brf ,x1 2))
                     (aref +aria-s1+ (aria-brf ,x1 1))
                     (aref +aria-s2+ (aria-brf ,x1 0)))
         ,x2 (logxor (aref +aria-x1+ (aria-brf ,x2 3))
                     (aref +aria-x2+ (aria-brf ,x2 2))
                     (aref +aria-s1+ (aria-brf ,x2 1))
                     (aref +aria-s2+ (aria-brf ,x2 0)))
         ,x3 (logxor (aref +aria-x1+ (aria-brf ,x3 3))
                     (aref +aria-x2+ (aria-brf ,x3 2))
                     (aref +aria-s1+ (aria-brf ,x3 1))
                     (aref +aria-s2+ (aria-brf ,x3 0)))))

(defmacro aria-p (x0 x1 x2 x3)
  (declare (ignorable x0))
  `(setf ,x1 (logxor (logand (mod32ash ,x1 8) #xff00ff00)
                     (logand (mod32ash ,x1 -8) #x00ff00ff))
         ,x2 (ror32 ,x2 16)
         ,x3 (logxor (mod32ash ,x3 -24)
                     (logand (mod32ash ,x3 -8) #x0000ff00)
                     (logand (mod32ash ,x3 8) #x00ff0000)
                     (mod32ash ,x3 24))))

(defmacro aria-m (x y)
  `(setf ,y (logxor (mod32ash ,x 8)
                    (mod32ash ,x -8)
                    (mod32ash ,x 16)
                    (mod32ash ,x -16)
                    (mod32ash ,x 24)
                    (mod32ash ,x -24))))

(defmacro aria-mm (x0 x1 x2 x3)
  `(setf ,x1 (logxor ,x1 ,x2)
         ,x2 (logxor ,x2 ,x3)
         ,x0 (logxor ,x0 ,x1)
         ,x3 (logxor ,x3 ,x1)
         ,x2 (logxor ,x2 ,x0)
         ,x1 (logxor ,x1 ,x2)))

(defmacro aria-fo (x0 x1 x2 x3)
  `(progn
     (aria-sbl1-m ,x0 ,x1 ,x2 ,x3)
     (aria-mm ,x0 ,x1 ,x2 ,x3)
     (aria-p ,x0 ,x1 ,x2 ,x3)
     (aria-mm ,x0 ,x1 ,x2 ,x3)))

(defmacro aria-fe (x0 x1 x2 x3)
  `(progn
     (aria-sbl2-m ,x0 ,x1 ,x2 ,x3)
     (aria-mm ,x0 ,x1 ,x2 ,x3)
     (aria-p ,x2 ,x3 ,x0 ,x1)
     (aria-mm ,x0 ,x1 ,x2 ,x3)))

(defun aria-gsrk (x y n keys k)
  (declare (type (simple-array (unsigned-byte 32) (*)) x y keys)
           (type (integer 0) n k))
  (let ((q (- 4 (floor n 32)))
        (r (mod n 32)))
    (setf (aref keys (* 4 k)) (logxor (aref x 0)
                                      (mod32ash (aref y (mod q 4)) (- r))
                                      (mod32ash (aref y (mod (+ q 3) 4)) (- 32 r)))
          (aref keys (+ (* 4 k) 1)) (logxor (aref x 1)
                                            (mod32ash (aref y (mod (+ q 1) 4)) (- r))
                                            (mod32ash (aref y (mod q 4)) (- 32 r)))
          (aref keys (+ (* 4 k) 2)) (logxor (aref x 2)
                                            (mod32ash (aref y (mod (+ q 2) 4)) (- r))
                                            (mod32ash (aref y (mod (+ q 1) 4)) (- 32 r)))
          (aref keys (+ (* 4 k) 3)) (logxor (aref x 3)
                                            (mod32ash (aref y (mod (+ q 3) 4)) (- r))
                                            (mod32ash (aref y (mod (+ q 2) 4)) (- 32 r))))))

(defun aria-process-block (in in-start out out-start keys rounds)
  (declare (type (simple-array (unsigned-byte 8) (*)) in out)
           (type (simple-array (unsigned-byte 32) (68)) keys)
           (type fixnum in-start out-start rounds)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((x0 (ub32ref/be in in-start))
        (x1 (ub32ref/be in (+ in-start 4)))
        (x2 (ub32ref/be in (+ in-start 8)))
        (x3 (ub32ref/be in (+ in-start 12))))
    (declare (type (unsigned-byte 32) x0 x1 x2 x3))
    (aria-kxl x0 x1 x2 x3 keys 0)
    (aria-fo x0 x1 x2 x3)
    (aria-kxl x0 x1 x2 x3 keys 1)
    (aria-fe x0 x1 x2 x3)
    (aria-kxl x0 x1 x2 x3 keys 2)
    (aria-fo x0 x1 x2 x3)
    (aria-kxl x0 x1 x2 x3 keys 3)
    (aria-fe x0 x1 x2 x3)
    (aria-kxl x0 x1 x2 x3 keys 4)
    (aria-fo x0 x1 x2 x3)
    (aria-kxl x0 x1 x2 x3 keys 5)
    (aria-fe x0 x1 x2 x3)
    (aria-kxl x0 x1 x2 x3 keys 6)
    (aria-fo x0 x1 x2 x3)
    (aria-kxl x0 x1 x2 x3 keys 7)
    (aria-fe x0 x1 x2 x3)
    (aria-kxl x0 x1 x2 x3 keys 8)
    (aria-fo x0 x1 x2 x3)
    (aria-kxl x0 x1 x2 x3 keys 9)
    (aria-fe x0 x1 x2 x3)
    (aria-kxl x0 x1 x2 x3 keys 10)
    (aria-fo x0 x1 x2 x3)
    (aria-kxl x0 x1 x2 x3 keys 11)
    (when (> rounds 12)
      (aria-fe x0 x1 x2 x3)
      (aria-kxl x0 x1 x2 x3 keys 12)
      (aria-fo x0 x1 x2 x3)
      (aria-kxl x0 x1 x2 x3 keys 13))
    (when (> rounds 14)
      (aria-fe x0 x1 x2 x3)
      (aria-kxl x0 x1 x2 x3 keys 14)
      (aria-fo x0 x1 x2 x3)
      (aria-kxl x0 x1 x2 x3 keys 15))

    (setf x0 (logxor (mod32ash (aref +aria-x1+ (aria-brf x0 3)) 24)
                     (logand (mod32ash (aref +aria-x2+ (aria-brf x0 2)) 8) #x00ff0000)
                     (logand (mod32ash (aref +aria-s1+ (aria-brf x0 1)) 8) #x0000ff00)
                     (logand (aref +aria-s2+ (aria-brf x0 0)) #x000000ff))
          x1 (logxor (mod32ash (aref +aria-x1+ (aria-brf x1 3)) 24)
                     (logand (mod32ash (aref +aria-x2+ (aria-brf x1 2)) 8) #x00ff0000)
                     (logand (mod32ash (aref +aria-s1+ (aria-brf x1 1)) 8) #x0000ff00)
                     (logand (aref +aria-s2+ (aria-brf x1 0)) #x000000ff))
          x2 (logxor (mod32ash (aref +aria-x1+ (aria-brf x2 3)) 24)
                     (logand (mod32ash (aref +aria-x2+ (aria-brf x2 2)) 8) #x00ff0000)
                     (logand (mod32ash (aref +aria-s1+ (aria-brf x2 1)) 8) #x0000ff00)
                     (logand (aref +aria-s2+ (aria-brf x2 0)) #x000000ff))
          x3 (logxor (mod32ash (aref +aria-x1+ (aria-brf x3 3)) 24)
                     (logand (mod32ash (aref +aria-x2+ (aria-brf x3 2)) 8) #x00ff0000)
                     (logand (mod32ash (aref +aria-s1+ (aria-brf x3 1)) 8) #x0000ff00)
                     (logand (aref +aria-s2+ (aria-brf x3 0)) #x000000ff)))
    (case rounds
      ((12) (aria-kxl x0 x1 x2 x3 keys 12))
      ((14) (aria-kxl x0 x1 x2 x3 keys 14))
      ((16) (aria-kxl x0 x1 x2 x3 keys 16)))

    (setf (ub32ref/be out out-start) x0
          (ub32ref/be out (+ out-start 4)) x1
          (ub32ref/be out (+ out-start 8)) x2
          (ub32ref/be out (+ out-start 12)) x3))
  (values))

(defclass aria (cipher 16-byte-block-mixin)
  ((rounds :accessor rounds
           :type (integer 12 16))
   (encryption-round-keys :accessor encryption-round-keys
                          :type (simple-array (unsigned-byte 32) (68)))
   (decryption-round-keys :accessor decryption-round-keys
                          :type (simple-array (unsigned-byte 32) (68)))))

(defmethod schedule-key ((cipher aria) key)
  (let* ((key-length (length key))
         (rounds (ecase key-length
                   ((16) 12)
                   ((24) 14)
                   ((32) 16)))
         (ck1 (ecase key-length
                ((16) +aria-c1+)
                ((24) +aria-c2+)
                ((32) +aria-c3+)))
         (ck2 (ecase key-length
                ((16) +aria-c2+)
                ((24) +aria-c3+)
                ((32) +aria-c1+)))
         (ck3 (ecase key-length
                ((16) +aria-c3+)
                ((24) +aria-c1+)
                ((32) +aria-c2+)))
         (encryption-keys (make-array 68 :element-type '(unsigned-byte 32)))
         (decryption-keys (make-array 68 :element-type '(unsigned-byte 32)))
         (k0 (ub32ref/be key 0))
         (k1 (ub32ref/be key 4))
         (k2 (ub32ref/be key 8))
         (k3 (ub32ref/be key 12))
         (w0 (make-array 4 :element-type '(unsigned-byte 32)))
         (w1 (make-array 4 :element-type '(unsigned-byte 32)))
         (w2 (make-array 4 :element-type '(unsigned-byte 32)))
         (w3 (make-array 4 :element-type '(unsigned-byte 32))))
    (declare (type (unsigned-byte 32) k0 k1 k2 k3)
             (type (simple-array (unsigned-byte 32) (4)) w0 w1 w2 w3))
    (setf (aref w0 0) k0
          (aref w0 1) k1
          (aref w0 2) k2
          (aref w0 3) k3)

    (setf k0 (logxor k0 (aref ck1 0))
          k1 (logxor k1 (aref ck1 1))
          k2 (logxor k2 (aref ck1 2))
          k3 (logxor k3 (aref ck1 3)))
    (aria-fo k0 k1 k2 k3)
    (setf k0 (logxor k0 (if (> key-length 16) (ub32ref/be key 16) 0))
          k1 (logxor k1 (if (> key-length 16) (ub32ref/be key 20) 0))
          k2 (logxor k2 (if (> key-length 24) (ub32ref/be key 24) 0))
          k3 (logxor k3 (if (> key-length 24) (ub32ref/be key 28) 0)))
    (setf (aref w1 0) k0
          (aref w1 1) k1
          (aref w1 2) k2
          (aref w1 3) k3)

    (setf k0 (logxor k0 (aref ck2 0))
          k1 (logxor k1 (aref ck2 1))
          k2 (logxor k2 (aref ck2 2))
          k3 (logxor k3 (aref ck2 3)))
    (aria-fe k0 k1 k2 k3)
    (setf k0 (logxor k0 (aref w0 0))
          k1 (logxor k1 (aref w0 1))
          k2 (logxor k2 (aref w0 2))
          k3 (logxor k3 (aref w0 3)))
    (setf (aref w2 0) k0
          (aref w2 1) k1
          (aref w2 2) k2
          (aref w2 3) k3)

    (setf k0 (logxor k0 (aref ck3 0))
          k1 (logxor k1 (aref ck3 1))
          k2 (logxor k2 (aref ck3 2))
          k3 (logxor k3 (aref ck3 3)))
    (aria-fo k0 k1 k2 k3)
    (setf (aref w3 0) (logxor k0 (aref w1 0))
          (aref w3 1) (logxor k1 (aref w1 1))
          (aref w3 2) (logxor k2 (aref w1 2))
          (aref w3 3) (logxor k3 (aref w1 3)))

    (aria-gsrk w0 w1 19 encryption-keys 0)
    (aria-gsrk w1 w2 19 encryption-keys 1)
    (aria-gsrk w2 w3 19 encryption-keys 2)
    (aria-gsrk w3 w0 19 encryption-keys 3)
    (aria-gsrk w0 w1 31 encryption-keys 4)
    (aria-gsrk w1 w2 31 encryption-keys 5)
    (aria-gsrk w2 w3 31 encryption-keys 6)
    (aria-gsrk w3 w0 31 encryption-keys 7)
    (aria-gsrk w0 w1 67 encryption-keys 8)
    (aria-gsrk w1 w2 67 encryption-keys 9)
    (aria-gsrk w2 w3 67 encryption-keys 10)
    (aria-gsrk w3 w0 67 encryption-keys 11)
    (aria-gsrk w0 w1 97 encryption-keys 12)
    (when (> rounds 12)
      (aria-gsrk w1 w2 97 encryption-keys 13)
      (aria-gsrk w2 w3 97 encryption-keys 14))
    (when (> rounds 14)
      (aria-gsrk w3 w0 97 encryption-keys 15)
      (aria-gsrk w0 w1 109 encryption-keys 16))

    (loop for i from (* 4 rounds) downto 0 by 4
          for j from 0 by 4
          do (progn
               (replace decryption-keys encryption-keys :start1 j :start2 i :end2 (+ i 4))
               (unless (or (zerop i) (zerop j))
                 (aria-m (aref encryption-keys i) k0)
                 (aria-m (aref encryption-keys (+ i 1)) k1)
                 (aria-m (aref encryption-keys (+ i 2)) k2)
                 (aria-m (aref encryption-keys (+ i 3)) k3)
                 (aria-mm k0 k1 k2 k3)
                 (aria-p k0 k1 k2 k3)
                 (aria-mm k0 k1 k2 k3)
                 (setf (aref decryption-keys j) k0
                       (aref decryption-keys (+ j 1)) k1
                       (aref decryption-keys (+ j 2)) k2
                       (aref decryption-keys (+ j 3)) k3))))

    (setf (rounds cipher) rounds
          (encryption-round-keys cipher) encryption-keys
          (decryption-round-keys cipher) decryption-keys)
    cipher))

(define-block-encryptor aria 16
  (aria-process-block plaintext plaintext-start
                      ciphertext ciphertext-start
                      (encryption-round-keys context)
                      (rounds context)))

(define-block-decryptor aria 16
  (aria-process-block ciphertext ciphertext-start
                      plaintext plaintext-start
                      (decryption-round-keys context)
                      (rounds context)))

(defcipher aria
  (:encrypt-function aria-encrypt-block)
  (:decrypt-function aria-decrypt-block)
  (:block-length 16)
  (:key-length (:fixed 16 24 32)))
