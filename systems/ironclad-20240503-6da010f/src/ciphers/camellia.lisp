;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; camellia.lisp - implementation of the Camellia block cipher

(in-package :crypto)


(defconst +camellia-sbox1+
  (make-array 256
              :element-type '(unsigned-byte 32)
              :initial-contents '(#x70707000 #x82828200 #x2c2c2c00 #xececec00
                                  #xb3b3b300 #x27272700 #xc0c0c000 #xe5e5e500
                                  #xe4e4e400 #x85858500 #x57575700 #x35353500
                                  #xeaeaea00 #x0c0c0c00 #xaeaeae00 #x41414100
                                  #x23232300 #xefefef00 #x6b6b6b00 #x93939300
                                  #x45454500 #x19191900 #xa5a5a500 #x21212100
                                  #xededed00 #x0e0e0e00 #x4f4f4f00 #x4e4e4e00
                                  #x1d1d1d00 #x65656500 #x92929200 #xbdbdbd00
                                  #x86868600 #xb8b8b800 #xafafaf00 #x8f8f8f00
                                  #x7c7c7c00 #xebebeb00 #x1f1f1f00 #xcecece00
                                  #x3e3e3e00 #x30303000 #xdcdcdc00 #x5f5f5f00
                                  #x5e5e5e00 #xc5c5c500 #x0b0b0b00 #x1a1a1a00
                                  #xa6a6a600 #xe1e1e100 #x39393900 #xcacaca00
                                  #xd5d5d500 #x47474700 #x5d5d5d00 #x3d3d3d00
                                  #xd9d9d900 #x01010100 #x5a5a5a00 #xd6d6d600
                                  #x51515100 #x56565600 #x6c6c6c00 #x4d4d4d00
                                  #x8b8b8b00 #x0d0d0d00 #x9a9a9a00 #x66666600
                                  #xfbfbfb00 #xcccccc00 #xb0b0b000 #x2d2d2d00
                                  #x74747400 #x12121200 #x2b2b2b00 #x20202000
                                  #xf0f0f000 #xb1b1b100 #x84848400 #x99999900
                                  #xdfdfdf00 #x4c4c4c00 #xcbcbcb00 #xc2c2c200
                                  #x34343400 #x7e7e7e00 #x76767600 #x05050500
                                  #x6d6d6d00 #xb7b7b700 #xa9a9a900 #x31313100
                                  #xd1d1d100 #x17171700 #x04040400 #xd7d7d700
                                  #x14141400 #x58585800 #x3a3a3a00 #x61616100
                                  #xdedede00 #x1b1b1b00 #x11111100 #x1c1c1c00
                                  #x32323200 #x0f0f0f00 #x9c9c9c00 #x16161600
                                  #x53535300 #x18181800 #xf2f2f200 #x22222200
                                  #xfefefe00 #x44444400 #xcfcfcf00 #xb2b2b200
                                  #xc3c3c300 #xb5b5b500 #x7a7a7a00 #x91919100
                                  #x24242400 #x08080800 #xe8e8e800 #xa8a8a800
                                  #x60606000 #xfcfcfc00 #x69696900 #x50505000
                                  #xaaaaaa00 #xd0d0d000 #xa0a0a000 #x7d7d7d00
                                  #xa1a1a100 #x89898900 #x62626200 #x97979700
                                  #x54545400 #x5b5b5b00 #x1e1e1e00 #x95959500
                                  #xe0e0e000 #xffffff00 #x64646400 #xd2d2d200
                                  #x10101000 #xc4c4c400 #x00000000 #x48484800
                                  #xa3a3a300 #xf7f7f700 #x75757500 #xdbdbdb00
                                  #x8a8a8a00 #x03030300 #xe6e6e600 #xdadada00
                                  #x09090900 #x3f3f3f00 #xdddddd00 #x94949400
                                  #x87878700 #x5c5c5c00 #x83838300 #x02020200
                                  #xcdcdcd00 #x4a4a4a00 #x90909000 #x33333300
                                  #x73737300 #x67676700 #xf6f6f600 #xf3f3f300
                                  #x9d9d9d00 #x7f7f7f00 #xbfbfbf00 #xe2e2e200
                                  #x52525200 #x9b9b9b00 #xd8d8d800 #x26262600
                                  #xc8c8c800 #x37373700 #xc6c6c600 #x3b3b3b00
                                  #x81818100 #x96969600 #x6f6f6f00 #x4b4b4b00
                                  #x13131300 #xbebebe00 #x63636300 #x2e2e2e00
                                  #xe9e9e900 #x79797900 #xa7a7a700 #x8c8c8c00
                                  #x9f9f9f00 #x6e6e6e00 #xbcbcbc00 #x8e8e8e00
                                  #x29292900 #xf5f5f500 #xf9f9f900 #xb6b6b600
                                  #x2f2f2f00 #xfdfdfd00 #xb4b4b400 #x59595900
                                  #x78787800 #x98989800 #x06060600 #x6a6a6a00
                                  #xe7e7e700 #x46464600 #x71717100 #xbababa00
                                  #xd4d4d400 #x25252500 #xababab00 #x42424200
                                  #x88888800 #xa2a2a200 #x8d8d8d00 #xfafafa00
                                  #x72727200 #x07070700 #xb9b9b900 #x55555500
                                  #xf8f8f800 #xeeeeee00 #xacacac00 #x0a0a0a00
                                  #x36363600 #x49494900 #x2a2a2a00 #x68686800
                                  #x3c3c3c00 #x38383800 #xf1f1f100 #xa4a4a400
                                  #x40404000 #x28282800 #xd3d3d300 #x7b7b7b00
                                  #xbbbbbb00 #xc9c9c900 #x43434300 #xc1c1c100
                                  #x15151500 #xe3e3e300 #xadadad00 #xf4f4f400
                                  #x77777700 #xc7c7c700 #x80808000 #x9e9e9e00)))

(defconst +camellia-sbox2+
  (make-array 256
              :element-type '(unsigned-byte 32)
              :initial-contents '(#x00e0e0e0 #x00050505 #x00585858 #x00d9d9d9
                                  #x00676767 #x004e4e4e #x00818181 #x00cbcbcb
                                  #x00c9c9c9 #x000b0b0b #x00aeaeae #x006a6a6a
                                  #x00d5d5d5 #x00181818 #x005d5d5d #x00828282
                                  #x00464646 #x00dfdfdf #x00d6d6d6 #x00272727
                                  #x008a8a8a #x00323232 #x004b4b4b #x00424242
                                  #x00dbdbdb #x001c1c1c #x009e9e9e #x009c9c9c
                                  #x003a3a3a #x00cacaca #x00252525 #x007b7b7b
                                  #x000d0d0d #x00717171 #x005f5f5f #x001f1f1f
                                  #x00f8f8f8 #x00d7d7d7 #x003e3e3e #x009d9d9d
                                  #x007c7c7c #x00606060 #x00b9b9b9 #x00bebebe
                                  #x00bcbcbc #x008b8b8b #x00161616 #x00343434
                                  #x004d4d4d #x00c3c3c3 #x00727272 #x00959595
                                  #x00ababab #x008e8e8e #x00bababa #x007a7a7a
                                  #x00b3b3b3 #x00020202 #x00b4b4b4 #x00adadad
                                  #x00a2a2a2 #x00acacac #x00d8d8d8 #x009a9a9a
                                  #x00171717 #x001a1a1a #x00353535 #x00cccccc
                                  #x00f7f7f7 #x00999999 #x00616161 #x005a5a5a
                                  #x00e8e8e8 #x00242424 #x00565656 #x00404040
                                  #x00e1e1e1 #x00636363 #x00090909 #x00333333
                                  #x00bfbfbf #x00989898 #x00979797 #x00858585
                                  #x00686868 #x00fcfcfc #x00ececec #x000a0a0a
                                  #x00dadada #x006f6f6f #x00535353 #x00626262
                                  #x00a3a3a3 #x002e2e2e #x00080808 #x00afafaf
                                  #x00282828 #x00b0b0b0 #x00747474 #x00c2c2c2
                                  #x00bdbdbd #x00363636 #x00222222 #x00383838
                                  #x00646464 #x001e1e1e #x00393939 #x002c2c2c
                                  #x00a6a6a6 #x00303030 #x00e5e5e5 #x00444444
                                  #x00fdfdfd #x00888888 #x009f9f9f #x00656565
                                  #x00878787 #x006b6b6b #x00f4f4f4 #x00232323
                                  #x00484848 #x00101010 #x00d1d1d1 #x00515151
                                  #x00c0c0c0 #x00f9f9f9 #x00d2d2d2 #x00a0a0a0
                                  #x00555555 #x00a1a1a1 #x00414141 #x00fafafa
                                  #x00434343 #x00131313 #x00c4c4c4 #x002f2f2f
                                  #x00a8a8a8 #x00b6b6b6 #x003c3c3c #x002b2b2b
                                  #x00c1c1c1 #x00ffffff #x00c8c8c8 #x00a5a5a5
                                  #x00202020 #x00898989 #x00000000 #x00909090
                                  #x00474747 #x00efefef #x00eaeaea #x00b7b7b7
                                  #x00151515 #x00060606 #x00cdcdcd #x00b5b5b5
                                  #x00121212 #x007e7e7e #x00bbbbbb #x00292929
                                  #x000f0f0f #x00b8b8b8 #x00070707 #x00040404
                                  #x009b9b9b #x00949494 #x00212121 #x00666666
                                  #x00e6e6e6 #x00cecece #x00ededed #x00e7e7e7
                                  #x003b3b3b #x00fefefe #x007f7f7f #x00c5c5c5
                                  #x00a4a4a4 #x00373737 #x00b1b1b1 #x004c4c4c
                                  #x00919191 #x006e6e6e #x008d8d8d #x00767676
                                  #x00030303 #x002d2d2d #x00dedede #x00969696
                                  #x00262626 #x007d7d7d #x00c6c6c6 #x005c5c5c
                                  #x00d3d3d3 #x00f2f2f2 #x004f4f4f #x00191919
                                  #x003f3f3f #x00dcdcdc #x00797979 #x001d1d1d
                                  #x00525252 #x00ebebeb #x00f3f3f3 #x006d6d6d
                                  #x005e5e5e #x00fbfbfb #x00696969 #x00b2b2b2
                                  #x00f0f0f0 #x00313131 #x000c0c0c #x00d4d4d4
                                  #x00cfcfcf #x008c8c8c #x00e2e2e2 #x00757575
                                  #x00a9a9a9 #x004a4a4a #x00575757 #x00848484
                                  #x00111111 #x00454545 #x001b1b1b #x00f5f5f5
                                  #x00e4e4e4 #x000e0e0e #x00737373 #x00aaaaaa
                                  #x00f1f1f1 #x00dddddd #x00595959 #x00141414
                                  #x006c6c6c #x00929292 #x00545454 #x00d0d0d0
                                  #x00787878 #x00707070 #x00e3e3e3 #x00494949
                                  #x00808080 #x00505050 #x00a7a7a7 #x00f6f6f6
                                  #x00777777 #x00939393 #x00868686 #x00838383
                                  #x002a2a2a #x00c7c7c7 #x005b5b5b #x00e9e9e9
                                  #x00eeeeee #x008f8f8f #x00010101 #x003d3d3d)))

(defconst +camellia-sbox3+
  (make-array 256
              :element-type '(unsigned-byte 32)
              :initial-contents '(#x38003838 #x41004141 #x16001616 #x76007676
                                  #xd900d9d9 #x93009393 #x60006060 #xf200f2f2
                                  #x72007272 #xc200c2c2 #xab00abab #x9a009a9a
                                  #x75007575 #x06000606 #x57005757 #xa000a0a0
                                  #x91009191 #xf700f7f7 #xb500b5b5 #xc900c9c9
                                  #xa200a2a2 #x8c008c8c #xd200d2d2 #x90009090
                                  #xf600f6f6 #x07000707 #xa700a7a7 #x27002727
                                  #x8e008e8e #xb200b2b2 #x49004949 #xde00dede
                                  #x43004343 #x5c005c5c #xd700d7d7 #xc700c7c7
                                  #x3e003e3e #xf500f5f5 #x8f008f8f #x67006767
                                  #x1f001f1f #x18001818 #x6e006e6e #xaf00afaf
                                  #x2f002f2f #xe200e2e2 #x85008585 #x0d000d0d
                                  #x53005353 #xf000f0f0 #x9c009c9c #x65006565
                                  #xea00eaea #xa300a3a3 #xae00aeae #x9e009e9e
                                  #xec00ecec #x80008080 #x2d002d2d #x6b006b6b
                                  #xa800a8a8 #x2b002b2b #x36003636 #xa600a6a6
                                  #xc500c5c5 #x86008686 #x4d004d4d #x33003333
                                  #xfd00fdfd #x66006666 #x58005858 #x96009696
                                  #x3a003a3a #x09000909 #x95009595 #x10001010
                                  #x78007878 #xd800d8d8 #x42004242 #xcc00cccc
                                  #xef00efef #x26002626 #xe500e5e5 #x61006161
                                  #x1a001a1a #x3f003f3f #x3b003b3b #x82008282
                                  #xb600b6b6 #xdb00dbdb #xd400d4d4 #x98009898
                                  #xe800e8e8 #x8b008b8b #x02000202 #xeb00ebeb
                                  #x0a000a0a #x2c002c2c #x1d001d1d #xb000b0b0
                                  #x6f006f6f #x8d008d8d #x88008888 #x0e000e0e
                                  #x19001919 #x87008787 #x4e004e4e #x0b000b0b
                                  #xa900a9a9 #x0c000c0c #x79007979 #x11001111
                                  #x7f007f7f #x22002222 #xe700e7e7 #x59005959
                                  #xe100e1e1 #xda00dada #x3d003d3d #xc800c8c8
                                  #x12001212 #x04000404 #x74007474 #x54005454
                                  #x30003030 #x7e007e7e #xb400b4b4 #x28002828
                                  #x55005555 #x68006868 #x50005050 #xbe00bebe
                                  #xd000d0d0 #xc400c4c4 #x31003131 #xcb00cbcb
                                  #x2a002a2a #xad00adad #x0f000f0f #xca00caca
                                  #x70007070 #xff00ffff #x32003232 #x69006969
                                  #x08000808 #x62006262 #x00000000 #x24002424
                                  #xd100d1d1 #xfb00fbfb #xba00baba #xed00eded
                                  #x45004545 #x81008181 #x73007373 #x6d006d6d
                                  #x84008484 #x9f009f9f #xee00eeee #x4a004a4a
                                  #xc300c3c3 #x2e002e2e #xc100c1c1 #x01000101
                                  #xe600e6e6 #x25002525 #x48004848 #x99009999
                                  #xb900b9b9 #xb300b3b3 #x7b007b7b #xf900f9f9
                                  #xce00cece #xbf00bfbf #xdf00dfdf #x71007171
                                  #x29002929 #xcd00cdcd #x6c006c6c #x13001313
                                  #x64006464 #x9b009b9b #x63006363 #x9d009d9d
                                  #xc000c0c0 #x4b004b4b #xb700b7b7 #xa500a5a5
                                  #x89008989 #x5f005f5f #xb100b1b1 #x17001717
                                  #xf400f4f4 #xbc00bcbc #xd300d3d3 #x46004646
                                  #xcf00cfcf #x37003737 #x5e005e5e #x47004747
                                  #x94009494 #xfa00fafa #xfc00fcfc #x5b005b5b
                                  #x97009797 #xfe00fefe #x5a005a5a #xac00acac
                                  #x3c003c3c #x4c004c4c #x03000303 #x35003535
                                  #xf300f3f3 #x23002323 #xb800b8b8 #x5d005d5d
                                  #x6a006a6a #x92009292 #xd500d5d5 #x21002121
                                  #x44004444 #x51005151 #xc600c6c6 #x7d007d7d
                                  #x39003939 #x83008383 #xdc00dcdc #xaa00aaaa
                                  #x7c007c7c #x77007777 #x56005656 #x05000505
                                  #x1b001b1b #xa400a4a4 #x15001515 #x34003434
                                  #x1e001e1e #x1c001c1c #xf800f8f8 #x52005252
                                  #x20002020 #x14001414 #xe900e9e9 #xbd00bdbd
                                  #xdd00dddd #xe400e4e4 #xa100a1a1 #xe000e0e0
                                  #x8a008a8a #xf100f1f1 #xd600d6d6 #x7a007a7a
                                  #xbb00bbbb #xe300e3e3 #x40004040 #x4f004f4f)))

(defconst +camellia-sbox4+
  (make-array 256
              :element-type '(unsigned-byte 32)
              :initial-contents '(#x70700070 #x2c2c002c #xb3b300b3 #xc0c000c0
                                  #xe4e400e4 #x57570057 #xeaea00ea #xaeae00ae
                                  #x23230023 #x6b6b006b #x45450045 #xa5a500a5
                                  #xeded00ed #x4f4f004f #x1d1d001d #x92920092
                                  #x86860086 #xafaf00af #x7c7c007c #x1f1f001f
                                  #x3e3e003e #xdcdc00dc #x5e5e005e #x0b0b000b
                                  #xa6a600a6 #x39390039 #xd5d500d5 #x5d5d005d
                                  #xd9d900d9 #x5a5a005a #x51510051 #x6c6c006c
                                  #x8b8b008b #x9a9a009a #xfbfb00fb #xb0b000b0
                                  #x74740074 #x2b2b002b #xf0f000f0 #x84840084
                                  #xdfdf00df #xcbcb00cb #x34340034 #x76760076
                                  #x6d6d006d #xa9a900a9 #xd1d100d1 #x04040004
                                  #x14140014 #x3a3a003a #xdede00de #x11110011
                                  #x32320032 #x9c9c009c #x53530053 #xf2f200f2
                                  #xfefe00fe #xcfcf00cf #xc3c300c3 #x7a7a007a
                                  #x24240024 #xe8e800e8 #x60600060 #x69690069
                                  #xaaaa00aa #xa0a000a0 #xa1a100a1 #x62620062
                                  #x54540054 #x1e1e001e #xe0e000e0 #x64640064
                                  #x10100010 #x00000000 #xa3a300a3 #x75750075
                                  #x8a8a008a #xe6e600e6 #x09090009 #xdddd00dd
                                  #x87870087 #x83830083 #xcdcd00cd #x90900090
                                  #x73730073 #xf6f600f6 #x9d9d009d #xbfbf00bf
                                  #x52520052 #xd8d800d8 #xc8c800c8 #xc6c600c6
                                  #x81810081 #x6f6f006f #x13130013 #x63630063
                                  #xe9e900e9 #xa7a700a7 #x9f9f009f #xbcbc00bc
                                  #x29290029 #xf9f900f9 #x2f2f002f #xb4b400b4
                                  #x78780078 #x06060006 #xe7e700e7 #x71710071
                                  #xd4d400d4 #xabab00ab #x88880088 #x8d8d008d
                                  #x72720072 #xb9b900b9 #xf8f800f8 #xacac00ac
                                  #x36360036 #x2a2a002a #x3c3c003c #xf1f100f1
                                  #x40400040 #xd3d300d3 #xbbbb00bb #x43430043
                                  #x15150015 #xadad00ad #x77770077 #x80800080
                                  #x82820082 #xecec00ec #x27270027 #xe5e500e5
                                  #x85850085 #x35350035 #x0c0c000c #x41410041
                                  #xefef00ef #x93930093 #x19190019 #x21210021
                                  #x0e0e000e #x4e4e004e #x65650065 #xbdbd00bd
                                  #xb8b800b8 #x8f8f008f #xebeb00eb #xcece00ce
                                  #x30300030 #x5f5f005f #xc5c500c5 #x1a1a001a
                                  #xe1e100e1 #xcaca00ca #x47470047 #x3d3d003d
                                  #x01010001 #xd6d600d6 #x56560056 #x4d4d004d
                                  #x0d0d000d #x66660066 #xcccc00cc #x2d2d002d
                                  #x12120012 #x20200020 #xb1b100b1 #x99990099
                                  #x4c4c004c #xc2c200c2 #x7e7e007e #x05050005
                                  #xb7b700b7 #x31310031 #x17170017 #xd7d700d7
                                  #x58580058 #x61610061 #x1b1b001b #x1c1c001c
                                  #x0f0f000f #x16160016 #x18180018 #x22220022
                                  #x44440044 #xb2b200b2 #xb5b500b5 #x91910091
                                  #x08080008 #xa8a800a8 #xfcfc00fc #x50500050
                                  #xd0d000d0 #x7d7d007d #x89890089 #x97970097
                                  #x5b5b005b #x95950095 #xffff00ff #xd2d200d2
                                  #xc4c400c4 #x48480048 #xf7f700f7 #xdbdb00db
                                  #x03030003 #xdada00da #x3f3f003f #x94940094
                                  #x5c5c005c #x02020002 #x4a4a004a #x33330033
                                  #x67670067 #xf3f300f3 #x7f7f007f #xe2e200e2
                                  #x9b9b009b #x26260026 #x37370037 #x3b3b003b
                                  #x96960096 #x4b4b004b #xbebe00be #x2e2e002e
                                  #x79790079 #x8c8c008c #x6e6e006e #x8e8e008e
                                  #xf5f500f5 #xb6b600b6 #xfdfd00fd #x59590059
                                  #x98980098 #x6a6a006a #x46460046 #xbaba00ba
                                  #x25250025 #x42420042 #xa2a200a2 #xfafa00fa
                                  #x07070007 #x55550055 #xeeee00ee #x0a0a000a
                                  #x49490049 #x68680068 #x38380038 #xa4a400a4
                                  #x28280028 #x7b7b007b #xc9c900c9 #xc1c100c1
                                  #xe3e300e3 #xf4f400f4 #xc7c700c7 #x9e9e009e)))

(defconst +camellia-sigma+
  (make-array 12
              :element-type '(unsigned-byte 32)
              :initial-contents '(#xa09e667f #x3bcc908b #xb67ae858 #x4caa73b2
                                  #xc6ef372f #xe94f82be #x54ff53a5 #xf1d36f1c
                                  #x10e527fa #xde682d1d #xb05688c2 #xb3e6c1fd)))

(defconst +camellia-ksft1+
  (make-array 26
              :element-type '(unsigned-byte 7)
              :initial-contents '(0 64 0 64 15 79 15 79 30 94 45 109 45
                                  124 60 124 77 13 94 30 94 30 111 47 111 47)))

(defconst +camellia-kidx1+
  (make-array 26
              :element-type '(unsigned-byte 4)
              :initial-contents '(0 0 8 8 0 0 8 8 8 8 0 0 8
                                  0 8 8 0 0 0 0 8 8 0 0 8 8)))

(defconst +camellia-ksft2+
  (make-array 34
              :element-type '(unsigned-byte 7)
              :initial-contents '(0 64 0 64 15 79 15 79 30 94 30 94 45 109 45 109 60
                                  124 60 124 60 124 77 13 77 13 94 30 94 30 111 47 111 47)))

(defconst +camellia-kidx2+
  (make-array 34
              :element-type '(unsigned-byte 4)
              :initial-contents '(0 0 12 12 4 4 8 8 4 4 12 12 0 0 8 8 0
                                  0 4 4 12 12 0 0 8 8 4 4 8 8 0 0 12 12)))

(declaim (ftype (function ((simple-array (unsigned-byte 32) (*))
                           fixnum
                           (simple-array (unsigned-byte 32) (*))
                           fixnum
                           fixnum))
                camellia-feistel)
         (inline camellia-feistel))
(defun camellia-feistel (data data-start keys keys-start key-offset)
  (declare (type (simple-array (unsigned-byte 32) (*)) data keys)
           (type fixnum data-start keys-start key-offset)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (macrolet ((sbox1 (n)
               `(aref +camellia-sbox1+ (logand ,n 255)))
             (sbox2 (n)
               `(aref +camellia-sbox2+ (logand ,n 255)))
             (sbox3 (n)
               `(aref +camellia-sbox3+ (logand ,n 255)))
             (sbox4 (n)
               `(aref +camellia-sbox4+ (logand ,n 255))))
    (let ((d 0)
          (u 0)
          (s1 0)
          (s2 0))
      (declare (type (unsigned-byte 32) d u s1 s2))
      (setf s1 (logxor (aref data data-start)
                       (aref keys keys-start))
            u (logxor (sbox4 s1)
                      (sbox3 (mod32ash s1 -8))
                      (sbox2 (mod32ash s1 -16))
                      (sbox1 (mod32ash s1 -24)))
            s2 (logxor (aref data (+ data-start 1))
                       (aref keys (+ keys-start 1)))
            d (logxor (sbox1 s2)
                      (sbox4 (mod32ash s2 -8))
                      (sbox3 (mod32ash s2 -16))
                      (sbox2 (mod32ash s2 -24))))
      (setf (aref data (+ data-start 2)) (logxor (aref data (+ data-start 2)) d u)
            (aref data (+ data-start 3)) (logxor (aref data (+ data-start 3)) d u (ror32 u 8)))
      (setf s1 (logxor (aref data (+ data-start 2))
                       (aref keys (+ keys-start key-offset)))
            u (logxor (sbox4 s1)
                      (sbox3 (mod32ash s1 -8))
                      (sbox2 (mod32ash s1 -16))
                      (sbox1 (mod32ash s1 -24)))
            s2 (logxor (aref data (+ data-start 3))
                       (aref keys (+ keys-start key-offset 1)))
            d (logxor (sbox1 s2)
                      (sbox4 (mod32ash s2 -8))
                      (sbox3 (mod32ash s2 -16))
                      (sbox2 (mod32ash s2 -24))))
      (setf (aref data data-start) (logxor (aref data data-start) d u)
            (aref data (+ data-start 1)) (logxor (aref data (+ data-start 1)) d u (ror32 u 8)))
      (values))))

(defclass camellia (cipher 16-byte-block-mixin)
  ((round-keys :accessor round-keys
               :type (simple-array (unsigned-byte 32) (68)))
   (grand-rounds :accessor grand-rounds
                 :type (integer 3 4))))

(defmethod schedule-key ((cipher camellia) key)
  (declare (type (simple-array (unsigned-byte 8) (*)) key))
  (let ((round-keys (make-array 68 :element-type '(unsigned-byte 32)))
        (data (make-array 16 :element-type '(unsigned-byte 32))))
    (declare (type (simple-array (unsigned-byte 32) (*)) round-keys data)
             (dynamic-extent data))
    (ecase (length key)
      ((16)
       (dotimes (i 4)
         (setf (aref data i) (ub32ref/be key (* 4 i))))
       (fill data 0 :start 4 :end 8))

      ((24)
       (dotimes (i 6)
         (setf (aref data i) (ub32ref/be key (* 4 i))))
       (setf (aref data 6) (mod32lognot (aref data 4))
             (aref data 7) (mod32lognot (aref data 5))))

      ((32)
       (dotimes (i 8)
         (setf (aref data i) (ub32ref/be key (* 4 i))))))

    (dotimes (i 4)
      (setf (aref data (+ i 8)) (logxor (aref data i) (aref data (+ i 4)))))
    (camellia-feistel data 8 +camellia-sigma+ 0 2)
    (dotimes (i 4)
      (setf (aref data (+ i 8)) (logxor (aref data (+ i 8)) (aref data i))))
    (camellia-feistel data 8 +camellia-sigma+ 4 2)

    (flet ((rotblock (in in-start out out-start n)
             (let* ((r (logand n 31))
                    (idx (ash n -5))
                    (idx1 (logand (1+ idx) 3))
                    (idx2 (logand (1+ idx1) 3)))
               (setf (aref out out-start) (logior (mod32ash (aref in (+ in-start idx))
                                                            r)
                                                  (mod32ash (aref in (+ in-start idx1))
                                                            (- r 32)))
                     (aref out (+ out-start 1)) (logior (mod32ash (aref in (+ in-start idx1))
                                                                  r)
                                                        (mod32ash (aref in (+ in-start idx2))
                                                                  (- r 32))))
               (values))))
      (if (= 16 (length key))
          (progn
            (setf (grand-rounds cipher) 3)
            (replace round-keys data :end2 4)
            (replace round-keys data :start1 4 :start2 8 :end2 12)
            (loop for i from 4 below 26 by 2 do
              (rotblock data (aref +camellia-kidx1+ i)
                        round-keys (* 2 i)
                        (aref +camellia-ksft1+ i))
              (rotblock data (aref +camellia-kidx1+ (1+ i))
                        round-keys (+ (* 2 i) 2)
                        (aref +camellia-ksft1+ (1+ i)))))
          (progn
            (setf (grand-rounds cipher) 4)
            (dotimes (i 4)
              (setf (aref data (+ i 12)) (logxor (aref data (+ i 8)) (aref data (+ i 4)))))
            (camellia-feistel data 12 +camellia-sigma+ 8 2)
            (replace round-keys data :end2 4)
            (replace round-keys data :start1 4 :start2 12 :end2 16)
            (loop for i from 4 below 34 by 2 do
              (rotblock data (aref +camellia-kidx2+ i)
                        round-keys (* 2 i)
                        (aref +camellia-ksft2+ i))
              (rotblock data (aref +camellia-kidx2+ (1+ i))
                        round-keys (+ (* 2 i) 2)
                        (aref +camellia-ksft2+ (1+ i)))))))
    (setf (round-keys cipher) round-keys)
    cipher))

(define-block-encryptor camellia 16
  (let ((round-keys (round-keys context))
        (keys-start 4)
        (grand-rounds (grand-rounds context))
        (data (make-array 4 :element-type '(unsigned-byte 32))))
    (declare (type (simple-array (unsigned-byte 32) (*)) round-keys data)
             (type fixnum keys-start grand-rounds)
             (dynamic-extent data))
    (setf (aref data 0) (logxor (ub32ref/be plaintext plaintext-start)
                                (aref round-keys 0))
          (aref data 1) (logxor (ub32ref/be plaintext (+ plaintext-start 4))
                                (aref round-keys 1))
          (aref data 2) (logxor (ub32ref/be plaintext (+ plaintext-start 8))
                                (aref round-keys 2))
          (aref data 3) (logxor (ub32ref/be plaintext (+ plaintext-start 12))
                                (aref round-keys 3)))

    (dotimes (i grand-rounds)
      (dotimes (j 3)
        (camellia-feistel data 0 round-keys keys-start 2)
        (incf keys-start 4))
      (when (< i (1- grand-rounds))
        (setf (aref data 1) (logxor (aref data 1)
                                    (rol32 (logand (aref data 0)
                                                   (aref round-keys keys-start))
                                           1))
              (aref data 0) (logxor (aref data 0)
                                    (logior (aref data 1) (aref round-keys (+ keys-start 1))))
              (aref data 2) (logxor (aref data 2)
                                    (logior (aref data 3) (aref round-keys (+ keys-start 3))))
              (aref data 3) (logxor (aref data 3)
                                    (rol32 (logand (aref data 2)
                                                   (aref round-keys (+ keys-start 2)))
                                           1)))
        (incf keys-start 4)))

    (rotatef (aref data 0) (aref data 2))
    (rotatef (aref data 1) (aref data 3))
    (setf (ub32ref/be ciphertext ciphertext-start)
          (logxor (aref data 0) (aref round-keys keys-start)))
    (setf (ub32ref/be ciphertext (+ ciphertext-start 4))
          (logxor (aref data 1) (aref round-keys (+ keys-start 1))))
    (setf (ub32ref/be ciphertext (+ ciphertext-start 8))
          (logxor (aref data 2) (aref round-keys (+ keys-start 2))))
    (setf (ub32ref/be ciphertext (+ ciphertext-start 12))
          (logxor (aref data 3) (aref round-keys (+ keys-start 3))))))

(define-block-decryptor camellia 16
  (let* ((round-keys (round-keys context))
         (grand-rounds (grand-rounds context))
         (keys-start (if (= 3 grand-rounds) 48 64))
         (data (make-array 4 :element-type '(unsigned-byte 32))))
    (declare (type (simple-array (unsigned-byte 32) (*)) round-keys data)
             (type fixnum keys-start grand-rounds)
             (dynamic-extent data))
    (setf (aref data 0) (logxor (ub32ref/be ciphertext ciphertext-start)
                                (aref round-keys keys-start))
          (aref data 1) (logxor (ub32ref/be ciphertext (+ ciphertext-start 4))
                                (aref round-keys (+ keys-start 1)))
          (aref data 2) (logxor (ub32ref/be ciphertext (+ ciphertext-start 8))
                                (aref round-keys (+ keys-start 2)))
          (aref data 3) (logxor (ub32ref/be ciphertext (+ ciphertext-start 12))
                                (aref round-keys (+ keys-start 3))))
    (decf keys-start 2)

    (dotimes (i grand-rounds)
      (dotimes (j 3)
        (camellia-feistel data 0 round-keys keys-start -2)
        (decf keys-start 4))
      (when (< i (1- grand-rounds))
        (setf (aref data 1) (logxor (aref data 1)
                                    (rol32 (logand (aref data 0)
                                                   (aref round-keys keys-start))
                                           1))
              (aref data 0) (logxor (aref data 0)
                                    (logior (aref data 1) (aref round-keys (+ keys-start 1))))
              (aref data 2) (logxor (aref data 2)
                                    (logior (aref data 3) (aref round-keys (- keys-start 1))))
              (aref data 3) (logxor (aref data 3)
                                    (rol32 (logand (aref data 2)
                                                   (aref round-keys (- keys-start 2)))
                                           1)))
        (decf keys-start 4)))

    (decf keys-start 2)
    (rotatef (aref data 0) (aref data 2))
    (rotatef (aref data 1) (aref data 3))
    (setf (ub32ref/be plaintext plaintext-start)
          (logxor (aref data 0) (aref round-keys keys-start)))
    (setf (ub32ref/be plaintext (+ plaintext-start 4))
          (logxor (aref data 1) (aref round-keys (+ keys-start 1))))
    (setf (ub32ref/be plaintext (+ plaintext-start 8))
          (logxor (aref data 2) (aref round-keys (+ keys-start 2))))
    (setf (ub32ref/be plaintext (+ plaintext-start 12))
          (logxor (aref data 3) (aref round-keys (+ keys-start 3))))))

(defcipher camellia
  (:encrypt-function camellia-encrypt-block)
  (:decrypt-function camellia-decrypt-block)
  (:block-length 16)
  (:key-length (:fixed 16 24 32)))
