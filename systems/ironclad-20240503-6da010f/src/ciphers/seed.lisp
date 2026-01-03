;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; seed.lisp - implementation of the SEED block cipher

(in-package :crypto)


(defconst +seed-sbox0+
  (make-array 256
              :element-type '(unsigned-byte 32)
              :initial-contents '(#x2989a1a8 #x05858184 #x16c6d2d4 #x13c3d3d0
                                  #x14445054 #x1d0d111c #x2c8ca0ac #x25052124
                                  #x1d4d515c #x03434340 #x18081018 #x1e0e121c
                                  #x11415150 #x3cccf0fc #x0acac2c8 #x23436360
                                  #x28082028 #x04444044 #x20002020 #x1d8d919c
                                  #x20c0e0e0 #x22c2e2e0 #x08c8c0c8 #x17071314
                                  #x2585a1a4 #x0f8f838c #x03030300 #x3b4b7378
                                  #x3b8bb3b8 #x13031310 #x12c2d2d0 #x2ecee2ec
                                  #x30407070 #x0c8c808c #x3f0f333c #x2888a0a8
                                  #x32023230 #x1dcdd1dc #x36c6f2f4 #x34447074
                                  #x2ccce0ec #x15859194 #x0b0b0308 #x17475354
                                  #x1c4c505c #x1b4b5358 #x3d8db1bc #x01010100
                                  #x24042024 #x1c0c101c #x33437370 #x18889098
                                  #x10001010 #x0cccc0cc #x32c2f2f0 #x19c9d1d8
                                  #x2c0c202c #x27c7e3e4 #x32427270 #x03838380
                                  #x1b8b9398 #x11c1d1d0 #x06868284 #x09c9c1c8
                                  #x20406060 #x10405050 #x2383a3a0 #x2bcbe3e8
                                  #x0d0d010c #x3686b2b4 #x1e8e929c #x0f4f434c
                                  #x3787b3b4 #x1a4a5258 #x06c6c2c4 #x38487078
                                  #x2686a2a4 #x12021210 #x2f8fa3ac #x15c5d1d4
                                  #x21416160 #x03c3c3c0 #x3484b0b4 #x01414140
                                  #x12425250 #x3d4d717c #x0d8d818c #x08080008
                                  #x1f0f131c #x19899198 #x00000000 #x19091118
                                  #x04040004 #x13435350 #x37c7f3f4 #x21c1e1e0
                                  #x3dcdf1fc #x36467274 #x2f0f232c #x27072324
                                  #x3080b0b0 #x0b8b8388 #x0e0e020c #x2b8ba3a8
                                  #x2282a2a0 #x2e4e626c #x13839390 #x0d4d414c
                                  #x29496168 #x3c4c707c #x09090108 #x0a0a0208
                                  #x3f8fb3bc #x2fcfe3ec #x33c3f3f0 #x05c5c1c4
                                  #x07878384 #x14041014 #x3ecef2fc #x24446064
                                  #x1eced2dc #x2e0e222c #x0b4b4348 #x1a0a1218
                                  #x06060204 #x21012120 #x2b4b6368 #x26466264
                                  #x02020200 #x35c5f1f4 #x12829290 #x0a8a8288
                                  #x0c0c000c #x3383b3b0 #x3e4e727c #x10c0d0d0
                                  #x3a4a7278 #x07474344 #x16869294 #x25c5e1e4
                                  #x26062224 #x00808080 #x2d8da1ac #x1fcfd3dc
                                  #x2181a1a0 #x30003030 #x37073334 #x2e8ea2ac
                                  #x36063234 #x15051114 #x22022220 #x38083038
                                  #x34c4f0f4 #x2787a3a4 #x05454144 #x0c4c404c
                                  #x01818180 #x29c9e1e8 #x04848084 #x17879394
                                  #x35053134 #x0bcbc3c8 #x0ecec2cc #x3c0c303c
                                  #x31417170 #x11011110 #x07c7c3c4 #x09898188
                                  #x35457174 #x3bcbf3f8 #x1acad2d8 #x38c8f0f8
                                  #x14849094 #x19495158 #x02828280 #x04c4c0c4
                                  #x3fcff3fc #x09494148 #x39093138 #x27476364
                                  #x00c0c0c0 #x0fcfc3cc #x17c7d3d4 #x3888b0b8
                                  #x0f0f030c #x0e8e828c #x02424240 #x23032320
                                  #x11819190 #x2c4c606c #x1bcbd3d8 #x2484a0a4
                                  #x34043034 #x31c1f1f0 #x08484048 #x02c2c2c0
                                  #x2f4f636c #x3d0d313c #x2d0d212c #x00404040
                                  #x3e8eb2bc #x3e0e323c #x3c8cb0bc #x01c1c1c0
                                  #x2a8aa2a8 #x3a8ab2b8 #x0e4e424c #x15455154
                                  #x3b0b3338 #x1cccd0dc #x28486068 #x3f4f737c
                                  #x1c8c909c #x18c8d0d8 #x0a4a4248 #x16465254
                                  #x37477374 #x2080a0a0 #x2dcde1ec #x06464244
                                  #x3585b1b4 #x2b0b2328 #x25456164 #x3acaf2f8
                                  #x23c3e3e0 #x3989b1b8 #x3181b1b0 #x1f8f939c
                                  #x1e4e525c #x39c9f1f8 #x26c6e2e4 #x3282b2b0
                                  #x31013130 #x2acae2e8 #x2d4d616c #x1f4f535c
                                  #x24c4e0e4 #x30c0f0f0 #x0dcdc1cc #x08888088
                                  #x16061214 #x3a0a3238 #x18485058 #x14c4d0d4
                                  #x22426260 #x29092128 #x07070304 #x33033330
                                  #x28c8e0e8 #x1b0b1318 #x05050104 #x39497178
                                  #x10809090 #x2a4a6268 #x2a0a2228 #x1a8a9298)))

(defconst +seed-sbox1+
  (make-array 256
              :element-type '(unsigned-byte 32)
              :initial-contents '(#x38380830 #xe828c8e0 #x2c2d0d21 #xa42686a2
                                  #xcc0fcfc3 #xdc1eced2 #xb03383b3 #xb83888b0
                                  #xac2f8fa3 #x60204060 #x54154551 #xc407c7c3
                                  #x44044440 #x6c2f4f63 #x682b4b63 #x581b4b53
                                  #xc003c3c3 #x60224262 #x30330333 #xb43585b1
                                  #x28290921 #xa02080a0 #xe022c2e2 #xa42787a3
                                  #xd013c3d3 #x90118191 #x10110111 #x04060602
                                  #x1c1c0c10 #xbc3c8cb0 #x34360632 #x480b4b43
                                  #xec2fcfe3 #x88088880 #x6c2c4c60 #xa82888a0
                                  #x14170713 #xc404c4c0 #x14160612 #xf434c4f0
                                  #xc002c2c2 #x44054541 #xe021c1e1 #xd416c6d2
                                  #x3c3f0f33 #x3c3d0d31 #x8c0e8e82 #x98188890
                                  #x28280820 #x4c0e4e42 #xf436c6f2 #x3c3e0e32
                                  #xa42585a1 #xf839c9f1 #x0c0d0d01 #xdc1fcfd3
                                  #xd818c8d0 #x282b0b23 #x64264662 #x783a4a72
                                  #x24270723 #x2c2f0f23 #xf031c1f1 #x70324272
                                  #x40024242 #xd414c4d0 #x40014141 #xc000c0c0
                                  #x70334373 #x64274763 #xac2c8ca0 #x880b8b83
                                  #xf437c7f3 #xac2d8da1 #x80008080 #x1c1f0f13
                                  #xc80acac2 #x2c2c0c20 #xa82a8aa2 #x34340430
                                  #xd012c2d2 #x080b0b03 #xec2ecee2 #xe829c9e1
                                  #x5c1d4d51 #x94148490 #x18180810 #xf838c8f0
                                  #x54174753 #xac2e8ea2 #x08080800 #xc405c5c1
                                  #x10130313 #xcc0dcdc1 #x84068682 #xb83989b1
                                  #xfc3fcff3 #x7c3d4d71 #xc001c1c1 #x30310131
                                  #xf435c5f1 #x880a8a82 #x682a4a62 #xb03181b1
                                  #xd011c1d1 #x20200020 #xd417c7d3 #x00020202
                                  #x20220222 #x04040400 #x68284860 #x70314171
                                  #x04070703 #xd81bcbd3 #x9c1d8d91 #x98198991
                                  #x60214161 #xbc3e8eb2 #xe426c6e2 #x58194951
                                  #xdc1dcdd1 #x50114151 #x90108090 #xdc1cccd0
                                  #x981a8a92 #xa02383a3 #xa82b8ba3 #xd010c0d0
                                  #x80018181 #x0c0f0f03 #x44074743 #x181a0a12
                                  #xe023c3e3 #xec2ccce0 #x8c0d8d81 #xbc3f8fb3
                                  #x94168692 #x783b4b73 #x5c1c4c50 #xa02282a2
                                  #xa02181a1 #x60234363 #x20230323 #x4c0d4d41
                                  #xc808c8c0 #x9c1e8e92 #x9c1c8c90 #x383a0a32
                                  #x0c0c0c00 #x2c2e0e22 #xb83a8ab2 #x6c2e4e62
                                  #x9c1f8f93 #x581a4a52 #xf032c2f2 #x90128292
                                  #xf033c3f3 #x48094941 #x78384870 #xcc0cccc0
                                  #x14150511 #xf83bcbf3 #x70304070 #x74354571
                                  #x7c3f4f73 #x34350531 #x10100010 #x00030303
                                  #x64244460 #x6c2d4d61 #xc406c6c2 #x74344470
                                  #xd415c5d1 #xb43484b0 #xe82acae2 #x08090901
                                  #x74364672 #x18190911 #xfc3ecef2 #x40004040
                                  #x10120212 #xe020c0e0 #xbc3d8db1 #x04050501
                                  #xf83acaf2 #x00010101 #xf030c0f0 #x282a0a22
                                  #x5c1e4e52 #xa82989a1 #x54164652 #x40034343
                                  #x84058581 #x14140410 #x88098981 #x981b8b93
                                  #xb03080b0 #xe425c5e1 #x48084840 #x78394971
                                  #x94178793 #xfc3cccf0 #x1c1e0e12 #x80028282
                                  #x20210121 #x8c0c8c80 #x181b0b13 #x5c1f4f53
                                  #x74374773 #x54144450 #xb03282b2 #x1c1d0d11
                                  #x24250521 #x4c0f4f43 #x00000000 #x44064642
                                  #xec2dcde1 #x58184850 #x50124252 #xe82bcbe3
                                  #x7c3e4e72 #xd81acad2 #xc809c9c1 #xfc3dcdf1
                                  #x30300030 #x94158591 #x64254561 #x3c3c0c30
                                  #xb43686b2 #xe424c4e0 #xb83b8bb3 #x7c3c4c70
                                  #x0c0e0e02 #x50104050 #x38390931 #x24260622
                                  #x30320232 #x84048480 #x68294961 #x90138393
                                  #x34370733 #xe427c7e3 #x24240420 #xa42484a0
                                  #xc80bcbc3 #x50134353 #x080a0a02 #x84078783
                                  #xd819c9d1 #x4c0c4c40 #x80038383 #x8c0f8f83
                                  #xcc0ecec2 #x383b0b33 #x480a4a42 #xb43787b3)))

(defconst +seed-sbox2+
  (make-array 256
              :element-type '(unsigned-byte 32)
              :initial-contents '(#xa1a82989 #x81840585 #xd2d416c6 #xd3d013c3
                                  #x50541444 #x111c1d0d #xa0ac2c8c #x21242505
                                  #x515c1d4d #x43400343 #x10181808 #x121c1e0e
                                  #x51501141 #xf0fc3ccc #xc2c80aca #x63602343
                                  #x20282808 #x40440444 #x20202000 #x919c1d8d
                                  #xe0e020c0 #xe2e022c2 #xc0c808c8 #x13141707
                                  #xa1a42585 #x838c0f8f #x03000303 #x73783b4b
                                  #xb3b83b8b #x13101303 #xd2d012c2 #xe2ec2ece
                                  #x70703040 #x808c0c8c #x333c3f0f #xa0a82888
                                  #x32303202 #xd1dc1dcd #xf2f436c6 #x70743444
                                  #xe0ec2ccc #x91941585 #x03080b0b #x53541747
                                  #x505c1c4c #x53581b4b #xb1bc3d8d #x01000101
                                  #x20242404 #x101c1c0c #x73703343 #x90981888
                                  #x10101000 #xc0cc0ccc #xf2f032c2 #xd1d819c9
                                  #x202c2c0c #xe3e427c7 #x72703242 #x83800383
                                  #x93981b8b #xd1d011c1 #x82840686 #xc1c809c9
                                  #x60602040 #x50501040 #xa3a02383 #xe3e82bcb
                                  #x010c0d0d #xb2b43686 #x929c1e8e #x434c0f4f
                                  #xb3b43787 #x52581a4a #xc2c406c6 #x70783848
                                  #xa2a42686 #x12101202 #xa3ac2f8f #xd1d415c5
                                  #x61602141 #xc3c003c3 #xb0b43484 #x41400141
                                  #x52501242 #x717c3d4d #x818c0d8d #x00080808
                                  #x131c1f0f #x91981989 #x00000000 #x11181909
                                  #x00040404 #x53501343 #xf3f437c7 #xe1e021c1
                                  #xf1fc3dcd #x72743646 #x232c2f0f #x23242707
                                  #xb0b03080 #x83880b8b #x020c0e0e #xa3a82b8b
                                  #xa2a02282 #x626c2e4e #x93901383 #x414c0d4d
                                  #x61682949 #x707c3c4c #x01080909 #x02080a0a
                                  #xb3bc3f8f #xe3ec2fcf #xf3f033c3 #xc1c405c5
                                  #x83840787 #x10141404 #xf2fc3ece #x60642444
                                  #xd2dc1ece #x222c2e0e #x43480b4b #x12181a0a
                                  #x02040606 #x21202101 #x63682b4b #x62642646
                                  #x02000202 #xf1f435c5 #x92901282 #x82880a8a
                                  #x000c0c0c #xb3b03383 #x727c3e4e #xd0d010c0
                                  #x72783a4a #x43440747 #x92941686 #xe1e425c5
                                  #x22242606 #x80800080 #xa1ac2d8d #xd3dc1fcf
                                  #xa1a02181 #x30303000 #x33343707 #xa2ac2e8e
                                  #x32343606 #x11141505 #x22202202 #x30383808
                                  #xf0f434c4 #xa3a42787 #x41440545 #x404c0c4c
                                  #x81800181 #xe1e829c9 #x80840484 #x93941787
                                  #x31343505 #xc3c80bcb #xc2cc0ece #x303c3c0c
                                  #x71703141 #x11101101 #xc3c407c7 #x81880989
                                  #x71743545 #xf3f83bcb #xd2d81aca #xf0f838c8
                                  #x90941484 #x51581949 #x82800282 #xc0c404c4
                                  #xf3fc3fcf #x41480949 #x31383909 #x63642747
                                  #xc0c000c0 #xc3cc0fcf #xd3d417c7 #xb0b83888
                                  #x030c0f0f #x828c0e8e #x42400242 #x23202303
                                  #x91901181 #x606c2c4c #xd3d81bcb #xa0a42484
                                  #x30343404 #xf1f031c1 #x40480848 #xc2c002c2
                                  #x636c2f4f #x313c3d0d #x212c2d0d #x40400040
                                  #xb2bc3e8e #x323c3e0e #xb0bc3c8c #xc1c001c1
                                  #xa2a82a8a #xb2b83a8a #x424c0e4e #x51541545
                                  #x33383b0b #xd0dc1ccc #x60682848 #x737c3f4f
                                  #x909c1c8c #xd0d818c8 #x42480a4a #x52541646
                                  #x73743747 #xa0a02080 #xe1ec2dcd #x42440646
                                  #xb1b43585 #x23282b0b #x61642545 #xf2f83aca
                                  #xe3e023c3 #xb1b83989 #xb1b03181 #x939c1f8f
                                  #x525c1e4e #xf1f839c9 #xe2e426c6 #xb2b03282
                                  #x31303101 #xe2e82aca #x616c2d4d #x535c1f4f
                                  #xe0e424c4 #xf0f030c0 #xc1cc0dcd #x80880888
                                  #x12141606 #x32383a0a #x50581848 #xd0d414c4
                                  #x62602242 #x21282909 #x03040707 #x33303303
                                  #xe0e828c8 #x13181b0b #x01040505 #x71783949
                                  #x90901080 #x62682a4a #x22282a0a #x92981a8a)))

(defconst +seed-sbox3+
  (make-array 256
              :element-type '(unsigned-byte 32)
              :initial-contents '(#x08303838 #xc8e0e828 #x0d212c2d #x86a2a426
                                  #xcfc3cc0f #xced2dc1e #x83b3b033 #x88b0b838
                                  #x8fa3ac2f #x40606020 #x45515415 #xc7c3c407
                                  #x44404404 #x4f636c2f #x4b63682b #x4b53581b
                                  #xc3c3c003 #x42626022 #x03333033 #x85b1b435
                                  #x09212829 #x80a0a020 #xc2e2e022 #x87a3a427
                                  #xc3d3d013 #x81919011 #x01111011 #x06020406
                                  #x0c101c1c #x8cb0bc3c #x06323436 #x4b43480b
                                  #xcfe3ec2f #x88808808 #x4c606c2c #x88a0a828
                                  #x07131417 #xc4c0c404 #x06121416 #xc4f0f434
                                  #xc2c2c002 #x45414405 #xc1e1e021 #xc6d2d416
                                  #x0f333c3f #x0d313c3d #x8e828c0e #x88909818
                                  #x08202828 #x4e424c0e #xc6f2f436 #x0e323c3e
                                  #x85a1a425 #xc9f1f839 #x0d010c0d #xcfd3dc1f
                                  #xc8d0d818 #x0b23282b #x46626426 #x4a72783a
                                  #x07232427 #x0f232c2f #xc1f1f031 #x42727032
                                  #x42424002 #xc4d0d414 #x41414001 #xc0c0c000
                                  #x43737033 #x47636427 #x8ca0ac2c #x8b83880b
                                  #xc7f3f437 #x8da1ac2d #x80808000 #x0f131c1f
                                  #xcac2c80a #x0c202c2c #x8aa2a82a #x04303434
                                  #xc2d2d012 #x0b03080b #xcee2ec2e #xc9e1e829
                                  #x4d515c1d #x84909414 #x08101818 #xc8f0f838
                                  #x47535417 #x8ea2ac2e #x08000808 #xc5c1c405
                                  #x03131013 #xcdc1cc0d #x86828406 #x89b1b839
                                  #xcff3fc3f #x4d717c3d #xc1c1c001 #x01313031
                                  #xc5f1f435 #x8a82880a #x4a62682a #x81b1b031
                                  #xc1d1d011 #x00202020 #xc7d3d417 #x02020002
                                  #x02222022 #x04000404 #x48606828 #x41717031
                                  #x07030407 #xcbd3d81b #x8d919c1d #x89919819
                                  #x41616021 #x8eb2bc3e #xc6e2e426 #x49515819
                                  #xcdd1dc1d #x41515011 #x80909010 #xccd0dc1c
                                  #x8a92981a #x83a3a023 #x8ba3a82b #xc0d0d010
                                  #x81818001 #x0f030c0f #x47434407 #x0a12181a
                                  #xc3e3e023 #xcce0ec2c #x8d818c0d #x8fb3bc3f
                                  #x86929416 #x4b73783b #x4c505c1c #x82a2a022
                                  #x81a1a021 #x43636023 #x03232023 #x4d414c0d
                                  #xc8c0c808 #x8e929c1e #x8c909c1c #x0a32383a
                                  #x0c000c0c #x0e222c2e #x8ab2b83a #x4e626c2e
                                  #x8f939c1f #x4a52581a #xc2f2f032 #x82929012
                                  #xc3f3f033 #x49414809 #x48707838 #xccc0cc0c
                                  #x05111415 #xcbf3f83b #x40707030 #x45717435
                                  #x4f737c3f #x05313435 #x00101010 #x03030003
                                  #x44606424 #x4d616c2d #xc6c2c406 #x44707434
                                  #xc5d1d415 #x84b0b434 #xcae2e82a #x09010809
                                  #x46727436 #x09111819 #xcef2fc3e #x40404000
                                  #x02121012 #xc0e0e020 #x8db1bc3d #x05010405
                                  #xcaf2f83a #x01010001 #xc0f0f030 #x0a22282a
                                  #x4e525c1e #x89a1a829 #x46525416 #x43434003
                                  #x85818405 #x04101414 #x89818809 #x8b93981b
                                  #x80b0b030 #xc5e1e425 #x48404808 #x49717839
                                  #x87939417 #xccf0fc3c #x0e121c1e #x82828002
                                  #x01212021 #x8c808c0c #x0b13181b #x4f535c1f
                                  #x47737437 #x44505414 #x82b2b032 #x0d111c1d
                                  #x05212425 #x4f434c0f #x00000000 #x46424406
                                  #xcde1ec2d #x48505818 #x42525012 #xcbe3e82b
                                  #x4e727c3e #xcad2d81a #xc9c1c809 #xcdf1fc3d
                                  #x00303030 #x85919415 #x45616425 #x0c303c3c
                                  #x86b2b436 #xc4e0e424 #x8bb3b83b #x4c707c3c
                                  #x0e020c0e #x40505010 #x09313839 #x06222426
                                  #x02323032 #x84808404 #x49616829 #x83939013
                                  #x07333437 #xc7e3e427 #x04202424 #x84a0a424
                                  #xcbc3c80b #x43535013 #x0a02080a #x87838407
                                  #xc9d1d819 #x4c404c0c #x83838003 #x8f838c0f
                                  #xcec2cc0e #x0b33383b #x4a42480a #x87b3b437)))

(defconst +seed-kc+
  (make-array 16
              :element-type '(unsigned-byte 32)
              :initial-contents '(#x9e3779b9 #x3c6ef373 #x78dde6e6 #xf1bbcdcc
                                  #xe3779b99 #xc6ef3733 #x8dde6e67 #x1bbcdccf
                                  #x3779b99e #x6ef3733c #xdde6e678 #xbbcdccf1
                                  #x779b99e3 #xef3733c6 #xde6e678d #xbcdccf1b)))

(defmacro seed-g (x)
  `(logxor (aref +seed-sbox0+ (logand ,x 255))
           (aref +seed-sbox1+ (logand (ash ,x -8) 255))
           (aref +seed-sbox2+ (logand (ash ,x -16) 255))
           (aref +seed-sbox3+ (logand (ash ,x -24) 255))))

(defmacro seed-f (r0 r1 k0 k1)
  `(let* ((a (logxor ,r0 ,k0))
          (b (seed-g (logxor a ,r1 ,k1)))
          (c (seed-g (mod32+ b a))))
     (declare (type (unsigned-byte 32) a b c))
     (setf ,r1 (seed-g (mod32+ c b))
           ,r0 (mod32+ ,r1 c))))

(defclass seed (cipher 16-byte-block-mixin)
  ((round-keys :accessor round-keys
               :type (simple-array (unsigned-byte 32) (32)))))

(defmethod schedule-key ((cipher seed) key)
  (declare (type (simple-array (unsigned-byte 8) (*)) key))
  (let ((round-keys (make-array 32 :element-type '(unsigned-byte 32)))
        (k0 (ub32ref/be key 0))
        (k1 (ub32ref/be key 4))
        (k2 (ub32ref/be key 8))
        (k3 (ub32ref/be key 12)))
    (dotimes (i 16)
      (setf (aref round-keys (* 2 i)) (seed-g (mod32- (mod32+ k0 k2) (aref +seed-kc+ i)))
            (aref round-keys (+ (* 2 i) 1)) (seed-g (mod32+ (mod32- k1 k3) (aref +seed-kc+ i))))
      (if (evenp i)
          (let ((n (mod32ash k0 24)))
            (setf k0 (logior (mod32ash k0 -8) (mod32ash k1 24))
                  k1 (logior (mod32ash k1 -8) n)))
          (let ((n (mod32ash k2 -24)))
            (setf k2 (logior (mod32ash k2 8) (mod32ash k3 -24))
                  k3 (logior (mod32ash k3 8) n)))))
    (setf (round-keys cipher) round-keys)
    cipher))

(define-block-encryptor seed 16
  (let ((round-keys (round-keys context))
        (t0 0)
        (t1 0))
    (declare (type (simple-array (unsigned-byte 32) (32)) round-keys)
             (type (unsigned-byte 32) t0 t1))
    (with-words ((l0 l1 r0 r1) plaintext plaintext-start :size 4)
      (dotimes-unrolled (i 15)
        (setf t0 r0
              t1 r1)
        (seed-f r0 r1 (aref round-keys (* 2 i)) (aref round-keys (+ (* 2 i) 1)))
        (setf r0 (logxor r0 l0)
              r1 (logxor r1 l1)
              l0 t0
              l1 t1))
      (setf t0 r0
            t1 r1)
      (seed-f t0 t1 (aref round-keys 30) (aref round-keys 31))
      (setf l0 (logxor l0 t0)
            l1 (logxor l1 t1))
      (store-words ciphertext ciphertext-start l0 l1 r0 r1))))

(define-block-decryptor seed 16
  (let ((round-keys (round-keys context))
        (t0 0)
        (t1 0))
    (declare (type (simple-array (unsigned-byte 32) (32)) round-keys)
             (type (unsigned-byte 32) t0 t1))
    (with-words ((l0 l1 r0 r1) ciphertext ciphertext-start :size 4)
      (dotimes-unrolled (i 15)
        (setf t0 r0
              t1 r1)
        (seed-f r0 r1 (aref round-keys (- 30 (* 2 i))) (aref round-keys (- 31 (* 2 i))))
        (setf r0 (logxor r0 l0)
              r1 (logxor r1 l1)
              l0 t0
              l1 t1))
      (setf t0 r0
            t1 r1)
      (seed-f t0 t1 (aref round-keys 0) (aref round-keys 1))
      (setf l0 (logxor l0 t0)
            l1 (logxor l1 t1))
      (store-words plaintext plaintext-start l0 l1 r0 r1))))

(defcipher seed
  (:encrypt-function seed-encrypt-block)
  (:decrypt-function seed-decrypt-block)
  (:block-length 16)
  (:key-length (:fixed 16)))
