;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; aes.lisp -- implementation of the Rijndael block cipher
;;;
;;; Currently limited to 128-bit block sizes, although the full range of
;;; key sizes is supported.

(in-package :crypto)
(in-ironclad-readtable)


;;; FIXME: is it work it to combine these into one large array and
;;; subscript into that rather than having separate arrays?  CMUCL
;;; and SBCL don't seem to want to keep the constant in a register,
;;; preferring to reload it at every reference, so a single large
;;; array scheme might not be the best for them (yet)
(declaim (type (simple-array (unsigned-byte 32) (256))
               Te0 Te1 Te2 Te3 Te4 Td0 Td1 Td2 Td3 Td4))
(defconst Te0
#32@(#xc66363a5 #xf87c7c84 #xee777799 #xf67b7b8d
    #xfff2f20d #xd66b6bbd #xde6f6fb1 #x91c5c554
    #x60303050 #x02010103 #xce6767a9 #x562b2b7d
    #xe7fefe19 #xb5d7d762 #x4dababe6 #xec76769a
    #x8fcaca45 #x1f82829d #x89c9c940 #xfa7d7d87
    #xeffafa15 #xb25959eb #x8e4747c9 #xfbf0f00b
    #x41adadec #xb3d4d467 #x5fa2a2fd #x45afafea
    #x239c9cbf #x53a4a4f7 #xe4727296 #x9bc0c05b
    #x75b7b7c2 #xe1fdfd1c #x3d9393ae #x4c26266a
    #x6c36365a #x7e3f3f41 #xf5f7f702 #x83cccc4f
    #x6834345c #x51a5a5f4 #xd1e5e534 #xf9f1f108
    #xe2717193 #xabd8d873 #x62313153 #x2a15153f
    #x0804040c #x95c7c752 #x46232365 #x9dc3c35e
    #x30181828 #x379696a1 #x0a05050f #x2f9a9ab5
    #x0e070709 #x24121236 #x1b80809b #xdfe2e23d
    #xcdebeb26 #x4e272769 #x7fb2b2cd #xea75759f
    #x1209091b #x1d83839e #x582c2c74 #x341a1a2e
    #x361b1b2d #xdc6e6eb2 #xb45a5aee #x5ba0a0fb
    #xa45252f6 #x763b3b4d #xb7d6d661 #x7db3b3ce
    #x5229297b #xdde3e33e #x5e2f2f71 #x13848497
    #xa65353f5 #xb9d1d168 #x00000000 #xc1eded2c
    #x40202060 #xe3fcfc1f #x79b1b1c8 #xb65b5bed
    #xd46a6abe #x8dcbcb46 #x67bebed9 #x7239394b
    #x944a4ade #x984c4cd4 #xb05858e8 #x85cfcf4a
    #xbbd0d06b #xc5efef2a #x4faaaae5 #xedfbfb16
    #x864343c5 #x9a4d4dd7 #x66333355 #x11858594
    #x8a4545cf #xe9f9f910 #x04020206 #xfe7f7f81
    #xa05050f0 #x783c3c44 #x259f9fba #x4ba8a8e3
    #xa25151f3 #x5da3a3fe #x804040c0 #x058f8f8a
    #x3f9292ad #x219d9dbc #x70383848 #xf1f5f504
    #x63bcbcdf #x77b6b6c1 #xafdada75 #x42212163
    #x20101030 #xe5ffff1a #xfdf3f30e #xbfd2d26d
    #x81cdcd4c #x180c0c14 #x26131335 #xc3ecec2f
    #xbe5f5fe1 #x359797a2 #x884444cc #x2e171739
    #x93c4c457 #x55a7a7f2 #xfc7e7e82 #x7a3d3d47
    #xc86464ac #xba5d5de7 #x3219192b #xe6737395
    #xc06060a0 #x19818198 #x9e4f4fd1 #xa3dcdc7f
    #x44222266 #x542a2a7e #x3b9090ab #x0b888883
    #x8c4646ca #xc7eeee29 #x6bb8b8d3 #x2814143c
    #xa7dede79 #xbc5e5ee2 #x160b0b1d #xaddbdb76
    #xdbe0e03b #x64323256 #x743a3a4e #x140a0a1e
    #x924949db #x0c06060a #x4824246c #xb85c5ce4
    #x9fc2c25d #xbdd3d36e #x43acacef #xc46262a6
    #x399191a8 #x319595a4 #xd3e4e437 #xf279798b
    #xd5e7e732 #x8bc8c843 #x6e373759 #xda6d6db7
    #x018d8d8c #xb1d5d564 #x9c4e4ed2 #x49a9a9e0
    #xd86c6cb4 #xac5656fa #xf3f4f407 #xcfeaea25
    #xca6565af #xf47a7a8e #x47aeaee9 #x10080818
    #x6fbabad5 #xf0787888 #x4a25256f #x5c2e2e72
    #x381c1c24 #x57a6a6f1 #x73b4b4c7 #x97c6c651
    #xcbe8e823 #xa1dddd7c #xe874749c #x3e1f1f21
    #x964b4bdd #x61bdbddc #x0d8b8b86 #x0f8a8a85
    #xe0707090 #x7c3e3e42 #x71b5b5c4 #xcc6666aa
    #x904848d8 #x06030305 #xf7f6f601 #x1c0e0e12
    #xc26161a3 #x6a35355f #xae5757f9 #x69b9b9d0
    #x17868691 #x99c1c158 #x3a1d1d27 #x279e9eb9
    #xd9e1e138 #xebf8f813 #x2b9898b3 #x22111133
    #xd26969bb #xa9d9d970 #x078e8e89 #x339494a7
    #x2d9b9bb6 #x3c1e1e22 #x15878792 #xc9e9e920
    #x87cece49 #xaa5555ff #x50282878 #xa5dfdf7a
    #x038c8c8f #x59a1a1f8 #x09898980 #x1a0d0d17
    #x65bfbfda #xd7e6e631 #x844242c6 #xd06868b8
    #x824141c3 #x299999b0 #x5a2d2d77 #x1e0f0f11
    #x7bb0b0cb #xa85454fc #x6dbbbbd6 #x2c16163a))

(defconst Te1
#32@(#xa5c66363 #x84f87c7c #x99ee7777 #x8df67b7b
    #x0dfff2f2 #xbdd66b6b #xb1de6f6f #x5491c5c5
    #x50603030 #x03020101 #xa9ce6767 #x7d562b2b
    #x19e7fefe #x62b5d7d7 #xe64dabab #x9aec7676
    #x458fcaca #x9d1f8282 #x4089c9c9 #x87fa7d7d
    #x15effafa #xebb25959 #xc98e4747 #x0bfbf0f0
    #xec41adad #x67b3d4d4 #xfd5fa2a2 #xea45afaf
    #xbf239c9c #xf753a4a4 #x96e47272 #x5b9bc0c0
    #xc275b7b7 #x1ce1fdfd #xae3d9393 #x6a4c2626
    #x5a6c3636 #x417e3f3f #x02f5f7f7 #x4f83cccc
    #x5c683434 #xf451a5a5 #x34d1e5e5 #x08f9f1f1
    #x93e27171 #x73abd8d8 #x53623131 #x3f2a1515
    #x0c080404 #x5295c7c7 #x65462323 #x5e9dc3c3
    #x28301818 #xa1379696 #x0f0a0505 #xb52f9a9a
    #x090e0707 #x36241212 #x9b1b8080 #x3ddfe2e2
    #x26cdebeb #x694e2727 #xcd7fb2b2 #x9fea7575
    #x1b120909 #x9e1d8383 #x74582c2c #x2e341a1a
    #x2d361b1b #xb2dc6e6e #xeeb45a5a #xfb5ba0a0
    #xf6a45252 #x4d763b3b #x61b7d6d6 #xce7db3b3
    #x7b522929 #x3edde3e3 #x715e2f2f #x97138484
    #xf5a65353 #x68b9d1d1 #x00000000 #x2cc1eded
    #x60402020 #x1fe3fcfc #xc879b1b1 #xedb65b5b
    #xbed46a6a #x468dcbcb #xd967bebe #x4b723939
    #xde944a4a #xd4984c4c #xe8b05858 #x4a85cfcf
    #x6bbbd0d0 #x2ac5efef #xe54faaaa #x16edfbfb
    #xc5864343 #xd79a4d4d #x55663333 #x94118585
    #xcf8a4545 #x10e9f9f9 #x06040202 #x81fe7f7f
    #xf0a05050 #x44783c3c #xba259f9f #xe34ba8a8
    #xf3a25151 #xfe5da3a3 #xc0804040 #x8a058f8f
    #xad3f9292 #xbc219d9d #x48703838 #x04f1f5f5
    #xdf63bcbc #xc177b6b6 #x75afdada #x63422121
    #x30201010 #x1ae5ffff #x0efdf3f3 #x6dbfd2d2
    #x4c81cdcd #x14180c0c #x35261313 #x2fc3ecec
    #xe1be5f5f #xa2359797 #xcc884444 #x392e1717
    #x5793c4c4 #xf255a7a7 #x82fc7e7e #x477a3d3d
    #xacc86464 #xe7ba5d5d #x2b321919 #x95e67373
    #xa0c06060 #x98198181 #xd19e4f4f #x7fa3dcdc
    #x66442222 #x7e542a2a #xab3b9090 #x830b8888
    #xca8c4646 #x29c7eeee #xd36bb8b8 #x3c281414
    #x79a7dede #xe2bc5e5e #x1d160b0b #x76addbdb
    #x3bdbe0e0 #x56643232 #x4e743a3a #x1e140a0a
    #xdb924949 #x0a0c0606 #x6c482424 #xe4b85c5c
    #x5d9fc2c2 #x6ebdd3d3 #xef43acac #xa6c46262
    #xa8399191 #xa4319595 #x37d3e4e4 #x8bf27979
    #x32d5e7e7 #x438bc8c8 #x596e3737 #xb7da6d6d
    #x8c018d8d #x64b1d5d5 #xd29c4e4e #xe049a9a9
    #xb4d86c6c #xfaac5656 #x07f3f4f4 #x25cfeaea
    #xafca6565 #x8ef47a7a #xe947aeae #x18100808
    #xd56fbaba #x88f07878 #x6f4a2525 #x725c2e2e
    #x24381c1c #xf157a6a6 #xc773b4b4 #x5197c6c6
    #x23cbe8e8 #x7ca1dddd #x9ce87474 #x213e1f1f
    #xdd964b4b #xdc61bdbd #x860d8b8b #x850f8a8a
    #x90e07070 #x427c3e3e #xc471b5b5 #xaacc6666
    #xd8904848 #x05060303 #x01f7f6f6 #x121c0e0e
    #xa3c26161 #x5f6a3535 #xf9ae5757 #xd069b9b9
    #x91178686 #x5899c1c1 #x273a1d1d #xb9279e9e
    #x38d9e1e1 #x13ebf8f8 #xb32b9898 #x33221111
    #xbbd26969 #x70a9d9d9 #x89078e8e #xa7339494
    #xb62d9b9b #x223c1e1e #x92158787 #x20c9e9e9
    #x4987cece #xffaa5555 #x78502828 #x7aa5dfdf
    #x8f038c8c #xf859a1a1 #x80098989 #x171a0d0d
    #xda65bfbf #x31d7e6e6 #xc6844242 #xb8d06868
    #xc3824141 #xb0299999 #x775a2d2d #x111e0f0f
    #xcb7bb0b0 #xfca85454 #xd66dbbbb #x3a2c1616))

(defconst Te2
#32@(#x63a5c663 #x7c84f87c #x7799ee77 #x7b8df67b
    #xf20dfff2 #x6bbdd66b #x6fb1de6f #xc55491c5
    #x30506030 #x01030201 #x67a9ce67 #x2b7d562b
    #xfe19e7fe #xd762b5d7 #xabe64dab #x769aec76
    #xca458fca #x829d1f82 #xc94089c9 #x7d87fa7d
    #xfa15effa #x59ebb259 #x47c98e47 #xf00bfbf0
    #xadec41ad #xd467b3d4 #xa2fd5fa2 #xafea45af
    #x9cbf239c #xa4f753a4 #x7296e472 #xc05b9bc0
    #xb7c275b7 #xfd1ce1fd #x93ae3d93 #x266a4c26
    #x365a6c36 #x3f417e3f #xf702f5f7 #xcc4f83cc
    #x345c6834 #xa5f451a5 #xe534d1e5 #xf108f9f1
    #x7193e271 #xd873abd8 #x31536231 #x153f2a15
    #x040c0804 #xc75295c7 #x23654623 #xc35e9dc3
    #x18283018 #x96a13796 #x050f0a05 #x9ab52f9a
    #x07090e07 #x12362412 #x809b1b80 #xe23ddfe2
    #xeb26cdeb #x27694e27 #xb2cd7fb2 #x759fea75
    #x091b1209 #x839e1d83 #x2c74582c #x1a2e341a
    #x1b2d361b #x6eb2dc6e #x5aeeb45a #xa0fb5ba0
    #x52f6a452 #x3b4d763b #xd661b7d6 #xb3ce7db3
    #x297b5229 #xe33edde3 #x2f715e2f #x84971384
    #x53f5a653 #xd168b9d1 #x00000000 #xed2cc1ed
    #x20604020 #xfc1fe3fc #xb1c879b1 #x5bedb65b
    #x6abed46a #xcb468dcb #xbed967be #x394b7239
    #x4ade944a #x4cd4984c #x58e8b058 #xcf4a85cf
    #xd06bbbd0 #xef2ac5ef #xaae54faa #xfb16edfb
    #x43c58643 #x4dd79a4d #x33556633 #x85941185
    #x45cf8a45 #xf910e9f9 #x02060402 #x7f81fe7f
    #x50f0a050 #x3c44783c #x9fba259f #xa8e34ba8
    #x51f3a251 #xa3fe5da3 #x40c08040 #x8f8a058f
    #x92ad3f92 #x9dbc219d #x38487038 #xf504f1f5
    #xbcdf63bc #xb6c177b6 #xda75afda #x21634221
    #x10302010 #xff1ae5ff #xf30efdf3 #xd26dbfd2
    #xcd4c81cd #x0c14180c #x13352613 #xec2fc3ec
    #x5fe1be5f #x97a23597 #x44cc8844 #x17392e17
    #xc45793c4 #xa7f255a7 #x7e82fc7e #x3d477a3d
    #x64acc864 #x5de7ba5d #x192b3219 #x7395e673
    #x60a0c060 #x81981981 #x4fd19e4f #xdc7fa3dc
    #x22664422 #x2a7e542a #x90ab3b90 #x88830b88
    #x46ca8c46 #xee29c7ee #xb8d36bb8 #x143c2814
    #xde79a7de #x5ee2bc5e #x0b1d160b #xdb76addb
    #xe03bdbe0 #x32566432 #x3a4e743a #x0a1e140a
    #x49db9249 #x060a0c06 #x246c4824 #x5ce4b85c
    #xc25d9fc2 #xd36ebdd3 #xacef43ac #x62a6c462
    #x91a83991 #x95a43195 #xe437d3e4 #x798bf279
    #xe732d5e7 #xc8438bc8 #x37596e37 #x6db7da6d
    #x8d8c018d #xd564b1d5 #x4ed29c4e #xa9e049a9
    #x6cb4d86c #x56faac56 #xf407f3f4 #xea25cfea
    #x65afca65 #x7a8ef47a #xaee947ae #x08181008
    #xbad56fba #x7888f078 #x256f4a25 #x2e725c2e
    #x1c24381c #xa6f157a6 #xb4c773b4 #xc65197c6
    #xe823cbe8 #xdd7ca1dd #x749ce874 #x1f213e1f
    #x4bdd964b #xbddc61bd #x8b860d8b #x8a850f8a
    #x7090e070 #x3e427c3e #xb5c471b5 #x66aacc66
    #x48d89048 #x03050603 #xf601f7f6 #x0e121c0e
    #x61a3c261 #x355f6a35 #x57f9ae57 #xb9d069b9
    #x86911786 #xc15899c1 #x1d273a1d #x9eb9279e
    #xe138d9e1 #xf813ebf8 #x98b32b98 #x11332211
    #x69bbd269 #xd970a9d9 #x8e89078e #x94a73394
    #x9bb62d9b #x1e223c1e #x87921587 #xe920c9e9
    #xce4987ce #x55ffaa55 #x28785028 #xdf7aa5df
    #x8c8f038c #xa1f859a1 #x89800989 #x0d171a0d
    #xbfda65bf #xe631d7e6 #x42c68442 #x68b8d068
    #x41c38241 #x99b02999 #x2d775a2d #x0f111e0f
    #xb0cb7bb0 #x54fca854 #xbbd66dbb #x163a2c16))

(defconst Te3
#32@(#x6363a5c6 #x7c7c84f8 #x777799ee #x7b7b8df6
    #xf2f20dff #x6b6bbdd6 #x6f6fb1de #xc5c55491
    #x30305060 #x01010302 #x6767a9ce #x2b2b7d56
    #xfefe19e7 #xd7d762b5 #xababe64d #x76769aec
    #xcaca458f #x82829d1f #xc9c94089 #x7d7d87fa
    #xfafa15ef #x5959ebb2 #x4747c98e #xf0f00bfb
    #xadadec41 #xd4d467b3 #xa2a2fd5f #xafafea45
    #x9c9cbf23 #xa4a4f753 #x727296e4 #xc0c05b9b
    #xb7b7c275 #xfdfd1ce1 #x9393ae3d #x26266a4c
    #x36365a6c #x3f3f417e #xf7f702f5 #xcccc4f83
    #x34345c68 #xa5a5f451 #xe5e534d1 #xf1f108f9
    #x717193e2 #xd8d873ab #x31315362 #x15153f2a
    #x04040c08 #xc7c75295 #x23236546 #xc3c35e9d
    #x18182830 #x9696a137 #x05050f0a #x9a9ab52f
    #x0707090e #x12123624 #x80809b1b #xe2e23ddf
    #xebeb26cd #x2727694e #xb2b2cd7f #x75759fea
    #x09091b12 #x83839e1d #x2c2c7458 #x1a1a2e34
    #x1b1b2d36 #x6e6eb2dc #x5a5aeeb4 #xa0a0fb5b
    #x5252f6a4 #x3b3b4d76 #xd6d661b7 #xb3b3ce7d
    #x29297b52 #xe3e33edd #x2f2f715e #x84849713
    #x5353f5a6 #xd1d168b9 #x00000000 #xeded2cc1
    #x20206040 #xfcfc1fe3 #xb1b1c879 #x5b5bedb6
    #x6a6abed4 #xcbcb468d #xbebed967 #x39394b72
    #x4a4ade94 #x4c4cd498 #x5858e8b0 #xcfcf4a85
    #xd0d06bbb #xefef2ac5 #xaaaae54f #xfbfb16ed
    #x4343c586 #x4d4dd79a #x33335566 #x85859411
    #x4545cf8a #xf9f910e9 #x02020604 #x7f7f81fe
    #x5050f0a0 #x3c3c4478 #x9f9fba25 #xa8a8e34b
    #x5151f3a2 #xa3a3fe5d #x4040c080 #x8f8f8a05
    #x9292ad3f #x9d9dbc21 #x38384870 #xf5f504f1
    #xbcbcdf63 #xb6b6c177 #xdada75af #x21216342
    #x10103020 #xffff1ae5 #xf3f30efd #xd2d26dbf
    #xcdcd4c81 #x0c0c1418 #x13133526 #xecec2fc3
    #x5f5fe1be #x9797a235 #x4444cc88 #x1717392e
    #xc4c45793 #xa7a7f255 #x7e7e82fc #x3d3d477a
    #x6464acc8 #x5d5de7ba #x19192b32 #x737395e6
    #x6060a0c0 #x81819819 #x4f4fd19e #xdcdc7fa3
    #x22226644 #x2a2a7e54 #x9090ab3b #x8888830b
    #x4646ca8c #xeeee29c7 #xb8b8d36b #x14143c28
    #xdede79a7 #x5e5ee2bc #x0b0b1d16 #xdbdb76ad
    #xe0e03bdb #x32325664 #x3a3a4e74 #x0a0a1e14
    #x4949db92 #x06060a0c #x24246c48 #x5c5ce4b8
    #xc2c25d9f #xd3d36ebd #xacacef43 #x6262a6c4
    #x9191a839 #x9595a431 #xe4e437d3 #x79798bf2
    #xe7e732d5 #xc8c8438b #x3737596e #x6d6db7da
    #x8d8d8c01 #xd5d564b1 #x4e4ed29c #xa9a9e049
    #x6c6cb4d8 #x5656faac #xf4f407f3 #xeaea25cf
    #x6565afca #x7a7a8ef4 #xaeaee947 #x08081810
    #xbabad56f #x787888f0 #x25256f4a #x2e2e725c
    #x1c1c2438 #xa6a6f157 #xb4b4c773 #xc6c65197
    #xe8e823cb #xdddd7ca1 #x74749ce8 #x1f1f213e
    #x4b4bdd96 #xbdbddc61 #x8b8b860d #x8a8a850f
    #x707090e0 #x3e3e427c #xb5b5c471 #x6666aacc
    #x4848d890 #x03030506 #xf6f601f7 #x0e0e121c
    #x6161a3c2 #x35355f6a #x5757f9ae #xb9b9d069
    #x86869117 #xc1c15899 #x1d1d273a #x9e9eb927
    #xe1e138d9 #xf8f813eb #x9898b32b #x11113322
    #x6969bbd2 #xd9d970a9 #x8e8e8907 #x9494a733
    #x9b9bb62d #x1e1e223c #x87879215 #xe9e920c9
    #xcece4987 #x5555ffaa #x28287850 #xdfdf7aa5
    #x8c8c8f03 #xa1a1f859 #x89898009 #x0d0d171a
    #xbfbfda65 #xe6e631d7 #x4242c684 #x6868b8d0
    #x4141c382 #x9999b029 #x2d2d775a #x0f0f111e
    #xb0b0cb7b #x5454fca8 #xbbbbd66d #x16163a2c))

(defconst Te4
#32@(#x63636363 #x7c7c7c7c #x77777777 #x7b7b7b7b
    #xf2f2f2f2 #x6b6b6b6b #x6f6f6f6f #xc5c5c5c5
    #x30303030 #x01010101 #x67676767 #x2b2b2b2b
    #xfefefefe #xd7d7d7d7 #xabababab #x76767676
    #xcacacaca #x82828282 #xc9c9c9c9 #x7d7d7d7d
    #xfafafafa #x59595959 #x47474747 #xf0f0f0f0
    #xadadadad #xd4d4d4d4 #xa2a2a2a2 #xafafafaf
    #x9c9c9c9c #xa4a4a4a4 #x72727272 #xc0c0c0c0
    #xb7b7b7b7 #xfdfdfdfd #x93939393 #x26262626
    #x36363636 #x3f3f3f3f #xf7f7f7f7 #xcccccccc
    #x34343434 #xa5a5a5a5 #xe5e5e5e5 #xf1f1f1f1
    #x71717171 #xd8d8d8d8 #x31313131 #x15151515
    #x04040404 #xc7c7c7c7 #x23232323 #xc3c3c3c3
    #x18181818 #x96969696 #x05050505 #x9a9a9a9a
    #x07070707 #x12121212 #x80808080 #xe2e2e2e2
    #xebebebeb #x27272727 #xb2b2b2b2 #x75757575
    #x09090909 #x83838383 #x2c2c2c2c #x1a1a1a1a
    #x1b1b1b1b #x6e6e6e6e #x5a5a5a5a #xa0a0a0a0
    #x52525252 #x3b3b3b3b #xd6d6d6d6 #xb3b3b3b3
    #x29292929 #xe3e3e3e3 #x2f2f2f2f #x84848484
    #x53535353 #xd1d1d1d1 #x00000000 #xedededed
    #x20202020 #xfcfcfcfc #xb1b1b1b1 #x5b5b5b5b
    #x6a6a6a6a #xcbcbcbcb #xbebebebe #x39393939
    #x4a4a4a4a #x4c4c4c4c #x58585858 #xcfcfcfcf
    #xd0d0d0d0 #xefefefef #xaaaaaaaa #xfbfbfbfb
    #x43434343 #x4d4d4d4d #x33333333 #x85858585
    #x45454545 #xf9f9f9f9 #x02020202 #x7f7f7f7f
    #x50505050 #x3c3c3c3c #x9f9f9f9f #xa8a8a8a8
    #x51515151 #xa3a3a3a3 #x40404040 #x8f8f8f8f
    #x92929292 #x9d9d9d9d #x38383838 #xf5f5f5f5
    #xbcbcbcbc #xb6b6b6b6 #xdadadada #x21212121
    #x10101010 #xffffffff #xf3f3f3f3 #xd2d2d2d2
    #xcdcdcdcd #x0c0c0c0c #x13131313 #xecececec
    #x5f5f5f5f #x97979797 #x44444444 #x17171717
    #xc4c4c4c4 #xa7a7a7a7 #x7e7e7e7e #x3d3d3d3d
    #x64646464 #x5d5d5d5d #x19191919 #x73737373
    #x60606060 #x81818181 #x4f4f4f4f #xdcdcdcdc
    #x22222222 #x2a2a2a2a #x90909090 #x88888888
    #x46464646 #xeeeeeeee #xb8b8b8b8 #x14141414
    #xdededede #x5e5e5e5e #x0b0b0b0b #xdbdbdbdb
    #xe0e0e0e0 #x32323232 #x3a3a3a3a #x0a0a0a0a
    #x49494949 #x06060606 #x24242424 #x5c5c5c5c
    #xc2c2c2c2 #xd3d3d3d3 #xacacacac #x62626262
    #x91919191 #x95959595 #xe4e4e4e4 #x79797979
    #xe7e7e7e7 #xc8c8c8c8 #x37373737 #x6d6d6d6d
    #x8d8d8d8d #xd5d5d5d5 #x4e4e4e4e #xa9a9a9a9
    #x6c6c6c6c #x56565656 #xf4f4f4f4 #xeaeaeaea
    #x65656565 #x7a7a7a7a #xaeaeaeae #x08080808
    #xbabababa #x78787878 #x25252525 #x2e2e2e2e
    #x1c1c1c1c #xa6a6a6a6 #xb4b4b4b4 #xc6c6c6c6
    #xe8e8e8e8 #xdddddddd #x74747474 #x1f1f1f1f
    #x4b4b4b4b #xbdbdbdbd #x8b8b8b8b #x8a8a8a8a
    #x70707070 #x3e3e3e3e #xb5b5b5b5 #x66666666
    #x48484848 #x03030303 #xf6f6f6f6 #x0e0e0e0e
    #x61616161 #x35353535 #x57575757 #xb9b9b9b9
    #x86868686 #xc1c1c1c1 #x1d1d1d1d #x9e9e9e9e
    #xe1e1e1e1 #xf8f8f8f8 #x98989898 #x11111111
    #x69696969 #xd9d9d9d9 #x8e8e8e8e #x94949494
    #x9b9b9b9b #x1e1e1e1e #x87878787 #xe9e9e9e9
    #xcececece #x55555555 #x28282828 #xdfdfdfdf
    #x8c8c8c8c #xa1a1a1a1 #x89898989 #x0d0d0d0d
    #xbfbfbfbf #xe6e6e6e6 #x42424242 #x68686868
    #x41414141 #x99999999 #x2d2d2d2d #x0f0f0f0f
    #xb0b0b0b0 #x54545454 #xbbbbbbbb #x16161616))

(defconst Td0
#32@(#x51f4a750 #x7e416553 #x1a17a4c3 #x3a275e96
    #x3bab6bcb #x1f9d45f1 #xacfa58ab #x4be30393
    #x2030fa55 #xad766df6 #x88cc7691 #xf5024c25
    #x4fe5d7fc #xc52acbd7 #x26354480 #xb562a38f
    #xdeb15a49 #x25ba1b67 #x45ea0e98 #x5dfec0e1
    #xc32f7502 #x814cf012 #x8d4697a3 #x6bd3f9c6
    #x038f5fe7 #x15929c95 #xbf6d7aeb #x955259da
    #xd4be832d #x587421d3 #x49e06929 #x8ec9c844
    #x75c2896a #xf48e7978 #x99583e6b #x27b971dd
    #xbee14fb6 #xf088ad17 #xc920ac66 #x7dce3ab4
    #x63df4a18 #xe51a3182 #x97513360 #x62537f45
    #xb16477e0 #xbb6bae84 #xfe81a01c #xf9082b94
    #x70486858 #x8f45fd19 #x94de6c87 #x527bf8b7
    #xab73d323 #x724b02e2 #xe31f8f57 #x6655ab2a
    #xb2eb2807 #x2fb5c203 #x86c57b9a #xd33708a5
    #x302887f2 #x23bfa5b2 #x02036aba #xed16825c
    #x8acf1c2b #xa779b492 #xf307f2f0 #x4e69e2a1
    #x65daf4cd #x0605bed5 #xd134621f #xc4a6fe8a
    #x342e539d #xa2f355a0 #x058ae132 #xa4f6eb75
    #x0b83ec39 #x4060efaa #x5e719f06 #xbd6e1051
    #x3e218af9 #x96dd063d #xdd3e05ae #x4de6bd46
    #x91548db5 #x71c45d05 #x0406d46f #x605015ff
    #x1998fb24 #xd6bde997 #x894043cc #x67d99e77
    #xb0e842bd #x07898b88 #xe7195b38 #x79c8eedb
    #xa17c0a47 #x7c420fe9 #xf8841ec9 #x00000000
    #x09808683 #x322bed48 #x1e1170ac #x6c5a724e
    #xfd0efffb #x0f853856 #x3daed51e #x362d3927
    #x0a0fd964 #x685ca621 #x9b5b54d1 #x24362e3a
    #x0c0a67b1 #x9357e70f #xb4ee96d2 #x1b9b919e
    #x80c0c54f #x61dc20a2 #x5a774b69 #x1c121a16
    #xe293ba0a #xc0a02ae5 #x3c22e043 #x121b171d
    #x0e090d0b #xf28bc7ad #x2db6a8b9 #x141ea9c8
    #x57f11985 #xaf75074c #xee99ddbb #xa37f60fd
    #xf701269f #x5c72f5bc #x44663bc5 #x5bfb7e34
    #x8b432976 #xcb23c6dc #xb6edfc68 #xb8e4f163
    #xd731dcca #x42638510 #x13972240 #x84c61120
    #x854a247d #xd2bb3df8 #xaef93211 #xc729a16d
    #x1d9e2f4b #xdcb230f3 #x0d8652ec #x77c1e3d0
    #x2bb3166c #xa970b999 #x119448fa #x47e96422
    #xa8fc8cc4 #xa0f03f1a #x567d2cd8 #x223390ef
    #x87494ec7 #xd938d1c1 #x8ccaa2fe #x98d40b36
    #xa6f581cf #xa57ade28 #xdab78e26 #x3fadbfa4
    #x2c3a9de4 #x5078920d #x6a5fcc9b #x547e4662
    #xf68d13c2 #x90d8b8e8 #x2e39f75e #x82c3aff5
    #x9f5d80be #x69d0937c #x6fd52da9 #xcf2512b3
    #xc8ac993b #x10187da7 #xe89c636e #xdb3bbb7b
    #xcd267809 #x6e5918f4 #xec9ab701 #x834f9aa8
    #xe6956e65 #xaaffe67e #x21bccf08 #xef15e8e6
    #xbae79bd9 #x4a6f36ce #xea9f09d4 #x29b07cd6
    #x31a4b2af #x2a3f2331 #xc6a59430 #x35a266c0
    #x744ebc37 #xfc82caa6 #xe090d0b0 #x33a7d815
    #xf104984a #x41ecdaf7 #x7fcd500e #x1791f62f
    #x764dd68d #x43efb04d #xccaa4d54 #xe49604df
    #x9ed1b5e3 #x4c6a881b #xc12c1fb8 #x4665517f
    #x9d5eea04 #x018c355d #xfa877473 #xfb0b412e
    #xb3671d5a #x92dbd252 #xe9105633 #x6dd64713
    #x9ad7618c #x37a10c7a #x59f8148e #xeb133c89
    #xcea927ee #xb761c935 #xe11ce5ed #x7a47b13c
    #x9cd2df59 #x55f2733f #x1814ce79 #x73c737bf
    #x53f7cdea #x5ffdaa5b #xdf3d6f14 #x7844db86
    #xcaaff381 #xb968c43e #x3824342c #xc2a3405f
    #x161dc372 #xbce2250c #x283c498b #xff0d9541
    #x39a80171 #x080cb3de #xd8b4e49c #x6456c190
    #x7bcb8461 #xd532b670 #x486c5c74 #xd0b85742))

(defconst Td1
#32@(#x5051f4a7 #x537e4165 #xc31a17a4 #x963a275e
    #xcb3bab6b #xf11f9d45 #xabacfa58 #x934be303
    #x552030fa #xf6ad766d #x9188cc76 #x25f5024c
    #xfc4fe5d7 #xd7c52acb #x80263544 #x8fb562a3
    #x49deb15a #x6725ba1b #x9845ea0e #xe15dfec0
    #x02c32f75 #x12814cf0 #xa38d4697 #xc66bd3f9
    #xe7038f5f #x9515929c #xebbf6d7a #xda955259
    #x2dd4be83 #xd3587421 #x2949e069 #x448ec9c8
    #x6a75c289 #x78f48e79 #x6b99583e #xdd27b971
    #xb6bee14f #x17f088ad #x66c920ac #xb47dce3a
    #x1863df4a #x82e51a31 #x60975133 #x4562537f
    #xe0b16477 #x84bb6bae #x1cfe81a0 #x94f9082b
    #x58704868 #x198f45fd #x8794de6c #xb7527bf8
    #x23ab73d3 #xe2724b02 #x57e31f8f #x2a6655ab
    #x07b2eb28 #x032fb5c2 #x9a86c57b #xa5d33708
    #xf2302887 #xb223bfa5 #xba02036a #x5ced1682
    #x2b8acf1c #x92a779b4 #xf0f307f2 #xa14e69e2
    #xcd65daf4 #xd50605be #x1fd13462 #x8ac4a6fe
    #x9d342e53 #xa0a2f355 #x32058ae1 #x75a4f6eb
    #x390b83ec #xaa4060ef #x065e719f #x51bd6e10
    #xf93e218a #x3d96dd06 #xaedd3e05 #x464de6bd
    #xb591548d #x0571c45d #x6f0406d4 #xff605015
    #x241998fb #x97d6bde9 #xcc894043 #x7767d99e
    #xbdb0e842 #x8807898b #x38e7195b #xdb79c8ee
    #x47a17c0a #xe97c420f #xc9f8841e #x00000000
    #x83098086 #x48322bed #xac1e1170 #x4e6c5a72
    #xfbfd0eff #x560f8538 #x1e3daed5 #x27362d39
    #x640a0fd9 #x21685ca6 #xd19b5b54 #x3a24362e
    #xb10c0a67 #x0f9357e7 #xd2b4ee96 #x9e1b9b91
    #x4f80c0c5 #xa261dc20 #x695a774b #x161c121a
    #x0ae293ba #xe5c0a02a #x433c22e0 #x1d121b17
    #x0b0e090d #xadf28bc7 #xb92db6a8 #xc8141ea9
    #x8557f119 #x4caf7507 #xbbee99dd #xfda37f60
    #x9ff70126 #xbc5c72f5 #xc544663b #x345bfb7e
    #x768b4329 #xdccb23c6 #x68b6edfc #x63b8e4f1
    #xcad731dc #x10426385 #x40139722 #x2084c611
    #x7d854a24 #xf8d2bb3d #x11aef932 #x6dc729a1
    #x4b1d9e2f #xf3dcb230 #xec0d8652 #xd077c1e3
    #x6c2bb316 #x99a970b9 #xfa119448 #x2247e964
    #xc4a8fc8c #x1aa0f03f #xd8567d2c #xef223390
    #xc787494e #xc1d938d1 #xfe8ccaa2 #x3698d40b
    #xcfa6f581 #x28a57ade #x26dab78e #xa43fadbf
    #xe42c3a9d #x0d507892 #x9b6a5fcc #x62547e46
    #xc2f68d13 #xe890d8b8 #x5e2e39f7 #xf582c3af
    #xbe9f5d80 #x7c69d093 #xa96fd52d #xb3cf2512
    #x3bc8ac99 #xa710187d #x6ee89c63 #x7bdb3bbb
    #x09cd2678 #xf46e5918 #x01ec9ab7 #xa8834f9a
    #x65e6956e #x7eaaffe6 #x0821bccf #xe6ef15e8
    #xd9bae79b #xce4a6f36 #xd4ea9f09 #xd629b07c
    #xaf31a4b2 #x312a3f23 #x30c6a594 #xc035a266
    #x37744ebc #xa6fc82ca #xb0e090d0 #x1533a7d8
    #x4af10498 #xf741ecda #x0e7fcd50 #x2f1791f6
    #x8d764dd6 #x4d43efb0 #x54ccaa4d #xdfe49604
    #xe39ed1b5 #x1b4c6a88 #xb8c12c1f #x7f466551
    #x049d5eea #x5d018c35 #x73fa8774 #x2efb0b41
    #x5ab3671d #x5292dbd2 #x33e91056 #x136dd647
    #x8c9ad761 #x7a37a10c #x8e59f814 #x89eb133c
    #xeecea927 #x35b761c9 #xede11ce5 #x3c7a47b1
    #x599cd2df #x3f55f273 #x791814ce #xbf73c737
    #xea53f7cd #x5b5ffdaa #x14df3d6f #x867844db
    #x81caaff3 #x3eb968c4 #x2c382434 #x5fc2a340
    #x72161dc3 #x0cbce225 #x8b283c49 #x41ff0d95
    #x7139a801 #xde080cb3 #x9cd8b4e4 #x906456c1
    #x617bcb84 #x70d532b6 #x74486c5c #x42d0b857))

(defconst Td2
#32@(#xa75051f4 #x65537e41 #xa4c31a17 #x5e963a27
    #x6bcb3bab #x45f11f9d #x58abacfa #x03934be3
    #xfa552030 #x6df6ad76 #x769188cc #x4c25f502
    #xd7fc4fe5 #xcbd7c52a #x44802635 #xa38fb562
    #x5a49deb1 #x1b6725ba #x0e9845ea #xc0e15dfe
    #x7502c32f #xf012814c #x97a38d46 #xf9c66bd3
    #x5fe7038f #x9c951592 #x7aebbf6d #x59da9552
    #x832dd4be #x21d35874 #x692949e0 #xc8448ec9
    #x896a75c2 #x7978f48e #x3e6b9958 #x71dd27b9
    #x4fb6bee1 #xad17f088 #xac66c920 #x3ab47dce
    #x4a1863df #x3182e51a #x33609751 #x7f456253
    #x77e0b164 #xae84bb6b #xa01cfe81 #x2b94f908
    #x68587048 #xfd198f45 #x6c8794de #xf8b7527b
    #xd323ab73 #x02e2724b #x8f57e31f #xab2a6655
    #x2807b2eb #xc2032fb5 #x7b9a86c5 #x08a5d337
    #x87f23028 #xa5b223bf #x6aba0203 #x825ced16
    #x1c2b8acf #xb492a779 #xf2f0f307 #xe2a14e69
    #xf4cd65da #xbed50605 #x621fd134 #xfe8ac4a6
    #x539d342e #x55a0a2f3 #xe132058a #xeb75a4f6
    #xec390b83 #xefaa4060 #x9f065e71 #x1051bd6e

    #x8af93e21 #x063d96dd #x05aedd3e #xbd464de6
    #x8db59154 #x5d0571c4 #xd46f0406 #x15ff6050
    #xfb241998 #xe997d6bd #x43cc8940 #x9e7767d9
    #x42bdb0e8 #x8b880789 #x5b38e719 #xeedb79c8
    #x0a47a17c #x0fe97c42 #x1ec9f884 #x00000000
    #x86830980 #xed48322b #x70ac1e11 #x724e6c5a
    #xfffbfd0e #x38560f85 #xd51e3dae #x3927362d
    #xd9640a0f #xa621685c #x54d19b5b #x2e3a2436
    #x67b10c0a #xe70f9357 #x96d2b4ee #x919e1b9b
    #xc54f80c0 #x20a261dc #x4b695a77 #x1a161c12
    #xba0ae293 #x2ae5c0a0 #xe0433c22 #x171d121b
    #x0d0b0e09 #xc7adf28b #xa8b92db6 #xa9c8141e
    #x198557f1 #x074caf75 #xddbbee99 #x60fda37f
    #x269ff701 #xf5bc5c72 #x3bc54466 #x7e345bfb
    #x29768b43 #xc6dccb23 #xfc68b6ed #xf163b8e4
    #xdccad731 #x85104263 #x22401397 #x112084c6
    #x247d854a #x3df8d2bb #x3211aef9 #xa16dc729
    #x2f4b1d9e #x30f3dcb2 #x52ec0d86 #xe3d077c1
    #x166c2bb3 #xb999a970 #x48fa1194 #x642247e9
    #x8cc4a8fc #x3f1aa0f0 #x2cd8567d #x90ef2233
    #x4ec78749 #xd1c1d938 #xa2fe8cca #x0b3698d4
    #x81cfa6f5 #xde28a57a #x8e26dab7 #xbfa43fad
    #x9de42c3a #x920d5078 #xcc9b6a5f #x4662547e
    #x13c2f68d #xb8e890d8 #xf75e2e39 #xaff582c3
    #x80be9f5d #x937c69d0 #x2da96fd5 #x12b3cf25
    #x993bc8ac #x7da71018 #x636ee89c #xbb7bdb3b
    #x7809cd26 #x18f46e59 #xb701ec9a #x9aa8834f
    #x6e65e695 #xe67eaaff #xcf0821bc #xe8e6ef15
    #x9bd9bae7 #x36ce4a6f #x09d4ea9f #x7cd629b0
    #xb2af31a4 #x23312a3f #x9430c6a5 #x66c035a2
    #xbc37744e #xcaa6fc82 #xd0b0e090 #xd81533a7
    #x984af104 #xdaf741ec #x500e7fcd #xf62f1791
    #xd68d764d #xb04d43ef #x4d54ccaa #x04dfe496
    #xb5e39ed1 #x881b4c6a #x1fb8c12c #x517f4665
    #xea049d5e #x355d018c #x7473fa87 #x412efb0b
    #x1d5ab367 #xd25292db #x5633e910 #x47136dd6
    #x618c9ad7 #x0c7a37a1 #x148e59f8 #x3c89eb13
    #x27eecea9 #xc935b761 #xe5ede11c #xb13c7a47
    #xdf599cd2 #x733f55f2 #xce791814 #x37bf73c7
    #xcdea53f7 #xaa5b5ffd #x6f14df3d #xdb867844
    #xf381caaf #xc43eb968 #x342c3824 #x405fc2a3
    #xc372161d #x250cbce2 #x498b283c #x9541ff0d
    #x017139a8 #xb3de080c #xe49cd8b4 #xc1906456
    #x84617bcb #xb670d532 #x5c74486c #x5742d0b8))

(defconst Td3
#32@(#xf4a75051 #x4165537e #x17a4c31a #x275e963a
    #xab6bcb3b #x9d45f11f #xfa58abac #xe303934b
    #x30fa5520 #x766df6ad #xcc769188 #x024c25f5
    #xe5d7fc4f #x2acbd7c5 #x35448026 #x62a38fb5
    #xb15a49de #xba1b6725 #xea0e9845 #xfec0e15d
    #x2f7502c3 #x4cf01281 #x4697a38d #xd3f9c66b
    #x8f5fe703 #x929c9515 #x6d7aebbf #x5259da95
    #xbe832dd4 #x7421d358 #xe0692949 #xc9c8448e
    #xc2896a75 #x8e7978f4 #x583e6b99 #xb971dd27
    #xe14fb6be #x88ad17f0 #x20ac66c9 #xce3ab47d
    #xdf4a1863 #x1a3182e5 #x51336097 #x537f4562
    #x6477e0b1 #x6bae84bb #x81a01cfe #x082b94f9
    #x48685870 #x45fd198f #xde6c8794 #x7bf8b752
    #x73d323ab #x4b02e272 #x1f8f57e3 #x55ab2a66
    #xeb2807b2 #xb5c2032f #xc57b9a86 #x3708a5d3
    #x2887f230 #xbfa5b223 #x036aba02 #x16825ced
    #xcf1c2b8a #x79b492a7 #x07f2f0f3 #x69e2a14e
    #xdaf4cd65 #x05bed506 #x34621fd1 #xa6fe8ac4
    #x2e539d34 #xf355a0a2 #x8ae13205 #xf6eb75a4
    #x83ec390b #x60efaa40 #x719f065e #x6e1051bd
    #x218af93e #xdd063d96 #x3e05aedd #xe6bd464d
    #x548db591 #xc45d0571 #x06d46f04 #x5015ff60
    #x98fb2419 #xbde997d6 #x4043cc89 #xd99e7767
    #xe842bdb0 #x898b8807 #x195b38e7 #xc8eedb79
    #x7c0a47a1 #x420fe97c #x841ec9f8 #x00000000
    #x80868309 #x2bed4832 #x1170ac1e #x5a724e6c
    #x0efffbfd #x8538560f #xaed51e3d #x2d392736
    #x0fd9640a #x5ca62168 #x5b54d19b #x362e3a24
    #x0a67b10c #x57e70f93 #xee96d2b4 #x9b919e1b
    #xc0c54f80 #xdc20a261 #x774b695a #x121a161c
    #x93ba0ae2 #xa02ae5c0 #x22e0433c #x1b171d12
    #x090d0b0e #x8bc7adf2 #xb6a8b92d #x1ea9c814
    #xf1198557 #x75074caf #x99ddbbee #x7f60fda3
    #x01269ff7 #x72f5bc5c #x663bc544 #xfb7e345b
    #x4329768b #x23c6dccb #xedfc68b6 #xe4f163b8
    #x31dccad7 #x63851042 #x97224013 #xc6112084
    #x4a247d85 #xbb3df8d2 #xf93211ae #x29a16dc7
    #x9e2f4b1d #xb230f3dc #x8652ec0d #xc1e3d077
    #xb3166c2b #x70b999a9 #x9448fa11 #xe9642247
    #xfc8cc4a8 #xf03f1aa0 #x7d2cd856 #x3390ef22
    #x494ec787 #x38d1c1d9 #xcaa2fe8c #xd40b3698
    #xf581cfa6 #x7ade28a5 #xb78e26da #xadbfa43f
    #x3a9de42c #x78920d50 #x5fcc9b6a #x7e466254
    #x8d13c2f6 #xd8b8e890 #x39f75e2e #xc3aff582
    #x5d80be9f #xd0937c69 #xd52da96f #x2512b3cf
    #xac993bc8 #x187da710 #x9c636ee8 #x3bbb7bdb
    #x267809cd #x5918f46e #x9ab701ec #x4f9aa883
    #x956e65e6 #xffe67eaa #xbccf0821 #x15e8e6ef
    #xe79bd9ba #x6f36ce4a #x9f09d4ea #xb07cd629
    #xa4b2af31 #x3f23312a #xa59430c6 #xa266c035
    #x4ebc3774 #x82caa6fc #x90d0b0e0 #xa7d81533
    #x04984af1 #xecdaf741 #xcd500e7f #x91f62f17
    #x4dd68d76 #xefb04d43 #xaa4d54cc #x9604dfe4
    #xd1b5e39e #x6a881b4c #x2c1fb8c1 #x65517f46
    #x5eea049d #x8c355d01 #x877473fa #x0b412efb
    #x671d5ab3 #xdbd25292 #x105633e9 #xd647136d
    #xd7618c9a #xa10c7a37 #xf8148e59 #x133c89eb
    #xa927eece #x61c935b7 #x1ce5ede1 #x47b13c7a
    #xd2df599c #xf2733f55 #x14ce7918 #xc737bf73
    #xf7cdea53 #xfdaa5b5f #x3d6f14df #x44db8678
    #xaff381ca #x68c43eb9 #x24342c38 #xa3405fc2
    #x1dc37216 #xe2250cbc #x3c498b28 #x0d9541ff
    #xa8017139 #x0cb3de08 #xb4e49cd8 #x56c19064
    #xcb84617b #x32b670d5 #x6c5c7448 #xb85742d0))

(defconst Td4
#32@(#x52525252 #x09090909 #x6a6a6a6a #xd5d5d5d5
    #x30303030 #x36363636 #xa5a5a5a5 #x38383838
    #xbfbfbfbf #x40404040 #xa3a3a3a3 #x9e9e9e9e
    #x81818181 #xf3f3f3f3 #xd7d7d7d7 #xfbfbfbfb
    #x7c7c7c7c #xe3e3e3e3 #x39393939 #x82828282
    #x9b9b9b9b #x2f2f2f2f #xffffffff #x87878787
    #x34343434 #x8e8e8e8e #x43434343 #x44444444
    #xc4c4c4c4 #xdededede #xe9e9e9e9 #xcbcbcbcb
    #x54545454 #x7b7b7b7b #x94949494 #x32323232
    #xa6a6a6a6 #xc2c2c2c2 #x23232323 #x3d3d3d3d
    #xeeeeeeee #x4c4c4c4c #x95959595 #x0b0b0b0b
    #x42424242 #xfafafafa #xc3c3c3c3 #x4e4e4e4e
    #x08080808 #x2e2e2e2e #xa1a1a1a1 #x66666666
    #x28282828 #xd9d9d9d9 #x24242424 #xb2b2b2b2
    #x76767676 #x5b5b5b5b #xa2a2a2a2 #x49494949
    #x6d6d6d6d #x8b8b8b8b #xd1d1d1d1 #x25252525
    #x72727272 #xf8f8f8f8 #xf6f6f6f6 #x64646464
    #x86868686 #x68686868 #x98989898 #x16161616
    #xd4d4d4d4 #xa4a4a4a4 #x5c5c5c5c #xcccccccc
    #x5d5d5d5d #x65656565 #xb6b6b6b6 #x92929292
    #x6c6c6c6c #x70707070 #x48484848 #x50505050
    #xfdfdfdfd #xedededed #xb9b9b9b9 #xdadadada
    #x5e5e5e5e #x15151515 #x46464646 #x57575757
    #xa7a7a7a7 #x8d8d8d8d #x9d9d9d9d #x84848484
    #x90909090 #xd8d8d8d8 #xabababab #x00000000
    #x8c8c8c8c #xbcbcbcbc #xd3d3d3d3 #x0a0a0a0a
    #xf7f7f7f7 #xe4e4e4e4 #x58585858 #x05050505
    #xb8b8b8b8 #xb3b3b3b3 #x45454545 #x06060606
    #xd0d0d0d0 #x2c2c2c2c #x1e1e1e1e #x8f8f8f8f
    #xcacacaca #x3f3f3f3f #x0f0f0f0f #x02020202
    #xc1c1c1c1 #xafafafaf #xbdbdbdbd #x03030303
    #x01010101 #x13131313 #x8a8a8a8a #x6b6b6b6b
    #x3a3a3a3a #x91919191 #x11111111 #x41414141
    #x4f4f4f4f #x67676767 #xdcdcdcdc #xeaeaeaea
    #x97979797 #xf2f2f2f2 #xcfcfcfcf #xcececece
    #xf0f0f0f0 #xb4b4b4b4 #xe6e6e6e6 #x73737373
    #x96969696 #xacacacac #x74747474 #x22222222
    #xe7e7e7e7 #xadadadad #x35353535 #x85858585
    #xe2e2e2e2 #xf9f9f9f9 #x37373737 #xe8e8e8e8
    #x1c1c1c1c #x75757575 #xdfdfdfdf #x6e6e6e6e
    #x47474747 #xf1f1f1f1 #x1a1a1a1a #x71717171
    #x1d1d1d1d #x29292929 #xc5c5c5c5 #x89898989
    #x6f6f6f6f #xb7b7b7b7 #x62626262 #x0e0e0e0e
    #xaaaaaaaa #x18181818 #xbebebebe #x1b1b1b1b
    #xfcfcfcfc #x56565656 #x3e3e3e3e #x4b4b4b4b
    #xc6c6c6c6 #xd2d2d2d2 #x79797979 #x20202020
    #x9a9a9a9a #xdbdbdbdb #xc0c0c0c0 #xfefefefe
    #x78787878 #xcdcdcdcd #x5a5a5a5a #xf4f4f4f4
    #x1f1f1f1f #xdddddddd #xa8a8a8a8 #x33333333
    #x88888888 #x07070707 #xc7c7c7c7 #x31313131
    #xb1b1b1b1 #x12121212 #x10101010 #x59595959
    #x27272727 #x80808080 #xecececec #x5f5f5f5f
    #x60606060 #x51515151 #x7f7f7f7f #xa9a9a9a9
    #x19191919 #xb5b5b5b5 #x4a4a4a4a #x0d0d0d0d
    #x2d2d2d2d #xe5e5e5e5 #x7a7a7a7a #x9f9f9f9f
    #x93939393 #xc9c9c9c9 #x9c9c9c9c #xefefefef
    #xa0a0a0a0 #xe0e0e0e0 #x3b3b3b3b #x4d4d4d4d
    #xaeaeaeae #x2a2a2a2a #xf5f5f5f5 #xb0b0b0b0
    #xc8c8c8c8 #xebebebeb #xbbbbbbbb #x3c3c3c3c
    #x83838383 #x53535353 #x99999999 #x61616161
    #x17171717 #x2b2b2b2b #x04040404 #x7e7e7e7e
    #xbabababa #x77777777 #xd6d6d6d6 #x26262626
    #xe1e1e1e1 #x69696969 #x14141414 #x63636363
    #x55555555 #x21212121 #x0c0c0c0c #x7d7d7d7d))

(declaim (type (simple-array (unsigned-byte 32) (10)) round-constants))
(defconst round-constants
#32@(#x01000000 #x02000000 #x04000000 #x08000000 #x10000000
                #x20000000 #x40000000 #x80000000 #x1B000000 #x36000000))


;;; the actual AES implementation

;;; waste a little space for "common" 128-bit keys, but is anybody really
;;; going to notice?
(deftype aes-round-keys () '(simple-array (unsigned-byte 32) (60)))

(defclass aes (cipher 16-byte-block-mixin)
  ((encryption-round-keys :accessor encryption-round-keys
                          :type aes-round-keys)
   (decryption-round-keys :accessor decryption-round-keys
                          :type aes-round-keys)
   (n-rounds :accessor n-rounds)))

(defun allocate-round-keys (key)
  (declare (type (simple-array (unsigned-byte 8) (*)) key))
  (ecase (length key)
    ((16 24 32)
     (make-array 60 :element-type '(unsigned-byte 32) :initial-element 0))))

(defun generate-128-bit-round-keys (round-keys key)
  (declare (type aes-round-keys round-keys)
           (type (simple-array (unsigned-byte 8) (16)) key)
           (optimize (speed 3) (space 0) (debug 0)))
  (let ((round-key-offset 0))
    (declare (type (integer 0 43) round-key-offset))
    (macrolet ((rk-ref (x) `(aref round-keys (+ ,x round-key-offset))))
      (dotimes (i 4)
        (setf (rk-ref i) (ub32ref/be key (* 4 i))))
      (dotimes (i 10 (values round-keys 10))
        (declare (type (integer 0 10) i))
        (let ((tmp (rk-ref 3)))
          (declare (type (unsigned-byte 32) tmp))
          (setf (rk-ref 4)
                (logxor (rk-ref 0)
                        (logand (aref Te4 (third-byte tmp)) #xff000000)
                        (logand (aref Te4 (second-byte tmp)) #x00ff0000)
                        (logand (aref Te4 (first-byte tmp)) #x0000ff00)
                        (logand (aref Te4 (fourth-byte tmp)) #x000000ff)
                        (aref round-constants i))
                (rk-ref 5) (logxor (rk-ref 1) (rk-ref 4))
                (rk-ref 6) (logxor (rk-ref 2) (rk-ref 5))
                (rk-ref 7) (logxor (rk-ref 3) (rk-ref 6)))
          (incf round-key-offset 4))))))

(defun generate-192-bit-round-keys (round-keys key)
  (declare (type aes-round-keys round-keys)
           (type (simple-array (unsigned-byte 8) (24)) key)
           (optimize (speed 3) (space 0) (debug 0)))
  (let ((round-key-offset 0))
    (declare (type (integer 0 51) round-key-offset))
    (macrolet ((rk-ref (x) `(aref round-keys (+ ,x round-key-offset))))
      (dotimes (i 6)
        (setf (rk-ref i) (ub32ref/be key (* 4 i))))
      (dotimes (i 8)
        (let ((tmp (rk-ref 5)))
          (declare (type (unsigned-byte 32) tmp))
          (setf (rk-ref 6)
                (logxor (rk-ref 0)
                        (logand (aref Te4 (third-byte tmp)) #xff000000)
                        (logand (aref Te4 (second-byte tmp)) #x00ff0000)
                        (logand (aref Te4 (first-byte tmp)) #x0000ff00)
                        (logand (aref Te4 (fourth-byte tmp)) #x000000ff)
                        (aref round-constants i))
                (rk-ref 7) (logxor (rk-ref 1) (rk-ref 6))
                (rk-ref 8) (logxor (rk-ref 2) (rk-ref 7))
                (rk-ref 9) (logxor (rk-ref 3) (rk-ref 8)))
          (when (= 8 (1+ i))
            (return-from generate-192-bit-round-keys (values round-keys 12)))
          (setf (rk-ref 10) (logxor (rk-ref 4) (rk-ref 9))
                (rk-ref 11) (logxor (rk-ref 5) (rk-ref 10)))
          (incf round-key-offset 6))))))

(defun generate-256-bit-round-keys (round-keys key)
  (declare (type aes-round-keys round-keys)
           (type (simple-array (unsigned-byte 8) (32)) key)
           (optimize (speed 3) (space 0) (debug 0)))
  (let ((round-key-offset 0))
    (declare (type (integer 0 59) round-key-offset))
    (macrolet ((rk-ref (x) `(aref round-keys (+ ,x round-key-offset))))
      (dotimes (i 8)
        (setf (rk-ref i) (ub32ref/be key (* 4 i))))
      (dotimes (i 7)
        (let ((tmp (rk-ref 7)))
          (declare (type (unsigned-byte 32) tmp))
          (setf (rk-ref 8)
                (logxor (rk-ref 0)
                        (logand (aref Te4 (third-byte tmp)) #xff000000)
                        (logand (aref Te4 (second-byte tmp)) #x00ff0000)
                        (logand (aref Te4 (first-byte tmp)) #x0000ff00)
                        (logand (aref Te4 (fourth-byte tmp)) #x000000ff)
                        (aref round-constants i))
                (rk-ref 9) (logxor (rk-ref 1) (rk-ref 8))
                (rk-ref 10) (logxor (rk-ref 2) (rk-ref 9))
                (rk-ref 11) (logxor (rk-ref 3) (rk-ref 10)))
          (when (= 7 (1+ i))
            (return-from generate-256-bit-round-keys (values round-keys 14))))
        (let ((tmp (rk-ref 11)))
          (declare (type (unsigned-byte 32) tmp))
          (setf (rk-ref 12)
                (logxor (rk-ref 4)
                        (logand (aref Te4 (fourth-byte tmp)) #xff000000)
                        (logand (aref Te4 (third-byte tmp)) #x00ff0000)
                        (logand (aref Te4 (second-byte tmp)) #x0000ff00)
                        (logand (aref Te4 (first-byte tmp)) #x000000ff))
                (rk-ref 13) (logxor (rk-ref 5) (rk-ref 12))
                (rk-ref 14) (logxor (rk-ref 6) (rk-ref 13))
                (rk-ref 15) (logxor (rk-ref 7) (rk-ref 14)))
          (incf round-key-offset 8))))))

(defun generate-round-keys-for-encryption (key round-keys)
  (declare (type (simple-array (unsigned-byte 8) (*)) key))
  (ecase (length key)
    (16 (generate-128-bit-round-keys round-keys key))
    (24 (generate-192-bit-round-keys round-keys key))
    (32 (generate-256-bit-round-keys round-keys key))))

(defun generate-round-keys-for-decryption (round-keys n-rounds)
  (declare (type aes-round-keys round-keys)
           (type (unsigned-byte 16) n-rounds))
  ;; invert the order of the round keys
  (do ((i 0 (+ 4 i))
       (j (* 4 n-rounds) (- j 4)))
      ((>= i j))
    (declare (type (unsigned-byte 16) i j))
    (rotatef (aref round-keys i) (aref round-keys j))
    (rotatef (aref round-keys (+ 1 i)) (aref round-keys (+ 1 j)))
    (rotatef (aref round-keys (+ 2 i)) (aref round-keys (+ 2 j)))
    (rotatef (aref round-keys (+ 3 i)) (aref round-keys (+ 3 j))))
  ;; apply inverse MixColumn transform to all round keys but the first
  (macrolet ((rk-ref (x) `(aref round-keys (+ ,x round-keys-offset))))
    (do ((i 1 (+ 1 i))
         (round-keys-offset 4 (+ 4 round-keys-offset)))
        ((>= i n-rounds) (values round-keys n-rounds))
      (declare (type (unsigned-byte 16) round-keys-offset))
      (macrolet ((mix-column (x)
                   `(let ((column (rk-ref ,x)))
                      (declare (type (unsigned-byte 32) column))
                      (setf (rk-ref ,x)
                            (logxor
                             (aref Td0 (first-byte (aref Te4 (fourth-byte column))))
                             (aref Td1 (first-byte (aref Te4 (third-byte column))))
                             (aref Td2 (first-byte (aref Te4 (second-byte column))))
                             (aref Td3 (first-byte (aref Te4 (first-byte column)))))))))
        (mix-column 0) (mix-column 1) (mix-column 2) (mix-column 3)))))

(macrolet ((mix (rk a0 a1 a2 a3 sym0 sym1 sym2 sym3)
                   `(logxor (aref ,a0 (fourth-byte ,sym0))
                            (aref ,a1 (third-byte ,sym1))
                            (aref ,a2 (second-byte ,sym2))
                            (aref ,a3 (first-byte ,sym3))
                            (rk-ref ,rk)))
           (mix-s-into-t-encrypting (offset)
             `(setf t0 (mix ,offset Te0 Te1 Te2 Te3 s0 s1 s2 s3)
               t1 (mix (1+ ,offset) Te0 Te1 Te2 Te3 s1 s2 s3 s0)
               t2 (mix (+ ,offset 2) Te0 Te1 Te2 Te3 s2 s3 s0 s1)
               t3 (mix (+ ,offset 3) Te0 Te1 Te2 Te3 s3 s0 s1 s2)))
           (mix-t-into-s-encrypting (offset)
               `(setf s0 (mix ,offset Te0 Te1 Te2 Te3 t0 t1 t2 t3)
                 s1 (mix (1+ ,offset) Te0 Te1 Te2 Te3 t1 t2 t3 t0)
                 s2 (mix (+ ,offset 2) Te0 Te1 Te2 Te3 t2 t3 t0 t1)
                 s3 (mix (+ ,offset 3) Te0 Te1 Te2 Te3 t3 t0 t1 t2)))
           (mix-s-into-t-decrypting (offset)
             `(setf t0 (mix ,offset Td0 Td1 Td2 Td3 s0 s3 s2 s1)
               t1 (mix (1+ ,offset) Td0 Td1 Td2 Td3 s1 s0 s3 s2)
               t2 (mix (+ ,offset 2) Td0 Td1 Td2 Td3 s2 s1 s0 s3)
               t3 (mix (+ ,offset 3) Td0 Td1 Td2 Td3 s3 s2 s1 s0)))
           (mix-t-into-s-decrypting (offset)
               `(setf s0 (mix ,offset Td0 Td1 Td2 Td3 t0 t3 t2 t1)
                 s1 (mix (1+ ,offset) Td0 Td1 Td2 Td3 t1 t0 t3 t2)
                 s2 (mix (+ ,offset 2) Td0 Td1 Td2 Td3 t2 t1 t0 t3)
                 s3 (mix (+ ,offset 3) Td0 Td1 Td2 Td3 t3 t2 t1 t0)))
           (rk-ref (x) `(aref round-keys (+ ,x round-key-offset)))
           #+nil (rk-ref (x) `(aref round-keys (+ ,x 0))))

(define-block-encryptor aes 16
  (let ((round-keys (encryption-round-keys context))
        (n-rounds (n-rounds context)))
    (declare (type aes-round-keys round-keys))
    (declare (type (integer 0 14) n-rounds))
    (if #+(and sbcl x86-64 ironclad-assembly) (aes-ni-supported-p)
        #-(and sbcl x86-64 ironclad-assembly) nil
        #+(and sbcl x86-64 ironclad-assembly)
        (aes-ni-encrypt plaintext plaintext-start
                        ciphertext ciphertext-start
                        round-keys n-rounds)
        #-(and sbcl x86-64 ironclad-assembly) nil
        (with-words ((s0 s1 s2 s3) plaintext plaintext-start)
          ;; the "optimized implementation" also had a fully unrolled version
          ;; of this loop hanging around. it might be worthwhile to translate
          ;; it and see if it actually gains us anything. a wizard would do
          ;; this with a macro which allows one to easily switch between
          ;; unrolled and non-unrolled versions. I am not a wizard.
          (let ((t0 0) (t1 0) (t2 0) (t3 0)
                (round-key-offset 0))
            (declare (type (unsigned-byte 32) t0 t1 t2 t3))
            (declare (type (unsigned-byte 16) round-key-offset))
            ;; initial whitening
            (setf s0 (logxor s0 (aref round-keys 0))
                  s1 (logxor s1 (aref round-keys 1))
                  s2 (logxor s2 (aref round-keys 2))
                  s3 (logxor s3 (aref round-keys 3)))
            (do ((round (truncate n-rounds 2) (1- round)))
                ((zerop round))
              (declare (type (unsigned-byte 16) round))
              (mix-s-into-t-encrypting 4)
              (incf round-key-offset 8)
              (when (= round 1)
                (return-from nil (values)))
              (mix-t-into-s-encrypting 0))
            ;; apply last round and dump cipher state into the ciphertext
            (flet ((apply-round (round-key u0 u1 u2 u3)
                     (declare (type (unsigned-byte 32) round-key u0 u1 u2 u3))
                     (logxor (logand (aref Te4 (fourth-byte u0)) #xff000000)
                             (logand (aref Te4 (third-byte u1)) #x00ff0000)
                             (logand (aref Te4 (second-byte u2)) #x0000ff00)
                             (logand (aref Te4 (first-byte u3)) #x000000ff)
                             round-key)))
              (declare (inline apply-round))
              (store-words ciphertext ciphertext-start
                           (apply-round (rk-ref 0) t0 t1 t2 t3)
                           (apply-round (rk-ref 1) t1 t2 t3 t0)
                           (apply-round (rk-ref 2) t2 t3 t0 t1)
                           (apply-round (rk-ref 3) t3 t0 t1 t2))))))))

(define-block-decryptor aes 16
  (let ((round-keys (decryption-round-keys context))
        (n-rounds (n-rounds context)))
    (declare (type aes-round-keys round-keys))
    (declare (type (unsigned-byte 16) n-rounds))
    (if #+(and sbcl x86-64 ironclad-assembly) (aes-ni-supported-p)
        #-(and sbcl x86-64 ironclad-assembly) nil
        #+(and sbcl x86-64 ironclad-assembly)
        (aes-ni-decrypt ciphertext ciphertext-start
                        plaintext plaintext-start
                        round-keys n-rounds)
        #-(and sbcl x86-64 ironclad-assembly) nil
        (with-words ((s0 s1 s2 s3) ciphertext ciphertext-start)
          (let ((t0 0) (t1 0) (t2 0) (t3 0)
                (round-key-offset 0))
            (declare (type (unsigned-byte 32) t0 t1 t2 t3))
            (declare (type (unsigned-byte 16) round-key-offset))
            ;; initial whitening
            (setf s0 (logxor s0 (aref round-keys 0))
                  s1 (logxor s1 (aref round-keys 1))
                  s2 (logxor s2 (aref round-keys 2))
                  s3 (logxor s3 (aref round-keys 3)))
            (do ((round (truncate n-rounds 2) (1- round)))
                ((zerop round))
              (declare (type (unsigned-byte 16) round))
              (mix-s-into-t-decrypting 4)
              (incf round-key-offset 8)
              (when (= round 1)
                (return-from nil (values)))
              (mix-t-into-s-decrypting 0))
            ;; apply last round and dump cipher state into plaintext
            (flet ((apply-round (round-key u0 u1 u2 u3)
                     (declare (type (unsigned-byte 32) round-key u0 u1 u2 u3))
                     (logxor (logand (aref Td4 (fourth-byte u0)) #xff000000)
                             (logand (aref Td4 (third-byte u1)) #x00ff0000)
                             (logand (aref Td4 (second-byte u2)) #x0000ff00)
                             (logand (aref Td4 (first-byte u3)) #x000000ff)
                             round-key)))
              (declare (inline apply-round))
              (store-words plaintext plaintext-start
                           (apply-round (rk-ref 0) t0 t3 t2 t1)
                           (apply-round (rk-ref 1) t1 t0 t3 t2)
                           (apply-round (rk-ref 2) t2 t1 t0 t3)
                           (apply-round (rk-ref 3) t3 t2 t1 t0))))))))

) ; MACROLET

(defmethod schedule-key ((cipher aes) key)
  (if #+(and sbcl x86-64 ironclad-assembly) (aes-ni-supported-p)
      #-(and sbcl x86-64 ironclad-assembly) nil
      (let ((encryption-keys (allocate-round-keys key))
            (decryption-keys (allocate-round-keys key))
            (n-rounds (ecase (length key)
                        (16 10)
                        (24 12)
                        (32 14))))
        (declare (type aes-round-keys encryption-keys decryption-keys))
        #+(and sbcl x86-64 ironclad-assembly)
        (aes-ni-generate-round-keys key (length key)
                                    encryption-keys decryption-keys)
        (setf (encryption-round-keys cipher) encryption-keys
              (decryption-round-keys cipher) decryption-keys
              (n-rounds cipher) n-rounds)
        cipher)
      (multiple-value-bind (encryption-keys n-rounds)
          (generate-round-keys-for-encryption key (allocate-round-keys key))
        (declare (type aes-round-keys encryption-keys))
        (let ((decryption-keys (copy-seq encryption-keys)))
          (generate-round-keys-for-decryption decryption-keys n-rounds)
          (setf (encryption-round-keys cipher) encryption-keys
                (decryption-round-keys cipher) decryption-keys
                (n-rounds cipher) n-rounds)
          cipher))))

(defcipher aes
  (:encrypt-function aes-encrypt-block)
  (:decrypt-function aes-decrypt-block)
  (:block-length 16)
  (:key-length (:fixed 16 24 32)))
