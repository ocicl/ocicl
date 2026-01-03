;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; twofish.lisp -- implementation of Counterpane's Twofish AES candidate

(in-package :crypto)
(in-ironclad-readtable)


;;; various constant data arrays used by Twofish

(declaim (type (simple-octet-vector 256)
               +twofish-q0+ +twofish-q1+))
(declaim (type (simple-octet-vector 255)
               +twofish-exp-to-poly+ +twofish-poly-to-exp+))
(defconst +twofish-q0+
#8@(#xA9 #x67 #xB3 #xE8 #x04 #xFD #xA3 #x76 #x9A #x92 #x80 #x78 #xE4
#xDD #xD1 #x38 #x0D #xC6 #x35 #x98 #x18 #xF7 #xEC #x6C #x43 #x75
#x37 #x26 #xFA #x13 #x94 #x48 #xF2 #xD0 #x8B #x30 #x84 #x54 #xDF
#x23 #x19 #x5B #x3D #x59 #xF3 #xAE #xA2 #x82 #x63 #x01 #x83 #x2E
#xD9 #x51 #x9B #x7C #xA6 #xEB #xA5 #xBE #x16 #x0C #xE3 #x61 #xC0
#x8C #x3A #xF5 #x73 #x2C #x25 #x0B #xBB #x4E #x89 #x6B #x53 #x6A
#xB4 #xF1 #xE1 #xE6 #xBD #x45 #xE2 #xF4 #xB6 #x66 #xCC #x95 #x03
#x56 #xD4 #x1C #x1E #xD7 #xFB #xC3 #x8E #xB5 #xE9 #xCF #xBF #xBA
#xEA #x77 #x39 #xAF #x33 #xC9 #x62 #x71 #x81 #x79 #x09 #xAD #x24
#xCD #xF9 #xD8 #xE5 #xC5 #xB9 #x4D #x44 #x08 #x86 #xE7 #xA1 #x1D
#xAA #xED #x06 #x70 #xB2 #xD2 #x41 #x7B #xA0 #x11 #x31 #xC2 #x27
#x90 #x20 #xF6 #x60 #xFF #x96 #x5C #xB1 #xAB #x9E #x9C #x52 #x1B
#x5F #x93 #x0A #xEF #x91 #x85 #x49 #xEE #x2D #x4F #x8F #x3B #x47
#x87 #x6D #x46 #xD6 #x3E #x69 #x64 #x2A #xCE #xCB #x2F #xFC #x97
#x05 #x7A #xAC #x7F #xD5 #x1A #x4B #x0E #xA7 #x5A #x28 #x14 #x3F
#x29 #x88 #x3C #x4C #x02 #xB8 #xDA #xB0 #x17 #x55 #x1F #x8A #x7D
#x57 #xC7 #x8D #x74 #xB7 #xC4 #x9F #x72 #x7E #x15 #x22 #x12 #x58
#x07 #x99 #x34 #x6E #x50 #xDE #x68 #x65 #xBC #xDB #xF8 #xC8 #xA8
#x2B #x40 #xDC #xFE #x32 #xA4 #xCA #x10 #x21 #xF0 #xD3 #x5D #x0F
#x00 #x6F #x9D #x36 #x42 #x4A #x5E #xC1 #xE0))

(defconst +twofish-q1+
#8@(#x75 #xF3 #xC6 #xF4 #xDB #x7B #xFB #xC8 #x4A #xD3 #xE6 #x6B #x45
#x7D #xE8 #x4B #xD6 #x32 #xD8 #xFD #x37 #x71 #xF1 #xE1 #x30 #x0F
#xF8 #x1B #x87 #xFA #x06 #x3F #x5E #xBA #xAE #x5B #x8A #x00 #xBC
#x9D #x6D #xC1 #xB1 #x0E #x80 #x5D #xD2 #xD5 #xA0 #x84 #x07 #x14
#xB5 #x90 #x2C #xA3 #xB2 #x73 #x4C #x54 #x92 #x74 #x36 #x51 #x38
#xB0 #xBD #x5A #xFC #x60 #x62 #x96 #x6C #x42 #xF7 #x10 #x7C #x28
#x27 #x8C #x13 #x95 #x9C #xC7 #x24 #x46 #x3B #x70 #xCA #xE3 #x85
#xCB #x11 #xD0 #x93 #xB8 #xA6 #x83 #x20 #xFF #x9F #x77 #xC3 #xCC
#x03 #x6F #x08 #xBF #x40 #xE7 #x2B #xE2 #x79 #x0C #xAA #x82 #x41
#x3A #xEA #xB9 #xE4 #x9A #xA4 #x97 #x7E #xDA #x7A #x17 #x66 #x94
#xA1 #x1D #x3D #xF0 #xDE #xB3 #x0B #x72 #xA7 #x1C #xEF #xD1 #x53
#x3E #x8F #x33 #x26 #x5F #xEC #x76 #x2A #x49 #x81 #x88 #xEE #x21
#xC4 #x1A #xEB #xD9 #xC5 #x39 #x99 #xCD #xAD #x31 #x8B #x01 #x18
#x23 #xDD #x1F #x4E #x2D #xF9 #x48 #x4F #xF2 #x65 #x8E #x78 #x5C
#x58 #x19 #x8D #xE5 #x98 #x57 #x67 #x7F #x05 #x64 #xAF #x63 #xB6
#xFE #xF5 #xB7 #x3C #xA5 #xCE #xE9 #x68 #x44 #xE0 #x4D #x43 #x69
#x29 #x2E #xAC #x15 #x59 #xA8 #x0A #x9E #x6E #x47 #xDF #x34 #x35
#x6A #xCF #xDC #x22 #xC9 #xC0 #x9B #x89 #xD4 #xED #xAB #x12 #xA2
#x0D #x52 #xBB #x02 #x2F #xA9 #xD7 #x61 #x1E #xB4 #x50 #x04 #xF6
#xC2 #x16 #x25 #x86 #x56 #x55 #x09 #xBE #x91))

(defconst +twofish-rs+
#8@(#x01 #xA4 #x02 #xA4 #xA4 #x56 #xA1 #x55
#x55 #x82 #xFC #x87 #x87 #xF3 #xC1 #x5A
#x5A #x1E #x47 #x58 #x58 #xC6 #xAE #xDB
#xDB #x68 #x3D #x9E #x9E #xE5 #x19 #x03))

(defconst +twofish-exp-to-poly+
#8@(#x01 #x02 #x04 #x08 #x10 #x20 #x40 #x80 #x4D #x9A #x79 #xF2 #xA9
#x1F #x3E #x7C #xF8 #xBD #x37 #x6E #xDC #xF5 #xA7 #x03 #x06 #x0C
#x18 #x30 #x60 #xC0 #xCD #xD7 #xE3 #x8B #x5B #xB6 #x21 #x42 #x84
#x45 #x8A #x59 #xB2 #x29 #x52 #xA4 #x05 #x0A #x14 #x28 #x50 #xA0
#x0D #x1A #x34 #x68 #xD0 #xED #x97 #x63 #xC6 #xC1 #xCF #xD3 #xEB
#x9B #x7B #xF6 #xA1 #x0F #x1E #x3C #x78 #xF0 #xAD #x17 #x2E #x5C
#xB8 #x3D #x7A #xF4 #xA5 #x07 #x0E #x1C #x38 #x70 #xE0 #x8D #x57
#xAE #x11 #x22 #x44 #x88 #x5D #xBA #x39 #x72 #xE4 #x85 #x47 #x8E
#x51 #xA2 #x09 #x12 #x24 #x48 #x90 #x6D #xDA #xF9 #xBF #x33 #x66
#xCC #xD5 #xE7 #x83 #x4B #x96 #x61 #xC2 #xC9 #xDF #xF3 #xAB #x1B
#x36 #x6C #xD8 #xFD #xB7 #x23 #x46 #x8C #x55 #xAA #x19 #x32 #x64
#xC8 #xDD #xF7 #xA3 #x0B #x16 #x2C #x58 #xB0 #x2D #x5A #xB4 #x25
#x4A #x94 #x65 #xCA #xD9 #xFF #xB3 #x2B #x56 #xAC #x15 #x2A #x54
#xA8 #x1D #x3A #x74 #xE8 #x9D #x77 #xEE #x91 #x6F #xDE #xF1 #xAF
#x13 #x26 #x4C #x98 #x7D #xFA #xB9 #x3F #x7E #xFC #xB5 #x27 #x4E
#x9C #x75 #xEA #x99 #x7F #xFE #xB1 #x2F #x5E #xBC #x35 #x6A #xD4
#xE5 #x87 #x43 #x86 #x41 #x82 #x49 #x92 #x69 #xD2 #xE9 #x9F #x73
#xE6 #x81 #x4F #x9E #x71 #xE2 #x89 #x5F #xBE #x31 #x62 #xC4 #xC5
#xC7 #xC3 #xCB #xDB #xFB #xBB #x3B #x76 #xEC #x95 #x67 #xCE #xD1
#xEF #x93 #x6B #xD6 #xE1 #x8F #x53 #xA6))

(defconst +twofish-poly-to-exp+
#8@(#x00 #x01 #x17 #x02 #x2E #x18 #x53 #x03 #x6A #x2F #x93 #x19 #x34
#x54 #x45 #x04 #x5C #x6B #xB6 #x30 #xA6 #x94 #x4B #x1A #x8C #x35
#x81 #x55 #xAA #x46 #x0D #x05 #x24 #x5D #x87 #x6C #x9B #xB7 #xC1
#x31 #x2B #xA7 #xA3 #x95 #x98 #x4C #xCA #x1B #xE6 #x8D #x73 #x36
#xCD #x82 #x12 #x56 #x62 #xAB #xF0 #x47 #x4F #x0E #xBD #x06 #xD4
#x25 #xD2 #x5E #x27 #x88 #x66 #x6D #xD6 #x9C #x79 #xB8 #x08 #xC2
#xDF #x32 #x68 #x2C #xFD #xA8 #x8A #xA4 #x5A #x96 #x29 #x99 #x22
#x4D #x60 #xCB #xE4 #x1C #x7B #xE7 #x3B #x8E #x9E #x74 #xF4 #x37
#xD8 #xCE #xF9 #x83 #x6F #x13 #xB2 #x57 #xE1 #x63 #xDC #xAC #xC4
#xF1 #xAF #x48 #x0A #x50 #x42 #x0F #xBA #xBE #xC7 #x07 #xDE #xD5
#x78 #x26 #x65 #xD3 #xD1 #x5F #xE3 #x28 #x21 #x89 #x59 #x67 #xFC
#x6E #xB1 #xD7 #xF8 #x9D #xF3 #x7A #x3A #xB9 #xC6 #x09 #x41 #xC3
#xAE #xE0 #xDB #x33 #x44 #x69 #x92 #x2D #x52 #xFE #x16 #xA9 #x0C
#x8B #x80 #xA5 #x4A #x5B #xB5 #x97 #xC9 #x2A #xA2 #x9A #xC0 #x23
#x86 #x4E #xBC #x61 #xEF #xCC #x11 #xE5 #x72 #x1D #x3D #x7C #xEB
#xE8 #xE9 #x3C #xEA #x8F #x7D #x9F #xEC #x75 #x1E #xF5 #x3E #x38
#xF6 #xD9 #x3F #xCF #x76 #xFA #x1F #x84 #xA0 #x70 #xED #x14 #x90
#xB3 #x7E #x58 #xFB #xE2 #x20 #x64 #xD0 #xDD #x77 #xAD #xDA #xC5
#x40 #xF2 #x39 #xB0 #xF7 #x49 #xB4 #x0B #x7F #x51 #x15 #x43 #x91
#x10 #x71 #xBB #xEE #xBF #x85 #xC8 #xA1))

(declaim (type (simple-array (unsigned-byte 32) (256))
               +twofish-mds0+ +twofish-mds1+ +twofish-mds2+ +twofish-mds3+))
(defconst +twofish-mds0+
#32@(#xBCBC3275 #xECEC21F3 #x202043C6 #xB3B3C9F4 #xDADA03DB #x02028B7B
#xE2E22BFB #x9E9EFAC8 #xC9C9EC4A #xD4D409D3 #x18186BE6 #x1E1E9F6B
#x98980E45 #xB2B2387D #xA6A6D2E8 #x2626B74B #x3C3C57D6 #x93938A32
#x8282EED8 #x525298FD #x7B7BD437 #xBBBB3771 #x5B5B97F1 #x474783E1
#x24243C30 #x5151E20F #xBABAC6F8 #x4A4AF31B #xBFBF4887 #x0D0D70FA
#xB0B0B306 #x7575DE3F #xD2D2FD5E #x7D7D20BA #x666631AE #x3A3AA35B
#x59591C8A #x00000000 #xCDCD93BC #x1A1AE09D #xAEAE2C6D #x7F7FABC1
#x2B2BC7B1 #xBEBEB90E #xE0E0A080 #x8A8A105D #x3B3B52D2 #x6464BAD5
#xD8D888A0 #xE7E7A584 #x5F5FE807 #x1B1B1114 #x2C2CC2B5 #xFCFCB490
#x3131272C #x808065A3 #x73732AB2 #x0C0C8173 #x79795F4C #x6B6B4154
#x4B4B0292 #x53536974 #x94948F36 #x83831F51 #x2A2A3638 #xC4C49CB0
#x2222C8BD #xD5D5F85A #xBDBDC3FC #x48487860 #xFFFFCE62 #x4C4C0796
#x4141776C #xC7C7E642 #xEBEB24F7 #x1C1C1410 #x5D5D637C #x36362228
#x6767C027 #xE9E9AF8C #x4444F913 #x1414EA95 #xF5F5BB9C #xCFCF18C7
#x3F3F2D24 #xC0C0E346 #x7272DB3B #x54546C70 #x29294CCA #xF0F035E3
#x0808FE85 #xC6C617CB #xF3F34F11 #x8C8CE4D0 #xA4A45993 #xCACA96B8
#x68683BA6 #xB8B84D83 #x38382820 #xE5E52EFF #xADAD569F #x0B0B8477
#xC8C81DC3 #x9999FFCC #x5858ED03 #x19199A6F #x0E0E0A08 #x95957EBF
#x70705040 #xF7F730E7 #x6E6ECF2B #x1F1F6EE2 #xB5B53D79 #x09090F0C
#x616134AA #x57571682 #x9F9F0B41 #x9D9D803A #x111164EA #x2525CDB9
#xAFAFDDE4 #x4545089A #xDFDF8DA4 #xA3A35C97 #xEAEAD57E #x353558DA
#xEDEDD07A #x4343FC17 #xF8F8CB66 #xFBFBB194 #x3737D3A1 #xFAFA401D
#xC2C2683D #xB4B4CCF0 #x32325DDE #x9C9C71B3 #x5656E70B #xE3E3DA72
#x878760A7 #x15151B1C #xF9F93AEF #x6363BFD1 #x3434A953 #x9A9A853E
#xB1B1428F #x7C7CD133 #x88889B26 #x3D3DA65F #xA1A1D7EC #xE4E4DF76
#x8181942A #x91910149 #x0F0FFB81 #xEEEEAA88 #x161661EE #xD7D77321
#x9797F5C4 #xA5A5A81A #xFEFE3FEB #x6D6DB5D9 #x7878AEC5 #xC5C56D39
#x1D1DE599 #x7676A4CD #x3E3EDCAD #xCBCB6731 #xB6B6478B #xEFEF5B01
#x12121E18 #x6060C523 #x6A6AB0DD #x4D4DF61F #xCECEE94E #xDEDE7C2D
#x55559DF9 #x7E7E5A48 #x2121B24F #x03037AF2 #xA0A02665 #x5E5E198E
#x5A5A6678 #x65654B5C #x62624E58 #xFDFD4519 #x0606F48D #x404086E5
#xF2F2BE98 #x3333AC57 #x17179067 #x05058E7F #xE8E85E05 #x4F4F7D64
#x89896AAF #x10109563 #x74742FB6 #x0A0A75FE #x5C5C92F5 #x9B9B74B7
#x2D2D333C #x3030D6A5 #x2E2E49CE #x494989E9 #x46467268 #x77775544
#xA8A8D8E0 #x9696044D #x2828BD43 #xA9A92969 #xD9D97929 #x8686912E
#xD1D187AC #xF4F44A15 #x8D8D1559 #xD6D682A8 #xB9B9BC0A #x42420D9E
#xF6F6C16E #x2F2FB847 #xDDDD06DF #x23233934 #xCCCC6235 #xF1F1C46A
#xC1C112CF #x8585EBDC #x8F8F9E22 #x7171A1C9 #x9090F0C0 #xAAAA539B
#x0101F189 #x8B8BE1D4 #x4E4E8CED #x8E8E6FAB #xABABA212 #x6F6F3EA2
#xE6E6540D #xDBDBF252 #x92927BBB #xB7B7B602 #x6969CA2F #x3939D9A9
#xD3D30CD7 #xA7A72361 #xA2A2AD1E #xC3C399B4 #x6C6C4450 #x07070504
#x04047FF6 #x272746C2 #xACACA716 #xD0D07625 #x50501386 #xDCDCF756
#x84841A55 #xE1E15109 #x7A7A25BE #x1313EF91))

(defconst +twofish-mds1+
#32@(#xA9D93939 #x67901717 #xB3719C9C #xE8D2A6A6 #x04050707 #xFD985252
#xA3658080 #x76DFE4E4 #x9A084545 #x92024B4B #x80A0E0E0 #x78665A5A
#xE4DDAFAF #xDDB06A6A #xD1BF6363 #x38362A2A #x0D54E6E6 #xC6432020
#x3562CCCC #x98BEF2F2 #x181E1212 #xF724EBEB #xECD7A1A1 #x6C774141
#x43BD2828 #x7532BCBC #x37D47B7B #x269B8888 #xFA700D0D #x13F94444
#x94B1FBFB #x485A7E7E #xF27A0303 #xD0E48C8C #x8B47B6B6 #x303C2424
#x84A5E7E7 #x54416B6B #xDF06DDDD #x23C56060 #x1945FDFD #x5BA33A3A
#x3D68C2C2 #x59158D8D #xF321ECEC #xAE316666 #xA23E6F6F #x82165757
#x63951010 #x015BEFEF #x834DB8B8 #x2E918686 #xD9B56D6D #x511F8383
#x9B53AAAA #x7C635D5D #xA63B6868 #xEB3FFEFE #xA5D63030 #xBE257A7A
#x16A7ACAC #x0C0F0909 #xE335F0F0 #x6123A7A7 #xC0F09090 #x8CAFE9E9
#x3A809D9D #xF5925C5C #x73810C0C #x2C273131 #x2576D0D0 #x0BE75656
#xBB7B9292 #x4EE9CECE #x89F10101 #x6B9F1E1E #x53A93434 #x6AC4F1F1
#xB499C3C3 #xF1975B5B #xE1834747 #xE66B1818 #xBDC82222 #x450E9898
#xE26E1F1F #xF4C9B3B3 #xB62F7474 #x66CBF8F8 #xCCFF9999 #x95EA1414
#x03ED5858 #x56F7DCDC #xD4E18B8B #x1C1B1515 #x1EADA2A2 #xD70CD3D3
#xFB2BE2E2 #xC31DC8C8 #x8E195E5E #xB5C22C2C #xE9894949 #xCF12C1C1
#xBF7E9595 #xBA207D7D #xEA641111 #x77840B0B #x396DC5C5 #xAF6A8989
#x33D17C7C #xC9A17171 #x62CEFFFF #x7137BBBB #x81FB0F0F #x793DB5B5
#x0951E1E1 #xADDC3E3E #x242D3F3F #xCDA47676 #xF99D5555 #xD8EE8282
#xE5864040 #xC5AE7878 #xB9CD2525 #x4D049696 #x44557777 #x080A0E0E
#x86135050 #xE730F7F7 #xA1D33737 #x1D40FAFA #xAA346161 #xED8C4E4E
#x06B3B0B0 #x706C5454 #xB22A7373 #xD2523B3B #x410B9F9F #x7B8B0202
#xA088D8D8 #x114FF3F3 #x3167CBCB #xC2462727 #x27C06767 #x90B4FCFC
#x20283838 #xF67F0404 #x60784848 #xFF2EE5E5 #x96074C4C #x5C4B6565
#xB1C72B2B #xAB6F8E8E #x9E0D4242 #x9CBBF5F5 #x52F2DBDB #x1BF34A4A
#x5FA63D3D #x9359A4A4 #x0ABCB9B9 #xEF3AF9F9 #x91EF1313 #x85FE0808
#x49019191 #xEE611616 #x2D7CDEDE #x4FB22121 #x8F42B1B1 #x3BDB7272
#x47B82F2F #x8748BFBF #x6D2CAEAE #x46E3C0C0 #xD6573C3C #x3E859A9A
#x6929A9A9 #x647D4F4F #x2A948181 #xCE492E2E #xCB17C6C6 #x2FCA6969
#xFCC3BDBD #x975CA3A3 #x055EE8E8 #x7AD0EDED #xAC87D1D1 #x7F8E0505
#xD5BA6464 #x1AA8A5A5 #x4BB72626 #x0EB9BEBE #xA7608787 #x5AF8D5D5
#x28223636 #x14111B1B #x3FDE7575 #x2979D9D9 #x88AAEEEE #x3C332D2D
#x4C5F7979 #x02B6B7B7 #xB896CACA #xDA583535 #xB09CC4C4 #x17FC4343
#x551A8484 #x1FF64D4D #x8A1C5959 #x7D38B2B2 #x57AC3333 #xC718CFCF
#x8DF40606 #x74695353 #xB7749B9B #xC4F59797 #x9F56ADAD #x72DAE3E3
#x7ED5EAEA #x154AF4F4 #x229E8F8F #x12A2ABAB #x584E6262 #x07E85F5F
#x99E51D1D #x34392323 #x6EC1F6F6 #x50446C6C #xDE5D3232 #x68724646
#x6526A0A0 #xBC93CDCD #xDB03DADA #xF8C6BABA #xC8FA9E9E #xA882D6D6
#x2BCF6E6E #x40507070 #xDCEB8585 #xFE750A0A #x328A9393 #xA48DDFDF
#xCA4C2929 #x10141C1C #x2173D7D7 #xF0CCB4B4 #xD309D4D4 #x5D108A8A
#x0FE25151 #x00000000 #x6F9A1919 #x9DE01A1A #x368F9494 #x42E6C7C7
#x4AECC9C9 #x5EFDD2D2 #xC1AB7F7F #xE0D8A8A8))

(defconst +twofish-mds2+
#32@(#xBC75BC32 #xECF3EC21 #x20C62043 #xB3F4B3C9 #xDADBDA03 #x027B028B
#xE2FBE22B #x9EC89EFA #xC94AC9EC #xD4D3D409 #x18E6186B #x1E6B1E9F
#x9845980E #xB27DB238 #xA6E8A6D2 #x264B26B7 #x3CD63C57 #x9332938A
#x82D882EE #x52FD5298 #x7B377BD4 #xBB71BB37 #x5BF15B97 #x47E14783
#x2430243C #x510F51E2 #xBAF8BAC6 #x4A1B4AF3 #xBF87BF48 #x0DFA0D70
#xB006B0B3 #x753F75DE #xD25ED2FD #x7DBA7D20 #x66AE6631 #x3A5B3AA3
#x598A591C #x00000000 #xCDBCCD93 #x1A9D1AE0 #xAE6DAE2C #x7FC17FAB
#x2BB12BC7 #xBE0EBEB9 #xE080E0A0 #x8A5D8A10 #x3BD23B52 #x64D564BA
#xD8A0D888 #xE784E7A5 #x5F075FE8 #x1B141B11 #x2CB52CC2 #xFC90FCB4
#x312C3127 #x80A38065 #x73B2732A #x0C730C81 #x794C795F #x6B546B41
#x4B924B02 #x53745369 #x9436948F #x8351831F #x2A382A36 #xC4B0C49C
#x22BD22C8 #xD55AD5F8 #xBDFCBDC3 #x48604878 #xFF62FFCE #x4C964C07
#x416C4177 #xC742C7E6 #xEBF7EB24 #x1C101C14 #x5D7C5D63 #x36283622
#x672767C0 #xE98CE9AF #x441344F9 #x149514EA #xF59CF5BB #xCFC7CF18
#x3F243F2D #xC046C0E3 #x723B72DB #x5470546C #x29CA294C #xF0E3F035
#x088508FE #xC6CBC617 #xF311F34F #x8CD08CE4 #xA493A459 #xCAB8CA96
#x68A6683B #xB883B84D #x38203828 #xE5FFE52E #xAD9FAD56 #x0B770B84
#xC8C3C81D #x99CC99FF #x580358ED #x196F199A #x0E080E0A #x95BF957E
#x70407050 #xF7E7F730 #x6E2B6ECF #x1FE21F6E #xB579B53D #x090C090F
#x61AA6134 #x57825716 #x9F419F0B #x9D3A9D80 #x11EA1164 #x25B925CD
#xAFE4AFDD #x459A4508 #xDFA4DF8D #xA397A35C #xEA7EEAD5 #x35DA3558
#xED7AEDD0 #x431743FC #xF866F8CB #xFB94FBB1 #x37A137D3 #xFA1DFA40
#xC23DC268 #xB4F0B4CC #x32DE325D #x9CB39C71 #x560B56E7 #xE372E3DA
#x87A78760 #x151C151B #xF9EFF93A #x63D163BF #x345334A9 #x9A3E9A85
#xB18FB142 #x7C337CD1 #x8826889B #x3D5F3DA6 #xA1ECA1D7 #xE476E4DF
#x812A8194 #x91499101 #x0F810FFB #xEE88EEAA #x16EE1661 #xD721D773
#x97C497F5 #xA51AA5A8 #xFEEBFE3F #x6DD96DB5 #x78C578AE #xC539C56D
#x1D991DE5 #x76CD76A4 #x3EAD3EDC #xCB31CB67 #xB68BB647 #xEF01EF5B
#x1218121E #x602360C5 #x6ADD6AB0 #x4D1F4DF6 #xCE4ECEE9 #xDE2DDE7C
#x55F9559D #x7E487E5A #x214F21B2 #x03F2037A #xA065A026 #x5E8E5E19
#x5A785A66 #x655C654B #x6258624E #xFD19FD45 #x068D06F4 #x40E54086
#xF298F2BE #x335733AC #x17671790 #x057F058E #xE805E85E #x4F644F7D
#x89AF896A #x10631095 #x74B6742F #x0AFE0A75 #x5CF55C92 #x9BB79B74
#x2D3C2D33 #x30A530D6 #x2ECE2E49 #x49E94989 #x46684672 #x77447755
#xA8E0A8D8 #x964D9604 #x284328BD #xA969A929 #xD929D979 #x862E8691
#xD1ACD187 #xF415F44A #x8D598D15 #xD6A8D682 #xB90AB9BC #x429E420D
#xF66EF6C1 #x2F472FB8 #xDDDFDD06 #x23342339 #xCC35CC62 #xF16AF1C4
#xC1CFC112 #x85DC85EB #x8F228F9E #x71C971A1 #x90C090F0 #xAA9BAA53
#x018901F1 #x8BD48BE1 #x4EED4E8C #x8EAB8E6F #xAB12ABA2 #x6FA26F3E
#xE60DE654 #xDB52DBF2 #x92BB927B #xB702B7B6 #x692F69CA #x39A939D9
#xD3D7D30C #xA761A723 #xA21EA2AD #xC3B4C399 #x6C506C44 #x07040705
#x04F6047F #x27C22746 #xAC16ACA7 #xD025D076 #x50865013 #xDC56DCF7
#x8455841A #xE109E151 #x7ABE7A25 #x139113EF))

(defconst +twofish-mds3+
#32@(#xD939A9D9 #x90176790 #x719CB371 #xD2A6E8D2 #x05070405 #x9852FD98
#x6580A365 #xDFE476DF #x08459A08 #x024B9202 #xA0E080A0 #x665A7866
#xDDAFE4DD #xB06ADDB0 #xBF63D1BF #x362A3836 #x54E60D54 #x4320C643
#x62CC3562 #xBEF298BE #x1E12181E #x24EBF724 #xD7A1ECD7 #x77416C77
#xBD2843BD #x32BC7532 #xD47B37D4 #x9B88269B #x700DFA70 #xF94413F9
#xB1FB94B1 #x5A7E485A #x7A03F27A #xE48CD0E4 #x47B68B47 #x3C24303C
#xA5E784A5 #x416B5441 #x06DDDF06 #xC56023C5 #x45FD1945 #xA33A5BA3
#x68C23D68 #x158D5915 #x21ECF321 #x3166AE31 #x3E6FA23E #x16578216
#x95106395 #x5BEF015B #x4DB8834D #x91862E91 #xB56DD9B5 #x1F83511F
#x53AA9B53 #x635D7C63 #x3B68A63B #x3FFEEB3F #xD630A5D6 #x257ABE25
#xA7AC16A7 #x0F090C0F #x35F0E335 #x23A76123 #xF090C0F0 #xAFE98CAF
#x809D3A80 #x925CF592 #x810C7381 #x27312C27 #x76D02576 #xE7560BE7
#x7B92BB7B #xE9CE4EE9 #xF10189F1 #x9F1E6B9F #xA93453A9 #xC4F16AC4
#x99C3B499 #x975BF197 #x8347E183 #x6B18E66B #xC822BDC8 #x0E98450E
#x6E1FE26E #xC9B3F4C9 #x2F74B62F #xCBF866CB #xFF99CCFF #xEA1495EA
#xED5803ED #xF7DC56F7 #xE18BD4E1 #x1B151C1B #xADA21EAD #x0CD3D70C
#x2BE2FB2B #x1DC8C31D #x195E8E19 #xC22CB5C2 #x8949E989 #x12C1CF12
#x7E95BF7E #x207DBA20 #x6411EA64 #x840B7784 #x6DC5396D #x6A89AF6A
#xD17C33D1 #xA171C9A1 #xCEFF62CE #x37BB7137 #xFB0F81FB #x3DB5793D
#x51E10951 #xDC3EADDC #x2D3F242D #xA476CDA4 #x9D55F99D #xEE82D8EE
#x8640E586 #xAE78C5AE #xCD25B9CD #x04964D04 #x55774455 #x0A0E080A
#x13508613 #x30F7E730 #xD337A1D3 #x40FA1D40 #x3461AA34 #x8C4EED8C
#xB3B006B3 #x6C54706C #x2A73B22A #x523BD252 #x0B9F410B #x8B027B8B
#x88D8A088 #x4FF3114F #x67CB3167 #x4627C246 #xC06727C0 #xB4FC90B4
#x28382028 #x7F04F67F #x78486078 #x2EE5FF2E #x074C9607 #x4B655C4B
#xC72BB1C7 #x6F8EAB6F #x0D429E0D #xBBF59CBB #xF2DB52F2 #xF34A1BF3
#xA63D5FA6 #x59A49359 #xBCB90ABC #x3AF9EF3A #xEF1391EF #xFE0885FE
#x01914901 #x6116EE61 #x7CDE2D7C #xB2214FB2 #x42B18F42 #xDB723BDB
#xB82F47B8 #x48BF8748 #x2CAE6D2C #xE3C046E3 #x573CD657 #x859A3E85
#x29A96929 #x7D4F647D #x94812A94 #x492ECE49 #x17C6CB17 #xCA692FCA
#xC3BDFCC3 #x5CA3975C #x5EE8055E #xD0ED7AD0 #x87D1AC87 #x8E057F8E
#xBA64D5BA #xA8A51AA8 #xB7264BB7 #xB9BE0EB9 #x6087A760 #xF8D55AF8
#x22362822 #x111B1411 #xDE753FDE #x79D92979 #xAAEE88AA #x332D3C33
#x5F794C5F #xB6B702B6 #x96CAB896 #x5835DA58 #x9CC4B09C #xFC4317FC
#x1A84551A #xF64D1FF6 #x1C598A1C #x38B27D38 #xAC3357AC #x18CFC718
#xF4068DF4 #x69537469 #x749BB774 #xF597C4F5 #x56AD9F56 #xDAE372DA
#xD5EA7ED5 #x4AF4154A #x9E8F229E #xA2AB12A2 #x4E62584E #xE85F07E8
#xE51D99E5 #x39233439 #xC1F66EC1 #x446C5044 #x5D32DE5D #x72466872
#x26A06526 #x93CDBC93 #x03DADB03 #xC6BAF8C6 #xFA9EC8FA #x82D6A882
#xCF6E2BCF #x50704050 #xEB85DCEB #x750AFE75 #x8A93328A #x8DDFA48D
#x4C29CA4C #x141C1014 #x73D72173 #xCCB4F0CC #x09D4D309 #x108A5D10
#xE2510FE2 #x00000000 #x9A196F9A #xE01A9DE0 #x8F94368F #xE6C742E6
#xECC94AEC #xFDD25EFD #xAB7FC1AB #xD8A8E0D8))


;;; the actual implementation of Twofish

(deftype twofish-s-boxes () '(simple-array (unsigned-byte 32) (1024)))
(deftype twofish-round-keys () '(simple-array (unsigned-byte 32) (40)))

(defclass twofish (cipher 16-byte-block-mixin)
  ((round-keys :accessor round-keys :type twofish-round-keys)
   (s-boxes :accessor s-boxes :type twofish-s-boxes)))

(defun reed-solomon-multiply (box box-offset key rs0 rs1 rs2 rs3)
  (declare (type (simple-octet-vector 16) box))
  (declare (type (integer 0 12) box-offset))
  (unless (zerop key)
    (let ((temp (aref +twofish-poly-to-exp+ (1- key))))
      ;; Lispworks doesn't seem to like doing this with a straight
      ;; MACROLET and no #., so we go ahead and build everything at
      ;; read-time.
      #.(flet ((mod-box-element (index)
                 (let ((rs-sym (symbolicate '#:rs index)))
                   `(setf (aref box (+ box-offset ,index))
                          (logxor (aref box (+ box-offset ,index))
                                  (aref +twofish-exp-to-poly+
                                        (mod (+ temp (aref +twofish-poly-to-exp+
                                                           (1- ,rs-sym)))
                                             255)))))))
          `(progn
             ,(mod-box-element 0)
             ,(mod-box-element 1)
             ,(mod-box-element 2)
             ,(mod-box-element 3)))))
  (values))

(defun twofish-key-schedule (key)
  (declare (type (simple-array (unsigned-byte 8) (*)) key))
  (let ((rs-box (make-array 16 :element-type '(unsigned-byte 8)
                            :initial-element 0))
        (round-keys (make-array 40 :element-type '(unsigned-byte 32)))
        (s-boxes (make-array 1024 :element-type '(unsigned-byte 32))))
    (declare (type (simple-octet-vector 16) rs-box))
    (declare (dynamic-extent rs-box))
    ;; fill the rs-box
    (dotimes (i (length key))
      (reed-solomon-multiply rs-box (* 4 (truncate i 8))
                             (aref key i)
                             (aref +twofish-rs+ (mod (* 4 i) 32))
                             (aref +twofish-rs+ (mod (+ (* 4 i) 1) 32))
                             (aref +twofish-rs+ (mod (+ (* 4 i) 2) 32))
                             (aref +twofish-rs+ (mod (+ (* 4 i) 3) 32))))
    (case (length key)
      (16 (twofish-schedule-16-byte-key round-keys s-boxes key rs-box))
      (24 (twofish-schedule-24-byte-key round-keys s-boxes key rs-box))
      (32 (twofish-schedule-32-byte-key round-keys s-boxes key rs-box)))))

(macrolet ((s-box (s-boxes which index)
             `(aref ,s-boxes (+ (* 256 ,which) ,index)))
           (s-box-0 (s-boxes index) `(s-box ,s-boxes 0 ,index))
           (s-box-1 (s-boxes index) `(s-box ,s-boxes 1 ,index))
           (s-box-2 (s-boxes index) `(s-box ,s-boxes 2 ,index))
           (s-box-3 (s-boxes index) `(s-box ,s-boxes 3 ,index)))
  (defun twofish-schedule-16-byte-key (round-keys s-boxes key box)
    (declare (type twofish-round-keys round-keys)
             (type twofish-s-boxes s-boxes)
             (type (simple-octet-vector 16) key box))
    (macrolet ((q-frob (i1 i2 d1 d2)
                 (let ((q0 (symbolicate '#:+twofish-q (ldb (byte 1 1) i1) '#:+))
                       (q1 (symbolicate '#:+twofish-q (ldb (byte 1 0) i1) '#:+)))
                   `(logxor (aref ,q0 (logxor (aref ,q1 ,i2) ,d1)) ,d2))))
      (dotimes (i 256)
        (setf (s-box-0 s-boxes i) (aref +twofish-mds0+
                                        (q-frob 0 i (aref box 0) (aref box 4)))
              (s-box-1 s-boxes i) (aref +twofish-mds1+
                                        (q-frob 1 i (aref box 1) (aref box 5)))
              (s-box-2 s-boxes i) (aref +twofish-mds2+
                                        (q-frob 2 i (aref box 2) (aref box 6)))
              (s-box-3 s-boxes i) (aref +twofish-mds3+
                                        (q-frob 3 i (aref box 3) (aref box 7)))))
      (loop for i from 0 below 40 by 2 do
            (let ((x (logxor (aref +twofish-mds0+
                                   (q-frob 0 i (aref key 8) (aref key 0)))
                             (aref +twofish-mds1+
                                   (q-frob 1 i (aref key 9) (aref key 1)))
                             (aref +twofish-mds2+
                                   (q-frob 2 i (aref key 10) (aref key 2)))
                             (aref +twofish-mds3+
                                   (q-frob 3 i (aref key 11) (aref key 3)))))
                  (y (logxor (aref +twofish-mds0+
                                   (q-frob 0 (1+ i) (aref key 12) (aref key 4)))
                             (aref +twofish-mds1+
                                   (q-frob 1 (1+ i) (aref key 13) (aref key 5)))
                             (aref +twofish-mds2+
                                   (q-frob 2 (1+ i) (aref key 14) (aref key 6)))
                             (aref +twofish-mds3+
                                   (q-frob 3 (1+ i) (aref key 15) (aref key 7))))))
              (declare (type (unsigned-byte 32) x y))
              (setf y (rol32 y 8))
              (setf x (mod32+ x y))
              (setf y (mod32+ y x))
              (setf (aref round-keys i) x
                    (aref round-keys (1+ i)) (rol32 y 9)))
            finally (return (values round-keys s-boxes)))))

  (defun twofish-schedule-24-byte-key (round-keys s-boxes key box)
    (declare (type twofish-round-keys round-keys)
             (type twofish-s-boxes s-boxes)
             (type (simple-octet-vector 24) key)
             (type (simple-octet-vector 16) box))
    (macrolet ((q-frob (i1 i2 d1 d2 d3)
                 (let ((q0 (symbolicate '#:+twofish-q (ldb (byte 1 2) i1) '#:+))
                       (q1 (symbolicate '#:+twofish-q (ldb (byte 1 1) i1) '#:+))
                       (q2 (symbolicate '#:+twofish-q (ldb (byte 1 0) i1) '#:+)))
                   `(logxor (aref ,q0 (logxor (aref ,q1 (logxor (aref ,q2 ,i2)
                                                                ,d1))
                                              ,d2))
                            ,d3))))
      (dotimes (i 256)
        (setf (s-box-0 s-boxes i) (aref +twofish-mds0+
                                        (q-frob 1 i (aref box 0) (aref box 4) (aref box 8)))
              (s-box-1 s-boxes i) (aref +twofish-mds1+
                                        (q-frob 3 i (aref box 1) (aref box 5) (aref box 9)))
              (s-box-2 s-boxes i) (aref +twofish-mds2+
                                        (q-frob 4 i (aref box 2) (aref box 6) (aref box 10)))
              (s-box-3 s-boxes i) (aref +twofish-mds3+
                                        (q-frob 6 i (aref box 3) (aref box 7) (aref box 11)))))
      (loop for i from 0 below 40 by 2 do
            (let ((x (logxor (aref +twofish-mds0+
                                   (q-frob 1 i (aref key 16) (aref key 8) (aref key 0)))
                             (aref +twofish-mds1+
                                   (q-frob 3 i (aref key 17) (aref key 9) (aref key 1)))
                             (aref +twofish-mds2+
                                   (q-frob 4 i (aref key 18) (aref key 10) (aref key 2)))
                             (aref +twofish-mds3+
                                   (q-frob 6 i (aref key 19) (aref key 11) (aref key 3)))))
                  (y (logxor (aref +twofish-mds0+
                                   (q-frob 1 (1+ i) (aref key 20) (aref key 12) (aref key 4)))
                             (aref +twofish-mds1+
                                   (q-frob 3 (1+ i) (aref key 21) (aref key 13) (aref key 5)))
                             (aref +twofish-mds2+
                                   (q-frob 4 (1+ i) (aref key 22) (aref key 14) (aref key 6)))
                             (aref +twofish-mds3+
                                   (q-frob 6 (1+ i) (aref key 23) (aref key 15) (aref key 7))))))
              (declare (type (unsigned-byte 32) x y))
              (setf y (rol32 y 8))
              (setf x (mod32+ x y))
              (setf y (mod32+ y x))
              (setf (aref round-keys i) x
                    (aref round-keys (1+ i)) (rol32 y 9)))
            finally (return (values round-keys s-boxes)))))

  (defun twofish-schedule-32-byte-key (round-keys s-boxes key box)
    (declare (type twofish-round-keys round-keys)
             (type twofish-s-boxes s-boxes)
             (type (simple-octet-vector 32) key)
             (type (simple-octet-vector 16) box))
    (macrolet ((q-frob (i1 i2 d1 d2 d3 d4)
                 (let ((q0 (symbolicate '#:+twofish-q (ldb (byte 1 3) i1) '#:+))
                       (q1 (symbolicate '#:+twofish-q (ldb (byte 1 2) i1) '#:+))
                       (q2 (symbolicate '#:+twofish-q (ldb (byte 1 1) i1) '#:+))
                       (q3 (symbolicate '#:+twofish-q (ldb (byte 1 0) i1) '#:+)))
                   `(logxor (aref ,q0 (logxor (aref ,q1 (logxor (aref ,q2 (logxor (aref ,q3 ,i2) ,d1)) ,d2)) ,d3)) ,d4))))
      (dotimes (i 256)
        (setf (s-box-0 s-boxes i) (aref +twofish-mds0+
                                        (q-frob #b0011 i (aref box 0) (aref box 4) (aref box 8) (aref box 12)))
              (s-box-1 s-boxes i) (aref +twofish-mds1+
                                        (q-frob #b0110 i (aref box 1) (aref box 5) (aref box 9) (aref box 13)))
              (s-box-2 s-boxes i) (aref +twofish-mds2+
                                        (q-frob #b1000 i (aref box 2) (aref box 6) (aref box 10) (aref box 14)))
              (s-box-3 s-boxes i) (aref +twofish-mds3+
                                        (q-frob #b1101 i (aref box 3) (aref box 7) (aref box 11) (aref box 15)))))
      (loop for i from 0 below 40 by 2 do
            (let ((x (logxor (aref +twofish-mds0+
                                   (q-frob #b0011 i (aref key 24) (aref key 16) (aref key 8) (aref key 0)))
                             (aref +twofish-mds1+
                                   (q-frob #b0110 i (aref key 25) (aref key 17) (aref key 9) (aref key 1)))
                             (aref +twofish-mds2+
                                   (q-frob #b1000 i (aref key 26) (aref key 18) (aref key 10) (aref key 2)))
                             (aref +twofish-mds3+
                                   (q-frob #b1101 i (aref key 27) (aref key 19) (aref key 11) (aref key 3)))))
                  (y (logxor (aref +twofish-mds0+
                                   (q-frob #b0011 (1+ i) (aref key 28) (aref key 20) (aref key 12) (aref key 4)))
                             (aref +twofish-mds1+
                                   (q-frob #b0110 (1+ i) (aref key 29) (aref key 21) (aref key 13) (aref key 5)))
                             (aref +twofish-mds2+
                                   (q-frob #b1000 (1+ i) (aref key 30) (aref key 22) (aref key 14) (aref key 6)))
                             (aref +twofish-mds3+
                                   (q-frob #b1101 (1+ i) (aref key 31) (aref key 23) (aref key 15) (aref key 7))))))
              (declare (type (unsigned-byte 32) x y))
              (setf y (rol32 y 8))
              (setf x (mod32+ x y))
              (setf y (mod32+ y x))
              (setf (aref round-keys i) x
                    (aref round-keys (1+ i)) (rol32 y 9)))
            finally (return (values round-keys s-boxes)))))

  (define-block-encryptor twofish 16
    (let ((round-keys (round-keys context))
          (s-boxes (s-boxes context)))
      (declare (type twofish-round-keys round-keys))
      (declare (type twofish-s-boxes s-boxes))
      (macrolet ((encrypt-round (a b c d round)
                   `(let ((x (logxor (s-box-0 s-boxes (first-byte ,a))
                                     (s-box-1 s-boxes (second-byte ,a))
                                     (s-box-2 s-boxes (third-byte ,a))
                                     (s-box-3 s-boxes (fourth-byte ,a))))
                          (y (logxor (s-box-0 s-boxes (fourth-byte ,b))
                                     (s-box-1 s-boxes (first-byte ,b))
                                     (s-box-2 s-boxes (second-byte ,b))
                                     (s-box-3 s-boxes (third-byte ,b)))))
                      (declare (type (unsigned-byte 32) x y))
                      (setf x (mod32+ x y))
                      (setf y (mod32+ y (mod32+ x (aref round-keys (+ (* ,round 2) 9)))))
                      (setf x (mod32+ x (aref round-keys (+ (* ,round 2) 8))))
                      (setf ,c (rol32 (logxor ,c x) 31)
                            ,d (logxor (rol32 ,d 1) y)))))
        (with-words ((a b c d) plaintext plaintext-start :big-endian nil)
          (setf a (logxor a (aref round-keys 0))
                b (logxor b (aref round-keys 1))
                c (logxor c (aref round-keys 2))
                d (logxor d (aref round-keys 3)))
          #.(loop for i from 0 below 16
                  if (evenp i)
                  collect `(encrypt-round a b c d ,i) into rounds
                  else
                  collect `(encrypt-round c d a b ,i) into rounds
                  finally (return `(progn ,@rounds)))
          (setf c (logxor c (aref round-keys 4))
                d (logxor d (aref round-keys 5))
                a (logxor a (aref round-keys 6))
                b (logxor b (aref round-keys 7)))
          (store-words ciphertext ciphertext-start c d a b)
          (values)))))

  (define-block-decryptor twofish 16
    (let ((round-keys (round-keys context))
          (s-boxes (s-boxes context)))
      (declare (type twofish-round-keys round-keys))
      (declare (type twofish-s-boxes s-boxes))
      (macrolet ((decrypt-round (a b c d round)
                   `(let ((x (logxor (s-box-0 s-boxes (first-byte ,a))
                                     (s-box-1 s-boxes (second-byte ,a))
                                     (s-box-2 s-boxes (third-byte ,a))
                                     (s-box-3 s-boxes (fourth-byte ,a))))
                          (y (logxor (s-box-0 s-boxes (fourth-byte ,b))
                                     (s-box-1 s-boxes (first-byte ,b))
                                     (s-box-2 s-boxes (second-byte ,b))
                                     (s-box-3 s-boxes (third-byte ,b)))))
                      (declare (type (unsigned-byte 32) x y))
                      (setf x (mod32+ x y))
                      (setf y (mod32+ y (mod32+ x (aref round-keys (+ (* ,round 2) 9)))))
                      (setf x (mod32+ x (aref round-keys (+ (* ,round 2) 8))))
                      (setf ,c (logxor (rol32 ,c 1) x)
                            ,d (rol32 (logxor ,d y) 31)))))
        (with-words ((c d a b) ciphertext ciphertext-start :big-endian nil)
          (setf c (logxor c (aref round-keys 4))
                d (logxor d (aref round-keys 5))
                a (logxor a (aref round-keys 6))
                b (logxor b (aref round-keys 7)))
          #.(loop for i from 15 downto 0
                  if (evenp i)
                  collect `(decrypt-round a b c d ,i) into rounds
                  else
                  collect `(decrypt-round c d a b ,i) into rounds
                  finally (return `(progn ,@rounds)))
          (setf a (logxor a (aref round-keys 0))
                b (logxor b (aref round-keys 1))
                c (logxor c (aref round-keys 2))
                d (logxor d (aref round-keys 3)))
          (store-words plaintext plaintext-start a b c d)
          (values)))))
  ) ; MACROLET

(defmethod schedule-key ((cipher twofish) key)
  (multiple-value-bind (round-keys s-boxes) (twofish-key-schedule key)
    (setf (round-keys cipher) round-keys
          (s-boxes cipher) s-boxes)
    cipher))

(defcipher twofish
  (:encrypt-function twofish-encrypt-block)
  (:decrypt-function twofish-decrypt-block)
  (:block-length 16)
  (:key-length (:fixed 16 24 32)))
