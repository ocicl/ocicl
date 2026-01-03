;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; cast5.lisp -- implementation of rfc2144 CAST5 algorithm

(in-package :crypto)
(in-ironclad-readtable)


;;; s-boxes

(declaim (type (simple-array (unsigned-byte 32) (256))
               +cast5-sbox0+ +cast5-sbox1+ +cast5-sbox2+ +cast5-sbox3+
               +cast5-sbox4+ +cast5-sbox5+ +cast5-sbox6+ +cast5-sbox7+))

(defconst +cast5-sbox0+
#32@(#x30fb40d4 #x9fa0ff0b #x6beccd2f #x3f258c7a #x1e213f2f #x9c004dd3 #x6003e540 #xcf9fc949
#xbfd4af27 #x88bbbdb5 #xe2034090 #x98d09675 #x6e63a0e0 #x15c361d2 #xc2e7661d #x22d4ff8e
#x28683b6f #xc07fd059 #xff2379c8 #x775f50e2 #x43c340d3 #xdf2f8656 #x887ca41a #xa2d2bd2d
#xa1c9e0d6 #x346c4819 #x61b76d87 #x22540f2f #x2abe32e1 #xaa54166b #x22568e3a #xa2d341d0
#x66db40c8 #xa784392f #x004dff2f #x2db9d2de #x97943fac #x4a97c1d8 #x527644b7 #xb5f437a7
#xb82cbaef #xd751d159 #x6ff7f0ed #x5a097a1f #x827b68d0 #x90ecf52e #x22b0c054 #xbc8e5935
#x4b6d2f7f #x50bb64a2 #xd2664910 #xbee5812d #xb7332290 #xe93b159f #xb48ee411 #x4bff345d
#xfd45c240 #xad31973f #xc4f6d02e #x55fc8165 #xd5b1caad #xa1ac2dae #xa2d4b76d #xc19b0c50
#x882240f2 #x0c6e4f38 #xa4e4bfd7 #x4f5ba272 #x564c1d2f #xc59c5319 #xb949e354 #xb04669fe
#xb1b6ab8a #xc71358dd #x6385c545 #x110f935d #x57538ad5 #x6a390493 #xe63d37e0 #x2a54f6b3
#x3a787d5f #x6276a0b5 #x19a6fcdf #x7a42206a #x29f9d4d5 #xf61b1891 #xbb72275e #xaa508167
#x38901091 #xc6b505eb #x84c7cb8c #x2ad75a0f #x874a1427 #xa2d1936b #x2ad286af #xaa56d291
#xd7894360 #x425c750d #x93b39e26 #x187184c9 #x6c00b32d #x73e2bb14 #xa0bebc3c #x54623779
#x64459eab #x3f328b82 #x7718cf82 #x59a2cea6 #x04ee002e #x89fe78e6 #x3fab0950 #x325ff6c2
#x81383f05 #x6963c5c8 #x76cb5ad6 #xd49974c9 #xca180dcf #x380782d5 #xc7fa5cf6 #x8ac31511
#x35e79e13 #x47da91d0 #xf40f9086 #xa7e2419e #x31366241 #x051ef495 #xaa573b04 #x4a805d8d
#x548300d0 #x00322a3c #xbf64cddf #xba57a68e #x75c6372b #x50afd341 #xa7c13275 #x915a0bf5
#x6b54bfab #x2b0b1426 #xab4cc9d7 #x449ccd82 #xf7fbf265 #xab85c5f3 #x1b55db94 #xaad4e324
#xcfa4bd3f #x2deaa3e2 #x9e204d02 #xc8bd25ac #xeadf55b3 #xd5bd9e98 #xe31231b2 #x2ad5ad6c
#x954329de #xadbe4528 #xd8710f69 #xaa51c90f #xaa786bf6 #x22513f1e #xaa51a79b #x2ad344cc
#x7b5a41f0 #xd37cfbad #x1b069505 #x41ece491 #xb4c332e6 #x032268d4 #xc9600acc #xce387e6d
#xbf6bb16c #x6a70fb78 #x0d03d9c9 #xd4df39de #xe01063da #x4736f464 #x5ad328d8 #xb347cc96
#x75bb0fc3 #x98511bfb #x4ffbcc35 #xb58bcf6a #xe11f0abc #xbfc5fe4a #xa70aec10 #xac39570a
#x3f04442f #x6188b153 #xe0397a2e #x5727cb79 #x9ceb418f #x1cacd68d #x2ad37c96 #x0175cb9d
#xc69dff09 #xc75b65f0 #xd9db40d8 #xec0e7779 #x4744ead4 #xb11c3274 #xdd24cb9e #x7e1c54bd
#xf01144f9 #xd2240eb1 #x9675b3fd #xa3ac3755 #xd47c27af #x51c85f4d #x56907596 #xa5bb15e6
#x580304f0 #xca042cf1 #x011a37ea #x8dbfaadb #x35ba3e4a #x3526ffa0 #xc37b4d09 #xbc306ed9
#x98a52666 #x5648f725 #xff5e569d #x0ced63d0 #x7c63b2cf #x700b45e1 #xd5ea50f1 #x85a92872
#xaf1fbda7 #xd4234870 #xa7870bf3 #x2d3b4d79 #x42e04198 #x0cd0ede7 #x26470db8 #xf881814c
#x474d6ad7 #x7c0c5e5c #xd1231959 #x381b7298 #xf5d2f4db #xab838653 #x6e2f1e23 #x83719c9e
#xbd91e046 #x9a56456e #xdc39200c #x20c8c571 #x962bda1c #xe1e696ff #xb141ab08 #x7cca89b9
#x1a69e783 #x02cc4843 #xa2f7c579 #x429ef47d #x427b169c #x5ac9f049 #xdd8f0f00 #x5c8165bf))

(defconst +cast5-sbox1+
#32@(#x1f201094 #xef0ba75b #x69e3cf7e #x393f4380 #xfe61cf7a #xeec5207a #x55889c94 #x72fc0651
#xada7ef79 #x4e1d7235 #xd55a63ce #xde0436ba #x99c430ef #x5f0c0794 #x18dcdb7d #xa1d6eff3
#xa0b52f7b #x59e83605 #xee15b094 #xe9ffd909 #xdc440086 #xef944459 #xba83ccb3 #xe0c3cdfb
#xd1da4181 #x3b092ab1 #xf997f1c1 #xa5e6cf7b #x01420ddb #xe4e7ef5b #x25a1ff41 #xe180f806
#x1fc41080 #x179bee7a #xd37ac6a9 #xfe5830a4 #x98de8b7f #x77e83f4e #x79929269 #x24fa9f7b
#xe113c85b #xacc40083 #xd7503525 #xf7ea615f #x62143154 #x0d554b63 #x5d681121 #xc866c359
#x3d63cf73 #xcee234c0 #xd4d87e87 #x5c672b21 #x071f6181 #x39f7627f #x361e3084 #xe4eb573b
#x602f64a4 #xd63acd9c #x1bbc4635 #x9e81032d #x2701f50c #x99847ab4 #xa0e3df79 #xba6cf38c
#x10843094 #x2537a95e #xf46f6ffe #xa1ff3b1f #x208cfb6a #x8f458c74 #xd9e0a227 #x4ec73a34
#xfc884f69 #x3e4de8df #xef0e0088 #x3559648d #x8a45388c #x1d804366 #x721d9bfd #xa58684bb
#xe8256333 #x844e8212 #x128d8098 #xfed33fb4 #xce280ae1 #x27e19ba5 #xd5a6c252 #xe49754bd
#xc5d655dd #xeb667064 #x77840b4d #xa1b6a801 #x84db26a9 #xe0b56714 #x21f043b7 #xe5d05860
#x54f03084 #x066ff472 #xa31aa153 #xdadc4755 #xb5625dbf #x68561be6 #x83ca6b94 #x2d6ed23b
#xeccf01db #xa6d3d0ba #xb6803d5c #xaf77a709 #x33b4a34c #x397bc8d6 #x5ee22b95 #x5f0e5304
#x81ed6f61 #x20e74364 #xb45e1378 #xde18639b #x881ca122 #xb96726d1 #x8049a7e8 #x22b7da7b
#x5e552d25 #x5272d237 #x79d2951c #xc60d894c #x488cb402 #x1ba4fe5b #xa4b09f6b #x1ca815cf
#xa20c3005 #x8871df63 #xb9de2fcb #x0cc6c9e9 #x0beeff53 #xe3214517 #xb4542835 #x9f63293c
#xee41e729 #x6e1d2d7c #x50045286 #x1e6685f3 #xf33401c6 #x30a22c95 #x31a70850 #x60930f13
#x73f98417 #xa1269859 #xec645c44 #x52c877a9 #xcdff33a6 #xa02b1741 #x7cbad9a2 #x2180036f
#x50d99c08 #xcb3f4861 #xc26bd765 #x64a3f6ab #x80342676 #x25a75e7b #xe4e6d1fc #x20c710e6
#xcdf0b680 #x17844d3b #x31eef84d #x7e0824e4 #x2ccb49eb #x846a3bae #x8ff77888 #xee5d60f6
#x7af75673 #x2fdd5cdb #xa11631c1 #x30f66f43 #xb3faec54 #x157fd7fa #xef8579cc #xd152de58
#xdb2ffd5e #x8f32ce19 #x306af97a #x02f03ef8 #x99319ad5 #xc242fa0f #xa7e3ebb0 #xc68e4906
#xb8da230c #x80823028 #xdcdef3c8 #xd35fb171 #x088a1bc8 #xbec0c560 #x61a3c9e8 #xbca8f54d
#xc72feffa #x22822e99 #x82c570b4 #xd8d94e89 #x8b1c34bc #x301e16e6 #x273be979 #xb0ffeaa6
#x61d9b8c6 #x00b24869 #xb7ffce3f #x08dc283b #x43daf65a #xf7e19798 #x7619b72f #x8f1c9ba4
#xdc8637a0 #x16a7d3b1 #x9fc393b7 #xa7136eeb #xc6bcc63e #x1a513742 #xef6828bc #x520365d6
#x2d6a77ab #x3527ed4b #x821fd216 #x095c6e2e #xdb92f2fb #x5eea29cb #x145892f5 #x91584f7f
#x5483697b #x2667a8cc #x85196048 #x8c4bacea #x833860d4 #x0d23e0f9 #x6c387e8a #x0ae6d249
#xb284600c #xd835731d #xdcb1c647 #xac4c56ea #x3ebd81b3 #x230eabb0 #x6438bc87 #xf0b5b1fa
#x8f5ea2b3 #xfc184642 #x0a036b7a #x4fb089bd #x649da589 #xa345415e #x5c038323 #x3e5d3bb9
#x43d79572 #x7e6dd07c #x06dfdf1e #x6c6cc4ef #x7160a539 #x73bfbe70 #x83877605 #x4523ecf1))

(defconst +cast5-sbox2+
#32@(#x8defc240 #x25fa5d9f #xeb903dbf #xe810c907 #x47607fff #x369fe44b #x8c1fc644 #xaececa90
#xbeb1f9bf #xeefbcaea #xe8cf1950 #x51df07ae #x920e8806 #xf0ad0548 #xe13c8d83 #x927010d5
#x11107d9f #x07647db9 #xb2e3e4d4 #x3d4f285e #xb9afa820 #xfade82e0 #xa067268b #x8272792e
#x553fb2c0 #x489ae22b #xd4ef9794 #x125e3fbc #x21fffcee #x825b1bfd #x9255c5ed #x1257a240
#x4e1a8302 #xbae07fff #x528246e7 #x8e57140e #x3373f7bf #x8c9f8188 #xa6fc4ee8 #xc982b5a5
#xa8c01db7 #x579fc264 #x67094f31 #xf2bd3f5f #x40fff7c1 #x1fb78dfc #x8e6bd2c1 #x437be59b
#x99b03dbf #xb5dbc64b #x638dc0e6 #x55819d99 #xa197c81c #x4a012d6e #xc5884a28 #xccc36f71
#xb843c213 #x6c0743f1 #x8309893c #x0feddd5f #x2f7fe850 #xd7c07f7e #x02507fbf #x5afb9a04
#xa747d2d0 #x1651192e #xaf70bf3e #x58c31380 #x5f98302e #x727cc3c4 #x0a0fb402 #x0f7fef82
#x8c96fdad #x5d2c2aae #x8ee99a49 #x50da88b8 #x8427f4a0 #x1eac5790 #x796fb449 #x8252dc15
#xefbd7d9b #xa672597d #xada840d8 #x45f54504 #xfa5d7403 #xe83ec305 #x4f91751a #x925669c2
#x23efe941 #xa903f12e #x60270df2 #x0276e4b6 #x94fd6574 #x927985b2 #x8276dbcb #x02778176
#xf8af918d #x4e48f79e #x8f616ddf #xe29d840e #x842f7d83 #x340ce5c8 #x96bbb682 #x93b4b148
#xef303cab #x984faf28 #x779faf9b #x92dc560d #x224d1e20 #x8437aa88 #x7d29dc96 #x2756d3dc
#x8b907cee #xb51fd240 #xe7c07ce3 #xe566b4a1 #xc3e9615e #x3cf8209d #x6094d1e3 #xcd9ca341
#x5c76460e #x00ea983b #xd4d67881 #xfd47572c #xf76cedd9 #xbda8229c #x127dadaa #x438a074e
#x1f97c090 #x081bdb8a #x93a07ebe #xb938ca15 #x97b03cff #x3dc2c0f8 #x8d1ab2ec #x64380e51
#x68cc7bfb #xd90f2788 #x12490181 #x5de5ffd4 #xdd7ef86a #x76a2e214 #xb9a40368 #x925d958f
#x4b39fffa #xba39aee9 #xa4ffd30b #xfaf7933b #x6d498623 #x193cbcfa #x27627545 #x825cf47a
#x61bd8ba0 #xd11e42d1 #xcead04f4 #x127ea392 #x10428db7 #x8272a972 #x9270c4a8 #x127de50b
#x285ba1c8 #x3c62f44f #x35c0eaa5 #xe805d231 #x428929fb #xb4fcdf82 #x4fb66a53 #x0e7dc15b
#x1f081fab #x108618ae #xfcfd086d #xf9ff2889 #x694bcc11 #x236a5cae #x12deca4d #x2c3f8cc5
#xd2d02dfe #xf8ef5896 #xe4cf52da #x95155b67 #x494a488c #xb9b6a80c #x5c8f82bc #x89d36b45
#x3a609437 #xec00c9a9 #x44715253 #x0a874b49 #xd773bc40 #x7c34671c #x02717ef6 #x4feb5536
#xa2d02fff #xd2bf60c4 #xd43f03c0 #x50b4ef6d #x07478cd1 #x006e1888 #xa2e53f55 #xb9e6d4bc
#xa2048016 #x97573833 #xd7207d67 #xde0f8f3d #x72f87b33 #xabcc4f33 #x7688c55d #x7b00a6b0
#x947b0001 #x570075d2 #xf9bb88f8 #x8942019e #x4264a5ff #x856302e0 #x72dbd92b #xee971b69
#x6ea22fde #x5f08ae2b #xaf7a616d #xe5c98767 #xcf1febd2 #x61efc8c2 #xf1ac2571 #xcc8239c2
#x67214cb8 #xb1e583d1 #xb7dc3e62 #x7f10bdce #xf90a5c38 #x0ff0443d #x606e6dc6 #x60543a49
#x5727c148 #x2be98a1d #x8ab41738 #x20e1be24 #xaf96da0f #x68458425 #x99833be5 #x600d457d
#x282f9350 #x8334b362 #xd91d1120 #x2b6d8da0 #x642b1e31 #x9c305a00 #x52bce688 #x1b03588a
#xf7baefd5 #x4142ed9c #xa4315c11 #x83323ec5 #xdfef4636 #xa133c501 #xe9d3531c #xee353783))

(defconst +cast5-sbox3+
#32@(#x9db30420 #x1fb6e9de #xa7be7bef #xd273a298 #x4a4f7bdb #x64ad8c57 #x85510443 #xfa020ed1
#x7e287aff #xe60fb663 #x095f35a1 #x79ebf120 #xfd059d43 #x6497b7b1 #xf3641f63 #x241e4adf
#x28147f5f #x4fa2b8cd #xc9430040 #x0cc32220 #xfdd30b30 #xc0a5374f #x1d2d00d9 #x24147b15
#xee4d111a #x0fca5167 #x71ff904c #x2d195ffe #x1a05645f #x0c13fefe #x081b08ca #x05170121
#x80530100 #xe83e5efe #xac9af4f8 #x7fe72701 #xd2b8ee5f #x06df4261 #xbb9e9b8a #x7293ea25
#xce84ffdf #xf5718801 #x3dd64b04 #xa26f263b #x7ed48400 #x547eebe6 #x446d4ca0 #x6cf3d6f5
#x2649abdf #xaea0c7f5 #x36338cc1 #x503f7e93 #xd3772061 #x11b638e1 #x72500e03 #xf80eb2bb
#xabe0502e #xec8d77de #x57971e81 #xe14f6746 #xc9335400 #x6920318f #x081dbb99 #xffc304a5
#x4d351805 #x7f3d5ce3 #xa6c866c6 #x5d5bcca9 #xdaec6fea #x9f926f91 #x9f46222f #x3991467d
#xa5bf6d8e #x1143c44f #x43958302 #xd0214eeb #x022083b8 #x3fb6180c #x18f8931e #x281658e6
#x26486e3e #x8bd78a70 #x7477e4c1 #xb506e07c #xf32d0a25 #x79098b02 #xe4eabb81 #x28123b23
#x69dead38 #x1574ca16 #xdf871b62 #x211c40b7 #xa51a9ef9 #x0014377b #x041e8ac8 #x09114003
#xbd59e4d2 #xe3d156d5 #x4fe876d5 #x2f91a340 #x557be8de #x00eae4a7 #x0ce5c2ec #x4db4bba6
#xe756bdff #xdd3369ac #xec17b035 #x06572327 #x99afc8b0 #x56c8c391 #x6b65811c #x5e146119
#x6e85cb75 #xbe07c002 #xc2325577 #x893ff4ec #x5bbfc92d #xd0ec3b25 #xb7801ab7 #x8d6d3b24
#x20c763ef #xc366a5fc #x9c382880 #x0ace3205 #xaac9548a #xeca1d7c7 #x041afa32 #x1d16625a
#x6701902c #x9b757a54 #x31d477f7 #x9126b031 #x36cc6fdb #xc70b8b46 #xd9e66a48 #x56e55a79
#x026a4ceb #x52437eff #x2f8f76b4 #x0df980a5 #x8674cde3 #xedda04eb #x17a9be04 #x2c18f4df
#xb7747f9d #xab2af7b4 #xefc34d20 #x2e096b7c #x1741a254 #xe5b6a035 #x213d42f6 #x2c1c7c26
#x61c2f50f #x6552daf9 #xd2c231f8 #x25130f69 #xd8167fa2 #x0418f2c8 #x001a96a6 #x0d1526ab
#x63315c21 #x5e0a72ec #x49bafefd #x187908d9 #x8d0dbd86 #x311170a7 #x3e9b640c #xcc3e10d7
#xd5cad3b6 #x0caec388 #xf73001e1 #x6c728aff #x71eae2a1 #x1f9af36e #xcfcbd12f #xc1de8417
#xac07be6b #xcb44a1d8 #x8b9b0f56 #x013988c3 #xb1c52fca #xb4be31cd #xd8782806 #x12a3a4e2
#x6f7de532 #x58fd7eb6 #xd01ee900 #x24adffc2 #xf4990fc5 #x9711aac5 #x001d7b95 #x82e5e7d2
#x109873f6 #x00613096 #xc32d9521 #xada121ff #x29908415 #x7fbb977f #xaf9eb3db #x29c9ed2a
#x5ce2a465 #xa730f32c #xd0aa3fe8 #x8a5cc091 #xd49e2ce7 #x0ce454a9 #xd60acd86 #x015f1919
#x77079103 #xdea03af6 #x78a8565e #xdee356df #x21f05cbe #x8b75e387 #xb3c50651 #xb8a5c3ef
#xd8eeb6d2 #xe523be77 #xc2154529 #x2f69efdf #xafe67afb #xf470c4b2 #xf3e0eb5b #xd6cc9876
#x39e4460c #x1fda8538 #x1987832f #xca007367 #xa99144f8 #x296b299e #x492fc295 #x9266beab
#xb5676e69 #x9bd3ddda #xdf7e052f #xdb25701c #x1b5e51ee #xf65324e6 #x6afce36c #x0316cc04
#x8644213e #xb7dc59d0 #x7965291f #xccd6fd43 #x41823979 #x932bcdf6 #xb657c34d #x4edfd282
#x7ae5290c #x3cb9536b #x851e20fe #x9833557e #x13ecf0b0 #xd3ffb372 #x3f85c5c1 #x0aef7ed2))

(defconst +cast5-sbox4+
#32@(#x7ec90c04 #x2c6e74b9 #x9b0e66df #xa6337911 #xb86a7fff #x1dd358f5 #x44dd9d44 #x1731167f
#x08fbf1fa #xe7f511cc #xd2051b00 #x735aba00 #x2ab722d8 #x386381cb #xacf6243a #x69befd7a
#xe6a2e77f #xf0c720cd #xc4494816 #xccf5c180 #x38851640 #x15b0a848 #xe68b18cb #x4caadeff
#x5f480a01 #x0412b2aa #x259814fc #x41d0efe2 #x4e40b48d #x248eb6fb #x8dba1cfe #x41a99b02
#x1a550a04 #xba8f65cb #x7251f4e7 #x95a51725 #xc106ecd7 #x97a5980a #xc539b9aa #x4d79fe6a
#xf2f3f763 #x68af8040 #xed0c9e56 #x11b4958b #xe1eb5a88 #x8709e6b0 #xd7e07156 #x4e29fea7
#x6366e52d #x02d1c000 #xc4ac8e05 #x9377f571 #x0c05372a #x578535f2 #x2261be02 #xd642a0c9
#xdf13a280 #x74b55bd2 #x682199c0 #xd421e5ec #x53fb3ce8 #xc8adedb3 #x28a87fc9 #x3d959981
#x5c1ff900 #xfe38d399 #x0c4eff0b #x062407ea #xaa2f4fb1 #x4fb96976 #x90c79505 #xb0a8a774
#xef55a1ff #xe59ca2c2 #xa6b62d27 #xe66a4263 #xdf65001f #x0ec50966 #xdfdd55bc #x29de0655
#x911e739a #x17af8975 #x32c7911c #x89f89468 #x0d01e980 #x524755f4 #x03b63cc9 #x0cc844b2
#xbcf3f0aa #x87ac36e9 #xe53a7426 #x01b3d82b #x1a9e7449 #x64ee2d7e #xcddbb1da #x01c94910
#xb868bf80 #x0d26f3fd #x9342ede7 #x04a5c284 #x636737b6 #x50f5b616 #xf24766e3 #x8eca36c1
#x136e05db #xfef18391 #xfb887a37 #xd6e7f7d4 #xc7fb7dc9 #x3063fcdf #xb6f589de #xec2941da
#x26e46695 #xb7566419 #xf654efc5 #xd08d58b7 #x48925401 #xc1bacb7f #xe5ff550f #xb6083049
#x5bb5d0e8 #x87d72e5a #xab6a6ee1 #x223a66ce #xc62bf3cd #x9e0885f9 #x68cb3e47 #x086c010f
#xa21de820 #xd18b69de #xf3f65777 #xfa02c3f6 #x407edac3 #xcbb3d550 #x1793084d #xb0d70eba
#x0ab378d5 #xd951fb0c #xded7da56 #x4124bbe4 #x94ca0b56 #x0f5755d1 #xe0e1e56e #x6184b5be
#x580a249f #x94f74bc0 #xe327888e #x9f7b5561 #xc3dc0280 #x05687715 #x646c6bd7 #x44904db3
#x66b4f0a3 #xc0f1648a #x697ed5af #x49e92ff6 #x309e374f #x2cb6356a #x85808573 #x4991f840
#x76f0ae02 #x083be84d #x28421c9a #x44489406 #x736e4cb8 #xc1092910 #x8bc95fc6 #x7d869cf4
#x134f616f #x2e77118d #xb31b2be1 #xaa90b472 #x3ca5d717 #x7d161bba #x9cad9010 #xaf462ba2
#x9fe459d2 #x45d34559 #xd9f2da13 #xdbc65487 #xf3e4f94e #x176d486f #x097c13ea #x631da5c7
#x445f7382 #x175683f4 #xcdc66a97 #x70be0288 #xb3cdcf72 #x6e5dd2f3 #x20936079 #x459b80a5
#xbe60e2db #xa9c23101 #xeba5315c #x224e42f2 #x1c5c1572 #xf6721b2c #x1ad2fff3 #x8c25404e
#x324ed72f #x4067b7fd #x0523138e #x5ca3bc78 #xdc0fd66e #x75922283 #x784d6b17 #x58ebb16e
#x44094f85 #x3f481d87 #xfcfeae7b #x77b5ff76 #x8c2302bf #xaaf47556 #x5f46b02a #x2b092801
#x3d38f5f7 #x0ca81f36 #x52af4a8a #x66d5e7c0 #xdf3b0874 #x95055110 #x1b5ad7a8 #xf61ed5ad
#x6cf6e479 #x20758184 #xd0cefa65 #x88f7be58 #x4a046826 #x0ff6f8f3 #xa09c7f70 #x5346aba0
#x5ce96c28 #xe176eda3 #x6bac307f #x376829d2 #x85360fa9 #x17e3fe2a #x24b79767 #xf5a96b20
#xd6cd2595 #x68ff1ebf #x7555442c #xf19f06be #xf9e0659a #xeeb9491d #x34010718 #xbb30cab8
#xe822fe15 #x88570983 #x750e6249 #xda627e55 #x5e76ffa8 #xb1534546 #x6d47de08 #xefe9e7d4))

(defconst +cast5-sbox5+
#32@(#xf6fa8f9d #x2cac6ce1 #x4ca34867 #xe2337f7c #x95db08e7 #x016843b4 #xeced5cbc #x325553ac
#xbf9f0960 #xdfa1e2ed #x83f0579d #x63ed86b9 #x1ab6a6b8 #xde5ebe39 #xf38ff732 #x8989b138
#x33f14961 #xc01937bd #xf506c6da #xe4625e7e #xa308ea99 #x4e23e33c #x79cbd7cc #x48a14367
#xa3149619 #xfec94bd5 #xa114174a #xeaa01866 #xa084db2d #x09a8486f #xa888614a #x2900af98
#x01665991 #xe1992863 #xc8f30c60 #x2e78ef3c #xd0d51932 #xcf0fec14 #xf7ca07d2 #xd0a82072
#xfd41197e #x9305a6b0 #xe86be3da #x74bed3cd #x372da53c #x4c7f4448 #xdab5d440 #x6dba0ec3
#x083919a7 #x9fbaeed9 #x49dbcfb0 #x4e670c53 #x5c3d9c01 #x64bdb941 #x2c0e636a #xba7dd9cd
#xea6f7388 #xe70bc762 #x35f29adb #x5c4cdd8d #xf0d48d8c #xb88153e2 #x08a19866 #x1ae2eac8
#x284caf89 #xaa928223 #x9334be53 #x3b3a21bf #x16434be3 #x9aea3906 #xefe8c36e #xf890cdd9
#x80226dae #xc340a4a3 #xdf7e9c09 #xa694a807 #x5b7c5ecc #x221db3a6 #x9a69a02f #x68818a54
#xceb2296f #x53c0843a #xfe893655 #x25bfe68a #xb4628abc #xcf222ebf #x25ac6f48 #xa9a99387
#x53bddb65 #xe76ffbe7 #xe967fd78 #x0ba93563 #x8e342bc1 #xe8a11be9 #x4980740d #xc8087dfc
#x8de4bf99 #xa11101a0 #x7fd37975 #xda5a26c0 #xe81f994f #x9528cd89 #xfd339fed #xb87834bf
#x5f04456d #x22258698 #xc9c4c83b #x2dc156be #x4f628daa #x57f55ec5 #xe2220abe #xd2916ebf
#x4ec75b95 #x24f2c3c0 #x42d15d99 #xcd0d7fa0 #x7b6e27ff #xa8dc8af0 #x7345c106 #xf41e232f
#x35162386 #xe6ea8926 #x3333b094 #x157ec6f2 #x372b74af #x692573e4 #xe9a9d848 #xf3160289
#x3a62ef1d #xa787e238 #xf3a5f676 #x74364853 #x20951063 #x4576698d #xb6fad407 #x592af950
#x36f73523 #x4cfb6e87 #x7da4cec0 #x6c152daa #xcb0396a8 #xc50dfe5d #xfcd707ab #x0921c42f
#x89dff0bb #x5fe2be78 #x448f4f33 #x754613c9 #x2b05d08d #x48b9d585 #xdc049441 #xc8098f9b
#x7dede786 #xc39a3373 #x42410005 #x6a091751 #x0ef3c8a6 #x890072d6 #x28207682 #xa9a9f7be
#xbf32679d #xd45b5b75 #xb353fd00 #xcbb0e358 #x830f220a #x1f8fb214 #xd372cf08 #xcc3c4a13
#x8cf63166 #x061c87be #x88c98f88 #x6062e397 #x47cf8e7a #xb6c85283 #x3cc2acfb #x3fc06976
#x4e8f0252 #x64d8314d #xda3870e3 #x1e665459 #xc10908f0 #x513021a5 #x6c5b68b7 #x822f8aa0
#x3007cd3e #x74719eef #xdc872681 #x073340d4 #x7e432fd9 #x0c5ec241 #x8809286c #xf592d891
#x08a930f6 #x957ef305 #xb7fbffbd #xc266e96f #x6fe4ac98 #xb173ecc0 #xbc60b42a #x953498da
#xfba1ae12 #x2d4bd736 #x0f25faab #xa4f3fceb #xe2969123 #x257f0c3d #x9348af49 #x361400bc
#xe8816f4a #x3814f200 #xa3f94043 #x9c7a54c2 #xbc704f57 #xda41e7f9 #xc25ad33a #x54f4a084
#xb17f5505 #x59357cbe #xedbd15c8 #x7f97c5ab #xba5ac7b5 #xb6f6deaf #x3a479c3a #x5302da25
#x653d7e6a #x54268d49 #x51a477ea #x5017d55b #xd7d25d88 #x44136c76 #x0404a8c8 #xb8e5a121
#xb81a928a #x60ed5869 #x97c55b96 #xeaec991b #x29935913 #x01fdb7f1 #x088e8dfa #x9ab6f6f5
#x3b4cbf9f #x4a5de3ab #xe6051d35 #xa0e1d855 #xd36b4cf1 #xf544edeb #xb0e93524 #xbebb8fbd
#xa2d762cf #x49c92f54 #x38b5f331 #x7128a454 #x48392905 #xa65b1db8 #x851c97bd #xd675cf2f))

(defconst +cast5-sbox6+
#32@(#x85e04019 #x332bf567 #x662dbfff #xcfc65693 #x2a8d7f6f #xab9bc912 #xde6008a1 #x2028da1f
#x0227bce7 #x4d642916 #x18fac300 #x50f18b82 #x2cb2cb11 #xb232e75c #x4b3695f2 #xb28707de
#xa05fbcf6 #xcd4181e9 #xe150210c #xe24ef1bd #xb168c381 #xfde4e789 #x5c79b0d8 #x1e8bfd43
#x4d495001 #x38be4341 #x913cee1d #x92a79c3f #x089766be #xbaeeadf4 #x1286becf #xb6eacb19
#x2660c200 #x7565bde4 #x64241f7a #x8248dca9 #xc3b3ad66 #x28136086 #x0bd8dfa8 #x356d1cf2
#x107789be #xb3b2e9ce #x0502aa8f #x0bc0351e #x166bf52a #xeb12ff82 #xe3486911 #xd34d7516
#x4e7b3aff #x5f43671b #x9cf6e037 #x4981ac83 #x334266ce #x8c9341b7 #xd0d854c0 #xcb3a6c88
#x47bc2829 #x4725ba37 #xa66ad22b #x7ad61f1e #x0c5cbafa #x4437f107 #xb6e79962 #x42d2d816
#x0a961288 #xe1a5c06e #x13749e67 #x72fc081a #xb1d139f7 #xf9583745 #xcf19df58 #xbec3f756
#xc06eba30 #x07211b24 #x45c28829 #xc95e317f #xbc8ec511 #x38bc46e9 #xc6e6fa14 #xbae8584a
#xad4ebc46 #x468f508b #x7829435f #xf124183b #x821dba9f #xaff60ff4 #xea2c4e6d #x16e39264
#x92544a8b #x009b4fc3 #xaba68ced #x9ac96f78 #x06a5b79a #xb2856e6e #x1aec3ca9 #xbe838688
#x0e0804e9 #x55f1be56 #xe7e5363b #xb3a1f25d #xf7debb85 #x61fe033c #x16746233 #x3c034c28
#xda6d0c74 #x79aac56c #x3ce4e1ad #x51f0c802 #x98f8f35a #x1626a49f #xeed82b29 #x1d382fe3
#x0c4fb99a #xbb325778 #x3ec6d97b #x6e77a6a9 #xcb658b5c #xd45230c7 #x2bd1408b #x60c03eb7
#xb9068d78 #xa33754f4 #xf430c87d #xc8a71302 #xb96d8c32 #xebd4e7be #xbe8b9d2d #x7979fb06
#xe7225308 #x8b75cf77 #x11ef8da4 #xe083c858 #x8d6b786f #x5a6317a6 #xfa5cf7a0 #x5dda0033
#xf28ebfb0 #xf5b9c310 #xa0eac280 #x08b9767a #xa3d9d2b0 #x79d34217 #x021a718d #x9ac6336a
#x2711fd60 #x438050e3 #x069908a8 #x3d7fedc4 #x826d2bef #x4eeb8476 #x488dcf25 #x36c9d566
#x28e74e41 #xc2610aca #x3d49a9cf #xbae3b9df #xb65f8de6 #x92aeaf64 #x3ac7d5e6 #x9ea80509
#xf22b017d #xa4173f70 #xdd1e16c3 #x15e0d7f9 #x50b1b887 #x2b9f4fd5 #x625aba82 #x6a017962
#x2ec01b9c #x15488aa9 #xd716e740 #x40055a2c #x93d29a22 #xe32dbf9a #x058745b9 #x3453dc1e
#xd699296e #x496cff6f #x1c9f4986 #xdfe2ed07 #xb87242d1 #x19de7eae #x053e561a #x15ad6f8c
#x66626c1c #x7154c24c #xea082b2a #x93eb2939 #x17dcb0f0 #x58d4f2ae #x9ea294fb #x52cf564c
#x9883fe66 #x2ec40581 #x763953c3 #x01d6692e #xd3a0c108 #xa1e7160e #xe4f2dfa6 #x693ed285
#x74904698 #x4c2b0edd #x4f757656 #x5d393378 #xa132234f #x3d321c5d #xc3f5e194 #x4b269301
#xc79f022f #x3c997e7e #x5e4f9504 #x3ffafbbd #x76f7ad0e #x296693f4 #x3d1fce6f #xc61e45be
#xd3b5ab34 #xf72bf9b7 #x1b0434c0 #x4e72b567 #x5592a33d #xb5229301 #xcfd2a87f #x60aeb767
#x1814386b #x30bcc33d #x38a0c07d #xfd1606f2 #xc363519b #x589dd390 #x5479f8e6 #x1cb8d647
#x97fd61a9 #xea7759f4 #x2d57539d #x569a58cf #xe84e63ad #x462e1b78 #x6580f87e #xf3817914
#x91da55f4 #x40a230f3 #xd1988f35 #xb6e318d2 #x3ffa50bc #x3d40f021 #xc3c0bdae #x4958c24c
#x518f36b2 #x84b1d370 #x0fedce83 #x878ddada #xf2a279c7 #x94e01be8 #x90716f4b #x954b8aa3))

(defconst +cast5-sbox7+
#32@(#xe216300d #xbbddfffc #xa7ebdabd #x35648095 #x7789f8b7 #xe6c1121b #x0e241600 #x052ce8b5
#x11a9cfb0 #xe5952f11 #xece7990a #x9386d174 #x2a42931c #x76e38111 #xb12def3a #x37ddddfc
#xde9adeb1 #x0a0cc32c #xbe197029 #x84a00940 #xbb243a0f #xb4d137cf #xb44e79f0 #x049eedfd
#x0b15a15d #x480d3168 #x8bbbde5a #x669ded42 #xc7ece831 #x3f8f95e7 #x72df191b #x7580330d
#x94074251 #x5c7dcdfa #xabbe6d63 #xaa402164 #xb301d40a #x02e7d1ca #x53571dae #x7a3182a2
#x12a8ddec #xfdaa335d #x176f43e8 #x71fb46d4 #x38129022 #xce949ad4 #xb84769ad #x965bd862
#x82f3d055 #x66fb9767 #x15b80b4e #x1d5b47a0 #x4cfde06f #xc28ec4b8 #x57e8726e #x647a78fc
#x99865d44 #x608bd593 #x6c200e03 #x39dc5ff6 #x5d0b00a3 #xae63aff2 #x7e8bd632 #x70108c0c
#xbbd35049 #x2998df04 #x980cf42a #x9b6df491 #x9e7edd53 #x06918548 #x58cb7e07 #x3b74ef2e
#x522fffb1 #xd24708cc #x1c7e27cd #xa4eb215b #x3cf1d2e2 #x19b47a38 #x424f7618 #x35856039
#x9d17dee7 #x27eb35e6 #xc9aff67b #x36baf5b8 #x09c467cd #xc18910b1 #xe11dbf7b #x06cd1af8
#x7170c608 #x2d5e3354 #xd4de495a #x64c6d006 #xbcc0c62c #x3dd00db3 #x708f8f34 #x77d51b42
#x264f620f #x24b8d2bf #x15c1b79e #x46a52564 #xf8d7e54e #x3e378160 #x7895cda5 #x859c15a5
#xe6459788 #xc37bc75f #xdb07ba0c #x0676a3ab #x7f229b1e #x31842e7b #x24259fd7 #xf8bef472
#x835ffcb8 #x6df4c1f2 #x96f5b195 #xfd0af0fc #xb0fe134c #xe2506d3d #x4f9b12ea #xf215f225
#xa223736f #x9fb4c428 #x25d04979 #x34c713f8 #xc4618187 #xea7a6e98 #x7cd16efc #x1436876c
#xf1544107 #xbedeee14 #x56e9af27 #xa04aa441 #x3cf7c899 #x92ecbae6 #xdd67016d #x151682eb
#xa842eedf #xfdba60b4 #xf1907b75 #x20e3030f #x24d8c29e #xe139673b #xefa63fb8 #x71873054
#xb6f2cf3b #x9f326442 #xcb15a4cc #xb01a4504 #xf1e47d8d #x844a1be5 #xbae7dfdc #x42cbda70
#xcd7dae0a #x57e85b7a #xd53f5af6 #x20cf4d8c #xcea4d428 #x79d130a4 #x3486ebfb #x33d3cddc
#x77853b53 #x37effcb5 #xc5068778 #xe580b3e6 #x4e68b8f4 #xc5c8b37e #x0d809ea2 #x398feb7c
#x132a4f94 #x43b7950e #x2fee7d1c #x223613bd #xdd06caa2 #x37df932b #xc4248289 #xacf3ebc3
#x5715f6b7 #xef3478dd #xf267616f #xc148cbe4 #x9052815e #x5e410fab #xb48a2465 #x2eda7fa4
#xe87b40e4 #xe98ea084 #x5889e9e1 #xefd390fc #xdd07d35b #xdb485694 #x38d7e5b2 #x57720101
#x730edebc #x5b643113 #x94917e4f #x503c2fba #x646f1282 #x7523d24a #xe0779695 #xf9c17a8f
#x7a5b2121 #xd187b896 #x29263a4d #xba510cdf #x81f47c9f #xad1163ed #xea7b5965 #x1a00726e
#x11403092 #x00da6d77 #x4a0cdd61 #xad1f4603 #x605bdfb0 #x9eedc364 #x22ebe6a8 #xcee7d28a
#xa0e736a0 #x5564a6b9 #x10853209 #xc7eb8f37 #x2de705ca #x8951570f #xdf09822b #xbd691a6c
#xaa12e4f2 #x87451c0f #xe0f6a27a #x3ada4819 #x4cf1764f #x0d771c2b #x67cdb156 #x350d8384
#x5938fa0f #x42399ef3 #x36997b07 #x0e84093d #x4aa93e61 #x8360d87b #x1fa98b0c #x1149382c
#xe97625a5 #x0614d1b7 #x0e25244b #x0c768347 #x589e8d82 #x0d2059d1 #xa466bb1e #xf8da0a82
#x04f19130 #xba6e4ec0 #x99265164 #x1ee7230d #x50b2ad80 #xeaee6801 #x8db2a283 #xea8bf59e))


;;; the actual CAST5 implementation

(deftype cast5-mask-vector () '(simple-array (unsigned-byte 32) (16)))
(deftype cast5-rotate-vector () '(simple-array (unsigned-byte 8) (16)))

(defclass cast5 (cipher 8-byte-block-mixin)
  ((mask-vector :accessor mask-vector :type cast5-mask-vector)
   (rotate-vector :accessor rotate-vector :type cast5-rotate-vector)
   (n-rounds :accessor n-rounds)))

(declaim (inline cast5-f1 cast5-f2 cast5-f3))

(macrolet ((cast5-s-box (s-box-index index)
             `(aref ,(symbolicate '#:+cast5-sbox s-box-index '#:+)
                    ,index)))

(defun cast5-f1 (input mask rotate)
  (declare (type (unsigned-byte 32) input mask))
  (declare (type (unsigned-byte 5) rotate))
  (let ((value (rol32 (mod32+ input mask) rotate)))
    (declare (type (unsigned-byte 32) value))
    (mod32+ (cast5-s-box 3 (first-byte value))
            (mod32- (logxor (cast5-s-box 1 (third-byte value))
                            (cast5-s-box 0 (fourth-byte value)))
                    (cast5-s-box 2 (second-byte value))))))

(defun cast5-f2 (input mask rotate)
  (declare (type (unsigned-byte 32) input mask))
  (declare (type (unsigned-byte 5) rotate))
  (let ((value (rol32 (logxor input mask) rotate)))
    (declare (type (unsigned-byte 32) value))
    (logxor (cast5-s-box 3 (first-byte value))
            (mod32+ (cast5-s-box 2 (second-byte value))
                    (mod32- (cast5-s-box 0 (fourth-byte value))
                            (cast5-s-box 1 (third-byte value)))))))

(defun cast5-f3 (input mask rotate)
  (declare (type (unsigned-byte 32) input mask))
  (declare (type (unsigned-byte 5) rotate))
  (let ((value (rol32 (mod32- mask input) rotate)))
    (declare (type (unsigned-byte 32) value))
    (mod32- (logxor (cast5-s-box 2 (second-byte value))
                    (mod32+ (cast5-s-box 1 (third-byte value))
                            (cast5-s-box 0 (fourth-byte value))))
            (cast5-s-box 3 (first-byte value)))))

(define-block-encryptor cast5 8
  (let ((mask-vector (mask-vector context))
        (rotate-vector (rotate-vector context))
        (n-rounds (n-rounds context)))
    (declare (type cast5-mask-vector mask-vector))
    (declare (type cast5-rotate-vector rotate-vector))
    (declare (type (or (integer 12 12) (integer 16 16)) n-rounds))
    (with-words ((l0 r0) plaintext plaintext-start)
      #.(loop for i from 0 below 16
              for round-function = (ecase i
                                     ((0 3 6 9 12 15) 'cast5-f1)
                                     ((1 4 7 10 13) 'cast5-f2)
                                     ((2 5 8 11 14) 'cast5-f3))
              collect `(let ((x (logxor l0 (,round-function r0
                                                            (aref mask-vector ,i)
                                                            (aref rotate-vector ,i)))))
                        (declare (type (unsigned-byte 32) x))
                        (setf l0 r0 r0 x)) into forms
              finally (return `(progn ,@(subseq forms 0 12)
                                (when (= n-rounds 16)
                                  ,@(subseq forms 12)))))
      (store-words ciphertext ciphertext-start r0 l0))))

(define-block-decryptor cast5 8
  (let ((mask-vector (mask-vector context))
        (rotate-vector (rotate-vector context))
        (n-rounds (n-rounds context)))
    (declare (type cast5-mask-vector mask-vector))
    (declare (type cast5-rotate-vector rotate-vector))
    (declare (type (or (integer 12 12) (integer 16 16)) n-rounds))
    (with-words ((l0 r0) ciphertext ciphertext-start)
      #.(loop for i from 15 downto 0
              for round-function = (ecase i
                                     ((0 3 6 9 12 15) 'cast5-f1)
                                     ((1 4 7 10 13) 'cast5-f2)
                                     ((2 5 8 11 14) 'cast5-f3))
              collect `(let ((x (logxor l0 (,round-function r0
                                                            (aref mask-vector ,i)
                                                            (aref rotate-vector ,i)))))
                        (declare (type (unsigned-byte 32) x))
                        (setf l0 r0 r0 x)) into forms
              finally (return `(progn (when (= n-rounds 16)
                                        ,@(subseq forms 0 4))
                                ,@(subseq forms 4))))
      (store-words plaintext plaintext-start r0 l0))))
) ; MACROLET

(defun generate-cast5-key-schedule (key)
  (declare (type (simple-array (unsigned-byte 8) (16)) key))
  (with-words ((x0 x4 x8 xc) key 0)
    (let* ((mask-vector (make-array 16 :element-type '(unsigned-byte 32)))
           (rotate-vector (make-array 16 :element-type '(unsigned-byte 8)))
           (z0 0)
           (z4 0)
           (z8 0)
           (zc 0))
      (declare (type (unsigned-byte 32) z0 z4 z8 zc))
      ;;; generate mask material
      (setf z0 (logxor x0 (aref +cast5-sbox4+ (third-byte xc)) (aref +cast5-sbox5+ (first-byte xc)) (aref +cast5-sbox6+ (fourth-byte xc)) (aref +cast5-sbox7+ (second-byte xc)) (aref +cast5-sbox6+ (fourth-byte x8))))
      (setf z4 (logxor x8 (aref +cast5-sbox4+ (fourth-byte z0)) (aref +cast5-sbox5+ (second-byte z0)) (aref +cast5-sbox6+ (third-byte z0)) (aref +cast5-sbox7+ (first-byte z0)) (aref +cast5-sbox7+ (second-byte x8))))
      (setf z8 (logxor xc (aref +cast5-sbox4+ (first-byte z4)) (aref +cast5-sbox5+ (second-byte z4)) (aref +cast5-sbox6+ (third-byte z4)) (aref +cast5-sbox7+ (fourth-byte z4)) (aref +cast5-sbox4+ (third-byte x8))))
      (setf zc (logxor x4 (aref +cast5-sbox4+ (second-byte z8)) (aref +cast5-sbox5+ (third-byte z8)) (aref +cast5-sbox6+ (first-byte z8)) (aref +cast5-sbox7+ (fourth-byte z8)) (aref +cast5-sbox5+ (first-byte x8))))
      (setf (aref mask-vector (- 1 1))  (logxor (aref +cast5-sbox4+ (fourth-byte z8)) (aref +cast5-sbox5+ (third-byte z8)) (aref +cast5-sbox6+ (first-byte z4)) (aref +cast5-sbox7+ (second-byte z4)) (aref +cast5-sbox4+ (second-byte z0))))
      (setf (aref mask-vector (- 2 1))  (logxor (aref +cast5-sbox4+ (second-byte z8)) (aref +cast5-sbox5+ (first-byte z8)) (aref +cast5-sbox6+ (third-byte z4)) (aref +cast5-sbox7+ (fourth-byte z4)) (aref +cast5-sbox5+ (second-byte z4))))
      (setf (aref mask-vector (- 3 1))  (logxor (aref +cast5-sbox4+ (fourth-byte zc)) (aref +cast5-sbox5+ (third-byte zc)) (aref +cast5-sbox6+ (first-byte z0)) (aref +cast5-sbox7+ (second-byte z0)) (aref +cast5-sbox6+ (third-byte z8))))
      (setf (aref mask-vector (- 4 1))  (logxor (aref +cast5-sbox4+ (second-byte zc)) (aref +cast5-sbox5+ (first-byte zc)) (aref +cast5-sbox6+ (third-byte z0)) (aref +cast5-sbox7+ (fourth-byte z0)) (aref +cast5-sbox7+ (fourth-byte zc))))
      (setf x0 (logxor z8 (aref +cast5-sbox4+ (third-byte z4)) (aref +cast5-sbox5+ (first-byte z4)) (aref +cast5-sbox6+ (fourth-byte z4)) (aref +cast5-sbox7+ (second-byte z4)) (aref +cast5-sbox6+ (fourth-byte z0))))
      (setf x4 (logxor z0 (aref +cast5-sbox4+ (fourth-byte x0)) (aref +cast5-sbox5+ (second-byte x0)) (aref +cast5-sbox6+ (third-byte x0)) (aref +cast5-sbox7+ (first-byte x0)) (aref +cast5-sbox7+ (second-byte z0))))
      (setf x8 (logxor z4 (aref +cast5-sbox4+ (first-byte x4)) (aref +cast5-sbox5+ (second-byte x4)) (aref +cast5-sbox6+ (third-byte x4)) (aref +cast5-sbox7+ (fourth-byte x4)) (aref +cast5-sbox4+ (third-byte z0))))
      (setf xc (logxor zc (aref +cast5-sbox4+ (second-byte x8)) (aref +cast5-sbox5+ (third-byte x8)) (aref +cast5-sbox6+ (first-byte x8)) (aref +cast5-sbox7+ (fourth-byte x8)) (aref +cast5-sbox5+ (first-byte z0))))
      (setf (aref mask-vector (- 5 1))  (logxor (aref +cast5-sbox4+ (first-byte x0)) (aref +cast5-sbox5+ (second-byte x0)) (aref +cast5-sbox6+ (fourth-byte xc)) (aref +cast5-sbox7+ (third-byte xc)) (aref +cast5-sbox4+ (fourth-byte x8))))
      (setf (aref mask-vector (- 6 1))  (logxor (aref +cast5-sbox4+ (third-byte x0)) (aref +cast5-sbox5+ (fourth-byte x0)) (aref +cast5-sbox6+ (second-byte xc)) (aref +cast5-sbox7+ (first-byte xc)) (aref +cast5-sbox5+ (third-byte xc))))
      (setf (aref mask-vector (- 7 1))  (logxor (aref +cast5-sbox4+ (first-byte x4)) (aref +cast5-sbox5+ (second-byte x4)) (aref +cast5-sbox6+ (fourth-byte x8)) (aref +cast5-sbox7+ (third-byte x8)) (aref +cast5-sbox6+ (first-byte x0))))
      (setf (aref mask-vector (- 8 1))  (logxor (aref +cast5-sbox4+ (third-byte x4)) (aref +cast5-sbox5+ (fourth-byte x4)) (aref +cast5-sbox6+ (second-byte x8)) (aref +cast5-sbox7+ (first-byte x8)) (aref +cast5-sbox7+ (first-byte x4))))
      (setf z0 (logxor x0 (aref +cast5-sbox4+ (third-byte xc)) (aref +cast5-sbox5+ (first-byte xc)) (aref +cast5-sbox6+ (fourth-byte xc)) (aref +cast5-sbox7+ (second-byte xc)) (aref +cast5-sbox6+ (fourth-byte x8))))
      (setf z4 (logxor x8 (aref +cast5-sbox4+ (fourth-byte z0)) (aref +cast5-sbox5+ (second-byte z0)) (aref +cast5-sbox6+ (third-byte z0)) (aref +cast5-sbox7+ (first-byte z0)) (aref +cast5-sbox7+ (second-byte x8))))
      (setf z8 (logxor xc (aref +cast5-sbox4+ (first-byte z4)) (aref +cast5-sbox5+ (second-byte z4)) (aref +cast5-sbox6+ (third-byte z4)) (aref +cast5-sbox7+ (fourth-byte z4)) (aref +cast5-sbox4+ (third-byte x8))))
      (setf zc (logxor x4 (aref +cast5-sbox4+ (second-byte z8)) (aref +cast5-sbox5+ (third-byte z8)) (aref +cast5-sbox6+ (first-byte z8)) (aref +cast5-sbox7+ (fourth-byte z8)) (aref +cast5-sbox5+ (first-byte x8))))
      (setf (aref mask-vector (- 9 1))  (logxor (aref +cast5-sbox4+ (first-byte z0)) (aref +cast5-sbox5+ (second-byte z0)) (aref +cast5-sbox6+ (fourth-byte zc)) (aref +cast5-sbox7+ (third-byte zc)) (aref +cast5-sbox4+ (third-byte z8))))
      (setf (aref mask-vector (- 10 1)) (logxor (aref +cast5-sbox4+ (third-byte z0)) (aref +cast5-sbox5+ (fourth-byte z0)) (aref +cast5-sbox6+ (second-byte zc)) (aref +cast5-sbox7+ (first-byte zc)) (aref +cast5-sbox5+ (fourth-byte zc))))
      (setf (aref mask-vector (- 11 1)) (logxor (aref +cast5-sbox4+ (first-byte z4)) (aref +cast5-sbox5+ (second-byte z4)) (aref +cast5-sbox6+ (fourth-byte z8)) (aref +cast5-sbox7+ (third-byte z8)) (aref +cast5-sbox6+ (second-byte z0))))
      (setf (aref mask-vector (- 12 1)) (logxor (aref +cast5-sbox4+ (third-byte z4)) (aref +cast5-sbox5+ (fourth-byte z4)) (aref +cast5-sbox6+ (second-byte z8)) (aref +cast5-sbox7+ (first-byte z8)) (aref +cast5-sbox7+ (second-byte z4))))
      (setf x0 (logxor z8 (aref +cast5-sbox4+ (third-byte z4)) (aref +cast5-sbox5+ (first-byte z4)) (aref +cast5-sbox6+ (fourth-byte z4)) (aref +cast5-sbox7+ (second-byte z4)) (aref +cast5-sbox6+ (fourth-byte z0))))
      (setf x4 (logxor z0 (aref +cast5-sbox4+ (fourth-byte x0)) (aref +cast5-sbox5+ (second-byte x0)) (aref +cast5-sbox6+ (third-byte x0)) (aref +cast5-sbox7+ (first-byte x0)) (aref +cast5-sbox7+ (second-byte z0))))
      (setf x8 (logxor z4 (aref +cast5-sbox4+ (first-byte x4)) (aref +cast5-sbox5+ (second-byte x4)) (aref +cast5-sbox6+ (third-byte x4)) (aref +cast5-sbox7+ (fourth-byte x4)) (aref +cast5-sbox4+ (third-byte z0))))
      (setf xc (logxor zc (aref +cast5-sbox4+ (second-byte x8)) (aref +cast5-sbox5+ (third-byte x8)) (aref +cast5-sbox6+ (first-byte x8)) (aref +cast5-sbox7+ (fourth-byte x8)) (aref +cast5-sbox5+ (first-byte z0))))
      (setf (aref mask-vector (- 13 1)) (logxor (aref +cast5-sbox4+ (fourth-byte x8)) (aref +cast5-sbox5+ (third-byte x8)) (aref +cast5-sbox6+ (first-byte x4)) (aref +cast5-sbox7+ (second-byte x4)) (aref +cast5-sbox4+ (first-byte x0))))
      (setf (aref mask-vector (- 14 1)) (logxor (aref +cast5-sbox4+ (second-byte x8)) (aref +cast5-sbox5+ (first-byte x8)) (aref +cast5-sbox6+ (third-byte x4)) (aref +cast5-sbox7+ (fourth-byte x4)) (aref +cast5-sbox5+ (first-byte x4))))
      (setf (aref mask-vector (- 15 1)) (logxor (aref +cast5-sbox4+ (fourth-byte xc)) (aref +cast5-sbox5+ (third-byte xc)) (aref +cast5-sbox6+ (first-byte x0)) (aref +cast5-sbox7+ (second-byte x0)) (aref +cast5-sbox6+ (fourth-byte x8))))
      (setf (aref mask-vector (- 16 1)) (logxor (aref +cast5-sbox4+ (second-byte xc)) (aref +cast5-sbox5+ (first-byte xc)) (aref +cast5-sbox6+ (third-byte x0)) (aref +cast5-sbox7+ (fourth-byte x0)) (aref +cast5-sbox7+ (third-byte xc))))
      ;;; generate shift amounts
      (setf z0 (logxor x0 (aref +cast5-sbox4+ (third-byte xc)) (aref +cast5-sbox5+ (first-byte xc)) (aref +cast5-sbox6+ (fourth-byte xc)) (aref +cast5-sbox7+ (second-byte xc)) (aref +cast5-sbox6+ (fourth-byte x8))))
      (setf z4 (logxor x8 (aref +cast5-sbox4+ (fourth-byte z0)) (aref +cast5-sbox5+ (second-byte z0)) (aref +cast5-sbox6+ (third-byte z0)) (aref +cast5-sbox7+ (first-byte z0)) (aref +cast5-sbox7+ (second-byte x8))))
      (setf z8 (logxor xc (aref +cast5-sbox4+ (first-byte z4)) (aref +cast5-sbox5+ (second-byte z4)) (aref +cast5-sbox6+ (third-byte z4)) (aref +cast5-sbox7+ (fourth-byte z4)) (aref +cast5-sbox4+ (third-byte x8))))
      (setf zc (logxor x4 (aref +cast5-sbox4+ (second-byte z8)) (aref +cast5-sbox5+ (third-byte z8)) (aref +cast5-sbox6+ (first-byte z8)) (aref +cast5-sbox7+ (fourth-byte z8)) (aref +cast5-sbox5+ (first-byte x8))))
      (setf (aref rotate-vector (- 17 17)) (ldb (byte 5 0) (logxor (aref +cast5-sbox4+ (fourth-byte z8)) (aref +cast5-sbox5+ (third-byte z8)) (aref +cast5-sbox6+ (first-byte z4)) (aref +cast5-sbox7+ (second-byte z4)) (aref +cast5-sbox4+ (second-byte z0)))))
      (setf (aref rotate-vector (- 18 17)) (ldb (byte 5 0) (logxor (aref +cast5-sbox4+ (second-byte z8)) (aref +cast5-sbox5+ (first-byte z8)) (aref +cast5-sbox6+ (third-byte z4)) (aref +cast5-sbox7+ (fourth-byte z4)) (aref +cast5-sbox5+ (second-byte z4)))))
      (setf (aref rotate-vector (- 19 17)) (ldb (byte 5 0) (logxor (aref +cast5-sbox4+ (fourth-byte zc)) (aref +cast5-sbox5+ (third-byte zc)) (aref +cast5-sbox6+ (first-byte z0)) (aref +cast5-sbox7+ (second-byte z0)) (aref +cast5-sbox6+ (third-byte z8)))))
      (setf (aref rotate-vector (- 20 17)) (ldb (byte 5 0) (logxor (aref +cast5-sbox4+ (second-byte zc)) (aref +cast5-sbox5+ (first-byte zc)) (aref +cast5-sbox6+ (third-byte z0)) (aref +cast5-sbox7+ (fourth-byte z0)) (aref +cast5-sbox7+ (fourth-byte zc)))))
      (setf x0 (logxor z8 (aref +cast5-sbox4+ (third-byte z4)) (aref +cast5-sbox5+ (first-byte z4)) (aref +cast5-sbox6+ (fourth-byte z4)) (aref +cast5-sbox7+ (second-byte z4)) (aref +cast5-sbox6+ (fourth-byte z0))))
      (setf x4 (logxor z0 (aref +cast5-sbox4+ (fourth-byte x0)) (aref +cast5-sbox5+ (second-byte x0)) (aref +cast5-sbox6+ (third-byte x0)) (aref +cast5-sbox7+ (first-byte x0)) (aref +cast5-sbox7+ (second-byte z0))))
      (setf x8 (logxor z4 (aref +cast5-sbox4+ (first-byte x4)) (aref +cast5-sbox5+ (second-byte x4)) (aref +cast5-sbox6+ (third-byte x4)) (aref +cast5-sbox7+ (fourth-byte x4)) (aref +cast5-sbox4+ (third-byte z0))))
      (setf xc (logxor zc (aref +cast5-sbox4+ (second-byte x8)) (aref +cast5-sbox5+ (third-byte x8)) (aref +cast5-sbox6+ (first-byte x8)) (aref +cast5-sbox7+ (fourth-byte x8)) (aref +cast5-sbox5+ (first-byte z0))))
      (setf (aref rotate-vector (- 21 17)) (ldb (byte 5 0) (logxor (aref +cast5-sbox4+ (first-byte x0)) (aref +cast5-sbox5+ (second-byte x0)) (aref +cast5-sbox6+ (fourth-byte xc)) (aref +cast5-sbox7+ (third-byte xc)) (aref +cast5-sbox4+ (fourth-byte x8)))))
      (setf (aref rotate-vector (- 22 17)) (ldb (byte 5 0) (logxor (aref +cast5-sbox4+ (third-byte x0)) (aref +cast5-sbox5+ (fourth-byte x0)) (aref +cast5-sbox6+ (second-byte xc)) (aref +cast5-sbox7+ (first-byte xc)) (aref +cast5-sbox5+ (third-byte xc)))))
      (setf (aref rotate-vector (- 23 17)) (ldb (byte 5 0) (logxor (aref +cast5-sbox4+ (first-byte x4)) (aref +cast5-sbox5+ (second-byte x4)) (aref +cast5-sbox6+ (fourth-byte x8)) (aref +cast5-sbox7+ (third-byte x8)) (aref +cast5-sbox6+ (first-byte x0)))))
      (setf (aref rotate-vector (- 24 17)) (ldb (byte 5 0) (logxor (aref +cast5-sbox4+ (third-byte x4)) (aref +cast5-sbox5+ (fourth-byte x4)) (aref +cast5-sbox6+ (second-byte x8)) (aref +cast5-sbox7+ (first-byte x8)) (aref +cast5-sbox7+ (first-byte x4)))))
      (setf z0 (logxor x0 (aref +cast5-sbox4+ (third-byte xc)) (aref +cast5-sbox5+ (first-byte xc)) (aref +cast5-sbox6+ (fourth-byte xc)) (aref +cast5-sbox7+ (second-byte xc)) (aref +cast5-sbox6+ (fourth-byte x8))))
      (setf z4 (logxor x8 (aref +cast5-sbox4+ (fourth-byte z0)) (aref +cast5-sbox5+ (second-byte z0)) (aref +cast5-sbox6+ (third-byte z0)) (aref +cast5-sbox7+ (first-byte z0)) (aref +cast5-sbox7+ (second-byte x8))))
      (setf z8 (logxor xc (aref +cast5-sbox4+ (first-byte z4)) (aref +cast5-sbox5+ (second-byte z4)) (aref +cast5-sbox6+ (third-byte z4)) (aref +cast5-sbox7+ (fourth-byte z4)) (aref +cast5-sbox4+ (third-byte x8))))
      (setf zc (logxor x4 (aref +cast5-sbox4+ (second-byte z8)) (aref +cast5-sbox5+ (third-byte z8)) (aref +cast5-sbox6+ (first-byte z8)) (aref +cast5-sbox7+ (fourth-byte z8)) (aref +cast5-sbox5+ (first-byte x8))))
      (setf (aref rotate-vector (- 25 17)) (ldb (byte 5 0) (logxor (aref +cast5-sbox4+ (first-byte z0)) (aref +cast5-sbox5+ (second-byte z0)) (aref +cast5-sbox6+ (fourth-byte zc)) (aref +cast5-sbox7+ (third-byte zc)) (aref +cast5-sbox4+ (third-byte z8)))))
      (setf (aref rotate-vector (- 26 17)) (ldb (byte 5 0) (logxor (aref +cast5-sbox4+ (third-byte z0)) (aref +cast5-sbox5+ (fourth-byte z0)) (aref +cast5-sbox6+ (second-byte zc)) (aref +cast5-sbox7+ (first-byte zc)) (aref +cast5-sbox5+ (fourth-byte zc)))))
      (setf (aref rotate-vector (- 27 17)) (ldb (byte 5 0) (logxor (aref +cast5-sbox4+ (first-byte z4)) (aref +cast5-sbox5+ (second-byte z4)) (aref +cast5-sbox6+ (fourth-byte z8)) (aref +cast5-sbox7+ (third-byte z8)) (aref +cast5-sbox6+ (second-byte z0)))))
      (setf (aref rotate-vector (- 28 17)) (ldb (byte 5 0) (logxor (aref +cast5-sbox4+ (third-byte z4)) (aref +cast5-sbox5+ (fourth-byte z4)) (aref +cast5-sbox6+ (second-byte z8)) (aref +cast5-sbox7+ (first-byte z8)) (aref +cast5-sbox7+ (second-byte z4)))))
      (setf x0 (logxor z8 (aref +cast5-sbox4+ (third-byte z4)) (aref +cast5-sbox5+ (first-byte z4)) (aref +cast5-sbox6+ (fourth-byte z4)) (aref +cast5-sbox7+ (second-byte z4)) (aref +cast5-sbox6+ (fourth-byte z0))))
      (setf x4 (logxor z0 (aref +cast5-sbox4+ (fourth-byte x0)) (aref +cast5-sbox5+ (second-byte x0)) (aref +cast5-sbox6+ (third-byte x0)) (aref +cast5-sbox7+ (first-byte x0)) (aref +cast5-sbox7+ (second-byte z0))))
      (setf x8 (logxor z4 (aref +cast5-sbox4+ (first-byte x4)) (aref +cast5-sbox5+ (second-byte x4)) (aref +cast5-sbox6+ (third-byte x4)) (aref +cast5-sbox7+ (fourth-byte x4)) (aref +cast5-sbox4+ (third-byte z0))))
      (setf xc (logxor zc (aref +cast5-sbox4+ (second-byte x8)) (aref +cast5-sbox5+ (third-byte x8)) (aref +cast5-sbox6+ (first-byte x8)) (aref +cast5-sbox7+ (fourth-byte x8)) (aref +cast5-sbox5+ (first-byte z0))))
      (setf (aref rotate-vector (- 29 17)) (ldb (byte 5 0) (logxor (aref +cast5-sbox4+ (fourth-byte x8)) (aref +cast5-sbox5+ (third-byte x8)) (aref +cast5-sbox6+ (first-byte x4)) (aref +cast5-sbox7+ (second-byte x4)) (aref +cast5-sbox4+ (first-byte x0)))))
      (setf (aref rotate-vector (- 30 17)) (ldb (byte 5 0) (logxor (aref +cast5-sbox4+ (second-byte x8)) (aref +cast5-sbox5+ (first-byte x8)) (aref +cast5-sbox6+ (third-byte x4)) (aref +cast5-sbox7+ (fourth-byte x4)) (aref +cast5-sbox5+ (first-byte x4)))))
      (setf (aref rotate-vector (- 31 17)) (ldb (byte 5 0) (logxor (aref +cast5-sbox4+ (fourth-byte xc)) (aref +cast5-sbox5+ (third-byte xc)) (aref +cast5-sbox6+ (first-byte x0)) (aref +cast5-sbox7+ (second-byte x0)) (aref +cast5-sbox6+ (fourth-byte x8)))))
      (setf (aref rotate-vector (- 32 17)) (ldb (byte 5 0) (logxor (aref +cast5-sbox4+ (second-byte xc)) (aref +cast5-sbox5+ (first-byte xc)) (aref +cast5-sbox6+ (third-byte x0)) (aref +cast5-sbox7+ (fourth-byte x0)) (aref +cast5-sbox7+ (third-byte xc)))))
      (values mask-vector rotate-vector))))

(defmethod schedule-key ((cipher cast5) key)
  (declare (type (simple-array (unsigned-byte 8) (*)) key))
  (let ((length (length key))
        (key (if (= (length key) 16)
                 ;; no padding necessary
                 key
                 (let ((tmp (make-array 16 :element-type '(unsigned-byte 8)
                                        :initial-element 0)))
                   (replace tmp key)))))
    (declare (type (simple-array (unsigned-byte 8) (16)) key))
    (multiple-value-bind (mask-vector rotate-vector)
        (generate-cast5-key-schedule key)
      (let ((n-rounds (if (<= length 10)
                          12
                          16)))
        (setf (mask-vector cipher) mask-vector
              (rotate-vector cipher) rotate-vector
              (n-rounds cipher) n-rounds)
        cipher))))

(defcipher cast5
  (:encrypt-function cast5-encrypt-block)
  (:decrypt-function cast5-decrypt-block)
  (:block-length 8)
  (:key-length (:variable 5 16 1)))
