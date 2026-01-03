;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; streebog.lisp -- implementation of Streebog (GOST R 34.11-2012)

(in-package :crypto)
(in-ironclad-readtable)


;;;
;;; Parameters
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconst +streebog-buffer0+
    #64@(0 0 0 0 0 0 0 0))

  (defconst +streebog-buffer512+
    #64@(#x200 0 0 0 0 0 0 0))

  (defconst +streebog-c+
    (vector #64@(#xdd806559f2a64507 #x05767436cc744d23
                 #xa2422a08a460d315 #x4b7ce09192676901
                 #x714eb88d7585c4fc #x2f6a76432e45d016
                 #xebcb2f81c0657c1f #xb1085bda1ecadae9)
            #64@(#xe679047021b19bb7 #x55dda21bd7cbcd56
                 #x5cb561c2db0aa7ca #x9ab5176b12d69958
                 #x61d55e0f16b50131 #xf3feea720a232b98
                 #x4fe39d460f70b5d7 #x6fa3b58aa99d2f1a)
            #64@(#x991e96f50aba0ab2 #xc2b6f443867adb31
                 #xc1c93a376062db09 #xd3e20fe490359eb1
                 #xf2ea7514b1297b7b #x06f15e5f529c1f8b
                 #x0a39fc286a3d8435 #xf574dcac2bce2fc7)
            #64@(#x220cbebc84e3d12e #x3453eaa193e837f1
                 #xd8b71333935203be #xa9d72c82ed03d675
                 #x9d721cad685e353f #x488e857e335c3c7d
                 #xf948e1a05d71e4dd #xef1fdfb3e81566d2)
            #64@(#x601758fd7c6cfe57 #x7a56a27ea9ea63f5
                 #xdfff00b723271a16 #xbfcd1747253af5a3
                 #x359e35d7800fffbd #x7f151c1f1686104a
                 #x9a3f410c6ca92363 #x4bea6bacad474799)
            #64@(#xfa68407a46647d6e #xbf71c57236904f35
                 #x0af21f66c2bec6b6 #xcffaa6b71c9ab7b4
                 #x187f9ab49af08ec6 #x2d66c4f95142a46c
                 #x6fa4c33b7a3039c0 #xae4faeae1d3ad3d9)
            #64@(#x8886564d3a14d493 #x3517454ca23c4af3
                 #x06476983284a0504 #x0992abc52d822c37
                 #xd3473e33197a93c9 #x399ec6c7e6bf87c9
                 #x51ac86febf240954 #xf4c70e16eeaac5ec)
            #64@(#xa47f0dd4bf02e71e #x36acc2355951a8d9
                 #x69d18d2bd1a5c42f #xf4892bcb929b0690
                 #x89b4443b4ddbc49a #x4eb7f8719c36de1e
                 #x03e7aa020c6e4141 #x9b1f5b424d93c9a7)
            #64@(#x7261445183235adb #x0e38dc92cb1f2a60
                 #x7b2b8a9aa6079c54 #x800a440bdbb2ceb1
                 #x3cd955b7e00d0984 #x3a7d3a1b25894224
                 #x944c9ad8ec165fde #x378f5a541631229b)
            #64@(#x74b4c7fb98459ced #x3698fad1153bb6c3
                 #x7a1e6c303b7652f4 #x9fe76702af69334b
                 #x1fffe18a1b336103 #x8941e71cff8a78db
                 #x382ae548b2e4f3f3 #xabbedea680056f52)
            #64@(#x6bcaa4cd81f32d1b #xdea2594ac06fd85d
                 #xefbacd1d7d476e98 #x8a1d71efea48b9ca
                 #x2001802114846679 #xd8fa6bbbebab0761
                 #x3002c6cd635afe94 #x7bcd9ed0efc889fb)
            #64@(#x48bc924af11bd720 #xfaf417d5d9b21b99
                 #xe71da4aa88e12852 #x5d80ef9d1891cc86
                 #xf82012d430219f9b #xcda43c32bcdf1d77
                 #xd21380b00449b17a #x378ee767f11631ba)))

  (defconst +streebog-ax+
    (make-array '(8 256)
                :element-type '(unsigned-byte 64)
                :initial-contents '((#xd01f715b5c7ef8e6 #x16fa240980778325
                                     #xa8a42e857ee049c8 #x6ac1068fa186465b
                                     #x6e417bd7a2e9320b #x665c8167a437daab
                                     #x7666681aa89617f6 #x4b959163700bdcf5
                                     #xf14be6b78df36248 #xc585bd689a625cff
                                     #x9557d7fca67d82cb #x89f0b969af6dd366
                                     #xb0833d48749f6c35 #xa1998c23b1ecbc7c
                                     #x8d70c431ac02a736 #xd6dfbc2fd0a8b69e
                                     #x37aeb3e551fa198b #x0b7d128a40b5cf9c
                                     #x5a8f2008b5780cbc #xedec882284e333e5
                                     #xd25fc177d3c7c2ce #x5e0f5d50b61778ec
                                     #x1d873683c0c24cb9 #xad040bcbb45d208c
                                     #x2f89a0285b853c76 #x5732fff6791b8d58
                                     #x3e9311439ef6ec3f #xc9183a809fd3c00f
                                     #x83adf3f5260a01ee #xa6791941f4e8ef10
                                     #x103ae97d0ca1cd5d #x2ce948121dee1b4a
                                     #x39738421dbf2bf53 #x093da2a6cf0cf5b4
                                     #xcd9847d89cbcb45f #xf9561c078b2d8ae8
                                     #x9c6a755a6971777f #xbc1ebaa0712ef0c5
                                     #x72e61542abf963a6 #x78bb5fde229eb12e
                                     #x14ba94250fceb90d #x844d6697630e5282
                                     #x98ea08026a1e032f #xf06bbea144217f5c
                                     #xdb6263d11ccb377a #x641c314b2b8ee083
                                     #x320e96ab9b4770cf #x1ee7deb986a96b85
                                     #xe96cf57a878c47b5 #xfdd6615f8842feb8
                                     #xc83862965601dd1b #x2ea9f83e92572162
                                     #xf876441142ff97fc #xeb2c455608357d9d
                                     #x5612a7e0b0c9904c #x6c01cbfb2d500823
                                     #x4548a6a7fa037a2d #xabc4c6bf388b6ef4
                                     #xbade77d4fdf8bebd #x799b07c8eb4cac3a
                                     #x0c9d87e805b19cf0 #xcb588aac106afa27
                                     #xea0c1d40c1e76089 #x2869354a1e816f1a
                                     #xff96d17307fbc490 #x9f0a9d602f1a5043
                                     #x96373fc6e016a5f7 #x5292dab8b3a6e41c
                                     #x9b8ae0382c752413 #x4f15ec3b7364a8a5
                                     #x3fb349555724f12b #xc7c50d4415db66d7
                                     #x92b7429ee379d1a7 #xd37f99611a15dfda
                                     #x231427c05e34a086 #xa439a96d7b51d538
                                     #xb403401077f01865 #xdda2aea5901d7902
                                     #x0a5d4a9c8967d288 #xc265280adf660f93
                                     #x8bb0094520d4e94e #x2a29856691385532
                                     #x42a833c5bf072941 #x73c64d54622b7eb2
                                     #x07e095624504536c #x8a905153e906f45a
                                     #x6f6123c16b3b2f1f #xc6e55552dc097bc3
                                     #x4468feb133d16739 #xe211e7f0c7398829
                                     #xa2f96419f7879b40 #x19074bdbc3ad38e9
                                     #xf4ebc3f9474e0b0c #x43886bd376d53455
                                     #xd8028beb5aa01046 #x51f23282f5cdc320
                                     #xe7b1c2be0d84e16d #x081dfab006dee8a0
                                     #x3b33340d544b857b #x7f5bcabc679ae242
                                     #x0edd37c48a08a6d8 #x81ed43d9a9b33bc6
                                     #xb1a3655ebd4d7121 #x69a1eeb5e7ed6167
                                     #xf6ab73d5c8f73124 #x1a67a3e185c61fd5
                                     #x2dc91004d43c065e #x0240b02c8fb93a28
                                     #x90f7f2b26cc0eb8f #x3cd3a16f114fd617
                                     #xaae49ea9f15973e0 #x06c0cd748cd64e78
                                     #xda423bc7d5192a6e #xc345701c16b41287
                                     #x6d2193ede4821537 #xfcf639494190e3ac
                                     #x7c3b228621f1c57e #xfb16ac2b0494b0c0
                                     #xbf7e529a3745d7f9 #x6881b6a32e3f7c73
                                     #xca78d2bad9b8e733 #xbbfe2fc2342aa3a9
                                     #x0dbddffecc6381e4 #x70a6a56e2440598e
                                     #xe4d12a844befc651 #x8c509c2765d0ba22
                                     #xee8c6018c28814d9 #x17da7c1f49a59e31
                                     #x609c4c1328e194d3 #xb3e3d57232f44b09
                                     #x91d7aaa4a512f69b #x0ffd6fd243dabbcc
                                     #x50d26a943c1fde34 #x6be15e9968545b4f
                                     #x94778fea6faf9fdf #x2b09dd7058ea4826
                                     #x677cd9716de5c7bf #x49d5214fffb2e6dd
                                     #x0360e83a466b273c #x1fc786af4f7b7691
                                     #xa0b9d435783ea168 #xd49f0c035f118cb6
                                     #x01205816c9d21d14 #xac2453dd7d8f3d98
                                     #x545217cc3f70aa64 #x26b4028e9489c9c2
                                     #xdec2469fd6765e3e #x04807d58036f7450
                                     #xe5f17292823ddb45 #xf30b569b024a5860
                                     #x62dcfc3fa758aefb #xe84cad6c4e5e5aa1
                                     #xccb81fce556ea94b #x53b282ae7a74f908
                                     #x1b47fbf74c1402c1 #x368eebf39828049f
                                     #x7afbeff2ad278b06 #xbe5e0a8cfe97caed
                                     #xcfd8f7f413058e77 #xf78b2bc301252c30
                                     #x4d555c17fcdd928d #x5f2f05467fc565f8
                                     #x24f4b2a21b30f3ea #x860dd6bbecb768aa
                                     #x4c750401350f8f99 #x0000000000000000
                                     #xecccd0344d312ef1 #xb5231806be220571
                                     #xc105c030990d28af #x653c695de25cfd97
                                     #x159acc33c61ca419 #xb89ec7f872418495
                                     #xa9847693b73254dc #x58cf90243ac13694
                                     #x59efc832f3132b80 #x5c4fed7c39ae42c4
                                     #x828dabe3efd81cfa #xd13f294d95ace5f2
                                     #x7d1b7a90e823d86a #xb643f03cf849224d
                                     #x3df3f979d89dcb03 #x7426d836272f2dde
                                     #xdfe21e891fa4432a #x3a136c1b9d99986f
                                     #xfa36f43dcd46add4 #xc025982650df35bb
                                     #x856d3e81aadc4f96 #xc4a5e57e53b041eb
                                     #x4708168b75ba4005 #xaf44bbe73be41aa4
                                     #x971767d029c4b8e3 #xb9be9feebb939981
                                     #x215497ecd18d9aae #x316e7e91dd2c57f3
                                     #xcef8afe2dad79363 #x3853dc371220a247
                                     #x35ee03c9de4323a3 #xe6919aa8c456fc79
                                     #xe05157dc4880b201 #x7bdbb7e464f59612
                                     #x127a59518318f775 #x332ecebd52956ddb
                                     #x8f30741d23bb9d1e #xd922d3fd93720d52
                                     #x7746300c61440ae2 #x25d4eab4d2e2eefe
                                     #x75068020eefd30ca #x135a01474acaea61
                                     #x304e268714fe4ae7 #xa519f17bb283c82c
                                     #xdc82f6b359cf6416 #x5baf781e7caa11a8
                                     #xb2c38d64fb26561d #x34ce5bdf17913eb7
                                     #x5d6fb56af07c5fd0 #x182713cd0a7f25fd
                                     #x9e2ac576e6c84d57 #x9aaab82ee5a73907
                                     #xa3d93c0f3e558654 #x7e7b92aaae48ff56
                                     #x872d8ead256575be #x41c8dbfff96c0e7d
                                     #x99ca5014a3cc1e3b #x40e883e930be1369
                                     #x1ca76e95091051ad #x4e35b42dbab6b5b1
                                     #x05a0254ecabd6944 #xe1710fca8152af15
                                     #xf22b0e8dcb984574 #xb763a82a319b3f59
                                     #x63fca4296e8ab3ef #x9d4a2d4ca0a36a6b
                                     #xe331bfe60eeb953d #xd5bf541596c391a2
                                     #xf5cb9bef8e9c1618 #x46284e9dbc685d11
                                     #x2074cffa185f87ba #xbd3ee2b6b8fcedd1
                                     #xae64e3f1f23607b0 #xfeb68965ce29d984
                                     #x55724fdaf6a2b770 #x29496d5cd753720e
                                     #xa75941573d3af204 #x8e102c0bea69800a
                                     #x111ab16bc573d049 #xd7ffe439197aab8a
                                     #xefac380e0b5a09cd #x48f579593660fbc9
                                     #x22347fd697e6bd92 #x61bc1405e13389c7
                                     #x4ab5c975b9d9c1e1 #x80cd1bcf606126d2
                                     #x7186fd78ed92449a #x93971a882aabccb3
                                     #x88d0e17f66bfce72 #x27945a985d5bd4d6)
                                    (#xde553f8c05a811c8 #x1906b59631b4f565
                                     #x436e70d6b1964ff7 #x36d343cb8b1e9d85
                                     #x843dfacc858aab5a #xfdfc95c299bfc7f9
                                     #x0f634bdea1d51fa2 #x6d458b3b76efb3cd
                                     #x85c3f77cf8593f80 #x3c91315fbe737cb2
                                     #x2148b03366ace398 #x18f8b8264c6761bf
                                     #xc830c1c495c9fb0f #x981a76102086a0aa
                                     #xaa16012142f35760 #x35cc54060c763cf6
                                     #x42907d66cc45db2d #x8203d44b965af4bc
                                     #x3d6f3cefc3a0e868 #xbc73ff69d292bda7
                                     #x8722ed0102e20a29 #x8f8185e8cd34deb7
                                     #x9b0561dda7ee01d9 #x5335a0193227fad6
                                     #xc9cecc74e81a6fd5 #x54f5832e5c2431ea
                                     #x99e47ba05d553470 #xf7bee756acd226ce
                                     #x384e05a5571816fd #xd1367452a47d0e6a
                                     #xf29fde1c386ad85b #x320c77316275f7ca
                                     #xd0c879e2d9ae9ab0 #xdb7406c69110ef5d
                                     #x45505e51a2461011 #xfc029872e46c5323
                                     #xfa3cb6f5f7bc0cc5 #x031f17cd8768a173
                                     #xbd8df2d9af41297d #x9d3b4f5ab43e5e3f
                                     #x4071671b36feee84 #x716207e7d3e3b83d
                                     #x48d20ff2f9283a1a #x27769eb4757cbc7e
                                     #x5c56ebc793f2e574 #xa48b474f9ef5dc18
                                     #x52cbada94ff46e0c #x60c7da982d8199c6
                                     #x0e9d466edc068b78 #x4eec2175eaf865fc
                                     #x550b8e9e21f7a530 #x6b7ba5bc653fec2b
                                     #x5eb7f1ba6949d0dd #x57ea94e3db4c9099
                                     #xf640eae6d101b214 #xdd4a284182c0b0bb
                                     #xff1d8fbf6304f250 #xb8accb933bf9d7e8
                                     #xe8867c478eb68c4d #x3f8e2692391bddc1
                                     #xcb2fd60912a15a7c #xaec935dbab983d2f
                                     #xf55ffd2b56691367 #x80e2ce366ce1c115
                                     #x179bf3f8edb27e1d #x01fe0db07dd394da
                                     #xda8a0b76ecc37b87 #x44ae53e1df9584cb
                                     #xb310b4b77347a205 #xdfab323c787b8512
                                     #x3b511268d070b78e #x65e6e3d2b9396753
                                     #x6864b271e2574d58 #x259784c98fc789d7
                                     #x02e11a7dfabb35a9 #x8841a6dfa337158b
                                     #x7ade78c39b5dcdd0 #xb7cf804d9a2cc84a
                                     #x20b6bd831b7f7742 #x75bd331d3a88d272
                                     #x418f6aab4b2d7a5e #xd9951cbb6babdaf4
                                     #xb6318dfde7ff5c90 #x1f389b112264aa83
                                     #x492c024284fbaec0 #xe33a0363c608f9a0
                                     #x2688930408af28a4 #xc7538a1a341ce4ad
                                     #x5da8e677ee2171ae #x8c9e92254a5c7fc4
                                     #x63d8cd55aae938b5 #x29ebd8daa97a3706
                                     #x959827b37be88aa1 #x1484e4356adadf6e
                                     #xa7945082199d7d6b #xbf6ce8a455fa1cd4
                                     #x9cc542eac9edcae5 #x79c16f0e1c356ca3
                                     #x89bfab6fdee48151 #xd4174d1830c5f0ff
                                     #x9258048415eb419d #x6139d72850520d1c
                                     #x6a85a80c18ec78f1 #xcd11f88e0171059a
                                     #xcceff53e7ca29140 #xd229639f2315af19
                                     #x90b91ef9ef507434 #x5977d28d074a1be1
                                     #x311360fce51d56b9 #xc093a92d5a1f2f91
                                     #x1a19a25bb6dc5416 #xeb996b8a09de2d3e
                                     #xfee3820f1ed7668a #xd7085ad5b7ad518c
                                     #x7fff41890fe53345 #xec5948bd67dde602
                                     #x2fd5f65dbaaa68e0 #xa5754affe32648c2
                                     #xf8ddac880d07396c #x6fa491468c548664
                                     #x0c7c5c1326bdbed1 #x4a33158f03930fb3
                                     #x699abfc19f84d982 #xe4fa2054a80b329c
                                     #x6707f9af438252fa #x08a368e9cfd6d49e
                                     #x47b1442c58fd25b8 #xbbb3dc5ebc91769b
                                     #x1665fe489061eac7 #x33f27a811fa66310
                                     #x93a609346838d547 #x30ed6d4c98cec263
                                     #x1dd9816cd8df9f2a #x94662a03063b1e7b
                                     #x83fdd9fbeb896066 #x7b207573e68e590a
                                     #x5f49fc0a149a4407 #x343259b671a5a82c
                                     #xfbc2bb458a6f981f #xc272b350a0a41a38
                                     #x3aaf1fd8ada32354 #x6cbb868b0b3c2717
                                     #xa2b569c88d2583fe #xf180c9d1bf027928
                                     #xaf37386bd64ba9f5 #x12bacab2790a8088
                                     #x4c0d3b0810435055 #xb2eeb9070e9436df
                                     #xc5b29067cea7d104 #xdcb425f1ff132461
                                     #x4f122cc5972bf126 #xac282fa651230886
                                     #xe7e537992f6393ef #xe61b3a2952b00735
                                     #x709c0a57ae302ce7 #xe02514ae416058d3
                                     #xc44c9dd7b37445de #x5a68c5408022ba92
                                     #x1c278cdca50c0bf0 #x6e5a9cf6f18712be
                                     #x86dce0b17f319ef3 #x2d34ec2040115d49
                                     #x4bcd183f7e409b69 #x2815d56ad4a9a3dc
                                     #x24698979f2141d0d #x0000000000000000
                                     #x1ec696a15fb73e59 #xd86b110b16784e2e
                                     #x8e7f8858b0e74a6d #x063e2e8713d05fe6
                                     #xe2c40ed3bbdb6d7a #xb1f1aeca89fc97ac
                                     #xe1db191e3cb3cc09 #x6418ee62c4eaf389
                                     #xc6ad87aa49cf7077 #xd6f65765ca7ec556
                                     #x9afb6c6dda3d9503 #x7ce05644888d9236
                                     #x8d609f95378feb1e #x23a9aa4e9c17d631
                                     #x6226c0e5d73aac6f #x56149953a69f0443
                                     #xeeb852c09d66d3ab #x2b0ac2a753c102af
                                     #x07c023376e03cb3c #x2ccae1903dc2c993
                                     #xd3d76e2f5ec63bc3 #x9e2458973356ff4c
                                     #xa66a5d32644ee9b1 #x0a427294356de137
                                     #x783f62be61e6f879 #x1344c70204d91452
                                     #x5b96c8f0fdf12e48 #xa90916ecc59bf613
                                     #xbe92e5142829880e #x727d102a548b194e
                                     #x1be7afebcb0fc0cc #x3e702b2244c8491b
                                     #xd5e940a84d166425 #x66f9f41f3e51c620
                                     #xabe80c913f20c3ba #xf07ec461c2d1edf2
                                     #xf361d3ac45b94c81 #x0521394a94b8fe95
                                     #xadd622162cf09c5c #xe97871f7f3651897
                                     #xf4a1f09b2bba87bd #x095d6559b2054044
                                     #x0bbc7f2448be75ed #x2af4cf172e129675
                                     #x157ae98517094bb4 #x9fda55274e856b96
                                     #x914713499283e0ee #xb952c623462a4332
                                     #x74433ead475b46a8 #x8b5eb112245fb4f8
                                     #xa34b6478f0f61724 #x11a5dd7ffe6221fb
                                     #xc16da49d27ccbb4b #x76a224d0bde07301
                                     #x8aa0bca2598c2022 #x4df336b86d90c48f
                                     #xea67663a740db9e4 #xef465f70e0b54771
                                     #x39b008152acb8227 #x7d1e5bf4f55e06ec
                                     #x105bd0cf83b1b521 #x775c2960c033e7db
                                     #x7e014c397236a79f #x811cc386113255cf
                                     #xeda7450d1a0e72d8 #x5889df3d7a998f3b
                                     #x2e2bfbedc779fc3a #xce0eef438619a4e9
                                     #x372d4e7bf6cd095f #x04df34fae96b6a4f
                                     #xf923a13870d4adb6 #xa1aa7e050a4d228d
                                     #xa8f71b5cb84862c9 #xb52e9a306097fde3
                                     #x0d8251a35b6e2a0b #x2257a7fee1c442eb
                                     #x73831d9a29588d94 #x51d4ba64c89ccf7f
                                     #x502ab7d4b54f5ba5 #x97793dce8153bf08
                                     #xe5042de4d5d8a646 #x9687307efc802bd2
                                     #xa05473b5779eb657 #xb4d097801d446939
                                     #xcff0e2f3fbca3033 #xc38cbee0dd778ee2
                                     #x464f499c252eb162 #xcad1dbb96f72cea6
                                     #xba4dd1eec142e241 #xb00fa37af42f0376)
                                    (#xcce4cd3aa968b245 #x089d5484e80b7faf
                                     #x638246c1b3548304 #xd2fe0ec8c2355492
                                     #xa7fbdf7ff2374eee #x4df1600c92337a16
                                     #x84e503ea523b12fb #x0790bbfd53ab0c4a
                                     #x198a780f38f6ea9d #x2ab30c8f55ec48cb
                                     #xe0f7fed6b2c49db5 #xb6ecf3f422cadbdc
                                     #x409c9a541358df11 #xd3ce8a56dfde3fe3
                                     #xc3e9224312c8c1a0 #x0d6dfa58816ba507
                                     #xddf3e1b179952777 #x04c02a42748bb1d9
                                     #x94c2abff9f2decb8 #x4f91752da8f8acf4
                                     #x78682befb169bf7b #xe1c77a48af2ff6c4
                                     #x0c5d7ec69c80ce76 #x4cc1e4928fd81167
                                     #xfeed3d24d9997b62 #x518bb6dfc3a54a23
                                     #x6dbf2d26151f9b90 #xb5bc624b05ea664f
                                     #xe86aaa525acfe21a #x4801ced0fb53a0be
                                     #xc91463e6c00868ed #x1027a815cd16fe43
                                     #xf67069a0319204cd #xb04ccc976c8abce7
                                     #xc0b9b3fc35e87c33 #xf380c77c58f2de65
                                     #x50bb3241de4e2152 #xdf93f490435ef195
                                     #xf1e0d25d62390887 #xaf668bfb1a3c3141
                                     #xbc11b251f00a7291 #x73a5eed47e427d47
                                     #x25bee3f6ee4c3b2e #x43cc0beb34786282
                                     #xc824e778dde3039c #xf97d86d98a327728
                                     #xf2b043e24519b514 #xe297ebf7880f4b57
                                     #x3a94a49a98fab688 #x868516cb68f0c419
                                     #xeffa11af0964ee50 #xa4ab4ec0d517f37d
                                     #xa9c6b498547c567a #x8e18424f80fbbbb6
                                     #x0bcdc53bcf2bc23c #x137739aaea3643d0
                                     #x2c1333ec1bac2ff0 #x8d48d3f0a7db0625
                                     #x1e1ac3f26b5de6d7 #xf520f81f16b2b95e
                                     #x9f0f6ec450062e84 #x0130849e1deb6b71
                                     #xd45e31ab8c7533a9 #x652279a2fd14e43f
                                     #x3209f01e70f1c927 #xbe71a770cac1a473
                                     #x0e3d6be7a64b1894 #x7ec8148cff29d840
                                     #xcb7476c7fac3be0f #x72956a4a63a91636
                                     #x37f95ec21991138f #x9e3fea5a4ded45f5
                                     #x7b38ba50964902e8 #x222e580bbde73764
                                     #x61e253e0899f55e6 #xfc8d2805e352ad80
                                     #x35994be3235ac56d #x09add01af5e014de
                                     #x5e8659a6780539c6 #xb17c48097161d796
                                     #x026015213acbd6e2 #xd1ae9f77e515e901
                                     #xb7dc776a3f21b0ad #xaba6a1b96eb78098
                                     #x9bcf4486248d9f5d #x582666c536455efd
                                     #xfdbdac9bfeb9c6f1 #xc47999be4163cdea
                                     #x765540081722a7ef #x3e548ed8ec710751
                                     #x3d041f67cb51bac2 #x7958af71ac82d40a
                                     #x36c9da5c047a78fe #xed9a048e33af38b2
                                     #x26ee7249c96c86bd #x900281bdeba65d61
                                     #x11172c8bd0fd9532 #xea0abf73600434f8
                                     #x42fc8f75299309f3 #x34a9cf7d3eb1ae1c
                                     #x2b838811480723ba #x5ce64c8742ceef24
                                     #x1adae9b01fd6570e #x3c349bf9d6bad1b3
                                     #x82453c891c7b75c0 #x97923a40b80d512b
                                     #x4a61dbf1c198765c #xb48ce6d518010d3e
                                     #xcfb45c858e480fd6 #xd933cbf30d1e96ae
                                     #xd70ea014ab558e3a #xc189376228031742
                                     #x9262949cd16d8b83 #xeb3a3bed7def5f89
                                     #x49314a4ee6b8cbcf #xdcc3652f647e4c06
                                     #xda635a4c2a3e2b3d #x470c21a940f3d35b
                                     #x315961a157d174b4 #x6672e81dda3459ac
                                     #x5b76f77a1165e36e #x445cb01667d36ec8
                                     #xc5491d205c88a69b #x456c34887a3805b9
                                     #xffddb9bac4721013 #x99af51a71e4649bf
                                     #xa15be01cbc7729d5 #x52db2760e485f7b0
                                     #x8c78576eba306d54 #xae560f6507d75a30
                                     #x95f22f6182c687c9 #x71c5fbf54489aba5
                                     #xca44f259e728d57e #x88b87d2ccebbdc8d
                                     #xbab18d32be4a15aa #x8be8ec93e99b611e
                                     #x17b713e89ebdf209 #xb31c5d284baa0174
                                     #xeeca9531148f8521 #xb8d198138481c348
                                     #x8988f9b2d350b7fc #xb9e11c8d996aa839
                                     #x5a4673e40c8e881f #x1687977683569978
                                     #xbf4123eed72acf02 #x4ea1f1b3b513c785
                                     #xe767452be16f91ff #x7505d1b730021a7c
                                     #xa59bca5ec8fc980c #xad069eda20f7e7a3
                                     #x38f4b1bba231606a #x60d2d77e94743e97
                                     #x9affc0183966f42c #x248e6768f3a7505f
                                     #xcdd449a4b483d934 #x87b59255751baf68
                                     #x1bea6d2e023d3c7f #x6b1f12455b5ffcab
                                     #x743555292de9710d #xd8034f6d10f5fddf
                                     #xc6198c9f7ba81b08 #xbb8109aca3a17edb
                                     #xfa2d1766ad12cabb #xc729080166437079
                                     #x9c5fff7b77269317 #x0000000000000000
                                     #x15d706c9a47624eb #x6fdf38072fd44d72
                                     #x5fb6dd3865ee52b7 #xa33bf53d86bcff37
                                     #xe657c1b5fc84fa8e #xaa962527735cebe9
                                     #x39c43525bfda0b1b #x204e4d2a872ce186
                                     #x7a083ece8ba26999 #x554b9c9db72efbfa
                                     #xb22cd9b656416a05 #x96a2bedea5e63a5a
                                     #x802529a826b0a322 #x8115ad363b5bc853
                                     #x8375b81701901eb1 #x3069e53f4a3a1fc5
                                     #xbd2136cfede119e0 #x18bafc91251d81ec
                                     #x1d4a524d4c7d5b44 #x05f0aedc6960daa8
                                     #x29e39d3072ccf558 #x70f57f6b5962c0d4
                                     #x989fd53903ad22ce #xf84d024797d91c59
                                     #x547b1803aac5908b #xf0d056c37fd263f6
                                     #xd56eb535919e58d8 #x1c7ad6d351963035
                                     #x2e7326cd2167f912 #xac361a443d1c8cd2
                                     #x697f076461942a49 #x4b515f6fdc731d2d
                                     #x8ad8680df4700a6f #x41ac1eca0eb3b460
                                     #x7d988533d80965d3 #xa8f6300649973d0b
                                     #x7765c4960ac9cc9e #x7ca801adc5e20ea2
                                     #xdea3700e5eb59ae4 #xa06b6482a19c42a4
                                     #x6a2f96db46b497da #x27def6d7d487edcc
                                     #x463ca5375d18b82a #xa6cb5be1efdc259f
                                     #x53eba3fef96e9cc1 #xce84d81b93a364a7
                                     #xf4107c810b59d22f #x333974806d1aa256
                                     #x0f0def79bba073e5 #x231edc95a00c5c15
                                     #xe437d494c64f2c6c #x91320523f64d3610
                                     #x67426c83c7df32dd #x6eefbc99323f2603
                                     #x9d6f7be56acdf866 #x5916e25b2bae358c
                                     #x7ff89012e2c2b331 #x035091bf2720bd93
                                     #x561b0d22900e4669 #x28d319ae6f279e29
                                     #x2f43a2533c8c9263 #xd09e1be9f8fe8270
                                     #xf740ed3e2c796fbc #xdb53ded237d5404c
                                     #x62b2c25faebfe875 #x0afd41a5d2c0a94d
                                     #x6412fd3ce0ff8f4e #xe3a76f6995e42026
                                     #x6c8fa9b808f4f0e1 #xc2d9a6dd0f23aad1
                                     #x8f28c6d19d10d0c7 #x85d587744fd0798a
                                     #xa20b71a39b579446 #x684f83fa7c7f4138
                                     #xe507500adba4471d #x3f640a46f19a6c20
                                     #x1247bd34f7dd28a1 #x2d23b77206474481
                                     #x93521002cc86e0f2 #x572b89bc8de52d18
                                     #xfb1d93f8b0f9a1ca #xe95a2ecc4724896b
                                     #x3ba420048511ddf9 #xd63e248ab6bee54b
                                     #x5dd6c8195f258455 #x06a03f634e40673b
                                     #x1f2a476c76b68da6 #x217ec9b49ac78af7
                                     #xecaa80102e4453c3 #x14e78257b99d4f9a)
                                    (#x20329b2cc87bba05 #x4f5eb6f86546a531
                                     #xd4f44775f751b6b1 #x8266a47b850dfa8b
                                     #xbb986aa15a6ca985 #xc979eb08f9ae0f99
                                     #x2da6f447a2375ea1 #x1e74275dcd7d8576
                                     #xbc20180a800bc5f8 #xb4a2f701b2dc65be
                                     #xe726946f981b6d66 #x48e6c453bf21c94c
                                     #x42cad9930f0a4195 #xefa47b64aacccd20
                                     #x71180a8960409a42 #x8bb3329bf6a44e0c
                                     #xd34c35de2d36dacc #xa92f5b7cbc23dc96
                                     #xb31a85aa68bb09c3 #x13e04836a73161d2
                                     #xb24dfc4129c51d02 #x8ae44b70b7da5acd
                                     #xe671ed84d96579a7 #xa4bb3417d66f3832
                                     #x4572ab38d56d2de8 #xb1b47761ea47215c
                                     #xe81c09cf70aba15d #xffbdb872ce7f90ac
                                     #xa8782297fd5dc857 #x0d946f6b6a4ce4a4
                                     #xe4df1f4f5b995138 #x9ebc71edca8c5762
                                     #x0a2c1dc0b02b88d9 #x3b503c115d9d7b91
                                     #xc64376a8111ec3a2 #xcec199a323c963e4
                                     #xdc76a87ec58616f7 #x09d596e073a9b487
                                     #x14583a9d7d560daf #xf4c6dc593f2a0cb4
                                     #xdd21d19584f80236 #x4a4836983ddde1d3
                                     #xe58866a41ae745f9 #xf591a5b27e541875
                                     #x891dc05074586693 #x5b068c651810a89e
                                     #xa30346bc0c08544f #x3dbf3751c684032d
                                     #x2a1e86ec785032dc #xf73f5779fca830ea
                                     #xb60c05ca30204d21 #x0cc316802b32f065
                                     #x8770241bdd96be69 #xb861e18199ee95db
                                     #xf805cad91418fcd1 #x29e70dccbbd20e82
                                     #xc7140f435060d763 #x0f3a9da0e8b0cc3b
                                     #xa2543f574d76408e #xbd7761e1c175d139
                                     #x4b1f4f737ca3f512 #x6dc2df1f2fc137ab
                                     #xf1d05c3967b14856 #xa742bf3715ed046c
                                     #x654030141d1697ed #x07b872abda676c7d
                                     #x3ce84eba87fa17ec #xc1fb0403cb79afdf
                                     #x3e46bc7105063f73 #x278ae987121cd678
                                     #xa1adb4778ef47cd0 #x26dd906c5362c2b9
                                     #x05168060589b44e2 #xfbfc41f9d79ac08f
                                     #x0e6de44ba9ced8fa #x9feb08068bf243a3
                                     #x7b341749d06b129b #x229c69e74a87929a
                                     #xe09ee6c4427c011b #x5692e30e725c4c3a
                                     #xda99a33e5e9f6e4b #x353dd85af453a36b
                                     #x25241b4c90e0fee7 #x5de987258309d022
                                     #xe230140fc0802984 #x93281e86a0c0b3c6
                                     #xf229d719a4337408 #x6f6c2dd4ad3d1f34
                                     #x8ea5b2fbae3f0aee #x8331dd90c473ee4a
                                     #x346aa1b1b52db7aa #xdf8f235e06042aa9
                                     #xcc6f6b68a1354b7b #x6c95a6f46ebf236a
                                     #x52d31a856bb91c19 #x1a35ded6d498d555
                                     #xf37eaef2e54d60c9 #x72e181a9a3c2a61c
                                     #x98537aad51952fde #x16f6c856ffaa2530
                                     #xd960281e9d1d5215 #x3a0745fa1ce36f50
                                     #x0b7b642bf1559c18 #x59a87eae9aec8001
                                     #x5e100c05408bec7c #x0441f98b19e55023
                                     #xd70dcc5534d38aef #x927f676de1bea707
                                     #x9769e70db925e3e5 #x7a636ea29115065a
                                     #x468b201816ef11b6 #xab81a9b73edff409
                                     #xc0ac7de88a07bb1e #x1f235eb68c0391b7
                                     #x6056b074458dd30f #xbe8eeac102f7ed67
                                     #xcd381283e04b5fba #x5cbefecec277c4e3
                                     #xd21b4c356c48ce0d #x1019c31664b35d8c
                                     #x247362a7d19eea26 #xebe582efb3299d03
                                     #x02aef2cb82fc289f #x86275df09ce8aaa8
                                     #x28b07427faac1a43 #x38a9b7319e1f47cf
                                     #xc82e92e3b8d01b58 #x06ef0b409b1978bc
                                     #x62f842bfc771fb90 #x9904034610eb3b1f
                                     #xded85ab5477a3e68 #x90d195a663428f98
                                     #x5384636e2ac708d8 #xcbd719c37b522706
                                     #xae9729d76644b0eb #x7c8c65e20a0c7ee6
                                     #x80c856b007f1d214 #x8c0b40302cc32271
                                     #xdbcedad51fe17a8a #x740e8ae938dbdea0
                                     #xa615c6dc549310ad #x19cc55f6171ae90b
                                     #x49b1bdb8fe5fdd8d #xed0a89af2830e5bf
                                     #x6a7aadb4f5a65bd6 #x7e22972988f05679
                                     #xf952b3325566e810 #x39fecedadf61530e
                                     #x6101c99f04f3c7ce #x2e5f7f6761b562ff
                                     #xf08725d226cf5c97 #x63af3b54860fef51
                                     #x8ff2cb10ef411e2f #x884ab9bb35267252
                                     #x4df04433e7ba8dae #x9afd8866d3690741
                                     #x66b9bb34de94abb3 #x9baaf18d92171380
                                     #x543c11c5f0a064a5 #x17a1b1bdbed431f1
                                     #xb5f58eeaf3a2717f #xc355f6c849858740
                                     #xec5df044694ef17e #xd83751f5dc6346d4
                                     #xfc4433520dfdacf2 #x0000000000000000
                                     #x5a51f58e596ebc5f #x3285aaf12e34cf16
                                     #x8d5c39db6dbd36b0 #x12b731dde64f7513
                                     #x94906c2d7aa7dfbb #x302b583aacc8e789
                                     #x9d45facd090e6b3c #x2165e2c78905aec4
                                     #x68d45f7f775a7349 #x189b2c1d5664fdca
                                     #xe1c99f2f030215da #x6983269436246788
                                     #x8489af3b1e148237 #xe94b702431d5b59c
                                     #x33d2d31a6f4adbd7 #xbfd9932a4389f9a6
                                     #xb0e30e8aab39359d #xd1e2c715afcaf253
                                     #x150f43763c28196e #xc4ed846393e2eb3d
                                     #x03f98b20c3823c5e #xfd134ab94c83b833
                                     #x556b682eb1de7064 #x36c4537a37d19f35
                                     #x7559f30279a5ca61 #x799ae58252973a04
                                     #x9c12832648707ffd #x78cd9c6913e92ec5
                                     #x1d8dac7d0effb928 #x439da0784e745554
                                     #x413352b3cc887dcb #xbacf134a1b12bd44
                                     #x114ebafd25cd494d #x2f08068c20cb763e
                                     #x76a07822ba27f63f #xeab2fb04f25789c2
                                     #xe3676de481fe3d45 #x1b62a73d95e6c194
                                     #x641749ff5c68832c #xa5ec4dfc97112cf3
                                     #xf6682e92bdd6242b #x3f11c59a44782bb2
                                     #x317c21d1edb6f348 #xd65ab5be75ad9e2e
                                     #x6b2dd45fb4d84f17 #xfaab381296e4d44e
                                     #xd0b5befeeeb4e692 #x0882ef0b32d7a046
                                     #x512a91a5a83b2047 #x963e9ee6f85bf724
                                     #x4e09cf132438b1f0 #x77f701c9fb59e2fe
                                     #x7ddb1c094b726a27 #x5f4775ee01f5f8bd
                                     #x9186ec4d223c9b59 #xfeeac1998f01846d
                                     #xac39db1ce4b89874 #xb75b7c21715e59e0
                                     #xafc0503c273aa42a #x6e3b543fec430bf5
                                     #x704f7362213e8e83 #x58ff0745db9294c0
                                     #x67eec2df9feabf72 #xa0facd9ccf8a6811
                                     #xb936986ad890811a #x95c715c63bd9cb7a
                                     #xca8060283a2c33c7 #x507de84ee9453486
                                     #x85ded6d05f6a96f6 #x1cdad5964f81ade9
                                     #xd5a33e9eb62fa270 #x40642b588df6690a
                                     #x7f75eec2c98e42b8 #x2cf18dace3494a60
                                     #x23cb100c0bf9865b #xeef3028febb2d9e1
                                     #x4425d2d394133929 #xaad6d05c7fa1e0c8
                                     #xad6ea2f7a5c68cb5 #xc2028f2308fb9381
                                     #x819f2f5b468fc6d5 #xc5bafd88d29cfffc
                                     #x47dc59f357910577 #x2b49ff07392e261d
                                     #x57c59ae5332258fb #x73b6f842e2bcb2dd
                                     #xcf96e04862b77725 #x4ca73dd8a6c4996f
                                     #x015779eb417e14c1 #x37932a9176af8bf4)
                                    (#x190a2c9b249df23e #x2f62f8b62263e1e9
                                     #x7a7f754740993655 #x330b7ba4d5564d9f
                                     #x4c17a16a46672582 #xb22f08eb7d05f5b8
                                     #x535f47f40bc148cc #x3aec5d27d4883037
                                     #x10ed0a1825438f96 #x516101f72c233d17
                                     #x13cc6f949fd04eae #x739853c441474bfd
                                     #x653793d90d3f5b1b #x5240647b96b0fc2f
                                     #x0c84890ad27623e0 #xd7189b32703aaea3
                                     #x2685de3523bd9c41 #x99317c5b11bffefa
                                     #x0d9baa854f079703 #x70b93648fbd48ac5
                                     #xa80441fce30bc6be #x7287704bdc36ff1e
                                     #xb65384ed33dc1f13 #xd36417343ee34408
                                     #x39cd38ab6e1bf10f #x5ab861770a1f3564
                                     #x0ebacf09f594563b #xd04572b884708530
                                     #x3cae9722bdb3af47 #x4a556b6f2f5cbaf2
                                     #xe1704f1f76c4bd74 #x5ec4ed7144c6dfcf
                                     #x16afc01d4c7810e6 #x283f113cd629ca7a
                                     #xaf59a8761741ed2d #xeed5a3991e215fac
                                     #x3bf37ea849f984d4 #xe413e096a56ce33c
                                     #x2c439d3a98f020d1 #x637559dc6404c46b
                                     #x9e6c95d1e5f5d569 #x24bb9836045fe99a
                                     #x44efa466dac8ecc9 #xc6eab2a5c80895d6
                                     #x803b50c035220cc4 #x0321658cba93c138
                                     #x8f9ebc465dc7ee1c #xd15a5137190131d3
                                     #x0fa5ec8668e5e2d8 #x91c979578d1037b1
                                     #x0642ca05693b9f70 #xefca80168350eb4f
                                     #x38d21b24f36a45ec #xbeab81e1af73d658
                                     #x8cbfd9cae7542f24 #xfd19cc0d81f11102
                                     #x0ac6430fbb4dbc90 #x1d76a09d6a441895
                                     #x2a01573ff1cbbfa1 #xb572e161894fde2b
                                     #x8124734fa853b827 #x614b1fdf43e6b1b0
                                     #x68ac395c4238cc18 #x21d837bfd7f7b7d2
                                     #x20c714304a860331 #x5cfaab726324aa14
                                     #x74c5ba4eb50d606e #xf3a3030474654739
                                     #x23e671bcf015c209 #x45f087e947b9582a
                                     #xd8bd77b418df4c7b #xe06f6c90ebb50997
                                     #x0bd96080263c0873 #x7e03f9410e40dcfe
                                     #xb8e94be4c6484928 #xfb5b0608e8ca8e72
                                     #x1a2b49179e0e3306 #x4e29e76961855059
                                     #x4f36c4e6fcf4e4ba #x49740ee395cf7bca
                                     #xc2963ea386d17f7d #x90d65ad810618352
                                     #x12d34c1b02a1fa4d #xfa44258775bb3a91
                                     #x18150f14b9ec46dd #x1491861e6b9a653d
                                     #x9a1019d7ab2c3fc2 #x3668d42d06fe13d7
                                     #xdcc1fbb25606a6d0 #x969490dd795a1c22
                                     #x3549b1a1bc6dd2ef #xc94f5e23a0ed770e
                                     #xb9f6686b5b39fdcb #xc4d4f4a6efeae00d
                                     #xe732851a1fff2204 #x94aad6de5eb869f9
                                     #x3f8ff2ae07206e7f #xfe38a9813b62d03a
                                     #xa7a1ad7a8bee2466 #x7b6056c8dde882b6
                                     #x302a1e286fc58ca7 #x8da0fa457a259bc7
                                     #xb3302b64e074415b #x5402ae7eff8b635f
                                     #x08f8050c9cafc94b #xae468bf98a3059ce
                                     #x88c355cca98dc58f #xb10e6d67c7963480
                                     #xbad70de7e1aa3cf3 #xbfb4a26e320262bb
                                     #xcb711820870f02d5 #xce12b7a954a75c9d
                                     #x563ce87dd8691684 #x9f73b65e7884618a
                                     #x2b1e74b06cba0b42 #x47cec1ea605b2df1
                                     #x1c698312f735ac76 #x5fdbcefed9b76b2c
                                     #x831a354c8fb1cdfc #x820516c312c0791f
                                     #xb74ca762aeadabf0 #xfc06ef821c80a5e1
                                     #x5723cbf24518a267 #x9d4df05d5f661451
                                     #x588627742dfd40bf #xda8331b73f3d39a0
                                     #x17b0e392d109a405 #xf965400bcf28fba9
                                     #x7c3dbf4229a2a925 #x023e460327e275db
                                     #x6cd0b55a0ce126b3 #xe62da695828e96e7
                                     #x42ad6e63b3f373b9 #xe50cc319381d57df
                                     #xc5cbd729729b54ee #x46d1e265fd2a9912
                                     #x6428b056904eeff8 #x8be23040131e04b7
                                     #x6709d5da2add2ec0 #x075de98af44a2b93
                                     #x8447dcc67bfbe66f #x6616f655b7ac9a23
                                     #xd607b8bded4b1a40 #x0563af89d3a85e48
                                     #x3db1b4ad20c21ba4 #x11f22997b8323b75
                                     #x292032b34b587e99 #x7f1cdace9331681d
                                     #x8e819fc9c0b65aff #xa1e3677fe2d5bb16
                                     #xcd33d225ee349da5 #xd9a2543b85aef898
                                     #x795e10cbfa0af76d #x25a4bbb9992e5d79
                                     #x78413344677b438e #xf0826688cef68601
                                     #xd27b34bba392f0eb #x551d8df162fad7bc
                                     #x1e57c511d0d7d9ad #xdeffbdb171e4d30b
                                     #xf4feea8e802f6caa #xa480c8f6317de55e
                                     #xa0fc44f07fa40ff5 #x95b5f551c3c9dd1a
                                     #x22f952336d6476ea #x0000000000000000
                                     #xa6be8ef5169f9085 #xcc2cf1aa73452946
                                     #x2e7ddb39bf12550a #xd526dd3157d8db78
                                     #x486b2d6c08becf29 #x9b0f3a58365d8b21
                                     #xac78cdfaadd22c15 #xbc95c7e28891a383
                                     #x6a927f5f65dab9c3 #xc3891d2c1ba0cb9e
                                     #xeaa92f9f50f8b507 #xcf0d9426c9d6e87e
                                     #xca6e3baf1a7eb636 #xab25247059980786
                                     #x69b31ad3df4978fb #xe2512a93cc577c4c
                                     #xff278a0ea61364d9 #x71a615c766a53e26
                                     #x89dc764334fc716c #xf87a638452594f4a
                                     #xf2bc208be914f3da #x8766b94ac1682757
                                     #xbbc82e687cdb8810 #x626a7a53f9757088
                                     #xa2c202f358467a2e #x4d0882e5db169161
                                     #x09e7268301de7da8 #xe897699c771ac0dc
                                     #xc8507dac3d9cc3ed #xc0a878a0a1330aa6
                                     #x978bb352e42ba8c1 #xe9884a13ea6b743f
                                     #x279afdbabecc28a2 #x047c8c064ed9eaab
                                     #x507e2278b15289f4 #x599904fbb08cf45c
                                     #xbd8ae46d15e01760 #x31353da7f2b43844
                                     #x8558ff49e68a528c #x76fbfc4d92ef15b5
                                     #x3456922e211c660c #x86799ac55c1993b4
                                     #x3e90d1219a51da9c #x2d5cbeb505819432
                                     #x982e5fd48cce4a19 #xdb9c1238a24c8d43
                                     #xd439febecaa96f9b #x418c0bef0960b281
                                     #x158ea591f6ebd1de #x1f48e69e4da66d4e
                                     #x8afd13cf8e6fb054 #xf5e1c9011d5ed849
                                     #xe34e091c5126c8af #xad67ee7530a398f6
                                     #x43b24dec2e82c75a #x75da99c1287cd48d
                                     #x92e81cdb3783f689 #xa3dd217cc537cecd
                                     #x60543c50de970553 #x93f73f54aaf2426a
                                     #xa91b62737e7a725d #xf19d4507538732e2
                                     #x77e4dfc20f9ea156 #x7d229ccdb4d31dc6
                                     #x1b346a98037f87e5 #xedf4c615a4b29e94
                                     #x4093286094110662 #xb0114ee85ae78063
                                     #x6ff1d0d6b672e78b #x6dcf96d591909250
                                     #xdfe09e3eec9567e8 #x3214582b4827f97c
                                     #xb46dc2ee143e6ac8 #xf6c0ac8da7cd1971
                                     #xebb60c10cd8901e4 #xf7df8f023abcad92
                                     #x9c52d3d2c217a0b2 #x6b8d5cd0f8ab0d20
                                     #x3777f7a29b8fa734 #x011f238f9d71b4e3
                                     #xc1b75b2f3c42be45 #x5de588fdfe551ef7
                                     #x6eeef3592b035368 #xaa3a07ffc4e9b365
                                     #xecebe59a39c32a77 #x5ba742f8976e8187
                                     #x4b4a48e0b22d0e11 #xddded83dcb771233
                                     #xa59feb79ac0c51bd #xc7f5912a55792135)
                                    (#x6d6ae04668a9b08a #x3ab3f04b0be8c743
                                     #xe51e166b54b3c908 #xbe90a9eb35c2f139
                                     #xb2c7066637f2bec1 #xaa6945613392202c
                                     #x9a28c36f3b5201eb #xddce5a93ab536994
                                     #x0e34133ef6382827 #x52a02ba1ec55048b
                                     #xa2f88f97c4b2a177 #x8640e513ca2251a5
                                     #xcdf1d36258137622 #xfe6cb708dedf8ddb
                                     #x8a174a9ec8121e5d #x679896036b81560e
                                     #x59ed033395795fee #x1dd778ab8b74edaf
                                     #xee533ef92d9f926d #x2a8c79baf8a8d8f5
                                     #x6bcf398e69b119f6 #xe20491742fafdd95
                                     #x276488e0809c2aec #xea955b82d88f5cce
                                     #x7102c63a99d9e0c4 #xf9763017a5c39946
                                     #x429fa2501f151b3d #x4659c72bea05d59e
                                     #x984b7fdccf5a6634 #xf742232953fbb161
                                     #x3041860e08c021c7 #x747bfd9616cd9386
                                     #x4bb1367192312787 #x1b72a1638a6c44d3
                                     #x4a0e68a6e8359a66 #x169a5039f258b6ca
                                     #xb98a2ef44edee5a4 #xd9083fe85e43a737
                                     #x967f6ce239624e13 #x8874f62d3c1a7982
                                     #x3c1629830af06e3f #x9165ebfd427e5a8e
                                     #xb5dd81794ceeaa5c #x0de8f15a7834f219
                                     #x70bd98ede3dd5d25 #xaccc9ca9328a8950
                                     #x56664eda1945ca28 #x221db34c0f8859ae
                                     #x26dbd637fa98970d #x1acdffb4f068f932
                                     #x4585254f64090fa0 #x72de245e17d53afa
                                     #x1546b25d7c546cf4 #x207e0ffffb803e71
                                     #xfaaad2732bcf4378 #xb462dfae36ea17bd
                                     #xcf926fd1ac1b11fd #xe0672dc7dba7ba4a
                                     #xd3fa49ad5d6b41b3 #x8ba81449b216a3bc
                                     #x14f9ec8a0650d115 #x40fc1ee3eb1d7ce2
                                     #x23a2ed9b758ce44f #x782c521b14fddc7e
                                     #x1c68267cf170504e #xbcf31558c1ca96e6
                                     #xa781b43b4ba6d235 #xf6fd7dfe29ff0c80
                                     #xb0a4bad5c3fad91e #xd199f51ea963266c
                                     #x414340349119c103 #x5405f269ed4dadf7
                                     #xabd61bb649969dcd #x6813dbeae7bdc3c8
                                     #x65fb2ab09f8931d1 #xf1e7fae152e3181d
                                     #xc1a67cef5a2339da #x7a4feea8e0f5bba1
                                     #x1e0b9acf05783791 #x5b8ebf8061713831
                                     #x80e53cdbcb3af8d9 #x7e898bd315e57502
                                     #xc6bcfbf0213f2d47 #x95a38e86b76e942d
                                     #x092e94218d243cba #x8339debf453622e7
                                     #xb11be402b9fe64ff #x57d9100d634177c9
                                     #xcc4e8db52217cbc3 #x3b0cae9c71ec7aa2
                                     #xfb158ca451cbfe99 #x2b33276d82ac6514
                                     #x01bf5ed77a04bde1 #xc5601994af33f779
                                     #x75c4a3416cc92e67 #xf3844652a6eb7fc2
                                     #x3487e375fdd0ef64 #x18ae430704609eed
                                     #x4d14efb993298efb #x815a620cb13e4538
                                     #x125c354207487869 #x9eeea614ce42cf48
                                     #xce2d3106d61fac1c #xbbe99247bad6827b
                                     #x071a871f7b1c149d #x2e4a1cc10db81656
                                     #x77a71ff298c149b8 #x06a5d9c80118a97c
                                     #xad73c27e488e34b1 #x443a7b981e0db241
                                     #xe3bbcfa355ab6074 #x0af276450328e684
                                     #x73617a896dd1871b #x58525de4ef7de20f
                                     #xb7be3dcab8e6cd83 #x19111dd07e64230c
                                     #x842359a03e2a367a #x103f89f1f3401fb6
                                     #xdc710444d157d475 #xb835702334da5845
                                     #x4320fc876511a6dc #xd026abc9d3679b8d
                                     #x17250eee885c0b2b #x90dab52a387ae76f
                                     #x31fed8d972c49c26 #x89cba8fa461ec463
                                     #x2ff5421677bcabb7 #x396f122f85e41d7d
                                     #xa09b332430bac6a8 #xc888e8ced7070560
                                     #xaeaf201ac682ee8f #x1180d7268944a257
                                     #xf058a43628e7a5fc #xbd4c4b8fbbce2b07
                                     #xa1246df34abe7b49 #x7d5569b79be9af3c
                                     #xa9b5a705bd9efa12 #xdb6b835baa4bc0e8
                                     #x05793bac8f147342 #x21c1512881848390
                                     #xfdb0556c50d357e5 #x613d4fcb6a99ff72
                                     #x03dce2648e0cda3e #xe949b9e6568386f0
                                     #xfc0f0bbb2ad7ea04 #x6a70675913b5a417
                                     #x7f36d5046fe1c8e3 #x0c57af8d02304ff8
                                     #x32223abdfcc84618 #x0891caf6f720815b
                                     #xa63eeaec31a26fd4 #x2507345374944d33
                                     #x49d28ac266394058 #xf5219f9aa7f3d6be
                                     #x2d96fea583b4cc68 #x5a31e1571b7585d0
                                     #x8ed12fe53d02d0fe #xdfade6205f5b0e4b
                                     #x4cabb16ee92d331a #x04c6657bf510cea3
                                     #xd73c2cd6a87b8f10 #xe1d87310a1a307ab
                                     #x6cd5be9112ad0d6b #x97c032354366f3f2
                                     #xd4e0ceb22677552e #x0000000000000000
                                     #x29509bde76a402cb #xc27a9e8bd42fe3e4
                                     #x5ef7842cee654b73 #xaf107ecdbc86536e
                                     #x3fcacbe784fcb401 #xd55f90655c73e8cf
                                     #xe6c2f40fdabf1336 #xe8f6e7312c873b11
                                     #xeb2a0555a28be12f #xe4a148bc2eb774e9
                                     #x9b979db84156bc0a #x6eb60222e6a56ab4
                                     #x87ffbbc4b026ec44 #xc703a5275b3b90a6
                                     #x47e699fc9001687f #x9c8d1aa73a4aa897
                                     #x7cea3760e1ed12dd #x4ec80ddd1d2554c5
                                     #x13e36b957d4cc588 #x5d2b66486069914d
                                     #x92b90999cc7280b0 #x517cc9c56259deb5
                                     #xc937b619ad03b881 #xec30824ad997f5b2
                                     #xa45d565fc5aa080b #xd6837201d27f32f1
                                     #x635ef3789e9198ad #x531f75769651b96a
                                     #x4f77530a6721e924 #x486dd4151c3dfdb9
                                     #x5f48dafb9461f692 #x375b011173dc355a
                                     #x3da9775470f4d3de #x8d0dcd81b30e0ac0
                                     #x36e45fc609d888bb #x55baacbe97491016
                                     #x8cb29356c90ab721 #x76184125e2c5f459
                                     #x99f4210bb55edbd5 #x6f095cf59ca1d755
                                     #x9f51f8c3b44672a9 #x3538bda287d45285
                                     #x50c39712185d6354 #xf23b1885dcefc223
                                     #x79930ccc6ef9619f #xed8fdc9da3934853
                                     #xcb540aaa590bdf5e #x5c94389f1a6d2cac
                                     #xe77daad8a0bbaed7 #x28efc5090ca0bf2a
                                     #xbf2ff73c4fc64cd8 #xb37858b14df60320
                                     #xf8c96ec0dfc724a7 #x828680683f329f06
                                     #x941cd051cd6a29cc #xc3c5c05cae2b5e05
                                     #xb601631dc2e27062 #xc01922382027843b
                                     #x24b86a840e90f0d2 #xd245177a276ffc52
                                     #x0f8b4de98c3c95c6 #x3e759530fef809e0
                                     #x0b4d2892792c5b65 #xc4df4743d5374a98
                                     #xa5e20888bfaeb5ea #xba56cc90c0d23f9a
                                     #x38d04cf8ffe0a09c #x62e1adafe495254c
                                     #x0263bcb3f40867df #xcaeb547d230f62bf
                                     #x6082111c109d4293 #xdad4dd8cd04f7d09
                                     #xefec602e579b2f8c #x1fb4c4187f7c8a70
                                     #xffd3e9dfa4db303a #x7bf0b07f9af10640
                                     #xf49ec14dddf76b5f #x8f6e713247066d1f
                                     #x339d646a86ccfbf9 #x64447467e58d8c30
                                     #x2c29a072f9b07189 #xd8b7613f24471ad6
                                     #x6627c8d41185ebef #xa347d140beb61c96
                                     #xde12b8f7255fb3aa #x9d324470404e1576
                                     #x9306574eb6763d51 #xa80af9d2c79a47f3
                                     #x859c0777442e8b9b #x69ac853d9db97e29)
                                    (#xc3407dfc2de6377e #x5b9e93eea4256f77
                                     #xadb58fdd50c845e0 #x5219ff11a75bed86
                                     #x356b61cfd90b1de9 #xfb8f406e25abe037
                                     #x7a5a0231c0f60796 #x9d3cd216e1f5020b
                                     #x0c6550fb6b48d8f3 #xf57508c427ff1c62
                                     #x4ad35ffa71cb407d #x6290a2da1666aa6d
                                     #xe284ec2349355f9f #xb3c307c53d7c84ec
                                     #x05e23c0468365a02 #x190bac4d6c9ebfa8
                                     #x94bbbee9e28b80fa #xa34fc777529cb9b5
                                     #xcc7b39f095bcd978 #x2426addb0ce532e3
                                     #x7e79329312ce4fc7 #xab09a72eebec2917
                                     #xf8d15499f6b9d6c2 #x1a55b8babf8c895d
                                     #xdb8add17fb769a85 #xb57f2f368658e81b
                                     #x8acd36f18f3f41f6 #x5ce3b7bba50f11d3
                                     #x114dcc14d5ee2f0a #xb91a7fcded1030e8
                                     #x81d5425fe55de7a1 #xb6213bc1554adeee
                                     #x80144ef95f53f5f2 #x1e7688186db4c10c
                                     #x3b912965db5fe1bc #xc281715a97e8252d
                                     #x54a5d7e21c7f8171 #x4b12535ccbc5522e
                                     #x1d289cefbea6f7f9 #x6ef5f2217d2e729e
                                     #xe6a7dc819b0d17ce #x1b94b41c05829b0e
                                     #x33d7493c622f711e #xdcf7f942fa5ce421
                                     #x600fba8b7f7a8ecb #x46b60f011a83988e
                                     #x235b898e0dcf4c47 #x957ab24f588592a9
                                     #x4354330572b5c28c #xa5f3ef84e9b8d542
                                     #x8c711e02341b2d01 #x0b1874ae6a62a657
                                     #x1213d8e306fc19ff #xfe6d7c6a4d9dba35
                                     #x65ed868f174cd4c9 #x88522ea0e6236550
                                     #x899322065c2d7703 #xc01e690bfef4018b
                                     #x915982ed8abddaf8 #xbe675b98ec3a4e4c
                                     #xa996bf7f82f00db1 #xe1daf8d49a27696a
                                     #x2effd5d3dc8986e7 #xd153a51f2b1a2e81
                                     #x18caa0ebd690adfb #x390e3134b243c51a
                                     #x2778b92cdff70416 #x029f1851691c24a6
                                     #x5e7cafeacc133575 #xfa4e4cc89fa5f264
                                     #x5a5f9f481e2b7d24 #x484c47ab18d764db
                                     #x400a27f2a1a7f479 #xaeeb9b2a83da7315
                                     #x721c626879869734 #x042330a2d2384851
                                     #x85f672fd3765aff0 #xba446b3a3e02061d
                                     #x73dd6ecec3888567 #xffac70ccf793a866
                                     #xdfa9edb5294ed2d4 #x6c6aea7014325638
                                     #x834a5a0e8c41c307 #xcdba35562fb2cb2b
                                     #x0ad97808d06cb404 #x0f3b440cb85aee06
                                     #xe5f9c876481f213b #x98deee1289c35809
                                     #x59018bbfcd394bd1 #xe01bf47220297b39
                                     #xde68e1139340c087 #x9fa3ca4788e926ad
                                     #xbb85679c840c144e #x53d8f3b71d55ffd5
                                     #x0da45c5dd146caa0 #x6f34fe87c72060cd
                                     #x57fbc315cf6db784 #xcee421a1fca0fdde
                                     #x3d2d0196607b8d4b #x642c8a29ad42c69a
                                     #x14aff010bdd87508 #xac74837beac657b3
                                     #x3216459ad821634d #x3fb219c70967a9ed
                                     #x06bc28f3bb246cf7 #xf2082c9126d562c6
                                     #x66b39278c45ee23c #xbd394f6f3f2878b9
                                     #xfd33689d9e8f8cc0 #x37f4799eb017394f
                                     #x108cc0b26fe03d59 #xda4bd1b1417888d6
                                     #xb09d1332ee6eb219 #x2f3ed975668794b4
                                     #x58c0871977375982 #x7561463d78ace990
                                     #x09876cff037e82f1 #x7fb83e35a8c05d94
                                     #x26b9b58a65f91645 #xef20b07e9873953f
                                     #x3148516d0b3355b8 #x41cb2b541ba9e62a
                                     #x790416c613e43163 #xa011d380818e8f40
                                     #x3a5025c36151f3ef #xd57095bdf92266d0
                                     #x498d4b0da2d97688 #x8b0c3a57353153a5
                                     #x21c491df64d368e1 #x8f2f0af5e7091bf4
                                     #x2da1c1240f9bb012 #xc43d59a92ccc49da
                                     #xbfa6573e56345c1f #x828b56a8364fd154
                                     #x9a41f643e0df7caf #xbcf843c985266aea
                                     #x2b1de9d7b4bfdce5 #x20059d79dedd7ab2
                                     #x6dabe6d6ae3c446b #x45e81bf6c991ae7b
                                     #x6351ae7cac68b83e #xa432e32253b6c711
                                     #xd092a9b991143cd2 #xcac711032e98b58f
                                     #xd8d4c9e02864ac70 #xc5fc550f96c25b89
                                     #xd7ef8dec903e4276 #x67729ede7e50f06f
                                     #xeac28c7af045cf3d #xb15c1f945460a04a
                                     #x9cfddeb05bfb1058 #x93c69abce3a1fe5e
                                     #xeb0380dc4a4bdd6e #xd20db1e8f8081874
                                     #x229a8528b7c15e14 #x44291750739fbc28
                                     #xd3ccbd4e42060a27 #xf62b1c33f4ed2a97
                                     #x86a8660ae4779905 #xd62e814a2a305025
                                     #x477703a7a08d8add #x7b9b0e977af815c5
                                     #x78c51a60a9ea2330 #xa6adfb733aaae3b7
                                     #x97e5aa1e3199b60f #x0000000000000000
                                     #xf4b404629df10e31 #x5564db44a6719322
                                     #x9207961a59afec0d #x9624a6b88b97a45c
                                     #x363575380a192b1c #x2c60cd82b595a241
                                     #x7d272664c1dc7932 #x7142769faa94a1c1
                                     #xa1d0df263b809d13 #x1630e841d4c451ae
                                     #xc1df65ad44fa13d8 #x13d2d445bcf20bac
                                     #xd915c546926abe23 #x38cf3d92084dd749
                                     #xe766d0272103059d #xc7634d5effde7f2f
                                     #x077d2455012a7ea4 #xedbfa82ff16fb199
                                     #xaf2a978c39d46146 #x42953fa3c8bbd0df
                                     #xcb061da59496a7dc #x25e7a17db6eb20b0
                                     #x34aa6d6963050fba #xa76cf7d580a4f1e4
                                     #xf7ea10954ee338c4 #xfcf2643b24819e93
                                     #xcf252d0746aeef8d #x4ef06f58a3f3082c
                                     #x563acfb37563a5d7 #x5086e740ce47c920
                                     #x2982f186dda3f843 #x87696aac5e798b56
                                     #x5d22bb1d1f010380 #x035e14f7d31236f5
                                     #x3cec0d30da759f18 #xf3c920379cdb7095
                                     #xb8db736b571e22bb #xdd36f5e44052f672
                                     #xaac8ab8851e23b44 #xa857b3d938fe1fe2
                                     #x17f1e4e76eca43fd #xec7ea4894b61a3ca
                                     #x9e62c6e132e734fe #xd4b1991b432c7483
                                     #x6ad6c283af163acf #x1ce9904904a8e5aa
                                     #x5fbda34c761d2726 #xf910583f4cb7c491
                                     #xc6a241f845d06d7c #x4f3163fe19fd1a7f
                                     #xe99c988d2357f9c8 #x8eee06535d0709a7
                                     #x0efa48aa0254fc55 #xb4be23903c56fa48
                                     #x763f52caabbedf65 #xeee1bcd8227d876c
                                     #xe345e085f33b4dcc #x3e731561b369bbbe
                                     #x2843fd2067adea10 #x2adce5710eb1ceb6
                                     #xb7e03767ef44ccbd #x8db012a48e153f52
                                     #x61ceb62dc5749c98 #xe85d942b9959eb9b
                                     #x4c6f7709caef2c8a #x84377e5b8d6bbda3
                                     #x30895dcbb13d47eb #x74a04a9bc2a2fbc3
                                     #x6b17ce251518289c #xe438c4d0f2113368
                                     #x1fb784bed7bad35f #x9b80fae55ad16efc
                                     #x77fe5e6c11b0cd36 #xc858095247849129
                                     #x08466059b97090a2 #x01c10ca6ba0e1253
                                     #x6988d6747c040c3a #x6849dad2c60a1e69
                                     #x5147ebe67449db73 #xc99905f4fd8a837a
                                     #x991fe2b433cd4a5a #xf09734c04fc94660
                                     #xa28ecbd1e892abe6 #xf1563866f5c75433
                                     #x4dae7baf70e13ed9 #x7ce62ac27bd26b61
                                     #x70837a39109ab392 #x90988e4b30b3c8ab
                                     #xb2020b63877296bf #x156efcb607d6675b)
                                    (#xe63f55ce97c331d0 #x25b506b0015bba16
                                     #xc8706e29e6ad9ba8 #x5b43d3775d521f6a
                                     #x0bfa3d577035106e #xab95fc172afb0e66
                                     #xf64b63979e7a3276 #xf58b4562649dad4b
                                     #x48f7c3dbae0c83f1 #xff31916642f5c8c5
                                     #xcbb048dc1c4a0495 #x66b8f83cdf622989
                                     #x35c130e908e2b9b0 #x7c761a61f0b34fa1
                                     #x3601161cf205268d #x9e54ccfe2219b7d6
                                     #x8b7d90a538940837 #x9cd403588ea35d0b
                                     #xbc3c6fea9ccc5b5a #xe5ff733b6d24aeed
                                     #xceed22de0f7eb8d2 #xec8581cab1ab545e
                                     #xb96105e88ff8e71d #x8ca03501871a5ead
                                     #x76ccce65d6db2a2f #x5883f582a7b58057
                                     #x3f7be4ed2e8adc3e #x0fe7be06355cd9c9
                                     #xee054e6c1d11be83 #x1074365909b903a6
                                     #x5dde9f80b4813c10 #x4a770c7d02b6692c
                                     #x5379c8d5d7809039 #xb4067448161ed409
                                     #x5f5e5026183bd6cd #xe898029bf4c29df9
                                     #x7fb63c940a54d09c #xc5171f897f4ba8bc
                                     #xa6f28db7b31d3d72 #x2e4f3be7716eaa78
                                     #x0d6771a099e63314 #x82076254e41bf284
                                     #x2f0fd2b42733df98 #x5c9e76d3e2dc49f0
                                     #x7aeb569619606cdb #x83478b07b2468764
                                     #xcfadcb8d5923cd32 #x85dac7f05b95a41e
                                     #xb5469d1b4043a1e9 #xb821ecbbd9a592fd
                                     #x1b8e0b0e798c13c8 #x62a57b6d9a0be02e
                                     #xfcf1b793b81257f8 #x9d94ea0bd8fe28eb
                                     #x4cea408aeb654a56 #x23284a47e888996c
                                     #x2d8f1d128b893545 #xf4cbac3132c0d8ab
                                     #xbd7c86b9ca912eba #x3a268eef3dbe6079
                                     #xf0d62f6077a9110c #x2735c916ade150cb
                                     #x89fd5f03942ee2ea #x1acee25d2fd16628
                                     #x90f39bab41181bff #x430dfe8cde39939f
                                     #xf70b8ac4c8274796 #x1c53aeaac6024552
                                     #x13b410acf35e9c9b #xa532ab4249faa24f
                                     #x2b1251e5625a163f #xd7e3e676da4841c7
                                     #xa7b264e4e5404892 #xda8497d643ae72d3
                                     #x861ae105a1723b23 #x38a6414991048aa4
                                     #x6578dec92585b6b4 #x0280cfa6acbaeadd
                                     #x88bdb650c273970a #x9333bd5ebbff84c2
                                     #x4e6a8f2c47dfa08b #x321c954db76cef2a
                                     #x418d312a72837942 #xb29b38bfffcdf773
                                     #x6c022c38f90a4c07 #x5a033a240b0f6a8a
                                     #x1f93885f3ce5da6f #xc38a537e96988bc6
                                     #x39e6a81ac759ff44 #x29929e43cee0fce2
                                     #x40cdd87924de0ca2 #xe9d8ebc8a29fe819
                                     #x0c2798f3cfbb46f4 #x55e484223e53b343
                                     #x4650948ecd0d2fd8 #x20e86cb2126f0651
                                     #x6d42c56baf5739e7 #xa06fc1405ace1e08
                                     #x7babbfc54f3d193b #x424d17df8864e67f
                                     #xd8045870ef14980e #xc6d7397c85ac3781
                                     #x21a885e1443273b1 #x67f8116f893f5c69
                                     #x24f5efe35706cff6 #xd56329d076f2ab1a
                                     #x5e1eb9754e66a32d #x28d2771098bd8902
                                     #x8f6013f47dfdc190 #x17a993fdb637553c
                                     #xe0a219397e1012aa #x786b9930b5da8606
                                     #x6e82e39e55b0a6da #x875a0856f72f4ec3
                                     #x3741ff4fa458536d #xac4859b3957558fc
                                     #x7ef6d5c75c09a57c #xc04a758b6c7f14fb
                                     #xf9acdd91ab26ebbf #x7391a467c5ef9668
                                     #x335c7c1ee1319aca #xa91533b18641e4bb
                                     #xe4bf9a683b79db0d #x8e20faa72ba0b470
                                     #x51f907737b3a7ae4 #x2268a314bed5ec8c
                                     #xd944b123b949edee #x31dcb3b84d8b7017
                                     #xd3fe65279f218860 #x097af2f1dc8ffab3
                                     #x9b09a6fc312d0b91 #xcc6ded78a3c4520f
                                     #x3481d9ba5ebfcc50 #x4f2a667f1182d56b
                                     #xdfd9fdd4509ace94 #x26752045fbbc252b
                                     #xbffc491f662bc467 #xdd593272fc202449
                                     #x3cbbc218d46d4303 #x91b372f817456e1f
                                     #x681faf69bc6385a0 #xb686bbeebaa43ed4
                                     #x1469b5084cd0ca01 #x98c98009cbca94ac
                                     #x6438379a73d8c354 #xc2caba2dc0c5fe26
                                     #x3e3b0dbe78d7a9de #x50b9ee202d670f04
                                     #x4590b27b37eab0e5 #x6025b4cb36b10af3
                                     #xfb2c1237079c0162 #xa12f28130c936be8
                                     #x4b37e52e54eb1ccc #x083a1ba28ad28f53
                                     #xc10a9cd83a22611b #x9f1425ad7444c236
                                     #x069d4cf7e9d3237a #xedc56899e7f621be
                                     #x778c273680865fcf #x309c5aeb1bd605f7
                                     #x8de0dc52d1472b4d #xf8ec34c2fd7b9e5f
                                     #xea18cd3d58787724 #xaad515447ca67b86
                                     #x9989695a9d97e14c #x0000000000000000
                                     #xf196c63321f464ec #x71116bc169557cb5
                                     #xaf887f466f92c7c1 #x972e3e0ffe964d65
                                     #x190ec4a8d536f915 #x95aef1a9522ca7b8
                                     #xdc19db21aa7d51a9 #x94ee18fa0471d258
                                     #x8087adf248a11859 #xc457f6da2916dd5c
                                     #xfa6cfb6451c17482 #xf256e0c6db13fbd1
                                     #x6a9f60cf10d96f7d #x4daaa9d9bd383fb6
                                     #x03c026f5fae79f3d #xde99148706c7bb74
                                     #x2a52b8b6340763df #x6fc20acd03edd33a
                                     #xd423c08320afdefa #xbbe1ca4e23420dc0
                                     #x966ed75ca8cb3885 #xeb58246e0e2502c4
                                     #x055d6a021334bc47 #xa47242111fa7d7af
                                     #xe3623fcc84f78d97 #x81c744a11efc6db9
                                     #xaec8961539cfb221 #xf31609958d4e8e31
                                     #x63e5923ecc5695ce #x47107ddd9b505a38
                                     #xa3afe7b5a0298135 #x792b7063e387f3e6
                                     #x0140e953565d75e0 #x12f4f9ffa503e97b
                                     #x750ce8902c3cb512 #xdbc47e8515f30733
                                     #x1ed3610c6ab8af8f #x5239218681dde5d9
                                     #xe222d69fd2aaf877 #xfe71783514a8bd25
                                     #xcaf0a18f4a177175 #x61655d9860ec7f13
                                     #xe77fbc9dc19e4430 #x2ccff441ddd440a5
                                     #x16e97aaee06a20dc #xa855dae2d01c915b
                                     #x1d1347f9905f30b2 #xb7c652bdecf94b34
                                     #xd03e43d265c6175d #xfdb15ec0ee4f2218
                                     #x57644b8492e9599e #x07dda5a4bf8e569a
                                     #x54a46d71680ec6a3 #x5624a2d7c4b42c7e
                                     #xbebca04c3076b187 #x7d36f332a6ee3a41
                                     #x3b6667bc6be31599 #x695f463aea3ef040
                                     #xad08b0e0c3282d1c #xb15b1e4a052a684e
                                     #x44d05b2861b7c505 #x15295c5b1a8dbfe1
                                     #x744c01c37a61c0f2 #x59c31cd1f1e8f5b7
                                     #xef45a73f4b4ccb63 #x6bdf899c46841a9d
                                     #x3dfb2b4b823036e3 #xa2ef0ee6f674f4d5
                                     #x184e2dfb836b8cf5 #x1134df0a5fe47646
                                     #xbaa1231d751f7820 #xd17eaa81339b62bd
                                     #xb01bf71953771dae #x849a2ea30dc8d1fe
                                     #x705182923f080955 #x0ea757556301ac29
                                     #x041d83514569c9a7 #x0abad4042668658e
                                     #x49b72a88f851f611 #x8a3d79f66ec97dd7
                                     #xcd2d042bf59927ef #xc930877ab0f0ee48
                                     #x9273540deda2f122 #xc797d02fd3f14261
                                     #xe1e2f06a284d674a #xd2be8c74c97cfd80
                                     #x9a494faf67707e71 #xb3dbd1eca9908293
                                     #x72d14d3493b2e388 #xd6a30f258c153427)))))

(declaim (inline streebog-ax)
         (ftype (function ((integer 0 7) (integer 0 255)) (unsigned-byte 64)) streebog-ax))
(defun streebog-ax (i j)
  (declare (type (integer 0 7) i)
           (type (integer 0 255) j)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((constants (load-time-value +streebog-ax+ t)))
    (declare (type (simple-array (unsigned-byte 64) (8 256)) constants))
    (aref constants i j)))


;;;
;;; Steebog rounds
;;;

(defmacro streebog-x (x y z)
  `(setf (aref ,z 0) (logxor (aref ,x 0) (aref ,y 0))
         (aref ,z 1) (logxor (aref ,x 1) (aref ,y 1))
         (aref ,z 2) (logxor (aref ,x 2) (aref ,y 2))
         (aref ,z 3) (logxor (aref ,x 3) (aref ,y 3))
         (aref ,z 4) (logxor (aref ,x 4) (aref ,y 4))
         (aref ,z 5) (logxor (aref ,x 5) (aref ,y 5))
         (aref ,z 6) (logxor (aref ,x 6) (aref ,y 6))
         (aref ,z 7) (logxor (aref ,x 7) (aref ,y 7))))

(defmacro streebog-xlps (x y data)
  `(let ((r0 (logxor (aref ,x 0) (aref ,y 0)))
         (r1 (logxor (aref ,x 1) (aref ,y 1)))
         (r2 (logxor (aref ,x 2) (aref ,y 2)))
         (r3 (logxor (aref ,x 3) (aref ,y 3)))
         (r4 (logxor (aref ,x 4) (aref ,y 4)))
         (r5 (logxor (aref ,x 5) (aref ,y 5)))
         (r6 (logxor (aref ,x 6) (aref ,y 6)))
         (r7 (logxor (aref ,x 7) (aref ,y 7))))
     (declare (type (unsigned-byte 64) r0 r1 r2 r3 r4 r5 r6 r7))
     (dotimes-unrolled (i 8)
       (let ((r (- (ash i 3))))
         (setf (aref ,data i)
               (logxor (streebog-ax 0 (logand (ash r0 r) #xff))
                       (streebog-ax 1 (logand (ash r1 r) #xff))
                       (streebog-ax 2 (logand (ash r2 r) #xff))
                       (streebog-ax 3 (logand (ash r3 r) #xff))
                       (streebog-ax 4 (logand (ash r4 r) #xff))
                       (streebog-ax 5 (logand (ash r5 r) #xff))
                       (streebog-ax 6 (logand (ash r6 r) #xff))
                       (streebog-ax 7 (logand (ash r7 r) #xff))))))))

(defmacro streebog-round (i ki data)
  `(progn
     (streebog-xlps ,ki (the (simple-array (unsigned-byte 64) (8)) (aref +streebog-c+ ,i)) ,ki)
     (streebog-xlps ,ki ,data ,data)))

(defun streebog-add512 (x y r)
  (declare (type (simple-array (unsigned-byte 64) (8)) x y r)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((cf 0)
        (of 0)
        (tmp 0))
    (declare (type (unsigned-byte 8) cf of)
             (type (unsigned-byte 64) tmp))
    (dotimes (i 8)
      (setf tmp (mod64+ (aref x i) (aref y i)))
      (setf of (if (< tmp (aref x i)) 1 0))
      (setf tmp (mod64+ tmp cf))
      (when (and (plusp cf) (zerop tmp))
        (setf of 1))
      (setf cf of)
      (setf (aref r i) tmp)))
  (values))

(defun streebog-g (h n m)
  (declare (type (simple-array (unsigned-byte 64) (8)) h n m)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((ki (make-array 8 :element-type '(unsigned-byte 64)
                          :initial-element 0))
        (data (make-array 8 :element-type '(unsigned-byte 64)
                            :initial-element 0)))
    (declare (type (simple-array (unsigned-byte 64) (8)) ki data)
             (dynamic-extent ki data))
    (streebog-xlps h n data)
    (replace ki data)
    (streebog-xlps ki m data)
    (dotimes (i 11)
      (streebog-round i ki data))
    (streebog-xlps ki (the (simple-array (unsigned-byte 64) (8)) (aref +streebog-c+ 11)) ki)
    (streebog-x ki data data)
    (streebog-x data h data)
    (streebog-x data m h))
  (values))


;;;
;;; Digest structures and functions
;;;

(defstruct (streebog
            (:constructor %make-streebog-digest nil)
            (:copier nil))
  (buffer (make-array 64 :element-type '(unsigned-byte 8))
          :type (simple-array (unsigned-byte 8) (64)))
  (buffer-index 0 :type (integer 0 64))
  (h (make-array 8 :element-type '(unsigned-byte 64)
                   :initial-element 0)
     :type (simple-array (unsigned-byte 64) (8)))
  (n (make-array 8 :element-type '(unsigned-byte 64)
                   :initial-element 0)
     :type (simple-array (unsigned-byte 64) (8)))
  (sigma (make-array 8 :element-type '(unsigned-byte 64)
                       :initial-element 0)
         :type (simple-array (unsigned-byte 64) (8))))

(defstruct (streebog/256
            (:include streebog)
            (:constructor %make-streebog/256-digest
                (&aux (h (make-array 8 :element-type '(unsigned-byte 64)
                                       :initial-element #x0101010101010101))))
            (:copier nil)))

(defmethod reinitialize-instance ((state streebog) &rest initargs)
  (declare (ignore initargs))
  (setf (streebog-buffer-index state) 0)
  (fill (streebog-h state) (etypecase state
                             (streebog/256 #x0101010101010101)
                             (streebog 0)))
  (fill (streebog-n state) 0)
  (fill (streebog-sigma state) 0)
  state)

(defmethod copy-digest ((state streebog) &optional copy)
  (check-type copy (or null streebog))
  (let ((copy (if copy
                  copy
                  (etypecase state
                    (streebog/256 (%make-streebog/256-digest))
                    (streebog (%make-streebog-digest))))))
    (declare (type streebog copy))
    (replace (streebog-buffer copy) (streebog-buffer state))
    (setf (streebog-buffer-index copy) (streebog-buffer-index state))
    (replace (streebog-h copy) (streebog-h state))
    (replace (streebog-n copy) (streebog-n state))
    (replace (streebog-sigma copy) (streebog-sigma state))
    copy))

(defun streebog-stage2 (state data start)
  (declare (type streebog state)
           (type (simple-array (unsigned-byte 8) (*)) data)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((tmp (make-array 8 :element-type '(unsigned-byte 64)))
        (h (streebog-h state))
        (n (streebog-n state))
        (sigma (streebog-sigma state)))
    (declare (type (simple-array (unsigned-byte 64) (8)) tmp h n sigma)
             (dynamic-extent tmp))
    (dotimes (i 8)
      (setf (aref tmp i) (ub64ref/le data (+ start (* i 8)))))
    (streebog-g h n tmp)
    (streebog-add512 n +streebog-buffer512+ n)
    (streebog-add512 sigma tmp sigma))
  (values))

(defun streebog-pad (state)
  (declare (type streebog state)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((buffer (streebog-buffer state))
        (buffer-index (streebog-buffer-index state)))
    (when (< buffer-index 64)
      (fill buffer 0 :start buffer-index)
      (setf (aref buffer buffer-index) 1)))
  (values))

(defun streebog-stage3 (state)
  (declare (type streebog state)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((buffer (streebog-buffer state))
        (buffer-index (streebog-buffer-index state))
        (h (streebog-h state))
        (n (streebog-n state))
        (sigma (streebog-sigma state))
        (buf (make-array 8 :element-type '(unsigned-byte 64)
                           :initial-element 0))
        (tmp (make-array 8 :element-type '(unsigned-byte 64))))
    (declare (type (simple-array (unsigned-byte 8) (64)) buffer)
             (type (integer 0 64) buffer-index)
             (type (simple-array (unsigned-byte 64) (8)) h n sigma buf tmp)
             (dynamic-extent buf tmp))
    (setf (aref buf 0) (ash buffer-index 3))
    (streebog-pad state)
    (dotimes (i 8)
      (setf (aref tmp i) (ub64ref/le buffer (* i 8))))
    (streebog-g h n tmp)
    (streebog-add512 n buf n)
    (streebog-add512 sigma tmp sigma)
    (streebog-g h +streebog-buffer0+ n)
    (streebog-g h +streebog-buffer0+ sigma))
  (values))

(define-digest-updater streebog
  (let ((buffer (streebog-buffer state))
        (buffer-index (streebog-buffer-index state))
        (length (- end start))
        (n 0))
    (declare (type (simple-array (unsigned-byte 8) (64)) buffer)
             (type (integer 0 64) buffer-index n)
             (type fixnum length))
    (when (plusp buffer-index)
      (setf n (min length (- 64 buffer-index)))
      (replace buffer sequence :start1 buffer-index :start2 start :end2 (+ start n))
      (incf buffer-index n)
      (incf start n)
      (decf length n)
      (when (= buffer-index 64)
        (streebog-stage2 state buffer 0)
        (setf buffer-index 0)))

    (loop until (< length 64) do
      (streebog-stage2 state sequence start)
      (incf start 64)
      (decf length 64))

    (when (plusp length)
      (replace buffer sequence :start2 start :end2 end)
      (setf buffer-index length))

    (setf (streebog-buffer-index state) buffer-index)
    (values)))

(define-digest-finalizer ((streebog 64)
                          (streebog/256 32))
  (streebog-stage3 state)
  (let ((h (streebog-h state))
        (output (make-array 64 :element-type '(unsigned-byte 8)))
        (offset (ecase (digest-length state)
                  ((32) 32)
                  ((64) 0))))
    (dotimes (i 8)
      (setf (ub64ref/le output (* i 8)) (aref h i)))
    (replace digest output :start1 digest-start :start2 offset :end2 64)
    digest))

(defdigest streebog :digest-length 64 :block-length 64)
(defdigest streebog/256 :digest-length 32 :block-length 64)
