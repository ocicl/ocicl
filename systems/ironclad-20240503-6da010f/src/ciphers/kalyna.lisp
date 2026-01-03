;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; kalyna.lisp -- implementation of the Kalyna block ciphers (DSTU 7624:2014)

(in-package :crypto)


;;;
;;; Constants
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconst +kalyna-t+
    (make-array '(8 256)
                :element-type '(unsigned-byte 64)
                :initial-contents '((#xa832a829d77f9aa8 #x4352432297d41143
                                     #x5f3e5fc2df80615f #x061e063014121806
                                     #x6bda6b7f670cb16b #x75bc758f2356c975
                                     #x6cc16c477519ad6c #x592059f2cb927959
                                     #x71a871af3b4ad971 #xdf84dfb6f8275bdf
                                     #x87a1874c35b22687 #x95fb95dc59cc6e95
                                     #x174b17b872655c17 #xf017f0d31aeae7f0
                                     #xd89fd88eea3247d8 #x092d0948363f2409
                                     #x6dc46d4f731ea96d #xf318f3cb10e3ebf3
                                     #x1d691de84e53741d #xcbc0cb16804b0bcb
                                     #xc9cac9068c4503c9 #x4d644d52b3fe294d
                                     #x2c9c2c7de8c4b02c #xaf29af11c56a86af
                                     #x798079ef0b72f979 #xe047e0537a9aa7e0
                                     #x97f197cc55c26697 #xfd2efdbb34c9d3fd
                                     #x6fce6f5f7f10a16f #x4b7a4b62a7ec314b
                                     #x454c451283c60945 #x39dd39d596afe439
                                     #x3ec63eed84baf83e #xdd8edda6f42953dd
                                     #xa315a371ed4eb6a3 #x4f6e4f42bff0214f
                                     #xb45eb4c99f2beab4 #xb654b6d99325e2b6
                                     #x9ac89aa47be1529a #x0e360e70242a380e
                                     #x1f631ff8425d7c1f #xbf79bf91a51ac6bf
                                     #x154115a87e6b5415 #xe142e15b7c9da3e1
                                     #x49704972abe23949 #xd2bdd2ded6046fd2
                                     #x93e593ec4dde7693 #xc6f9c67eae683fc6
                                     #x92e092e44bd97292 #x72a772b73143d572
                                     #x9edc9e8463fd429e #x61f8612f5b3a9961
                                     #xd1b2d1c6dc0d63d1 #x63f2633f57349163
                                     #xfa35fa8326dccffa #xee71ee235eb09fee
                                     #xf403f4f302f6f7f4 #x197d19c8564f6419
                                     #xd5a6d5e6c41173d5 #xad23ad01c9648ead
                                     #x582558facd957d58 #xa40ea449ff5baaa4
                                     #xbb6dbbb1bd06d6bb #xa11fa161e140bea1
                                     #xdc8bdcaef22e57dc #xf21df2c316e4eff2
                                     #x83b5836c2dae3683 #x37eb37a5b285dc37
                                     #x4257422a91d31542 #xe453e4736286b7e4
                                     #x7a8f7af7017bf57a #x32fa328dac9ec832
                                     #x9cd69c946ff34a9c #xccdbcc2e925e17cc
                                     #xab3dab31dd7696ab #x4a7f4a6aa1eb354a
                                     #x8f898f0c058a068f #x6ecb6e577917a56e
                                     #x04140420181c1004 #x27bb2725d2f59c27
                                     #x2e962e6de4cab82e #xe75ce76b688fbbe7
                                     #xe24de2437694afe2 #x5a2f5aeac19b755a
                                     #x96f496c453c56296 #x164e16b074625816
                                     #x23af2305cae98c23 #x2b872b45fad1ac2b
                                     #xc2edc25eb6742fc2 #x65ec650f43268965
                                     #x66e36617492f8566 #x0f330f78222d3c0f
                                     #xbc76bc89af13cabc #xa937a921d1789ea9
                                     #x474647028fc80147 #x415841329bda1941
                                     #x34e434bdb88cd034 #x4875487aade53d48
                                     #xfc2bfcb332ced7fc #xb751b7d19522e6b7
                                     #x6adf6a77610bb56a #x88928834179f1a88
                                     #xa50ba541f95caea5 #x530253a2f7a45153
                                     #x86a4864433b52286 #xf93af99b2cd5c3f9
                                     #x5b2a5be2c79c715b #xdb90db96e03b4bdb
                                     #x38d838dd90a8e038 #x7b8a7bff077cf17b
                                     #xc3e8c356b0732bc3 #x1e661ef0445a781e
                                     #x22aa220dccee8822 #x33ff3385aa99cc33
                                     #x24b4243dd8fc9024 #x2888285df0d8a028
                                     #x36ee36adb482d836 #xc7fcc776a86f3bc7
                                     #xb240b2f98b39f2b2 #x3bd73bc59aa1ec3b
                                     #x8e8c8e04038d028e #x77b6779f2f58c177
                                     #xba68bab9bb01d2ba #xf506f5fb04f1f3f5
                                     #x144414a0786c5014 #x9fd99f8c65fa469f
                                     #x0828084030382008 #x551c5592e3b64955
                                     #x9bcd9bac7de6569b #x4c614c5ab5f92d4c
                                     #xfe21fea33ec0dffe #x60fd60275d3d9d60
                                     #x5c315cdad5896d5c #xda95da9ee63c4fda
                                     #x187818c050486018 #x4643460a89cf0546
                                     #xcddecd26945913cd #x7d947dcf136ee97d
                                     #x21a52115c6e78421 #xb04ab0e98737fab0
                                     #x3fc33fe582bdfc3f #x1b771bd85a416c1b
                                     #x8997893c11981e89 #xff24ffab38c7dbff
                                     #xeb60eb0b40ab8beb #x84ae84543fbb2a84
                                     #x69d0696f6b02b969 #x3ad23acd9ca6e83a
                                     #x9dd39d9c69f44e9d #xd7acd7f6c81f7bd7
                                     #xd3b8d3d6d0036bd3 #x70ad70a73d4ddd70
                                     #x67e6671f4f288167 #x405d403a9ddd1d40
                                     #xb55bb5c1992ceeb5 #xde81debefe205fde
                                     #x5d345dd2d38e695d #x30f0309da090c030
                                     #x91ef91fc41d07e91 #xb14fb1e18130feb1
                                     #x788578e70d75fd78 #x1155118866774411
                                     #x0105010806070401 #xe556e57b6481b3e5
                                     #x0000000000000000 #x68d568676d05bd68
                                     #x98c298b477ef5a98 #xa01aa069e747baa0
                                     #xc5f6c566a46133c5 #x020a02100c0e0802
                                     #xa604a659f355a2a6 #x74b974872551cd74
                                     #x2d992d75eec3b42d #x0b270b583a312c0b
                                     #xa210a279eb49b2a2 #x76b37697295fc576
                                     #xb345b3f18d3ef6b3 #xbe7cbe99a31dc2be
                                     #xced1ce3e9e501fce #xbd73bd81a914cebd
                                     #xae2cae19c36d82ae #xe96ae91b4ca583e9
                                     #x8a988a241b91128a #x31f53195a697c431
                                     #x1c6c1ce04854701c #xec7bec3352be97ec
                                     #xf112f1db1cede3f1 #x99c799bc71e85e99
                                     #x94fe94d45fcb6a94 #xaa38aa39db7192aa
                                     #xf609f6e30ef8fff6 #x26be262dd4f29826
                                     #x2f932f65e2cdbc2f #xef74ef2b58b79bef
                                     #xe86fe8134aa287e8 #x8c868c140f830a8c
                                     #x35e135b5be8bd435 #x030f03180a090c03
                                     #xd4a3d4eec21677d4 #x7f9e7fdf1f60e17f
                                     #xfb30fb8b20dbcbfb #x051105281e1b1405
                                     #xc1e2c146bc7d23c1 #x5e3b5ecad987655e
                                     #x90ea90f447d77a90 #x20a0201dc0e08020
                                     #x3dc93df58eb3f43d #x82b082642ba93282
                                     #xf70cf7eb08fffbf7 #xea65ea0346ac8fea
                                     #x0a220a503c36280a #x0d390d682e23340d
                                     #x7e9b7ed71967e57e #xf83ff8932ad2c7f8
                                     #x500d50bafdad5d50 #x1a721ad05c46681a
                                     #xc4f3c46ea26637c4 #x071b073812151c07
                                     #x57165782efb84157 #xb862b8a9b70fdab8
                                     #x3ccc3cfd88b4f03c #x62f7623751339562
                                     #xe348e34b7093abe3 #xc8cfc80e8a4207c8
                                     #xac26ac09cf638aac #x520752aaf1a35552
                                     #x64e9640745218d64 #x1050108060704010
                                     #xd0b7d0ceda0a67d0 #xd99ad986ec3543d9
                                     #x135f13986a794c13 #x0c3c0c602824300c
                                     #x125a12906c7e4812 #x298d2955f6dfa429
                                     #x510851b2fbaa5951 #xb967b9a1b108deb9
                                     #xcfd4cf3698571bcf #xd6a9d6fece187fd6
                                     #x73a273bf3744d173 #x8d838d1c09840e8d
                                     #x81bf817c21a03e81 #x5419549ae5b14d54
                                     #xc0e7c04eba7a27c0 #xed7eed3b54b993ed
                                     #x4e6b4e4ab9f7254e #x4449441a85c10d44
                                     #xa701a751f552a6a7 #x2a822a4dfcd6a82a
                                     #x85ab855c39bc2e85 #x25b12535defb9425
                                     #xe659e6636e88bfe6 #xcac5ca1e864c0fca
                                     #x7c917cc71569ed7c #x8b9d8b2c1d96168b
                                     #x5613568ae9bf4556 #x80ba807427a73a80)
                                    (#xd1ce3e9e501fcece #x6dbbb1bd06d6bbbb
                                     #x60eb0b40ab8bebeb #xe092e44bd9729292
                                     #x65ea0346ac8feaea #xc0cb16804b0bcbcb
                                     #x5f13986a794c1313 #xe2c146bc7d23c1c1
                                     #x6ae91b4ca583e9e9 #xd23acd9ca6e83a3a
                                     #xa9d6fece187fd6d6 #x40b2f98b39f2b2b2
                                     #xbdd2ded6046fd2d2 #xea90f447d77a9090
                                     #x4b17b872655c1717 #x3ff8932ad2c7f8f8
                                     #x57422a91d3154242 #x4115a87e6b541515
                                     #x13568ae9bf455656 #x5eb4c99f2beab4b4
                                     #xec650f4326896565 #x6c1ce04854701c1c
                                     #x928834179f1a8888 #x52432297d4114343
                                     #xf6c566a46133c5c5 #x315cdad5896d5c5c
                                     #xee36adb482d83636 #x68bab9bb01d2baba
                                     #x06f5fb04f1f3f5f5 #x165782efb8415757
                                     #xe6671f4f28816767 #x838d1c09840e8d8d
                                     #xf53195a697c43131 #x09f6e30ef8fff6f6
                                     #xe9640745218d6464 #x2558facd957d5858
                                     #xdc9e8463fd429e9e #x03f4f302f6f7f4f4
                                     #xaa220dccee882222 #x38aa39db7192aaaa
                                     #xbc758f2356c97575 #x330f78222d3c0f0f
                                     #x0a02100c0e080202 #x4fb1e18130feb1b1
                                     #x84dfb6f8275bdfdf #xc46d4f731ea96d6d
                                     #xa273bf3744d17373 #x644d52b3fe294d4d
                                     #x917cc71569ed7c7c #xbe262dd4f2982626
                                     #x962e6de4cab82e2e #x0cf7eb08fffbf7f7
                                     #x2808403038200808 #x345dd2d38e695d5d
                                     #x49441a85c10d4444 #xc63eed84baf83e3e
                                     #xd99f8c65fa469f9f #x4414a0786c501414
                                     #xcfc80e8a4207c8c8 #x2cae19c36d82aeae
                                     #x19549ae5b14d5454 #x5010806070401010
                                     #x9fd88eea3247d8d8 #x76bc89af13cabcbc
                                     #x721ad05c46681a1a #xda6b7f670cb16b6b
                                     #xd0696f6b02b96969 #x18f3cb10e3ebf3f3
                                     #x73bd81a914cebdbd #xff3385aa99cc3333
                                     #x3dab31dd7696abab #x35fa8326dccffafa
                                     #xb2d1c6dc0d63d1d1 #xcd9bac7de6569b9b
                                     #xd568676d05bd6868 #x6b4e4ab9f7254e4e
                                     #x4e16b07462581616 #xfb95dc59cc6e9595
                                     #xef91fc41d07e9191 #x71ee235eb09feeee
                                     #x614c5ab5f92d4c4c #xf2633f5734916363
                                     #x8c8e04038d028e8e #x2a5be2c79c715b5b
                                     #xdbcc2e925e17cccc #xcc3cfd88b4f03c3c
                                     #x7d19c8564f641919 #x1fa161e140bea1a1
                                     #xbf817c21a03e8181 #x704972abe2394949
                                     #x8a7bff077cf17b7b #x9ad986ec3543d9d9
                                     #xce6f5f7f10a16f6f #xeb37a5b285dc3737
                                     #xfd60275d3d9d6060 #xc5ca1e864c0fcaca
                                     #x5ce76b688fbbe7e7 #x872b45fad1ac2b2b
                                     #x75487aade53d4848 #x2efdbb34c9d3fdfd
                                     #xf496c453c5629696 #x4c451283c6094545
                                     #x2bfcb332ced7fcfc #x5841329bda194141
                                     #x5a12906c7e481212 #x390d682e23340d0d
                                     #x8079ef0b72f97979 #x56e57b6481b3e5e5
                                     #x97893c11981e8989 #x868c140f830a8c8c
                                     #x48e34b7093abe3e3 #xa0201dc0e0802020
                                     #xf0309da090c03030 #x8bdcaef22e57dcdc
                                     #x51b7d19522e6b7b7 #xc16c477519ad6c6c
                                     #x7f4a6aa1eb354a4a #x5bb5c1992ceeb5b5
                                     #xc33fe582bdfc3f3f #xf197cc55c2669797
                                     #xa3d4eec21677d4d4 #xf762375133956262
                                     #x992d75eec3b42d2d #x1e06301412180606
                                     #x0ea449ff5baaa4a4 #x0ba541f95caea5a5
                                     #xb5836c2dae368383 #x3e5fc2df80615f5f
                                     #x822a4dfcd6a82a2a #x95da9ee63c4fdada
                                     #xcac9068c4503c9c9 #x0000000000000000
                                     #x9b7ed71967e57e7e #x10a279eb49b2a2a2
                                     #x1c5592e3b6495555 #x79bf91a51ac6bfbf
                                     #x5511886677441111 #xa6d5e6c41173d5d5
                                     #xd69c946ff34a9c9c #xd4cf3698571bcfcf
                                     #x360e70242a380e0e #x220a503c36280a0a
                                     #xc93df58eb3f43d3d #x0851b2fbaa595151
                                     #x947dcf136ee97d7d #xe593ec4dde769393
                                     #x771bd85a416c1b1b #x21fea33ec0dffefe
                                     #xf3c46ea26637c4c4 #x4647028fc8014747
                                     #x2d0948363f240909 #xa4864433b5228686
                                     #x270b583a312c0b0b #x898f0c058a068f8f
                                     #xd39d9c69f44e9d9d #xdf6a77610bb56a6a
                                     #x1b073812151c0707 #x67b9a1b108deb9b9
                                     #x4ab0e98737fab0b0 #xc298b477ef5a9898
                                     #x7818c05048601818 #xfa328dac9ec83232
                                     #xa871af3b4ad97171 #x7a4b62a7ec314b4b
                                     #x74ef2b58b79befef #xd73bc59aa1ec3b3b
                                     #xad70a73d4ddd7070 #x1aa069e747baa0a0
                                     #x53e4736286b7e4e4 #x5d403a9ddd1d4040
                                     #x24ffab38c7dbffff #xe8c356b0732bc3c3
                                     #x37a921d1789ea9a9 #x59e6636e88bfe6e6
                                     #x8578e70d75fd7878 #x3af99b2cd5c3f9f9
                                     #x9d8b2c1d96168b8b #x43460a89cf054646
                                     #xba807427a73a8080 #x661ef0445a781e1e
                                     #xd838dd90a8e03838 #x42e15b7c9da3e1e1
                                     #x62b8a9b70fdab8b8 #x32a829d77f9aa8a8
                                     #x47e0537a9aa7e0e0 #x3c0c602824300c0c
                                     #xaf2305cae98c2323 #xb37697295fc57676
                                     #x691de84e53741d1d #xb12535defb942525
                                     #xb4243dd8fc902424 #x1105281e1b140505
                                     #x12f1db1cede3f1f1 #xcb6e577917a56e6e
                                     #xfe94d45fcb6a9494 #x88285df0d8a02828
                                     #xc89aa47be1529a9a #xae84543fbb2a8484
                                     #x6fe8134aa287e8e8 #x15a371ed4eb6a3a3
                                     #x6e4f42bff0214f4f #xb6779f2f58c17777
                                     #xb8d3d6d0036bd3d3 #xab855c39bc2e8585
                                     #x4de2437694afe2e2 #x0752aaf1a3555252
                                     #x1df2c316e4eff2f2 #xb082642ba9328282
                                     #x0d50bafdad5d5050 #x8f7af7017bf57a7a
                                     #x932f65e2cdbc2f2f #xb974872551cd7474
                                     #x0253a2f7a4515353 #x45b3f18d3ef6b3b3
                                     #xf8612f5b3a996161 #x29af11c56a86afaf
                                     #xdd39d596afe43939 #xe135b5be8bd43535
                                     #x81debefe205fdede #xdecd26945913cdcd
                                     #x631ff8425d7c1f1f #xc799bc71e85e9999
                                     #x26ac09cf638aacac #x23ad01c9648eadad
                                     #xa772b73143d57272 #x9c2c7de8c4b02c2c
                                     #x8edda6f42953dddd #xb7d0ceda0a67d0d0
                                     #xa1874c35b2268787 #x7cbe99a31dc2bebe
                                     #x3b5ecad987655e5e #x04a659f355a2a6a6
                                     #x7bec3352be97ecec #x140420181c100404
                                     #xf9c67eae683fc6c6 #x0f03180a090c0303
                                     #xe434bdb88cd03434 #x30fb8b20dbcbfbfb
                                     #x90db96e03b4bdbdb #x2059f2cb92795959
                                     #x54b6d99325e2b6b6 #xedc25eb6742fc2c2
                                     #x0501080607040101 #x17f0d31aeae7f0f0
                                     #x2f5aeac19b755a5a #x7eed3b54b993eded
                                     #x01a751f552a6a7a7 #xe36617492f856666
                                     #xa52115c6e7842121 #x9e7fdf1f60e17f7f
                                     #x988a241b91128a8a #xbb2725d2f59c2727
                                     #xfcc776a86f3bc7c7 #xe7c04eba7a27c0c0
                                     #x8d2955f6dfa42929 #xacd7f6c81f7bd7d7)
                                    (#x93ec4dde769393e5 #xd986ec3543d9d99a
                                     #x9aa47be1529a9ac8 #xb5c1992ceeb5b55b
                                     #x98b477ef5a9898c2 #x220dccee882222aa
                                     #x451283c60945454c #xfcb332ced7fcfc2b
                                     #xbab9bb01d2baba68 #x6a77610bb56a6adf
                                     #xdfb6f8275bdfdf84 #x02100c0e0802020a
                                     #x9f8c65fa469f9fd9 #xdcaef22e57dcdc8b
                                     #x51b2fbaa59515108 #x59f2cb9279595920
                                     #x4a6aa1eb354a4a7f #x17b872655c17174b
                                     #x2b45fad1ac2b2b87 #xc25eb6742fc2c2ed
                                     #x94d45fcb6a9494fe #xf4f302f6f7f4f403
                                     #xbbb1bd06d6bbbb6d #xa371ed4eb6a3a315
                                     #x62375133956262f7 #xe4736286b7e4e453
                                     #x71af3b4ad97171a8 #xd4eec21677d4d4a3
                                     #xcd26945913cdcdde #x70a73d4ddd7070ad
                                     #x16b074625816164e #xe15b7c9da3e1e142
                                     #x4972abe239494970 #x3cfd88b4f03c3ccc
                                     #xc04eba7a27c0c0e7 #xd88eea3247d8d89f
                                     #x5cdad5896d5c5c31 #x9bac7de6569b9bcd
                                     #xad01c9648eadad23 #x855c39bc2e8585ab
                                     #x53a2f7a451535302 #xa161e140bea1a11f
                                     #x7af7017bf57a7a8f #xc80e8a4207c8c8cf
                                     #x2d75eec3b42d2d99 #xe0537a9aa7e0e047
                                     #xd1c6dc0d63d1d1b2 #x72b73143d57272a7
                                     #xa659f355a2a6a604 #x2c7de8c4b02c2c9c
                                     #xc46ea26637c4c4f3 #xe34b7093abe3e348
                                     #x7697295fc57676b3 #x78e70d75fd787885
                                     #xb7d19522e6b7b751 #xb4c99f2beab4b45e
                                     #x0948363f2409092d #x3bc59aa1ec3b3bd7
                                     #x0e70242a380e0e36 #x41329bda19414158
                                     #x4c5ab5f92d4c4c61 #xdebefe205fdede81
                                     #xb2f98b39f2b2b240 #x90f447d77a9090ea
                                     #x2535defb942525b1 #xa541f95caea5a50b
                                     #xd7f6c81f7bd7d7ac #x03180a090c03030f
                                     #x1188667744111155 #x0000000000000000
                                     #xc356b0732bc3c3e8 #x2e6de4cab82e2e96
                                     #x92e44bd9729292e0 #xef2b58b79befef74
                                     #x4e4ab9f7254e4e6b #x12906c7e4812125a
                                     #x9d9c69f44e9d9dd3 #x7dcf136ee97d7d94
                                     #xcb16804b0bcbcbc0 #x35b5be8bd43535e1
                                     #x1080607040101050 #xd5e6c41173d5d5a6
                                     #x4f42bff0214f4f6e #x9e8463fd429e9edc
                                     #x4d52b3fe294d4d64 #xa921d1789ea9a937
                                     #x5592e3b64955551c #xc67eae683fc6c6f9
                                     #xd0ceda0a67d0d0b7 #x7bff077cf17b7b8a
                                     #x18c0504860181878 #x97cc55c2669797f1
                                     #xd3d6d0036bd3d3b8 #x36adb482d83636ee
                                     #xe6636e88bfe6e659 #x487aade53d484875
                                     #x568ae9bf45565613 #x817c21a03e8181bf
                                     #x8f0c058a068f8f89 #x779f2f58c17777b6
                                     #xcc2e925e17ccccdb #x9c946ff34a9c9cd6
                                     #xb9a1b108deb9b967 #xe2437694afe2e24d
                                     #xac09cf638aacac26 #xb8a9b70fdab8b862
                                     #x2f65e2cdbc2f2f93 #x15a87e6b54151541
                                     #xa449ff5baaa4a40e #x7cc71569ed7c7c91
                                     #xda9ee63c4fdada95 #x38dd90a8e03838d8
                                     #x1ef0445a781e1e66 #x0b583a312c0b0b27
                                     #x05281e1b14050511 #xd6fece187fd6d6a9
                                     #x14a0786c50141444 #x6e577917a56e6ecb
                                     #x6c477519ad6c6cc1 #x7ed71967e57e7e9b
                                     #x6617492f856666e3 #xfdbb34c9d3fdfd2e
                                     #xb1e18130feb1b14f #xe57b6481b3e5e556
                                     #x60275d3d9d6060fd #xaf11c56a86afaf29
                                     #x5ecad987655e5e3b #x3385aa99cc3333ff
                                     #x874c35b2268787a1 #xc9068c4503c9c9ca
                                     #xf0d31aeae7f0f017 #x5dd2d38e695d5d34
                                     #x6d4f731ea96d6dc4 #x3fe582bdfc3f3fc3
                                     #x8834179f1a888892 #x8d1c09840e8d8d83
                                     #xc776a86f3bc7c7fc #xf7eb08fffbf7f70c
                                     #x1de84e53741d1d69 #xe91b4ca583e9e96a
                                     #xec3352be97ecec7b #xed3b54b993eded7e
                                     #x807427a73a8080ba #x2955f6dfa429298d
                                     #x2725d2f59c2727bb #xcf3698571bcfcfd4
                                     #x99bc71e85e9999c7 #xa829d77f9aa8a832
                                     #x50bafdad5d50500d #x0f78222d3c0f0f33
                                     #x37a5b285dc3737eb #x243dd8fc902424b4
                                     #x285df0d8a0282888 #x309da090c03030f0
                                     #x95dc59cc6e9595fb #xd2ded6046fd2d2bd
                                     #x3eed84baf83e3ec6 #x5be2c79c715b5b2a
                                     #x403a9ddd1d40405d #x836c2dae368383b5
                                     #xb3f18d3ef6b3b345 #x696f6b02b96969d0
                                     #x5782efb841575716 #x1ff8425d7c1f1f63
                                     #x073812151c07071b #x1ce04854701c1c6c
                                     #x8a241b91128a8a98 #xbc89af13cabcbc76
                                     #x201dc0e0802020a0 #xeb0b40ab8bebeb60
                                     #xce3e9e501fceced1 #x8e04038d028e8e8c
                                     #xab31dd7696abab3d #xee235eb09feeee71
                                     #x3195a697c43131f5 #xa279eb49b2a2a210
                                     #x73bf3744d17373a2 #xf99b2cd5c3f9f93a
                                     #xca1e864c0fcacac5 #x3acd9ca6e83a3ad2
                                     #x1ad05c46681a1a72 #xfb8b20dbcbfbfb30
                                     #x0d682e23340d0d39 #xc146bc7d23c1c1e2
                                     #xfea33ec0dffefe21 #xfa8326dccffafa35
                                     #xf2c316e4eff2f21d #x6f5f7f10a16f6fce
                                     #xbd81a914cebdbd73 #x96c453c5629696f4
                                     #xdda6f42953dddd8e #x432297d411434352
                                     #x52aaf1a355525207 #xb6d99325e2b6b654
                                     #x0840303820080828 #xf3cb10e3ebf3f318
                                     #xae19c36d82aeae2c #xbe99a31dc2bebe7c
                                     #x19c8564f6419197d #x893c11981e898997
                                     #x328dac9ec83232fa #x262dd4f2982626be
                                     #xb0e98737fab0b04a #xea0346ac8feaea65
                                     #x4b62a7ec314b4b7a #x640745218d6464e9
                                     #x84543fbb2a8484ae #x82642ba9328282b0
                                     #x6b7f670cb16b6bda #xf5fb04f1f3f5f506
                                     #x79ef0b72f9797980 #xbf91a51ac6bfbf79
                                     #x0108060704010105 #x5fc2df80615f5f3e
                                     #x758f2356c97575bc #x633f5734916363f2
                                     #x1bd85a416c1b1b77 #x2305cae98c2323af
                                     #x3df58eb3f43d3dc9 #x68676d05bd6868d5
                                     #x2a4dfcd6a82a2a82 #x650f4326896565ec
                                     #xe8134aa287e8e86f #x91fc41d07e9191ef
                                     #xf6e30ef8fff6f609 #xffab38c7dbffff24
                                     #x13986a794c13135f #x58facd957d585825
                                     #xf1db1cede3f1f112 #x47028fc801474746
                                     #x0a503c36280a0a22 #x7fdf1f60e17f7f9e
                                     #xc566a46133c5c5f6 #xa751f552a6a7a701
                                     #xe76b688fbbe7e75c #x612f5b3a996161f8
                                     #x5aeac19b755a5a2f #x063014121806061e
                                     #x460a89cf05464643 #x441a85c10d444449
                                     #x422a91d315424257 #x0420181c10040414
                                     #xa069e747baa0a01a #xdb96e03b4bdbdb90
                                     #x39d596afe43939dd #x864433b5228686a4
                                     #x549ae5b14d545419 #xaa39db7192aaaa38
                                     #x8c140f830a8c8c86 #x34bdb88cd03434e4
                                     #x2115c6e7842121a5 #x8b2c1d96168b8b9d
                                     #xf8932ad2c7f8f83f #x0c602824300c0c3c
                                     #x74872551cd7474b9 #x671f4f28816767e6)
                                    (#x676d05bd6868d568 #x1c09840e8d8d838d
                                     #x1e864c0fcacac5ca #x52b3fe294d4d644d
                                     #xbf3744d17373a273 #x62a7ec314b4b7a4b
                                     #x4ab9f7254e4e6b4e #x4dfcd6a82a2a822a
                                     #xeec21677d4d4a3d4 #xaaf1a35552520752
                                     #x2dd4f2982626be26 #xf18d3ef6b3b345b3
                                     #x9ae5b14d54541954 #xf0445a781e1e661e
                                     #xc8564f6419197d19 #xf8425d7c1f1f631f
                                     #x0dccee882222aa22 #x180a090c03030f03
                                     #x0a89cf0546464346 #xf58eb3f43d3dc93d
                                     #x75eec3b42d2d992d #x6aa1eb354a4a7f4a
                                     #xa2f7a45153530253 #x6c2dae368383b583
                                     #x986a794c13135f13 #x241b91128a8a988a
                                     #xd19522e6b7b751b7 #xe6c41173d5d5a6d5
                                     #x35defb942525b125 #xef0b72f979798079
                                     #xfb04f1f3f5f506f5 #x81a914cebdbd73bd
                                     #xfacd957d58582558 #x65e2cdbc2f2f932f
                                     #x682e23340d0d390d #x100c0e0802020a02
                                     #x3b54b993eded7eed #xb2fbaa5951510851
                                     #x8463fd429e9edc9e #x8866774411115511
                                     #xc316e4eff2f21df2 #xed84baf83e3ec63e
                                     #x92e3b64955551c55 #xcad987655e5e3b5e
                                     #xc6dc0d63d1d1b2d1 #xb074625816164e16
                                     #xfd88b4f03c3ccc3c #x17492f856666e366
                                     #xa73d4ddd7070ad70 #xd2d38e695d5d345d
                                     #xcb10e3ebf3f318f3 #x1283c60945454c45
                                     #x3a9ddd1d40405d40 #x2e925e17ccccdbcc
                                     #x134aa287e8e86fe8 #xd45fcb6a9494fe94
                                     #x8ae9bf4556561356 #x4030382008082808
                                     #x3e9e501fceced1ce #xd05c46681a1a721a
                                     #xcd9ca6e83a3ad23a #xded6046fd2d2bdd2
                                     #x5b7c9da3e1e142e1 #xb6f8275bdfdf84df
                                     #xc1992ceeb5b55bb5 #xdd90a8e03838d838
                                     #x577917a56e6ecb6e #x70242a380e0e360e
                                     #x7b6481b3e5e556e5 #xf302f6f7f4f403f4
                                     #x9b2cd5c3f9f93af9 #x4433b5228686a486
                                     #x1b4ca583e9e96ae9 #x42bff0214f4f6e4f
                                     #xfece187fd6d6a9d6 #x5c39bc2e8585ab85
                                     #x05cae98c2323af23 #x3698571bcfcfd4cf
                                     #x8dac9ec83232fa32 #xbc71e85e9999c799
                                     #x95a697c43131f531 #xa0786c5014144414
                                     #x19c36d82aeae2cae #x235eb09feeee71ee
                                     #x0e8a4207c8c8cfc8 #x7aade53d48487548
                                     #xd6d0036bd3d3b8d3 #x9da090c03030f030
                                     #x61e140bea1a11fa1 #xe44bd9729292e092
                                     #x329bda1941415841 #xe18130feb1b14fb1
                                     #xc050486018187818 #x6ea26637c4c4f3c4
                                     #x7de8c4b02c2c9c2c #xaf3b4ad97171a871
                                     #xb73143d57272a772 #x1a85c10d44444944
                                     #xa87e6b5415154115 #xbb34c9d3fdfd2efd
                                     #xa5b285dc3737eb37 #x99a31dc2bebe7cbe
                                     #xc2df80615f5f3e5f #x39db7192aaaa38aa
                                     #xac7de6569b9bcd9b #x34179f1a88889288
                                     #x8eea3247d8d89fd8 #x31dd7696abab3dab
                                     #x3c11981e89899789 #x946ff34a9c9cd69c
                                     #x8326dccffafa35fa #x275d3d9d6060fd60
                                     #x0346ac8feaea65ea #x89af13cabcbc76bc
                                     #x375133956262f762 #x602824300c0c3c0c
                                     #x3dd8fc902424b424 #x59f355a2a6a604a6
                                     #x29d77f9aa8a832a8 #x3352be97ecec7bec
                                     #x1f4f28816767e667 #x1dc0e0802020a020
                                     #x96e03b4bdbdb90db #xc71569ed7c7c917c
                                     #x5df0d8a028288828 #xa6f42953dddd8edd
                                     #x09cf638aacac26ac #xe2c79c715b5b2a5b
                                     #xbdb88cd03434e434 #xd71967e57e7e9b7e
                                     #x8060704010105010 #xdb1cede3f1f112f1
                                     #xff077cf17b7b8a7b #x0c058a068f8f898f
                                     #x3f5734916363f263 #x69e747baa0a01aa0
                                     #x281e1b1405051105 #xa47be1529a9ac89a
                                     #x2297d41143435243 #x9f2f58c17777b677
                                     #x15c6e7842121a521 #x91a51ac6bfbf79bf
                                     #x25d2f59c2727bb27 #x48363f2409092d09
                                     #x56b0732bc3c3e8c3 #x8c65fa469f9fd99f
                                     #xd99325e2b6b654b6 #xf6c81f7bd7d7acd7
                                     #x55f6dfa429298d29 #x5eb6742fc2c2edc2
                                     #x0b40ab8bebeb60eb #x4eba7a27c0c0e7c0
                                     #x49ff5baaa4a40ea4 #x2c1d96168b8b9d8b
                                     #x140f830a8c8c868c #xe84e53741d1d691d
                                     #x8b20dbcbfbfb30fb #xab38c7dbffff24ff
                                     #x46bc7d23c1c1e2c1 #xf98b39f2b2b240b2
                                     #xcc55c2669797f197 #x6de4cab82e2e962e
                                     #x932ad2c7f8f83ff8 #x0f4326896565ec65
                                     #xe30ef8fff6f609f6 #x8f2356c97575bc75
                                     #x3812151c07071b07 #x20181c1004041404
                                     #x72abe23949497049 #x85aa99cc3333ff33
                                     #x736286b7e4e453e4 #x86ec3543d9d99ad9
                                     #xa1b108deb9b967b9 #xceda0a67d0d0b7d0
                                     #x2a91d31542425742 #x76a86f3bc7c7fcc7
                                     #x477519ad6c6cc16c #xf447d77a9090ea90
                                     #x0000000000000000 #x04038d028e8e8c8e
                                     #x5f7f10a16f6fce6f #xbafdad5d50500d50
                                     #x0806070401010501 #x66a46133c5c5f6c5
                                     #x9ee63c4fdada95da #x028fc80147474647
                                     #xe582bdfc3f3fc33f #x26945913cdcddecd
                                     #x6f6b02b96969d069 #x79eb49b2a2a210a2
                                     #x437694afe2e24de2 #xf7017bf57a7a8f7a
                                     #x51f552a6a7a701a7 #x7eae683fc6c6f9c6
                                     #xec4dde769393e593 #x78222d3c0f0f330f
                                     #x503c36280a0a220a #x3014121806061e06
                                     #x636e88bfe6e659e6 #x45fad1ac2b2b872b
                                     #xc453c5629696f496 #x71ed4eb6a3a315a3
                                     #xe04854701c1c6c1c #x11c56a86afaf29af
                                     #x77610bb56a6adf6a #x906c7e4812125a12
                                     #x543fbb2a8484ae84 #xd596afe43939dd39
                                     #x6b688fbbe7e75ce7 #xe98737fab0b04ab0
                                     #x642ba9328282b082 #xeb08fffbf7f70cf7
                                     #xa33ec0dffefe21fe #x9c69f44e9d9dd39d
                                     #x4c35b2268787a187 #xdad5896d5c5c315c
                                     #x7c21a03e8181bf81 #xb5be8bd43535e135
                                     #xbefe205fdede81de #xc99f2beab4b45eb4
                                     #x41f95caea5a50ba5 #xb332ced7fcfc2bfc
                                     #x7427a73a8080ba80 #x2b58b79befef74ef
                                     #x16804b0bcbcbc0cb #xb1bd06d6bbbb6dbb
                                     #x7f670cb16b6bda6b #x97295fc57676b376
                                     #xb9bb01d2baba68ba #xeac19b755a5a2f5a
                                     #xcf136ee97d7d947d #xe70d75fd78788578
                                     #x583a312c0b0b270b #xdc59cc6e9595fb95
                                     #x4b7093abe3e348e3 #x01c9648eadad23ad
                                     #x872551cd7474b974 #xb477ef5a9898c298
                                     #xc59aa1ec3b3bd73b #xadb482d83636ee36
                                     #x0745218d6464e964 #x4f731ea96d6dc46d
                                     #xaef22e57dcdc8bdc #xd31aeae7f0f017f0
                                     #xf2cb927959592059 #x21d1789ea9a937a9
                                     #x5ab5f92d4c4c614c #xb872655c17174b17
                                     #xdf1f60e17f7f9e7f #xfc41d07e9191ef91
                                     #xa9b70fdab8b862b8 #x068c4503c9c9cac9
                                     #x82efb84157571657 #xd85a416c1b1b771b
                                     #x537a9aa7e0e047e0 #x2f5b3a996161f861)
                                    (#xd77f9aa8a832a829 #x97d4114343524322
                                     #xdf80615f5f3e5fc2 #x14121806061e0630
                                     #x670cb16b6bda6b7f #x2356c97575bc758f
                                     #x7519ad6c6cc16c47 #xcb927959592059f2
                                     #x3b4ad97171a871af #xf8275bdfdf84dfb6
                                     #x35b2268787a1874c #x59cc6e9595fb95dc
                                     #x72655c17174b17b8 #x1aeae7f0f017f0d3
                                     #xea3247d8d89fd88e #x363f2409092d0948
                                     #x731ea96d6dc46d4f #x10e3ebf3f318f3cb
                                     #x4e53741d1d691de8 #x804b0bcbcbc0cb16
                                     #x8c4503c9c9cac906 #xb3fe294d4d644d52
                                     #xe8c4b02c2c9c2c7d #xc56a86afaf29af11
                                     #x0b72f979798079ef #x7a9aa7e0e047e053
                                     #x55c2669797f197cc #x34c9d3fdfd2efdbb
                                     #x7f10a16f6fce6f5f #xa7ec314b4b7a4b62
                                     #x83c60945454c4512 #x96afe43939dd39d5
                                     #x84baf83e3ec63eed #xf42953dddd8edda6
                                     #xed4eb6a3a315a371 #xbff0214f4f6e4f42
                                     #x9f2beab4b45eb4c9 #x9325e2b6b654b6d9
                                     #x7be1529a9ac89aa4 #x242a380e0e360e70
                                     #x425d7c1f1f631ff8 #xa51ac6bfbf79bf91
                                     #x7e6b5415154115a8 #x7c9da3e1e142e15b
                                     #xabe2394949704972 #xd6046fd2d2bdd2de
                                     #x4dde769393e593ec #xae683fc6c6f9c67e
                                     #x4bd9729292e092e4 #x3143d57272a772b7
                                     #x63fd429e9edc9e84 #x5b3a996161f8612f
                                     #xdc0d63d1d1b2d1c6 #x5734916363f2633f
                                     #x26dccffafa35fa83 #x5eb09feeee71ee23
                                     #x02f6f7f4f403f4f3 #x564f6419197d19c8
                                     #xc41173d5d5a6d5e6 #xc9648eadad23ad01
                                     #xcd957d58582558fa #xff5baaa4a40ea449
                                     #xbd06d6bbbb6dbbb1 #xe140bea1a11fa161
                                     #xf22e57dcdc8bdcae #x16e4eff2f21df2c3
                                     #x2dae368383b5836c #xb285dc3737eb37a5
                                     #x91d315424257422a #x6286b7e4e453e473
                                     #x017bf57a7a8f7af7 #xac9ec83232fa328d
                                     #x6ff34a9c9cd69c94 #x925e17ccccdbcc2e
                                     #xdd7696abab3dab31 #xa1eb354a4a7f4a6a
                                     #x058a068f8f898f0c #x7917a56e6ecb6e57
                                     #x181c100404140420 #xd2f59c2727bb2725
                                     #xe4cab82e2e962e6d #x688fbbe7e75ce76b
                                     #x7694afe2e24de243 #xc19b755a5a2f5aea
                                     #x53c5629696f496c4 #x74625816164e16b0
                                     #xcae98c2323af2305 #xfad1ac2b2b872b45
                                     #xb6742fc2c2edc25e #x4326896565ec650f
                                     #x492f856666e36617 #x222d3c0f0f330f78
                                     #xaf13cabcbc76bc89 #xd1789ea9a937a921
                                     #x8fc8014747464702 #x9bda194141584132
                                     #xb88cd03434e434bd #xade53d484875487a
                                     #x32ced7fcfc2bfcb3 #x9522e6b7b751b7d1
                                     #x610bb56a6adf6a77 #x179f1a8888928834
                                     #xf95caea5a50ba541 #xf7a45153530253a2
                                     #x33b5228686a48644 #x2cd5c3f9f93af99b
                                     #xc79c715b5b2a5be2 #xe03b4bdbdb90db96
                                     #x90a8e03838d838dd #x077cf17b7b8a7bff
                                     #xb0732bc3c3e8c356 #x445a781e1e661ef0
                                     #xccee882222aa220d #xaa99cc3333ff3385
                                     #xd8fc902424b4243d #xf0d8a0282888285d
                                     #xb482d83636ee36ad #xa86f3bc7c7fcc776
                                     #x8b39f2b2b240b2f9 #x9aa1ec3b3bd73bc5
                                     #x038d028e8e8c8e04 #x2f58c17777b6779f
                                     #xbb01d2baba68bab9 #x04f1f3f5f506f5fb
                                     #x786c5014144414a0 #x65fa469f9fd99f8c
                                     #x3038200808280840 #xe3b64955551c5592
                                     #x7de6569b9bcd9bac #xb5f92d4c4c614c5a
                                     #x3ec0dffefe21fea3 #x5d3d9d6060fd6027
                                     #xd5896d5c5c315cda #xe63c4fdada95da9e
                                     #x50486018187818c0 #x89cf05464643460a
                                     #x945913cdcddecd26 #x136ee97d7d947dcf
                                     #xc6e7842121a52115 #x8737fab0b04ab0e9
                                     #x82bdfc3f3fc33fe5 #x5a416c1b1b771bd8
                                     #x11981e898997893c #x38c7dbffff24ffab
                                     #x40ab8bebeb60eb0b #x3fbb2a8484ae8454
                                     #x6b02b96969d0696f #x9ca6e83a3ad23acd
                                     #x69f44e9d9dd39d9c #xc81f7bd7d7acd7f6
                                     #xd0036bd3d3b8d3d6 #x3d4ddd7070ad70a7
                                     #x4f28816767e6671f #x9ddd1d40405d403a
                                     #x992ceeb5b55bb5c1 #xfe205fdede81debe
                                     #xd38e695d5d345dd2 #xa090c03030f0309d
                                     #x41d07e9191ef91fc #x8130feb1b14fb1e1
                                     #x0d75fd78788578e7 #x6677441111551188
                                     #x0607040101050108 #x6481b3e5e556e57b
                                     #x0000000000000000 #x6d05bd6868d56867
                                     #x77ef5a9898c298b4 #xe747baa0a01aa069
                                     #xa46133c5c5f6c566 #x0c0e0802020a0210
                                     #xf355a2a6a604a659 #x2551cd7474b97487
                                     #xeec3b42d2d992d75 #x3a312c0b0b270b58
                                     #xeb49b2a2a210a279 #x295fc57676b37697
                                     #x8d3ef6b3b345b3f1 #xa31dc2bebe7cbe99
                                     #x9e501fceced1ce3e #xa914cebdbd73bd81
                                     #xc36d82aeae2cae19 #x4ca583e9e96ae91b
                                     #x1b91128a8a988a24 #xa697c43131f53195
                                     #x4854701c1c6c1ce0 #x52be97ecec7bec33
                                     #x1cede3f1f112f1db #x71e85e9999c799bc
                                     #x5fcb6a9494fe94d4 #xdb7192aaaa38aa39
                                     #x0ef8fff6f609f6e3 #xd4f2982626be262d
                                     #xe2cdbc2f2f932f65 #x58b79befef74ef2b
                                     #x4aa287e8e86fe813 #x0f830a8c8c868c14
                                     #xbe8bd43535e135b5 #x0a090c03030f0318
                                     #xc21677d4d4a3d4ee #x1f60e17f7f9e7fdf
                                     #x20dbcbfbfb30fb8b #x1e1b140505110528
                                     #xbc7d23c1c1e2c146 #xd987655e5e3b5eca
                                     #x47d77a9090ea90f4 #xc0e0802020a0201d
                                     #x8eb3f43d3dc93df5 #x2ba9328282b08264
                                     #x08fffbf7f70cf7eb #x46ac8feaea65ea03
                                     #x3c36280a0a220a50 #x2e23340d0d390d68
                                     #x1967e57e7e9b7ed7 #x2ad2c7f8f83ff893
                                     #xfdad5d50500d50ba #x5c46681a1a721ad0
                                     #xa26637c4c4f3c46e #x12151c07071b0738
                                     #xefb8415757165782 #xb70fdab8b862b8a9
                                     #x88b4f03c3ccc3cfd #x5133956262f76237
                                     #x7093abe3e348e34b #x8a4207c8c8cfc80e
                                     #xcf638aacac26ac09 #xf1a35552520752aa
                                     #x45218d6464e96407 #x6070401010501080
                                     #xda0a67d0d0b7d0ce #xec3543d9d99ad986
                                     #x6a794c13135f1398 #x2824300c0c3c0c60
                                     #x6c7e4812125a1290 #xf6dfa429298d2955
                                     #xfbaa5951510851b2 #xb108deb9b967b9a1
                                     #x98571bcfcfd4cf36 #xce187fd6d6a9d6fe
                                     #x3744d17373a273bf #x09840e8d8d838d1c
                                     #x21a03e8181bf817c #xe5b14d545419549a
                                     #xba7a27c0c0e7c04e #x54b993eded7eed3b
                                     #xb9f7254e4e6b4e4a #x85c10d444449441a
                                     #xf552a6a7a701a751 #xfcd6a82a2a822a4d
                                     #x39bc2e8585ab855c #xdefb942525b12535
                                     #x6e88bfe6e659e663 #x864c0fcacac5ca1e
                                     #x1569ed7c7c917cc7 #x1d96168b8b9d8b2c
                                     #xe9bf45565613568a #x27a73a8080ba8074)
                                    (#x501fceced1ce3e9e #x06d6bbbb6dbbb1bd
                                     #xab8bebeb60eb0b40 #xd9729292e092e44b
                                     #xac8feaea65ea0346 #x4b0bcbcbc0cb1680
                                     #x794c13135f13986a #x7d23c1c1e2c146bc
                                     #xa583e9e96ae91b4c #xa6e83a3ad23acd9c
                                     #x187fd6d6a9d6fece #x39f2b2b240b2f98b
                                     #x046fd2d2bdd2ded6 #xd77a9090ea90f447
                                     #x655c17174b17b872 #xd2c7f8f83ff8932a
                                     #xd315424257422a91 #x6b5415154115a87e
                                     #xbf45565613568ae9 #x2beab4b45eb4c99f
                                     #x26896565ec650f43 #x54701c1c6c1ce048
                                     #x9f1a888892883417 #xd411434352432297
                                     #x6133c5c5f6c566a4 #x896d5c5c315cdad5
                                     #x82d83636ee36adb4 #x01d2baba68bab9bb
                                     #xf1f3f5f506f5fb04 #xb8415757165782ef
                                     #x28816767e6671f4f #x840e8d8d838d1c09
                                     #x97c43131f53195a6 #xf8fff6f609f6e30e
                                     #x218d6464e9640745 #x957d58582558facd
                                     #xfd429e9edc9e8463 #xf6f7f4f403f4f302
                                     #xee882222aa220dcc #x7192aaaa38aa39db
                                     #x56c97575bc758f23 #x2d3c0f0f330f7822
                                     #x0e0802020a02100c #x30feb1b14fb1e181
                                     #x275bdfdf84dfb6f8 #x1ea96d6dc46d4f73
                                     #x44d17373a273bf37 #xfe294d4d644d52b3
                                     #x69ed7c7c917cc715 #xf2982626be262dd4
                                     #xcab82e2e962e6de4 #xfffbf7f70cf7eb08
                                     #x3820080828084030 #x8e695d5d345dd2d3
                                     #xc10d444449441a85 #xbaf83e3ec63eed84
                                     #xfa469f9fd99f8c65 #x6c5014144414a078
                                     #x4207c8c8cfc80e8a #x6d82aeae2cae19c3
                                     #xb14d545419549ae5 #x7040101050108060
                                     #x3247d8d89fd88eea #x13cabcbc76bc89af
                                     #x46681a1a721ad05c #x0cb16b6bda6b7f67
                                     #x02b96969d0696f6b #xe3ebf3f318f3cb10
                                     #x14cebdbd73bd81a9 #x99cc3333ff3385aa
                                     #x7696abab3dab31dd #xdccffafa35fa8326
                                     #x0d63d1d1b2d1c6dc #xe6569b9bcd9bac7d
                                     #x05bd6868d568676d #xf7254e4e6b4e4ab9
                                     #x625816164e16b074 #xcc6e9595fb95dc59
                                     #xd07e9191ef91fc41 #xb09feeee71ee235e
                                     #xf92d4c4c614c5ab5 #x34916363f2633f57
                                     #x8d028e8e8c8e0403 #x9c715b5b2a5be2c7
                                     #x5e17ccccdbcc2e92 #xb4f03c3ccc3cfd88
                                     #x4f6419197d19c856 #x40bea1a11fa161e1
                                     #xa03e8181bf817c21 #xe2394949704972ab
                                     #x7cf17b7b8a7bff07 #x3543d9d99ad986ec
                                     #x10a16f6fce6f5f7f #x85dc3737eb37a5b2
                                     #x3d9d6060fd60275d #x4c0fcacac5ca1e86
                                     #x8fbbe7e75ce76b68 #xd1ac2b2b872b45fa
                                     #xe53d484875487aad #xc9d3fdfd2efdbb34
                                     #xc5629696f496c453 #xc60945454c451283
                                     #xced7fcfc2bfcb332 #xda1941415841329b
                                     #x7e4812125a12906c #x23340d0d390d682e
                                     #x72f979798079ef0b #x81b3e5e556e57b64
                                     #x981e898997893c11 #x830a8c8c868c140f
                                     #x93abe3e348e34b70 #xe0802020a0201dc0
                                     #x90c03030f0309da0 #x2e57dcdc8bdcaef2
                                     #x22e6b7b751b7d195 #x19ad6c6cc16c4775
                                     #xeb354a4a7f4a6aa1 #x2ceeb5b55bb5c199
                                     #xbdfc3f3fc33fe582 #xc2669797f197cc55
                                     #x1677d4d4a3d4eec2 #x33956262f7623751
                                     #xc3b42d2d992d75ee #x121806061e063014
                                     #x5baaa4a40ea449ff #x5caea5a50ba541f9
                                     #xae368383b5836c2d #x80615f5f3e5fc2df
                                     #xd6a82a2a822a4dfc #x3c4fdada95da9ee6
                                     #x4503c9c9cac9068c #x0000000000000000
                                     #x67e57e7e9b7ed719 #x49b2a2a210a279eb
                                     #xb64955551c5592e3 #x1ac6bfbf79bf91a5
                                     #x7744111155118866 #x1173d5d5a6d5e6c4
                                     #xf34a9c9cd69c946f #x571bcfcfd4cf3698
                                     #x2a380e0e360e7024 #x36280a0a220a503c
                                     #xb3f43d3dc93df58e #xaa5951510851b2fb
                                     #x6ee97d7d947dcf13 #xde769393e593ec4d
                                     #x416c1b1b771bd85a #xc0dffefe21fea33e
                                     #x6637c4c4f3c46ea2 #xc80147474647028f
                                     #x3f2409092d094836 #xb5228686a4864433
                                     #x312c0b0b270b583a #x8a068f8f898f0c05
                                     #xf44e9d9dd39d9c69 #x0bb56a6adf6a7761
                                     #x151c07071b073812 #x08deb9b967b9a1b1
                                     #x37fab0b04ab0e987 #xef5a9898c298b477
                                     #x486018187818c050 #x9ec83232fa328dac
                                     #x4ad97171a871af3b #xec314b4b7a4b62a7
                                     #xb79befef74ef2b58 #xa1ec3b3bd73bc59a
                                     #x4ddd7070ad70a73d #x47baa0a01aa069e7
                                     #x86b7e4e453e47362 #xdd1d40405d403a9d
                                     #xc7dbffff24ffab38 #x732bc3c3e8c356b0
                                     #x789ea9a937a921d1 #x88bfe6e659e6636e
                                     #x75fd78788578e70d #xd5c3f9f93af99b2c
                                     #x96168b8b9d8b2c1d #xcf05464643460a89
                                     #xa73a8080ba807427 #x5a781e1e661ef044
                                     #xa8e03838d838dd90 #x9da3e1e142e15b7c
                                     #x0fdab8b862b8a9b7 #x7f9aa8a832a829d7
                                     #x9aa7e0e047e0537a #x24300c0c3c0c6028
                                     #xe98c2323af2305ca #x5fc57676b3769729
                                     #x53741d1d691de84e #xfb942525b12535de
                                     #xfc902424b4243dd8 #x1b1405051105281e
                                     #xede3f1f112f1db1c #x17a56e6ecb6e5779
                                     #xcb6a9494fe94d45f #xd8a0282888285df0
                                     #xe1529a9ac89aa47b #xbb2a8484ae84543f
                                     #xa287e8e86fe8134a #x4eb6a3a315a371ed
                                     #xf0214f4f6e4f42bf #x58c17777b6779f2f
                                     #x036bd3d3b8d3d6d0 #xbc2e8585ab855c39
                                     #x94afe2e24de24376 #xa35552520752aaf1
                                     #xe4eff2f21df2c316 #xa9328282b082642b
                                     #xad5d50500d50bafd #x7bf57a7a8f7af701
                                     #xcdbc2f2f932f65e2 #x51cd7474b9748725
                                     #xa45153530253a2f7 #x3ef6b3b345b3f18d
                                     #x3a996161f8612f5b #x6a86afaf29af11c5
                                     #xafe43939dd39d596 #x8bd43535e135b5be
                                     #x205fdede81debefe #x5913cdcddecd2694
                                     #x5d7c1f1f631ff842 #xe85e9999c799bc71
                                     #x638aacac26ac09cf #x648eadad23ad01c9
                                     #x43d57272a772b731 #xc4b02c2c9c2c7de8
                                     #x2953dddd8edda6f4 #x0a67d0d0b7d0ceda
                                     #xb2268787a1874c35 #x1dc2bebe7cbe99a3
                                     #x87655e5e3b5ecad9 #x55a2a6a604a659f3
                                     #xbe97ecec7bec3352 #x1c10040414042018
                                     #x683fc6c6f9c67eae #x090c03030f03180a
                                     #x8cd03434e434bdb8 #xdbcbfbfb30fb8b20
                                     #x3b4bdbdb90db96e0 #x927959592059f2cb
                                     #x25e2b6b654b6d993 #x742fc2c2edc25eb6
                                     #x0704010105010806 #xeae7f0f017f0d31a
                                     #x9b755a5a2f5aeac1 #xb993eded7eed3b54
                                     #x52a6a7a701a751f5 #x2f856666e3661749
                                     #xe7842121a52115c6 #x60e17f7f9e7fdf1f
                                     #x91128a8a988a241b #xf59c2727bb2725d2
                                     #x6f3bc7c7fcc776a8 #x7a27c0c0e7c04eba
                                     #xdfa429298d2955f6 #x1f7bd7d7acd7f6c8)
                                    (#x769393e593ec4dde #x43d9d99ad986ec35
                                     #x529a9ac89aa47be1 #xeeb5b55bb5c1992c
                                     #x5a9898c298b477ef #x882222aa220dccee
                                     #x0945454c451283c6 #xd7fcfc2bfcb332ce
                                     #xd2baba68bab9bb01 #xb56a6adf6a77610b
                                     #x5bdfdf84dfb6f827 #x0802020a02100c0e
                                     #x469f9fd99f8c65fa #x57dcdc8bdcaef22e
                                     #x5951510851b2fbaa #x7959592059f2cb92
                                     #x354a4a7f4a6aa1eb #x5c17174b17b87265
                                     #xac2b2b872b45fad1 #x2fc2c2edc25eb674
                                     #x6a9494fe94d45fcb #xf7f4f403f4f302f6
                                     #xd6bbbb6dbbb1bd06 #xb6a3a315a371ed4e
                                     #x956262f762375133 #xb7e4e453e4736286
                                     #xd97171a871af3b4a #x77d4d4a3d4eec216
                                     #x13cdcddecd269459 #xdd7070ad70a73d4d
                                     #x5816164e16b07462 #xa3e1e142e15b7c9d
                                     #x394949704972abe2 #xf03c3ccc3cfd88b4
                                     #x27c0c0e7c04eba7a #x47d8d89fd88eea32
                                     #x6d5c5c315cdad589 #x569b9bcd9bac7de6
                                     #x8eadad23ad01c964 #x2e8585ab855c39bc
                                     #x5153530253a2f7a4 #xbea1a11fa161e140
                                     #xf57a7a8f7af7017b #x07c8c8cfc80e8a42
                                     #xb42d2d992d75eec3 #xa7e0e047e0537a9a
                                     #x63d1d1b2d1c6dc0d #xd57272a772b73143
                                     #xa2a6a604a659f355 #xb02c2c9c2c7de8c4
                                     #x37c4c4f3c46ea266 #xabe3e348e34b7093
                                     #xc57676b37697295f #xfd78788578e70d75
                                     #xe6b7b751b7d19522 #xeab4b45eb4c99f2b
                                     #x2409092d0948363f #xec3b3bd73bc59aa1
                                     #x380e0e360e70242a #x1941415841329bda
                                     #x2d4c4c614c5ab5f9 #x5fdede81debefe20
                                     #xf2b2b240b2f98b39 #x7a9090ea90f447d7
                                     #x942525b12535defb #xaea5a50ba541f95c
                                     #x7bd7d7acd7f6c81f #x0c03030f03180a09
                                     #x4411115511886677 #x0000000000000000
                                     #x2bc3c3e8c356b073 #xb82e2e962e6de4ca
                                     #x729292e092e44bd9 #x9befef74ef2b58b7
                                     #x254e4e6b4e4ab9f7 #x4812125a12906c7e
                                     #x4e9d9dd39d9c69f4 #xe97d7d947dcf136e
                                     #x0bcbcbc0cb16804b #xd43535e135b5be8b
                                     #x4010105010806070 #x73d5d5a6d5e6c411
                                     #x214f4f6e4f42bff0 #x429e9edc9e8463fd
                                     #x294d4d644d52b3fe #x9ea9a937a921d178
                                     #x4955551c5592e3b6 #x3fc6c6f9c67eae68
                                     #x67d0d0b7d0ceda0a #xf17b7b8a7bff077c
                                     #x6018187818c05048 #x669797f197cc55c2
                                     #x6bd3d3b8d3d6d003 #xd83636ee36adb482
                                     #xbfe6e659e6636e88 #x3d484875487aade5
                                     #x45565613568ae9bf #x3e8181bf817c21a0
                                     #x068f8f898f0c058a #xc17777b6779f2f58
                                     #x17ccccdbcc2e925e #x4a9c9cd69c946ff3
                                     #xdeb9b967b9a1b108 #xafe2e24de2437694
                                     #x8aacac26ac09cf63 #xdab8b862b8a9b70f
                                     #xbc2f2f932f65e2cd #x5415154115a87e6b
                                     #xaaa4a40ea449ff5b #xed7c7c917cc71569
                                     #x4fdada95da9ee63c #xe03838d838dd90a8
                                     #x781e1e661ef0445a #x2c0b0b270b583a31
                                     #x1405051105281e1b #x7fd6d6a9d6fece18
                                     #x5014144414a0786c #xa56e6ecb6e577917
                                     #xad6c6cc16c477519 #xe57e7e9b7ed71967
                                     #x856666e36617492f #xd3fdfd2efdbb34c9
                                     #xfeb1b14fb1e18130 #xb3e5e556e57b6481
                                     #x9d6060fd60275d3d #x86afaf29af11c56a
                                     #x655e5e3b5ecad987 #xcc3333ff3385aa99
                                     #x268787a1874c35b2 #x03c9c9cac9068c45
                                     #xe7f0f017f0d31aea #x695d5d345dd2d38e
                                     #xa96d6dc46d4f731e #xfc3f3fc33fe582bd
                                     #x1a8888928834179f #x0e8d8d838d1c0984
                                     #x3bc7c7fcc776a86f #xfbf7f70cf7eb08ff
                                     #x741d1d691de84e53 #x83e9e96ae91b4ca5
                                     #x97ecec7bec3352be #x93eded7eed3b54b9
                                     #x3a8080ba807427a7 #xa429298d2955f6df
                                     #x9c2727bb2725d2f5 #x1bcfcfd4cf369857
                                     #x5e9999c799bc71e8 #x9aa8a832a829d77f
                                     #x5d50500d50bafdad #x3c0f0f330f78222d
                                     #xdc3737eb37a5b285 #x902424b4243dd8fc
                                     #xa0282888285df0d8 #xc03030f0309da090
                                     #x6e9595fb95dc59cc #x6fd2d2bdd2ded604
                                     #xf83e3ec63eed84ba #x715b5b2a5be2c79c
                                     #x1d40405d403a9ddd #x368383b5836c2dae
                                     #xf6b3b345b3f18d3e #xb96969d0696f6b02
                                     #x415757165782efb8 #x7c1f1f631ff8425d
                                     #x1c07071b07381215 #x701c1c6c1ce04854
                                     #x128a8a988a241b91 #xcabcbc76bc89af13
                                     #x802020a0201dc0e0 #x8bebeb60eb0b40ab
                                     #x1fceced1ce3e9e50 #x028e8e8c8e04038d
                                     #x96abab3dab31dd76 #x9feeee71ee235eb0
                                     #xc43131f53195a697 #xb2a2a210a279eb49
                                     #xd17373a273bf3744 #xc3f9f93af99b2cd5
                                     #x0fcacac5ca1e864c #xe83a3ad23acd9ca6
                                     #x681a1a721ad05c46 #xcbfbfb30fb8b20db
                                     #x340d0d390d682e23 #x23c1c1e2c146bc7d
                                     #xdffefe21fea33ec0 #xcffafa35fa8326dc
                                     #xeff2f21df2c316e4 #xa16f6fce6f5f7f10
                                     #xcebdbd73bd81a914 #x629696f496c453c5
                                     #x53dddd8edda6f429 #x11434352432297d4
                                     #x5552520752aaf1a3 #xe2b6b654b6d99325
                                     #x2008082808403038 #xebf3f318f3cb10e3
                                     #x82aeae2cae19c36d #xc2bebe7cbe99a31d
                                     #x6419197d19c8564f #x1e898997893c1198
                                     #xc83232fa328dac9e #x982626be262dd4f2
                                     #xfab0b04ab0e98737 #x8feaea65ea0346ac
                                     #x314b4b7a4b62a7ec #x8d6464e964074521
                                     #x2a8484ae84543fbb #x328282b082642ba9
                                     #xb16b6bda6b7f670c #xf3f5f506f5fb04f1
                                     #xf979798079ef0b72 #xc6bfbf79bf91a51a
                                     #x0401010501080607 #x615f5f3e5fc2df80
                                     #xc97575bc758f2356 #x916363f2633f5734
                                     #x6c1b1b771bd85a41 #x8c2323af2305cae9
                                     #xf43d3dc93df58eb3 #xbd6868d568676d05
                                     #xa82a2a822a4dfcd6 #x896565ec650f4326
                                     #x87e8e86fe8134aa2 #x7e9191ef91fc41d0
                                     #xfff6f609f6e30ef8 #xdbffff24ffab38c7
                                     #x4c13135f13986a79 #x7d58582558facd95
                                     #xe3f1f112f1db1ced #x0147474647028fc8
                                     #x280a0a220a503c36 #xe17f7f9e7fdf1f60
                                     #x33c5c5f6c566a461 #xa6a7a701a751f552
                                     #xbbe7e75ce76b688f #x996161f8612f5b3a
                                     #x755a5a2f5aeac19b #x1806061e06301412
                                     #x05464643460a89cf #x0d444449441a85c1
                                     #x15424257422a91d3 #x100404140420181c
                                     #xbaa0a01aa069e747 #x4bdbdb90db96e03b
                                     #xe43939dd39d596af #x228686a4864433b5
                                     #x4d545419549ae5b1 #x92aaaa38aa39db71
                                     #x0a8c8c868c140f83 #xd03434e434bdb88c
                                     #x842121a52115c6e7 #x168b8b9d8b2c1d96
                                     #xc7f8f83ff8932ad2 #x300c0c3c0c602824
                                     #xcd7474b974872551 #x816767e6671f4f28)
                                    (#x6868d568676d05bd #x8d8d838d1c09840e
                                     #xcacac5ca1e864c0f #x4d4d644d52b3fe29
                                     #x7373a273bf3744d1 #x4b4b7a4b62a7ec31
                                     #x4e4e6b4e4ab9f725 #x2a2a822a4dfcd6a8
                                     #xd4d4a3d4eec21677 #x52520752aaf1a355
                                     #x2626be262dd4f298 #xb3b345b3f18d3ef6
                                     #x545419549ae5b14d #x1e1e661ef0445a78
                                     #x19197d19c8564f64 #x1f1f631ff8425d7c
                                     #x2222aa220dccee88 #x03030f03180a090c
                                     #x464643460a89cf05 #x3d3dc93df58eb3f4
                                     #x2d2d992d75eec3b4 #x4a4a7f4a6aa1eb35
                                     #x53530253a2f7a451 #x8383b5836c2dae36
                                     #x13135f13986a794c #x8a8a988a241b9112
                                     #xb7b751b7d19522e6 #xd5d5a6d5e6c41173
                                     #x2525b12535defb94 #x79798079ef0b72f9
                                     #xf5f506f5fb04f1f3 #xbdbd73bd81a914ce
                                     #x58582558facd957d #x2f2f932f65e2cdbc
                                     #x0d0d390d682e2334 #x02020a02100c0e08
                                     #xeded7eed3b54b993 #x51510851b2fbaa59
                                     #x9e9edc9e8463fd42 #x1111551188667744
                                     #xf2f21df2c316e4ef #x3e3ec63eed84baf8
                                     #x55551c5592e3b649 #x5e5e3b5ecad98765
                                     #xd1d1b2d1c6dc0d63 #x16164e16b0746258
                                     #x3c3ccc3cfd88b4f0 #x6666e36617492f85
                                     #x7070ad70a73d4ddd #x5d5d345dd2d38e69
                                     #xf3f318f3cb10e3eb #x45454c451283c609
                                     #x40405d403a9ddd1d #xccccdbcc2e925e17
                                     #xe8e86fe8134aa287 #x9494fe94d45fcb6a
                                     #x565613568ae9bf45 #x0808280840303820
                                     #xceced1ce3e9e501f #x1a1a721ad05c4668
                                     #x3a3ad23acd9ca6e8 #xd2d2bdd2ded6046f
                                     #xe1e142e15b7c9da3 #xdfdf84dfb6f8275b
                                     #xb5b55bb5c1992cee #x3838d838dd90a8e0
                                     #x6e6ecb6e577917a5 #x0e0e360e70242a38
                                     #xe5e556e57b6481b3 #xf4f403f4f302f6f7
                                     #xf9f93af99b2cd5c3 #x8686a4864433b522
                                     #xe9e96ae91b4ca583 #x4f4f6e4f42bff021
                                     #xd6d6a9d6fece187f #x8585ab855c39bc2e
                                     #x2323af2305cae98c #xcfcfd4cf3698571b
                                     #x3232fa328dac9ec8 #x9999c799bc71e85e
                                     #x3131f53195a697c4 #x14144414a0786c50
                                     #xaeae2cae19c36d82 #xeeee71ee235eb09f
                                     #xc8c8cfc80e8a4207 #x484875487aade53d
                                     #xd3d3b8d3d6d0036b #x3030f0309da090c0
                                     #xa1a11fa161e140be #x9292e092e44bd972
                                     #x41415841329bda19 #xb1b14fb1e18130fe
                                     #x18187818c0504860 #xc4c4f3c46ea26637
                                     #x2c2c9c2c7de8c4b0 #x7171a871af3b4ad9
                                     #x7272a772b73143d5 #x444449441a85c10d
                                     #x15154115a87e6b54 #xfdfd2efdbb34c9d3
                                     #x3737eb37a5b285dc #xbebe7cbe99a31dc2
                                     #x5f5f3e5fc2df8061 #xaaaa38aa39db7192
                                     #x9b9bcd9bac7de656 #x8888928834179f1a
                                     #xd8d89fd88eea3247 #xabab3dab31dd7696
                                     #x898997893c11981e #x9c9cd69c946ff34a
                                     #xfafa35fa8326dccf #x6060fd60275d3d9d
                                     #xeaea65ea0346ac8f #xbcbc76bc89af13ca
                                     #x6262f76237513395 #x0c0c3c0c60282430
                                     #x2424b4243dd8fc90 #xa6a604a659f355a2
                                     #xa8a832a829d77f9a #xecec7bec3352be97
                                     #x6767e6671f4f2881 #x2020a0201dc0e080
                                     #xdbdb90db96e03b4b #x7c7c917cc71569ed
                                     #x282888285df0d8a0 #xdddd8edda6f42953
                                     #xacac26ac09cf638a #x5b5b2a5be2c79c71
                                     #x3434e434bdb88cd0 #x7e7e9b7ed71967e5
                                     #x1010501080607040 #xf1f112f1db1cede3
                                     #x7b7b8a7bff077cf1 #x8f8f898f0c058a06
                                     #x6363f2633f573491 #xa0a01aa069e747ba
                                     #x05051105281e1b14 #x9a9ac89aa47be152
                                     #x434352432297d411 #x7777b6779f2f58c1
                                     #x2121a52115c6e784 #xbfbf79bf91a51ac6
                                     #x2727bb2725d2f59c #x09092d0948363f24
                                     #xc3c3e8c356b0732b #x9f9fd99f8c65fa46
                                     #xb6b654b6d99325e2 #xd7d7acd7f6c81f7b
                                     #x29298d2955f6dfa4 #xc2c2edc25eb6742f
                                     #xebeb60eb0b40ab8b #xc0c0e7c04eba7a27
                                     #xa4a40ea449ff5baa #x8b8b9d8b2c1d9616
                                     #x8c8c868c140f830a #x1d1d691de84e5374
                                     #xfbfb30fb8b20dbcb #xffff24ffab38c7db
                                     #xc1c1e2c146bc7d23 #xb2b240b2f98b39f2
                                     #x9797f197cc55c266 #x2e2e962e6de4cab8
                                     #xf8f83ff8932ad2c7 #x6565ec650f432689
                                     #xf6f609f6e30ef8ff #x7575bc758f2356c9
                                     #x07071b073812151c #x0404140420181c10
                                     #x4949704972abe239 #x3333ff3385aa99cc
                                     #xe4e453e4736286b7 #xd9d99ad986ec3543
                                     #xb9b967b9a1b108de #xd0d0b7d0ceda0a67
                                     #x424257422a91d315 #xc7c7fcc776a86f3b
                                     #x6c6cc16c477519ad #x9090ea90f447d77a
                                     #x0000000000000000 #x8e8e8c8e04038d02
                                     #x6f6fce6f5f7f10a1 #x50500d50bafdad5d
                                     #x0101050108060704 #xc5c5f6c566a46133
                                     #xdada95da9ee63c4f #x47474647028fc801
                                     #x3f3fc33fe582bdfc #xcdcddecd26945913
                                     #x6969d0696f6b02b9 #xa2a210a279eb49b2
                                     #xe2e24de2437694af #x7a7a8f7af7017bf5
                                     #xa7a701a751f552a6 #xc6c6f9c67eae683f
                                     #x9393e593ec4dde76 #x0f0f330f78222d3c
                                     #x0a0a220a503c3628 #x06061e0630141218
                                     #xe6e659e6636e88bf #x2b2b872b45fad1ac
                                     #x9696f496c453c562 #xa3a315a371ed4eb6
                                     #x1c1c6c1ce0485470 #xafaf29af11c56a86
                                     #x6a6adf6a77610bb5 #x12125a12906c7e48
                                     #x8484ae84543fbb2a #x3939dd39d596afe4
                                     #xe7e75ce76b688fbb #xb0b04ab0e98737fa
                                     #x8282b082642ba932 #xf7f70cf7eb08fffb
                                     #xfefe21fea33ec0df #x9d9dd39d9c69f44e
                                     #x8787a1874c35b226 #x5c5c315cdad5896d
                                     #x8181bf817c21a03e #x3535e135b5be8bd4
                                     #xdede81debefe205f #xb4b45eb4c99f2bea
                                     #xa5a50ba541f95cae #xfcfc2bfcb332ced7
                                     #x8080ba807427a73a #xefef74ef2b58b79b
                                     #xcbcbc0cb16804b0b #xbbbb6dbbb1bd06d6
                                     #x6b6bda6b7f670cb1 #x7676b37697295fc5
                                     #xbaba68bab9bb01d2 #x5a5a2f5aeac19b75
                                     #x7d7d947dcf136ee9 #x78788578e70d75fd
                                     #x0b0b270b583a312c #x9595fb95dc59cc6e
                                     #xe3e348e34b7093ab #xadad23ad01c9648e
                                     #x7474b974872551cd #x9898c298b477ef5a
                                     #x3b3bd73bc59aa1ec #x3636ee36adb482d8
                                     #x6464e9640745218d #x6d6dc46d4f731ea9
                                     #xdcdc8bdcaef22e57 #xf0f017f0d31aeae7
                                     #x59592059f2cb9279 #xa9a937a921d1789e
                                     #x4c4c614c5ab5f92d #x17174b17b872655c
                                     #x7f7f9e7fdf1f60e1 #x9191ef91fc41d07e
                                     #xb8b862b8a9b70fda #xc9c9cac9068c4503
                                     #x5757165782efb841 #x1b1b771bd85a416c
                                     #xe0e047e0537a9aa7 #x6161f8612f5b3a99))))

  (defconst +kalyna-it+
    (make-array '(8 256)
                :element-type '(unsigned-byte 64)
                :initial-contents '((#x7826942b9f5f8a9a #x210f43c934970c53
                                     #x5f028fdd9d0551b8 #x14facd82b494c83b
                                     #x2b72ab886edd68c0 #xa6a87e5bff19d9b4
                                     #xa29ae571db6443ea #x039b2c911be8e5b6
                                     #xd9275dcb5fd32cc6 #x10c856a890e95265
                                     #x7d96e085b27ab85d #x31c71561a47e5e36
                                     #x74702455f3d83978 #xe8e048aafbad72f0
                                     #x9b39db4437e03460 #x75f2cbd1fa8091e1
                                     #x1ab5bee9caa336f6 #x8395a6b8eff34fb9
                                     #x64b872fd63316b1d #xe1068c7aba0ff3d5
                                     #xeecb1095cd60a581 #xbc1dc0b235baef42
                                     #xf04c355623be0929 #xb252b3d94b8d118f
                                     #x18ac7dfcd8137bd9 #xbbb477090a2f90aa
                                     #x8625d216c2d67d7e #x66a1b1e871812632
                                     #x6f4775383023a717 #x92df1f947642b545
                                     #xe962a72ef2f5da69 #x8bf18deca7096605
                                     #xc86de4e7c662d63a #xaafece25939e6a56
                                     #x5c99a34c86edb40e #x52d6d027f8da4ac3
                                     #x6b75ee12145e3d49 #x54fd8818ce179db2
                                     #xa3180af5d23ceb73 #xbe0403a7270aa26d
                                     #xfe03463d5d89f7e4 #xf1cedad22ae6a1b0
                                     #xd143769f1729057a #xc7a07808b10d806e
                                     #xfc1a85284f39bacb #xa4b1bd4eeda9949b
                                     #x0bff07c55312cc0a #xef49ff11c4380d18
                                     #xc392e32295701a30 #x7f8f2390a0caf572
                                     #x62932ac255fcbc6c #xc9ef0b63cf3a7ea3
                                     #xf9aaf186621c880c #x818c65adfd430296
                                     #x325c39f0bf96bb80 #x0c56b07e6c87b3e2
                                     #x4bf8425f29919983 #xb5fb046274186e67
                                     #x462c1da54c4e82f8 #x90c6dc8164f2f86a
                                     #xf8281e026b442095 #x6af701961d0695d0
                                     #x5766a489d5ff7804 #xf3d719c73856ec9f
                                     #xad57799eac0b15be #x1b37516dc3fb9e6f
                                     #xc009cfb38e98ff86 #x9576a82f49d7caad
                                     #xe6af3bc1859a8c3d #x208dac4d3dcfa4ca
                                     #x8ddad5d391c4b174 #x8e41f9428a2c54c2
                                     #x6cdc59a92bcb42a1 #xe53417509e72698b
                                     #xd0c1991b1e71ade3 #x8217493ce6abe720
                                     #xd4f302313a0c37bd #x5e806059945df921
                                     #x73d993eecc4d4690 #xf5fc41f80e9b3bee
                                     #x13537a398b01b7d3 #x53543fa3f182e25a
                                     #x2d59f3b75810bfb1 #x35f58e4b8003c468
                                     #x886aa17dbce183b3 #x4c51f5e41604e66b
                                     #x98a2f7d52c08d1d6 #xa101c9e0c08ca65c
                                     #x4007459a7a835589 #xcc5f7fcde21f4c64
                                     #xa965e2b488768fe0 #x12d195bd82591f4a
                                     #x2f4030a24aa0f29e #x56e44b0ddca7d09d
                                     #x914433056daa50f3 #x37ec4d5e92b38947
                                     #xe31f4f6fa8bfbefa #x50cf1332ea6a07ec
                                     #x6d5eb62d2293ea38 #x09e6c4d041a28125
                                     #x8fc316c68374fc5b #x421e868f683318a6
                                     #xe08463feb3575b4c #x3821d1b1e5dcdf13
                                     #xed503c04d6884037 #xd35ab58a05994855
                                     #x976f6b3a5b678782 #x6ec59abc397b0f8e
                                     #x5929d7e2abc886c9 #xa53352cae4f13c02
                                     #x89e84ef9b5b92b2a #x1761e113af7c2d8d
                                     #x28e9871975358d76 #xdc97296572f61e01
                                     #x67235e6c78d98eab #x3d91a51fc8f9edd4
                                     #x68eec2830fb6d8ff #xfbb3329370acc523
                                     #x062b583f36cdd771 #x15782206bdcc60a2
                                     #x16e30e97a6248514 #x79a47baf96072203
                                     #xf7e582ed1c2b76c1 #xde8eea706046532e
                                     #xaf4eba8bbebb5891 #x08642b5448fa29bc
                                     #x24bf376719b23e94 #x231680dc2627417c
                                     #x0dd45ffa65df1b7b #x1d1c0952f536491e
                                     #xff81a9b954d15f7d #x992018512550794f
                                     #x71c050fbdefd0bbf #xc18b203787c0571f
                                     #x253dd8e310ea960d #xeb7b643be0459746
                                     #x0219c31512b04d2f #xc43b5499aae565d8
                                     #xeaf98bbfe91d3fdf #x3a3812a4f76c923c
                                     #x4dd31a601f5c4ef2 #xa8e70d30812e2779
                                     #x800e8a29f41baa0f #x1c9ee6d6fc6ee187
                                     #x5d1b4cc88fb51c97 #x610806534e1459da
                                     #xf255f643310e4406 #xd2d85a0e0cc1e0cc
                                     #x0182ef840958a899 #x7e0dcc14a9925deb
                                     #x653a9d796a69c384 #x4e4836f104b4ab44
                                     #x4fcad9750dec03dd #xcddd9049eb47e4fd
                                     #x0e4f736b7e37fecd #x4185aa1e73dbfd10
                                     #x725b7c6ac515ee09 #x8a736268ae51ce9c
                                     #xc5b9bb1da3bdcd41 #x7bbdb8ba84b76f2c
                                     #xdabc715a443bc970 #xe29da0eba1e71663
                                     #x935df0107f1a1ddc #x608ae9d7474cf143
                                     #xd571edb533549f24 #xa0832664c9d40ec5
                                     #xfd986aac46611252 #x4435deb05efecfd7
                                     #x0000000000000000 #x2cdb1c3351481728
                                     #x94f447ab408f6234 #x45b7313457a6674e
                                     #xb82f5b9811c7751c #x8c583a57989c19ed
                                     #xdd15c6e17baeb698 #x696c2d0706ee7066
                                     #x3f88660ada49a0fb #xf47eae7c07c39377
                                     #x05b074ae2d2532c7 #xb3d05c5d42d5b916
                                     #x39a33e35ec84778a #x0fcd9cef776f5654
                                     #xacd5961aa553bd27 #x5b3014f7b978cbe6
                                     #x347761cf895b6cf1 #xc622978cb85528f7
                                     #xb7e2c77766a82348 #x77eb08c4e830dcce
                                     #xb9adb41c189fdd85 #x114ab92c99b1fafc
                                     #x26a6f4720b0273bb #x1e8725c3eedeaca8
                                     #x2af0440c6785c059 #x04329b2a247d9a5e
                                     #xd7682ea021e4d20b #x7c140f01bb2210c4
                                     #x96ed84be523f2f1b #xca7427f2d4d29b15
                                     #x47aef22145162a61 #xa72a91dff641712d
                                     #x5ab2fb73b020637f #xcbf6c876dd8a338c
                                     #x6311c5465ca414f5 #x07a9b7bb3f957fe8
                                     #xe72dd4458cc224a4 #x9d12837b012de311
                                     #x843c1103d0663051 #x0a7de8415a4a6493
                                     #xd6eac12428bc7a92 #x9c906cff08754b88
                                     #x7042bf7fd7a5a326 #xbd9f2f363ce247db
                                     #xb66028f36ff08bd1 #x192e9278d14bd340
                                     #x9f0b406e139dae3e #x1f05ca47e7860431
                                     #x85befe87d93e98c8 #x439c690b616bb03f
                                     #xba36988d03773833 #x87a73d92cb8ed5e7
                                     #xaecc550fb7e3f008 #xc2100ca69c28b2a9
                                     #x9abb34c03eb89cf9 #x49e1814a3b21d4ac
                                     #xecd2d380dfd0e8ae #x296b689d7c6d25ef
                                     #x3c134a9bc1a1454d #xcfc4535cf9f7a9d2
                                     #x557f679cc74f352b #xb479ebe67d40c6fe
                                     #xf6676d691573de58 #x9e89afea1ac506a7
                                     #xd8a5b24f568b845f #x48636ece32797c35
                                     #xdf0c05f4691efbb7 #xe4b6f8d4972ac112
                                     #xfa31dd1779f46dba #xbf86ec232e520af4
                                     #x3e0a898ed3110862 #x7a3f573e8defc7b5
                                     #x27241bf6025adb22 #x58ab3866a2902e50
                                     #x3bbafd20fe343aa5 #x3045fae5ad26f6af
                                     #x2ec2df2643f85a07 #x22946f582f7fe9e5
                                     #x366ea2da9beb21de #x4a7aaddb20c9311a
                                     #xb1c99f485065f439 #xb04b70cc593d5ca0
                                     #xab7c21a19ac6c2cf #x33ded674b6ce1319
                                     #xce46bcd8f0af014b #xdb3e9ede4d6361e9
                                     #x7669e740e1687457 #x514dfcb6e332af75)
                                    (#x1f4f6fa8bfbefae3 #xf0440c6785c0592a
                                     #x1dc0b235baef42bc #x22978cb85528f7c6
                                     #xcedad22ae6a1b0f1 #x180af5d23ceb73a3
                                     #x946f582f7fe9e522 #xe44b0ddca7d09d56
                                     #x906cff08754b889c #x9f2f363ce247dbbd
                                     #xa1b1e87181263266 #x21d1b1e5dcdf1338
                                     #x31dd1779f46dbafa #x4b70cc593d5ca0b0
                                     #xd719c73856ec9ff3 #x8725c3eedeaca81e
                                     #x71edb533549f24d5 #x12837b012de3119d
                                     #x3dd8e310ea960d25 #x29d7e2abc886c959
                                     #xb477090a2f90aabb #x45fae5ad26f6af30
                                     #x9ee6d6fc6ee1871c #xbefe87d93e98c885
                                     #xe30e97a624851416 #xd6d027f8da4ac352
                                     #xcc550fb7e3f008ae #x5ab58a05994855d3
                                     #x806059945df9215e #x82ef840958a89901
                                     #x4ab92c99b1fafc11 #x281e026b442095f8
                                     #x62a72ef2f5da69e9 #x8b203787c0571fc1
                                     #x4f736b7e37fecd0e #xab3866a2902e5058
                                     #x6ea2da9beb21de36 #xf447ab408f623494
                                     #x235e6c78d98eab67 #x11c5465ca414f563
                                     #xd31a601f5c4ef24d #xa2f7d52c08d1d698
                                     #x85aa1e73dbfd1041 #xdc59a92bcb42a16c
                                     #x59f3b75810bfb12d #xe2c77766a82348b7
                                     #xb9bb1da3bdcd41c5 #x96e085b27ab85d7d
                                     #x99a34c86edb40e5c #x66a489d5ff780457
                                     #x95a6b8eff34fb983 #x7f679cc74f352b55
                                     #x7de8415a4a64930a #x9b2c911be8e5b603
                                     #x4836f104b4ab444e #xdb1c33514817282c
                                     #x15c6e17baeb698dd #xed84be523f2f1b96
                                     #xe1814a3b21d4ac49 #x503c04d6884037ed
                                     #x4c355623be0929f0 #x3b5499aae565d8c4
                                     #x0a898ed31108623e #xb074ae2d2532c705
                                     #x028fdd9d0551b85f #xf58e4b8003c46835
                                     #x3352cae4f13c02a5 #x6c2d0706ee706669
                                     #x7c21a19ac6c2cfab #x19c31512b04d2f02
                                     #xa6f4720b0273bb26 #x05ca47e78604311f
                                     #x46bcd8f0af014bce #x1e868f683318a642
                                     #x5c39f0bf96bb8032 #x79ebe67d40c6feb4
                                     #xff07c55312cc0a0b #xaef22145162a6147
                                     #xc1991b1e71ade3d0 #xded674b6ce131933
                                     #x7aaddb20c9311a4a #x4dfcb6e332af7551
                                     #x6de4e7c662d63ac8 #xbf376719b23e9424
                                     #x07459a7a83558940 #xac7dfcd8137bd918
                                     #xdf1f947642b54592 #x17493ce6abe72082
                                     #xfc41f80e9b3beef5 #xe70d30812e2779a8
                                     #xd993eecc4d469073 #x65e2b488768fe0a9
                                     #xd2d380dfd0e8aeec #xe6c4d041a2812509
                                     #x068c7aba0ff3d5e1 #x51f5e41604e66b4c
                                     #x41f9428a2c54c28e #x537a398b01b7d313
                                     #x782206bdcc60a215 #x89afea1ac506a79e
                                     #x8ae9d7474cf14360 #xf6c876dd8a338ccb
                                     #x43769f1729057ad1 #x8dac4d3dcfa4ca20
                                     #xb7313457a6674e45 #x2018512550794f99
                                     #xbb34c03eb89cf99a #xbafd20fe343aa53b
                                     #x03463d5d89f7e4fe #x42bf7fd7a5a32670
                                     #x3f573e8defc7b57a #xadb41c189fdd85b9
                                     #xcad9750dec03dd4f #x0f43c934970c5321
                                     #x2f5b9811c7751cb8 #xd85a0e0cc1e0ccd2
                                     #xe048aafbad72f0e8 #xf18deca70966058b
                                     #xdd9049eb47e4fdcd #xa87e5bff19d9b4a6
                                     #x5df0107f1a1ddc93 #xd195bd82591f4a12
                                     #x0c05f4691efbb7df #x8463feb3575b4ce0
                                     #x55f643310e4406f2 #xb6f8d4972ac112e4
                                     #x4030a24aa0f29e2f #xfd8818ce179db254
                                     #x3c1103d066305184 #x682ea021e4d20bd7
                                     #x81a9b954d15f7dff #x275dcb5fd32cc6d9
                                     #xfacd82b494c83b14 #x4433056daa50f391
                                     #xe9871975358d7628 #xeac12428bc7a92d6
                                     #x1a85284f39bacbfc #xf8425f299199834b
                                     #x676d691573de58f6 #xd05c5d42d5b916b3
                                     #x8eea706046532ede #xfb046274186e67b5
                                     #x134a9bc1a1454d3c #x57799eac0b15bead
                                     #x241bf6025adb2227 #x72ab886edd68c02b
                                     #x9ae571db6443eaa2 #xc050fbdefd0bbf71
                                     #xa5b24f568b845fd8 #xe84ef9b5b92b2a89
                                     #x6f6b3a5b67878297 #xc6dc8164f2f86a90
                                     #x7eae7c07c39377f4 #x5eb62d2293ea386d
                                     #x8c65adfd43029681 #x2dd4458cc224a4e7
                                     #xfece25939e6a56aa #xcd9cef776f56540f
                                     #xa33e35ec84778a39 #xc2df2643f85a072e
                                     #xbc715a443bc970da #xa07808b10d806ec7
                                     #x36988d03773833ba #x1680dc2627417c23
                                     #xcb1095cd60a581ee #xbdb8ba84b76f2c7b
                                     #x702455f3d8397874 #x35deb05efecfd744
                                     #x8f2390a0caf5727f #xb1bd4eeda9949ba4
                                     #x39db4437e034609b #xe582ed1c2b76c1f7
                                     #xc4535cf9f7a9d2cf #xb2fb73b020637f5a
                                     #x583a57989c19ed8c #x25d216c2d67d7e86
                                     #x0806534e1459da61 #x6b689d7c6d25ef29
                                     #x0dcc14a9925deb7e #xc99f485065f439b1
                                     #xa9b7bb3f957fe807 #x2a91dff641712da7
                                     #x1c0952f536491e1d #x75ee12145e3d496b
                                     #xf98bbfe91d3fdfea #x92e32295701a30c3
                                     #x3e9ede4d6361e9db #x76a82f49d7caad95
                                     #x9da0eba1e71663e2 #x09cfb38e98ff86c0
                                     #x9c690b616bb03f43 #xdad5d391c4b1748d
                                     #x3812a4f76c923c3a #x5f7fcde21f4c64cc
                                     #x6aa17dbce183b388 #xeec2830fb6d8ff68
                                     #x736268ae51ce9c8a #xa47baf9607220379
                                     #x543fa3f182e25a53 #x4eba8bbebb5891af
                                     #x2e9278d14bd34019 #x69e740e168745776
                                     #x37516dc3fb9e6f1b #xb3329370acc523fb
                                     #x3a9d796a69c38465 #x7761cf895b6cf134
                                     #x0000000000000000 #x88660ada49a0fb3f
                                     #xb5bee9caa336f61a #x5b7c6ac515ee0972
                                     #x52b3d94b8d118fb2 #x329b2a247d9a5e04
                                     #x0e8a29f41baa0f80 #x642b5448fa29bc08
                                     #x7b643be0459746eb #xd45ffa65df1b7b0d
                                     #xeb08c4e830dcce77 #xf2cbd1fa8091e175
                                     #xf302313a0c37bdd4 #x91a51fc8f9edd43d
                                     #xef0b63cf3a7ea3c9 #xc316c68374fc5b8f
                                     #x01c9e0c08ca65ca1 #x3417509e72698be5
                                     #x4775383023a7176f #x636ece32797c3548
                                     #x1b4cc88fb51c975d #x140f01bb2210c47c
                                     #x7427f2d4d29b15ca #xa73d92cb8ed5e787
                                     #xc71561a47e5e3631 #xaaf186621c880cf9
                                     #x6028f36ff08bd1b6 #x97296572f61e01dc
                                     #xc59abc397b0f8e6e #xec4d5e92b3894737
                                     #xb872fd63316b1d64 #xaf3bc1859a8c3de6
                                     #x0403a7270aa26dbe #x26942b9f5f8a9a78
                                     #x86ec232e520af4bf #x49ff11c4380d18ef
                                     #xf701961d0695d06a #x56b07e6c87b3e20c
                                     #xd5961aa553bd27ac #x61e113af7c2d8d17
                                     #x100ca69c28b2a9c2 #xcf1332ea6a07ec50
                                     #xc856a890e9526510 #x2b583f36cdd77106
                                     #x932ac255fcbc6c62 #x0b406e139dae3e9f
                                     #x832664c9d40ec5a0 #x3014f7b978cbe65b
                                     #x2c1da54c4e82f846 #x986aac46611252fd)
                                    (#x679cc74f352b557f #x376719b23e9424bf
                                     #xcc14a9925deb7e0d #xb07e6c87b3e20c56
                                     #xa17dbce183b3886a #xee12145e3d496b75
                                     #x406e139dae3e9f0b #x942b9f5f8a9a7826
                                     #xb24f568b845fd8a5 #xdf2643f85a072ec2
                                     #x8c7aba0ff3d5e106 #x0b63cf3a7ea3c9ef
                                     #x12a4f76c923c3a38 #x8bbfe91d3fdfeaf9
                                     #x9278d14bd340192e #xca47e78604311f05
                                     #x07c55312cc0a0bff #xcfb38e98ff86c009
                                     #x991b1e71ade3d0c1 #x16c68374fc5b8fc3
                                     #x39f0bf96bb80325c #x3d92cb8ed5e787a7
                                     #xac4d3dcfa4ca208d #xfae5ad26f6af3045
                                     #x63feb3575b4ce084 #x28f36ff08bd1b660
                                     #xc6e17baeb698dd15 #x84be523f2f1b96ed
                                     #x3c04d6884037ed50 #xce25939e6a56aafe
                                     #xa34c86edb40e5c99 #xebe67d40c6feb479
                                     #x27f2d4d29b15ca74 #x6d691573de58f667
                                     #x329370acc523fbb3 #x2c911be8e5b6039b
                                     #x871975358d7628e9 #x550fb7e3f008aecc
                                     #x7e5bff19d9b4a6a8 #xf8d4972ac112e4b6
                                     #xd1b1e5dcdf133821 #xfcb6e332af75514d
                                     #x1e026b442095f828 #x1f947642b54592df
                                     #x5e6c78d98eab6723 #x17509e72698be534
                                     #x2ac255fcbc6c6293 #x95bd82591f4a12d1
                                     #x799eac0b15bead57 #xf0107f1a1ddc935d
                                     #xd674b6ce131933de #xf5e41604e66b4c51
                                     #x8818ce179db254fd #x03a7270aa26dbe04
                                     #x1c33514817282cdb #x2f363ce247dbbd9f
                                     #xa72ef2f5da69e962 #x93eecc4d469073d9
                                     #xb92c99b1fafc114a #x77090a2f90aabbb4
                                     #x0ca69c28b2a9c210 #xc9e0c08ca65ca101
                                     #x4b0ddca7d09d56e4 #x988d03773833ba36
                                     #x06534e1459da6108 #x3a57989c19ed8c58
                                     #x0952f536491e1d1c #x0af5d23ceb73a318
                                     #x0d30812e2779a8e7 #xd7e2abc886c95929
                                     #xa51fc8f9edd43d91 #x690b616bb03f439c
                                     #x516dc3fb9e6f1b37 #xa489d5ff78045766
                                     #x52cae4f13c02a533 #x4cc88fb51c975d1b
                                     #x459a7a8355894007 #x9d796a69c384653a
                                     #x313457a6674e45b7 #x4a9bc1a1454d3c13
                                     #x6268ae51ce9c8a73 #xfe87d93e98c885be
                                     #xff11c4380d18ef49 #x8deca70966058bf1
                                     #xdeb05efecfd74435 #xd027f8da4ac352d6
                                     #xf186621c880cf9aa #x43c934970c53210f
                                     #xbee9caa336f61ab5 #x56a890e9526510c8
                                     #xe8415a4a64930a7d #xe32295701a30c392
                                     #x3e35ec84778a39a3 #x4f6fa8bfbefae31f
                                     #x5dcb5fd32cc6d927 #x9f485065f439b1c9
                                     #x1095cd60a581eecb #x978cb85528f7c622
                                     #x7baf9607220379a4 #xd216c2d67d7e8625
                                     #xe4e7c662d63ac86d #xb62d2293ea386d5e
                                     #x8a29f41baa0f800e #x5ffa65df1b7b0dd4
                                     #x61cf895b6cf13477 #xa6b8eff34fb98395
                                     #x814a3b21d4ac49e1 #xaddb20c9311a4a7a
                                     #x74ae2d2532c705b0 #x30a24aa0f29e2f40
                                     #x91dff641712da72a #x9049eb47e4fdcddd
                                     #x493ce6abe7208217 #x36f104b4ab444e48
                                     #xf22145162a6147ae #x5c5d42d5b916b3d0
                                     #xf7d52c08d1d698a2 #x7a398b01b7d31353
                                     #x6cff08754b889c90 #x14f7b978cbe65b30
                                     #xc4d041a2812509e6 #xe085b27ab85d7d96
                                     #xc0b235baef42bc1d #x868f683318a6421e
                                     #xea706046532ede8e #x4ef9b5b92b2a89e8
                                     #xdc8164f2f86a90c6 #x2455f3d839787470
                                     #x5499aae565d8c43b #x59a92bcb42a16cdc
                                     #xa9b954d15f7dff81 #xae7c07c39377f47e
                                     #x01961d0695d06af7 #xdb4437e034609b39
                                     #x3bc1859a8c3de6af #xaa1e73dbfd104185
                                     #x7dfcd8137bd918ac #x80dc2627417c2316
                                     #xd9750dec03dd4fca #xc5465ca414f56311
                                     #x203787c0571fc18b #xd5d391c4b1748dda
                                     #xc2830fb6d8ff68ee #xbcd8f0af014bce46
                                     #xa0eba1e71663e29d #xfb73b020637f5ab2
                                     #x7c6ac515ee09725b #x0000000000000000
                                     #xc876dd8a338ccbf6 #x9cef776f56540fcd
                                     #x47ab408f623494f4 #xcbd1fa8091e175f2
                                     #x9abc397b0f8e6ec5 #xb58a05994855d35a
                                     #x4d5e92b3894737ec #x961aa553bd27acd5
                                     #xc31512b04d2f0219 #xe6d6fc6ee1871c9e
                                     #xe2b488768fe0a965 #xb3d94b8d118fb252
                                     #x440c6785c0592af0 #x25c3eedeaca81e87
                                     #x583f36cdd771062b #x2d0706ee7066696c
                                     #x425f299199834bf8 #xfd20fe343aa53bba
                                     #xf643310e4406f255 #xdad22ae6a1b0f1ce
                                     #x1da54c4e82f8462c #x355623be0929f04c
                                     #x769f1729057ad143 #xbd4eeda9949ba4b1
                                     #xd8e310ea960d253d #x736b7e37fecd0e4f
                                     #x65adfd430296818c #xb8ba84b76f2c7bbd
                                     #x9b2a247d9a5e0432 #xc77766a82348b7e2
                                     #x08c4e830dcce77eb #x0e97a624851416e3
                                     #x898ed31108623e0a #xe571db6443eaa29a
                                     #x573e8defc7b57a3f #x21a19ac6c2cfab7c
                                     #x70cc593d5ca0b04b #x2664c9d40ec5a083
                                     #x296572f61e01dc97 #x85284f39bacbfc1a
                                     #x715a443bc970dabc #xef840958a8990182
                                     #xcd82b494c83b14fa #x48aafbad72f0e8e0
                                     #xe9d7474cf143608a #x2390a0caf5727f8f
                                     #xb7bb3f957fe807a9 #x82ed1c2b76c1f7e5
                                     #xbb1da3bdcd41c5b9 #x72fd63316b1d64b8
                                     #x7808b10d806ec7a0 #x837b012de3119d12
                                     #x689d7c6d25ef296b #x02313a0c37bdd4f3
                                     #x1103d0663051843c #xab886edd68c02b72
                                     #x6b3a5b678782976f #xe113af7c2d8d1761
                                     #x6aac46611252fd98 #x50fbdefd0bbf71c0
                                     #x2ea021e4d20bd768 #x5a0e0cc1e0ccd2d8
                                     #x34c03eb89cf99abb #xb41c189fdd85b9ad
                                     #x9ede4d6361e9db3e #xafea1ac506a79e89
                                     #x463d5d89f7e4fe03 #x18512550794f9920
                                     #x41f80e9b3beef5fc #xa82f49d7caad9576
                                     #x0f01bb2210c47c14 #xec232e520af4bf86
                                     #x1bf6025adb222724 #xa2da9beb21de366e
                                     #xedb533549f24d571 #x643be0459746eb7b
                                     #xbf7fd7a5a3267042 #x046274186e67b5fb
                                     #x8e4b8003c46835f5 #x1332ea6a07ec50cf
                                     #xd380dfd0e8aeecd2 #x6f582f7fe9e52294
                                     #xf9428a2c54c28e41 #x3fa3f182e25a5354
                                     #x535cf9f7a9d2cfc4 #x660ada49a0fb3f88
                                     #x33056daa50f39144 #x8fdd9d0551b85f02
                                     #x19c73856ec9ff3d7 #xb1e87181263266a1
                                     #x1561a47e5e3631c7 #xd4458cc224a4e72d
                                     #xe740e16874577669 #xc12428bc7a92d6ea
                                     #x3866a2902e5058ab #x1a601f5c4ef24dd3
                                     #x6059945df9215e80 #x05f4691efbb7df0c
                                     #x5b9811c7751cb82f #x2b5448fa29bc0864
                                     #xba8bbebb5891af4e #xf4720b0273bb26a6
                                     #xdd1779f46dbafa31 #x6ece32797c354863
                                     #x7fcde21f4c64cc5f #x2206bdcc60a21578
                                     #x75383023a7176f47 #xf3b75810bfb12d59)
                                    (#x03d0663051843c11 #xbfe91d3fdfeaf98b
                                     #xf80e9b3beef5fc41 #xe5ad26f6af3045fa
                                     #x5a443bc970dabc71 #x7b012de3119d1283
                                     #x82b494c83b14facd #x750dec03dd4fcad9
                                     #x090a2f90aabbb477 #xb6e332af75514dfc
                                     #xadfd430296818c65 #xfd63316b1d64b872
                                     #x3d5d89f7e4fe0346 #xd7474cf143608ae9
                                     #x7e6c87b3e20c56b0 #x601f5c4ef24dd31a
                                     #x40e16874577669e7 #x4437e034609b39db
                                     #xe7c662d63ac86de4 #xaf9607220379a47b
                                     #xea1ac506a79e89af #xd8f0af014bce46bc
                                     #x7fd7a5a3267042bf #x9f1729057ad14376
                                     #x1c189fdd85b9adb4 #x87d93e98c885befe
                                     #x57989c19ed8c583a #xa4f76c923c3a3812
                                     #x2a247d9a5e04329b #xc03eb89cf99abb34
                                     #xf6025adb2227241b #xa890e9526510c856
                                     #x06bdcc60a2157822 #xc73856ec9ff3d719
                                     #xcae4f13c02a53352 #xd6fc6ee1871c9ee6
                                     #xf0bf96bb80325c39 #x13af7c2d8d1761e1
                                     #x3be0459746eb7b64 #x99aae565d8c43b54
                                     #x95cd60a581eecb10 #x68ae51ce9c8a7362
                                     #xcde21f4c64cc5f7f #xdc2627417c231680
                                     #x428a2c54c28e41f9 #x76dd8a338ccbf6c8
                                     #xb8eff34fb98395a6 #xa69c28b2a9c2100c
                                     #x08b10d806ec7a078 #xc55312cc0a0bff07
                                     #x886edd68c02b72ab #xdd9d0551b85f028f
                                     #x1e73dbfd104185aa #x911be8e5b6039b2c
                                     #x30812e2779a8e70d #x3a5b678782976f6b
                                     #x20fe343aa53bbafd #xb954d15f7dff81a9
                                     #x9a7a835589400745 #x1fc8f9edd43d91a5
                                     #x0e0cc1e0ccd2d85a #xbb3f957fe807a9b7
                                     #xc3eedeaca81e8725 #x66a2902e5058ab38
                                     #xff08754b889c906c #xfeb3575b4ce08463
                                     #x107f1a1ddc935df0 #x25939e6a56aafece
                                     #xa92bcb42a16cdc59 #x32ea6a07ec50cf13
                                     #x947642b54592df1f #x1779f46dbafa31dd
                                     #x5623be0929f04c35 #xf2d4d29b15ca7427
                                     #x59945df9215e8060 #x9370acc523fbb332
                                     #xb05efecfd74435de #x71db6443eaa29ae5
                                     #xe2abc886c95929d7 #x458cc224a4e72dd4
                                     #xce32797c3548636e #x1aa553bd27acd596
                                     #x4a3b21d4ac49e181 #x284f39bacbfc1a85
                                     #xd94b8d118fb252b3 #xb235baef42bc1dc0
                                     #x2643f85a072ec2df #x8bbebb5891af4eba
                                     #x89d5ff78045766a4 #xeecc4d469073d993
                                     #x0b616bb03f439c69 #xe41604e66b4c51f5
                                     #x16c2d67d7e8625d2 #x6c78d98eab67235e
                                     #x9d7c6d25ef296b68 #x64c9d40ec5a08326
                                     #x2ef2f5da69e962a7 #xfa65df1b7b0dd45f
                                     #x12145e3d496b75ee #xfcd8137bd918ac7d
                                     #x52f536491e1d1c09 #xe67d40c6feb479eb
                                     #x2145162a6147aef2 #x29f41baa0f800e8a
                                     #x0000000000000000 #x840958a8990182ef
                                     #xc88fb51c975d1b4c #xc68374fc5b8fc316
                                     #x5d42d5b916b3d05c #x7dbce183b3886aa1
                                     #x512550794f992018 #xe17baeb698dd15c6
                                     #x43310e4406f255f6 #x6dc3fb9e6f1b3751
                                     #x86621c880cf9aaf1 #xbc397b0f8e6ec59a
                                     #x415a4a64930a7de8 #x04d6884037ed503c
                                     #xe9caa336f61ab5be #x0ada49a0fb3f8866
                                     #x55f3d83978747024 #x3ce6abe720821749
                                     #xf5d23ceb73a3180a #xa24aa0f29e2f4030
                                     #x582f7fe9e522946f #x7aba0ff3d5e1068c
                                     #x313a0c37bdd4f302 #x3787c0571fc18b20
                                     #x5cf9f7a9d2cfc453 #xbe523f2f1b96ed84
                                     #x85b27ab85d7d96e0 #x0706ee7066696c2d
                                     #x961d0695d06af701 #x1b1e71ade3d0c199
                                     #xc255fcbc6c62932a #x398b01b7d313537a
                                     #xcc593d5ca0b04b70 #x5f299199834bf842
                                     #x80dfd0e8aeecd2d3 #x9eac0b15bead5779
                                     #xef776f56540fcd9c #x2f49d7caad9576a8
                                     #x2c99b1fafc114ab9 #x8d03773833ba3698
                                     #x720b0273bb26a6f4 #x18ce179db254fd88
                                     #x8f683318a6421e86 #x4f568b845fd8a5b2
                                     #x8ed31108623e0a89 #xd22ae6a1b0f1ceda
                                     #x74b6ce131933ded6 #x97a624851416e30e
                                     #x6e139dae3e9f0b40 #xa7270aa26dbe0403
                                     #x5448fa29bc08642b #xe310ea960d253dd8
                                     #x706046532ede8eea #x485065f439b1c99f
                                     #x6b7e37fecd0e4f73 #xfbdefd0bbf71c050
                                     #xd391c4b1748ddad5 #xa021e4d20bd7682e
                                     #xab408f623494f447 #x5bff19d9b4a6a87e
                                     #xb1e5dcdf133821d1 #x026b442095f8281e
                                     #xdff641712da72a91 #x11c4380d18ef49ff
                                     #xae2d2532c705b074 #xc1859a8c3de6af3b
                                     #x4b8003c46835f58e #x92cb8ed5e787a73d
                                     #xcb5fd32cc6d9275d #x8cb85528f7c62297
                                     #x9bc1a1454d3c134a #x056daa50f3914433
                                     #xf4691efbb7df0c05 #xd1fa8091e175f2cb
                                     #x7c07c39377f47eae #x14a9925deb7e0dcc
                                     #xcf895b6cf1347761 #x0fb7e3f008aecc55
                                     #x8a05994855d35ab5 #xf104b4ab444e4836
                                     #x691573de58f6676d #x4eeda9949ba4b1bd
                                     #x2428bc7a92d6eac1 #xb75810bfb12d59f3
                                     #x63cf3a7ea3c9ef0b #x6274186e67b5fb04
                                     #x1512b04d2f0219c3 #xe87181263266a1b1
                                     #x1975358d7628e987 #x534e1459da610806
                                     #x47e78604311f05ca #xd4972ac112e4b6f8
                                     #x33514817282cdb1c #x90a0caf5727f8f23
                                     #x3e8defc7b57a3f57 #x3f36cdd771062b58
                                     #x796a69c384653a9d #x465ca414f56311c5
                                     #x5e92b3894737ec4d #x9811c7751cb82f5b
                                     #xd041a2812509e6c4 #x49eb47e4fdcddd90
                                     #x78d14bd340192e92 #xf9b5b92b2a89e84e
                                     #x61a47e5e3631c715 #x509e72698be53417
                                     #xb533549f24d571ed #x27f8da4ac352d6d0
                                     #x6572f61e01dc9729 #xde4d6361e9db3e9e
                                     #x3457a6674e45b731 #xa54c4e82f8462c1d
                                     #xbd82591f4a12d195 #x830fb6d8ff68eec2
                                     #x383023a7176f4775 #x7766a82348b7e2c7
                                     #x0c6785c0592af044 #xba84b76f2c7bbdb8
                                     #xe0c08ca65ca101c9 #xeba1e71663e29da0
                                     #xd52c08d1d698a2f7 #xc4e830dcce77eb08
                                     #xda9beb21de366ea2 #xa3f182e25a53543f
                                     #xac46611252fd986a #xb38e98ff86c009cf
                                     #xf36ff08bd1b66028 #xdb20c9311a4a7aad
                                     #xa19ac6c2cfab7c21 #x6ac515ee09725b7c
                                     #x4c86edb40e5c99a3 #x363ce247dbbd9f2f
                                     #x8164f2f86a90c6dc #x35ec84778a39a33e
                                     #xb488768fe0a965e2 #x73b020637f5ab2fb
                                     #x232e520af4bf86ec #x6fa8bfbefae31f4f
                                     #xeca70966058bf18d #x1da3bdcd41c5b9bb
                                     #x9cc74f352b557f67 #x4d3dcfa4ca208dac
                                     #x2b9f5f8a9a782694 #xaafbad72f0e8e048
                                     #xc934970c53210f43 #xed1c2b76c1f7e582
                                     #x01bb2210c47c140f #x0ddca7d09d56e44b
                                     #x2d2293ea386d5eb6 #xf7b978cbe65b3014
                                     #x6719b23e9424bf37 #x2295701a30c392e3)
                                    (#x9f5f8a9a7826942b #x34970c53210f43c9
                                     #x9d0551b85f028fdd #xb494c83b14facd82
                                     #x6edd68c02b72ab88 #xff19d9b4a6a87e5b
                                     #xdb6443eaa29ae571 #x1be8e5b6039b2c91
                                     #x5fd32cc6d9275dcb #x90e9526510c856a8
                                     #xb27ab85d7d96e085 #xa47e5e3631c71561
                                     #xf3d8397874702455 #xfbad72f0e8e048aa
                                     #x37e034609b39db44 #xfa8091e175f2cbd1
                                     #xcaa336f61ab5bee9 #xeff34fb98395a6b8
                                     #x63316b1d64b872fd #xba0ff3d5e1068c7a
                                     #xcd60a581eecb1095 #x35baef42bc1dc0b2
                                     #x23be0929f04c3556 #x4b8d118fb252b3d9
                                     #xd8137bd918ac7dfc #x0a2f90aabbb47709
                                     #xc2d67d7e8625d216 #x7181263266a1b1e8
                                     #x3023a7176f477538 #x7642b54592df1f94
                                     #xf2f5da69e962a72e #xa70966058bf18dec
                                     #xc662d63ac86de4e7 #x939e6a56aafece25
                                     #x86edb40e5c99a34c #xf8da4ac352d6d027
                                     #x145e3d496b75ee12 #xce179db254fd8818
                                     #xd23ceb73a3180af5 #x270aa26dbe0403a7
                                     #x5d89f7e4fe03463d #x2ae6a1b0f1cedad2
                                     #x1729057ad143769f #xb10d806ec7a07808
                                     #x4f39bacbfc1a8528 #xeda9949ba4b1bd4e
                                     #x5312cc0a0bff07c5 #xc4380d18ef49ff11
                                     #x95701a30c392e322 #xa0caf5727f8f2390
                                     #x55fcbc6c62932ac2 #xcf3a7ea3c9ef0b63
                                     #x621c880cf9aaf186 #xfd430296818c65ad
                                     #xbf96bb80325c39f0 #x6c87b3e20c56b07e
                                     #x299199834bf8425f #x74186e67b5fb0462
                                     #x4c4e82f8462c1da5 #x64f2f86a90c6dc81
                                     #x6b442095f8281e02 #x1d0695d06af70196
                                     #xd5ff78045766a489 #x3856ec9ff3d719c7
                                     #xac0b15bead57799e #xc3fb9e6f1b37516d
                                     #x8e98ff86c009cfb3 #x49d7caad9576a82f
                                     #x859a8c3de6af3bc1 #x3dcfa4ca208dac4d
                                     #x91c4b1748ddad5d3 #x8a2c54c28e41f942
                                     #x2bcb42a16cdc59a9 #x9e72698be5341750
                                     #x1e71ade3d0c1991b #xe6abe7208217493c
                                     #x3a0c37bdd4f30231 #x945df9215e806059
                                     #xcc4d469073d993ee #x0e9b3beef5fc41f8
                                     #x8b01b7d313537a39 #xf182e25a53543fa3
                                     #x5810bfb12d59f3b7 #x8003c46835f58e4b
                                     #xbce183b3886aa17d #x1604e66b4c51f5e4
                                     #x2c08d1d698a2f7d5 #xc08ca65ca101c9e0
                                     #x7a8355894007459a #xe21f4c64cc5f7fcd
                                     #x88768fe0a965e2b4 #x82591f4a12d195bd
                                     #x4aa0f29e2f4030a2 #xdca7d09d56e44b0d
                                     #x6daa50f391443305 #x92b3894737ec4d5e
                                     #xa8bfbefae31f4f6f #xea6a07ec50cf1332
                                     #x2293ea386d5eb62d #x41a2812509e6c4d0
                                     #x8374fc5b8fc316c6 #x683318a6421e868f
                                     #xb3575b4ce08463fe #xe5dcdf133821d1b1
                                     #xd6884037ed503c04 #x05994855d35ab58a
                                     #x5b678782976f6b3a #x397b0f8e6ec59abc
                                     #xabc886c95929d7e2 #xe4f13c02a53352ca
                                     #xb5b92b2a89e84ef9 #xaf7c2d8d1761e113
                                     #x75358d7628e98719 #x72f61e01dc972965
                                     #x78d98eab67235e6c #xc8f9edd43d91a51f
                                     #x0fb6d8ff68eec283 #x70acc523fbb33293
                                     #x36cdd771062b583f #xbdcc60a215782206
                                     #xa624851416e30e97 #x9607220379a47baf
                                     #x1c2b76c1f7e582ed #x6046532ede8eea70
                                     #xbebb5891af4eba8b #x48fa29bc08642b54
                                     #x19b23e9424bf3767 #x2627417c231680dc
                                     #x65df1b7b0dd45ffa #xf536491e1d1c0952
                                     #x54d15f7dff81a9b9 #x2550794f99201851
                                     #xdefd0bbf71c050fb #x87c0571fc18b2037
                                     #x10ea960d253dd8e3 #xe0459746eb7b643b
                                     #x12b04d2f0219c315 #xaae565d8c43b5499
                                     #xe91d3fdfeaf98bbf #xf76c923c3a3812a4
                                     #x1f5c4ef24dd31a60 #x812e2779a8e70d30
                                     #xf41baa0f800e8a29 #xfc6ee1871c9ee6d6
                                     #x8fb51c975d1b4cc8 #x4e1459da61080653
                                     #x310e4406f255f643 #x0cc1e0ccd2d85a0e
                                     #x0958a8990182ef84 #xa9925deb7e0dcc14
                                     #x6a69c384653a9d79 #x04b4ab444e4836f1
                                     #x0dec03dd4fcad975 #xeb47e4fdcddd9049
                                     #x7e37fecd0e4f736b #x73dbfd104185aa1e
                                     #xc515ee09725b7c6a #xae51ce9c8a736268
                                     #xa3bdcd41c5b9bb1d #x84b76f2c7bbdb8ba
                                     #x443bc970dabc715a #xa1e71663e29da0eb
                                     #x7f1a1ddc935df010 #x474cf143608ae9d7
                                     #x33549f24d571edb5 #xc9d40ec5a0832664
                                     #x46611252fd986aac #x5efecfd74435deb0
                                     #x0000000000000000 #x514817282cdb1c33
                                     #x408f623494f447ab #x57a6674e45b73134
                                     #x11c7751cb82f5b98 #x989c19ed8c583a57
                                     #x7baeb698dd15c6e1 #x06ee7066696c2d07
                                     #xda49a0fb3f88660a #x07c39377f47eae7c
                                     #x2d2532c705b074ae #x42d5b916b3d05c5d
                                     #xec84778a39a33e35 #x776f56540fcd9cef
                                     #xa553bd27acd5961a #xb978cbe65b3014f7
                                     #x895b6cf1347761cf #xb85528f7c622978c
                                     #x66a82348b7e2c777 #xe830dcce77eb08c4
                                     #x189fdd85b9adb41c #x99b1fafc114ab92c
                                     #x0b0273bb26a6f472 #xeedeaca81e8725c3
                                     #x6785c0592af0440c #x247d9a5e04329b2a
                                     #x21e4d20bd7682ea0 #xbb2210c47c140f01
                                     #x523f2f1b96ed84be #xd4d29b15ca7427f2
                                     #x45162a6147aef221 #xf641712da72a91df
                                     #xb020637f5ab2fb73 #xdd8a338ccbf6c876
                                     #x5ca414f56311c546 #x3f957fe807a9b7bb
                                     #x8cc224a4e72dd445 #x012de3119d12837b
                                     #xd0663051843c1103 #x5a4a64930a7de841
                                     #x28bc7a92d6eac124 #x08754b889c906cff
                                     #xd7a5a3267042bf7f #x3ce247dbbd9f2f36
                                     #x6ff08bd1b66028f3 #xd14bd340192e9278
                                     #x139dae3e9f0b406e #xe78604311f05ca47
                                     #xd93e98c885befe87 #x616bb03f439c690b
                                     #x03773833ba36988d #xcb8ed5e787a73d92
                                     #xb7e3f008aecc550f #x9c28b2a9c2100ca6
                                     #x3eb89cf99abb34c0 #x3b21d4ac49e1814a
                                     #xdfd0e8aeecd2d380 #x7c6d25ef296b689d
                                     #xc1a1454d3c134a9b #xf9f7a9d2cfc4535c
                                     #xc74f352b557f679c #x7d40c6feb479ebe6
                                     #x1573de58f6676d69 #x1ac506a79e89afea
                                     #x568b845fd8a5b24f #x32797c3548636ece
                                     #x691efbb7df0c05f4 #x972ac112e4b6f8d4
                                     #x79f46dbafa31dd17 #x2e520af4bf86ec23
                                     #xd31108623e0a898e #x8defc7b57a3f573e
                                     #x025adb2227241bf6 #xa2902e5058ab3866
                                     #xfe343aa53bbafd20 #xad26f6af3045fae5
                                     #x43f85a072ec2df26 #x2f7fe9e522946f58
                                     #x9beb21de366ea2da #x20c9311a4a7aaddb
                                     #x5065f439b1c99f48 #x593d5ca0b04b70cc
                                     #x9ac6c2cfab7c21a1 #xb6ce131933ded674
                                     #xf0af014bce46bcd8 #x4d6361e9db3e9ede
                                     #xe16874577669e740 #xe332af75514dfcb6)
                                    (#xbfbefae31f4f6fa8 #x85c0592af0440c67
                                     #xbaef42bc1dc0b235 #x5528f7c622978cb8
                                     #xe6a1b0f1cedad22a #x3ceb73a3180af5d2
                                     #x7fe9e522946f582f #xa7d09d56e44b0ddc
                                     #x754b889c906cff08 #xe247dbbd9f2f363c
                                     #x81263266a1b1e871 #xdcdf133821d1b1e5
                                     #xf46dbafa31dd1779 #x3d5ca0b04b70cc59
                                     #x56ec9ff3d719c738 #xdeaca81e8725c3ee
                                     #x549f24d571edb533 #x2de3119d12837b01
                                     #xea960d253dd8e310 #xc886c95929d7e2ab
                                     #x2f90aabbb477090a #x26f6af3045fae5ad
                                     #x6ee1871c9ee6d6fc #x3e98c885befe87d9
                                     #x24851416e30e97a6 #xda4ac352d6d027f8
                                     #xe3f008aecc550fb7 #x994855d35ab58a05
                                     #x5df9215e80605994 #x58a8990182ef8409
                                     #xb1fafc114ab92c99 #x442095f8281e026b
                                     #xf5da69e962a72ef2 #xc0571fc18b203787
                                     #x37fecd0e4f736b7e #x902e5058ab3866a2
                                     #xeb21de366ea2da9b #x8f623494f447ab40
                                     #xd98eab67235e6c78 #xa414f56311c5465c
                                     #x5c4ef24dd31a601f #x08d1d698a2f7d52c
                                     #xdbfd104185aa1e73 #xcb42a16cdc59a92b
                                     #x10bfb12d59f3b758 #xa82348b7e2c77766
                                     #xbdcd41c5b9bb1da3 #x7ab85d7d96e085b2
                                     #xedb40e5c99a34c86 #xff78045766a489d5
                                     #xf34fb98395a6b8ef #x4f352b557f679cc7
                                     #x4a64930a7de8415a #xe8e5b6039b2c911b
                                     #xb4ab444e4836f104 #x4817282cdb1c3351
                                     #xaeb698dd15c6e17b #x3f2f1b96ed84be52
                                     #x21d4ac49e1814a3b #x884037ed503c04d6
                                     #xbe0929f04c355623 #xe565d8c43b5499aa
                                     #x1108623e0a898ed3 #x2532c705b074ae2d
                                     #x0551b85f028fdd9d #x03c46835f58e4b80
                                     #xf13c02a53352cae4 #xee7066696c2d0706
                                     #xc6c2cfab7c21a19a #xb04d2f0219c31512
                                     #x0273bb26a6f4720b #x8604311f05ca47e7
                                     #xaf014bce46bcd8f0 #x3318a6421e868f68
                                     #x96bb80325c39f0bf #x40c6feb479ebe67d
                                     #x12cc0a0bff07c553 #x162a6147aef22145
                                     #x71ade3d0c1991b1e #xce131933ded674b6
                                     #xc9311a4a7aaddb20 #x32af75514dfcb6e3
                                     #x62d63ac86de4e7c6 #xb23e9424bf376719
                                     #x8355894007459a7a #x137bd918ac7dfcd8
                                     #x42b54592df1f9476 #xabe7208217493ce6
                                     #x9b3beef5fc41f80e #x2e2779a8e70d3081
                                     #x4d469073d993eecc #x768fe0a965e2b488
                                     #xd0e8aeecd2d380df #xa2812509e6c4d041
                                     #x0ff3d5e1068c7aba #x04e66b4c51f5e416
                                     #x2c54c28e41f9428a #x01b7d313537a398b
                                     #xcc60a215782206bd #xc506a79e89afea1a
                                     #x4cf143608ae9d747 #x8a338ccbf6c876dd
                                     #x29057ad143769f17 #xcfa4ca208dac4d3d
                                     #xa6674e45b7313457 #x50794f9920185125
                                     #xb89cf99abb34c03e #x343aa53bbafd20fe
                                     #x89f7e4fe03463d5d #xa5a3267042bf7fd7
                                     #xefc7b57a3f573e8d #x9fdd85b9adb41c18
                                     #xec03dd4fcad9750d #x970c53210f43c934
                                     #xc7751cb82f5b9811 #xc1e0ccd2d85a0e0c
                                     #xad72f0e8e048aafb #x0966058bf18deca7
                                     #x47e4fdcddd9049eb #x19d9b4a6a87e5bff
                                     #x1a1ddc935df0107f #x591f4a12d195bd82
                                     #x1efbb7df0c05f469 #x575b4ce08463feb3
                                     #x0e4406f255f64331 #x2ac112e4b6f8d497
                                     #xa0f29e2f4030a24a #x179db254fd8818ce
                                     #x663051843c1103d0 #xe4d20bd7682ea021
                                     #xd15f7dff81a9b954 #xd32cc6d9275dcb5f
                                     #x94c83b14facd82b4 #xaa50f3914433056d
                                     #x358d7628e9871975 #xbc7a92d6eac12428
                                     #x39bacbfc1a85284f #x9199834bf8425f29
                                     #x73de58f6676d6915 #xd5b916b3d05c5d42
                                     #x46532ede8eea7060 #x186e67b5fb046274
                                     #xa1454d3c134a9bc1 #x0b15bead57799eac
                                     #x5adb2227241bf602 #xdd68c02b72ab886e
                                     #x6443eaa29ae571db #xfd0bbf71c050fbde
                                     #x8b845fd8a5b24f56 #xb92b2a89e84ef9b5
                                     #x678782976f6b3a5b #xf2f86a90c6dc8164
                                     #xc39377f47eae7c07 #x93ea386d5eb62d22
                                     #x430296818c65adfd #xc224a4e72dd4458c
                                     #x9e6a56aafece2593 #x6f56540fcd9cef77
                                     #x84778a39a33e35ec #xf85a072ec2df2643
                                     #x3bc970dabc715a44 #x0d806ec7a07808b1
                                     #x773833ba36988d03 #x27417c231680dc26
                                     #x60a581eecb1095cd #xb76f2c7bbdb8ba84
                                     #xd8397874702455f3 #xfecfd74435deb05e
                                     #xcaf5727f8f2390a0 #xa9949ba4b1bd4eed
                                     #xe034609b39db4437 #x2b76c1f7e582ed1c
                                     #xf7a9d2cfc4535cf9 #x20637f5ab2fb73b0
                                     #x9c19ed8c583a5798 #xd67d7e8625d216c2
                                     #x1459da610806534e #x6d25ef296b689d7c
                                     #x925deb7e0dcc14a9 #x65f439b1c99f4850
                                     #x957fe807a9b7bb3f #x41712da72a91dff6
                                     #x36491e1d1c0952f5 #x5e3d496b75ee1214
                                     #x1d3fdfeaf98bbfe9 #x701a30c392e32295
                                     #x6361e9db3e9ede4d #xd7caad9576a82f49
                                     #xe71663e29da0eba1 #x98ff86c009cfb38e
                                     #x6bb03f439c690b61 #xc4b1748ddad5d391
                                     #x6c923c3a3812a4f7 #x1f4c64cc5f7fcde2
                                     #xe183b3886aa17dbc #xb6d8ff68eec2830f
                                     #x51ce9c8a736268ae #x07220379a47baf96
                                     #x82e25a53543fa3f1 #xbb5891af4eba8bbe
                                     #x4bd340192e9278d1 #x6874577669e740e1
                                     #xfb9e6f1b37516dc3 #xacc523fbb3329370
                                     #x69c384653a9d796a #x5b6cf1347761cf89
                                     #x0000000000000000 #x49a0fb3f88660ada
                                     #xa336f61ab5bee9ca #x15ee09725b7c6ac5
                                     #x8d118fb252b3d94b #x7d9a5e04329b2a24
                                     #x1baa0f800e8a29f4 #xfa29bc08642b5448
                                     #x459746eb7b643be0 #xdf1b7b0dd45ffa65
                                     #x30dcce77eb08c4e8 #x8091e175f2cbd1fa
                                     #x0c37bdd4f302313a #xf9edd43d91a51fc8
                                     #x3a7ea3c9ef0b63cf #x74fc5b8fc316c683
                                     #x8ca65ca101c9e0c0 #x72698be53417509e
                                     #x23a7176f47753830 #x797c3548636ece32
                                     #xb51c975d1b4cc88f #x2210c47c140f01bb
                                     #xd29b15ca7427f2d4 #x8ed5e787a73d92cb
                                     #x7e5e3631c71561a4 #x1c880cf9aaf18662
                                     #xf08bd1b66028f36f #xf61e01dc97296572
                                     #x7b0f8e6ec59abc39 #xb3894737ec4d5e92
                                     #x316b1d64b872fd63 #x9a8c3de6af3bc185
                                     #x0aa26dbe0403a727 #x5f8a9a7826942b9f
                                     #x520af4bf86ec232e #x380d18ef49ff11c4
                                     #x0695d06af701961d #x87b3e20c56b07e6c
                                     #x53bd27acd5961aa5 #x7c2d8d1761e113af
                                     #x28b2a9c2100ca69c #x6a07ec50cf1332ea
                                     #xe9526510c856a890 #xcdd771062b583f36
                                     #xfcbc6c62932ac255 #x9dae3e9f0b406e13
                                     #xd40ec5a0832664c9 #x78cbe65b3014f7b9
                                     #x4e82f8462c1da54c #x611252fd986aac46)
                                    (#x352b557f679cc74f #x3e9424bf376719b2
                                     #x5deb7e0dcc14a992 #xb3e20c56b07e6c87
                                     #x83b3886aa17dbce1 #x3d496b75ee12145e
                                     #xae3e9f0b406e139d #x8a9a7826942b9f5f
                                     #x845fd8a5b24f568b #x5a072ec2df2643f8
                                     #xf3d5e1068c7aba0f #x7ea3c9ef0b63cf3a
                                     #x923c3a3812a4f76c #x3fdfeaf98bbfe91d
                                     #xd340192e9278d14b #x04311f05ca47e786
                                     #xcc0a0bff07c55312 #xff86c009cfb38e98
                                     #xade3d0c1991b1e71 #xfc5b8fc316c68374
                                     #xbb80325c39f0bf96 #xd5e787a73d92cb8e
                                     #xa4ca208dac4d3dcf #xf6af3045fae5ad26
                                     #x5b4ce08463feb357 #x8bd1b66028f36ff0
                                     #xb698dd15c6e17bae #x2f1b96ed84be523f
                                     #x4037ed503c04d688 #x6a56aafece25939e
                                     #xb40e5c99a34c86ed #xc6feb479ebe67d40
                                     #x9b15ca7427f2d4d2 #xde58f6676d691573
                                     #xc523fbb3329370ac #xe5b6039b2c911be8
                                     #x8d7628e987197535 #xf008aecc550fb7e3
                                     #xd9b4a6a87e5bff19 #xc112e4b6f8d4972a
                                     #xdf133821d1b1e5dc #xaf75514dfcb6e332
                                     #x2095f8281e026b44 #xb54592df1f947642
                                     #x8eab67235e6c78d9 #x698be53417509e72
                                     #xbc6c62932ac255fc #x1f4a12d195bd8259
                                     #x15bead57799eac0b #x1ddc935df0107f1a
                                     #x131933ded674b6ce #xe66b4c51f5e41604
                                     #x9db254fd8818ce17 #xa26dbe0403a7270a
                                     #x17282cdb1c335148 #x47dbbd9f2f363ce2
                                     #xda69e962a72ef2f5 #x469073d993eecc4d
                                     #xfafc114ab92c99b1 #x90aabbb477090a2f
                                     #xb2a9c2100ca69c28 #xa65ca101c9e0c08c
                                     #xd09d56e44b0ddca7 #x3833ba36988d0377
                                     #x59da610806534e14 #x19ed8c583a57989c
                                     #x491e1d1c0952f536 #xeb73a3180af5d23c
                                     #x2779a8e70d30812e #x86c95929d7e2abc8
                                     #xedd43d91a51fc8f9 #xb03f439c690b616b
                                     #x9e6f1b37516dc3fb #x78045766a489d5ff
                                     #x3c02a53352cae4f1 #x1c975d1b4cc88fb5
                                     #x55894007459a7a83 #xc384653a9d796a69
                                     #x674e45b7313457a6 #x454d3c134a9bc1a1
                                     #xce9c8a736268ae51 #x98c885befe87d93e
                                     #x0d18ef49ff11c438 #x66058bf18deca709
                                     #xcfd74435deb05efe #x4ac352d6d027f8da
                                     #x880cf9aaf186621c #x0c53210f43c93497
                                     #x36f61ab5bee9caa3 #x526510c856a890e9
                                     #x64930a7de8415a4a #x1a30c392e3229570
                                     #x778a39a33e35ec84 #xbefae31f4f6fa8bf
                                     #x2cc6d9275dcb5fd3 #xf439b1c99f485065
                                     #xa581eecb1095cd60 #x28f7c622978cb855
                                     #x220379a47baf9607 #x7d7e8625d216c2d6
                                     #xd63ac86de4e7c662 #xea386d5eb62d2293
                                     #xaa0f800e8a29f41b #x1b7b0dd45ffa65df
                                     #x6cf1347761cf895b #x4fb98395a6b8eff3
                                     #xd4ac49e1814a3b21 #x311a4a7aaddb20c9
                                     #x32c705b074ae2d25 #xf29e2f4030a24aa0
                                     #x712da72a91dff641 #xe4fdcddd9049eb47
                                     #xe7208217493ce6ab #xab444e4836f104b4
                                     #x2a6147aef2214516 #xb916b3d05c5d42d5
                                     #xd1d698a2f7d52c08 #xb7d313537a398b01
                                     #x4b889c906cff0875 #xcbe65b3014f7b978
                                     #x812509e6c4d041a2 #xb85d7d96e085b27a
                                     #xef42bc1dc0b235ba #x18a6421e868f6833
                                     #x532ede8eea706046 #x2b2a89e84ef9b5b9
                                     #xf86a90c6dc8164f2 #x397874702455f3d8
                                     #x65d8c43b5499aae5 #x42a16cdc59a92bcb
                                     #x5f7dff81a9b954d1 #x9377f47eae7c07c3
                                     #x95d06af701961d06 #x34609b39db4437e0
                                     #x8c3de6af3bc1859a #xfd104185aa1e73db
                                     #x7bd918ac7dfcd813 #x417c231680dc2627
                                     #x03dd4fcad9750dec #x14f56311c5465ca4
                                     #x571fc18b203787c0 #xb1748ddad5d391c4
                                     #xd8ff68eec2830fb6 #x014bce46bcd8f0af
                                     #x1663e29da0eba1e7 #x637f5ab2fb73b020
                                     #xee09725b7c6ac515 #x0000000000000000
                                     #x338ccbf6c876dd8a #x56540fcd9cef776f
                                     #x623494f447ab408f #x91e175f2cbd1fa80
                                     #x0f8e6ec59abc397b #x4855d35ab58a0599
                                     #x894737ec4d5e92b3 #xbd27acd5961aa553
                                     #x4d2f0219c31512b0 #xe1871c9ee6d6fc6e
                                     #x8fe0a965e2b48876 #x118fb252b3d94b8d
                                     #xc0592af0440c6785 #xaca81e8725c3eede
                                     #xd771062b583f36cd #x7066696c2d0706ee
                                     #x99834bf8425f2991 #x3aa53bbafd20fe34
                                     #x4406f255f643310e #xa1b0f1cedad22ae6
                                     #x82f8462c1da54c4e #x0929f04c355623be
                                     #x057ad143769f1729 #x949ba4b1bd4eeda9
                                     #x960d253dd8e310ea #xfecd0e4f736b7e37
                                     #x0296818c65adfd43 #x6f2c7bbdb8ba84b7
                                     #x9a5e04329b2a247d #x2348b7e2c77766a8
                                     #xdcce77eb08c4e830 #x851416e30e97a624
                                     #x08623e0a898ed311 #x43eaa29ae571db64
                                     #xc7b57a3f573e8def #xc2cfab7c21a19ac6
                                     #x5ca0b04b70cc593d #x0ec5a0832664c9d4
                                     #x1e01dc97296572f6 #xbacbfc1a85284f39
                                     #xc970dabc715a443b #xa8990182ef840958
                                     #xc83b14facd82b494 #x72f0e8e048aafbad
                                     #xf143608ae9d7474c #xf5727f8f2390a0ca
                                     #x7fe807a9b7bb3f95 #x76c1f7e582ed1c2b
                                     #xcd41c5b9bb1da3bd #x6b1d64b872fd6331
                                     #x806ec7a07808b10d #xe3119d12837b012d
                                     #x25ef296b689d7c6d #x37bdd4f302313a0c
                                     #x3051843c1103d066 #x68c02b72ab886edd
                                     #x8782976f6b3a5b67 #x2d8d1761e113af7c
                                     #x1252fd986aac4661 #x0bbf71c050fbdefd
                                     #xd20bd7682ea021e4 #xe0ccd2d85a0e0cc1
                                     #x9cf99abb34c03eb8 #xdd85b9adb41c189f
                                     #x61e9db3e9ede4d63 #x06a79e89afea1ac5
                                     #xf7e4fe03463d5d89 #x794f992018512550
                                     #x3beef5fc41f80e9b #xcaad9576a82f49d7
                                     #x10c47c140f01bb22 #x0af4bf86ec232e52
                                     #xdb2227241bf6025a #x21de366ea2da9beb
                                     #x9f24d571edb53354 #x9746eb7b643be045
                                     #xa3267042bf7fd7a5 #x6e67b5fb04627418
                                     #xc46835f58e4b8003 #x07ec50cf1332ea6a
                                     #xe8aeecd2d380dfd0 #xe9e522946f582f7f
                                     #x54c28e41f9428a2c #xe25a53543fa3f182
                                     #xa9d2cfc4535cf9f7 #xa0fb3f88660ada49
                                     #x50f3914433056daa #x51b85f028fdd9d05
                                     #xec9ff3d719c73856 #x263266a1b1e87181
                                     #x5e3631c71561a47e #x24a4e72dd4458cc2
                                     #x74577669e740e168 #x7a92d6eac12428bc
                                     #x2e5058ab3866a290 #x4ef24dd31a601f5c
                                     #xf9215e806059945d #xfbb7df0c05f4691e
                                     #x751cb82f5b9811c7 #x29bc08642b5448fa
                                     #x5891af4eba8bbebb #x73bb26a6f4720b02
                                     #x6dbafa31dd1779f4 #x7c3548636ece3279
                                     #x4c64cc5f7fcde21f #x60a215782206bdcc
                                     #xa7176f4775383023 #xbfb12d59f3b75810)
                                    (#x51843c1103d06630 #xdfeaf98bbfe91d3f
                                     #xeef5fc41f80e9b3b #xaf3045fae5ad26f6
                                     #x70dabc715a443bc9 #x119d12837b012de3
                                     #x3b14facd82b494c8 #xdd4fcad9750dec03
                                     #xaabbb477090a2f90 #x75514dfcb6e332af
                                     #x96818c65adfd4302 #x1d64b872fd63316b
                                     #xe4fe03463d5d89f7 #x43608ae9d7474cf1
                                     #xe20c56b07e6c87b3 #xf24dd31a601f5c4e
                                     #x577669e740e16874 #x609b39db4437e034
                                     #x3ac86de4e7c662d6 #x0379a47baf960722
                                     #xa79e89afea1ac506 #x4bce46bcd8f0af01
                                     #x267042bf7fd7a5a3 #x7ad143769f172905
                                     #x85b9adb41c189fdd #xc885befe87d93e98
                                     #xed8c583a57989c19 #x3c3a3812a4f76c92
                                     #x5e04329b2a247d9a #xf99abb34c03eb89c
                                     #x2227241bf6025adb #x6510c856a890e952
                                     #xa215782206bdcc60 #x9ff3d719c73856ec
                                     #x02a53352cae4f13c #x871c9ee6d6fc6ee1
                                     #x80325c39f0bf96bb #x8d1761e113af7c2d
                                     #x46eb7b643be04597 #xd8c43b5499aae565
                                     #x81eecb1095cd60a5 #x9c8a736268ae51ce
                                     #x64cc5f7fcde21f4c #x7c231680dc262741
                                     #xc28e41f9428a2c54 #x8ccbf6c876dd8a33
                                     #xb98395a6b8eff34f #xa9c2100ca69c28b2
                                     #x6ec7a07808b10d80 #x0a0bff07c55312cc
                                     #xc02b72ab886edd68 #xb85f028fdd9d0551
                                     #x104185aa1e73dbfd #xb6039b2c911be8e5
                                     #x79a8e70d30812e27 #x82976f6b3a5b6787
                                     #xa53bbafd20fe343a #x7dff81a9b954d15f
                                     #x894007459a7a8355 #xd43d91a51fc8f9ed
                                     #xccd2d85a0e0cc1e0 #xe807a9b7bb3f957f
                                     #xa81e8725c3eedeac #x5058ab3866a2902e
                                     #x889c906cff08754b #x4ce08463feb3575b
                                     #xdc935df0107f1a1d #x56aafece25939e6a
                                     #xa16cdc59a92bcb42 #xec50cf1332ea6a07
                                     #x4592df1f947642b5 #xbafa31dd1779f46d
                                     #x29f04c355623be09 #x15ca7427f2d4d29b
                                     #x215e806059945df9 #x23fbb3329370acc5
                                     #xd74435deb05efecf #xeaa29ae571db6443
                                     #xc95929d7e2abc886 #xa4e72dd4458cc224
                                     #x3548636ece32797c #x27acd5961aa553bd
                                     #xac49e1814a3b21d4 #xcbfc1a85284f39ba
                                     #x8fb252b3d94b8d11 #x42bc1dc0b235baef
                                     #x072ec2df2643f85a #x91af4eba8bbebb58
                                     #x045766a489d5ff78 #x9073d993eecc4d46
                                     #x3f439c690b616bb0 #x6b4c51f5e41604e6
                                     #x7e8625d216c2d67d #xab67235e6c78d98e
                                     #xef296b689d7c6d25 #xc5a0832664c9d40e
                                     #x69e962a72ef2f5da #x7b0dd45ffa65df1b
                                     #x496b75ee12145e3d #xd918ac7dfcd8137b
                                     #x1e1d1c0952f53649 #xfeb479ebe67d40c6
                                     #x6147aef22145162a #x0f800e8a29f41baa
                                     #x0000000000000000 #x990182ef840958a8
                                     #x975d1b4cc88fb51c #x5b8fc316c68374fc
                                     #x16b3d05c5d42d5b9 #xb3886aa17dbce183
                                     #x4f99201851255079 #x98dd15c6e17baeb6
                                     #x06f255f643310e44 #x6f1b37516dc3fb9e
                                     #x0cf9aaf186621c88 #x8e6ec59abc397b0f
                                     #x930a7de8415a4a64 #x37ed503c04d68840
                                     #xf61ab5bee9caa336 #xfb3f88660ada49a0
                                     #x7874702455f3d839 #x208217493ce6abe7
                                     #x73a3180af5d23ceb #x9e2f4030a24aa0f2
                                     #xe522946f582f7fe9 #xd5e1068c7aba0ff3
                                     #xbdd4f302313a0c37 #x1fc18b203787c057
                                     #xd2cfc4535cf9f7a9 #x1b96ed84be523f2f
                                     #x5d7d96e085b27ab8 #x66696c2d0706ee70
                                     #xd06af701961d0695 #xe3d0c1991b1e71ad
                                     #x6c62932ac255fcbc #xd313537a398b01b7
                                     #xa0b04b70cc593d5c #x834bf8425f299199
                                     #xaeecd2d380dfd0e8 #xbead57799eac0b15
                                     #x540fcd9cef776f56 #xad9576a82f49d7ca
                                     #xfc114ab92c99b1fa #x33ba36988d037738
                                     #xbb26a6f4720b0273 #xb254fd8818ce179d
                                     #xa6421e868f683318 #x5fd8a5b24f568b84
                                     #x623e0a898ed31108 #xb0f1cedad22ae6a1
                                     #x1933ded674b6ce13 #x1416e30e97a62485
                                     #x3e9f0b406e139dae #x6dbe0403a7270aa2
                                     #xbc08642b5448fa29 #x0d253dd8e310ea96
                                     #x2ede8eea70604653 #x39b1c99f485065f4
                                     #xcd0e4f736b7e37fe #xbf71c050fbdefd0b
                                     #x748ddad5d391c4b1 #x0bd7682ea021e4d2
                                     #x3494f447ab408f62 #xb4a6a87e5bff19d9
                                     #x133821d1b1e5dcdf #x95f8281e026b4420
                                     #x2da72a91dff64171 #x18ef49ff11c4380d
                                     #xc705b074ae2d2532 #x3de6af3bc1859a8c
                                     #x6835f58e4b8003c4 #xe787a73d92cb8ed5
                                     #xc6d9275dcb5fd32c #xf7c622978cb85528
                                     #x4d3c134a9bc1a145 #xf3914433056daa50
                                     #xb7df0c05f4691efb #xe175f2cbd1fa8091
                                     #x77f47eae7c07c393 #xeb7e0dcc14a9925d
                                     #xf1347761cf895b6c #x08aecc550fb7e3f0
                                     #x55d35ab58a059948 #x444e4836f104b4ab
                                     #x58f6676d691573de #x9ba4b1bd4eeda994
                                     #x92d6eac12428bc7a #xb12d59f3b75810bf
                                     #xa3c9ef0b63cf3a7e #x67b5fb046274186e
                                     #x2f0219c31512b04d #x3266a1b1e8718126
                                     #x7628e9871975358d #xda610806534e1459
                                     #x311f05ca47e78604 #x12e4b6f8d4972ac1
                                     #x282cdb1c33514817 #x727f8f2390a0caf5
                                     #xb57a3f573e8defc7 #x71062b583f36cdd7
                                     #x84653a9d796a69c3 #xf56311c5465ca414
                                     #x4737ec4d5e92b389 #x1cb82f5b9811c775
                                     #x2509e6c4d041a281 #xfdcddd9049eb47e4
                                     #x40192e9278d14bd3 #x2a89e84ef9b5b92b
                                     #x3631c71561a47e5e #x8be53417509e7269
                                     #x24d571edb533549f #xc352d6d027f8da4a
                                     #x01dc97296572f61e #xe9db3e9ede4d6361
                                     #x4e45b7313457a667 #xf8462c1da54c4e82
                                     #x4a12d195bd82591f #xff68eec2830fb6d8
                                     #x176f4775383023a7 #x48b7e2c77766a823
                                     #x592af0440c6785c0 #x2c7bbdb8ba84b76f
                                     #x5ca101c9e0c08ca6 #x63e29da0eba1e716
                                     #xd698a2f7d52c08d1 #xce77eb08c4e830dc
                                     #xde366ea2da9beb21 #x5a53543fa3f182e2
                                     #x52fd986aac466112 #x86c009cfb38e98ff
                                     #xd1b66028f36ff08b #x1a4a7aaddb20c931
                                     #xcfab7c21a19ac6c2 #x09725b7c6ac515ee
                                     #x0e5c99a34c86edb4 #xdbbd9f2f363ce247
                                     #x6a90c6dc8164f2f8 #x8a39a33e35ec8477
                                     #xe0a965e2b488768f #x7f5ab2fb73b02063
                                     #xf4bf86ec232e520a #xfae31f4f6fa8bfbe
                                     #x058bf18deca70966 #x41c5b9bb1da3bdcd
                                     #x2b557f679cc74f35 #xca208dac4d3dcfa4
                                     #x9a7826942b9f5f8a #xf0e8e048aafbad72
                                     #x53210f43c934970c #xc1f7e582ed1c2b76
                                     #xc47c140f01bb2210 #x9d56e44b0ddca7d0
                                     #x386d5eb62d2293ea #xe65b3014f7b978cb
                                     #x9424bf376719b23e #x30c392e32295701a))))

  (defconst +kalyna-s+
    (make-array '(4 256)
                :element-type '(unsigned-byte 8)
                :initial-contents '((#xa8 #x43 #x5f #x06 #x6b #x75 #x6c #x59
                                     #x71 #xdf #x87 #x95 #x17 #xf0 #xd8 #x09
                                     #x6d #xf3 #x1d #xcb #xc9 #x4d #x2c #xaf
                                     #x79 #xe0 #x97 #xfd #x6f #x4b #x45 #x39
                                     #x3e #xdd #xa3 #x4f #xb4 #xb6 #x9a #x0e
                                     #x1f #xbf #x15 #xe1 #x49 #xd2 #x93 #xc6
                                     #x92 #x72 #x9e #x61 #xd1 #x63 #xfa #xee
                                     #xf4 #x19 #xd5 #xad #x58 #xa4 #xbb #xa1
                                     #xdc #xf2 #x83 #x37 #x42 #xe4 #x7a #x32
                                     #x9c #xcc #xab #x4a #x8f #x6e #x04 #x27
                                     #x2e #xe7 #xe2 #x5a #x96 #x16 #x23 #x2b
                                     #xc2 #x65 #x66 #x0f #xbc #xa9 #x47 #x41
                                     #x34 #x48 #xfc #xb7 #x6a #x88 #xa5 #x53
                                     #x86 #xf9 #x5b #xdb #x38 #x7b #xc3 #x1e
                                     #x22 #x33 #x24 #x28 #x36 #xc7 #xb2 #x3b
                                     #x8e #x77 #xba #xf5 #x14 #x9f #x08 #x55
                                     #x9b #x4c #xfe #x60 #x5c #xda #x18 #x46
                                     #xcd #x7d #x21 #xb0 #x3f #x1b #x89 #xff
                                     #xeb #x84 #x69 #x3a #x9d #xd7 #xd3 #x70
                                     #x67 #x40 #xb5 #xde #x5d #x30 #x91 #xb1
                                     #x78 #x11 #x01 #xe5 #x00 #x68 #x98 #xa0
                                     #xc5 #x02 #xa6 #x74 #x2d #x0b #xa2 #x76
                                     #xb3 #xbe #xce #xbd #xae #xe9 #x8a #x31
                                     #x1c #xec #xf1 #x99 #x94 #xaa #xf6 #x26
                                     #x2f #xef #xe8 #x8c #x35 #x03 #xd4 #x7f
                                     #xfb #x05 #xc1 #x5e #x90 #x20 #x3d #x82
                                     #xf7 #xea #x0a #x0d #x7e #xf8 #x50 #x1a
                                     #xc4 #x07 #x57 #xb8 #x3c #x62 #xe3 #xc8
                                     #xac #x52 #x64 #x10 #xd0 #xd9 #x13 #x0c
                                     #x12 #x29 #x51 #xb9 #xcf #xd6 #x73 #x8d
                                     #x81 #x54 #xc0 #xed #x4e #x44 #xa7 #x2a
                                     #x85 #x25 #xe6 #xca #x7c #x8b #x56 #x80)
                                    (#xce #xbb #xeb #x92 #xea #xcb #x13 #xc1
                                     #xe9 #x3a #xd6 #xb2 #xd2 #x90 #x17 #xf8
                                     #x42 #x15 #x56 #xb4 #x65 #x1c #x88 #x43
                                     #xc5 #x5c #x36 #xba #xf5 #x57 #x67 #x8d
                                     #x31 #xf6 #x64 #x58 #x9e #xf4 #x22 #xaa
                                     #x75 #x0f #x02 #xb1 #xdf #x6d #x73 #x4d
                                     #x7c #x26 #x2e #xf7 #x08 #x5d #x44 #x3e
                                     #x9f #x14 #xc8 #xae #x54 #x10 #xd8 #xbc
                                     #x1a #x6b #x69 #xf3 #xbd #x33 #xab #xfa
                                     #xd1 #x9b #x68 #x4e #x16 #x95 #x91 #xee
                                     #x4c #x63 #x8e #x5b #xcc #x3c #x19 #xa1
                                     #x81 #x49 #x7b #xd9 #x6f #x37 #x60 #xca
                                     #xe7 #x2b #x48 #xfd #x96 #x45 #xfc #x41
                                     #x12 #x0d #x79 #xe5 #x89 #x8c #xe3 #x20
                                     #x30 #xdc #xb7 #x6c #x4a #xb5 #x3f #x97
                                     #xd4 #x62 #x2d #x06 #xa4 #xa5 #x83 #x5f
                                     #x2a #xda #xc9 #x00 #x7e #xa2 #x55 #xbf
                                     #x11 #xd5 #x9c #xcf #x0e #x0a #x3d #x51
                                     #x7d #x93 #x1b #xfe #xc4 #x47 #x09 #x86
                                     #x0b #x8f #x9d #x6a #x07 #xb9 #xb0 #x98
                                     #x18 #x32 #x71 #x4b #xef #x3b #x70 #xa0
                                     #xe4 #x40 #xff #xc3 #xa9 #xe6 #x78 #xf9
                                     #x8b #x46 #x80 #x1e #x38 #xe1 #xb8 #xa8
                                     #xe0 #x0c #x23 #x76 #x1d #x25 #x24 #x05
                                     #xf1 #x6e #x94 #x28 #x9a #x84 #xe8 #xa3
                                     #x4f #x77 #xd3 #x85 #xe2 #x52 #xf2 #x82
                                     #x50 #x7a #x2f #x74 #x53 #xb3 #x61 #xaf
                                     #x39 #x35 #xde #xcd #x1f #x99 #xac #xad
                                     #x72 #x2c #xdd #xd0 #x87 #xbe #x5e #xa6
                                     #xec #x04 #xc6 #x03 #x34 #xfb #xdb #x59
                                     #xb6 #xc2 #x01 #xf0 #x5a #xed #xa7 #x66
                                     #x21 #x7f #x8a #x27 #xc7 #xc0 #x29 #xd7)
                                    (#x93 #xd9 #x9a #xb5 #x98 #x22 #x45 #xfc
                                     #xba #x6a #xdf #x02 #x9f #xdc #x51 #x59
                                     #x4a #x17 #x2b #xc2 #x94 #xf4 #xbb #xa3
                                     #x62 #xe4 #x71 #xd4 #xcd #x70 #x16 #xe1
                                     #x49 #x3c #xc0 #xd8 #x5c #x9b #xad #x85
                                     #x53 #xa1 #x7a #xc8 #x2d #xe0 #xd1 #x72
                                     #xa6 #x2c #xc4 #xe3 #x76 #x78 #xb7 #xb4
                                     #x09 #x3b #x0e #x41 #x4c #xde #xb2 #x90
                                     #x25 #xa5 #xd7 #x03 #x11 #x00 #xc3 #x2e
                                     #x92 #xef #x4e #x12 #x9d #x7d #xcb #x35
                                     #x10 #xd5 #x4f #x9e #x4d #xa9 #x55 #xc6
                                     #xd0 #x7b #x18 #x97 #xd3 #x36 #xe6 #x48
                                     #x56 #x81 #x8f #x77 #xcc #x9c #xb9 #xe2
                                     #xac #xb8 #x2f #x15 #xa4 #x7c #xda #x38
                                     #x1e #x0b #x05 #xd6 #x14 #x6e #x6c #x7e
                                     #x66 #xfd #xb1 #xe5 #x60 #xaf #x5e #x33
                                     #x87 #xc9 #xf0 #x5d #x6d #x3f #x88 #x8d
                                     #xc7 #xf7 #x1d #xe9 #xec #xed #x80 #x29
                                     #x27 #xcf #x99 #xa8 #x50 #x0f #x37 #x24
                                     #x28 #x30 #x95 #xd2 #x3e #x5b #x40 #x83
                                     #xb3 #x69 #x57 #x1f #x07 #x1c #x8a #xbc
                                     #x20 #xeb #xce #x8e #xab #xee #x31 #xa2
                                     #x73 #xf9 #xca #x3a #x1a #xfb #x0d #xc1
                                     #xfe #xfa #xf2 #x6f #xbd #x96 #xdd #x43
                                     #x52 #xb6 #x08 #xf3 #xae #xbe #x19 #x89
                                     #x32 #x26 #xb0 #xea #x4b #x64 #x84 #x82
                                     #x6b #xf5 #x79 #xbf #x01 #x5f #x75 #x63
                                     #x1b #x23 #x3d #x68 #x2a #x65 #xe8 #x91
                                     #xf6 #xff #x13 #x58 #xf1 #x47 #x0a #x7f
                                     #xc5 #xa7 #xe7 #x61 #x5a #x06 #x46 #x44
                                     #x42 #x04 #xa0 #xdb #x39 #x86 #x54 #xaa
                                     #x8c #x34 #x21 #x8b #xf8 #x0c #x74 #x67)
                                    (#x68 #x8d #xca #x4d #x73 #x4b #x4e #x2a
                                     #xd4 #x52 #x26 #xb3 #x54 #x1e #x19 #x1f
                                     #x22 #x03 #x46 #x3d #x2d #x4a #x53 #x83
                                     #x13 #x8a #xb7 #xd5 #x25 #x79 #xf5 #xbd
                                     #x58 #x2f #x0d #x02 #xed #x51 #x9e #x11
                                     #xf2 #x3e #x55 #x5e #xd1 #x16 #x3c #x66
                                     #x70 #x5d #xf3 #x45 #x40 #xcc #xe8 #x94
                                     #x56 #x08 #xce #x1a #x3a #xd2 #xe1 #xdf
                                     #xb5 #x38 #x6e #x0e #xe5 #xf4 #xf9 #x86
                                     #xe9 #x4f #xd6 #x85 #x23 #xcf #x32 #x99
                                     #x31 #x14 #xae #xee #xc8 #x48 #xd3 #x30
                                     #xa1 #x92 #x41 #xb1 #x18 #xc4 #x2c #x71
                                     #x72 #x44 #x15 #xfd #x37 #xbe #x5f #xaa
                                     #x9b #x88 #xd8 #xab #x89 #x9c #xfa #x60
                                     #xea #xbc #x62 #x0c #x24 #xa6 #xa8 #xec
                                     #x67 #x20 #xdb #x7c #x28 #xdd #xac #x5b
                                     #x34 #x7e #x10 #xf1 #x7b #x8f #x63 #xa0
                                     #x05 #x9a #x43 #x77 #x21 #xbf #x27 #x09
                                     #xc3 #x9f #xb6 #xd7 #x29 #xc2 #xeb #xc0
                                     #xa4 #x8b #x8c #x1d #xfb #xff #xc1 #xb2
                                     #x97 #x2e #xf8 #x65 #xf6 #x75 #x07 #x04
                                     #x49 #x33 #xe4 #xd9 #xb9 #xd0 #x42 #xc7
                                     #x6c #x90 #x00 #x8e #x6f #x50 #x01 #xc5
                                     #xda #x47 #x3f #xcd #x69 #xa2 #xe2 #x7a
                                     #xa7 #xc6 #x93 #x0f #x0a #x06 #xe6 #x2b
                                     #x96 #xa3 #x1c #xaf #x6a #x12 #x84 #x39
                                     #xe7 #xb0 #x82 #xf7 #xfe #x9d #x87 #x5c
                                     #x81 #x35 #xde #xb4 #xa5 #xfc #x80 #xef
                                     #xcb #xbb #x6b #x76 #xba #x5a #x7d #x78
                                     #x0b #x95 #xe3 #xad #x74 #x98 #x3b #x36
                                     #x64 #x6d #xdc #xf0 #x59 #xa9 #x4c #x17
                                     #x7f #x91 #xb8 #xc9 #x57 #x1b #xe0 #x61))))

  (defconst +kalyna-is+
    (make-array '(4 256)
                :element-type '(unsigned-byte 8)
                :initial-contents '((#xa4 #xa2 #xa9 #xc5 #x4e #xc9 #x03 #xd9
                                     #x7e #x0f #xd2 #xad #xe7 #xd3 #x27 #x5b
                                     #xe3 #xa1 #xe8 #xe6 #x7c #x2a #x55 #x0c
                                     #x86 #x39 #xd7 #x8d #xb8 #x12 #x6f #x28
                                     #xcd #x8a #x70 #x56 #x72 #xf9 #xbf #x4f
                                     #x73 #xe9 #xf7 #x57 #x16 #xac #x50 #xc0
                                     #x9d #xb7 #x47 #x71 #x60 #xc4 #x74 #x43
                                     #x6c #x1f #x93 #x77 #xdc #xce #x20 #x8c
                                     #x99 #x5f #x44 #x01 #xf5 #x1e #x87 #x5e
                                     #x61 #x2c #x4b #x1d #x81 #x15 #xf4 #x23
                                     #xd6 #xea #xe1 #x67 #xf1 #x7f #xfe #xda
                                     #x3c #x07 #x53 #x6a #x84 #x9c #xcb #x02
                                     #x83 #x33 #xdd #x35 #xe2 #x59 #x5a #x98
                                     #xa5 #x92 #x64 #x04 #x06 #x10 #x4d #x1c
                                     #x97 #x08 #x31 #xee #xab #x05 #xaf #x79
                                     #xa0 #x18 #x46 #x6d #xfc #x89 #xd4 #xc7
                                     #xff #xf0 #xcf #x42 #x91 #xf8 #x68 #x0a
                                     #x65 #x8e #xb6 #xfd #xc3 #xef #x78 #x4c
                                     #xcc #x9e #x30 #x2e #xbc #x0b #x54 #x1a
                                     #xa6 #xbb #x26 #x80 #x48 #x94 #x32 #x7d
                                     #xa7 #x3f #xae #x22 #x3d #x66 #xaa #xf6
                                     #x00 #x5d #xbd #x4a #xe0 #x3b #xb4 #x17
                                     #x8b #x9f #x76 #xb0 #x24 #x9a #x25 #x63
                                     #xdb #xeb #x7a #x3e #x5c #xb3 #xb1 #x29
                                     #xf2 #xca #x58 #x6e #xd8 #xa8 #x2f #x75
                                     #xdf #x14 #xfb #x13 #x49 #x88 #xb2 #xec
                                     #xe4 #x34 #x2d #x96 #xc6 #x3a #xed #x95
                                     #x0e #xe5 #x85 #x6b #x40 #x21 #x9b #x09
                                     #x19 #x2b #x52 #xde #x45 #xa3 #xfa #x51
                                     #xc2 #xb5 #xd1 #x90 #xb9 #xf3 #x37 #xc1
                                     #x0d #xba #x41 #x11 #x38 #x7b #xbe #xd0
                                     #xd5 #x69 #x36 #xc8 #x62 #x1b #x82 #x8f)
                                    (#x83 #xf2 #x2a #xeb #xe9 #xbf #x7b #x9c
                                     #x34 #x96 #x8d #x98 #xb9 #x69 #x8c #x29
                                     #x3d #x88 #x68 #x06 #x39 #x11 #x4c #x0e
                                     #xa0 #x56 #x40 #x92 #x15 #xbc #xb3 #xdc
                                     #x6f #xf8 #x26 #xba #xbe #xbd #x31 #xfb
                                     #xc3 #xfe #x80 #x61 #xe1 #x7a #x32 #xd2
                                     #x70 #x20 #xa1 #x45 #xec #xd9 #x1a #x5d
                                     #xb4 #xd8 #x09 #xa5 #x55 #x8e #x37 #x76
                                     #xa9 #x67 #x10 #x17 #x36 #x65 #xb1 #x95
                                     #x62 #x59 #x74 #xa3 #x50 #x2f #x4b #xc8
                                     #xd0 #x8f #xcd #xd4 #x3c #x86 #x12 #x1d
                                     #x23 #xef #xf4 #x53 #x19 #x35 #xe6 #x7f
                                     #x5e #xd6 #x79 #x51 #x22 #x14 #xf7 #x1e
                                     #x4a #x42 #x9b #x41 #x73 #x2d #xc1 #x5c
                                     #xa6 #xa2 #xe0 #x2e #xd3 #x28 #xbb #xc9
                                     #xae #x6a #xd1 #x5a #x30 #x90 #x84 #xf9
                                     #xb2 #x58 #xcf #x7e #xc5 #xcb #x97 #xe4
                                     #x16 #x6c #xfa #xb0 #x6d #x1f #x52 #x99
                                     #x0d #x4e #x03 #x91 #xc2 #x4d #x64 #x77
                                     #x9f #xdd #xc4 #x49 #x8a #x9a #x24 #x38
                                     #xa7 #x57 #x85 #xc7 #x7c #x7d #xe7 #xf6
                                     #xb7 #xac #x27 #x46 #xde #xdf #x3b #xd7
                                     #x9e #x2b #x0b #xd5 #x13 #x75 #xf0 #x72
                                     #xb6 #x9d #x1b #x01 #x3f #x44 #xe5 #x87
                                     #xfd #x07 #xf1 #xab #x94 #x18 #xea #xfc
                                     #x3a #x82 #x5f #x05 #x54 #xdb #x00 #x8b
                                     #xe3 #x48 #x0c #xca #x78 #x89 #x0a #xff
                                     #x3e #x5b #x81 #xee #x71 #xe2 #xda #x2c
                                     #xb8 #xb5 #xcc #x6e #xa8 #x6b #xad #x60
                                     #xc6 #x08 #x04 #x02 #xe8 #xf5 #x4f #xa4
                                     #xf3 #xc0 #xce #x43 #x25 #x1c #x21 #x33
                                     #x0f #xaf #x47 #xed #x66 #x63 #x93 #xaa)
                                    (#x45 #xd4 #x0b #x43 #xf1 #x72 #xed #xa4
                                     #xc2 #x38 #xe6 #x71 #xfd #xb6 #x3a #x95
                                     #x50 #x44 #x4b #xe2 #x74 #x6b #x1e #x11
                                     #x5a #xc6 #xb4 #xd8 #xa5 #x8a #x70 #xa3
                                     #xa8 #xfa #x05 #xd9 #x97 #x40 #xc9 #x90
                                     #x98 #x8f #xdc #x12 #x31 #x2c #x47 #x6a
                                     #x99 #xae #xc8 #x7f #xf9 #x4f #x5d #x96
                                     #x6f #xf4 #xb3 #x39 #x21 #xda #x9c #x85
                                     #x9e #x3b #xf0 #xbf #xef #x06 #xee #xe5
                                     #x5f #x20 #x10 #xcc #x3c #x54 #x4a #x52
                                     #x94 #x0e #xc0 #x28 #xf6 #x56 #x60 #xa2
                                     #xe3 #x0f #xec #x9d #x24 #x83 #x7e #xd5
                                     #x7c #xeb #x18 #xd7 #xcd #xdd #x78 #xff
                                     #xdb #xa1 #x09 #xd0 #x76 #x84 #x75 #xbb
                                     #x1d #x1a #x2f #xb0 #xfe #xd6 #x34 #x63
                                     #x35 #xd2 #x2a #x59 #x6d #x4d #x77 #xe7
                                     #x8e #x61 #xcf #x9f #xce #x27 #xf5 #x80
                                     #x86 #xc7 #xa6 #xfb #xf8 #x87 #xab #x62
                                     #x3f #xdf #x48 #x00 #x14 #x9a #xbd #x5b
                                     #x04 #x92 #x02 #x25 #x65 #x4c #x53 #x0c
                                     #xf2 #x29 #xaf #x17 #x6c #x41 #x30 #xe9
                                     #x93 #x55 #xf7 #xac #x68 #x26 #xc4 #x7d
                                     #xca #x7a #x3e #xa0 #x37 #x03 #xc1 #x36
                                     #x69 #x66 #x08 #x16 #xa7 #xbc #xc5 #xd3
                                     #x22 #xb7 #x13 #x46 #x32 #xe8 #x57 #x88
                                     #x2b #x81 #xb2 #x4e #x64 #x1c #xaa #x91
                                     #x58 #x2e #x9b #x5c #x1b #x51 #x73 #x42
                                     #x23 #x01 #x6e #xf3 #x0d #xbe #x3d #x0a
                                     #x2d #x1f #x67 #x33 #x19 #x7b #x5e #xea
                                     #xde #x8b #xcb #xa9 #x8c #x8d #xad #x49
                                     #x82 #xe4 #xba #xc3 #x15 #xd1 #xe0 #x89
                                     #xfc #xb1 #xb9 #xb5 #x07 #x79 #xb8 #xe1)
                                    (#xb2 #xb6 #x23 #x11 #xa7 #x88 #xc5 #xa6
                                     #x39 #x8f #xc4 #xe8 #x73 #x22 #x43 #xc3
                                     #x82 #x27 #xcd #x18 #x51 #x62 #x2d #xf7
                                     #x5c #x0e #x3b #xfd #xca #x9b #x0d #x0f
                                     #x79 #x8c #x10 #x4c #x74 #x1c #x0a #x8e
                                     #x7c #x94 #x07 #xc7 #x5e #x14 #xa1 #x21
                                     #x57 #x50 #x4e #xa9 #x80 #xd9 #xef #x64
                                     #x41 #xcf #x3c #xee #x2e #x13 #x29 #xba
                                     #x34 #x5a #xae #x8a #x61 #x33 #x12 #xb9
                                     #x55 #xa8 #x15 #x05 #xf6 #x03 #x06 #x49
                                     #xb5 #x25 #x09 #x16 #x0c #x2a #x38 #xfc
                                     #x20 #xf4 #xe5 #x7f #xd7 #x31 #x2b #x66
                                     #x6f #xff #x72 #x86 #xf0 #xa3 #x2f #x78
                                     #x00 #xbc #xcc #xe2 #xb0 #xf1 #x42 #xb4
                                     #x30 #x5f #x60 #x04 #xec #xa5 #xe3 #x8b
                                     #xe7 #x1d #xbf #x84 #x7b #xe6 #x81 #xf8
                                     #xde #xd8 #xd2 #x17 #xce #x4b #x47 #xd6
                                     #x69 #x6c #x19 #x99 #x9a #x01 #xb3 #x85
                                     #xb1 #xf9 #x59 #xc2 #x37 #xe9 #xc8 #xa0
                                     #xed #x4f #x89 #x68 #x6d #xd5 #x26 #x91
                                     #x87 #x58 #xbd #xc9 #x98 #xdc #x75 #xc0
                                     #x76 #xf5 #x67 #x6b #x7e #xeb #x52 #xcb
                                     #xd1 #x5b #x9f #x0b #xdb #x40 #x92 #x1a
                                     #xfa #xac #xe4 #xe1 #x71 #x1f #x65 #x8d
                                     #x97 #x9e #x95 #x90 #x5d #xb7 #xc1 #xaf
                                     #x54 #xfb #x02 #xe0 #x35 #xbb #x3a #x4d
                                     #xad #x2c #x3d #x56 #x08 #x1b #x4a #x93
                                     #x6a #xab #xb8 #x7a #xf2 #x7d #xda #x3f
                                     #xfe #x3e #xbe #xea #xaa #x44 #xc6 #xd0
                                     #x36 #x48 #x70 #x96 #x77 #x24 #x53 #xdf
                                     #xf3 #x83 #x28 #x32 #x45 #x1e #xa4 #xd3
                                     #xa2 #x46 #x6e #x9c #xdd #x63 #xd4 #x9d)))))

(declaim (type (simple-array (unsigned-byte 64) (8 256)) +kalyna-t+ +kalyna-it+)
         (type (simple-array (unsigned-byte 8) (4 256)) +kalyna-s+ +kalyna-is+))


;;;
;;; Common functions and macros
;;;

(declaim (inline kalyna-make-odd-key))
(defun kalyna-make-odd-key (n ek ek-start ok ok-start)
  (declare (type (integer 0 8) n)
           (type (simple-array (unsigned-byte 64) (*)) ek ok)
           (type (integer 0 144) ek-start ok-start)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c d e)
               `(setf (aref ok (+ ok-start ,a))
                      (logior (mod64ash (aref ek (+ ek-start ,b)) ,c)
                              (mod64ash (aref ek (+ ek-start ,d)) ,e)))))
    (ecase n
      (2
       (m 0 1 8 0 -56) (m 1 0 8 1 -56))
      (4
       (m 0 2 40 1 -24) (m 1 3 40 2 -24) (m 2 0 40 3 -24) (m 3 1 40 0 -24))
      (8
       (m 0 3 40 2 -24) (m 1 4 40 3 -24) (m 2 5 40 4 -24) (m 3 6 40 5 -24)
       (m 4 7 40 6 -24) (m 5 0 40 7 -24) (m 6 1 40 0 -24) (m 7 2 40 1 -24))))
  (values))

(declaim (inline kalyna-swap-blocks))
(defun kalyna-swap-blocks (n k)
  (declare (type (integer 0 8) n)
           (type (simple-array (unsigned-byte 64) (*)) k)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (ecase n
    (2 (rotatef (aref k 0) (aref k 1)))
    (4 (rotatef (aref k 0) (aref k 1) (aref k 2) (aref k 3)))
    (8 (rotatef (aref k 0) (aref k 1) (aref k 2) (aref k 3)
                (aref k 4) (aref k 5) (aref k 6) (aref k 7))))
  (values))

(defmacro kalyna-add-key (n x x-start y k)
  `(dotimes-unrolled (i ,n)
     (setf (aref ,y i) (mod64+ (aref ,x (+ ,x-start i)) (aref ,k i)))))

(defmacro kalyna-sub-key (n x y k k-start)
  `(dotimes-unrolled (i ,n)
     (setf (aref ,y i) (mod64- (aref ,x i) (aref ,k (+ ,k-start i))))))

(defmacro kalyna-add-constant (n x y c)
  `(dotimes-unrolled (i ,n)
     (setf (aref ,y i) (mod64+ (aref ,x i) ,c))))


;;;
;;; Kalyna128
;;;

(declaim (inline kalyna-g0128))
(defun kalyna-g0128 (x y)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c)
               `(aref +kalyna-t+ ,a (logand (mod64ash (aref x ,b) ,c) #xff))))
    (setf (aref y 0)
          (logxor (m 0 0 0) (m 1 0 -8) (m 2 0 -16) (m 3 0 -24)
                  (m 4 1 -32) (m 5 1 -40) (m 6 1 -48) (m 7 1 -56)))
    (setf (aref y 1)
          (logxor (m 0 1 0) (m 1 1 -8) (m 2 1 -16) (m 3 1 -24)
                  (m 4 0 -32) (m 5 0 -40) (m 6 0 -48)(m 7 0 -56))))
  (values))

(declaim (inline kalyna-gl128))
(defun kalyna-gl128 (x y y-start k k-start)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y k)
           (type (integer 0 30) y-start k-start)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c)
               `(aref +kalyna-t+ ,a (logand (mod64ash (aref x ,b) ,c) #xff))))
    (setf (aref y (+ y-start 0))
          (mod64+ (aref k (+ k-start 0))
                  (logxor (m 0 0 0) (m 1 0 -8) (m 2 0 -16) (m 3 0 -24)
                          (m 4 1 -32) (m 5 1 -40) (m 6 1 -48) (m 7 1 -56))))
    (setf (aref y (+ y-start 1))
          (mod64+ (aref k (+ k-start 1))
                  (logxor (m 0 1 0) (m 1 1 -8) (m 2 1 -16) (m 3 1 -24)
                          (m 4 0 -32) (m 5 0 -40) (m 6 0 -48) (m 7 0 -56)))))
  (values))

(declaim (inline kalyna-imc128))
(defun kalyna-imc128 (x x-start)
  (declare (type (simple-array (unsigned-byte 64) (*)) x)
           (type (integer 0 30) x-start)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c d)
               `(aref +kalyna-it+
                      ,a
                      (aref +kalyna-s+
                            ,b
                            (logand (mod64ash (aref x (+ x-start ,c)) ,d) #xff)))))
    (setf (aref x (+ x-start 0))
          (logxor (m 0 0 0 0) (m 1 1 0 -8) (m 2 2 0 -16) (m 3 3 0 -24)
                  (m 4 0 0 -32) (m 5 1 0 -40) (m 6 2 0 -48) (m 7 3 0 -56)))
    (setf (aref x (+ x-start 1))
          (logxor (m 0 0 1 0) (m 1 1 1 -8) (m 2 2 1 -16) (m 3 3 1 -24)
                  (m 4 0 1 -32) (m 5 1 1 -40) (m 6 2 1 -48) (m 7 3 1 -56))))
  (values))

(declaim (inline kalyna-ig128))
(defun kalyna-ig128 (x y k k-start)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y k)
           (type (integer 0 30) k-start)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c)
               `(aref +kalyna-it+ ,a (logand (mod64ash (aref x ,b) ,c) #xff))))
    (setf (aref y 0)
          (logxor (aref k (+ k-start 0))
                  (m 0 0 0) (m 1 0 -8) (m 2 0 -16) (m 3 0 -24)
                  (m 4 1 -32) (m 5 1 -40) (m 6 1 -48) (m 7 1 -56)))
    (setf (aref y 1)
          (logxor (aref k (+ k-start 1))
                  (m 0 1 0) (m 1 1 -8) (m 2 1 -16) (m 3 1 -24)
                  (m 4 0 -32) (m 5 0 -40) (m 6 0 -48) (m 7 0 -56))))
  (values))

(declaim (inline kalyna-igl128))
(defun kalyna-igl128 (x y k k-start)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y k)
           (type (integer 0 30) k-start)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c d)
               `(mod64ash (aref +kalyna-is+
                                ,a
                                (logand (mod64ash (aref x ,b) ,c) #xff))
                          ,d)))
    (setf (aref y 0)
          (mod64- (logxor (m 0 0 0 0) (m 1 0 -8 8) (m 2 0 -16 16) (m 3 0 -24 24)
                          (m 0 1 -32 32) (m 1 1 -40 40) (m 2 1 -48 48) (m 3 1 -56 56))
                  (aref k (+ k-start 0))))
    (setf (aref y 1)
          (mod64- (logxor (m 0 1 0 0) (m 1 1 -8 8) (m 2 1 -16 16) (m 3 1 -24 24)
                          (m 0 0 -32 32) (m 1 0 -40 40) (m 2 0 -48 48) (m 3 0 -56 56))
                  (aref k (+ k-start 1)))))
  (values))

(declaim (inline kalyna-g128))
(defun kalyna-g128 (x y k k-start)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y k)
           (type (integer 0 30) k-start)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c)
               `(aref +kalyna-t+ ,a (logand (mod64ash (aref x ,b) ,c) #xff))))
    (setf (aref y 0)
          (logxor (aref k (+ k-start 0))
                  (m 0 0 0) (m 1 0 -8) (m 2 0 -16) (m 3 0 -24)
                  (m 4 1 -32) (m 5 1 -40) (m 6 1 -48) (m 7 1 -56)))
    (setf (aref y 1)
          (logxor (aref k (+ k-start 1))
                  (m 0 1 0) (m 1 1 -8) (m 2 1 -16) (m 3 1 -24)
                  (m 4 0 -32) (m 5 0 -40) (m 6 0 -48) (m 7 0 -56))))
  (values))

(defclass kalyna128 (cipher 16-byte-block-mixin)
  ((encryption-round-keys :accessor encryption-round-keys
                          :initform (make-array 30 :element-type '(unsigned-byte 64))
                          :type (simple-array (unsigned-byte 64) (30)))
   (decryption-round-keys :accessor decryption-round-keys
                          :initform (make-array 30 :element-type '(unsigned-byte 64))
                          :type (simple-array (unsigned-byte 64) (30)))
   (n-rounds :accessor n-rounds)))

(defmethod schedule-key ((cipher kalyna128) key)
  (let ((encryption-round-keys (encryption-round-keys cipher))
        (decryption-round-keys (decryption-round-keys cipher)))
    (declare (type (simple-array (unsigned-byte 64) (30)) encryption-round-keys)
             (type (simple-array (unsigned-byte 64) (30)) decryption-round-keys))
    (ecase (length key)
      (16
       (let ((key (make-array 2 :element-type '(unsigned-byte 64)
                                :initial-contents (list (ub64ref/le key 0)
                                                        (ub64ref/le key 8))))
             (ks (make-array 2 :element-type '(unsigned-byte 64)))
             (ksc (make-array 2 :element-type '(unsigned-byte 64)))
             (t1 (make-array 2 :element-type '(unsigned-byte 64)))
             (t2 (make-array 2 :element-type '(unsigned-byte 64)))
             (k (make-array 2 :element-type '(unsigned-byte 64)))
             (kswapped (make-array 2 :element-type '(unsigned-byte 64)))
             (constant #x0001000100010001))
         (declare (type (simple-array (unsigned-byte 64) (2)) key ks ksc t1 t2 k kswapped)
                  (dynamic-extent key ks ksc t1 t2 k kswapped)
                  (type (unsigned-byte 64) constant))
         (setf (n-rounds cipher) 10)
         (setf (aref t1 0) (/ (+ 128 128 64) 64)
               (aref t1 1) 0)
         (kalyna-add-key 2 t1 0 t2 key)
         (kalyna-g128 t2 t1 key 0)
         (kalyna-gl128 t1 t2 0 key 0)
         (kalyna-g0128 t2 ks)

         ;; Round 0
         (replace k key)
         (setf (aref kswapped 1) (aref k 0)
               (aref kswapped 0) (aref k 1))
         (kalyna-add-constant 2 ks ksc constant)
         (kalyna-add-key 2 k 0 t2 ksc)
         (kalyna-g128 t2 t1 ksc 0)
         (kalyna-gl128 t1 encryption-round-keys 0 ksc 0)
         (kalyna-make-odd-key 2 encryption-round-keys 0 encryption-round-keys 2)

         ;; Rounds 2 to 9
         (flet ((r (v n)
                  (setf constant (mod64ash constant 1))
                  (kalyna-add-constant 2 ks ksc constant)
                  (kalyna-add-key 2 v 0 t2 ksc)
                  (kalyna-g128 t2 t1 ksc 0)
                  (kalyna-gl128 t1 encryption-round-keys n ksc 0)
                  (kalyna-make-odd-key 2
                                       encryption-round-keys n
                                       encryption-round-keys (+ n 2))))
           (r kswapped 4)
           (r k 8)
           (r kswapped 12)
           (r k 16))

         ;; Round 10
         (setf constant (mod64ash constant 1))
         (kalyna-add-constant 2 ks ksc constant)
         (kalyna-add-key 2 kswapped 0 t2 ksc)
         (kalyna-g128 t2 t1 ksc 0)
         (kalyna-gl128 t1 encryption-round-keys 20 ksc 0)

         (replace decryption-round-keys encryption-round-keys)
         (loop for n from 18 downto 2 by 2 do
           (kalyna-imc128 decryption-round-keys n))))

      (32
       (let ((key (make-array 4 :element-type '(unsigned-byte 64)
                                :initial-contents (list (ub64ref/le key 0)
                                                        (ub64ref/le key 8)
                                                        (ub64ref/le key 16)
                                                        (ub64ref/le key 24))))
             (ks (make-array 2 :element-type '(unsigned-byte 64)))
             (ksc (make-array 2 :element-type '(unsigned-byte 64)))
             (t1 (make-array 2 :element-type '(unsigned-byte 64)))
             (t2 (make-array 2 :element-type '(unsigned-byte 64)))
             (k (make-array 4 :element-type '(unsigned-byte 64)))
             (ka (make-array 2 :element-type '(unsigned-byte 64)))
             (ko (make-array 2 :element-type '(unsigned-byte 64)))
             (constant #x0001000100010001))
         (declare (type (simple-array (unsigned-byte 64) (4)) key k)
                  (type (simple-array (unsigned-byte 64) (2)) ks ksc t1 t2 ka ko)
                  (dynamic-extent key ks ksc t1 t2 k ka ko)
                  (type (unsigned-byte 64) constant))
         (setf (n-rounds cipher) 14)
         (setf (aref t1 0) (/ (+ 128 256 64) 64)
               (aref t1 1) 0)
         (replace ka key :end2 2)
         (replace ko key :start2 2)
         (kalyna-add-key 2 t1 0 t2 ka)
         (kalyna-g128 t2 t1 ko 0)
         (kalyna-gl128 t1 t2 0 ka 0)
         (kalyna-g0128 t2 ks)

         ;; Round 0
         (replace k key)
         (kalyna-add-constant 2 ks ksc constant)
         (kalyna-add-key 2 k 0 t2 ksc)
         (kalyna-g128 t2 t1 ksc 0)
         (kalyna-gl128 t1 encryption-round-keys 0 ksc 0)
         (kalyna-make-odd-key 2 encryption-round-keys 0 encryption-round-keys 2)

         ;; Rounds 2 to 13
         (flet ((r (v n)
                  (when (zerop v)
                    (kalyna-swap-blocks 4 k))
                  (setf constant (mod64ash constant 1))
                  (kalyna-add-constant 2 ks ksc constant)
                  (kalyna-add-key 2 k v t2 ksc)
                  (kalyna-g128 t2 t1 ksc 0)
                  (kalyna-gl128 t1 encryption-round-keys n ksc 0)
                  (kalyna-make-odd-key 2
                                       encryption-round-keys n
                                       encryption-round-keys (+ n 2))))
           (r 2 4)
           (r 0 8)
           (r 2 12)
           (r 0 16)
           (r 2 20)
           (r 0 24))

         ;; Round 14
         (setf constant (mod64ash constant 1))
         (kalyna-add-constant 2 ks ksc constant)
         (kalyna-add-key 2 k 2 t2 ksc)
         (kalyna-g128 t2 t1 ksc 0)
         (kalyna-gl128 t1 encryption-round-keys 28 ksc 0)

         (replace decryption-round-keys encryption-round-keys)
         (loop for n from 26 downto 2 by 2 do
           (kalyna-imc128 decryption-round-keys n)))))
    cipher))

(define-block-encryptor kalyna128 16
  (let ((encryption-round-keys (encryption-round-keys context))
        (t1 (make-array 2 :element-type '(unsigned-byte 64)))
        (t2 (make-array 2 :element-type '(unsigned-byte 64))))
    (declare (type (simple-array (unsigned-byte 64) (30)) encryption-round-keys)
             (type (simple-array (unsigned-byte 64) (2)) t1 t2)
             (dynamic-extent t1 t2))
    (setf (aref t2 0) (ub64ref/le plaintext plaintext-start)
          (aref t2 1) (ub64ref/le plaintext (+ plaintext-start 8)))
    (kalyna-add-key 2 t2 0 t1 encryption-round-keys)
    (kalyna-g128 t1 t2 encryption-round-keys 2)
    (kalyna-g128 t2 t1 encryption-round-keys 4)
    (kalyna-g128 t1 t2 encryption-round-keys 6)
    (kalyna-g128 t2 t1 encryption-round-keys 8)
    (kalyna-g128 t1 t2 encryption-round-keys 10)
    (kalyna-g128 t2 t1 encryption-round-keys 12)
    (kalyna-g128 t1 t2 encryption-round-keys 14)
    (kalyna-g128 t2 t1 encryption-round-keys 16)
    (kalyna-g128 t1 t2 encryption-round-keys 18)
    (ecase (n-rounds context)
      (10
       (kalyna-gl128 t2 t1 0 encryption-round-keys 20))
      (14
       (kalyna-g128 t2 t1 encryption-round-keys 20)
       (kalyna-g128 t1 t2 encryption-round-keys 22)
       (kalyna-g128 t2 t1 encryption-round-keys 24)
       (kalyna-g128 t1 t2 encryption-round-keys 26)
       (kalyna-gl128 t2 t1 0 encryption-round-keys 28)))
    (setf (ub64ref/le ciphertext ciphertext-start) (aref t1 0)
          (ub64ref/le ciphertext (+ ciphertext-start 8)) (aref t1 1))
    (values)))

(define-block-decryptor kalyna128 16
  (let ((decryption-round-keys (decryption-round-keys context))
        (t1 (make-array 2 :element-type '(unsigned-byte 64)))
        (t2 (make-array 2 :element-type '(unsigned-byte 64))))
    (declare (type (simple-array (unsigned-byte 64) (30)) decryption-round-keys)
             (type (simple-array (unsigned-byte 64) (2)) t1 t2)
             (dynamic-extent t1 t2))
    (setf (aref t2 0) (ub64ref/le ciphertext ciphertext-start)
          (aref t2 1) (ub64ref/le ciphertext (+ ciphertext-start 8)))
    (ecase (n-rounds context)
      (10
       (kalyna-sub-key 2 t2 t1 decryption-round-keys 20)
       (kalyna-imc128 t1 0))
      (14
       (kalyna-sub-key 2 t2 t1 decryption-round-keys 28)
       (kalyna-imc128 t1 0)
       (kalyna-ig128 t1 t2 decryption-round-keys 26)
       (kalyna-ig128 t2 t1 decryption-round-keys 24)
       (kalyna-ig128 t1 t2 decryption-round-keys 22)
       (kalyna-ig128 t2 t1 decryption-round-keys 20)))
    (kalyna-ig128 t1 t2 decryption-round-keys 18)
    (kalyna-ig128 t2 t1 decryption-round-keys 16)
    (kalyna-ig128 t1 t2 decryption-round-keys 14)
    (kalyna-ig128 t2 t1 decryption-round-keys 12)
    (kalyna-ig128 t1 t2 decryption-round-keys 10)
    (kalyna-ig128 t2 t1 decryption-round-keys 8)
    (kalyna-ig128 t1 t2 decryption-round-keys 6)
    (kalyna-ig128 t2 t1 decryption-round-keys 4)
    (kalyna-ig128 t1 t2 decryption-round-keys 2)
    (kalyna-igl128 t2 t1 decryption-round-keys 0)
    (setf (ub64ref/le plaintext plaintext-start) (aref t1 0)
          (ub64ref/le plaintext (+ plaintext-start 8)) (aref t1 1))
    (values)))

(defcipher kalyna128
  (:encrypt-function kalyna128-encrypt-block)
  (:decrypt-function kalyna128-decrypt-block)
  (:block-length 16)
  (:key-length (:fixed 16 32)))


;;;
;;; Kalyna256
;;;

(declaim (inline kalyna-g0256))
(defun kalyna-g0256 (x y)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c)
               `(aref +kalyna-t+ ,a (logand (mod64ash (aref x ,b) ,c) #xff))))
    (setf (aref y 0)
          (logxor (m 0 0 0) (m 1 0 -8) (m 2 3 -16) (m 3 3 -24)
                  (m 4 2 -32) (m 5 2 -40) (m 6 1 -48) (m 7 1 -56)))
    (setf (aref y 1)
          (logxor (m 0 1 0) (m 1 1 -8) (m 2 0 -16) (m 3 0 -24)
                  (m 4 3 -32) (m 5 3 -40) (m 6 2 -48) (m 7 2 -56)))
    (setf (aref y 2)
          (logxor (m 0 2 0) (m 1 2 -8) (m 2 1 -16) (m 3 1 -24)
                  (m 4 0 -32) (m 5 0 -40) (m 6 3 -48) (m 7 3 -56)))
    (setf (aref y 3)
          (logxor (m 0 3 0) (m 1 3 -8) (m 2 2 -16) (m 3 2 -24)
                  (m 4 1 -32) (m 5 1 -40) (m 6 0 -48) (m 7 0 -56))))
  (values))

(declaim (inline kalyna-gl256))
(defun kalyna-gl256 (x y y-start k k-start)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y k)
           (type (integer 0 76) y-start k-start)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c)
               `(aref +kalyna-t+ ,a (logand (mod64ash (aref x ,b) ,c) #xff))))
    (setf (aref y (+ y-start 0))
          (mod64+ (aref k (+ k-start 0))
                  (logxor (m 0 0 0) (m 1 0 -8) (m 2 3 -16) (m 3 3 -24)
                          (m 4 2 -32) (m 5 2 -40) (m 6 1 -48) (m 7 1 -56))))
    (setf (aref y (+ y-start 1))
          (mod64+ (aref k (+ k-start 1))
                  (logxor (m 0 1 0) (m 1 1 -8) (m 2 0 -16) (m 3 0 -24)
                          (m 4 3 -32) (m 5 3 -40) (m 6 2 -48) (m 7 2 -56))))
    (setf (aref y (+ y-start 2))
          (mod64+ (aref k (+ k-start 2))
                  (logxor (m 0 2 0) (m 1 2 -8) (m 2 1 -16) (m 3 1 -24)
                          (m 4 0 -32) (m 5 0 -40) (m 6 3 -48) (m 7 3 -56))))
    (setf (aref y (+ y-start 3))
          (mod64+ (aref k (+ k-start 3))
                  (logxor (m 0 3 0) (m 1 3 -8) (m 2 2 -16) (m 3 2 -24)
                          (m 4 1 -32) (m 5 1 -40) (m 6 0 -48) (m 7 0 -56)))))
  (values))

(declaim (inline kalyna-imc256))
(defun kalyna-imc256 (x x-start)
  (declare (type (simple-array (unsigned-byte 64) (*)) x)
           (type (integer 0 76) x-start)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c d)
               `(aref +kalyna-it+
                      ,a
                      (aref +kalyna-s+
                            ,b
                            (logand (mod64ash (aref x (+ x-start ,c)) ,d) #xff)))))
    (setf (aref x (+ x-start 0))
          (logxor (m 0 0 0 0) (m 1 1 0 -8) (m 2 2 0 -16) (m 3 3 0 -24)
                  (m 4 0 0 -32) (m 5 1 0 -40) (m 6 2 0 -48) (m 7 3 0 -56)))
    (setf (aref x (+ x-start 1))
          (logxor (m 0 0 1 0) (m 1 1 1 -8) (m 2 2 1 -16) (m 3 3 1 -24)
                  (m 4 0 1 -32) (m 5 1 1 -40) (m 6 2 1 -48) (m 7 3 1 -56)))
    (setf (aref x (+ x-start 2))
          (logxor (m 0 0 2 0) (m 1 1 2 -8) (m 2 2 2 -16) (m 3 3 2 -24)
                  (m 4 0 2 -32) (m 5 1 2 -40) (m 6 2 2 -48) (m 7 3 2 -56)))
    (setf (aref x (+ x-start 3))
          (logxor (m 0 0 3 0) (m 1 1 3 -8) (m 2 2 3 -16) (m 3 3 3 -24)
                  (m 4 0 3 -32) (m 5 1 3 -40) (m 6 2 3 -48) (m 7 3 3 -56))))
  (values))

(declaim (inline kalyna-ig256))
(defun kalyna-ig256 (x y k k-start)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y k)
           (type (integer 0 76) k-start)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c)
               `(aref +kalyna-it+ ,a (logand (mod64ash (aref x ,b) ,c) #xff))))
    (setf (aref y 0)
          (logxor (aref k (+ k-start 0))
                  (m 0 0 0) (m 1 0 -8) (m 2 1 -16) (m 3 1 -24)
                  (m 4 2 -32) (m 5 2 -40) (m 6 3 -48) (m 7 3 -56)))
    (setf (aref y 1)
          (logxor (aref k (+ k-start 1))
                  (m 0 1 0) (m 1 1 -8) (m 2 2 -16) (m 3 2 -24)
                  (m 4 3 -32) (m 5 3 -40) (m 6 0 -48) (m 7 0 -56)))
    (setf (aref y 2)
          (logxor (aref k (+ k-start 2))
                  (m 0 2 0) (m 1 2 -8) (m 2 3 -16) (m 3 3 -24)
                  (m 4 0 -32) (m 5 0 -40) (m 6 1 -48) (m 7 1 -56)))
    (setf (aref y 3)
          (logxor (aref k (+ k-start 3))
                  (m 0 3 0) (m 1 3 -8) (m 2 0 -16) (m 3 0 -24)
                  (m 4 1 -32) (m 5 1 -40) (m 6 2 -48) (m 7 2 -56))))
  (values))

(declaim (inline kalyna-igl256))
(defun kalyna-igl256 (x y k k-start)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y k)
           (type (integer 0 76) k-start)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c d)
               `(mod64ash (aref +kalyna-is+
                                ,a
                                (logand (mod64ash (aref x ,b) ,c) #xff))
                          ,d)))
    (setf (aref y 0)
          (mod64- (logxor (m 0 0 0 0) (m 1 0 -8 8) (m 2 1 -16 16) (m 3 1 -24 24)
                          (m 0 2 -32 32) (m 1 2 -40 40) (m 2 3 -48 48) (m 3 3 -56 56))
                  (aref k (+ k-start 0))))
    (setf (aref y 1)
          (mod64- (logxor (m 0 1 0 0) (m 1 1 -8 8) (m 2 2 -16 16) (m 3 2 -24 24)
                          (m 0 3 -32 32) (m 1 3 -40 40) (m 2 0 -48 48) (m 3 0 -56 56))
                  (aref k (+ k-start 1))))
    (setf (aref y 2)
          (mod64- (logxor (m 0 2 0 0) (m 1 2 -8 8) (m 2 3 -16 16) (m 3 3 -24 24)
                          (m 0 0 -32 32) (m 1 0 -40 40) (m 2 1 -48 48) (m 3 1 -56 56))
                  (aref k (+ k-start 2))))
    (setf (aref y 3)
          (mod64- (logxor (m 0 3 0 0) (m 1 3 -8 8) (m 2 0 -16 16) (m 3 0 -24 24)
                          (m 0 1 -32 32) (m 1 1 -40 40) (m 2 2 -48 48) (m 3 2 -56 56))
                  (aref k (+ k-start 3)))))
  (values))

(declaim (inline kalyna-g256))
(defun kalyna-g256 (x y k k-start)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y k)
           (type (integer 0 76) k-start)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c)
               `(aref +kalyna-t+ ,a (logand (mod64ash (aref x ,b) ,c) #xff))))
    (setf (aref y 0)
          (logxor (aref k (+ k-start 0))
                  (m 0 0 0) (m 1 0 -8) (m 2 3 -16) (m 3 3 -24)
                  (m 4 2 -32) (m 5 2 -40) (m 6 1 -48) (m 7 1 -56)))
    (setf (aref y 1)
          (logxor (aref k (+ k-start 1))
                  (m 0 1 0) (m 1 1 -8) (m 2 0 -16) (m 3 0 -24)
                  (m 4 3 -32) (m 5 3 -40) (m 6 2 -48) (m 7 2 -56)))
    (setf (aref y 2)
          (logxor (aref k (+ k-start 2))
                  (m 0 2 0) (m 1 2 -8) (m 2 1 -16) (m 3 1 -24)
                  (m 4 0 -32) (m 5 0 -40) (m 6 3 -48) (m 7 3 -56)))
    (setf (aref y 3)
          (logxor (aref k (+ k-start 3))
                  (m 0 3 0) (m 1 3 -8) (m 2 2 -16) (m 3 2 -24)
                  (m 4 1 -32) (m 5 1 -40) (m 6 0 -48) (m 7 0 -56))))
  (values))

(defclass kalyna256 (cipher 32-byte-block-mixin)
  ((encryption-round-keys :accessor encryption-round-keys
                          :initform (make-array 76 :element-type '(unsigned-byte 64))
                          :type (simple-array (unsigned-byte 64) (76)))
   (decryption-round-keys :accessor decryption-round-keys
                          :initform (make-array 76 :element-type '(unsigned-byte 64))
                          :type (simple-array (unsigned-byte 64) (76)))
   (n-rounds :accessor n-rounds)))

(defmethod schedule-key ((cipher kalyna256) key)
  (let ((encryption-round-keys (encryption-round-keys cipher))
        (decryption-round-keys (decryption-round-keys cipher)))
    (declare (type (simple-array (unsigned-byte 64) (76)) encryption-round-keys)
             (type (simple-array (unsigned-byte 64) (76)) decryption-round-keys))
    (ecase (length key)
      (32
       (let ((key (make-array 4 :element-type '(unsigned-byte 64)
                                :initial-contents (list (ub64ref/le key 0)
                                                        (ub64ref/le key 8)
                                                        (ub64ref/le key 16)
                                                        (ub64ref/le key 24))))
             (ks (make-array 4 :element-type '(unsigned-byte 64)))
             (ksc (make-array 4 :element-type '(unsigned-byte 64)))
             (t1 (make-array 4 :element-type '(unsigned-byte 64)))
             (t2 (make-array 4 :element-type '(unsigned-byte 64)))
             (k (make-array 4 :element-type '(unsigned-byte 64)))
             (constant #x0001000100010001))
         (declare (type (simple-array (unsigned-byte 64) (4)) key ks ksc t1 t2 k)
                  (dynamic-extent key ks ksc t1 t2 k)
                  (type (unsigned-byte 64) constant))
         (setf (n-rounds cipher) 14)
         (fill t1 0)
         (setf (aref t1 0) (/ (+ 256 256 64) 64))
         (kalyna-add-key 4 t1 0 t2 key)
         (kalyna-g256 t2 t1 key 0)
         (kalyna-gl256 t1 t2 0 key 0)
         (kalyna-g0256 t2 ks)

         ;; Round 0
         (replace k key)
         (kalyna-add-constant 4 ks ksc constant)
         (kalyna-add-key 4 k 0 t2 ksc)
         (kalyna-g256 t2 t1 ksc 0)
         (kalyna-gl256 t1 encryption-round-keys 0 ksc 0)
         (kalyna-make-odd-key 4 encryption-round-keys 0 encryption-round-keys 4)

         ;; Rounds 2 to 13
         (flet ((r (n)
                  (kalyna-swap-blocks 4 k)
                  (setf constant (mod64ash constant 1))
                  (kalyna-add-constant 4 ks ksc constant)
                  (kalyna-add-key 4 k 0 t2 ksc)
                  (kalyna-g256 t2 t1 ksc 0)
                  (kalyna-gl256 t1 encryption-round-keys n ksc 0)
                  (kalyna-make-odd-key 4
                                       encryption-round-keys n
                                       encryption-round-keys (+ n 4))))
           (r 8)
           (r 16)
           (r 24)
           (r 32)
           (r 40)
           (r 48))

         ;; Round 14
         (kalyna-swap-blocks 4 k)
         (setf constant (mod64ash constant 1))
         (kalyna-add-constant 4 ks ksc constant)
         (kalyna-add-key 4 k 0 t2 ksc)
         (kalyna-g256 t2 t1 ksc 0)
         (kalyna-gl256 t1 encryption-round-keys 56 ksc 0)

         (replace decryption-round-keys encryption-round-keys)
         (loop for n from 52 downto 4 by 4 do
           (kalyna-imc256 decryption-round-keys n))))

      (64
       (let ((key (make-array 8 :element-type '(unsigned-byte 64)
                                :initial-contents (list (ub64ref/le key 0)
                                                        (ub64ref/le key 8)
                                                        (ub64ref/le key 16)
                                                        (ub64ref/le key 24)
                                                        (ub64ref/le key 32)
                                                        (ub64ref/le key 40)
                                                        (ub64ref/le key 48)
                                                        (ub64ref/le key 56))))
             (ks (make-array 4 :element-type '(unsigned-byte 64)))
             (ksc (make-array 4 :element-type '(unsigned-byte 64)))
             (t1 (make-array 4 :element-type '(unsigned-byte 64)))
             (t2 (make-array 4 :element-type '(unsigned-byte 64)))
             (k (make-array 8 :element-type '(unsigned-byte 64)))
             (ko (make-array 4 :element-type '(unsigned-byte 64)))
             (ka (make-array 4 :element-type '(unsigned-byte 64)))
             (constant #x0001000100010001))
         (declare (type (simple-array (unsigned-byte 64) (8)) key k)
                  (type (simple-array (unsigned-byte 64) (4)) ks ksc t1 t2 ko ka)
                  (dynamic-extent key ks ksc t1 t2 k ko ka)
                  (type (unsigned-byte 64) constant))
         (setf (n-rounds cipher) 18)
         (fill t1 0)
         (setf (aref t1 0) (/ (+ 512 256 64) 64))
         (replace ka key :end2 4)
         (replace ko key :start2 4)
         (kalyna-add-key 4 t1 0 t2 ka)
         (kalyna-g256 t2 t1 ko 0)
         (kalyna-gl256 t1 t2 0 ka 0)
         (kalyna-g0256 t2 ks)

         ;; Round 0
         (replace k key)
         (kalyna-add-constant 4 ks ksc constant)
         (kalyna-add-key 4 k 0 t2 ksc)
         (kalyna-g256 t2 t1 ksc 0)
         (kalyna-gl256 t1 encryption-round-keys 0 ksc 0)
         (kalyna-make-odd-key 4 encryption-round-keys 0 encryption-round-keys 4)

         ;; Rounds 2 to 17
         (flet ((r (v n)
                  (when (zerop v)
                    (kalyna-swap-blocks 8 k))
                  (setf constant (mod64ash constant 1))
                  (kalyna-add-constant 4 ks ksc constant)
                  (kalyna-add-key 4 k v t2 ksc)
                  (kalyna-g256 t2 t1 ksc 0)
                  (kalyna-gl256 t1 encryption-round-keys n ksc 0)
                  (kalyna-make-odd-key 4
                                       encryption-round-keys n
                                       encryption-round-keys (+ n 4))))
           (r 4 8)
           (r 0 16)
           (r 4 24)
           (r 0 32)
           (r 4 40)
           (r 0 48)
           (r 4 56)
           (r 0 64))

         ;; Round 18
         (setf constant (mod64ash constant 1))
         (kalyna-add-constant 4 ks ksc constant)
         (kalyna-add-key 4 k 4 t2 ksc)
         (kalyna-g256 t2 t1 ksc 0)
         (kalyna-gl256 t1 encryption-round-keys 72 ksc 0)

         (replace decryption-round-keys encryption-round-keys)
         (loop for n from 68 downto 4 by 4 do
           (kalyna-imc256 decryption-round-keys n)))))
    cipher))

(define-block-encryptor kalyna256 32
  (let ((encryption-round-keys (encryption-round-keys context))
        (t1 (make-array 4 :element-type '(unsigned-byte 64)))
        (t2 (make-array 4 :element-type '(unsigned-byte 64))))
    (declare (type (simple-array (unsigned-byte 64) (76)) encryption-round-keys)
             (type (simple-array (unsigned-byte 64) (4)) t1 t2)
             (dynamic-extent t1 t2))
    (setf (aref t2 0) (ub64ref/le plaintext plaintext-start)
          (aref t2 1) (ub64ref/le plaintext (+ plaintext-start 8))
          (aref t2 2) (ub64ref/le plaintext (+ plaintext-start 16))
          (aref t2 3) (ub64ref/le plaintext (+ plaintext-start 24)))
    (kalyna-add-key 4 t2 0 t1 encryption-round-keys)
    (kalyna-g256 t1 t2 encryption-round-keys 4)
    (kalyna-g256 t2 t1 encryption-round-keys 8)
    (kalyna-g256 t1 t2 encryption-round-keys 12)
    (kalyna-g256 t2 t1 encryption-round-keys 16)
    (kalyna-g256 t1 t2 encryption-round-keys 20)
    (kalyna-g256 t2 t1 encryption-round-keys 24)
    (kalyna-g256 t1 t2 encryption-round-keys 28)
    (kalyna-g256 t2 t1 encryption-round-keys 32)
    (kalyna-g256 t1 t2 encryption-round-keys 36)
    (kalyna-g256 t2 t1 encryption-round-keys 40)
    (kalyna-g256 t1 t2 encryption-round-keys 44)
    (kalyna-g256 t2 t1 encryption-round-keys 48)
    (kalyna-g256 t1 t2 encryption-round-keys 52)
    (ecase (n-rounds context)
      (14
       (kalyna-gl256 t2 t1 0 encryption-round-keys 56))
      (18
       (kalyna-g256 t2 t1 encryption-round-keys 56)
       (kalyna-g256 t1 t2 encryption-round-keys 60)
       (kalyna-g256 t2 t1 encryption-round-keys 64)
       (kalyna-g256 t1 t2 encryption-round-keys 68)
       (kalyna-gl256 t2 t1 0 encryption-round-keys 72)))
    (setf (ub64ref/le ciphertext ciphertext-start) (aref t1 0)
          (ub64ref/le ciphertext (+ ciphertext-start 8)) (aref t1 1)
          (ub64ref/le ciphertext (+ ciphertext-start 16)) (aref t1 2)
          (ub64ref/le ciphertext (+ ciphertext-start 24)) (aref t1 3))
    (values)))

(define-block-decryptor kalyna256 32
  (let ((decryption-round-keys (decryption-round-keys context))
        (t1 (make-array 4 :element-type '(unsigned-byte 64)))
        (t2 (make-array 4 :element-type '(unsigned-byte 64))))
    (declare (type (simple-array (unsigned-byte 64) (76)) decryption-round-keys)
             (type (simple-array (unsigned-byte 64) (4)) t1 t2)
             (dynamic-extent t1 t2))
    (setf (aref t2 0) (ub64ref/le ciphertext ciphertext-start)
          (aref t2 1) (ub64ref/le ciphertext (+ ciphertext-start 8))
          (aref t2 2) (ub64ref/le ciphertext (+ ciphertext-start 16))
          (aref t2 3) (ub64ref/le ciphertext (+ ciphertext-start 24)))
    (ecase (n-rounds context)
      (14
       (kalyna-sub-key 4 t2 t1 decryption-round-keys 56)
       (kalyna-imc256 t1 0))
      (18
       (kalyna-sub-key 4 t2 t1 decryption-round-keys 72)
       (kalyna-imc256 t1 0)
       (kalyna-ig256 t1 t2 decryption-round-keys 68)
       (kalyna-ig256 t2 t1 decryption-round-keys 64)
       (kalyna-ig256 t1 t2 decryption-round-keys 60)
       (kalyna-ig256 t2 t1 decryption-round-keys 56)))
    (kalyna-ig256 t1 t2 decryption-round-keys 52)
    (kalyna-ig256 t2 t1 decryption-round-keys 48)
    (kalyna-ig256 t1 t2 decryption-round-keys 44)
    (kalyna-ig256 t2 t1 decryption-round-keys 40)
    (kalyna-ig256 t1 t2 decryption-round-keys 36)
    (kalyna-ig256 t2 t1 decryption-round-keys 32)
    (kalyna-ig256 t1 t2 decryption-round-keys 28)
    (kalyna-ig256 t2 t1 decryption-round-keys 24)
    (kalyna-ig256 t1 t2 decryption-round-keys 20)
    (kalyna-ig256 t2 t1 decryption-round-keys 16)
    (kalyna-ig256 t1 t2 decryption-round-keys 12)
    (kalyna-ig256 t2 t1 decryption-round-keys 8)
    (kalyna-ig256 t1 t2 decryption-round-keys 4)
    (kalyna-igl256 t2 t1 decryption-round-keys 0)
    (setf (ub64ref/le plaintext plaintext-start) (aref t1 0)
          (ub64ref/le plaintext (+ plaintext-start 8)) (aref t1 1)
          (ub64ref/le plaintext (+ plaintext-start 16)) (aref t1 2)
          (ub64ref/le plaintext (+ plaintext-start 24)) (aref t1 3))
    (values)))

(defcipher kalyna256
  (:encrypt-function kalyna256-encrypt-block)
  (:decrypt-function kalyna256-decrypt-block)
  (:block-length 32)
  (:key-length (:fixed 32 64)))


;;;
;;; Kalyna512
;;;

(declaim (inline kalyna-g0512))
(defun kalyna-g0512 (x y)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c)
               `(aref +kalyna-t+ ,a (logand (mod64ash (aref x ,b) ,c) #xff))))
    (setf (aref y 0)
          (logxor (m 0 0 0) (m 1 7 -8) (m 2 6 -16) (m 3 5 -24)
                  (m 4 4 -32) (m 5 3 -40) (m 6 2 -48) (m 7 1 -56)))
    (setf (aref y 1)
          (logxor (m 0 1 0) (m 1 0 -8) (m 2 7 -16) (m 3 6 -24)
                  (m 4 5 -32) (m 5 4 -40) (m 6 3 -48) (m 7 2 -56)))
    (setf (aref y 2)
          (logxor (m 0 2 0) (m 1 1 -8) (m 2 0 -16) (m 3 7 -24)
                  (m 4 6 -32) (m 5 5 -40) (m 6 4 -48) (m 7 3 -56)))
    (setf (aref y 3)
          (logxor (m 0 3 0) (m 1 2 -8) (m 2 1 -16) (m 3 0 -24)
                  (m 4 7 -32) (m 5 6 -40) (m 6 5 -48) (m 7 4 -56)))
    (setf (aref y 4)
          (logxor (m 0 4 0) (m 1 3 -8) (m 2 2 -16) (m 3 1 -24)
                  (m 4 0 -32) (m 5 7 -40) (m 6 6 -48) (m 7 5 -56)))
    (setf (aref y 5)
          (logxor (m 0 5 0) (m 1 4 -8) (m 2 3 -16) (m 3 2 -24)
                  (m 4 1 -32) (m 5 0 -40) (m 6 7 -48) (m 7 6 -56)))
    (setf (aref y 6)
          (logxor (m 0 6 0) (m 1 5 -8) (m 2 4 -16) (m 3 3 -24)
                  (m 4 2 -32) (m 5 1 -40) (m 6 0 -48) (m 7 7 -56)))
    (setf (aref y 7)
          (logxor (m 0 7 0) (m 1 6 -8) (m 2 5 -16) (m 3 4 -24)
                  (m 4 3 -32) (m 5 2 -40) (m 6 1 -48) (m 7 0 -56))))
  (values))

(declaim (inline kalyna-gl512))
(defun kalyna-gl512 (x y y-start k k-start)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y k)
           (type (integer 0 152) y-start k-start)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c)
               `(aref +kalyna-t+ ,a (logand (mod64ash (aref x ,b) ,c) #xff))))
    (setf (aref y (+ y-start 0))
          (mod64+ (aref k (+ k-start 0))
                  (logxor (m 0 0 0) (m 1 7 -8) (m 2 6 -16) (m 3 5 -24)
                          (m 4 4 -32) (m 5 3 -40) (m 6 2 -48) (m 7 1 -56))))
    (setf (aref y (+ y-start 1))
          (mod64+ (aref k (+ k-start 1))
                  (logxor (m 0 1 0) (m 1 0 -8) (m 2 7 -16) (m 3 6 -24)
                          (m 4 5 -32) (m 5 4 -40) (m 6 3 -48) (m 7 2 -56))))
    (setf (aref y (+ y-start 2))
          (mod64+ (aref k (+ k-start 2))
                  (logxor (m 0 2 0) (m 1 1 -8) (m 2 0 -16) (m 3 7 -24)
                          (m 4 6 -32) (m 5 5 -40) (m 6 4 -48) (m 7 3 -56))))
    (setf (aref y (+ y-start 3))
          (mod64+ (aref k (+ k-start 3))
                  (logxor (m 0 3 0) (m 1 2 -8) (m 2 1 -16) (m 3 0 -24)
                          (m 4 7 -32) (m 5 6 -40) (m 6 5 -48) (m 7 4 -56))))
    (setf (aref y (+ y-start 4))
          (mod64+ (aref k (+ k-start 4))
                  (logxor (m 0 4 0) (m 1 3 -8) (m 2 2 -16) (m 3 1 -24)
                          (m 4 0 -32) (m 5 7 -40) (m 6 6 -48) (m 7 5 -56))))
    (setf (aref y (+ y-start 5))
          (mod64+ (aref k (+ k-start 5))
                  (logxor (m 0 5 0) (m 1 4 -8) (m 2 3 -16) (m 3 2 -24)
                          (m 4 1 -32) (m 5 0 -40) (m 6 7 -48) (m 7 6 -56))))
    (setf (aref y (+ y-start 6))
          (mod64+ (aref k (+ k-start 6))
                  (logxor (m 0 6 0) (m 1 5 -8) (m 2 4 -16) (m 3 3 -24)
                          (m 4 2 -32) (m 5 1 -40) (m 6 0 -48) (m 7 7 -56))))
    (setf (aref y (+ y-start 7))
          (mod64+ (aref k (+ k-start 7))
                  (logxor (m 0 7 0) (m 1 6 -8) (m 2 5 -16) (m 3 4 -24)
                          (m 4 3 -32) (m 5 2 -40) (m 6 1 -48) (m 7 0 -56)))))
  (values))

(declaim (inline kalyna-imc512))
(defun kalyna-imc512 (x x-start)
  (declare (type (simple-array (unsigned-byte 64) (*)) x)
           (type (integer 0 152) x-start)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c d)
               `(aref +kalyna-it+
                      ,a
                      (aref +kalyna-s+
                            ,b
                            (logand (mod64ash (aref x (+ x-start ,c)) ,d) #xff)))))
    (setf (aref x (+ x-start 0))
          (logxor (m 0 0 0 0) (m 1 1 0 -8) (m 2 2 0 -16) (m 3 3 0 -24)
                  (m 4 0 0 -32) (m 5 1 0 -40) (m 6 2 0 -48) (m 7 3 0 -56)))
    (setf (aref x (+ x-start 1))
          (logxor (m 0 0 1 0) (m 1 1 1 -8) (m 2 2 1 -16) (m 3 3 1 -24)
                  (m 4 0 1 -32) (m 5 1 1 -40) (m 6 2 1 -48) (m 7 3 1 -56)))
    (setf (aref x (+ x-start 2))
          (logxor (m 0 0 2 0) (m 1 1 2 -8) (m 2 2 2 -16) (m 3 3 2 -24)
                  (m 4 0 2 -32) (m 5 1 2 -40) (m 6 2 2 -48) (m 7 3 2 -56)))
    (setf (aref x (+ x-start 3))
          (logxor (m 0 0 3 0) (m 1 1 3 -8) (m 2 2 3 -16) (m 3 3 3 -24)
                  (m 4 0 3 -32) (m 5 1 3 -40) (m 6 2 3 -48) (m 7 3 3 -56)))
    (setf (aref x (+ x-start 4))
          (logxor (m 0 0 4 0) (m 1 1 4 -8) (m 2 2 4 -16) (m 3 3 4 -24)
                  (m 4 0 4 -32) (m 5 1 4 -40) (m 6 2 4 -48) (m 7 3 4 -56)))
    (setf (aref x (+ x-start 5))
          (logxor (m 0 0 5 0) (m 1 1 5 -8) (m 2 2 5 -16) (m 3 3 5 -24)
                  (m 4 0 5 -32) (m 5 1 5 -40) (m 6 2 5 -48) (m 7 3 5 -56)))
    (setf (aref x (+ x-start 6))
          (logxor (m 0 0 6 0) (m 1 1 6 -8) (m 2 2 6 -16) (m 3 3 6 -24)
                  (m 4 0 6 -32) (m 5 1 6 -40) (m 6 2 6 -48) (m 7 3 6 -56)))
    (setf (aref x (+ x-start 7))
          (logxor (m 0 0 7 0) (m 1 1 7 -8) (m 2 2 7 -16) (m 3 3 7 -24)
                  (m 4 0 7 -32) (m 5 1 7 -40) (m 6 2 7 -48) (m 7 3 7 -56))))
  (values))

(declaim (inline kalyna-ig512))
(defun kalyna-ig512 (x y k k-start)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y k)
           (type (integer 0 152) k-start)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c)
               `(aref +kalyna-it+ ,a (logand (mod64ash (aref x ,b) ,c) #xff))))
    (setf (aref y 0)
          (logxor (aref k (+ k-start 0))
                  (m 0 0 0) (m 1 1 -8) (m 2 2 -16) (m 3 3 -24)
                  (m 4 4 -32) (m 5 5 -40) (m 6 6 -48) (m 7 7 -56)))
    (setf (aref y 1)
          (logxor (aref k (+ k-start 1))
                  (m 0 1 0) (m 1 2 -8) (m 2 3 -16) (m 3 4 -24)
                  (m 4 5 -32) (m 5 6 -40) (m 6 7 -48) (m 7 0 -56)))
    (setf (aref y 2)
          (logxor (aref k (+ k-start 2))
                  (m 0 2 0) (m 1 3 -8) (m 2 4 -16) (m 3 5 -24)
                  (m 4 6 -32) (m 5 7 -40) (m 6 0 -48) (m 7 1 -56)))
    (setf (aref y 3)
          (logxor (aref k (+ k-start 3))
                  (m 0 3 0) (m 1 4 -8) (m 2 5 -16) (m 3 6 -24)
                  (m 4 7 -32) (m 5 0 -40) (m 6 1 -48) (m 7 2 -56)))
    (setf (aref y 4)
          (logxor (aref k (+ k-start 4))
                  (m 0 4 0) (m 1 5 -8) (m 2 6 -16) (m 3 7 -24)
                  (m 4 0 -32) (m 5 1 -40) (m 6 2 -48) (m 7 3 -56)))
    (setf (aref y 5)
          (logxor (aref k (+ k-start 5))
                  (m 0 5 0) (m 1 6 -8) (m 2 7 -16) (m 3 0 -24)
                  (m 4 1 -32) (m 5 2 -40) (m 6 3 -48) (m 7 4 -56)))
    (setf (aref y 6)
          (logxor (aref k (+ k-start 6))
                  (m 0 6 0) (m 1 7 -8) (m 2 0 -16) (m 3 1 -24)
                  (m 4 2 -32) (m 5 3 -40) (m 6 4 -48) (m 7 5 -56)))
    (setf (aref y 7)
          (logxor (aref k (+ k-start 7))
                  (m 0 7 0) (m 1 0 -8) (m 2 1 -16) (m 3 2 -24)
                  (m 4 3 -32) (m 5 4 -40) (m 6 5 -48) (m 7 6 -56))))
  (values))

(declaim (inline kalyna-igl512))
(defun kalyna-igl512 (x y k k-start)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y k)
           (type (integer 0 152) k-start)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c d)
               `(mod64ash (aref +kalyna-is+
                                ,a
                                (logand (mod64ash (aref x ,b) ,c) #xff))
                          ,d)))
    (setf (aref y 0)
          (mod64- (logxor (m 0 0 0 0) (m 1 1 -8 8) (m 2 2 -16 16) (m 3 3 -24 24)
                          (m 0 4 -32 32) (m 1 5 -40 40) (m 2 6 -48 48) (m 3 7 -56 56))
                  (aref k (+ k-start 0))))
    (setf (aref y 1)
          (mod64- (logxor (m 0 1 0 0) (m 1 2 -8 8) (m 2 3 -16 16) (m 3 4 -24 24)
                          (m 0 5 -32 32) (m 1 6 -40 40) (m 2 7 -48 48) (m 3 0 -56 56))
                  (aref k (+ k-start 1))))
    (setf (aref y 2)
          (mod64- (logxor (m 0 2 0 0) (m 1 3 -8 8) (m 2 4 -16 16) (m 3 5 -24 24)
                          (m 0 6 -32 32) (m 1 7 -40 40) (m 2 0 -48 48) (m 3 1 -56 56))
                  (aref k (+ k-start 2))))
    (setf (aref y 3)
          (mod64- (logxor (m 0 3 0 0) (m 1 4 -8 8) (m 2 5 -16 16) (m 3 6 -24 24)
                          (m 0 7 -32 32) (m 1 0 -40 40) (m 2 1 -48 48) (m 3 2 -56 56))
                  (aref k (+ k-start 3))))
    (setf (aref y 4)
          (mod64- (logxor (m 0 4 0 0) (m 1 5 -8 8) (m 2 6 -16 16) (m 3 7 -24 24)
                          (m 0 0 -32 32) (m 1 1 -40 40) (m 2 2 -48 48) (m 3 3 -56 56))
                  (aref k (+ k-start 4))))
    (setf (aref y 5)
          (mod64- (logxor (m 0 5 0 0) (m 1 6 -8 8) (m 2 7 -16 16) (m 3 0 -24 24)
                          (m 0 1 -32 32) (m 1 2 -40 40) (m 2 3 -48 48) (m 3 4 -56 56))
                  (aref k (+ k-start 5))))
    (setf (aref y 6)
          (mod64- (logxor (m 0 6 0 0) (m 1 7 -8 8) (m 2 0 -16 16) (m 3 1 -24 24)
                          (m 0 2 -32 32) (m 1 3 -40 40) (m 2 4 -48 48) (m 3 5 -56 56))
                  (aref k (+ k-start 6))))
    (setf (aref y 7)
          (mod64- (logxor (m 0 7 0 0) (m 1 0 -8 8) (m 2 1 -16 16) (m 3 2 -24 24)
                          (m 0 3 -32 32) (m 1 4 -40 40) (m 2 5 -48 48) (m 3 6 -56 56))
                  (aref k (+ k-start 7)))))
  (values))

(declaim (inline kalyna-g512))
(defun kalyna-g512 (x y k k-start)
  (declare (type (simple-array (unsigned-byte 64) (*)) x y k)
           (type (integer 0 152) k-start)
           (optimize (speed 3) (space 0) (debug 0) (safety 0)))
  (macrolet ((m (a b c)
               `(aref +kalyna-t+ ,a (logand (mod64ash (aref x ,b) ,c) #xff))))
    (setf (aref y 0)
          (logxor (aref k (+ k-start 0))
                  (m 0 0 0) (m 1 7 -8) (m 2 6 -16) (m 3 5 -24)
                  (m 4 4 -32) (m 5 3 -40) (m 6 2 -48) (m 7 1 -56)))
    (setf (aref y 1)
          (logxor (aref k (+ k-start 1))
                  (m 0 1 0) (m 1 0 -8) (m 2 7 -16) (m 3 6 -24)
                  (m 4 5 -32) (m 5 4 -40) (m 6 3 -48) (m 7 2 -56)))
    (setf (aref y 2)
          (logxor (aref k (+ k-start 2))
                  (m 0 2 0) (m 1 1 -8) (m 2 0 -16) (m 3 7 -24)
                  (m 4 6 -32) (m 5 5 -40) (m 6 4 -48) (m 7 3 -56)))
    (setf (aref y 3)
          (logxor (aref k (+ k-start 3))
                  (m 0 3 0) (m 1 2 -8) (m 2 1 -16) (m 3 0 -24)
                  (m 4 7 -32) (m 5 6 -40) (m 6 5 -48) (m 7 4 -56)))
    (setf (aref y 4)
          (logxor (aref k (+ k-start 4))
                  (m 0 4 0) (m 1 3 -8) (m 2 2 -16) (m 3 1 -24)
                  (m 4 0 -32) (m 5 7 -40) (m 6 6 -48) (m 7 5 -56)))
    (setf (aref y 5)
          (logxor (aref k (+ k-start 5))
                  (m 0 5 0) (m 1 4 -8) (m 2 3 -16) (m 3 2 -24)
                  (m 4 1 -32) (m 5 0 -40) (m 6 7 -48) (m 7 6 -56)))
    (setf (aref y 6)
          (logxor (aref k (+ k-start 6))
                  (m 0 6 0) (m 1 5 -8) (m 2 4 -16) (m 3 3 -24)
                  (m 4 2 -32) (m 5 1 -40) (m 6 0 -48) (m 7 7 -56)))
    (setf (aref y 7)
          (logxor (aref k (+ k-start 7))
                  (m 0 7 0) (m 1 6 -8) (m 2 5 -16) (m 3 4 -24)
                  (m 4 3 -32) (m 5 2 -40) (m 6 1 -48) (m 7 0 -56))))
  (values))

(defclass kalyna512 (cipher 64-byte-block-mixin)
  ((encryption-round-keys :accessor encryption-round-keys
                          :initform (make-array 152 :element-type '(unsigned-byte 64))
                          :type (simple-array (unsigned-byte 64) (152)))
   (decryption-round-keys :accessor decryption-round-keys
                          :initform (make-array 152 :element-type '(unsigned-byte 64))
                          :type (simple-array (unsigned-byte 64) (152)))
   (n-rounds :accessor n-rounds)))

(defmethod schedule-key ((cipher kalyna512) key)
  (let ((encryption-round-keys (encryption-round-keys cipher))
        (decryption-round-keys (decryption-round-keys cipher))
        (key (make-array 8 :element-type '(unsigned-byte 64)
                           :initial-contents (list (ub64ref/le key 0)
                                                   (ub64ref/le key 8)
                                                   (ub64ref/le key 16)
                                                   (ub64ref/le key 24)
                                                   (ub64ref/le key 32)
                                                   (ub64ref/le key 40)
                                                   (ub64ref/le key 48)
                                                   (ub64ref/le key 56))))
        (ks (make-array 8 :element-type '(unsigned-byte 64)))
        (ksc (make-array 8 :element-type '(unsigned-byte 64)))
        (t1 (make-array 8 :element-type '(unsigned-byte 64)))
        (t2 (make-array 8 :element-type '(unsigned-byte 64)))
        (k (make-array 8 :element-type '(unsigned-byte 64)))
        (constant #x0001000100010001))
    (declare (type (simple-array (unsigned-byte 64) (152)) encryption-round-keys)
             (type (simple-array (unsigned-byte 64) (152)) decryption-round-keys)
             (type (simple-array (unsigned-byte 64) (8)) key ks ksc t1 t2 k)
             (dynamic-extent key ks ksc t1 t2 k)
             (type (unsigned-byte 64) constant))
    (setf (n-rounds cipher) 18)
    (fill t1 0)
    (setf (aref t1 0) (/ (+ 512 512 64) 64))
    (kalyna-add-key 8 t1 0 t2 key)
    (kalyna-g512 t2 t1 key 0)
    (kalyna-gl512 t1 t2 0 key 0)
    (kalyna-g0512 t2 ks)

    ;; Round 0
    (replace k key)
    (kalyna-add-constant 8 ks ksc constant)
    (kalyna-add-key 8 k 0 t2 ksc)
    (kalyna-g512 t2 t1 ksc 0)
    (kalyna-gl512 t1 encryption-round-keys 0 ksc 0)
    (kalyna-make-odd-key 8 encryption-round-keys 0 encryption-round-keys 8)

    ;; Rounds 2 to 17
    (flet ((r (n)
             (kalyna-swap-blocks 8 k)
             (setf constant (mod64ash constant 1))
             (kalyna-add-constant 8 ks ksc constant)
             (kalyna-add-key 8 k 0 t2 ksc)
             (kalyna-g512 t2 t1 ksc 0)
             (kalyna-gl512 t1 encryption-round-keys n ksc 0)
             (kalyna-make-odd-key 8
                                  encryption-round-keys n
                                  encryption-round-keys (+ n 8))))
      (r 16)
      (r 32)
      (r 48)
      (r 64)
      (r 80)
      (r 96)
      (r 112)
      (r 128))

    ;; Round 18
    (kalyna-swap-blocks 8 k)
    (setf constant (mod64ash constant 1))
    (kalyna-add-constant 8 ks ksc constant)
    (kalyna-add-key 8 k 0 t2 ksc)
    (kalyna-g512 t2 t1 ksc 0)
    (kalyna-gl512 t1 encryption-round-keys 144 ksc 0)

    (replace decryption-round-keys encryption-round-keys)
    (loop for n from 136 downto 8 by 8 do
      (kalyna-imc512 decryption-round-keys n))
    cipher))

(define-block-encryptor kalyna512 64
  (let ((encryption-round-keys (encryption-round-keys context))
        (t1 (make-array 8 :element-type '(unsigned-byte 64)))
        (t2 (make-array 8 :element-type '(unsigned-byte 64))))
    (declare (type (simple-array (unsigned-byte 64) (152)) encryption-round-keys)
             (type (simple-array (unsigned-byte 64) (8)) t1 t2)
             (dynamic-extent t1 t2))
    (setf (aref t2 0) (ub64ref/le plaintext plaintext-start)
          (aref t2 1) (ub64ref/le plaintext (+ plaintext-start 8))
          (aref t2 2) (ub64ref/le plaintext (+ plaintext-start 16))
          (aref t2 3) (ub64ref/le plaintext (+ plaintext-start 24))
          (aref t2 4) (ub64ref/le plaintext (+ plaintext-start 32))
          (aref t2 5) (ub64ref/le plaintext (+ plaintext-start 40))
          (aref t2 6) (ub64ref/le plaintext (+ plaintext-start 48))
          (aref t2 7) (ub64ref/le plaintext (+ plaintext-start 56)))
    (kalyna-add-key 8 t2 0 t1 encryption-round-keys)
    (kalyna-g512 t1 t2 encryption-round-keys 8)
    (kalyna-g512 t2 t1 encryption-round-keys 16)
    (kalyna-g512 t1 t2 encryption-round-keys 24)
    (kalyna-g512 t2 t1 encryption-round-keys 32)
    (kalyna-g512 t1 t2 encryption-round-keys 40)
    (kalyna-g512 t2 t1 encryption-round-keys 48)
    (kalyna-g512 t1 t2 encryption-round-keys 56)
    (kalyna-g512 t2 t1 encryption-round-keys 64)
    (kalyna-g512 t1 t2 encryption-round-keys 72)
    (kalyna-g512 t2 t1 encryption-round-keys 80)
    (kalyna-g512 t1 t2 encryption-round-keys 88)
    (kalyna-g512 t2 t1 encryption-round-keys 96)
    (kalyna-g512 t1 t2 encryption-round-keys 104)
    (kalyna-g512 t2 t1 encryption-round-keys 112)
    (kalyna-g512 t1 t2 encryption-round-keys 120)
    (kalyna-g512 t2 t1 encryption-round-keys 128)
    (kalyna-g512 t1 t2 encryption-round-keys 136)
    (kalyna-gl512 t2 t1 0 encryption-round-keys 144)
    (setf (ub64ref/le ciphertext ciphertext-start) (aref t1 0)
          (ub64ref/le ciphertext (+ ciphertext-start 8)) (aref t1 1)
          (ub64ref/le ciphertext (+ ciphertext-start 16)) (aref t1 2)
          (ub64ref/le ciphertext (+ ciphertext-start 24)) (aref t1 3)
          (ub64ref/le ciphertext (+ ciphertext-start 32)) (aref t1 4)
          (ub64ref/le ciphertext (+ ciphertext-start 40)) (aref t1 5)
          (ub64ref/le ciphertext (+ ciphertext-start 48)) (aref t1 6)
          (ub64ref/le ciphertext (+ ciphertext-start 56)) (aref t1 7))
    (values)))

(define-block-decryptor kalyna512 64
  (let ((decryption-round-keys (decryption-round-keys context))
        (t1 (make-array 8 :element-type '(unsigned-byte 64)))
        (t2 (make-array 8 :element-type '(unsigned-byte 64))))
    (declare (type (simple-array (unsigned-byte 64) (152)) decryption-round-keys)
             (type (simple-array (unsigned-byte 64) (8)) t1 t2)
             (dynamic-extent t1 t2))
    (setf (aref t2 0) (ub64ref/le ciphertext ciphertext-start)
          (aref t2 1) (ub64ref/le ciphertext (+ ciphertext-start 8))
          (aref t2 2) (ub64ref/le ciphertext (+ ciphertext-start 16))
          (aref t2 3) (ub64ref/le ciphertext (+ ciphertext-start 24))
          (aref t2 4) (ub64ref/le ciphertext (+ ciphertext-start 32))
          (aref t2 5) (ub64ref/le ciphertext (+ ciphertext-start 40))
          (aref t2 6) (ub64ref/le ciphertext (+ ciphertext-start 48))
          (aref t2 7) (ub64ref/le ciphertext (+ ciphertext-start 56)))
    (kalyna-sub-key 8 t2 t1 decryption-round-keys 144)
    (kalyna-imc512 t1 0)
    (kalyna-ig512 t1 t2 decryption-round-keys 136)
    (kalyna-ig512 t2 t1 decryption-round-keys 128)
    (kalyna-ig512 t1 t2 decryption-round-keys 120)
    (kalyna-ig512 t2 t1 decryption-round-keys 112)
    (kalyna-ig512 t1 t2 decryption-round-keys 104)
    (kalyna-ig512 t2 t1 decryption-round-keys 96)
    (kalyna-ig512 t1 t2 decryption-round-keys 88)
    (kalyna-ig512 t2 t1 decryption-round-keys 80)
    (kalyna-ig512 t1 t2 decryption-round-keys 72)
    (kalyna-ig512 t2 t1 decryption-round-keys 64)
    (kalyna-ig512 t1 t2 decryption-round-keys 56)
    (kalyna-ig512 t2 t1 decryption-round-keys 48)
    (kalyna-ig512 t1 t2 decryption-round-keys 40)
    (kalyna-ig512 t2 t1 decryption-round-keys 32)
    (kalyna-ig512 t1 t2 decryption-round-keys 24)
    (kalyna-ig512 t2 t1 decryption-round-keys 16)
    (kalyna-ig512 t1 t2 decryption-round-keys 8)
    (kalyna-igl512 t2 t1 decryption-round-keys 0)
    (setf (ub64ref/le plaintext plaintext-start) (aref t1 0)
          (ub64ref/le plaintext (+ plaintext-start 8)) (aref t1 1)
          (ub64ref/le plaintext (+ plaintext-start 16)) (aref t1 2)
          (ub64ref/le plaintext (+ plaintext-start 24)) (aref t1 3)
          (ub64ref/le plaintext (+ plaintext-start 32)) (aref t1 4)
          (ub64ref/le plaintext (+ plaintext-start 40)) (aref t1 5)
          (ub64ref/le plaintext (+ plaintext-start 48)) (aref t1 6)
          (ub64ref/le plaintext (+ plaintext-start 56)) (aref t1 7))
    (values)))

(defcipher kalyna512
  (:encrypt-function kalyna512-encrypt-block)
  (:decrypt-function kalyna512-decrypt-block)
  (:block-length 64)
  (:key-length (:fixed 64)))
