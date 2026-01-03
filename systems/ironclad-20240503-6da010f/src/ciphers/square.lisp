;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; square.lisp -- implementation of the Square block cipher

;;; based on a public domain implementation by Paulo Baretto (FIXME!)

(in-package :crypto)
(in-ironclad-readtable)

(declaim (type (simple-array (unsigned-byte 8) (256))
               alogtable logtable))
(eval-when (:compile-toplevel :load-toplevel :execute)
(defconst alogtable
    #.(let ((table (make-array 256 :element-type '(unsigned-byte 8)
                               :initial-element 1)))
        (do ((i 1 (1+ i)))
            ((>= i 256) table)
          (let ((j (ash (aref table (1- i)) 1)))
            (when (logbitp 8 j)
              (setf j (logxor j #x1f5)))
            (setf (aref table i) (logand j #xff))))))
)

(defconst logtable
    #.(let ((table (make-array 256 :element-type '(unsigned-byte 8)
                               :initial-element 0)))
        (do ((i 1 (1+ i)))
            ((>= i 256) (setf (aref table 1) 0) table)
          (setf (aref table (aref alogtable i)) i))))

(declaim (type (simple-array (unsigned-byte 8) (4 4))
               g-matrix inverse-g-matrix))
(defconst g-matrix (make-array (list 4 4) :element-type '(unsigned-byte 8)
                               :initial-contents
                               (list (list 2 1 1 3)
                                     (list 3 2 1 1)
                                     (list 1 3 2 1)
                                     (list 1 1 3 2))))
(defconst inverse-g-matrix (make-array (list 4 4) :element-type '(unsigned-byte 8)
                                       :initial-contents
                                       (list (list #xe #x9 #xd #xb)
                                             (list #xb #xe #x9 #xd)
                                             (list #xd #xb #xe #x9)
                                             (list #x9 #xd #xb #xe))))

(declaim (type (simple-array (unsigned-byte 8) (256))
               s-encryption-table s-decryption-table))
(defconst s-encryption-table
#8@(177 206 195 149  90 173 231   2  77  68 251 145  12 135 161  80 
203 103  84 221  70 143 225  78 240 253 252 235 249 196  26 110 
 94 245 204 141  28  86  67 254   7  97 248 117  89 255   3  34 
138 209  19 238 136   0  14  52  21 128 148 227 237 181  83  35 
 75  71  23 167 144  53 171 216 184 223  79  87 154 146 219  27 
 60 200 153   4 142 224 215 125 133 187  64  44  58  69 241  66 
101  32  65  24 114  37 147 112  54   5 242  11 163 121 236   8 
 39  49  50 182 124 176  10 115  91 123 183 129 210  13 106  38 
158  88 156 131 116 179 172  48 122 105 119  15 174  33 222 208 
 46 151  16 164 152 168 212 104  45  98  41 109  22  73 118 199 
232 193 150  55 229 202 244 233  99  18 194 166  20 188 211  40 
175  47 230  36  82 198 160   9 189 140 207  93  17  95   1 197 
159  61 162 155 201  59 190  81  25  31  63  92 178 239  74 205 
191 186 111 100 217 243  62 180 170 220 213   6 192 126 246 102 
108 132 113  56 185  29 127 157  72 139  42 218 165  51 130  57 
214 120 134 250 228  43 169  30 137  96 107 234  85  76 247 226))

(defconst s-decryption-table
#8@(53 190   7  46  83 105 219  40 111 183 118 107  12 125  54 139 
146 188 169  50 172  56 156  66  99 200  30  79  36 229 247 201 
 97 141  47  63 179 101 127 112 175 154 234 245  91 152 144 177 
135 113 114 237  55  69 104 163 227 239  92 197  80 193 214 202 
 90  98  95  38   9  93  20  65 232 157 206  64 253   8  23  74 
 15 199 180  62  18 252  37  75 129  44   4 120 203 187  32 189 
249  41 153 168 211  96 223  17 151 137 126 250 224 155  31 210 
103 226 100 119 132  43 158 138 241 109 136 121 116  87 221 230 
 57 123 238 131 225  88 242  13  52 248  48 233 185  35  84  21 
 68  11  77 102  58   3 162 145 148  82  76 195 130 231 128 192 
182  14 194 108 147 236 171  67 149 246 216  70 134   5 140 176 
117   0 204 133 215  61 115 122  72 228 209  89 173 184 198 208 
220 161 170   2  29 191 181 159  81 196 165  16  34 207   1 186 
143  49 124 174 150 218 240  86  71 212 235  78 217  19 142  73 
 85  22 255  59 244 164 178   6 160 167 251  27 110  60  51 205 
 24  94 106 213 166  33 222 254  42  28 243  10  26  25  39  45))

(declaim (type (simple-array (unsigned-byte 32) (256))
               t-encryption-table t-decryption-table))
(defconst t-encryption-table
#32@(#x97b1b126 #x69cecea7 #x73c3c3b0 #xdf95954a
#xb45a5aee #xafadad02 #x3be7e7dc #x04020206
#x9a4d4dd7 #x884444cc #x03fbfbf8 #xd7919146
#x180c0c14 #xfb87877c #xb7a1a116 #xa05050f0
#x63cbcba8 #xce6767a9 #xa85454fc #x4fdddd92
#x8c4646ca #xeb8f8f64 #x37e1e1d6 #x9c4e4ed2
#x15f0f0e5 #x0ffdfdf2 #x0dfcfcf1 #x23ebebc8
#x07f9f9fe #x7dc4c4b9 #x341a1a2e #xdc6e6eb2
#xbc5e5ee2 #x1ff5f5ea #x6dcccca1 #xef8d8d62
#x381c1c24 #xac5656fa #x864343c5 #x09fefef7
#x0e070709 #xc26161a3 #x05f8f8fd #xea75759f
#xb25959eb #x0bfffff4 #x06030305 #x44222266
#xe18a8a6b #x57d1d186 #x26131335 #x29eeeec7
#xe588886d #x00000000 #x1c0e0e12 #x6834345c
#x2a15153f #xf5808075 #xdd949449 #x33e3e3d0
#x2fededc2 #x9fb5b52a #xa65353f5 #x46232365
#x964b4bdd #x8e4747c9 #x2e171739 #xbba7a71c
#xd5909045 #x6a35355f #xa3abab08 #x45d8d89d
#x85b8b83d #x4bdfdf94 #x9e4f4fd1 #xae5757f9
#xc19a9a5b #xd1929243 #x43dbdb98 #x361b1b2d
#x783c3c44 #x65c8c8ad #xc799995e #x0804040c
#xe98e8e67 #x35e0e0d5 #x5bd7d78c #xfa7d7d87
#xff85857a #x83bbbb38 #x804040c0 #x582c2c74
#x743a3a4e #x8a4545cf #x17f1f1e6 #x844242c6
#xca6565af #x40202060 #x824141c3 #x30181828
#xe4727296 #x4a25256f #xd3939340 #xe0707090
#x6c36365a #x0a05050f #x11f2f2e3 #x160b0b1d
#xb3a3a310 #xf279798b #x2dececc1 #x10080818
#x4e272769 #x62313153 #x64323256 #x99b6b62f
#xf87c7c84 #x95b0b025 #x140a0a1e #xe6737395
#xb65b5bed #xf67b7b8d #x9bb7b72c #xf7818176
#x51d2d283 #x1a0d0d17 #xd46a6abe #x4c26266a
#xc99e9e57 #xb05858e8 #xcd9c9c51 #xf3838370
#xe874749c #x93b3b320 #xadacac01 #x60303050
#xf47a7a8e #xd26969bb #xee777799 #x1e0f0f11
#xa9aeae07 #x42212163 #x49dede97 #x55d0d085
#x5c2e2e72 #xdb97974c #x20101030 #xbda4a419
#xc598985d #xa5a8a80d #x5dd4d489 #xd06868b8
#x5a2d2d77 #xc46262a6 #x5229297b #xda6d6db7
#x2c16163a #x924949db #xec76769a #x7bc7c7bc
#x25e8e8cd #x77c1c1b6 #xd996964f #x6e373759
#x3fe5e5da #x61cacaab #x1df4f4e9 #x27e9e9ce
#xc66363a5 #x24121236 #x71c2c2b3 #xb9a6a61f
#x2814143c #x8dbcbc31 #x53d3d380 #x50282878
#xabafaf04 #x5e2f2f71 #x39e6e6df #x4824246c
#xa45252f6 #x79c6c6bf #xb5a0a015 #x1209091b
#x8fbdbd32 #xed8c8c61 #x6bcfcfa4 #xba5d5de7
#x22111133 #xbe5f5fe1 #x02010103 #x7fc5c5ba
#xcb9f9f54 #x7a3d3d47 #xb1a2a213 #xc39b9b58
#x67c9c9ae #x763b3b4d #x89bebe37 #xa25151f3
#x3219192b #x3e1f1f21 #x7e3f3f41 #xb85c5ce4
#x91b2b223 #x2befefc4 #x944a4ade #x6fcdcda2
#x8bbfbf34 #x81baba3b #xde6f6fb1 #xc86464ac
#x47d9d99e #x13f3f3e0 #x7c3e3e42 #x9db4b429
#xa1aaaa0b #x4ddcdc91 #x5fd5d58a #x0c06060a
#x75c0c0b5 #xfc7e7e82 #x19f6f6ef #xcc6666aa
#xd86c6cb4 #xfd848479 #xe2717193 #x70383848
#x87b9b93e #x3a1d1d27 #xfe7f7f81 #xcf9d9d52
#x904848d8 #xe38b8b68 #x542a2a7e #x41dada9b
#xbfa5a51a #x66333355 #xf1828273 #x7239394b
#x59d6d68f #xf0787888 #xf986867f #x01fafafb
#x3de4e4d9 #x562b2b7d #xa7a9a90e #x3c1e1e22
#xe789896e #xc06060a0 #xd66b6bbd #x21eaeacb
#xaa5555ff #x984c4cd4 #x1bf7f7ec #x31e2e2d3))

(defconst t-decryption-table
#32@(#xe368bc02 #x5585620c #x2a3f2331 #x61ab13f7
#x98d46d72 #x21cb9a19 #x3c22a461 #x459d3dcd
#x05fdb423 #x2bc4075f #x9b2c01c0 #x3dd9800f
#x486c5c74 #xf97f7e85 #xf173ab1f #xb6edde0e
#x283c6bed #x4997781a #x9f2a918d #xc9579f33
#xa907a8aa #xa50ded7d #x7c422d8f #x764db0c9
#x4d91e857 #xcea963cc #xb4ee96d2 #x3028e1b6
#x0df161b9 #xbd196726 #x419bad80 #xc0a06ec7
#x5183f241 #x92dbf034 #x6fa21efc #x8f32ce4c
#x13e03373 #x69a7c66d #xe56d6493 #xbf1a2ffa
#xbb1cbfb7 #x587403b5 #xe76e2c4f #x5d89b796
#xe89c052a #x446619a3 #x342e71fb #x0ff22965
#xfe81827a #xb11322f1 #xa30835ec #xcd510f7e
#xff7aa614 #x5c7293f8 #x2fc29712 #xf370e3c3
#x992f491c #xd1431568 #xc2a3261b #x88cc32b3
#x8acf7a6f #xb0e8069f #x7a47f51e #xd2bb79da
#xe6950821 #x4398e55c #xd0b83106 #x11e37baf
#x7e416553 #xccaa2b10 #xd8b4e49c #x6456a7d4
#xfb7c3659 #x724b2084 #xea9f4df6 #x6a5faadf
#x2dc1dfce #x70486858 #xcaaff381 #x0605d891
#x5a774b69 #x94de28a5 #x39df1042 #x813bc347
#xfc82caa6 #x23c8d2c5 #x03f86cb2 #x080cd59a
#xdab7ac40 #x7db909e1 #x3824342c #xcf5247a2
#xdcb274d1 #x63a85b2b #x35d55595 #x479e7511
#x15e5ebe2 #x4b9430c6 #x4a6f14a8 #x91239c86
#x4c6acc39 #x5f8aff4a #x0406904d #xee99ddbb
#x1e1152ca #xaaffc418 #xeb646998 #x07fefcff
#x8b345e01 #x567d0ebe #xbae79bd9 #x4263c132
#x75b5dc7b #x97264417 #x67aecb66 #x95250ccb
#xec9a9567 #x57862ad0 #x60503799 #xb8e4d305
#x65ad83ba #x19efae35 #xa4f6c913 #xc15b4aa9
#x873e1bd6 #xa0f0595e #x18148a5b #xaf02703b
#xab04e076 #xdd4950bf #xdf4a1863 #xc6a5b656
#x853d530a #xfa871237 #x77b694a7 #x4665517f
#xed61b109 #x1bece6e9 #xd5458525 #xf5753b52
#x7fba413d #x27ce4288 #xb2eb4e43 #xd6bde997
#x527b9ef3 #x62537f45 #x2c3afba0 #x7bbcd170
#xb91ff76b #x121b171d #xfd79eec8 #x3a277cf0
#x0c0a45d7 #x96dd6079 #x2233f6ab #xacfa1c89
#xc8acbb5d #xa10b7d30 #xd4bea14b #xbee10b94
#x25cd0a54 #x547e4662 #xa2f31182 #x17e6a33e
#x263566e6 #xc3580275 #x83388b9b #x7844bdc2
#x020348dc #x4f92a08b #x2e39b37c #x4e6984e5
#xf0888f71 #x362d3927 #x9cd2fd3f #x01fb246e
#x893716dd #x00000000 #xf68d57e0 #xe293986c
#x744ef815 #x9320d45a #xad0138e7 #xd3405db4
#x1a17c287 #xb3106a2d #x5078d62f #xf48e1f3c
#xa70ea5a1 #x71b34c36 #x9ad725ae #x5e71db24
#x161d8750 #xef62f9d5 #x8d318690 #x1c121a16
#xa6f581cf #x5b8c6f07 #x37d61d49 #x6e593a92
#x84c67764 #x86c53fb8 #xd746cdf9 #xe090d0b0
#x29c74f83 #xe49640fd #x0e090d0b #x6da15620
#x8ec9ea22 #xdb4c882e #xf776738e #xb515b2bc
#x10185fc1 #x322ba96a #x6ba48eb1 #xaef95455
#x406089ee #x6655ef08 #xe9672144 #x3e21ecbd
#x2030be77 #xf28bc7ad #x80c0e729 #x141ecf8c
#xbce24348 #xc4a6fe8a #x31d3c5d8 #xb716fa60
#x5380ba9d #xd94fc0f2 #x1de93e78 #x24362e3a
#xe16bf4de #xcb54d7ef #x09f7f1f4 #x82c3aff5
#x0bf4b928 #x9d29d951 #xc75e9238 #xf8845aeb
#x90d8b8e8 #xdeb13c0d #x33d08d04 #x685ce203
#xc55ddae4 #x3bdc589e #x0a0f9d46 #x3fdac8d3
#x598f27db #xa8fc8cc4 #x79bf99ac #x6c5a724e
#x8ccaa2fe #x9ed1b5e3 #x1fea76a4 #x73b004ea))

(declaim (inline mul8))
(defun mul8 (a b)
  (declare (type (unsigned-byte 8) a b))
  (if (or (zerop a) (zerop b))
      0
      (aref alogtable (mod (+ (aref logtable a) (aref logtable b)) 255))))

;;; this function only runs during the key generation process, so consing
;;; is acceptable.
(defun transform (in in-offset out out-offset)
  (declare (type (simple-array (unsigned-byte 32) (*)) in out))
  (let ((a-matrix (make-array (list 4 4) :element-type '(unsigned-byte 8)))
        (b-matrix (make-array (list 4 4) :element-type '(unsigned-byte 8)
                              :initial-element 0)))
    (macrolet ((inref (index)
                 `(aref in (+ ,index in-offset)))
               (outref (index)
                 `(aref out (+ ,index out-offset))))
      (dotimes (i 4)
        (dotimes (j 4)
          (setf (aref a-matrix i j)
                (logand (ash (inref i) (- (- 24 (* j 8)))) #xff))))
      (dotimes (i 4)
        (dotimes (j 4)
          (dotimes (k 4)
            (setf (aref b-matrix i j)
                  (logand
                   (logxor (mul8 (aref a-matrix i k) (aref g-matrix k j))
                           (aref b-matrix i j))
                   #xff)))))
      (dotimes (i 4)
        (setf (outref i) 0)
        (dotimes (j 4)
          (setf (outref i)
                (logxor (outref i)
                        (ash (aref b-matrix i j) (- 24 (* j 8))))))))))

(defun generate-round-keys (key n-rounds encrypt-roundkeys decrypt-roundkeys)
  (declare (type (simple-array (unsigned-byte 32) (*))
                 encrypt-roundkeys decrypt-roundkeys)
           (type (simple-array (unsigned-byte 8) (16)) key))
  (let ((offset (make-array n-rounds :element-type '(unsigned-byte 8)
                            :initial-element 1))
        (tempkeys (make-array (* (1+ n-rounds) 4) :element-type '(unsigned-byte 32))))
    (declare (type (simple-array (unsigned-byte 8) (*)) offset)
             (type (simple-array (unsigned-byte 32) (*)) tempkeys))
    ;; hack for stupid C array punning
    (macrolet ((mdref (array i j)
                 `(aref ,array (+ (* ,i 4) ,j))))
      (do ((i 1 (1+ i)))
          ((>= i n-rounds))
        (setf (aref offset i) (mul8 2 (aref offset (1- i)))))
      (dotimes (i 4)
        (setf (mdref tempkeys 0 i) (ub32ref/be key (* 4 i))))
      (do ((i 1 (1+ i)))
          ((>= i (1+ n-rounds)))
        (setf (mdref tempkeys i 0)
              (logxor (mdref tempkeys (1- i) 0)
                      (rol32 (mdref tempkeys (1- i) 3) 8)
                      (ash (aref offset (1- i)) 24))
              (mdref tempkeys i 1)
              (logxor (mdref tempkeys (1- i) 1) (mdref tempkeys i 0))
              (mdref tempkeys i 2)
              (logxor (mdref tempkeys (1- i) 2) (mdref tempkeys i 1))
              (mdref tempkeys i 3)
              (logxor (mdref tempkeys (1- i) 3) (mdref tempkeys i 2))))
      (dotimes (i n-rounds)
        (transform tempkeys (* i 4) encrypt-roundkeys (* i 4)))
      (dotimes (i 4)
        (setf (mdref encrypt-roundkeys n-rounds i)
              (mdref tempkeys n-rounds i)))
      (dotimes (i n-rounds)
        (dotimes (j 4)
          (setf (mdref decrypt-roundkeys i j)
                (mdref tempkeys (- n-rounds i) j))))
      (dotimes (i 4)
        (setf (mdref decrypt-roundkeys n-rounds i)
              (mdref encrypt-roundkeys 0 i))))))

(declaim (inline square-munge-block))
(defun square-munge-block (round-keys n-rounds t-array s-array
                              plaintext plaintext-start
                              ciphertext ciphertext-start)
  (declare (type (simple-array (unsigned-byte 8) (*)) plaintext ciphertext)
           (type (simple-array (unsigned-byte 8) (256)) s-array)
           (type (simple-array (unsigned-byte 32) (*)) round-keys)
           (type (simple-array (unsigned-byte 32) (256)) t-array))
  (declare (type (integer 0 #.(- array-dimension-limit 16))
                 plaintext-start ciphertext-start))
  (with-words ((b0 b1 b2 b3) plaintext plaintext-start)
    (let ((a0 0) (a1 0) (a2 0) (a3 0))
      (declare (type (unsigned-byte 32) a0 a1 a2 a3))
      ;; initial key addition
      (setf b0 (logxor b0 (aref round-keys 0))
            b1 (logxor b1 (aref round-keys 1))
            b2 (logxor b2 (aref round-keys 2))
            b3 (logxor b3 (aref round-keys 3)))
      ;; full rounds
      (do ((i 0 (1+ i))
           (rk-offset 4 (+ rk-offset 4)))
          ((>= i (1- n-rounds)))
        (macrolet ((mix (tmpvar bytefun)
                     `(setf ,tmpvar
                            (logxor (aref t-array (,bytefun b0))
                                    (mod32+ (mod32ash (aref t-array (,bytefun b1)) -8)
                                            (mod32ash (aref t-array (,bytefun b1)) 24))
                                    (mod32+ (mod32ash (aref t-array (,bytefun b2)) -16)
                                            (mod32ash (aref t-array (,bytefun b2)) 16))
                                    (mod32+ (mod32ash (aref t-array (,bytefun b3)) -24)
                                            (mod32ash (aref t-array (,bytefun b3)) 8))))))
          (mix a0 fourth-byte)
          (mix a1 third-byte)
          (mix a2 second-byte)
          (mix a3 first-byte)
          (setf b0 (logxor a0 (aref round-keys (+ rk-offset 0)))
                b1 (logxor a1 (aref round-keys (+ rk-offset 1)))
                b2 (logxor a2 (aref round-keys (+ rk-offset 2)))
                b3 (logxor a3 (aref round-keys (+ rk-offset 3)))))))
      ;; last round
      (macrolet ((last-round (bytefun)
                   `(mod32+ (mod32ash (aref s-array (,bytefun b0)) 24)
                            (mod32+ (mod32ash (aref s-array (,bytefun b1)) 16)
                                    (mod32+ (mod32ash (aref s-array (,bytefun b2)) 8)
                                            (mod32ash (aref s-array (,bytefun b3)) 0)))))
                 (rkref (index)
                   `(aref round-keys (+ ,index (* n-rounds 4)))))
        (let ((t0 (last-round fourth-byte))
              (t1 (last-round third-byte))
              (t2 (last-round second-byte))
              (t3 (last-round first-byte)))
          (declare (type (unsigned-byte 32) t0 t1 t2 t3))
          (flet ((apply-rk (temp round-key)
                   (declare (type (unsigned-byte 32) temp round-key))
                   (logxor temp round-key)))
            (declare (inline apply-rk))
            (store-words ciphertext ciphertext-start
                         (apply-rk t0 (rkref 0))
                         (apply-rk t1 (rkref 1))
                         (apply-rk t2 (rkref 2))
                         (apply-rk t3 (rkref 3))))))))

(defclass square (cipher 16-byte-block-mixin)
  ((encryption-round-keys :accessor encryption-round-keys
                          :type (simple-array (unsigned-byte 32) (*)))
   (decryption-round-keys :accessor decryption-round-keys
                          :type (simple-array (unsigned-byte 32) (*)))
   (n-rounds :initarg :n-rounds :reader n-rounds))
  (:default-initargs :n-rounds 8))

(define-block-encryptor square 16
  (let ((n-rounds (n-rounds context))
        (round-keys (encryption-round-keys context)))
    (square-munge-block round-keys n-rounds t-encryption-table s-encryption-table
                        plaintext plaintext-start ciphertext ciphertext-start)))

(define-block-decryptor square 16
  (let ((n-rounds (n-rounds context))
        (round-keys (decryption-round-keys context)))
    (square-munge-block round-keys n-rounds t-decryption-table s-decryption-table
                        ciphertext ciphertext-start plaintext plaintext-start)))

(defmethod schedule-key ((cipher square) key)
  (let ((encryption-schedule (make-array (* 4 (1+ (n-rounds cipher)))
                                         :element-type '(unsigned-byte 32)))
        (decryption-schedule (make-array (* 4 (1+ (n-rounds cipher)))
                                         :element-type '(unsigned-byte 32))))
    (generate-round-keys key (n-rounds cipher)
                         encryption-schedule decryption-schedule)
    (setf (encryption-round-keys cipher) encryption-schedule
          (decryption-round-keys cipher) decryption-schedule)
    cipher))

(defcipher square
  (:encrypt-function square-encrypt-block)
  (:decrypt-function square-decrypt-block)
  (:block-length 16)
  (:key-length (:fixed 16)))
