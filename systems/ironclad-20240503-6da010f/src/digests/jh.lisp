;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; jh.lisp -- implementation of the JH hash function

(in-package :crypto)


;;;
;;; Parameters
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +jh-rounds+ 42)
  (defconstant +jh-block-size+ 64)

;;; Initial hash values

  (defconst +jh224-h0+
    (make-array 16
                :element-type '(unsigned-byte 64)
                :initial-contents '(#xac989af962ddfe2d #xe734d619d6ac7cae
                                    #x161230bc051083a4 #x941466c9c63860b8
                                    #x6f7080259f89d966 #xdc1a9b1d1ba39ece
                                    #x106e367b5f32e811 #xc106fa027f8594f9
                                    #xb340c8d85c1b4f1b #x9980736e7fa1f697
                                    #xd3a3eaada593dfdc #x689a53c9dee831a4
                                    #xe4a186ec8aa9b422 #xf06ce59c95ac74d5
                                    #xbf2babb5ea0d9615 #x6eea64ddf0dc1196)))

  (defconst +jh256-h0+
    (make-array 16
                :element-type '(unsigned-byte 64)
                :initial-contents '(#xebd3202c41a398eb #xc145b29c7bbecd92
                                    #xfac7d4609151931c #x038a507ed6820026
                                    #x45b92677269e23a4 #x77941ad4481afbe0
                                    #x7a176b0226abb5cd #xa82fff0f4224f056
                                    #x754d2e7f8996a371 #x62e27df70849141d
                                    #x948f2476f7957627 #x6c29804757b6d587
                                    #x6c0d8eac2d275e5c #x0f7a0557c6508451
                                    #xea12247067d3e47b #x69d71cd313abe389)))

  (defconst +jh384-h0+
    (make-array 16
                :element-type '(unsigned-byte 64)
                :initial-contents '(#x8a3913d8c63b1e48 #x9b87de4a895e3b6d
                                    #x2ead80d468eafa63 #x67820f4821cb2c33
                                    #x28b982904dc8ae98 #x4942114130ea55d4
                                    #xec474892b255f536 #xe13cf4ba930a25c7
                                    #x4c45db278a7f9b56 #x0eaf976349bdfc9e
                                    #xcd80aa267dc29f58 #xda2eeb9d8c8bc080
                                    #x3a37d5f8e881798a #x717ad1ddad6739f4
                                    #x94d375a4bdd3b4a9 #x7f734298ba3f6c97)))

  (defconst +jh512-h0+
    (make-array 16
                :element-type '(unsigned-byte 64)
                :initial-contents '(#x17aa003e964bd16f #x43d5157a052e6a63
                                    #x0bef970c8d5e228a #x61c3b3f2591234e9
                                    #x1e806f53c1a01d89 #x806d2bea6b05a92a
                                    #xa6ba7520dbcc8e58 #xf73bf8ba763a0fa9
                                    #x694ae34105e66901 #x5ae66f2e8e8ab546
                                    #x243c84c1d0a74710 #x99c15a2db1716e3b
                                    #x56f8b19decf657cf #x56b116577c8806a7
                                    #xfb1785e6dffcc2e3 #x4bdd8ccc78465a54)))

;;; Round constants

  (defconst +jh-round-constants+
    (make-array 168
                :element-type '(unsigned-byte 64)
                :initial-contents '(#x67f815dfa2ded572 #x571523b70a15847b
                                    #xf6875a4d90d6ab81 #x402bd1c3c54f9f4e
                                    #x9cfa455ce03a98ea #x9a99b26699d2c503
                                    #x8a53bbf2b4960266 #x31a2db881a1456b5
                                    #xdb0e199a5c5aa303 #x1044c1870ab23f40
                                    #x1d959e848019051c #xdccde75eadeb336f
                                    #x416bbf029213ba10 #xd027bbf7156578dc
                                    #x5078aa3739812c0a #xd3910041d2bf1a3f
                                    #x907eccf60d5a2d42 #xce97c0929c9f62dd
                                    #xac442bc70ba75c18 #x23fcc663d665dfd1
                                    #x1ab8e09e036c6e97 #xa8ec6c447e450521
                                    #xfa618e5dbb03f1ee #x97818394b29796fd
                                    #x2f3003db37858e4a #x956a9ffb2d8d672a
                                    #x6c69b8f88173fe8a #x14427fc04672c78a
                                    #xc45ec7bd8f15f4c5 #x80bb118fa76f4475
                                    #xbc88e4aeb775de52 #xf4a3a6981e00b882
                                    #x1563a3a9338ff48e #x89f9b7d524565faa
                                    #xfde05a7c20edf1b6 #x362c42065ae9ca36
                                    #x3d98fe4e433529ce #xa74b9a7374f93a53
                                    #x86814e6f591ff5d0 #x9f5ad8af81ad9d0e
                                    #x6a6234ee670605a7 #x2717b96ebe280b8b
                                    #x3f1080c626077447 #x7b487ec66f7ea0e0
                                    #xc0a4f84aa50a550d #x9ef18e979fe7e391
                                    #xd48d605081727686 #x62b0e5f3415a9e7e
                                    #x7a205440ec1f9ffc #x84c9f4ce001ae4e3
                                    #xd895fa9df594d74f #xa554c324117e2e55
                                    #x286efebd2872df5b #xb2c4a50fe27ff578
                                    #x2ed349eeef7c8905 #x7f5928eb85937e44
                                    #x4a3124b337695f70 #x65e4d61df128865e
                                    #xe720b95104771bc7 #x8a87d423e843fe74
                                    #xf2947692a3e8297d #xc1d9309b097acbdd
                                    #xe01bdc5bfb301b1d #xbf829cf24f4924da
                                    #xffbf70b431bae7a4 #x48bcf8de0544320d
                                    #x39d3bb5332fcae3b #xa08b29e0c1c39f45
                                    #x0f09aef7fd05c9e5 #x34f1904212347094
                                    #x95ed44e301b771a2 #x4a982f4f368e3be9
                                    #x15f66ca0631d4088 #xffaf52874b44c147
                                    #x30c60ae2f14abb7e #xe68c6eccc5b67046
                                    #x00ca4fbd56a4d5a4 #xae183ec84b849dda
                                    #xadd1643045ce5773 #x67255c1468cea6e8
                                    #x16e10ecbf28cdaa3 #x9a99949a5806e933
                                    #x7b846fc220b2601f #x1885d1a07facced1
                                    #xd319dd8da15b5932 #x46b4a5aac01c9a50
                                    #xba6b04e467633d9f #x7eee560bab19caf6
                                    #x742128a9ea79b11f #xee51363b35f7bde9
                                    #x76d350755aac571d #x01707da3fec2463a
                                    #x42d8a498afc135f7 #x79676b9e20eced78
                                    #xa8db3aea15638341 #x832c83324d3bc3fa
                                    #xf347271c1f3b40a7 #x9a762db734f04059
                                    #xfd4f21d26c4e3ee7 #xef5957dc398dfdb8
                                    #xdaeb492b490c9b8d #x0d70f36849d7a25b
                                    #x84558d7ad0ae3b7d #x658ef8e4f0e9a5f5
                                    #x533b1036f4a2b8a0 #x5aec3e759e07a80c
                                    #x4f88e85692946891 #x4cbcbaf8555cb05b
                                    #x7b9487f3993bbbe3 #x5d1c6b72d6f4da75
                                    #x6db334dc28acae64 #x71db28b850a5346c
                                    #x2a518d10f2e261f8 #xfc75dd593364dbe3
                                    #xa23fce43f1bcac1c #xb043e8023cd1bb67
                                    #x75a12988ca5b0a33 #x5c5316b44d19347f
                                    #x1e4d790ec3943b92 #x3fafeeb6d7757479
                                    #x21391abef7d4a8ea #x5127234c097ef45c
                                    #xd23c32ba5324a326 #xadd5a66d4a17a344
                                    #x08c9f2afa63e1db5 #x563c6b91983d5983
                                    #x4d608672a17cf84c #xf6c76e08cc3ee246
                                    #x5e76bcb1b333982f #x2ae6c4efa566d62b
                                    #x36d4c1bee8b6f406 #x6321efbc1582ee74
                                    #x69c953f40d4ec1fd #x26585806c45a7da7
                                    #x16fae0061614c17e #x3f9d63283daf907e
                                    #x0cd29b00e3f2c9d2 #x300cd4b730ceaa5f
                                    #x9832e0f216512a74 #x9af8cee3d830eb0d
                                    #x9279f1b57b9ec54b #xd36886046ee651ff
                                    #x316796e6574d239b #x05750a17f3a6e6cc
                                    #xce6c3213d98176b1 #x62a205f88452173c
                                    #x47154778b3cb2bf4 #x486a9323825446ff
                                    #x65655e4e0758df38 #x8e5086fc897cfcf2
                                    #x86ca0bd0442e7031 #x4e477830a20940f0
                                    #x8338f7d139eea065 #xbd3a2ce437e95ef7
                                    #x6ff8130126b29721 #xe7de9fefd1ed44a3
                                    #xd992257615dfa08b #xbe42dc12f6f7853c
                                    #x7eb027ab7ceca7d8 #xdea83eaada7d8d53
                                    #xd86902bd93ce25aa #xf908731afd43f65a
                                    #xa5194a17daef5fc0 #x6a21fd4c33664d97
                                    #x701541db3198b435 #x9b54cdedbb0f1eea
                                    #x72409751a163d09a #xe26f4791bf9d75f6))))


;;;
;;; Transformations
;;;

(defmacro jh-swap-1 (x)
  "Swapping bit 2i with bit 2i+1 of 64-bit X."
  `(setf ,x (logior (ash (logand ,x #x5555555555555555) 1)
                    (ash (logand ,x #xaaaaaaaaaaaaaaaa) -1))))

(defmacro jh-swap-2 (x)
  "Swapping bits 4i||4i+1 with bits 4i+2||4i+3 of 64-bit X."
  `(setf ,x (logior (ash (logand ,x #x3333333333333333) 2)
                    (ash (logand ,x #xcccccccccccccccc) -2))))

(defmacro jh-swap-4 (x)
  "Swapping bits 8i||8i+1||8i+2||8i+3 with bits
8i+4||8i+5||8i+6||8i+7 of 64-bit X."
  `(setf ,x (logior (ash (logand ,x #x0f0f0f0f0f0f0f0f) 4)
                    (ash (logand ,x #xf0f0f0f0f0f0f0f0) -4))))

(defmacro jh-swap-8 (x)
  "Swapping bits 16i||16i+1||......||16i+7 with bits
16i+8||16i+9||......||16i+15 of 64-bit X."
  `(setf ,x (logior (ash (logand ,x #x00ff00ff00ff00ff) 8)
                    (ash (logand ,x #xff00ff00ff00ff00) -8))))

(defmacro jh-swap-16 (x)
  "Swapping bits 32i||32i+1||......||32i+15 with bits
32i+16||32i+17||......||32i+31 of 64-bit X."
  `(setf ,x (logior (ash (logand ,x #x0000ffff0000ffff) 16)
                    (ash (logand ,x #xffff0000ffff0000) -16))))

(defmacro jh-swap-32 (x)
  "Swapping bits 64i||64i+1||......||64i+31 with bits
64i+32||64i+33||......||64i+63 of 64-bit X."
  `(setf ,x (logior (ash (logand ,x #x00000000ffffffff) 32)
                    (ash (logand ,x #xffffffff00000000) -32))))

(defmacro jh-l (m0 m1 m2 m3 m4 m5 m6 m7)
  "The MDS transform."
  `(setf ,m4 (logxor ,m4 ,m1)
         ,m5 (logxor ,m5 ,m2)
         ,m6 (logxor ,m6 (logxor ,m0 ,m3))
         ,m7 (logxor ,m7 ,m0)
         ,m0 (logxor ,m0 ,m5)
         ,m1 (logxor ,m1 ,m6)
         ,m2 (logxor ,m2 (logxor ,m4 ,m7))
         ,m3 (logxor ,m3 ,m4)))

(defmacro jh-ss (m0 m1 m2 m3 m4 m5 m6 m7 cc0 cc1 t0 t1)
  "The S-boxes."
  `(setf ,m3 (mod64lognot ,m3)
         ,m7 (mod64lognot ,m7)
         ,m0 (logxor ,m0 (logand (mod64lognot ,m2) ,cc0))
         ,m4 (logxor ,m4 (logand (mod64lognot ,m6) ,cc1))
         ,t0 (logxor ,cc0 (logand ,m0 ,m1))
         ,t1 (logxor ,cc1 (logand ,m4 ,m5))
         ,m0 (logxor ,m0 (logand ,m2 ,m3))
         ,m4 (logxor ,m4 (logand ,m6 ,m7))
         ,m3 (logxor ,m3 (logand (mod64lognot ,m1) ,m2))
         ,m7 (logxor ,m7 (logand (mod64lognot ,m5) ,m6))
         ,m1 (logxor ,m1 (logand ,m0 ,m2))
         ,m5 (logxor ,m5 (logand ,m4 ,m6))
         ,m2 (logxor ,m2 (logand ,m0 (mod64lognot ,m3)))
         ,m6 (logxor ,m6 (logand ,m4 (mod64lognot ,m7)))
         ,m0 (logxor ,m0 (logior ,m1 ,m3))
         ,m4 (logxor ,m4 (logior ,m5 ,m7))
         ,m3 (logxor ,m3 (logand ,m1 ,m2))
         ,m7 (logxor ,m7 (logand ,m5 ,m6))
         ,m1 (logxor ,m1 (logand ,t0 ,m0))
         ,m5 (logxor ,m5 (logand ,t1 ,m4))
         ,m2 (logxor ,m2 ,t0)
         ,m6 (logxor ,m6 ,t1)))


;;;
;;; Rounds
;;;

(declaim (ftype (function ((simple-array (unsigned-byte 64) (16)))) jh-e8))
(defun jh-e8 (s)
  "The bijective function."
  (declare (type (simple-array (unsigned-byte 64) (16)) s)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((constants (load-time-value +jh-round-constants+ t))
        (v0 (aref s 0))
        (v1 (aref s 1))
        (v2 (aref s 2))
        (v3 (aref s 3))
        (v4 (aref s 4))
        (v5 (aref s 5))
        (v6 (aref s 6))
        (v7 (aref s 7))
        (v8 (aref s 8))
        (v9 (aref s 9))
        (v10 (aref s 10))
        (v11 (aref s 11))
        (v12 (aref s 12))
        (v13 (aref s 13))
        (v14 (aref s 14))
        (v15 (aref s 15))
        (t0 0)
        (t1 0))
    (declare (type (simple-array (unsigned-byte 64) (168)) constants)
             (type (unsigned-byte 64) v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 t0 t1))
    (do ((round 0 (+ round 7)))
        ((= round +jh-rounds+))
      (declare (type (integer 0 42) round))
      (macrolet ((constant (i j)
                   `(aref constants (+ (* 4 ,i) ,j)))
                 (sub-round (i)
                   (let ((swap (ecase i
                                 ((0) 'jh-swap-1)
                                 ((1) 'jh-swap-2)
                                 ((2) 'jh-swap-4)
                                 ((3) 'jh-swap-8)
                                 ((4) 'jh-swap-16)
                                 ((5) 'jh-swap-32)
                                 ((6) nil))))
                     `(progn
                        (jh-ss v0 v4 v8 v12 v2 v6 v10 v14
                               (constant (+ round ,i) 0)
                               (constant (+ round ,i) 2)
                               t0 t1)
                        (jh-l v0 v4 v8 v12 v2 v6 v10 v14)
                        ,(when swap `(,swap v2))
                        ,(when swap `(,swap v6))
                        ,(when swap `(,swap v10))
                        ,(when swap `(,swap v14))
                        (jh-ss v1 v5 v9 v13 v3 v7 v11 v15
                               (constant (+ round ,i) 1)
                               (constant (+ round ,i) 3)
                               t0 t1)
                        (jh-l v1 v5 v9 v13 v3 v7 v11 v15)
                        ,(when swap `(,swap v3))
                        ,(when swap `(,swap v7))
                        ,(when swap `(,swap v11))
                        ,(when swap `(,swap v15))))))

        ;; Round 7*roundnumber+0: S-box, MDS and swapping layers
        (sub-round 0)

        ;; Round 7*roundnumber+1: S-box, MDS and swapping layers
        (sub-round 1)

        ;; Round 7*roundnumber+2: S-box, MDS and swapping layers
        (sub-round 2)

        ;; Round 7*roundnumber+3: S-box, MDS and swapping layers
        (sub-round 3)

        ;; Round 7*roundnumber+4: S-box, MDS and swapping layers
        (sub-round 4)

        ;; Round 7*roundnumber+5: S-box, MDS and swapping layers
        (sub-round 5)

        ;; Round 7*roundnumber+6: S-box and MDS layers
        (sub-round 6)

        ;; Round 7*roundnumber+6: swapping layer
        (rotatef v2 v3)
        (rotatef v6 v7)
        (rotatef v10 v11)
        (rotatef v14 v15)))

    ;; Save the new state
    (setf (aref s 0) v0
          (aref s 1) v1
          (aref s 2) v2
          (aref s 3) v3
          (aref s 4) v4
          (aref s 5) v5
          (aref s 6) v6
          (aref s 7) v7
          (aref s 8) v8
          (aref s 9) v9
          (aref s 10) v10
          (aref s 11) v11
          (aref s 12) v12
          (aref s 13) v13
          (aref s 14) v14
          (aref s 15) v15)

    (values)))

(defun jh-f8 (state)
  "The compression function."
  (declare (notinline jh-buffer jh-state))
  (let ((s (jh-state state))
        (buffer (jh-buffer state))
        (b (make-array 8 :element-type '(unsigned-byte 64))))
    (declare (type (simple-array (unsigned-byte 64) (16)) s)
             (type (simple-array (unsigned-byte 8) (64)) buffer)
             (type (simple-array (unsigned-byte 64) (8)) b)
             (dynamic-extent b))

    ;; Get input data as 64-bit little-endian integers
    (dotimes (i 8)
      (setf (aref b i) (ub64ref/le buffer (* 8 i))))

    ;; Xor the 512-bit message with the fist half of the 1024-bit hash state
    (dotimes (i 8)
      (setf (aref s i) (logxor (aref s i) (aref b i))))

    ;; Apply the bijective function E8
    (jh-e8 s)

    ;; Xor the 512-bit message with the second half of the 1024-bit hash state
    (dotimes (i 8)
      (setf (aref s (+ i 8)) (logxor (aref s (+ i 8)) (aref b i))))

    (values)))


;;;
;;; Digest structures and functions
;;;

(defstruct (jh
            (:constructor %make-jh-digest nil)
            (:copier nil))
  (state (copy-seq +jh512-h0+) :type (simple-array (unsigned-byte 64) (16)))
  (data-length 0 :type (unsigned-byte 64))
  (buffer (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)
          :type (simple-array (unsigned-byte 8) (64)))
  (buffer-index 0 :type (integer 0 64)))

(defstruct (jh/384
            (:include jh)
            (:constructor %make-jh/384-digest
                (&aux (state (copy-seq +jh384-h0+))))
            (:copier nil)))

(defstruct (jh/256
            (:include jh)
            (:constructor %make-jh/256-digest
                (&aux (state (copy-seq +jh256-h0+))))
            (:copier nil)))

(defstruct (jh/224
            (:include jh)
            (:constructor %make-jh/224-digest
                (&aux (state (copy-seq +jh224-h0+))))
            (:copier nil)))

(defmethod reinitialize-instance ((state jh) &rest initargs)
  (declare (ignore initargs))
  (setf (jh-state state) (etypecase state
                           (jh/224 (copy-seq +jh224-h0+))
                           (jh/256 (copy-seq +jh256-h0+))
                           (jh/384 (copy-seq +jh384-h0+))
                           (jh (copy-seq +jh512-h0+)))
        (jh-data-length state) 0
        (jh-buffer-index state) 0)
  state)

(defmethod copy-digest ((state jh) &optional copy)
  (check-type copy (or null jh))
  (let ((copy (if copy
                  copy
                  (etypecase state
                    (jh/224 (%make-jh/224-digest))
                    (jh/256 (%make-jh/256-digest))
                    (jh/384 (%make-jh/384-digest))
                    (jh (%make-jh-digest))))))
    (declare (type jh copy))
    (replace (jh-state copy) (jh-state state))
    (replace (jh-buffer copy) (jh-buffer state))
    (setf (jh-data-length copy) (jh-data-length state)
          (jh-buffer-index copy) (jh-buffer-index state))
    copy))

(defun jh-update (state input start end)
  (declare (type (simple-array (unsigned-byte 8) (*)) input)
           (type (unsigned-byte 64) start end))
  (let ((data-length (jh-data-length state))
        (buffer (jh-buffer state))
        (buffer-index (jh-buffer-index state))
        (length (- end start))
        (n 0))
    (declare (type (simple-array (unsigned-byte 8) (64)) buffer)
             (type (unsigned-byte 64) data-length length)
             (type (integer 0 64) buffer-index n))

    ;; Try to fill the buffer with the new data
    (setf n (min length (- +jh-block-size+ buffer-index)))
    (replace buffer input :start1 buffer-index :start2 start :end2 (+ start n))
    (incf data-length n)
    (incf buffer-index n)
    (incf start n)
    (decf length n)

    ;; Process data in buffer
    (when (= buffer-index +jh-block-size+)
      (jh-f8 state)
      (setf buffer-index 0))

    ;; Process data in message
    (loop until (< length +jh-block-size+) do
      (replace buffer input :start2 start)
      (jh-f8 state)
      (incf data-length +jh-block-size+)
      (incf start +jh-block-size+)
      (decf length +jh-block-size+))

    ;; Put remaining message data in buffer
    (when (plusp length)
      (replace buffer input :end1 length :start2 start)
      (incf data-length length)
      (incf buffer-index length))

    ;; Save the new state
    (setf (jh-data-length state) data-length
          (jh-buffer-index state) buffer-index)

    (values)))

(defun jh-finalize (state digest digest-start)
  (let ((digest-length (digest-length state))
        (jh-state (jh-state state))
        (data-length (jh-data-length state))
        (buffer (jh-buffer state))
        (buffer-index (jh-buffer-index state)))
    (declare (type (simple-array (unsigned-byte 64) (16)) jh-state)
             (type (simple-array (unsigned-byte 8) (64)) buffer)
             (type (unsigned-byte 64) data-length)
             (type (integer 0 64) buffer-index))

    ;; Set the rest of the bytes in the buffer to 0
    (fill buffer 0 :start buffer-index)

    ;; Pad and process the partial block
    (if (zerop buffer-index)
        (progn
          (setf (aref buffer buffer-index) #x80)
          (setf (ub64ref/be buffer 56) (* data-length 8))
          (jh-f8 state))
        (progn
          (setf (aref buffer buffer-index) #x80)
          (jh-f8 state)
          (fill buffer 0)
          (setf (ub64ref/be buffer 56) (* data-length 8))
          (jh-f8 state)))

    ;; Truncate the final hash value to generate the message digest
    (let ((output (make-array +jh-block-size+ :element-type '(unsigned-byte 8))))
      (dotimes (i 8)
        (setf (ub64ref/le output (* i 8)) (aref jh-state (+ i 8))))
      (replace digest output :start1 digest-start :start2 (- +jh-block-size+ digest-length))
      digest)))

(define-digest-updater jh
  (jh-update state sequence start end))

(define-digest-finalizer ((jh 64)
                          (jh/384 48)
                          (jh/256 32)
                          (jh/224 28))
  (jh-finalize state digest digest-start))

(defdigest jh :digest-length 64 :block-length 64)
(defdigest jh/384 :digest-length 48 :block-length 64)
(defdigest jh/256 :digest-length 32 :block-length 64)
(defdigest jh/224 :digest-length 28 :block-length 64)
