;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; skein.lisp -- implementation of the Skein hash functions

(in-package :crypto)
(in-ironclad-readtable)


;;; Parameter identifiers

(defconstant +skein-key+ 0)
(defconstant +skein-cfg+ 4)
(defconstant +skein-prs+ 8)
(defconstant +skein-pk+ 12)
(defconstant +skein-kdf+ 16)
(defconstant +skein-non+ 20)
(defconstant +skein-msg+ 48)
(defconstant +skein-out+ 63)


;;; Initial values

(declaim (type (simple-array (unsigned-byte 8) (32))
               +skein256-iv-128+ +skein256-iv-160+ +skein256-iv-224+
               +skein256-iv-256+))
(declaim (type (simple-array (unsigned-byte 8) (64))
               +skein512-iv-128+ +skein512-iv-160+ +skein512-iv-224+
               +skein512-iv-256+ +skein512-iv-384+ +skein512-iv-512+))
(declaim (type (simple-array (unsigned-byte 8) (128))
               +skein1024-iv-384+ +skein1024-iv-512+ +skein1024-iv-1024+))

(defconst +skein256-iv-128+ #8@(96 114 77 150 6 25 17 225
                                28 129 141 124 167 170 61 136
                                122 15 150 145 244 13 8 16
                                194 193 91 180 229 221 247 204))
(defconst +skein256-iv-160+ #8@(152 94 130 114 20 35 32 20
                                144 229 119 90 162 233 196 42
                                62 214 56 136 86 88 122 212
                                125 171 134 133 150 228 210 45))
(defconst +skein256-iv-224+ #8@(11 234 229 154 140 138 9 198
                                28 25 197 8 134 86 109 135
                                132 56 245 215 215 136 203 153
                                222 181 221 174 177 221 75 56))
(defconst +skein256-iv-256+ #8@(73 180 72 208 96 168 157 252
                                51 216 167 159 71 102 202 47
                                15 132 86 102 137 195 59 179
                                105 218 232 253 32 233 84 106))

(defconst +skein512-iv-128+ #8@(82 159 191 111 243 123 188 168
                                170 240 26 189 206 114 152 30
                                211 144 33 179 144 23 155 48
                                92 128 148 63 84 184 251 188
                                27 177 49 110 205 27 166 13
                                227 50 106 212 234 235 24 26
                                130 170 132 206 24 91 204 162
                                45 152 70 157 40 171 130 105))
(defconst +skein512-iv-160+ #8@(145 189 19 224 42 26 184 40
                                143 247 189 181 104 22 241 194
                                18 111 165 246 243 216 96 23
                                79 144 57 130 88 71 183 79
                                86 80 175 126 127 224 237 33
                                184 112 237 99 46 146 8 217
                                250 82 203 236 255 118 236 184
                                110 122 242 163 184 123 164 1))
(defconst +skein512-iv-224+ #8@(36 114 103 72 98 97 208 204
                                239 57 35 169 243 92 166 203
                                100 75 255 82 214 105 205 140
                                180 144 184 58 123 237 138 57
                                208 43 125 69 177 209 89 15
                                61 235 212 117 101 254 118 103
                                233 19 116 153 14 199 251 153
                                247 30 196 225 207 252 44 158))
(defconst +skein512-iv-256+ #8@(19 62 219 47 161 68 208 204
                                235 169 121 26 48 144 53 232
                                111 110 129 79 97 160 174 85
                                219 148 155 174 164 103 39 42
                                131 118 221 116 94 2 6 236
                                81 98 116 196 205 54 164 231
                                133 209 58 57 249 186 111 195
                                19 252 237 51 24 186 237 62))
(defconst +skein512-iv-384+ #8@(95 239 117 58 191 198 246 163
                                164 250 132 253 204 249 254 176
                                254 12 119 61 102 221 119 157
                                218 253 104 180 243 203 152 215
                                101 68 14 138 102 166 196 27
                                7 116 128 229 52 212 215 126
                                214 68 236 212 172 193 143 84
                                248 143 161 106 84 23 110 38))
(defconst +skein512-iv-512+ #8@(206 81 156 116 255 173 3 73
                                3 223 70 151 57 222 149 13
                                206 155 199 39 65 147 209 143
                                177 44 53 255 41 86 37 154
                                176 167 108 223 153 37 182 93
                                244 195 213 169 76 57 190 234
                                35 181 117 26 199 18 17 153
                                51 204 15 102 11 164 24 174))

(defconst +skein1024-iv-384+ #8@(53 74 137 193 184 182 2 81
                                 26 241 138 254 227 201 235 254
                                 113 237 43 227 6 127 128 12
                                 246 145 26 180 82 58 193 96
                                 56 124 145 212 93 211 22 151
                                 58 29 211 111 18 223 128 231
                                 58 48 152 200 182 70 120 121
                                 59 42 87 179 168 194 114 177
                                 108 74 16 166 3 130 188 201
                                 244 36 86 215 56 147 144 101
                                 160 129 63 75 104 197 188 148
                                 70 253 236 16 30 245 187 62
                                 66 133 176 238 11 15 245 45
                                 22 101 188 13 48 101 90 59
                                 225 188 123 22 210 156 75 72
                                 234 175 203 212 71 105 19 45))
(defconst +skein1024-iv-512+ #8@(24 27 27 124 93 14 236 202
                                 2 232 3 95 4 14 27 160
                                 133 40 145 237 81 4 132 51
                                 28 46 236 234 4 251 74 55
                                 247 129 53 129 226 160 37 223
                                 210 249 18 139 147 64 0 228
                                 182 57 237 194 57 213 98 166
                                 90 199 216 69 207 133 139 250
                                 150 231 237 41 142 237 22 131
                                 184 145 159 46 192 137 50 5
                                 115 139 81 109 29 239 248 195
                                 46 51 239 213 196 195 206 189
                                 135 68 151 34 82 126 154 84
                                 22 152 116 91 114 8 7 103
                                 209 27 88 240 251 40 205 185
                                 116 73 128 21 184 64 41 14))
(defconst +skein1024-iv-1024+ #8@(85 35 231 65 7 218 147 213
                                  12 224 115 172 17 229 181 21
                                  240 196 242 186 174 229 128 81
                                  175 175 188 252 211 65 189 3
                                  152 168 131 25 253 198 174 28
                                  159 88 208 205 139 11 81 110
                                  218 74 57 198 253 189 226 119
                                  163 176 220 36 181 29 30 193
                                  181 154 50 198 249 74 209 214
                                  13 126 182 110 252 11 155 106
                                  50 19 255 204 13 198 67 146
                                  212 2 63 116 222 29 31 26
                                  184 11 237 16 60 117 150 9
                                  154 150 180 242 34 221 114 101
                                  154 87 10 208 98 48 253 97
                                  57 229 130 134 110 83 224 29))

(defun skein-get-iv (block-bits digest-bits)
  (ecase block-bits
    (256 (ecase digest-bits
           (128 +skein256-iv-128+)
           (160 +skein256-iv-160+)
           (224 +skein256-iv-224+)
           (256 +skein256-iv-256+)))
    (512 (ecase digest-bits
           (128 +skein512-iv-128+)
           (160 +skein512-iv-160+)
           (224 +skein512-iv-224+)
           (256 +skein512-iv-256+)
           (384 +skein512-iv-384+)
           (512 +skein512-iv-512+)))
    (1024 (ecase digest-bits
            (384 +skein1024-iv-384+)
            (512 +skein1024-iv-512+)
            (1024 +skein1024-iv-1024+)))))


;;; Functions to generate and update the tweak

;;; This function is called a lot by skein-ubi,
;;; so we try to optimize it for speed.
(declaim (inline skein-increment-counter))
(defun skein-increment-counter (tweak n)
  (declare (type (simple-array (unsigned-byte 64) (2)) tweak)
           (type (unsigned-byte 32) n)
           #.(burn-baby-burn))
  (let* ((x (mod64+ (aref tweak 0) n))
         (y (mod32+ (logand (aref tweak 1) #xffffffff) (if (< x n) 1 0))))
    (declare (type (unsigned-byte 64) x)
             (type (unsigned-byte 32) y))
    (setf (aref tweak 0) x
          (ldb (byte 32 0) (aref tweak 1)) y)
    (values)))

(defun skein-update-tweak (tweak &key
                                   (first nil first-p)
                                   (final nil final-p)
                                   (type nil type-p)
                                   (position nil position-p)
                                   (position-increment nil position-increment-p))
  (when first-p
    (setf (ldb (byte 1 62) (aref tweak 1)) (if first 1 0)))
  (when final-p
    (setf (ldb (byte 1 63) (aref tweak 1)) (if final 1 0)))
  (when type-p
    (setf (ldb (byte 6 56) (aref tweak 1)) type))
  (when position-p
    (setf (aref tweak 0) (ldb (byte 64 0) position))
    (setf (ldb (byte 32 0) (aref tweak 1)) (ldb (byte 32 64) position)))
  (when position-increment-p
    (skein-increment-counter tweak position-increment))
  (values))

(defun skein-make-tweak (first final type position)
  (let ((tweak (make-array 2
                           :element-type '(unsigned-byte 64)
                           :initial-element 0)))
    (skein-update-tweak tweak
                        :first first
                        :final final
                        :type type
                        :position position)
    tweak))

(defun skein-make-configuration-string (output-length)
  (let ((cfg (make-array 32
                         :element-type '(unsigned-byte 8)
                         :initial-element 0)))
    (setf (subseq cfg 0 4) #(83 72 65 51))
    (setf (subseq cfg 4 6)
          (integer-to-octets 1 :n-bits 16 :big-endian nil))
    (setf (subseq cfg 8 16)
          (integer-to-octets output-length :n-bits 64 :big-endian nil))
    cfg))


;;; UBI (unique block iteration chaining)

(defgeneric skein-value (state))
(defgeneric skein-tweak (state))
(defgeneric skein-cfg (state))
(defgeneric skein-buffer (state))
(defgeneric skein-buffer-length (state))
(defgeneric skein-cipher (state))

;;; This function is called a lot by skein-ubi,
;;; so we try to optimize it for speed.
(declaim (inline skein-update-cipher))
(defun skein-update-cipher (block-length cipher-key cipher-tweak key tweak)
  (declare (type fixnum block-length)
           (type (simple-array (unsigned-byte 64) (*)) cipher-key)
           (type (simple-array (unsigned-byte 64) (3)) cipher-tweak)
           (type (simple-array (unsigned-byte 8) (*)) key)
           (type (simple-array (unsigned-byte 64) (2)) tweak)
           #.(burn-baby-burn))
  (let ((key-words (ash block-length -3))
        (parity +threefish-key-schedule-constant+)
        (n 0))
    (declare (type (unsigned-byte 64) parity n key-words))

    ;; Update key
    (loop for i of-type fixnum from 0 below key-words do
      (setf n (ub64ref/le key (ash i 3))
            (aref cipher-key i) n
            parity (logxor parity n)))
    (setf (aref cipher-key key-words) parity)

    ;; Update tweak
    (setf (aref cipher-tweak 0) (aref tweak 0)
          (aref cipher-tweak 1) (aref tweak 1)
          (aref cipher-tweak 2) (logxor (aref tweak 0) (aref tweak 1)))
    (values)))

(defun skein-ubi (state message start end &optional final)
  (declare (type (simple-array (unsigned-byte 8) (*)) message)
           (type index start end)
           #.(burn-baby-burn))
  (let* ((cipher (skein-cipher state))
         (encryption-function (encrypt-function cipher))
         (cipher-key (threefish-key cipher))
         (cipher-tweak (threefish-tweak cipher))
         (block-length (block-length state))
         (value (skein-value state))
         (tweak (skein-tweak state))
         (buffer (skein-buffer state))
         (buffer-length (skein-buffer-length state))
         (message-start start)
         (message-length (- end start))
         (ciphertext (make-array 128
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 0))
         (n 0))
    (declare (type (simple-array (unsigned-byte 64) (*)) cipher-key)
             (type (simple-array (unsigned-byte 64) (3)) cipher-tweak)
             (type (simple-array (unsigned-byte 8) (*)) value buffer)
             (type (simple-array (unsigned-byte 8) (128)) ciphertext)
             (dynamic-extent ciphertext)
             (type (simple-array (unsigned-byte 64) (2)) tweak)
             (type (integer 0 128) block-length buffer-length n)
             (type index message-start message-length))

    ;; Try to fill the buffer with the new data
    (setf n (min message-length (- block-length buffer-length)))
    (replace buffer message
             :start1 buffer-length
             :start2 message-start
             :end2 (+ message-start n))
    (incf buffer-length n)
    (incf message-start n)
    (decf message-length n)

    ;; Process as many blocks as we can, but unless we are in the
    ;; final call, keep some data in the buffer (so that it can be
    ;; processed with the 'final' tweak flag in the final call)

    ;; Process data in buffer
    (when (and (= buffer-length block-length)
               (or final (plusp message-length)))
      (unless final
        (skein-increment-counter tweak block-length))
      (skein-update-cipher block-length cipher-key cipher-tweak value tweak)
      (funcall encryption-function cipher buffer 0 ciphertext 0)
      (skein-update-tweak tweak :first nil)
      (xor-block block-length ciphertext 0 buffer 0 value 0)
      (setf buffer-length 0))

    ;; Process data in message
    (unless final
      (loop until (<= message-length block-length) do
        (skein-increment-counter tweak block-length)
        (skein-update-cipher block-length cipher-key cipher-tweak value tweak)
        (funcall encryption-function cipher message message-start ciphertext 0)
        (xor-block block-length ciphertext 0 message message-start value 0)
        (incf message-start block-length)
        (decf message-length block-length)))

    ;; Put remaining message data in buffer
    (when (plusp message-length)
      (replace buffer message :end1 message-length :start2 message-start)
      (incf buffer-length message-length))

    ;; Save the new state
    (setf (skein-buffer-length state) buffer-length)
    (values)))

(defun skein-finalize (state digest digest-start)
  (let* ((block-length (block-length state))
         (digest-length (digest-length state))
         (tweak (skein-tweak state))
         (buffer-length (skein-buffer-length state))
         (padding-length (- block-length buffer-length))
         (padding (make-array padding-length
                              :element-type '(unsigned-byte 8)
                              :initial-element 0)))
    ;; Process remaining data after padding it
    (skein-update-tweak tweak :final t :position-increment buffer-length)
    (skein-ubi state padding 0 padding-length t)

    ;; Generate output
    (do* ((value (copy-seq (skein-value state)))
          (noutputs (ceiling digest-length block-length))
          (output (make-array (* noutputs block-length)
                              :element-type '(unsigned-byte 8)))
          (i 0 (1+ i))
          (msg (make-array block-length
                           :element-type '(unsigned-byte 8)
                           :initial-element 0)))
         ((= i noutputs)
          (progn
            (replace digest output :start1 digest-start :end2 digest-length)
            digest))
      (replace msg (integer-to-octets i :n-bits 64 :big-endian nil) :end2 8)
      (replace (skein-value state) value)
      (skein-update-tweak tweak :first t :final t :type +skein-out+ :position 8)
      (skein-ubi state msg 0 block-length t)
      (replace output (skein-value state) :start1 (* i block-length) :end2 block-length))))

(defun skein-copy-cipher (cipher &optional copy)
  (let* ((tmp-key (make-array (block-length cipher)
                              :element-type '(unsigned-byte 8)))
         (cipher-name (ecase (block-length cipher)
                        (32 :threefish256)
                        (64 :threefish512)
                        (128 :threefish1024)))
         (copy (if copy copy (make-cipher cipher-name
                                          :key tmp-key
                                          :mode :ecb))))
    (setf (threefish-key copy) (copy-seq (threefish-key cipher)))
    (setf (threefish-tweak copy) (copy-seq (threefish-tweak cipher)))
    copy))


;;; Implementation for blocks of 256 bits

(defstruct (skein256
             (:constructor %make-skein256-digest nil)
             (:copier nil))
  (value (copy-seq (skein-get-iv 256 256))
         :type (simple-array (unsigned-byte 8) (32)))
  (tweak (skein-make-tweak t nil +skein-msg+ 0)
         :type (simple-array (unsigned-byte 64) (2)))
  (cfg (skein-make-configuration-string 256)
       :type (simple-array (unsigned-byte 8) (32)))
  (buffer (make-array 32 :element-type '(unsigned-byte 8))
          :type (simple-array (unsigned-byte 8) (32)))
  (buffer-length 0 :type integer)
  (cipher (make-cipher :threefish256
                       :key (skein-get-iv 256 256)
                       :mode :ecb)))

(defstruct (skein256/128
             (:include skein256)
             (:constructor %make-skein256/128-digest
                           (&aux (value (copy-seq (skein-get-iv 256 128)))
                                 (cfg (skein-make-configuration-string 128))
                                 (cipher (make-cipher :threefish256
                                                      :key (skein-get-iv 256 128)
                                                      :mode :ecb))))
             (:copier nil)))

(defstruct (skein256/160
             (:include skein256)
             (:constructor %make-skein256/160-digest
                           (&aux (value (copy-seq (skein-get-iv 256 160)))
                                 (cfg (skein-make-configuration-string 160))
                                 (cipher (make-cipher :threefish256
                                                      :key (skein-get-iv 256 160)
                                                      :mode :ecb))))
             (:copier nil)))

(defstruct (skein256/224
             (:include skein256)
             (:constructor %make-skein256/224-digest
                           (&aux (value (copy-seq (skein-get-iv 256 224)))
                                 (cfg (skein-make-configuration-string 224))
                                 (cipher (make-cipher :threefish256
                                                      :key (skein-get-iv 256 224)
                                                      :mode :ecb))))
             (:copier nil)))

(defmethod skein-value ((state skein256))
  (skein256-value state))

(defmethod skein-tweak ((state skein256))
  (skein256-tweak state))

(defmethod skein-cfg ((state skein256))
  (skein256-cfg state))

(defmethod skein-buffer ((state skein256))
  (skein256-buffer state))

(defmethod skein-buffer-length ((state skein256))
  (skein256-buffer-length state))

(defmethod (setf skein-buffer-length) (n (state skein256))
  (setf (skein256-buffer-length state) n))

(defmethod skein-cipher ((state skein256))
  (skein256-cipher state))

(defmethod (setf skein-cipher) (cipher (state skein256))
  (setf (skein256-cipher state) cipher))

(defun %reinitialize-skein256 (state digest-bits)
  (declare (type skein256 state))
  (replace (skein256-value state) (skein-get-iv 256 digest-bits))
  (replace (skein256-tweak state) (skein-make-tweak t nil +skein-msg+ 0))
  (replace (skein256-cfg state) (skein-make-configuration-string digest-bits))
  (setf (skein256-buffer-length state) 0)
  (setf (skein256-cipher state) (make-cipher :threefish256
                                             :key (skein-get-iv 256 digest-bits)
                                             :mode :ecb))
  state)
  
(defmethod reinitialize-instance ((state skein256) &rest initargs)
  (declare (ignore initargs))
  (%reinitialize-skein256 state 256))

(defmethod reinitialize-instance ((state skein256/128) &rest initargs)
  (declare (ignore initargs))
  (%reinitialize-skein256 state 128))

(defmethod reinitialize-instance ((state skein256/160) &rest initargs)
  (declare (ignore initargs))
  (%reinitialize-skein256 state 160))

(defmethod reinitialize-instance ((state skein256/224) &rest initargs)
  (declare (ignore initargs))
  (%reinitialize-skein256 state 224))

(defmethod copy-digest ((state skein256) &optional copy)
  (check-type copy (or null skein256))
  (let ((copy (if copy
                  copy
                  (etypecase state
                    (skein256/128 (%make-skein256/128-digest))
                    (skein256/160 (%make-skein256/160-digest))
                    (skein256/224 (%make-skein256/224-digest))
                    (skein256 (%make-skein256-digest))))))
    (declare (type skein256 copy))
    (replace (skein256-value copy) (skein256-value state))
    (replace (skein256-tweak copy) (skein256-tweak state))
    (replace (skein256-cfg copy) (skein256-cfg state))
    (replace (skein256-buffer copy) (skein256-buffer state))
    (setf (skein256-buffer-length copy) (skein256-buffer-length state))
    (setf (skein256-cipher copy) (skein-copy-cipher (skein256-cipher state)))
    copy))

(define-digest-updater skein256
  (skein-ubi state sequence start end))

(define-digest-finalizer ((skein256 32)
                          (skein256/224 28)
                          (skein256/160 20)
                          (skein256/128 16))
  (skein-finalize state digest digest-start))

(defdigest skein256 :digest-length 32 :block-length 32)
(defdigest skein256/128 :digest-length 16 :block-length 32)
(defdigest skein256/160 :digest-length 20 :block-length 32)
(defdigest skein256/224 :digest-length 28 :block-length 32)


;;; Implementation for blocks of 512 bits

(defstruct (skein512
             (:constructor %make-skein512-digest nil)
             (:copier nil))
  (value (copy-seq (skein-get-iv 512 512))
         :type (simple-array (unsigned-byte 8) (64)))
  (tweak (skein-make-tweak t nil +skein-msg+ 0)
         :type (simple-array (unsigned-byte 64) (2)))
  (cfg (skein-make-configuration-string 512)
       :type (simple-array (unsigned-byte 8) (32)))
  (buffer (make-array 64 :element-type '(unsigned-byte 8))
          :type (simple-array (unsigned-byte 8) (64)))
  (buffer-length 0 :type integer)
  (cipher (make-cipher :threefish512
                       :key (skein-get-iv 512 512)
                       :mode :ecb)))

(defstruct (skein512/128
             (:include skein512)
             (:constructor %make-skein512/128-digest
                           (&aux (value (copy-seq (skein-get-iv 512 128)))
                                 (cfg (skein-make-configuration-string 128))
                                 (cipher (make-cipher :threefish512
                                                      :key (skein-get-iv 512 128)
                                                      :mode :ecb))))
             (:copier nil)))

(defstruct (skein512/160
             (:include skein512)
             (:constructor %make-skein512/160-digest
                           (&aux (value (copy-seq (skein-get-iv 512 160)))
                                 (cfg (skein-make-configuration-string 160))
                                 (cipher (make-cipher :threefish512
                                                      :key (skein-get-iv 512 160)
                                                      :mode :ecb))))
             (:copier nil)))

(defstruct (skein512/224
             (:include skein512)
             (:constructor %make-skein512/224-digest
                           (&aux (value (copy-seq (skein-get-iv 512 224)))
                                 (cfg (skein-make-configuration-string 224))
                                 (cipher (make-cipher :threefish512
                                                      :key (skein-get-iv 512 224)
                                                      :mode :ecb))))
             (:copier nil)))

(defstruct (skein512/256
             (:include skein512)
             (:constructor %make-skein512/256-digest
                           (&aux (value (copy-seq (skein-get-iv 512 256)))
                                 (cfg (skein-make-configuration-string 256))
                                 (cipher (make-cipher :threefish512
                                                      :key (skein-get-iv 512 256)
                                                      :mode :ecb))))
             (:copier nil)))

(defstruct (skein512/384
             (:include skein512)
             (:constructor %make-skein512/384-digest
                           (&aux (value (copy-seq (skein-get-iv 512 384)))
                                 (cfg (skein-make-configuration-string 384))
                                 (cipher (make-cipher :threefish512
                                                      :key (skein-get-iv 512 384)
                                                      :mode :ecb))))
             (:copier nil)))

(defmethod skein-value ((state skein512))
  (skein512-value state))

(defmethod skein-tweak ((state skein512))
  (skein512-tweak state))

(defmethod skein-cfg ((state skein512))
  (skein512-cfg state))

(defmethod skein-buffer ((state skein512))
  (skein512-buffer state))

(defmethod skein-buffer-length ((state skein512))
  (skein512-buffer-length state))

(defmethod (setf skein-buffer-length) (n (state skein512))
  (setf (skein512-buffer-length state) n))

(defmethod skein-cipher ((state skein512))
  (skein512-cipher state))

(defmethod (setf skein-cipher) (cipher (state skein512))
  (setf (skein512-cipher state) cipher))

(defun %reinitialize-skein512 (state digest-bits)
  (declare (type skein512 state))
  (replace (skein512-value state) (skein-get-iv 512 digest-bits))
  (replace (skein512-tweak state) (skein-make-tweak t nil +skein-msg+ 0))
  (replace (skein512-cfg state) (skein-make-configuration-string digest-bits))
  (setf (skein512-buffer-length state) 0)
  (setf (skein512-cipher state) (make-cipher :threefish512
                                             :key (skein-get-iv 512 digest-bits)
                                             :mode :ecb))
  state)

(defmethod reinitialize-instance ((state skein512) &rest initargs)
  (declare (ignore initargs))
  (%reinitialize-skein512 state 512))

(defmethod reinitialize-instance ((state skein512/128) &rest initargs)
  (declare (ignore initargs))
  (%reinitialize-skein512 state 128))

(defmethod reinitialize-instance ((state skein512/160) &rest initargs)
  (declare (ignore initargs))
  (%reinitialize-skein512 state 160))

(defmethod reinitialize-instance ((state skein512/224) &rest initargs)
  (declare (ignore initargs))
  (%reinitialize-skein512 state 224))

(defmethod reinitialize-instance ((state skein512/256) &rest initargs)
  (declare (ignore initargs))
  (%reinitialize-skein512 state 256))

(defmethod reinitialize-instance ((state skein512/384) &rest initargs)
  (declare (ignore initargs))
  (%reinitialize-skein512 state 384))

(defmethod copy-digest ((state skein512) &optional copy)
  (declare (type (or null skein512) copy))
  (let ((copy (if copy
                  copy
                  (etypecase state
                    (skein512/128 (%make-skein512/128-digest))
                    (skein512/160 (%make-skein512/160-digest))
                    (skein512/224 (%make-skein512/224-digest))
                    (skein512/256 (%make-skein512/256-digest))
                    (skein512/384 (%make-skein512/384-digest))
                    (skein512 (%make-skein512-digest))))))
    (declare (type skein512 copy))
    (replace (skein512-value copy) (skein512-value state))
    (replace (skein512-tweak copy) (skein512-tweak state))
    (replace (skein512-cfg copy) (skein512-cfg state))
    (replace (skein512-buffer copy) (skein512-buffer state))
    (setf (skein512-buffer-length copy) (skein512-buffer-length state))
    (setf (skein512-cipher copy) (skein-copy-cipher (skein512-cipher state)))
    copy))

(define-digest-updater skein512
  (skein-ubi state sequence start end))

(define-digest-finalizer ((skein512 64)
                          (skein512/128 16)
                          (skein512/160 20)
                          (skein512/224 28)
                          (skein512/256 32)
                          (skein512/384 48))
  (skein-finalize state digest digest-start))

(defdigest skein512 :digest-length 64 :block-length 64)
(defdigest skein512/128 :digest-length 16 :block-length 64)
(defdigest skein512/160 :digest-length 20 :block-length 64)
(defdigest skein512/224 :digest-length 28 :block-length 64)
(defdigest skein512/256 :digest-length 32 :block-length 64)
(defdigest skein512/384 :digest-length 48 :block-length 64)


;;; Implementation for blocks of 1024 bits

(defstruct (skein1024
             (:constructor %make-skein1024-digest nil)
             (:copier nil))
  (value (copy-seq (skein-get-iv 1024 1024))
         :type (simple-array (unsigned-byte 8) (128)))
  (tweak (skein-make-tweak t nil +skein-msg+ 0)
         :type (simple-array (unsigned-byte 64) (2)))
  (cfg (skein-make-configuration-string 1024)
       :type (simple-array (unsigned-byte 8) (32)))
  (buffer (make-array 128 :element-type '(unsigned-byte 8))
          :type (simple-array (unsigned-byte 8) (128)))
  (buffer-length 0 :type integer)
  (cipher (make-cipher :threefish1024
                       :key (skein-get-iv 1024 1024)
                       :mode :ecb)))

(defstruct (skein1024/384
             (:include skein1024)
             (:constructor %make-skein1024/384-digest
                           (&aux (value (copy-seq (skein-get-iv 1024 384)))
                                 (cfg (skein-make-configuration-string 384))
                                 (cipher (make-cipher :threefish1024
                                                      :key (skein-get-iv 1024 384)
                                                      :mode :ecb))))
             (:copier nil)))

(defstruct (skein1024/512
             (:include skein1024)
             (:constructor %make-skein1024/512-digest
                           (&aux (value (copy-seq (skein-get-iv 1024 512)))
                                 (cfg (skein-make-configuration-string 512))
                                 (cipher (make-cipher :threefish1024
                                                      :key (skein-get-iv 1024 512)
                                                      :mode :ecb))))
             (:copier nil)))

(defmethod skein-value ((state skein1024))
  (skein1024-value state))

(defmethod skein-tweak ((state skein1024))
  (skein1024-tweak state))

(defmethod skein-cfg ((state skein1024))
  (skein1024-cfg state))

(defmethod skein-buffer ((state skein1024))
  (skein1024-buffer state))

(defmethod skein-buffer-length ((state skein1024))
  (skein1024-buffer-length state))

(defmethod (setf skein-buffer-length) (n (state skein1024))
  (setf (skein1024-buffer-length state) n))

(defmethod skein-cipher ((state skein1024))
  (skein1024-cipher state))

(defmethod (setf skein-cipher) (cipher (state skein1024))
  (setf (skein1024-cipher state) cipher))

(defun %reinitialize-skein1024 (state digest-bits)
  (declare (type skein1024 state))
  (replace (skein1024-value state) (skein-get-iv 1024 digest-bits))
  (replace (skein1024-tweak state) (skein-make-tweak t nil +skein-msg+ 0))
  (replace (skein1024-cfg state) (skein-make-configuration-string digest-bits))
  (setf (skein1024-buffer-length state) 0)
  (setf (skein1024-cipher state) (make-cipher :threefish1024
                                              :key (skein-get-iv 1024 digest-bits)
                                              :mode :ecb))
  state)

(defmethod reinitialize-instance ((state skein1024) &rest initargs)
  (declare (ignore initargs))
  (%reinitialize-skein1024 state 1024))

(defmethod reinitialize-instance ((state skein1024/384) &rest initargs)
  (declare (ignore initargs))
  (%reinitialize-skein1024 state 384))

(defmethod reinitialize-instance ((state skein1024/512) &rest initargs)
  (declare (ignore initargs))
  (%reinitialize-skein1024 state 512))

(defmethod copy-digest ((state skein1024) &optional copy)
  (declare (type (or null skein1024) copy))
  (let ((copy (if copy
                  copy
                  (etypecase state
                    (skein1024/384 (%make-skein1024/384-digest))
                    (skein1024/512 (%make-skein1024/512-digest))
                    (skein1024 (%make-skein1024-digest))))))
    (declare (type skein1024 copy))
    (replace (skein1024-value copy) (skein1024-value state))
    (replace (skein1024-tweak copy) (skein1024-tweak state))
    (replace (skein1024-cfg copy) (skein1024-cfg state))
    (replace (skein1024-buffer copy) (skein1024-buffer state))
    (setf (skein1024-buffer-length copy) (skein1024-buffer-length state))
    (setf (skein1024-cipher copy) (skein-copy-cipher (skein1024-cipher state)))
    copy))

(define-digest-updater skein1024
  (skein-ubi state sequence start end))

(define-digest-finalizer ((skein1024 128)
                          (skein1024/384 48)
                          (skein1024/512 64))
  (skein-finalize state digest digest-start))

(defdigest skein1024 :digest-length 128 :block-length 128)
(defdigest skein1024/384 :digest-length 48 :block-length 128)
(defdigest skein1024/512 :digest-length 64 :block-length 128)
