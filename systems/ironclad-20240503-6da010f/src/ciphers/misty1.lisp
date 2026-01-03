;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; misty1.lisp -- implementation of the MISTY1 block cipher from RFC 2994

(in-package :crypto)
(in-ironclad-readtable)


;;; required tables

(defconst +misty1-s7table+
#8@(#x1B #x32 #x33 #x5A #x3B #x10 #x17 #x54 #x5B #x1A #x72 #x73 #x6B
#x2C #x66 #x49 #x1F #x24 #x13 #x6C #x37 #x2E #x3F #x4A #x5D #x0F
#x40 #x56 #x25 #x51 #x1C #x04 #x0B #x46 #x20 #x0D #x7B #x35 #x44
#x42 #x2B #x1E #x41 #x14 #x4B #x79 #x15 #x6F #x0E #x55 #x09 #x36
#x74 #x0C #x67 #x53 #x28 #x0A #x7E #x38 #x02 #x07 #x60 #x29 #x19
#x12 #x65 #x2F #x30 #x39 #x08 #x68 #x5F #x78 #x2A #x4C #x64 #x45
#x75 #x3D #x59 #x48 #x03 #x57 #x7C #x4F #x62 #x3C #x1D #x21 #x5E
#x27 #x6A #x70 #x4D #x3A #x01 #x6D #x6E #x63 #x18 #x77 #x23 #x05
#x26 #x76 #x00 #x31 #x2D #x7A #x7F #x61 #x50 #x22 #x11 #x06 #x47
#x16 #x52 #x4E #x71 #x3E #x69 #x43 #x34 #x5C #x58 #x7D))

(defconst +misty1-s9table+
#16@(#x01C3 #x00CB #x0153 #x019F #x01E3 #x00E9 #x00FB #x0035 #x0181 #x00B9
#x0117 #x01EB #x0133 #x0009 #x002D #x00D3 #x00C7 #x014A #x0037 #x007E
#x00EB #x0164 #x0193 #x01D8 #x00A3 #x011E #x0055 #x002C #x001D #x01A2
#x0163 #x0118 #x014B #x0152 #x01D2 #x000F #x002B #x0030 #x013A #x00E5
#x0111 #x0138 #x018E #x0063 #x00E3 #x00C8 #x01F4 #x001B #x0001 #x009D
#x00F8 #x01A0 #x016D #x01F3 #x001C #x0146 #x007D #x00D1 #x0082 #x01EA
#x0183 #x012D #x00F4 #x019E #x01D3 #x00DD #x01E2 #x0128 #x01E0 #x00EC
#x0059 #x0091 #x0011 #x012F #x0026 #x00DC #x00B0 #x018C #x010F #x01F7
#x00E7 #x016C #x00B6 #x00F9 #x00D8 #x0151 #x0101 #x014C #x0103 #x00B8
#x0154 #x012B #x01AE #x0017 #x0071 #x000C #x0047 #x0058 #x007F #x01A4
#x0134 #x0129 #x0084 #x015D #x019D #x01B2 #x01A3 #x0048 #x007C #x0051
#x01CA #x0023 #x013D #x01A7 #x0165 #x003B #x0042 #x00DA #x0192 #x00CE
#x00C1 #x006B #x009F #x01F1 #x012C #x0184 #x00FA #x0196 #x01E1 #x0169
#x017D #x0031 #x0180 #x010A #x0094 #x01DA #x0186 #x013E #x011C #x0060
#x0175 #x01CF #x0067 #x0119 #x0065 #x0068 #x0099 #x0150 #x0008 #x0007
#x017C #x00B7 #x0024 #x0019 #x00DE #x0127 #x00DB #x00E4 #x01A9 #x0052
#x0109 #x0090 #x019C #x01C1 #x0028 #x01B3 #x0135 #x016A #x0176 #x00DF
#x01E5 #x0188 #x00C5 #x016E #x01DE #x01B1 #x00C3 #x01DF #x0036 #x00EE
#x01EE #x00F0 #x0093 #x0049 #x009A #x01B6 #x0069 #x0081 #x0125 #x000B
#x005E #x00B4 #x0149 #x01C7 #x0174 #x003E #x013B #x01B7 #x008E #x01C6
#x00AE #x0010 #x0095 #x01EF #x004E #x00F2 #x01FD #x0085 #x00FD #x00F6
#x00A0 #x016F #x0083 #x008A #x0156 #x009B #x013C #x0107 #x0167 #x0098
#x01D0 #x01E9 #x0003 #x01FE #x00BD #x0122 #x0089 #x00D2 #x018F #x0012
#x0033 #x006A #x0142 #x00ED #x0170 #x011B #x00E2 #x014F #x0158 #x0131
#x0147 #x005D #x0113 #x01CD #x0079 #x0161 #x01A5 #x0179 #x009E #x01B4
#x00CC #x0022 #x0132 #x001A #x00E8 #x0004 #x0187 #x01ED #x0197 #x0039
#x01BF #x01D7 #x0027 #x018B #x00C6 #x009C #x00D0 #x014E #x006C #x0034
#x01F2 #x006E #x00CA #x0025 #x00BA #x0191 #x00FE #x0013 #x0106 #x002F
#x01AD #x0172 #x01DB #x00C0 #x010B #x01D6 #x00F5 #x01EC #x010D #x0076
#x0114 #x01AB #x0075 #x010C #x01E4 #x0159 #x0054 #x011F #x004B #x00C4
#x01BE #x00F7 #x0029 #x00A4 #x000E #x01F0 #x0077 #x004D #x017A #x0086
#x008B #x00B3 #x0171 #x00BF #x010E #x0104 #x0097 #x015B #x0160 #x0168
#x00D7 #x00BB #x0066 #x01CE #x00FC #x0092 #x01C5 #x006F #x0016 #x004A
#x00A1 #x0139 #x00AF #x00F1 #x0190 #x000A #x01AA #x0143 #x017B #x0056
#x018D #x0166 #x00D4 #x01FB #x014D #x0194 #x019A #x0087 #x01F8 #x0123
#x00A7 #x01B8 #x0141 #x003C #x01F9 #x0140 #x002A #x0155 #x011A #x01A1
#x0198 #x00D5 #x0126 #x01AF #x0061 #x012E #x0157 #x01DC #x0072 #x018A
#x00AA #x0096 #x0115 #x00EF #x0045 #x007B #x008D #x0145 #x0053 #x005F
#x0178 #x00B2 #x002E #x0020 #x01D5 #x003F #x01C9 #x01E7 #x01AC #x0044
#x0038 #x0014 #x00B1 #x016B #x00AB #x00B5 #x005A #x0182 #x01C8 #x01D4
#x0018 #x0177 #x0064 #x00CF #x006D #x0100 #x0199 #x0130 #x015A #x0005
#x0120 #x01BB #x01BD #x00E0 #x004F #x00D6 #x013F #x01C4 #x012A #x0015
#x0006 #x00FF #x019B #x00A6 #x0043 #x0088 #x0050 #x015F #x01E8 #x0121
#x0073 #x017E #x00BC #x00C2 #x00C9 #x0173 #x0189 #x01F5 #x0074 #x01CC
#x01E6 #x01A8 #x0195 #x001F #x0041 #x000D #x01BA #x0032 #x003D #x01D1
#x0080 #x00A8 #x0057 #x01B9 #x0162 #x0148 #x00D9 #x0105 #x0062 #x007A
#x0021 #x01FF #x0112 #x0108 #x01C0 #x00A9 #x011D #x01B0 #x01A6 #x00CD
#x00F3 #x005C #x0102 #x005B #x01D9 #x0144 #x01F6 #x00AD #x00A5 #x003A
#x01CB #x0136 #x017F #x0046 #x00E1 #x001E #x01DD #x00E6 #x0137 #x01FA
#x0185 #x008C #x008F #x0040 #x01B5 #x00BE #x0078 #x0000 #x00AC #x0110
#x015E #x0124 #x0002 #x01BC #x00A2 #x00EA #x0070 #x01FC #x0116 #x015C
#x004C #x01C2))

;;; types and context definition

(deftype misty1-round-keys () '(simple-array (unsigned-byte 16) (32)))

(defclass misty1 (cipher 8-byte-block-mixin)
  ((round-keys :accessor round-keys :type misty1-round-keys)))

;;; block functions and key expansion

;;; Declaring these inline produces screwy results in SBCL (bug?).
(declaim (notinline fi fl fl-inv fo))

(defun fi (fi-in fi-key)
  (declare (type (unsigned-byte 16) fi-in fi-key))
  (let ((d9 (ash fi-in -7))
        (d7 (logand fi-in #x7f)))
    (declare (type (unsigned-byte 16) d9 d7))
    (setf d9 (logxor d7 (aref +misty1-s9table+ d9))
          d7 (logxor d9 (aref +misty1-s7table+ d7)))
    (setf d7 (logand d7 #x7f))
    (setf d7 (logxor d7 (ash fi-key -9))
          d9 (logxor d9 (logand fi-key #x1ff)))
    (setf d9 (logxor d7 (aref +misty1-s9table+ d9)))
    (ldb (byte 16 0) (logior (ash d7 9) d9))))

(defun fl (d0 d1 keys round)
  (declare (type misty1-round-keys keys))
  (declare (type (unsigned-byte 16) d0 d1))
  (cond
    ((evenp round)
     (let* ((d1 (logxor d1 (logand d0 (aref keys (truncate round 2)))))
            (d0 (logxor d0 (logior d1 (aref keys (+ (mod (+ (truncate round 2) 6) 8) 8))))))
       (values d0 d1)))
    (t
     (let* ((d1 (logxor d1 (logand d0 (aref keys (+ (mod (+ (truncate (1- round) 2) 2) 8) 8)))))
            (d0 (logxor d0 (logior d1 (aref keys (mod (+ (truncate (1- round) 2) 4) 8))))))
       (values d0 d1)))))

(defun fl-inv (d0 d1 keys round)
  (declare (type misty1-round-keys keys))
  (declare (type (unsigned-byte 16) d0 d1))
  (cond
    ((evenp round)
     (let* ((d0 (logxor d0 (logior d1 (aref keys (+ (mod (+ (truncate round 2) 6) 8) 8)))))
            (d1 (logxor d1 (logand d0 (aref keys (truncate round 2))))))
       (values d0 d1)))
    (t
     (let* ((d0 (logxor d0 (logior d1 (aref keys (mod (+ (truncate (1- round) 2) 4) 8)))))
            (d1 (logxor d1 (logand d0 (aref keys (+ (mod (+ (truncate (1- round) 2) 2) 8) 8))))))
       (values d0 d1)))))

(defun fo (t0 t1 keys round)
  (declare (type misty1-round-keys keys))
  (declare (type (unsigned-byte 16) t0 t1))
  (setf t0 (logxor t0 (aref keys round))
        t0 (fi t0 (aref keys (+ (mod (+ round 5) 8) 8)))
        t0 (logxor t0 t1)
        t1 (logxor t1 (aref keys (mod (+ round 2) 8)))
        t1 (fi t1 (aref keys (+ (mod (+ round 1) 8) 8)))
        t1 (logxor t1 t0)
        t0 (logxor t0 (aref keys (mod (+ round 7) 8)))
        t0 (fi t0 (aref keys (+ (mod (+ round 3) 8) 8)))
        t0 (logxor t0 t1)
        t1 (logxor t1 (aref keys (mod (+ round 4) 8))))
  (values t1 t0))

(define-block-encryptor misty1 8
  (let ((round-keys (round-keys context)))
    (with-words ((d00 d01 d10 d11) plaintext plaintext-start :size 2)
      #.(loop for i from 0 below 8
              if (evenp i)
                collect `(progn
                          (multiple-value-setq (d00 d01) (fl d00 d01 round-keys ,i))
                          (multiple-value-setq (d10 d11) (fl d10 d11 round-keys (1+ ,i)))
                          (multiple-value-bind (t0 t1) (fo d00 d01 round-keys ,i)
                            (declare (type (unsigned-byte 16) t0 t1))
                            (multiple-value-setq (d10 d11)
                              (values (logxor d10 t0) (logxor d11 t1))))) into forms
              else
                collect `(multiple-value-bind (t0 t1) (fo d10 d11 round-keys ,i)
                          (declare (type (unsigned-byte 16) t0 t1))
                          (multiple-value-setq (d00 d01)
                            (values (logxor d00 t0) (logxor d01 t1)))) into forms
              finally (return `(progn
                                ,@forms
                                (multiple-value-setq (d00 d01) (fl d00 d01 round-keys 8))
                                (multiple-value-setq (d10 d11) (fl d10 d11 round-keys 9)))))
      (store-words ciphertext ciphertext-start d10 d11 d00 d01))))

(define-block-decryptor misty1 8
  (let ((round-keys (round-keys context)))
    (with-words ((d10 d11 d00 d01) ciphertext ciphertext-start :size 2)
      #.(loop for i from 7 downto 0
              if (evenp i)
                collect `(progn
                          (multiple-value-bind (t0 t1) (fo d00 d01 round-keys ,i)
                            (declare (type (unsigned-byte 16) t0 t1))
                            (multiple-value-setq (d10 d11)
                              (values (logxor d10 t0) (logxor d11 t1))))
                          (multiple-value-setq (d00 d01) (fl-inv d00 d01 round-keys ,i))
                          (multiple-value-setq (d10 d11) (fl-inv d10 d11 round-keys (1+ ,i)))) into forms
              else
                collect `(multiple-value-bind (t0 t1) (fo d10 d11 round-keys ,i)
                          (declare (type (unsigned-byte 16) t0 t1))
                          (multiple-value-setq (d00 d01)
                            (values (logxor d00 t0) (logxor d01 t1)))) into forms
              finally (return `(progn
                                (multiple-value-setq (d00 d01) (fl-inv d00 d01 round-keys 8))
                                (multiple-value-setq (d10 d11) (fl-inv d10 d11 round-keys 9))
                                ,@forms)))
      (store-words plaintext plaintext-start d00 d01 d10 d11))))

(defun misty1-expand-key (key)
  (declare (type (simple-array (unsigned-byte 8) (16)) key))
  (let ((key-schedule (make-array 32 :element-type '(unsigned-byte 16))))
    (declare (type (simple-array (unsigned-byte 16) (32)) key-schedule))
    ;; fill in the expanded key schedule
    (loop for i from 0 below 16 by 2
          for j from 0 below 8
          do (setf (aref key-schedule j) (ub16ref/be key i)))
    ;; scramble
    (dotimes (i 8 key-schedule)
      (setf (aref key-schedule (+ i 8)) (fi (aref key-schedule i)
                                          (aref key-schedule (mod (1+ i) 8)))
            (aref key-schedule (+ i 16)) (logand (aref key-schedule (+ i 8))
                                               #x01ff)
            (aref key-schedule (+ i 24)) (ash (aref key-schedule (+ i 8)) -9)))))

(defmethod schedule-key ((cipher misty1) key)
  (let ((round-keys (misty1-expand-key key)))
    (setf (round-keys cipher) round-keys)
    cipher))

(defcipher misty1
  (:encrypt-function misty1-encrypt-block)
  (:decrypt-function misty1-decrypt-block)
  (:block-length 8)
  (:key-length (:fixed 16)))
