;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; sosemanuk.lisp - implementation of the Sosemanuk stream cipher

(in-package :crypto)


(defconst +sosemanuk-mul-a+
  (make-array 256
              :element-type '(unsigned-byte 32)
              :initial-contents '(#x00000000 #xE19FCF13 #x6B973726 #x8A08F835
                                  #xD6876E4C #x3718A15F #xBD10596A #x5C8F9679
                                  #x05A7DC98 #xE438138B #x6E30EBBE #x8FAF24AD
                                  #xD320B2D4 #x32BF7DC7 #xB8B785F2 #x59284AE1
                                  #x0AE71199 #xEB78DE8A #x617026BF #x80EFE9AC
                                  #xDC607FD5 #x3DFFB0C6 #xB7F748F3 #x566887E0
                                  #x0F40CD01 #xEEDF0212 #x64D7FA27 #x85483534
                                  #xD9C7A34D #x38586C5E #xB250946B #x53CF5B78
                                  #x1467229B #xF5F8ED88 #x7FF015BD #x9E6FDAAE
                                  #xC2E04CD7 #x237F83C4 #xA9777BF1 #x48E8B4E2
                                  #x11C0FE03 #xF05F3110 #x7A57C925 #x9BC80636
                                  #xC747904F #x26D85F5C #xACD0A769 #x4D4F687A
                                  #x1E803302 #xFF1FFC11 #x75170424 #x9488CB37
                                  #xC8075D4E #x2998925D #xA3906A68 #x420FA57B
                                  #x1B27EF9A #xFAB82089 #x70B0D8BC #x912F17AF
                                  #xCDA081D6 #x2C3F4EC5 #xA637B6F0 #x47A879E3
                                  #x28CE449F #xC9518B8C #x435973B9 #xA2C6BCAA
                                  #xFE492AD3 #x1FD6E5C0 #x95DE1DF5 #x7441D2E6
                                  #x2D699807 #xCCF65714 #x46FEAF21 #xA7616032
                                  #xFBEEF64B #x1A713958 #x9079C16D #x71E60E7E
                                  #x22295506 #xC3B69A15 #x49BE6220 #xA821AD33
                                  #xF4AE3B4A #x1531F459 #x9F390C6C #x7EA6C37F
                                  #x278E899E #xC611468D #x4C19BEB8 #xAD8671AB
                                  #xF109E7D2 #x109628C1 #x9A9ED0F4 #x7B011FE7
                                  #x3CA96604 #xDD36A917 #x573E5122 #xB6A19E31
                                  #xEA2E0848 #x0BB1C75B #x81B93F6E #x6026F07D
                                  #x390EBA9C #xD891758F #x52998DBA #xB30642A9
                                  #xEF89D4D0 #x0E161BC3 #x841EE3F6 #x65812CE5
                                  #x364E779D #xD7D1B88E #x5DD940BB #xBC468FA8
                                  #xE0C919D1 #x0156D6C2 #x8B5E2EF7 #x6AC1E1E4
                                  #x33E9AB05 #xD2766416 #x587E9C23 #xB9E15330
                                  #xE56EC549 #x04F10A5A #x8EF9F26F #x6F663D7C
                                  #x50358897 #xB1AA4784 #x3BA2BFB1 #xDA3D70A2
                                  #x86B2E6DB #x672D29C8 #xED25D1FD #x0CBA1EEE
                                  #x5592540F #xB40D9B1C #x3E056329 #xDF9AAC3A
                                  #x83153A43 #x628AF550 #xE8820D65 #x091DC276
                                  #x5AD2990E #xBB4D561D #x3145AE28 #xD0DA613B
                                  #x8C55F742 #x6DCA3851 #xE7C2C064 #x065D0F77
                                  #x5F754596 #xBEEA8A85 #x34E272B0 #xD57DBDA3
                                  #x89F22BDA #x686DE4C9 #xE2651CFC #x03FAD3EF
                                  #x4452AA0C #xA5CD651F #x2FC59D2A #xCE5A5239
                                  #x92D5C440 #x734A0B53 #xF942F366 #x18DD3C75
                                  #x41F57694 #xA06AB987 #x2A6241B2 #xCBFD8EA1
                                  #x977218D8 #x76EDD7CB #xFCE52FFE #x1D7AE0ED
                                  #x4EB5BB95 #xAF2A7486 #x25228CB3 #xC4BD43A0
                                  #x9832D5D9 #x79AD1ACA #xF3A5E2FF #x123A2DEC
                                  #x4B12670D #xAA8DA81E #x2085502B #xC11A9F38
                                  #x9D950941 #x7C0AC652 #xF6023E67 #x179DF174
                                  #x78FBCC08 #x9964031B #x136CFB2E #xF2F3343D
                                  #xAE7CA244 #x4FE36D57 #xC5EB9562 #x24745A71
                                  #x7D5C1090 #x9CC3DF83 #x16CB27B6 #xF754E8A5
                                  #xABDB7EDC #x4A44B1CF #xC04C49FA #x21D386E9
                                  #x721CDD91 #x93831282 #x198BEAB7 #xF81425A4
                                  #xA49BB3DD #x45047CCE #xCF0C84FB #x2E934BE8
                                  #x77BB0109 #x9624CE1A #x1C2C362F #xFDB3F93C
                                  #xA13C6F45 #x40A3A056 #xCAAB5863 #x2B349770
                                  #x6C9CEE93 #x8D032180 #x070BD9B5 #xE69416A6
                                  #xBA1B80DF #x5B844FCC #xD18CB7F9 #x301378EA
                                  #x693B320B #x88A4FD18 #x02AC052D #xE333CA3E
                                  #xBFBC5C47 #x5E239354 #xD42B6B61 #x35B4A472
                                  #x667BFF0A #x87E43019 #x0DECC82C #xEC73073F
                                  #xB0FC9146 #x51635E55 #xDB6BA660 #x3AF46973
                                  #x63DC2392 #x8243EC81 #x084B14B4 #xE9D4DBA7
                                  #xB55B4DDE #x54C482CD #xDECC7AF8 #x3F53B5EB)))

(defconst +sosemanuk-mul-ia+
  (make-array 256
              :element-type '(unsigned-byte 32)
              :initial-contents '(#x00000000 #x180F40CD #x301E8033 #x2811C0FE
                                  #x603CA966 #x7833E9AB #x50222955 #x482D6998
                                  #xC078FBCC #xD877BB01 #xF0667BFF #xE8693B32
                                  #xA04452AA #xB84B1267 #x905AD299 #x88559254
                                  #x29F05F31 #x31FF1FFC #x19EEDF02 #x01E19FCF
                                  #x49CCF657 #x51C3B69A #x79D27664 #x61DD36A9
                                  #xE988A4FD #xF187E430 #xD99624CE #xC1996403
                                  #x89B40D9B #x91BB4D56 #xB9AA8DA8 #xA1A5CD65
                                  #x5249BE62 #x4A46FEAF #x62573E51 #x7A587E9C
                                  #x32751704 #x2A7A57C9 #x026B9737 #x1A64D7FA
                                  #x923145AE #x8A3E0563 #xA22FC59D #xBA208550
                                  #xF20DECC8 #xEA02AC05 #xC2136CFB #xDA1C2C36
                                  #x7BB9E153 #x63B6A19E #x4BA76160 #x53A821AD
                                  #x1B854835 #x038A08F8 #x2B9BC806 #x339488CB
                                  #xBBC11A9F #xA3CE5A52 #x8BDF9AAC #x93D0DA61
                                  #xDBFDB3F9 #xC3F2F334 #xEBE333CA #xF3EC7307
                                  #xA492D5C4 #xBC9D9509 #x948C55F7 #x8C83153A
                                  #xC4AE7CA2 #xDCA13C6F #xF4B0FC91 #xECBFBC5C
                                  #x64EA2E08 #x7CE56EC5 #x54F4AE3B #x4CFBEEF6
                                  #x04D6876E #x1CD9C7A3 #x34C8075D #x2CC74790
                                  #x8D628AF5 #x956DCA38 #xBD7C0AC6 #xA5734A0B
                                  #xED5E2393 #xF551635E #xDD40A3A0 #xC54FE36D
                                  #x4D1A7139 #x551531F4 #x7D04F10A #x650BB1C7
                                  #x2D26D85F #x35299892 #x1D38586C #x053718A1
                                  #xF6DB6BA6 #xEED42B6B #xC6C5EB95 #xDECAAB58
                                  #x96E7C2C0 #x8EE8820D #xA6F942F3 #xBEF6023E
                                  #x36A3906A #x2EACD0A7 #x06BD1059 #x1EB25094
                                  #x569F390C #x4E9079C1 #x6681B93F #x7E8EF9F2
                                  #xDF2B3497 #xC724745A #xEF35B4A4 #xF73AF469
                                  #xBF179DF1 #xA718DD3C #x8F091DC2 #x97065D0F
                                  #x1F53CF5B #x075C8F96 #x2F4D4F68 #x37420FA5
                                  #x7F6F663D #x676026F0 #x4F71E60E #x577EA6C3
                                  #xE18D0321 #xF98243EC #xD1938312 #xC99CC3DF
                                  #x81B1AA47 #x99BEEA8A #xB1AF2A74 #xA9A06AB9
                                  #x21F5F8ED #x39FAB820 #x11EB78DE #x09E43813
                                  #x41C9518B #x59C61146 #x71D7D1B8 #x69D89175
                                  #xC87D5C10 #xD0721CDD #xF863DC23 #xE06C9CEE
                                  #xA841F576 #xB04EB5BB #x985F7545 #x80503588
                                  #x0805A7DC #x100AE711 #x381B27EF #x20146722
                                  #x68390EBA #x70364E77 #x58278E89 #x4028CE44
                                  #xB3C4BD43 #xABCBFD8E #x83DA3D70 #x9BD57DBD
                                  #xD3F81425 #xCBF754E8 #xE3E69416 #xFBE9D4DB
                                  #x73BC468F #x6BB30642 #x43A2C6BC #x5BAD8671
                                  #x1380EFE9 #x0B8FAF24 #x239E6FDA #x3B912F17
                                  #x9A34E272 #x823BA2BF #xAA2A6241 #xB225228C
                                  #xFA084B14 #xE2070BD9 #xCA16CB27 #xD2198BEA
                                  #x5A4C19BE #x42435973 #x6A52998D #x725DD940
                                  #x3A70B0D8 #x227FF015 #x0A6E30EB #x12617026
                                  #x451FD6E5 #x5D109628 #x750156D6 #x6D0E161B
                                  #x25237F83 #x3D2C3F4E #x153DFFB0 #x0D32BF7D
                                  #x85672D29 #x9D686DE4 #xB579AD1A #xAD76EDD7
                                  #xE55B844F #xFD54C482 #xD545047C #xCD4A44B1
                                  #x6CEF89D4 #x74E0C919 #x5CF109E7 #x44FE492A
                                  #x0CD320B2 #x14DC607F #x3CCDA081 #x24C2E04C
                                  #xAC977218 #xB49832D5 #x9C89F22B #x8486B2E6
                                  #xCCABDB7E #xD4A49BB3 #xFCB55B4D #xE4BA1B80
                                  #x17566887 #x0F59284A #x2748E8B4 #x3F47A879
                                  #x776AC1E1 #x6F65812C #x477441D2 #x5F7B011F
                                  #xD72E934B #xCF21D386 #xE7301378 #xFF3F53B5
                                  #xB7123A2D #xAF1D7AE0 #x870CBA1E #x9F03FAD3
                                  #x3EA637B6 #x26A9777B #x0EB8B785 #x16B7F748
                                  #x5E9A9ED0 #x4695DE1D #x6E841EE3 #x768B5E2E
                                  #xFEDECC7A #xE6D18CB7 #xCEC04C49 #xD6CF0C84
                                  #x9EE2651C #x86ED25D1 #xAEFCE52F #xB6F3A5E2)))

(defmacro sosemanuk-s0 (x0 x1 x2 x3 x4)
  `(setf ,x3 (logxor ,x3 ,x0)
         ,x4 ,x1
         ,x1 (logand ,x1 ,x3)
         ,x4 (logxor ,x4 ,x2)
         ,x1 (logxor ,x1 ,x0)
         ,x0 (logior ,x0 ,x3)
         ,x0 (logxor ,x0 ,x4)
         ,x4 (logxor ,x4 ,x3)
         ,x3 (logxor ,x3 ,x2)
         ,x2 (logior ,x2 ,x1)
         ,x2 (logxor ,x2 ,x4)
         ,x4 (mod32lognot ,x4)
         ,x4 (logior ,x4 ,x1)
         ,x1 (logxor ,x1 ,x3)
         ,x1 (logxor ,x1 ,x4)
         ,x3 (logior ,x3 ,x0)
         ,x1 (logxor ,x1 ,x3)
         ,x4 (logxor ,x4 ,x3)))

(defmacro sosemanuk-s1 (x0 x1 x2 x3 x4)
  `(setf ,x0 (mod32lognot ,x0)
         ,x2 (mod32lognot ,x2)
         ,x4 ,x0
         ,x0 (logand ,x0 ,x1)
         ,x2 (logxor ,x2 ,x0)
         ,x0 (logior ,x0 ,x3)
         ,x3 (logxor ,x3 ,x2)
         ,x1 (logxor ,x1 ,x0)
         ,x0 (logxor ,x0 ,x4)
         ,x4 (logior ,x4 ,x1)
         ,x1 (logxor ,x1 ,x3)
         ,x2 (logior ,x2 ,x0)
         ,x2 (logand ,x2 ,x4)
         ,x0 (logxor ,x0 ,x1)
         ,x1 (logand ,x1 ,x2)
         ,x1 (logxor ,x1 ,x0)
         ,x0 (logand ,x0 ,x2)
         ,x0 (logxor ,x0 ,x4)))

(defmacro sosemanuk-s2 (x0 x1 x2 x3 x4)
  `(setf ,x4 ,x0
         ,x0 (logand ,x0 ,x2)
         ,x0 (logxor ,x0 ,x3)
         ,x2 (logxor ,x2 ,x1)
         ,x2 (logxor ,x2 ,x0)
         ,x3 (logior ,x3 ,x4)
         ,x3 (logxor ,x3 ,x1)
         ,x4 (logxor ,x4 ,x2)
         ,x1 ,x3
         ,x3 (logior ,x3 ,x4)
         ,x3 (logxor ,x3 ,x0)
         ,x0 (logand ,x0 ,x1)
         ,x4 (logxor ,x4 ,x0)
         ,x1 (logxor ,x1 ,x3)
         ,x1 (logxor ,x1 ,x4)
         ,x4 (mod32lognot ,x4)))

(defmacro sosemanuk-s3 (x0 x1 x2 x3 x4)
  `(setf ,x4 ,x0
         ,x0 (logior ,x0 ,x3)
         ,x3 (logxor ,x3 ,x1)
         ,x1 (logand ,x1 ,x4)
         ,x4 (logxor ,x4 ,x2)
         ,x2 (logxor ,x2 ,x3)
         ,x3 (logand ,x3 ,x0)
         ,x4 (logior ,x4 ,x1)
         ,x3 (logxor ,x3 ,x4)
         ,x0 (logxor ,x0 ,x1)
         ,x4 (logand ,x4 ,x0)
         ,x1 (logxor ,x1 ,x3)
         ,x4 (logxor ,x4 ,x2)
         ,x1 (logior ,x1 ,x0)
         ,x1 (logxor ,x1 ,x2)
         ,x0 (logxor ,x0 ,x3)
         ,x2 ,x1
         ,x1 (logior ,x1 ,x3)
         ,x1 (logxor ,x1 ,x0)))

(defmacro sosemanuk-s4 (x0 x1 x2 x3 x4)
  `(setf ,x1 (logxor ,x1 ,x3)
         ,x3 (mod32lognot ,x3)
         ,x2 (logxor ,x2 ,x3)
         ,x3 (logxor ,x3 ,x0)
         ,x4 ,x1
         ,x1 (logand ,x1 ,x3)
         ,x1 (logxor ,x1 ,x2)
         ,x4 (logxor ,x4 ,x3)
         ,x0 (logxor ,x0 ,x4)
         ,x2 (logand ,x2 ,x4)
         ,x2 (logxor ,x2 ,x0)
         ,x0 (logand ,x0 ,x1)
         ,x3 (logxor ,x3 ,x0)
         ,x4 (logior ,x4 ,x1)
         ,x4 (logxor ,x4 ,x0)
         ,x0 (logior ,x0 ,x3)
         ,x0 (logxor ,x0 ,x2)
         ,x2 (logand ,x2 ,x3)
         ,x0 (mod32lognot ,x0)
         ,x4 (logxor ,x4 ,x2)))

(defmacro sosemanuk-s5 (x0 x1 x2 x3 x4)
  `(setf ,x0 (logxor ,x0 ,x1)
         ,x1 (logxor ,x1 ,x3)
         ,x3 (mod32lognot ,x3)
         ,x4 ,x1
         ,x1 (logand ,x1 ,x0)
         ,x2 (logxor ,x2 ,x3)
         ,x1 (logxor ,x1 ,x2)
         ,x2 (logior ,x2 ,x4)
         ,x4 (logxor ,x4 ,x3)
         ,x3 (logand ,x3 ,x1)
         ,x3 (logxor ,x3 ,x0)
         ,x4 (logxor ,x4 ,x1)
         ,x4 (logxor ,x4 ,x2)
         ,x2 (logxor ,x2 ,x0)
         ,x0 (logand ,x0 ,x3)
         ,x2 (mod32lognot ,x2)
         ,x0 (logxor ,x0 ,x4)
         ,x4 (logior ,x4 ,x3)
         ,x2 (logxor ,x2 ,x4)))

(defmacro sosemanuk-s6 (x0 x1 x2 x3 x4)
  `(setf ,x2 (mod32lognot ,x2)
         ,x4 ,x3
         ,x3 (logand ,x3 ,x0)
         ,x0 (logxor ,x0 ,x4)
         ,x3 (logxor ,x3 ,x2)
         ,x2 (logior ,x2 ,x4)
         ,x1 (logxor ,x1 ,x3)
         ,x2 (logxor ,x2 ,x0)
         ,x0 (logior ,x0 ,x1)
         ,x2 (logxor ,x2 ,x1)
         ,x4 (logxor ,x4 ,x0)
         ,x0 (logior ,x0 ,x3)
         ,x0 (logxor ,x0 ,x2)
         ,x4 (logxor ,x4 ,x3)
         ,x4 (logxor ,x4 ,x0)
         ,x3 (mod32lognot ,x3)
         ,x2 (logand ,x2 ,x4)
         ,x2 (logxor ,x2 ,x3)))

(defmacro sosemanuk-s7 (x0 x1 x2 x3 x4)
  `(setf ,x4 ,x1
         ,x1 (logior ,x1 ,x2)
         ,x1 (logxor ,x1 ,x3)
         ,x4 (logxor ,x4 ,x2)
         ,x2 (logxor ,x2 ,x1)
         ,x3 (logior ,x3 ,x4)
         ,x3 (logand ,x3 ,x0)
         ,x4 (logxor ,x4 ,x2)
         ,x3 (logxor ,x3 ,x1)
         ,x1 (logior ,x1 ,x4)
         ,x1 (logxor ,x1 ,x0)
         ,x0 (logior ,x0 ,x4)
         ,x0 (logxor ,x0 ,x2)
         ,x1 (logxor ,x1 ,x4)
         ,x2 (logxor ,x2 ,x1)
         ,x1 (logand ,x1 ,x0)
         ,x1 (logxor ,x1 ,x4)
         ,x2 (mod32lognot ,x2)
         ,x2 (logior ,x2 ,x0)
         ,x4 (logxor ,x4 ,x2)))

(defmacro sosemanuk-lt (x0 x1 x2 x3)
  `(setf ,x0 (rol32 ,x0 13)
         ,x2 (rol32 ,x2 3)
         ,x1 (logxor ,x1 ,x0 ,x2)
         ,x3 (logxor ,x3 ,x2 (mod32ash ,x0 3))
         ,x1 (rol32 ,x1 1)
         ,x3 (rol32 ,x3 7)
         ,x0 (logxor ,x0 ,x1 ,x3)
         ,x2 (logxor ,x2 ,x3 (mod32ash ,x1 7))
         ,x0 (rol32 ,x0 5)
         ,x2 (rol32 ,x2 22)))

(defclass sosemanuk (stream-cipher)
  ((state :accessor sosemanuk-state
          :initform (make-array 10 :element-type '(unsigned-byte 32))
          :type (simple-array (unsigned-byte 32) (10)))
   (state-r :accessor sosemanuk-state-r
            :initform (make-array 2 :element-type '(unsigned-byte 32))
            :type (simple-array (unsigned-byte 32) (2)))
   (keystream-buffer :accessor sosemanuk-keystream-buffer
                     :initform (make-array 80 :element-type '(unsigned-byte 8))
                     :type (simple-array (unsigned-byte 8) (80)))
   (keystream-buffer-remaining :accessor sosemanuk-keystream-buffer-remaining
                               :initform 0
                               :type (integer 0 80))
   (subkeys :accessor sosemanuk-subkeys
            :type (or (simple-array (unsigned-byte 32) (100)) null))))

(defmethod schedule-key ((cipher sosemanuk) key)
  (let ((key-length (length key))
        (subkeys (make-array 100 :element-type '(unsigned-byte 32)))
        (buffer (make-array 32 :element-type '(unsigned-byte 8)))
        (w0 0)
        (w1 0)
        (w2 0)
        (w3 0)
        (w4 0)
        (w5 0)
        (w6 0)
        (w7 0)
        (i 0))
    (declare (type (simple-array (unsigned-byte 32) (100)) subkeys)
             (type (simple-array (unsigned-byte 8) (32)) buffer)
             (type (unsigned-byte 32) w0 w1 w2 w3 w4 w5 w6 w7)
             (type fixnum key-length i))
    (replace buffer key :end2 key-length)
    (when (< key-length 32)
      (setf (aref buffer key-length) 1)
      (when (< key-length 31)
        (fill buffer 0 :start (1+ key-length))))
    (setf w0 (ub32ref/le buffer 0)
          w1 (ub32ref/le buffer 4)
          w2 (ub32ref/le buffer 8)
          w3 (ub32ref/le buffer 12)
          w4 (ub32ref/le buffer 16)
          w5 (ub32ref/le buffer 20)
          w6 (ub32ref/le buffer 24)
          w7 (ub32ref/le buffer 28))

    (macrolet ((sks (s o0 o1 o2 o3 d0 d1 d2 d3)
                 `(let ((r0 ,(symbolicate '#:w o0))
                        (r1 ,(symbolicate '#:w o1))
                        (r2 ,(symbolicate '#:w o2))
                        (r3 ,(symbolicate '#:w o3))
                        (r4 0))
                    (declare (type (unsigned-byte 32) r0 r1 r2 r3))
                    (,s r0 r1 r2 r3 r4)
                    (setf (aref subkeys i) ,(symbolicate '#:r d0))
                    (incf i)
                    (setf (aref subkeys i) ,(symbolicate '#:r d1))
                    (incf i)
                    (setf (aref subkeys i) ,(symbolicate '#:r d2))
                    (incf i)
                    (setf (aref subkeys i) ,(symbolicate '#:r d3))
                    (incf i)))
               (sks0 ()
                 `(sks sosemanuk-s0 4 5 6 7 1 4 2 0))
               (sks1 ()
                 `(sks sosemanuk-s1 0 1 2 3 2 0 3 1))
               (sks2 ()
                 `(sks sosemanuk-s2 4 5 6 7 2 3 1 4))
               (sks3 ()
                 `(sks sosemanuk-s3 0 1 2 3 1 2 3 4))
               (sks4 ()
                 `(sks sosemanuk-s4 4 5 6 7 1 4 0 3))
               (sks5 ()
                 `(sks sosemanuk-s5 0 1 2 3 1 3 0 2))
               (sks6 ()
                 `(sks sosemanuk-s6 4 5 6 7 0 1 4 2))
               (sks7 ()
                 `(sks sosemanuk-s7 0 1 2 3 4 3 1 0))
               (wup (wi wi5 wi3 wi1 cc)
                 `(setf ,wi (rol32 (logxor ,wi ,wi5 ,wi3 ,wi1 ,cc #x9e3779b9) 11)))
               (wup0 (cc)
                 `(progn
                    (wup w0 w3 w5 w7 ,cc)
                    (wup w1 w4 w6 w0 ,(+ cc 1))
                    (wup w2 w5 w7 w1 ,(+ cc 2))
                    (wup w3 w6 w0 w2 ,(+ cc 3))))
               (wup1 (cc)
                 `(progn
                    (wup w4 w7 w1 w3 ,cc)
                    (wup w5 w0 w2 w4 ,(+ cc 1))
                    (wup w6 w1 w3 w5 ,(+ cc 2))
                    (wup w7 w2 w4 w6 ,(+ cc 3)))))
      (wup0 0) (sks3)
      (wup1 4) (sks2)
      (wup0 8) (sks1)
      (wup1 12) (sks0)
      (wup0 16) (sks7)
      (wup1 20) (sks6)
      (wup0 24) (sks5)
      (wup1 28) (sks4)
      (wup0 32) (sks3)
      (wup1 36) (sks2)
      (wup0 40) (sks1)
      (wup1 44) (sks0)
      (wup0 48) (sks7)
      (wup1 52) (sks6)
      (wup0 56) (sks5)
      (wup1 60) (sks4)
      (wup0 64) (sks3)
      (wup1 68) (sks2)
      (wup0 72) (sks1)
      (wup1 76) (sks0)
      (wup0 80) (sks7)
      (wup1 84) (sks6)
      (wup0 88) (sks5)
      (wup1 92) (sks4)
      (wup0 96) (sks3)
      (setf (sosemanuk-subkeys cipher) subkeys)))
  cipher)

(defmethod shared-initialize :after ((cipher sosemanuk) slot-names &rest initargs &key initialization-vector &allow-other-keys)
  (declare (ignore slot-names initargs))
  (let ((state (sosemanuk-state cipher))
        (state-r (sosemanuk-state-r cipher))
        (subkeys (sosemanuk-subkeys cipher))
        (r0 0)
        (r1 0)
        (r2 0)
        (r3 0)
        (r4 0))
    (declare (type (simple-array (unsigned-byte 32) (*)) state state-r subkeys)
             (type (unsigned-byte 32) r0 r1 r2 r3 r4))
    (when initialization-vector
      (if (= (length initialization-vector) 16)
          (setf r0 (ub32ref/le initialization-vector 0)
                r1 (ub32ref/le initialization-vector 4)
                r2 (ub32ref/le initialization-vector 8)
                r3 (ub32ref/le initialization-vector 12))
          (error 'invalid-initialization-vector
                 :cipher (class-name (class-of cipher))
                 :block-length 16)))

    (macrolet ((ka (zc x0 x1 x2 x3)
                 `(setf ,x0 (logxor ,x0 (aref subkeys ,zc))
                        ,x1 (logxor ,x1 (aref subkeys ,(+ zc 1)))
                        ,x2 (logxor ,x2 (aref subkeys ,(+ zc 2)))
                        ,x3 (logxor ,x3 (aref subkeys ,(+ zc 3)))))
               (fss (zc s i0 i1 i2 i3 i4 o0 o1 o2 o3)
                 `(progn
                    (ka ,zc
                        ,(symbolicate '#:r i0)
                        ,(symbolicate '#:r i1)
                        ,(symbolicate '#:r i2)
                        ,(symbolicate '#:r i3))
                    (,s ,(symbolicate '#:r i0)
                        ,(symbolicate '#:r i1)
                        ,(symbolicate '#:r i2)
                        ,(symbolicate '#:r i3)
                        ,(symbolicate '#:r i4))
                    (sosemanuk-lt ,(symbolicate '#:r o0)
                                  ,(symbolicate '#:r o1)
                                  ,(symbolicate '#:r o2)
                                  ,(symbolicate '#:r o3))))
               (fsf (zc s i0 i1 i2 i3 i4 o0 o1 o2 o3)
                 `(progn
                    (fss ,zc ,s ,i0 ,i1 ,i2 ,i3 ,i4 ,o0 ,o1 ,o2 ,o3)
                    (ka ,(+ zc 4)
                        ,(symbolicate '#:r o0)
                        ,(symbolicate '#:r o1)
                        ,(symbolicate '#:r o2)
                        ,(symbolicate '#:r o3)))))
      (fss 0 sosemanuk-s0 0 1 2 3 4 1 4 2 0)
      (fss 4 sosemanuk-s1 1 4 2 0 3 2 1 0 4)
      (fss 8 sosemanuk-s2 2 1 0 4 3 0 4 1 3)
      (fss 12 sosemanuk-s3 0 4 1 3 2 4 1 3 2)
      (fss 16 sosemanuk-s4 4 1 3 2 0 1 0 4 2)
      (fss 20 sosemanuk-s5 1 0 4 2 3 0 2 1 4)
      (fss 24 sosemanuk-s6 0 2 1 4 3 0 2 3 1)
      (fss 28 sosemanuk-s7 0 2 3 1 4 4 1 2 0)
      (fss 32 sosemanuk-s0 4 1 2 0 3 1 3 2 4)
      (fss 36 sosemanuk-s1 1 3 2 4 0 2 1 4 3)
      (fss 40 sosemanuk-s2 2 1 4 3 0 4 3 1 0)
      (fss 44 sosemanuk-s3 4 3 1 0 2 3 1 0 2)
      (setf (aref state 9) r3
            (aref state 8) r1
            (aref state 7) r0
            (aref state 6) r2)
      (fss 48 sosemanuk-s4 3 1 0 2 4 1 4 3 2)
      (fss 52 sosemanuk-s5 1 4 3 2 0 4 2 1 3)
      (fss 56 sosemanuk-s6 4 2 1 3 0 4 2 0 1)
      (fss 60 sosemanuk-s7 4 2 0 1 3 3 1 2 4)
      (fss 64 sosemanuk-s0 3 1 2 4 0 1 0 2 3)
      (fss 68 sosemanuk-s1 1 0 2 3 4 2 1 3 0)
      (setf (aref state-r 0) r2
            (aref state 4) r1
            (aref state-r 1) r3
            (aref state 5) r0)
      (fss 72 sosemanuk-s2 2 1 3 0 4 3 0 1 4)
      (fss 76 sosemanuk-s3 3 0 1 4 2 0 1 4 2)
      (fss 80 sosemanuk-s4 0 1 4 2 3 1 3 0 2)
      (fss 84 sosemanuk-s5 1 3 0 2 4 3 2 1 0)
      (fss 88 sosemanuk-s6 3 2 1 0 4 3 2 4 1)
      (fsf 92 sosemanuk-s7 3 2 4 1 0 0 1 2 3)
      (setf (aref state 3) r0
            (aref state 2) r1
            (aref state 1) r2
            (aref state 0) r3))

    (fill subkeys 0)
    (setf (sosemanuk-subkeys cipher) nil
          (sosemanuk-keystream-buffer-remaining cipher) 0))
  cipher)

(defun sosemanuk-compute-block (state state-r buffer)
  (declare (type (simple-array (unsigned-byte 32) (*)) state state-r)
           (type (simple-array (unsigned-byte 8) (80)) buffer)
           (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((s0 (aref state 0))
        (s1 (aref state 1))
        (s2 (aref state 2))
        (s3 (aref state 3))
        (s4 (aref state 4))
        (s5 (aref state 5))
        (s6 (aref state 6))
        (s7 (aref state 7))
        (s8 (aref state 8))
        (s9 (aref state 9))
        (r1 (aref state-r 0))
        (r2 (aref state-r 1))
        (u0 0)
        (u1 0)
        (u2 0)
        (u3 0)
        (u4 0)
        (v0 0)
        (v1 0)
        (v2 0)
        (v3 0))
    (declare (type (unsigned-byte 32) s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 r1 r2)
             (type (unsigned-byte 32) u0 u1 u2 u3 u4 v0 v1 v2 v3))
    (macrolet ((mul-a (x)
                 `(logxor (mod32ash ,x 8) (aref +sosemanuk-mul-a+ (mod32ash ,x -24))))
               (mul-g (x)
                 `(logxor (mod32ash ,x -8) (aref +sosemanuk-mul-ia+ (logand ,x 255))))
               (xmux (c x y)
                 `(if (zerop (logand ,c 1)) ,x (logxor ,x ,y)))
               (fsm (x1 x8)
                 `(let ((tt 0)
                        (or1 0))
                    (declare (type (unsigned-byte 32) tt or1))
                    (setf tt (xmux r1 ,(symbolicate '#:s x1) ,(symbolicate '#:s x8))
                          or1 r1
                          r1 (mod32+ r2 tt)
                          tt (mod32* or1 #x54655307)
                          r2 (rol32 tt 7))))
               (lru (x0 x3 x9 dd)
                 `(setf ,dd ,(symbolicate '#:s x0)
                        ,(symbolicate '#:s x0) (logxor (mul-a ,(symbolicate '#:s x0))
                                                       (mul-g ,(symbolicate '#:s x3))
                                                       ,(symbolicate '#:s x9))))
               (cc1 (x9 ee)
                 `(setf ,ee (logxor (mod32+ ,(symbolicate '#:s x9) r1) r2)))
               (stp (x0 x1 x3 x8 x9 dd ee)
                 `(progn
                    (fsm ,x1 ,x8)
                    (lru ,x0 ,x3 ,x9 ,dd)
                    (cc1 ,x9 ,ee)))
               (srd (s x0 x1 x2 x3 ooff)
                 `(progn
                    (,s u0 u1 u2 u3 u4)
                    (setf (ub32ref/le buffer ,ooff) (logxor ,(symbolicate '#:u x0) v0)
                          (ub32ref/le buffer ,(+ ooff 4)) (logxor ,(symbolicate '#:u x1) v1)
                          (ub32ref/le buffer ,(+ ooff 8)) (logxor ,(symbolicate '#:u x2) v2)
                          (ub32ref/le buffer ,(+ ooff 12)) (logxor ,(symbolicate '#:u x3) v3)))))
      (stp 0 1 3 8 9 v0 u0)
      (stp 1 2 4 9 0 v1 u1)
      (stp 2 3 5 0 1 v2 u2)
      (stp 3 4 6 1 2 v3 u3)
      (srd sosemanuk-s2 2 3 1 4 0)
      (stp 4 5 7 2 3 v0 u0)
      (stp 5 6 8 3 4 v1 u1)
      (stp 6 7 9 4 5 v2 u2)
      (stp 7 8 0 5 6 v3 u3)
      (srd sosemanuk-s2 2 3 1 4 16)
      (stp 8 9 1 6 7 v0 u0)
      (stp 9 0 2 7 8 v1 u1)
      (stp 0 1 3 8 9 v2 u2)
      (stp 1 2 4 9 0 v3 u3)
      (srd sosemanuk-s2 2 3 1 4 32)
      (stp 2 3 5 0 1 v0 u0)
      (stp 3 4 6 1 2 v1 u1)
      (stp 4 5 7 2 3 v2 u2)
      (stp 5 6 8 3 4 v3 u3)
      (srd sosemanuk-s2 2 3 1 4 48)
      (stp 6 7 9 4 5 v0 u0)
      (stp 7 8 0 5 6 v1 u1)
      (stp 8 9 1 6 7 v2 u2)
      (stp 9 0 2 7 8 v3 u3)
      (srd sosemanuk-s2 2 3 1 4 64)

      (setf (aref state 0) s0
            (aref state 1) s1
            (aref state 2) s2
            (aref state 3) s3
            (aref state 4) s4
            (aref state 5) s5
            (aref state 6) s6
            (aref state 7) s7
            (aref state 8) s8
            (aref state 9) s9
            (aref state-r 0) r1
            (aref state-r 1) r2)))
  (values))

(define-stream-cryptor sosemanuk
  (let ((state (sosemanuk-state context))
        (state-r (sosemanuk-state-r context))
        (keystream-buffer (sosemanuk-keystream-buffer context))
        (keystream-buffer-remaining (sosemanuk-keystream-buffer-remaining context)))
    (declare (type (simple-array (unsigned-byte 32) (*)) state state-r)
             (type (simple-array (unsigned-byte 8) (80)) keystream-buffer)
             (type (integer 0 80) keystream-buffer-remaining))
    (unless (zerop length)
      (unless (zerop keystream-buffer-remaining)
        (let ((size (min length keystream-buffer-remaining)))
          (declare (type (integer 0 80) size))
          (xor-block size keystream-buffer (- 80 keystream-buffer-remaining)
                     plaintext plaintext-start
                     ciphertext ciphertext-start)
          (decf keystream-buffer-remaining size)
          (decf length size)
          (incf ciphertext-start size)
          (incf plaintext-start size)))
      (unless (zerop length)
        (loop
          (sosemanuk-compute-block state state-r keystream-buffer)
          (when (<= length 80)
            (xor-block length keystream-buffer 0 plaintext plaintext-start ciphertext ciphertext-start)
            (setf (sosemanuk-keystream-buffer-remaining context) (- 80 length))
            (return-from sosemanuk-crypt (values)))
          (xor-block 80 keystream-buffer 0 plaintext plaintext-start ciphertext ciphertext-start)
          (decf length 80)
          (incf ciphertext-start 80)
          (incf plaintext-start 80)))
      (setf (sosemanuk-keystream-buffer-remaining context) keystream-buffer-remaining))
    (values)))

(defcipher sosemanuk
  (:mode :stream)
  (:crypt-function sosemanuk-crypt)
  (:key-length (:variable 16 32 1)))
