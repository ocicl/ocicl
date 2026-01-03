;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; crc24.lisp

(in-package :crypto)
(in-ironclad-readtable)

(declaim (type (simple-array (unsigned-byte 32) (256)) +crc24-table+))
(defconst +crc24-table+
#32@(#x00000000 #x00864CFB #x008AD50D #x000C99F6 #x0093E6E1 #x0015AA1A
#x001933EC #x009F7F17 #x00A18139 #x0027CDC2 #x002B5434 #x00AD18CF
#x003267D8 #x00B42B23 #x00B8B2D5 #x003EFE2E #x00C54E89 #x00430272
#x004F9B84 #x00C9D77F #x0056A868 #x00D0E493 #x00DC7D65 #x005A319E
#x0064CFB0 #x00E2834B #x00EE1ABD #x00685646 #x00F72951 #x007165AA
#x007DFC5C #x00FBB0A7 #x000CD1E9 #x008A9D12 #x008604E4 #x0000481F
#x009F3708 #x00197BF3 #x0015E205 #x0093AEFE #x00AD50D0 #x002B1C2B
#x002785DD #x00A1C926 #x003EB631 #x00B8FACA #x00B4633C #x00322FC7
#x00C99F60 #x004FD39B #x00434A6D #x00C50696 #x005A7981 #x00DC357A
#x00D0AC8C #x0056E077 #x00681E59 #x00EE52A2 #x00E2CB54 #x006487AF
#x00FBF8B8 #x007DB443 #x00712DB5 #x00F7614E #x0019A3D2 #x009FEF29
#x009376DF #x00153A24 #x008A4533 #x000C09C8 #x0000903E #x0086DCC5
#x00B822EB #x003E6E10 #x0032F7E6 #x00B4BB1D #x002BC40A #x00AD88F1
#x00A11107 #x00275DFC #x00DCED5B #x005AA1A0 #x00563856 #x00D074AD
#x004F0BBA #x00C94741 #x00C5DEB7 #x0043924C #x007D6C62 #x00FB2099
#x00F7B96F #x0071F594 #x00EE8A83 #x0068C678 #x00645F8E #x00E21375
#x0015723B #x00933EC0 #x009FA736 #x0019EBCD #x008694DA #x0000D821
#x000C41D7 #x008A0D2C #x00B4F302 #x0032BFF9 #x003E260F #x00B86AF4
#x002715E3 #x00A15918 #x00ADC0EE #x002B8C15 #x00D03CB2 #x00567049
#x005AE9BF #x00DCA544 #x0043DA53 #x00C596A8 #x00C90F5E #x004F43A5
#x0071BD8B #x00F7F170 #x00FB6886 #x007D247D #x00E25B6A #x00641791
#x00688E67 #x00EEC29C #x003347A4 #x00B50B5F #x00B992A9 #x003FDE52
#x00A0A145 #x0026EDBE #x002A7448 #x00AC38B3 #x0092C69D #x00148A66
#x00181390 #x009E5F6B #x0001207C #x00876C87 #x008BF571 #x000DB98A
#x00F6092D #x007045D6 #x007CDC20 #x00FA90DB #x0065EFCC #x00E3A337
#x00EF3AC1 #x0069763A #x00578814 #x00D1C4EF #x00DD5D19 #x005B11E2
#x00C46EF5 #x0042220E #x004EBBF8 #x00C8F703 #x003F964D #x00B9DAB6
#x00B54340 #x00330FBB #x00AC70AC #x002A3C57 #x0026A5A1 #x00A0E95A
#x009E1774 #x00185B8F #x0014C279 #x00928E82 #x000DF195 #x008BBD6E
#x00872498 #x00016863 #x00FAD8C4 #x007C943F #x00700DC9 #x00F64132
#x00693E25 #x00EF72DE #x00E3EB28 #x0065A7D3 #x005B59FD #x00DD1506
#x00D18CF0 #x0057C00B #x00C8BF1C #x004EF3E7 #x00426A11 #x00C426EA
#x002AE476 #x00ACA88D #x00A0317B #x00267D80 #x00B90297 #x003F4E6C
#x0033D79A #x00B59B61 #x008B654F #x000D29B4 #x0001B042 #x0087FCB9
#x001883AE #x009ECF55 #x009256A3 #x00141A58 #x00EFAAFF #x0069E604
#x00657FF2 #x00E33309 #x007C4C1E #x00FA00E5 #x00F69913 #x0070D5E8
#x004E2BC6 #x00C8673D #x00C4FECB #x0042B230 #x00DDCD27 #x005B81DC
#x0057182A #x00D154D1 #x0026359F #x00A07964 #x00ACE092 #x002AAC69
#x00B5D37E #x00339F85 #x003F0673 #x00B94A88 #x0087B4A6 #x0001F85D
#x000D61AB #x008B2D50 #x00145247 #x00921EBC #x009E874A #x0018CBB1
#x00E37B16 #x006537ED #x0069AE1B #x00EFE2E0 #x00709DF7 #x00F6D10C
#x00FA48FA #x007C0401 #x0042FA2F #x00C4B6D4 #x00C82F22 #x004E63D9
#x00D11CCE #x00575035 #x005BC9C3 #x00DD8538))

(defstruct (crc24
             (:constructor %make-crc24-digest nil)
             (:constructor %make-crc24-state (crc))
             (:copier nil))
  (crc #xb704ce :type (unsigned-byte 32)))

(defmethod reinitialize-instance ((state crc24) &rest initargs)
  (declare (ignore initargs))
  (setf (crc24-crc state) #xb704ce)
  state)

(defmethod copy-digest ((state crc24) &optional copy)
  (check-type copy (or null crc24))
  (cond
    (copy
     (setf (crc24-crc copy) (crc24-crc state))
     copy)
    (t
     (%make-crc24-state (crc24-crc state)))))

(define-digest-updater crc24
  (let ((crc (crc24-crc state)))
    (declare (type (unsigned-byte 32) crc))
    (do ((i start (1+ i))
         (table +crc24-table+))
        ((>= i end)
         (setf (crc24-crc state) (ldb (byte 24 0) crc))
         state)
      (setf crc (logxor (aref table
                              (logand (logxor (mod32ash crc -16)
                                              (aref sequence i))
                                      #xff))
                        (mod32ash crc 8))))))

(define-digest-finalizer (crc24 3)
  (flet ((stuff-state (crc digest start)
           (declare (type (simple-array (unsigned-byte 8) (*)) digest))
           (declare (type (integer 0 #.(- array-dimension-limit 3)) start))
           (setf (aref digest (+ start 0)) (ldb (byte 8 16) crc)
                 (aref digest (+ start 1)) (ldb (byte 8 8) crc)
                 (aref digest (+ start 2)) (ldb (byte 8 0) crc))
           digest))
    (declare (inline stuff-state))
    (stuff-state (crc24-crc state) digest digest-start)))

(defdigest crc24 :digest-length 3 :block-length 1)
