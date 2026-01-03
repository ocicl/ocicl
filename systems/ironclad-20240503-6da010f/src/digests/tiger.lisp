;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; tiger.lisp -- implementation of the Tiger digest function

(in-package :crypto)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant +tiger-wordsize+
  #-(and sbcl x86-64)
  32
  #+(and sbcl x86-64)
  64)
(defconstant +tiger-block-n-words+
  (if (= +tiger-wordsize+ 32)
      16
      8))
(defconstant +tiger-block-copy-fn+
  (if (= +tiger-wordsize+ 32)
      'fill-block-ub8-le
      'fill-block-ub8-le/64)))

(deftype tiger-state-block ()
  `(simple-array (unsigned-byte ,+tiger-wordsize+)
                 (,+tiger-block-n-words+)))
(deftype tiger-sbox ()
  (let ((n-words (truncate 64 +tiger-wordsize+)))
    `(simple-array (unsigned-byte ,+tiger-wordsize+)
                   (,(* n-words 256)))))

#.(cl:if (cl:= +tiger-wordsize+ 32)
         '(define-digest-registers (tiger :endian :little)
           (a0 #x89abcdef)
           (a1 #x01234567)
           (b0 #x76543210)
           (b1 #xfedcba98)
           (c0 #xc3b2e187)
           (c1 #xf096a5b4))
         '(define-digest-registers (tiger :endian :little :size 8)
           (a #x0123456789abcdef)
           (b #xfedcba9876543210)
           (c #xf096a5b4c3b2e187)))

(defconst +pristine-tiger-registers+ (initial-tiger-regs))

(declaim (type tiger-sbox tiger-t1 tiger-t2 tiger-t3 tiger-t4))
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun make-tiger-sbox (elements)
  (let ((n-elements (length elements)))
    (if (= +tiger-wordsize+ 32)
        (make-array n-elements :element-type '(unsigned-byte 32)
                    :initial-contents elements)
        (loop for (xlo xhi . rest) on elements by #'cddr
           collect (logior (ash xhi 32) xlo) into contents
           finally (return
                     (make-array (truncate n-elements 2)
                                 :element-type '(unsigned-byte 64)
                                 :initial-contents contents)))))))
(defconst tiger-t1
  (make-tiger-sbox
   '(#xF7E90C5E #x02AAB17C #xE243A8EC #xAC424B03 
     #x0DD5FCD3 #x72CD5BE3 #xF6F97F3A #x6D019B93 
     #xD21F9193 #xCD9978FF #x708029E2 #x7573A1C9 
     #x922A83C3 #xB164326B #x04915870 #x46883EEE 
     #x7103ECE6 #xEAACE305 #x08A3535C #xC54169B8 
     #x8DDEC47C #x4CE75491 #xDC0DF40C #x0AA2F4DF 
     #xA74DBEFA #x10B76F18 #x5AD1AB6A #xC6CCB623 
     #x572FE2FF #x13726121 #x199D921E #x1A488C6F 
     #xDA0007CA #x4BC9F9F4 #xE85241C7 #x26F5E6F6 
     #xEA5947B6 #x859079DB #xC99E8C92 #x4F1885C5 
     #xA96F864B #xD78E761E #x52B5C17D #x8E36428C 
     #x373063C1 #x69CF6827 #x9BB4C56E #xB607C93D 
     #x0E76B5EA #x7D820E76 #xF07FDC42 #x645C9CC6 
     #x243342E0 #xBF38A078 #x9D2E7D04 #x5F6B343C 
     #x600B0EC6 #xF2C28AEB #x7254BCAC #x6C0ED85F 
     #xA4DB4FE5 #x71592281 #xCE0FED9F #x1967FA69 
     #xB96545DB #xFD5293F8 #xF2A7600B #xC879E9D7 
     #x0193194E #x86024892 #x2D9CC0B3 #xA4F9533B 
     #x15957613 #x9053836C #xFC357BF1 #xDB6DCF8A 
     #x7A370F57 #x18BEEA7A #x50B99066 #x037117CA 
     #x74424A35 #x6AB30A97 #xE325249B #xF4E92F02 
     #x061CCAE1 #x7739DB07 #xECA42A05 #xD8F3B49C 
     #x51382F73 #xBD56BE3F #x43B0BB28 #x45FAED58 
     #x11BF1F83 #x1C813D5C #xD75FA169 #x8AF0E4B6 
     #x87AD9999 #x33EE18A4 #xB1C94410 #x3C26E8EA 
     #xC0A822F9 #xB510102B #x0CE6123B #x141EEF31 
     #x59DDB154 #xFC65B900 #xC5E0E607 #xE0158640 
     #x26C3A3CF #x884E0798 #x23C535FD #x930D0D95 
     #x4E9A2B00 #x35638D75 #x40469DD5 #x4085FCCF 
     #x8BE23A4C #xC4B17AD2 #x6A3E6A2E #xCAB2F0FC 
     #x6B943FCD #x2860971A #x12E30446 #x3DDE6EE2 
     #xE01765AE #x6222F32A #x478308FE #x5D550BB5 
     #xA0EDA22A #xA9EFA98D #x86C40DA7 #xC351A716 
     #x9C867C84 #x1105586D #xFDA22853 #xDCFFEE85 
     #x2C5EEF76 #xCCFBD026 #x8990D201 #xBAF294CB 
     #x2AFAD975 #xE69464F5 #xDF133E14 #x94B013AF 
     #x2823C958 #x06A7D1A3 #x30F61119 #x6F95FE51 
     #x462C06C0 #xD92AB34E #x887C71D2 #xED7BDE33 
     #x6518393E #x79746D6E #x5D713329 #x5BA41938 
     #x48A97564 #x7C1BA6B9 #x7BFDAC67 #x31987C19 
     #x4B053D02 #xDE6C23C4 #xD002D64D #x581C49FE 
     #x38261571 #xDD474D63 #xE473D062 #xAA4546C3 
     #x9455F860 #x928FCE34 #xCAAB94D9 #x48161BBA 
     #x770E6F68 #x63912430 #x02C6641C #x6EC8A5E6 
     #x337DDD2B #x87282515 #x034B701B #x2CDA6B42 
     #x81CB096D #xB03D37C1 #x66C71C6F #xE1084382 
     #xEB51B255 #x2B3180C7 #x96C08BBC #xDF92B82F 
     #xA632F3BA #x5C68C8C0 #x1C3D0556 #x5504CC86 
     #x5FB26B8F #xABBFA4E5 #xB3BACEB4 #x41848B0A 
     #xAA445D32 #xB334A273 #xA85AD881 #xBCA696F0 
     #xB528D56C #x24F6EC65 #x90F4524A #x0CE1512E 
     #x5506D35A #x4E9DD79D #xC6CE9779 #x258905FA 
     #x3E109B33 #x2019295B #x73A054CC #xF8A9478B 
     #x34417EB0 #x2924F2F9 #x536D1BC4 #x3993357D 
     #x1DB6FF8B #x38A81AC2 #x7D6016BF #x47C4FBF1 
     #x7667E3F5 #x1E0FAADD #x938BEB96 #x7ABCFF62 
     #x8FC179C9 #xA78DAD94 #x2911E50D #x8F1F98B7 
     #x27121A91 #x61E48EAE #x31859808 #x4D62F7AD 
     #xEF5CEAEB #xECEBA345 #xBC9684CE #xF5CEB25E 
     #xB7F76221 #xF633E20C #xAB8293E4 #xA32CDF06 
     #xA5EE2CA4 #x985A202C #xCC8A8FB1 #xCF0B8447 
     #x979859A3 #x9F765244 #xA1240017 #xA8D516B1 
     #xBB5DC726 #x0BD7BA3E #xB86ADB39 #xE54BCA55 
     #x6C478063 #x1D7A3AFD #xE7669EDD #x519EC608 
     #xD149AA23 #x0E5715A2 #x848FF194 #x177D4571 
     #x41014C22 #xEEB55F32 #x3A6E2EC2 #x0F5E5CA1 
     #x75F5C361 #x8029927B #xC3D6E436 #xAD139FAB
     #x4CCF402F #x0D5DF1A9 #xBEA5DFC8 #x3E8BD948 
     #xBD3FF77E #xA5A0D357 #x1F74F645 #xA2D12E25 
     #x5E81A082 #x66FD9E52 #x7F687A49 #x2E0C90CE 
     #xBA973BC5 #xC2E8BCBE #xE509745F #x000001BC 
     #xE6DAB3D6 #x423777BB #xAEF06EB5 #xD1661C7E 
     #x4DAACFD8 #xA1781F35 #x2B16AFFC #x2D11284A 
     #xFA891D1F #xF1FC4F67 #xCB920ADA #x73ECC25D 
     #xC2A12651 #xAE610C22 #xD356B78A #x96E0A810 
     #x2FE7870F #x5A9A381F #xE94E5530 #xD5AD62ED 
     #x368D1427 #xD225E5E8 #xC7AF4631 #x65977B70 
     #xDE39D74F #x99F889B2 #x54E1D143 #x233F30BF 
     #xD9A63C97 #x9A9675D3 #xF334F9A8 #x5470554F 
     #x4A4F5688 #x166ACB74 #xB2E4AEAD #x70C74CAA 
     #x6F294D12 #xF0D09164 #x684031D1 #x57B82A89 
     #x61BE0B6B #xEFD95A5A #x69F2F29A #x2FBD12E9 
     #xFEFF9FE8 #x9BD37013 #xD6085A06 #x3F9B0404 
     #x166CFE15 #x4940C1F3 #xCDF3DEFB #x09542C4D 
     #x85CD5CE3 #xB4C52183 #x4462A641 #xC935B7DC 
     #x8ED3B63F #x3417F8A6 #x5B215B40 #xB8095929 
     #x3B8C8572 #xF99CDAEF #xF8FCB95D #x018C0614 
     #x1A3ACDF3 #x1B14ACCD #x00BB732D #x84D471F2 
     #x95E8DA16 #xC1A3110E #xBF1A82B8 #x430A7220 
     #x39DF210E #xB77E090D #x3CD05E9D #x5EF4BD9F 
     #x7E57A444 #x9D4FF6DA #x83D4A5F8 #xDA1D60E1 
     #x17998E47 #xB287C384 #x1BB31886 #xFE3EDC12 
     #x980CCBEF #xC7FE3CCC #x189BFD03 #xE46FB590 
     #x9A4C57DC #x3732FD46 #x7CF1AD65 #x7EF700A0 
     #xA31D8859 #x59C64468 #xD45B61F6 #x762FB0B4 
     #x99047718 #x155BAED0 #x3D50BAA6 #x68755E4C 
     #x22D8B4DF #xE9214E7F #x2EAC95F4 #x2ADDBF53 
     #xB4BD0109 #x32AE3909 #xB08E3450 #x834DF537 
     #x4220728D #xFA209DA8 #x9EFE23F7 #x9E691D9B 
     #xC4AE8D7F #x0446D288 #xE169785B #x7B4CC524 
     #x35CA1385 #x21D87F01 #x137B8AA5 #xCEBB400F 
     #x580796BE #x272E2B66 #x25C2B0DE #x36122641 
     #xAD1EFBB2 #x057702BD #xACF84BE9 #xD4BABB8E 
     #x641BC67B #x91583139 #x8036E024 #x8BDC2DE0 
     #xF49F68ED #x603C8156 #xDBEF5111 #xF7D236F7 
     #x8AD21E80 #x9727C459 #x670A5FD7 #xA08A0896 
     #x09EBA9CB #xCB4A8F43 #x0F7036A1 #x81AF564B 
     #x78199ABD #xC0B99AA7 #x3FC8E952 #x959F1EC8 
     #x794A81B9 #x8C505077 #x056338F0 #x3ACAAF8F 
     #x627A6778 #x07B43F50 #xF5ECCC77 #x4A44AB49 
     #xB679EE98 #x3BC3D6E4 #xCF14108C #x9CC0D4D1 
     #x206BC8A0 #x4406C00B #xC8D72D89 #x82A18854 
     #x5C3C432C #x67E366B3 #x102B37F2 #xB923DD61 
     #xD884271D #x56AB2779 #xFF1525AF #xBE83E1B0 
     #x217E49A9 #xFB7C65D4 #x6D48E7D4 #x6BDBE0E7 
     #x45D9179E #x08DF8287 #xDD53BD34 #x22EA6A9A 
     #x5622200A #xE36E141C #x8CB750EE #x7F805D1B 
     #x9F58E837 #xAFE5C7A5 #x4FB1C23C #xE27F996A 
     #x0775F0D0 #xD3867DFB #x6E88891A #xD0E673DE 
     #xAFB86C25 #x123AEB9E #xC145B895 #x30F1D5D5 
     #xEE7269E7 #xBB434A2D #xF931FA38 #x78CB67EC 
     #x323BBF9C #xF33B0372 #xFB279C74 #x52D66336 
     #x0AFB4EAA #x505F33AC #xA2CCE187 #xE8A5CD99 
     #x1E2D30BB #x53497480 #xD5876D90 #x8D2D5711 
     #x91BC038E #x1F1A4128 #x82E56648 #xD6E2E71D 
     #x497732B7 #x74036C3A #x6361F5AB #x89B67ED9 
     #xF1EA02A2 #xFFED95D8 #x1464D43D #xE72B3BD6 
     #x0BDC4820 #xA6300F17 #xED78A77A #xEBC18760)))

(defconst tiger-t2
  (make-tiger-sbox
   '(#x05A12138 #xE6A6BE5A #xB4F87C98 #xB5A122A5 
     #x140B6990 #x563C6089 #x391F5DD5 #x4C46CB2E 
     #xC9B79434 #xD932ADDB #x2015AFF5 #x08EA70E4 
     #x3E478CF1 #xD765A667 #xAB278D99 #xC4FB757E 
     #x2D6E0692 #xDF11C686 #x0D7F3B16 #xDDEB84F1 
     #xA665EA04 #x6F2EF604 #xF0E0DFB3 #x4A8E0F0F 
     #x3DBCBA51 #xA5EDEEF8 #x0EA4371E #xFC4F0A2A 
     #x5CB38429 #xE83E1DA8 #xBA1B1CE2 #xDC8FF882 
     #x8353E80D #xCD45505E #xD4DB0717 #x18D19A00 
     #xA5F38101 #x34A0CFED #x8887CAF2 #x0BE77E51 
     #xB3C45136 #x1E341438 #x9089CCF9 #xE05797F4 
     #xF2591D14 #xFFD23F9D #x8595C5CD #x543DDA22 
     #x99052A33 #x661F81FD #xDB0F7B76 #x8736E641 
     #x418E5307 #x15227725 #x162EB2FA #xE25F7F46 
     #x6C13D9FE #x48A8B212 #x92E76EEA #xAFDC5417 
     #xC6D1898F #x03D912BF #x1B83F51B #x31B1AAFA 
     #xE42AB7D9 #xF1AC2796 #xFCD2EBAC #x40A3A7D7 
     #x0AFBBCC5 #x1056136D #x9A6D0C85 #x7889E1DD 
     #x2A7974AA #xD3352578 #x078AC09B #xA7E25D09 
     #xEAC6EDD0 #xBD4138B3 #x71EB9E70 #x920ABFBE 
     #x4FC2625C #xA2A5D0F5 #x0B1290A3 #xC054E36B 
     #x62FE932B #xF6DD59FF #x11A8AC7D #x35373545 
     #x72FADCD4 #xCA845E91 #x329D20DC #x84F82B60 
     #xCD672F18 #x79C62CE1 #xD124642C #x8B09A2AD 
     #x19D9E726 #xD0C1E96A #x4BA9500C #x5A786A9B 
     #x634C43F3 #x0E020336 #xEB66D822 #xC17B474A 
     #xEC9BAAC2 #x6A731AE3 #xE0840258 #x8226667A 
     #x91CAECA5 #x67D45676 #x4875ADB5 #x1D94155C 
     #x5B813FDF #x6D00FD98 #xB774CD06 #x51286EFC 
     #x1FA744AF #x5E883447 #xE761AE2E #xF72CA0AE 
     #xAEE8E09A #xBE40E4CD #x5118F665 #xE9970BBB 
     #x33DF1964 #x726E4BEB #x29199762 #x703B0007 
     #xF5EF30A7 #x4631D816 #x1504A6BE #xB880B5B5 
     #x7ED84B6C #x641793C3 #xF6E97D96 #x7B21ED77 
     #x2EF96B73 #x77630631 #xE86FF3F4 #xAE528948 
     #x86A3F8F8 #x53DBD7F2 #x4CFC1063 #x16CADCE7 
     #xFA52C6DD #x005C19BD #x64D46AD3 #x68868F5D 
     #xCF1E186A #x3A9D512C #x385660AE #x367E62C2 
     #x77DCB1D7 #xE359E7EA #x749ABE6E #x526C0773 
     #xD09F734B #x735AE5F9 #x8A558BA8 #x493FC7CC 
     #x3041AB45 #xB0B9C153 #x470A59BD #x321958BA 
     #x5F46C393 #x852DB00B #xD336B0E5 #x91209B2B 
     #x659EF19F #x6E604F7D #x782CCB24 #xB99A8AE2 
     #xC814C4C7 #xCCF52AB6 #xBE11727B #x4727D9AF 
     #x0121B34D #x7E950D0C #x70AD471F #x756F4356 
     #x615A6849 #xF5ADD442 #x80B9957A #x4E87E099 
     #x50AEE355 #x2ACFA1DF #xFD2FD556 #xD898263A 
     #xD80C8FD6 #xC8F4924D #x754A173A #xCF99CA3D 
     #xAF91BF3C #xFE477BAC #xD690C12D #xED5371F6 
     #x5E687094 #x831A5C28 #x3708A0A4 #xC5D3C90A 
     #x17D06580 #x0F7F9037 #xB8FDF27F #x19F9BB13 
     #x4D502843 #xB1BD6F1B #x8FFF4012 #x1C761BA3 
     #xE2E21F3B #x0D1530C4 #xA7372C8A #x8943CE69 
     #xFEB5CE66 #xE5184E11 #xBD736621 #x618BDB80 
     #x8B574D0B #x7D29BAD6 #x25E6FE5B #x81BB613E 
     #xBC07913F #x071C9C10 #x09AC2D97 #xC7BEEB79 
     #x3BC5D757 #xC3E58D35 #xF38F61E8 #xEB017892 
     #x9B1CC21A #xD4EFFB9C #xF494F7AB #x99727D26 
     #x956B3E03 #xA3E063A2 #x4AA09C30 #x9D4A8B9A 
     #x00090FB4 #x3F6AB7D5 #x57268AC0 #x9CC0F2A0 
     #xEDBF42D1 #x3DEE9D2D #x7960A972 #x330F49C8 
     #x87421B41 #xC6B27202 #x7C00369C #x0AC59EC0 
     #xCB353425 #xEF4EAC49 #xEF0129D8 #xF450244E 
     #xCAF4DEB6 #x8ACC46E5 #x989263F7 #x2FFEAB63 
     #x5D7A4578 #x8F7CB9FE #x4E634635 #x5BD8F764 
     #xBF2DC900 #x427A7315 #x2125261C #x17D0C4AA 
     #x93518E50 #x3992486C #xA2D7D4C3 #xB4CBFEE0 
     #x2C5DDD8D #x7C75D620 #xE35B6C61 #xDBC295D8 
     #x02032B19 #x60B369D3 #xDCE44132 #xCE42685F 
     #xDDF65610 #x06F3DDB9 #xB5E148F0 #x8EA4D21D 
     #x2FCD496F #x20B0FCE6 #x58B0EE31 #x2C1B9123 
     #x18F5A308 #xB28317B8 #x9CA6D2CF #xA89C1E18 
     #x6AAADBC8 #x0C6B1857 #x1299FAE3 #xB65DEAA9 
     #x7F1027E7 #xFB2B794B #x443B5BEB #x04E4317F 
     #x5939D0A6 #x4B852D32 #xFB207FFC #xD5AE6BEE 
     #x81C7D374 #x309682B2 #x94C3B475 #xBAE309A1 
     #x13B49F05 #x8CC3F97B #xF8293967 #x98A9422F 
     #x1076FF7C #x244B16B0 #x663D67EE #xF8BF571C 
     #xEEE30DA1 #x1F0D6758 #x7ADEB9B7 #xC9B611D9 
     #x7B6C57A2 #xB7AFD588 #x6B984FE1 #x6290AE84 
     #xACC1A5FD #x94DF4CDE #xC5483AFF #x058A5BD1 
     #x42BA3C37 #x63166CC1 #xB2F76F40 #x8DB8526E 
     #x6F0D6D4E #xE1088003 #x971D311D #x9E0523C9 
     #xCC7CD691 #x45EC2824 #xE62382C9 #x575B8359 
     #xC4889995 #xFA9E400D #x45721568 #xD1823ECB 
     #x8206082F #xDAFD983B #x2386A8CB #xAA7D2908 
     #x03B87588 #x269FCD44 #x28BDD1E0 #x1B91F5F7 
     #x040201F6 #xE4669F39 #x8CF04ADE #x7A1D7C21 
     #xD79CE5CE #x65623C29 #x96C00BB1 #x23684490 
     #x9DA503BA #xAB9BF187 #xA458058E #xBC23ECB1 
     #xBB401ECC #x9A58DF01 #xA85F143D #xA070E868 
     #x7DF2239E #x4FF18830 #x1A641183 #x14D565B4 
     #x52701602 #xEE133374 #x3F285E09 #x950E3DCF 
     #xB9C80953 #x59930254 #x8930DA6D #x3BF29940 
     #x53691387 #xA955943F #xA9CB8784 #xA15EDECA 
     #x352BE9A0 #x29142127 #xFF4E7AFB #x76F0371F 
     #x274F2228 #x0239F450 #x1D5E868B #xBB073AF0 
     #xC10E96C1 #xBFC80571 #x68222E23 #xD2670885 
     #x8E80B5B0 #x9671A3D4 #xE193BB81 #x55B5D38A 
     #xA18B04B8 #x693AE2D0 #xADD5335F #x5C48B4EC 
     #x4916A1CA #xFD743B19 #x34BE98C4 #x25770181 
     #x3C54A4AD #xE77987E8 #xDA33E1B9 #x28E11014 
     #x226AA213 #x270CC59E #x6D1A5F60 #x71495F75 
     #x60AFEF77 #x9BE853FB #xF7443DBF #xADC786A7 
     #x73B29A82 #x09044561 #xC232BD5E #x58BC7A66 
     #x673AC8B2 #xF306558C #xB6C9772A #x41F639C6 
     #x9FDA35DA #x216DEFE9 #x1C7BE615 #x11640CC7 
     #x565C5527 #x93C43694 #x46777839 #xEA038E62 
     #x5A3E2469 #xF9ABF3CE #x0FD312D2 #x741E768D 
     #xCED652C6 #x0144B883 #xA33F8552 #xC20B5A5B 
     #xC3435A9D #x1AE69633 #x088CFDEC #x97A28CA4 
     #x1E96F420 #x8824A43C #x6EEEA746 #x37612FA6 
     #xF9CF0E5A #x6B4CB165 #xA0ABFB4A #x43AA1C06 
     #xF162796B #x7F4DC26F #x54ED9B0F #x6CBACC8E 
     #xD2BB253E #xA6B7FFEF #xB0A29D4F #x2E25BC95 
     #xDEF1388C #x86D6A58B #x76B6F054 #xDED74AC5 
     #x2B45805D #x8030BDBC #xE94D9289 #x3C81AF70 
     #x9E3100DB #x3EFF6DDA #xDFCC8847 #xB38DC39F 
     #x8D17B87E #x12388552 #x40B1B642 #xF2DA0ED2 
     #xD54BF9A9 #x44CEFADC #x433C7EE6 #x1312200E 
     #x3A78C748 #x9FFCC84F #x248576BB #xF0CD1F72 
     #x3638CFE4 #xEC697405 #x0CEC4E4C #x2BA7B67C 
     #xE5CE32ED #xAC2F4DF3 #x26EA4C11 #xCB33D143 
     #xC77E58BC #xA4E9044C #xD934FCEF #x5F513293 
     #x06E55444 #x5DC96455 #x317DE40A #x50DE418F 
     #x69DDE259 #x388CB31A #x55820A86 #x2DB4A834 
     #x84711AE9 #x9010A91E #xB1498371 #x4DF7F0B7 
     #xC0977179 #xD62A2EAB #xAA8D5C0E #x22FAC097)))

(defconst tiger-t3
  (make-tiger-sbox
   '(#xF1DAF39B #xF49FCC2F #x6FF29281 #x487FD5C6 
     #xFCDCA83F #xE8A30667 #xD2FCCE63 #x2C9B4BE3 
     #x93FBBBC2 #xDA3FF74B #xFE70BA66 #x2FA165D2 
     #x970E93D4 #xA103E279 #xB0E45E71 #xBECDEC77 
     #x3985E497 #xCFB41E72 #x5EF75017 #xB70AAA02 
     #x3840B8E0 #xD42309F0 #x35898579 #x8EFC1AD0 
     #xE2B2ABC5 #x96C6920B #x375A9172 #x66AF4163 
     #xCA7127FB #x2174ABDC #x4A72FF41 #xB33CCEA6 
     #x083066A5 #xF04A4933 #xD7289AF5 #x8D970ACD 
     #x31C8C25E #x8F96E8E0 #x76875D47 #xF3FEC022 
     #x056190DD #xEC7BF310 #xBB0F1491 #xF5ADB0AE 
     #x0FD58892 #x9B50F885 #x58B74DE8 #x49754883 
     #x91531C61 #xA3354FF6 #x81D2C6EE #x0702BBE4 
     #x7DEDED98 #x89FB2405 #x8596E902 #xAC307513 
     #x172772ED #x1D2D3580 #x8E6BC30D #xEB738FC2 
     #x63044326 #x5854EF8F #x5ADD3BBE #x9E5C5232 
     #x325C4623 #x90AA53CF #x349DD067 #xC1D24D51 
     #xA69EA624 #x2051CFEE #x862E7E4F #x13220F0A 
     #x04E04864 #xCE393994 #x7086FCB7 #xD9C42CA4 
     #x8A03E7CC #x685AD223 #xAB2FF1DB #x066484B2 
     #xEFBF79EC #xFE9D5D70 #x9C481854 #x5B13B9DD 
     #xED1509AD #x15F0D475 #x0EC79851 #x0BEBCD06 
     #x183AB7F8 #xD58C6791 #x52F3EEE4 #xD1187C50 
     #xE54E82FF #xC95D1192 #xB9AC6CA2 #x86EEA14C 
     #x53677D5D #x3485BEB1 #x1F8C492A #xDD191D78 
     #xA784EBF9 #xF60866BA #xA2D08C74 #x518F643B 
     #xE1087C22 #x8852E956 #xC410AE8D #xA768CB8D 
     #xBFEC8E1A #x38047726 #xCD3B45AA #xA67738B4 
     #xEC0DDE19 #xAD16691C #x80462E07 #xC6D43193 
     #x0BA61938 #xC5A5876D #xA58FD840 #x16B9FA1F 
     #x3CA74F18 #x188AB117 #xC99C021F #xABDA2F98 
     #x134AE816 #x3E0580AB #x73645ABB #x5F3B05B7 
     #x5575F2F6 #x2501A2BE #x4E7E8BA9 #x1B2F7400 
     #x71E8D953 #x1CD75803 #x62764E30 #x7F6ED895 
     #x596F003D #xB15926FF #xA8C5D6B9 #x9F65293D 
     #xD690F84C #x6ECEF04D #xFF33AF88 #x4782275F 
     #x3F820801 #xE4143308 #x9A1AF9B5 #xFD0DFE40 
     #x2CDB396B #x4325A334 #xB301B252 #x8AE77E62 
     #x6655615A #xC36F9E9F #x92D32C09 #x85455A2D 
     #x49477485 #xF2C7DEA9 #x33A39EBA #x63CFB4C1 
     #x6EBC5462 #x83B040CC #xFDB326B0 #x3B9454C8 
     #x87FFD78C #x56F56A9E #x99F42BC6 #x2DC2940D 
     #x6B096E2D #x98F7DF09 #x3AD852BF #x19A6E01E 
     #xDBD4B40B #x42A99CCB #x45E9C559 #xA59998AF 
     #x07D93186 #x366295E8 #xFAA1F773 #x6B48181B 
     #x157A0A1D #x1FEC57E2 #xF6201AD5 #x4667446A 
     #xCFB0F075 #xE615EBCA #x68290778 #xB8F31F4F 
     #xCE22D11E #x22713ED6 #x2EC3C93B #x3057C1A7 
     #x7C3F1F2F #xCB46ACC3 #x02AAF50E #xDBB893FD 
     #x600B9FCF #x331FD92E #x48EA3AD6 #xA498F961 
     #x8B6A83EA #xA8D8426E #xB7735CDC #xA089B274 
     #x1E524A11 #x87F6B373 #xCBC96749 #x118808E5 
     #xB19BD394 #x9906E4C7 #x9B24A20C #xAFED7F7E 
     #xEB3644A7 #x6509EADE #xE8EF0EDE #x6C1EF1D3 
     #xE9798FB4 #xB9C97D43 #x740C28A3 #xA2F2D784 
     #x6197566F #x7B849647 #xB65F069D #x7A5BE3E6 
     #x78BE6F10 #xF96330ED #x7A076A15 #xEEE60DE7 
     #xA08B9BD0 #x2B4BEE4A #xC7B8894E #x6A56A63E 
     #xBA34FEF4 #x02121359 #x283703FC #x4CBF99F8 
     #x0CAF30C8 #x39807135 #xF017687A #xD0A77A89 
     #x9E423569 #xF1C1A9EB #x2DEE8199 #x8C797628 
     #xDD1F7ABD #x5D1737A5 #x09A9FA80 #x4F53433C 
     #xDF7CA1D9 #xFA8B0C53 #x886CCB77 #x3FD9DCBC 
     #xA91B4720 #xC040917C #xF9D1DCDF #x7DD00142 
     #x4F387B58 #x8476FC1D #xF3316503 #x23F8E7C5 
     #xE7E37339 #x032A2244 #x50F5A74B #x5C87A5D7 
     #x3698992E #x082B4CC4 #xB858F63C #xDF917BEC 
     #x5BF86DDA #x3270B8FC #x29B5DD76 #x10AE72BB 
     #x7700362B #x576AC94E #xC61EFB8F #x1AD112DA 
     #xC5FAA427 #x691BC30E #xCC327143 #xFF246311 
     #x30E53206 #x3142368E #xE02CA396 #x71380E31 
     #x0AAD76F1 #x958D5C96 #xC16DA536 #xF8D6F430 
     #x1BE7E1D2 #xC8FFD13F #x004DDBE1 #x7578AE66 
     #x067BE646 #x05833F01 #x3BFE586D #xBB34B5AD 
     #xA12B97F0 #x095F34C9 #x25D60CA8 #x247AB645 
     #x017477D1 #xDCDBC6F3 #xDECAD24D #x4A2E14D4 
     #xBE0A1EEB #xBDB5E6D9 #x794301AB #x2A7E70F7 
     #x270540FD #xDEF42D8A #xA34C22C1 #x01078EC0 
     #xF4C16387 #xE5DE511A #xBD9A330A #x7EBB3A52 
     #xAA7D6435 #x77697857 #x03AE4C32 #x004E8316 
     #xAD78E312 #xE7A21020 #x6AB420F2 #x9D41A70C 
     #xEA1141E6 #x28E06C18 #x984F6B28 #xD2B28CBD 
     #x446E9D83 #x26B75F6C #x4D418D7F #xBA47568C 
     #xE6183D8E #xD80BADBF #x5F166044 #x0E206D7F 
     #x11CBCA3E #xE258A439 #xB21DC0BC #x723A1746 
     #xF5D7CDD3 #xC7CAA854 #x3D261D9C #x7CAC3288 
     #x23BA942C #x7690C264 #x478042B8 #x17E55524 
     #x56A2389F #xE0BE4776 #x67AB2DA0 #x4D289B5E 
     #x8FBBFD31 #x44862B9C #x9D141365 #xB47CC804 
     #x2B91C793 #x822C1B36 #xFB13DFD8 #x4EB14655 
     #x14E2A97B #x1ECBBA07 #x5CDE5F14 #x6143459D 
     #xD5F0AC89 #x53A8FBF1 #x1C5E5B00 #x97EA04D8 
     #xD4FDB3F3 #x622181A8 #x572A1208 #xE9BCD341 
     #x43CCE58A #x14112586 #xA4C6E0A4 #x9144C5FE 
     #x65CF620F #x0D33D065 #x9F219CA1 #x54A48D48 
     #x6D63C821 #xC43E5EAC #x72770DAF #xA9728B3A 
     #x20DF87EF #xD7934E7B #x1A3E86E5 #xE35503B6 
     #xC819D504 #xCAE321FB #xAC60BFA6 #x129A50B3 
     #x7E9FB6C3 #xCD5E68EA #x9483B1C7 #xB01C9019 
     #xC295376C #x3DE93CD5 #x2AB9AD13 #xAED52EDF 
     #xC0A07884 #x2E60F512 #xE36210C9 #xBC3D86A3 
     #x163951CE #x35269D9B #xD0CDB5FA #x0C7D6E2A 
     #xD87F5733 #x59E86297 #x898DB0E7 #x298EF221 
     #xD1A5AA7E #x55000029 #xB5061B45 #x8BC08AE1 
     #x6C92703A #xC2C31C2B #xAF25EF42 #x94CC596B 
     #x22540456 #x0A1D73DB #xD9C4179A #x04B6A0F9 
     #xAE3D3C60 #xEFFDAFA2 #xB49496C4 #xF7C8075B 
     #x1D1CD4E3 #x9CC5C714 #x218E5534 #x78BD1638 
     #xF850246A #xB2F11568 #x9502BC29 #xEDFABCFA 
     #xDA23051B #x796CE5F2 #xDC93537C #xAAE128B0 
     #xEE4B29AE #x3A493DA0 #x416895D7 #xB5DF6B2C 
     #x122D7F37 #xFCABBD25 #x105DC4B1 #x70810B58 
     #xF7882A90 #xE10FDD37 #x518A3F5C #x524DCAB5 
     #x8451255B #x3C9E8587 #x19BD34E2 #x40298281 
     #x5D3CECCB #x74A05B6F #x42E13ECA #xB6100215 
     #x2F59E2AC #x0FF979D1 #xE4F9CC50 #x6037DA27 
     #x0DF1847D #x5E92975A #xD3E623FE #xD66DE190 
     #x7B568048 #x5032D6B8 #x8235216E #x9A36B7CE 
     #x24F64B4A #x80272A7A #x8C6916F7 #x93EFED8B 
     #x4CCE1555 #x37DDBFF4 #x4B99BD25 #x4B95DB5D 
     #x69812FC0 #x92D3FDA1 #x90660BB6 #xFB1A4A9A 
     #x46A4B9B2 #x730C1969 #x7F49DA68 #x81E289AA 
     #x83B1A05F #x64669A0F #x9644F48B #x27B3FF7D 
     #x8DB675B3 #xCC6B615C #xBCEBBE95 #x674F20B9 
     #x75655982 #x6F312382 #x3E45CF05 #x5AE48871 
     #x54C21157 #xBF619F99 #x40A8EAE9 #xEABAC460 
     #xF2C0C1CD #x454C6FE9 #x6412691C #x419CF649 
     #x265B0F70 #xD3DC3BEF #xC3578A9E #x6D0E60F5)))

(defconst tiger-t4
  (make-tiger-sbox
   '(#x26323C55 #x5B0E6085 #xFA1B59F5 #x1A46C1A9 
     #x7C4C8FFA #xA9E245A1 #xDB2955D7 #x65CA5159 
     #xCE35AFC2 #x05DB0A76 #xA9113D45 #x81EAC77E 
     #xB6AC0A0D #x528EF88A #x597BE3FF #xA09EA253 
     #xAC48CD56 #x430DDFB3 #xF45CE46F #xC4B3A67A 
     #xFBE2D05E #x4ECECFD8 #xB39935F0 #x3EF56F10 
     #x9CD619C6 #x0B22D682 #x74DF2069 #x17FD460A 
     #x8510ED40 #x6CF8CC8E #x3A6ECAA7 #xD6C824BF 
     #x1A817049 #x61243D58 #xBBC163A2 #x048BACB6 
     #x7D44CC32 #xD9A38AC2 #xAAF410AB #x7FDDFF5B 
     #xA804824B #xAD6D495A #x2D8C9F94 #xE1A6A74F 
     #x35DEE8E3 #xD4F78512 #x6540D893 #xFD4B7F88 
     #x2AA4BFDA #x247C2004 #x17D1327C #x096EA1C5 
     #x361A6685 #xD56966B4 #x1221057D #x277DA5C3 
     #xA43ACFF7 #x94D59893 #xCDC02281 #x64F0C51C 
     #xFF6189DB #x3D33BCC4 #x4CE66AF1 #xE005CB18 
     #x1DB99BEA #xFF5CCD1D #xFE42980F #xB0B854A7 
     #x718D4B9F #x7BD46A6A #x22A5FD8C #xD10FA8CC 
     #x2BE4BD31 #xD3148495 #xCB243847 #xC7FA975F 
     #x5846C407 #x4886ED1E #x1EB70B04 #x28CDDB79 
     #xF573417F #xC2B00BE2 #x2180F877 #x5C959045 
     #xF370EB00 #x7A6BDDFF #xD6D9D6A4 #xCE509E38 
     #x647FA702 #xEBEB0F00 #x76606F06 #x1DCC06CF 
     #xA286FF0A #xE4D9F28B #xC918C262 #xD85A305D 
     #x32225F54 #x475B1D87 #x68CCB5FE #x2D4FB516 
     #xD72BBA20 #xA679B9D9 #x912D43A5 #x53841C0D 
     #xBF12A4E8 #x3B7EAA48 #xF22F1DDF #x781E0E47 
     #x0AB50973 #xEFF20CE6 #x9DFFB742 #x20D261D1 
     #x062A2E39 #x16A12B03 #x39650495 #x1960EB22 
     #xD50EB8B8 #x251C16FE #xF826016E #x9AC0C330 
     #x953E7671 #xED152665 #xA6369570 #x02D63194 
     #x94B1C987 #x5074F083 #x90B25CE1 #x70BA598C 
     #x0B9742F6 #x794A1581 #xFCAF8C6C #x0D5925E9 
     #xD868744E #x3067716C #xE8D7731B #x910AB077 
     #x5AC42F61 #x6A61BBDB #xF0851567 #x93513EFB 
     #x9E83E9D5 #xF494724B #x5C09648D #xE887E198 
     #x75370CFD #x34B1D3C6 #xBC0D255D #xDC35E433 
     #x34131BE0 #xD0AAB842 #xB48B7EAF #x08042A50 
     #x44A3AB35 #x9997C4EE #x201799D0 #x829A7B49 
     #xB7C54441 #x263B8307 #xFD6A6CA6 #x752F95F4 
     #x2C08C6E5 #x92721740 #xA795D9EE #x2A8AB754 
     #x2F72943D #xA442F755 #x19781208 #x2C31334E 
     #xEAEE6291 #x4FA98D7C #x665DB309 #x55C3862F 
     #x5D53B1F3 #xBD061017 #x40413F27 #x46FE6CB8 
     #xDF0CFA59 #x3FE03792 #x2EB85E8F #xCFE70037 
     #xADBCE118 #xA7BE29E7 #xDE8431DD #xE544EE5C 
     #x41F1873E #x8A781B1B #xA0D2F0E7 #xA5C94C78 
     #x77B60728 #x39412E28 #xAFC9A62C #xA1265EF3 
     #x6A2506C5 #xBCC2770C #xDCE1CE12 #x3AB66DD5 
     #x4A675B37 #xE65499D0 #x81BFD216 #x7D8F5234 
     #xEC15F389 #x0F6F64FC #x8B5B13C8 #x74EFBE61 
     #x14273E1D #xACDC82B7 #x03199D17 #xDD40BFE0 
     #xE7E061F8 #x37E99257 #x04775AAA #xFA526269 
     #x463D56F9 #x8BBBF63A #x43A26E64 #xF0013F15 
     #x879EC898 #xA8307E9F #x150177CC #xCC4C27A4 
     #xCA1D3348 #x1B432F2C #x9F6FA013 #xDE1D1F8F 
     #x47A7DDD6 #x606602A0 #xCC1CB2C7 #xD237AB64 
     #x25FCD1D3 #x9B938E72 #x8E0FF476 #xEC4E0370 
     #x3D03C12D #xFEB2FBDA #xEE43889A #xAE0BCED2 
     #xEBFB4F43 #x22CB8923 #x3CF7396D #x69360D01 
     #xD2D4E022 #x855E3602 #xD01F784C #x073805BA 
     #x3852F546 #x33E17A13 #x8AC7B638 #xDF487405 
     #x678AA14A #xBA92B29C #x6CFAADCD #x0CE89FC7 
     #x08339E34 #x5F9D4E09 #x1F5923B9 #xF1AFE929 
     #x0F4A265F #x6E3480F6 #xB29B841C #xEEBF3A2A 
     #x8F91B4AD #xE21938A8 #x45C6D3C3 #x57DFEFF8 
     #xF62CAAF2 #x2F006B0B #x6F75EE78 #x62F479EF 
     #x1C8916A9 #x11A55AD4 #x84FED453 #xF229D290 
     #x16B000E6 #x42F1C27B #x9823C074 #x2B1F7674 
     #xC2745360 #x4B76ECA3 #xB91691BD #x8C98F463 
     #xF1ADE66A #x14BCC93C #x6D458397 #x8885213E 
     #x274D4711 #x8E177DF0 #x503F2951 #xB49B73B5 
     #xC3F96B6B #x10168168 #x63CAB0AE #x0E3D963B 
     #x55A1DB14 #x8DFC4B56 #x6E14DE5C #xF789F135 
     #x4E51DAC1 #x683E68AF #x8D4B0FD9 #xC9A84F9D 
     #x52A0F9D1 #x3691E03F #xE1878E80 #x5ED86E46 
     #x99D07150 #x3C711A0E #x0C4E9310 #x5A0865B2 
     #xE4F0682E #x56FBFC1F #x105EDF9B #xEA8D5DE3 
     #x2379187A #x71ABFDB1 #xBEE77B9C #x2EB99DE1 
     #x33CF4523 #x21ECC0EA #x1805C7A1 #x59A4D752 
     #x56AE7C72 #x3896F5EB #xB18F75DC #xAA638F3D 
     #xABE9808E #x9F39358D #xC00B72AC #xB7DEFA91 
     #x62492D92 #x6B5541FD #xF92E4D5B #x6DC6DEE8 
     #xC4BEEA7E #x353F57AB #xDA5690CE #x735769D6 
     #x42391484 #x0A234AA6 #x28F80D9D #xF6F95080 
     #x7AB3F215 #xB8E319A2 #x51341A4D #x31AD9C11 
     #x7BEF5805 #x773C22A5 #x07968633 #x45C7561A 
     #x249DBE36 #xF913DA9E #x78A64C68 #xDA652D9B 
     #x3BC334EF #x4C27A97F #xE66B17F4 #x76621220 
     #x9ACD7D0B #x96774389 #xE0ED6782 #xF3EE5BCA 
     #x00C879FC #x409F7536 #xB5926DB6 #x06D09A39 
     #x317AC588 #x6F83AEB0 #x86381F21 #x01E6CA4A 
     #xD19F3025 #x66FF3462 #xDDFD3BFB #x72207C24 
     #xE2ECE2EB #x4AF6B6D3 #xC7EA08DE #x9C994DBE 
     #xB09A8BC4 #x49ACE597 #xCF0797BA #xB38C4766 
     #xC57C2A75 #x131B9373 #x61931E58 #xB1822CCE 
     #x09BA1C0C #x9D7555B9 #x937D11D2 #x127FAFDD 
     #xC66D92E4 #x29DA3BAD #x54C2ECBC #xA2C1D571 
     #x82F6FE24 #x58C5134D #x5B62274F #x1C3AE351 
     #x01CB8126 #xE907C82E #x13E37FCB #xF8ED0919 
     #xC80046C9 #x3249D8F9 #xE388FB63 #x80CF9BED 
     #x116CF19E #x1881539A #x6BD52457 #x5103F3F7 
     #xAE47F7A8 #x15B7E6F5 #xD47E9CCF #xDBD7C6DE 
     #x0228BB1A #x44E55C41 #x5EDB4E99 #xB647D425 
     #xB8AAFC30 #x5D11882B #x29D3212A #xF5098BBB 
     #xE90296B3 #x8FB5EA14 #x57DD025A #x677B9421 
     #xA390ACB5 #xFB58E7C0 #x83BD4A01 #x89D3674C 
     #x4BF3B93B #x9E2DA4DF #x8CAB4829 #xFCC41E32 
     #xBA582C52 #x03F38C96 #x7FD85DB2 #xCAD1BDBD 
     #x6082AE83 #xBBB442C1 #xA5DA9AB0 #xB95FE86B 
     #x3771A93F #xB22E0467 #x493152D8 #x845358C9 
     #x97B4541E #xBE2A4886 #xD38E6966 #x95A2DC2D 
     #x923C852B #xC02C11AC #x0DF2A87B #x2388B199 
     #x1B4F37BE #x7C8008FA #x4D54E503 #x1F70D0C8 
     #x7ECE57D4 #x5490ADEC #xD9063A3A #x002B3C27 
     #x8030A2BF #x7EAEA384 #xED2003C0 #xC602326D 
     #x69A94086 #x83A7287D #x30F57A8A #xC57A5FCB 
     #x79EBE779 #xB56844E4 #x05DCBCE9 #xA373B40F 
     #x88570EE2 #xD71A786E #xBDE8F6A0 #x879CBACD 
     #xC164A32F #x976AD1BC #x9666D78B #xAB21E25E 
     #xE5E5C33C #x901063AA #x48698D90 #x9818B344 
     #x3E1E8ABB #xE36487AE #x893BDCB4 #xAFBDF931 
     #x5FBBD519 #x6345A0DC #x9B9465CA #x8628FE26 
     #x3F9C51EC #x1E5D0160 #xA15049B7 #x4DE44006 
     #xF776CBB1 #xBF6C70E5 #xEF552BED #x411218F2 
     #x705A36A3 #xCB0C0708 #x4F986044 #xE74D1475 
     #x0EA8280E #xCD56D943 #x535F5065 #xC12591D7 
     #x720AEF96 #xC83223F1 #x7363A51F #xC3A0396F)))


#+#.(cl:if (cl:= ironclad::+tiger-wordsize+ 64) '(and) '(or))
(defun update-tiger-block (regs block)
  (declare (type tiger-regs regs)
           (type tiger-state-block block)
           #.(burn-baby-burn))
  (let ((a (tiger-regs-a regs))
        (b (tiger-regs-b regs))
        (c (tiger-regs-c regs)))
    (macrolet ((key-schedule (block)
                 `(setf (aref ,block 0)
                        (mod64- (aref ,block 0)
                                (logxor (aref ,block 7) #xa5a5a5a5a5a5a5a5))
                        (aref ,block 1)
                        (logxor (aref ,block 1) (aref ,block 0))
                        (aref ,block 2)
                        (mod64+ (aref ,block 2) (aref ,block 1))
                        (aref ,block 3)
                        (mod64- (aref ,block 3)
                                (logxor (aref ,block 2)
                                        (mod64ash (mod64lognot (aref ,block 1)) 19)))
                        (aref ,block 4)
                        (logxor (aref ,block 4) (aref ,block 3))
                        (aref ,block 5)
                        (mod64+ (aref ,block 5) (aref ,block 4))
                        (aref ,block 6)
                        (mod64- (aref ,block 6)
                                (logxor (aref ,block 5)
                                        (mod64ash (mod64lognot (aref ,block 4)) -23)))
                        (aref ,block 7)
                        (logxor (aref ,block 7) (aref ,block 6))
                        (aref ,block 0)
                        (mod64+ (aref ,block 0) (aref ,block 7))
                        (aref ,block 1)
                        (mod64- (aref ,block 1)
                                (logxor (aref ,block 0)
                                        (mod64ash (mod64lognot (aref ,block 7)) 19)))
                        (aref ,block 2)
                        (logxor (aref ,block 2) (aref ,block 1))
                        (aref ,block 3)
                        (mod64+ (aref ,block 3) (aref ,block 2))
                        (aref ,block 4)
                        (mod64- (aref ,block 4)
                                (logxor (aref ,block 3)
                                        (mod64ash (mod64lognot (aref ,block 2)) -23)))
                        (aref ,block 5)
                        (logxor (aref ,block 5) (aref ,block 4))
                        (aref ,block 6)
                        (mod64+ (aref ,block 6) (aref ,block 5))
                        (aref ,block 7)
                        (mod64- (aref ,block 7)
                                (logxor (aref ,block 6) #x0123456789abcdef))))
               (tiger-round (a b c block n mul)
                 `(progn
                    (setf ,c (logxor ,c (aref ,block ,n))
                        ,a (mod64- ,a
                                   (logxor (aref tiger-t1 (first-byte ,c))
                                           (aref tiger-t2 (third-byte ,c))
                                           (aref tiger-t3 (fifth-byte ,c))
                                           (aref tiger-t4 (seventh-byte ,c))))
                        ,b (mod64+ ,b
                                   (logxor (aref tiger-t1 (eighth-byte ,c))
                                           (aref tiger-t2 (sixth-byte ,c))
                                           (aref tiger-t3 (fourth-byte ,c))
                                           (aref tiger-t4 (second-byte ,c))))
                        ,b (mod64* ,b ,mul))))
               (pass (a b c block mul)
                 `(progn
                    (tiger-round ,a ,b ,c ,block 0 ,mul)
                    (tiger-round ,b ,c ,a ,block 1 ,mul)
                    (tiger-round ,c ,a ,b ,block 2 ,mul)
                    (tiger-round ,a ,b ,c ,block 3 ,mul)
                    (tiger-round ,b ,c ,a ,block 4 ,mul)
                    (tiger-round ,c ,a ,b ,block 5 ,mul)
                    (tiger-round ,a ,b ,c ,block 6 ,mul)
                    (tiger-round ,b ,c ,a ,block 7 ,mul))))
      (pass a b c block 5)
      (key-schedule block)
      (pass c a b block 7)
      (key-schedule block)
      (pass b c a block 9)
      (setf (tiger-regs-a regs) (logxor a (tiger-regs-a regs))
            (tiger-regs-b regs) (mod64- b (tiger-regs-b regs))
            (tiger-regs-c regs) (mod64+ c (tiger-regs-c regs)))
      regs)))
        
#+#.(cl:if (cl:= ironclad::+tiger-wordsize+ 32) '(and) '(or))
(defun update-tiger-block (regs block)
  (declare (type tiger-regs regs)
           (type (simple-array (unsigned-byte 32) (16)) block)
           #.(burn-baby-burn))
  (let ((a0 (tiger-regs-a0 regs)) (a1 (tiger-regs-a1 regs))
        (b0 (tiger-regs-b0 regs)) (b1 (tiger-regs-b1 regs))
        (c0 (tiger-regs-c0 regs)) (c1 (tiger-regs-c1 regs))
        (x00 (aref block 0)) (x01 (aref block 1))
        (x10 (aref block 2)) (x11 (aref block 3))
        (x20 (aref block 4)) (x21 (aref block 5))
        (x30 (aref block 6)) (x31 (aref block 7))
        (x40 (aref block 8)) (x41 (aref block 9))
        (x50 (aref block 10)) (x51 (aref block 11))
        (x60 (aref block 12)) (x61 (aref block 13))
        (x70 (aref block 14)) (x71 (aref block 15))
        (aa0 0) (aa1 0) (bb0 0) (bb1 0) (cc0 0) (cc1 0)
        (temp0 0) (temp1 0) (temps0 0) (tcarry 0))
    (declare (type (unsigned-byte 32) a0 a1 b0 b1 c0 c1
                   x00 x01 x10 x11 x20 x21 x30 x31
                   x40 x41 x50 x51 x60 x61 x70 x71
                   aa0 aa1 bb0 bb1 cc0 cc1
                   temp0 temp1 temps0)
             (type bit tcarry))
    (macrolet ((sub64 (s0 s1 p0 p1)
                 `(progn
                   (multiple-value-setq (temps0 tcarry)
                     (%subtract-with-borrow ,s0 ,p0 1))
                   (setf ,s0 temps0
                    ,s1 (%subtract-with-borrow ,s1 ,p1 tcarry))))
               (add64 (s0 s1 p0 p1)
                 `(progn
                   (multiple-value-setq (temps0 tcarry)
                     (%add-with-carry ,s0 ,p0 0))
                   (setf ,s0 temps0
                    ,s1 (%add-with-carry ,s1 ,p1 tcarry))))
               (xor64 (s0 s1 p0 p1)
                 `(setf ,s0 (logxor ,s0 ,p0) ,s1 (logxor ,s1 ,p1)))
               (mul5 (s0 s1)
                 `(let ((tempt0 (mod32ash ,s0 2))
                        (tempt1 (logior (mod32ash ,s1 2) (mod32ash ,s0 -30))))
                   (declare (type (unsigned-byte 32) tempt0 tempt1))
                   (add64 ,s0 ,s1 tempt0 tempt1)))
               (mul7 (s0 s1)
                 `(let ((tempt0 (mod32ash ,s0 3))
                        (tempt1 (logior (mod32ash ,s1 3) (mod32ash ,s0 -29))))
                   (declare (type (unsigned-byte 32) tempt0 tempt1))
                   (sub64 tempt0 tempt1 ,s0 ,s1)
                   (setf ,s0 tempt0 ,s1 tempt1)))
               (mul9 (s0 s1)
                 `(let ((tempt0 (mod32ash ,s0 3))
                        (tempt1 (logior (mod32ash ,s1 3) (mod32ash ,s0 -29))))
                   (declare (type (unsigned-byte 32) tempt0 tempt1))
                   (add64 ,s0 ,s1 tempt0 tempt1)))
               (save-abc ()
                 `(setf aa0 a0 aa1 a1 bb0 b0 bb1 b1 cc0 c0 cc1 c1))
               (loref (sbox index)
                 `(aref ,sbox (* 2 ,index)))
               (hiref (sbox index)
                 `(aref ,sbox (+ (* 2 ,index) 1)))
               (round-xor (sbox bytefun val)
                 `(setf temp0 (logxor temp0 (loref ,sbox (,bytefun ,val)))
                    temp1 (logxor temp1 (hiref ,sbox (,bytefun ,val)))))
               (tiger-round(a0 a1 b0 b1 c0 c1 x0 x1 mul)
                 `(progn
                   (xor64 ,c0 ,c1 ,x0 ,x1)
                   (setf temp0 (loref tiger-t1 (first-byte ,c0))
                    temp1 (hiref tiger-t1 (first-byte ,c0)))
                   (round-xor tiger-t2 third-byte ,c0)
                   (round-xor tiger-t3 first-byte ,c1)
                   (round-xor tiger-t4 third-byte ,c1)
                   (sub64 ,a0 ,a1 temp0 temp1)
                   (setf temp0 (loref tiger-t4 (second-byte ,c0))
                         temp1 (hiref tiger-t4 (second-byte ,c0)))
                   (round-xor tiger-t3 fourth-byte ,c0)
                   (round-xor tiger-t2 second-byte ,c1)
                   (round-xor tiger-t1 fourth-byte ,c1)
                   (add64 ,b0 ,b1 temp0 temp1)
                   (cond
                     ((= ,mul 5) (mul5 ,b0 ,b1))
                     ((= ,mul 7) (mul7 ,b0 ,b1))
                     (t (mul9 ,b0 ,b1)))))
               (pass (a0 a1 b0 b1 c0 c1 mul)
                 `(progn
                   (tiger-round ,a0 ,a1 ,b0 ,b1 ,c0 ,c1 x00 x01 ,mul)
                   (tiger-round ,b0 ,b1 ,c0 ,c1 ,a0 ,a1 x10 x11 ,mul)
                   (tiger-round ,c0 ,c1 ,a0 ,a1 ,b0 ,b1 x20 x21 ,mul)
                   (tiger-round ,a0 ,a1 ,b0 ,b1 ,c0 ,c1 x30 x31 ,mul)
                   (tiger-round ,b0 ,b1 ,c0 ,c1 ,a0 ,a1 x40 x41 ,mul)
                   (tiger-round ,c0 ,c1 ,a0 ,a1 ,b0 ,b1 x50 x51 ,mul)
                   (tiger-round ,a0 ,a1 ,b0 ,b1 ,c0 ,c1 x60 x61 ,mul)
                   (tiger-round ,b0 ,b1 ,c0 ,c1 ,a0 ,a1 x70 x71 ,mul)))
               (key-schedule ()
                 `(progn
                   (sub64 x00 x01
                    (logxor x70 #xa5a5a5a5) (logxor x71 #xa5a5a5a5))
                   (xor64 x10 x11 x00 x01)
                   (add64 x20 x21 x10 x11)
                   (sub64 x30 x31 (ldb (byte 32 0) (logxor x20 (mod32ash (mod32lognot x10) 19)))
                    (ldb (byte 32 0)
                     (mod32lognot (logxor x21
                                     (logior (mod32ash x11 19) (mod32ash x10 -13))))))
                   (xor64 x40 x41 x30 x31)
                   (add64 x50 x51 x40 x41)
                   (sub64 x60 x61 (ldb (byte 32 0)
                                   (mod32lognot (logxor x50
                                                   (logior (mod32ash x40 -23)
                                                           (mod32ash x41 9)))))
                    (ldb (byte 32 0) (logxor x51 (mod32ash (mod32lognot x41) -23))))
                   (xor64 x70 x71 x60 x61)
                   (add64 x00 x01 x70 x71)
                   (sub64 x10 x11 (ldb (byte 32 0) (logxor x00 (mod32ash (mod32lognot x70) 19)))
                    (ldb (byte 32 0) (mod32lognot (logxor x01
                                                     (logior (mod32ash x71 19)
                                                             (mod32ash x70 -13))))))
                   (xor64 x20 x21 x10 x11)
                   (add64 x30 x31 x20 x21)
                   (sub64 x40 x41 (ldb (byte 32 0)
                                   (mod32lognot (logxor x30
                                                   (logior (mod32ash x20 -23)
                                                           (mod32ash x21 9)))))
                    (ldb (byte 32 0) (logxor x31 (mod32ash (mod32lognot x21) -23))))
                   (xor64 x50 x51 x40 x41)
                   (add64 x60 x61 x50 x51)
                   (sub64 x70 x71
                    (logxor x60 #x89abcdef) (logxor x61 #x01234567))))
               (feed-forward ()
                 `(progn
                   (xor64 a0 a1 aa0 aa1)
                   (sub64 b0 b1 bb0 bb1)
                   (add64 c0 c1 cc0 cc1))))
      (save-abc)
      (dotimes (i 3)
        (unless (zerop i) (key-schedule))
        (pass a0 a1 b0 b1 c0 c1 (or (and (zerop i) 5) (and (= i 1) 7) 9))
        (psetq a0 c0 a1 c1 c0 b0 c1 b1 b0 a0 b1 a1))
      #+nil
      (progn
        (pass a0 a1 b0 b1 c0 c1 5)
        (key-schedule)
        (pass c0 c1 a0 a1 b0 b1 7)
        (key-schedule)
        (pass b0 b1 c0 c1 a0 a1 9))
      (feed-forward)
      (setf (tiger-regs-a0 regs) a0 (tiger-regs-a1 regs) a1
            (tiger-regs-b0 regs) b0 (tiger-regs-b1 regs) b1
            (tiger-regs-c0 regs) c0 (tiger-regs-c1 regs) c1)
      regs)))

(defstruct (tiger
             (:constructor %make-tiger-digest nil)
             (:constructor %make-tiger-state (regs amount block buffer buffer-index))
             (:copier nil)
             (:include mdx))
  (regs (initial-tiger-regs) :type tiger-regs :read-only t)
  (block (make-array #.+tiger-block-n-words+
                     :element-type '(unsigned-byte #.+tiger-wordsize+))
    :type tiger-state-block :read-only t))

(defmethod reinitialize-instance ((state tiger) &rest initargs)
  (declare (ignore initargs))
  (replace (tiger-regs state) +pristine-tiger-registers+)
  (setf (tiger-amount state) 0
        (tiger-buffer-index state) 0)
  state)

(defmethod copy-digest ((state tiger) &optional copy)
  (check-type copy (or null tiger))
  (cond
    (copy
     (replace (tiger-regs copy) (tiger-regs state))
     (replace (tiger-buffer copy) (tiger-buffer state))
     (setf (tiger-amount copy) (tiger-amount state)
           (tiger-buffer-index copy) (tiger-buffer-index state))
     copy)
    (t
     (%make-tiger-state (copy-seq (tiger-regs state))
                        (tiger-amount state)
                        (copy-seq (tiger-block state))
                        (copy-seq (tiger-buffer state))
                        (tiger-buffer-index state)))))

(define-digest-updater tiger
  "Update the given tiger-state from sequence, which is either a
simple-string or a simple-array with element-type (unsigned-byte 8),
bounded by start and end, which must be numeric bounding-indices."
  (flet ((compress (state sequence offset)
           (let ((block (tiger-block state)))
             (#.+tiger-block-copy-fn+ block sequence offset)
             (update-tiger-block (tiger-regs state) block))))
    (declare (dynamic-extent #'compress))
    (declare (notinline mdx-updater))
    (mdx-updater state #'compress sequence start end)))

(define-digest-finalizer (tiger 24)
  "If the given tiger-state has not already been finalized, finalize it,
by processing any remaining input in its buffer, with suitable padding
and appended bit-length, as specified by the TIGER standard.

The resulting TIGER message-digest is returned as an array of twenty-four
 (unsigned-byte 8) values.  Calling UPDATE-TIGER-STATE after a call to
FINALIZE-TIGER-STATE results in unspecified behaviour."
  (let ((regs (tiger-regs state))
        (block (tiger-block state))
        (buffer (tiger-buffer state))
        (buffer-index (tiger-buffer-index state))
        (total-length (* 8 (tiger-amount state))))
    (declare (type tiger-regs regs)
             (type (integer 0 63) buffer-index)
             (type tiger-state-block block)
             (type (simple-array (unsigned-byte 8) (*)) buffer))
    ;; Add mandatory bit 1 padding.
    ;; Note that Tiger does this differently from MD5.
    (setf (aref buffer buffer-index) #x01)
    ;; Fill with 0 bit padding
    (loop for index of-type (integer 0 64)
       from (1+ buffer-index) below 64
       do (setf (aref buffer index) #x00))
    (#.+tiger-block-copy-fn+ block buffer 0)
    ;; Flush block first if length wouldn't fit
    (when (>= buffer-index 56)
      (update-tiger-block regs block)
      ;; Create new fully 0 padded block
      (dotimes (i #.+tiger-block-n-words+)
        (setf (aref block i) 0)))
    ;; Add 64bit message bit length
    #.(if (= +tiger-wordsize+ 32)
          '(store-data-length block total-length 14)
          '(setf (aref block 7) total-length))
    ;; Flush last block
    (update-tiger-block regs block)
    ;; Done, remember digest for later calls
    (finalize-registers state regs)))

(defdigest tiger :digest-length 24 :block-length 64)
