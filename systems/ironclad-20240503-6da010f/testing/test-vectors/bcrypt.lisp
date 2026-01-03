(in-package :crypto-tests)


(rtest:deftest bcrypt-1
    (run-kdf-test (crypto:make-kdf 'crypto:bcrypt)
                  (crypto:ascii-string-to-byte-array "Kk4DQuMMfZL9o")
                  (crypto:hex-string-to-byte-array "79762be9970f5be73ac77c0e4f0a3851")
                  16
                  24
                  (crypto:hex-string-to-byte-array "db8f0360d2aa48e1415598bbc1b5c0d9103043ea39686ad2"))
  t)

(rtest:deftest bcrypt-2
    (run-kdf-test (crypto:make-kdf 'crypto:bcrypt)
                  (crypto:ascii-string-to-byte-array "U*U")
                  (crypto:hex-string-to-byte-array "10410410410410410410410410410410")
                  32
                  24
                  (crypto:hex-string-to-byte-array "1bb69143f9a8d304c8d23d99ab049a77a68e2ccc744206bb"))
  t)

(rtest:deftest bcrypt-3
    (run-kdf-test (crypto:make-kdf 'crypto:bcrypt)
                  (crypto:ascii-string-to-byte-array "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
                  (crypto:hex-string-to-byte-array "71d79f8218a39259a7a29aabb2dbafc3")
                  32
                  24
                  (crypto:hex-string-to-byte-array "eeee31f80919920425881002d140d555b28a5c72e00f097d"))
  t)

(rtest:deftest bcrypt-4
    (run-kdf-test (crypto:make-kdf 'crypto:bcrypt)
                  (crypto:ascii-string-to-byte-array "/MH51`!BP&0tj3%YCA;Xk%e3S`o\\EI")
                  (crypto:hex-string-to-byte-array "811902780b80d2a0e4e43f2bde449612")
                  1024
                  24
                  (crypto:hex-string-to-byte-array "d69e68b5c372ad72fb6488275f4687334afb1dba41d00cf1"))
  t)

(rtest:deftest bcrypt-pbkdf-1
    (run-kdf-test (crypto:make-kdf 'crypto:bcrypt-pbkdf)
                  (crypto:ascii-string-to-byte-array "password")
                  (crypto:ascii-string-to-byte-array "salt")
                  4
                  32
                  (crypto:hex-string-to-byte-array "5bbf0cc293587f1c3635555c27796598d47e579071bf427e9d8fbe842aba34d9"))
  t)

(rtest:deftest bcrypt-pbkdf-2
    (run-kdf-test (crypto:make-kdf 'crypto:bcrypt-pbkdf)
                  (crypto:ascii-string-to-byte-array "password")
                  (crypto:hex-string-to-byte-array "00")
                  4
                  16
                  (crypto:hex-string-to-byte-array "c12b566235eee04c212598970a579a67"))
  t)

(rtest:deftest bcrypt-pbkdf-3
    (run-kdf-test (crypto:make-kdf 'crypto:bcrypt-pbkdf)
                  (crypto:ascii-string-to-byte-array "password")
                  (crypto:ascii-string-to-byte-array "salt")
                  8
                  64
                  (crypto:hex-string-to-byte-array "e1367ec5151a33faac4cc1c144cd23fa15d5548493ecc99b9b5d9c0d3b27bec76227ea66088b849b20ab7aa478010246e74bba51723fefa9f9474d6508845e8d"))
  t)

(rtest:deftest bcrypt-pbkdf-4
    (run-kdf-test (crypto:make-kdf 'crypto:bcrypt-pbkdf)
                  (crypto:hex-string-to-byte-array "0db3ac94b3ee53284f4a22893b3c24ae")
                  (crypto:hex-string-to-byte-array "3a62f0f0dbcef823cfcc854856ea1028")
                  8
                  256
                  (crypto:hex-string-to-byte-array "2054b9fff34e3721440334746828e9ed38de4b72e0a69adc170a13b5e8d646385ea4034ae6d26600ee2332c5ed40ad557c86e3403fbb30e4e1dc1ae06b99a071368f518d2c426651c9e7e437fd6c915b1bbfc3a4cea71491490ea7afb7dd0290a678a4f441128db1792eab2776b21eb4238e0715add4127dff44e4b3e4cc4c4f9970083f3f74bd698873fdf648844f75c9bf7f9e0c4d9e5d89a7783997492966616707611cb901de31a19726b6e08c3a8001661f2d5c9dcc33b4aa072f90dd0b3f548d5eeba4211397e2fb062e526e1d68f46a4ce256185b4badc2685fbe78e1c7657b59f83ab9ab80cf9318d6add1f5933f12d6f36182c8e8115f68030a1244"))
  t)
