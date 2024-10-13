(defpackage cl-cookie-test
  (:use #:cl
        #:cl-cookie
        #:rove)
  (:import-from #:local-time
		#:timestamp=
		#:parse-timestring)
  (:import-from #:cl-cookie
		#:expired-cookie-p
                #:parse-cookie-date
                #:match-cookie-path
                #:match-cookie))
(in-package :cl-cookie-test)

(deftest parse-cookie-date
  (loop for (date . rfc3339) in '(("Wed, 06-Feb-2008 21:01:38 GMT" . "2008-02-06T21:01:38+0000")
				  ("Wed, 06-Feb-08 21:01:38 GMT"   . "2008-02-06T21:01:38+0000")
				  ("Tue Feb 13 08:00:00 2007 GMT"  . "2007-02-13T08:00:00+0000")
				  ("Wednesday, 07-February-2027 08:55:23 GMT" . "2027-02-07T08:55:23+0000")
				  ("Wed, 07-02-2017 10:34:45 GMT"  . "2017-02-07T10:34:45+0000"))
	do (let ((parsed (parse-cookie-date date)))
	     (ok (timestamp= (local-time:universal-to-timestamp parsed)
			     (parse-timestring rfc3339))))))

(deftest make-cookie
  (ok (string= (cookie-same-site (make-cookie :name "hallo" :same-site :none :secure-p t))
	       "None"))
  (ok (string= (cookie-same-site (make-cookie :name "hallo" :same-site :strict))
	       "Strict"))
  (ok (string= (cookie-same-site (make-cookie :name "hallo" :same-site :lax))
	       "Lax"))
  (ok (make-cookie :name "hallo" :same-site "none" :secure-p t))
  (ok (make-cookie :name "hallo" :same-site "Strict"))
  (ok (make-cookie :name "hallo" :same-site "lax"))
  (ok (signals (make-cookie :name "hallo" :same-site :none)
	  (quote invalid-cookie)))
  (ok (signals (make-cookie :name "hallo" :same-site "None")
	  (quote invalid-cookie)))
  (ok (signals (make-cookie :name "hallo" :same-site "SStrict")
	  (quote type-error)))
  (ok (signals (make-cookie :name "hallo" :same-site :laxx)
	  (quote type-error)))
  (ok (signals (make-cookie :value "asdg")
	  (quote invalid-cookie)))
  (ok (signals (make-cookie)
	  (quote invalid-cookie)))
  (ok (signals (make-cookie :name nil)
	  (quote invalid-cookie)))
  (ok (signals (make-cookie :name "")
	  (quote invalid-cookie)))
  (ok (signals (make-cookie :name "haalllo" :value "asdg" :same-site "None")
	  'invalid-cookie))
  (ok (not (null (make-cookie :name "haalllo" :value "asdg" :same-site "None" :sanity-check nil))))
  (ok (signals (make-cookie :name "haalllo" :partitioned t)
	  'invalid-cookie))
  (ok (not (null (let ((*sanity-check* nil))
		   (make-cookie :name "haalllo" :partitioned t))))))

(deftest parse-set-cookie-header
  (ok (cookie= (parse-set-cookie-header "SID=31d4d96e407aad42" "example.com" "/")
	       (make-cookie :name "SID" :value "31d4d96e407aad42" :origin-host "example.com" :path "/"))
      "name and value")
  (ok (cookie= (parse-set-cookie-header "SID=" "example.com" "/")
	       (make-cookie :name "SID" :value "" :origin-host "example.com" :path "/"))
      "no value")
  (ok (cookie= (parse-set-cookie-header "SID=31d4d96e407aad42; Path=/; Domain=example.com" "example.com" "/")
	       (make-cookie :name "SID" :value "31d4d96e407aad42" :origin-host "example.com" :path "/" :domain "example.com"))
      "path and domain" )
  (ok (cookie-equal (parse-set-cookie-header "SID=31d4d96e407aad42; Expires=Fri, 25 Jan 2002 19:22:06 GMT; Max-age=199999; Path=/"
					     "example.com" "/")
		    (make-cookie :name "SID" :value "31d4d96e407aad42" :origin-host "example.com" :path "/" :max-age 199999 :expires (encode-universal-time 6 22 19 25 1 2002 0)))
      "expires and max-age")
  (ok (cookie-equal (parse-set-cookie-header "SID=31d4d96e407aad42; Max-age=199999; Path=/; Secure; SameSite=Lax; Partitioned"
					     "example.com" "/")
		    (make-cookie :name "SID" :value "31d4d96e407aad42" :origin-host "example.com" :path "/" :partitioned t :secure-p t :max-age 199999 :same-site "Lax"))
      "partitioned, max-age, secure, and same-site")
  (ok (signals (parse-set-cookie-header "SID=31d4d96e407aad42; Max-age=199999; Path=/; SameSite=Lax; Partitioned"
					"example.com" "/")
	  (quote invalid-cookie)))
  (ok (cookie-equal (parse-set-cookie-header "SID=31d4d96e407aad42; Max-age=199999; Path=/; Secure; SameSite=Lax; Partitioned; Secure"
					     "example.com" "/")
		    (make-cookie :name "SID" :value "31d4d96e407aad42" :origin-host "example.com" :path "/" :partitioned t :secure-p t :max-age 199999 :same-site "Lax")))
  (ok (signals (cl-cookie:parse-set-cookie-header "SID=31d4d96e407aad42; Path=/; HttpOnly; Partitioned" "example.com" "/")
	  'invalid-cookie)))

(deftest write-cookie-header
  (ng (write-cookie-header nil))
  (ok (string= (write-cookie-header (make-cookie :name "SID" :value "31d4d96e407aad42"))
	       "SID=31d4d96e407aad42"))
  (ok (string= (write-cookie-header (let ((*sanity-check* nil))(make-cookie)))
	       "="))
  (ok (string= (write-cookie-header (make-cookie :name "test"))
	       "test="))
  (ok (string= (write-cookie-header (list (make-cookie :name "SID" :value "31d4d96e407aad42")
					  (make-cookie :name "lang" :value "en-US")))
	       "SID=31d4d96e407aad42; lang=en-US")))

(deftest match-cookie-path
  (ok (match-cookie-path "/" "/"))
  (ok (match-cookie-path "/" ""))
  (ok (match-cookie-path "" "/"))
  (ng (match-cookie-path "/" "/accounts"))
  (ok (match-cookie-path "/accounts" "/"))
  (ok (match-cookie-path "/accounts/nitro_idiot" "/"))
  (ng (match-cookie-path "/" "/accounts"))
  (ok (match-cookie-path "/accounts" "/accounts"))
  (ok (match-cookie-path "/accounts/" "/accounts"))
  (ng (match-cookie-path "/accounts-page" "/accounts"))
  (ok (match-cookie-path "/accounts/nitro_idiot" "/accounts")))

(deftest match-cookie
  (testing "cookie with domain and path"
    (let ((cookie
            (make-cookie :name "LSID" :value "DQAAAK...Eaem_vYg" :origin-host "docs.foo.com"
                         :domain ".foo.com" :path "/accounts")))
      (testing "path"
	(ng (match-cookie cookie "docs.foo.com" "/"))
	(ok (match-cookie cookie "docs.foo.com" "/accounts"))
	(ok (match-cookie cookie "docs.foo.com" "/accounts/"))
	(ok (match-cookie cookie "docs.foo.com" "/accounts/nitro_idiot"))
	(ng (match-cookie cookie "docs.foo.com" "/accounts-page" :securep t)))
      (testing "domain"
	(ng (match-cookie cookie "foo.com" "/" :securep t)
	    "Send only to the origin-host when :host is NIL")
	(ng (match-cookie cookie "one.docs.foo.com" "/" :securep t)
	    "Send only to the origin-host when :host is NIL"))))
  (testing "cookie with path"
    (let ((cookie
            (make-cookie :name "LSID" :value "DQAAAK...Eaem_vYg" :origin-host "docs.foo.com"
                         :path "/accounts" :secure-p t :httponly-p t)))
      (testing "secure"
	(ng (match-cookie cookie "docs.foo.com" "/accounts"))
	(ok (match-cookie cookie "docs.foo.com" "/accounts" :securep t)))
      (testing "path"
	(ng (match-cookie cookie "docs.foo.com" "/" :securep t))
	(ok (match-cookie cookie "docs.foo.com" "/accounts" :securep t))
	(ok (match-cookie cookie "docs.foo.com" "/accounts/" :securep t))
	(ok (match-cookie cookie "docs.foo.com" "/accounts/nitro_idiot" :securep t))
	(ng (match-cookie cookie "docs.foo.com" "/accounts-page" :securep t)))
      (testing "domain"
	(ng (match-cookie cookie "foo.com" "/" :securep t)
	    "Send only to the origin-host when :host is NIL")
	(ng (match-cookie cookie "one.docs.foo.com" "/" :securep t)
	    "Send only to the origin-host when :host is NIL")))))

(deftest cookie-jar
  (let ((cookie-jar (make-cookie-jar)))
    (ok (= (length (cookie-jar-cookies cookie-jar)) 0)
        "initial cookie jar is empty")
    (merge-cookies cookie-jar
                   (list (make-cookie :name "SID" :value "31d4d96e407aad42" :domain "example.com" :path "/")
                         (make-cookie :name "lang" :value "en-US" :domain "example.com" :path "/accounts")))
    (ok (= (length (cookie-jar-cookies cookie-jar)) 2))
    (merge-cookies cookie-jar
                   (list (make-cookie :name "id" :value "30" :domain "example.com")))
    (ok (= (length (cookie-jar-cookies cookie-jar)) 3))
    (merge-cookies cookie-jar
                   (list (make-cookie :name "lang" :value "ja-JP" :domain "example.com" :path "/accounts")))

    (testing "can overwrite"
      (ok (= (length (cookie-jar-cookies cookie-jar)) 3))
      (ok (string= (cookie-value
		    (find "lang" (cookie-jar-cookies cookie-jar) :key #'cookie-name :test #'string=))
		   "ja-JP")))

    (testing "not overwrite other domain cookies"
      (merge-cookies cookie-jar
                     (list (make-cookie :name "lang" :value "fr-FR" :domain "www.example.com")))
      (ok (= (length (cookie-jar-cookies cookie-jar)) 4)))

    (testing "Cross site cooking"
      (merge-cookies cookie-jar
                     (list (make-cookie :name "name" :value "Ultraman" :domain ".com")))
      (ng (cookie-jar-host-cookies cookie-jar "hatena.com" "/")))))

(deftest write-set-cookie-header
  (ok (string= (write-set-cookie-header (make-cookie :name "SID" :value "31d4d96e407aad42"))
	       "SID=31d4d96e407aad42")
      "name and value")
  (ok (string= (write-set-cookie-header (make-cookie :name "SID" :value "31d4d96e407aad42" :domain "www.example.com"))
	       "SID=31d4d96e407aad42; Domain=www.example.com")
      "name, value, and domain")
  (ok (string= (write-set-cookie-header (make-cookie :name "SID" :value "31d4d96e407aad42" :domain "www.example.com" :path "/users"))
	       "SID=31d4d96e407aad42; Path=/users; Domain=www.example.com")
      "name, value, domain, and path")
  (ok (string= (write-set-cookie-header (make-cookie :name "SID" :value "31d4d96e407aad42" :expires (encode-universal-time 6 22 19 25 1 2002 0)))
	       "SID=31d4d96e407aad42; Expires=Fri, 25 Jan 2002 19:22:06 GMT")
      "name, value, and expires")
  (ok (string= (write-set-cookie-header (make-cookie :name "SID" :value "31d4d96e407aad42" :max-age 3600 :same-site "Strict" :secure-p t :partitioned t))
	       "SID=31d4d96e407aad42; Max-age=3600; SameSite=Strict; Partitioned; Secure")
      "max-age, same-site, partitioned, secure")
  (ok (string= (write-set-cookie-header (make-cookie :name "adsg"))
	       "adsg="))
  (ok (string= (write-set-cookie-header (make-cookie :name "adsg" :value nil))
	       "adsg="))
  (ok (string= (write-set-cookie-header (let ((*sanity-check* nil))
					  (make-cookie :value "test-value" :secure-p t)))
	       "=test-value; Secure"))
  (ok (signals (write-set-cookie-header (make-cookie :name "SID" :value "31d4d96e407aad42" :max-age 3600 :same-site "Strict" :partitioned t))
	  (quote invalid-cookie)))
  (ok (string= (write-set-cookie-header (make-cookie :name "SID" :value "31d4d96e407aad42" :max-age 3600 :same-site "Strict" :partitioned t :sanity-check nil))
	       "SID=31d4d96e407aad42; Max-age=3600; SameSite=Strict; Partitioned")
      "max-age, same-site, partitioned, secure, no sanity-check")
  (ok (let ((*sanity-check* nil))
	(string= (write-set-cookie-header (make-cookie :name "SID" :value "31d4d96e407aad42" :max-age 3600 :same-site "Strict" :partitioned t))
		 "SID=31d4d96e407aad42; Max-age=3600; SameSite=Strict; Partitioned"))
      "max-age, same-site, partitioned, secure, no sanity-check")
  (ok (string= (write-set-cookie-header (make-cookie :name "SID" :value "31d4d96e407aad42" :expires (encode-universal-time 6 22 19 25 1 2002 0)
						     :secure-p t :httponly-p t))
	       "SID=31d4d96e407aad42; Expires=Fri, 25 Jan 2002 19:22:06 GMT; Secure; HttpOnly")))

(deftest expired-cookie-p
  (ok (expired-cookie-p
       (make-cookie :name "SID" :value "31d4d96e407aad42" :expires (encode-universal-time 6 22 19 25 1 2002 0)
		    :secure-p t :httponly-p t)))
  (ng (expired-cookie-p
       (make-cookie :name "SID" :value "31d4d96e407aad42" :expires (encode-universal-time 6 22 19 25 1 2002 0) :max-age 10000000000
		    :secure-p t :httponly-p t)))
  (ng (expired-cookie-p
       (make-cookie :name "SID" :value "31d4d96e407aad42" :expires (encode-universal-time 6 22 19 25 1 2300 0) :max-age 10000000
		    :secure-p t :httponly-p t)))
  (ng (expired-cookie-p (make-cookie :name "SID" :value "31d4d96e407aad42"))))

