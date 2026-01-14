(in-package :jsown-tests)

(def-suite test-readers
    :description "Tests the functionality the reader provides"
    :in test-all)

(in-suite test-readers)

(test parse-singular
      (is (equal (parse "{\"foo\":\"bar\"}")
                 '(:obj ("foo" . "bar")))
          "Matching of a single simple string")
      (is (equal (parse "{\"bar\":1000}")
                 '(:obj ("bar" . 1000)))
          "Matching of a single number")
      (is (equal (parse "{\"bar\":10.1}")
                 '(:obj ("bar" . 101/10)))
          "Matching of a single rational")
      (is (equal (parse "{\"bar\":[\"foo\",10,101.10]}")
                 '(:obj ("bar" "foo" 10 1011/10)))
          "Matching of an array with various types of elements"))

(test parse-multiple
      (is (equal (parse "{\"foo\":\"bar\",\"baz\":\"bang\",\"bing\":100,\"bingo\":1.1,\"bazo\":[1,2,\"foo\"]}")
                 '(:obj ("foo" . "bar") ("baz" . "bang") ("bing" . 100) ("bingo" . 11/10) ("bazo" 1 2 "foo")))
          "Parsing of multiple items of all kinds"))

(test parse-nested
      (is (equal (parse "{\"foo\":{\"bar\":\"baz\"}}")
                 '(:obj ("foo" :obj ("bar" . "baz"))))
          "One object in one object")
      (is (equal (parse "{\"foo\":\"bar\",\"bie\":{\"bar\":\"baz\",\"bang\":1000},\"bing\":\"bingo\"}")
                 '(:obj ("foo" . "bar") ("bie" :obj ("bar" . "baz") ("bang" . 1000)) ("bing" . "bingo"))))
      (is (equal (parse "{\"foo\":[{\"foo\":\"bar\",\"baz\":1000}]}")
                 '(:obj ("foo" (:obj ("foo" . "bar") ("baz" . 1000)))))
          "Object in an array")
      (is (equal (parse "{\"foo\":\"bar\",\"baz\":{\"boo\":100.10}}")
                 '(:obj ("foo" . "bar") ("baz" :obj ("boo" . 1001/10))))
          "Rational in inner object"))

(test some-keys-only
      (is (equal (parse "{\"foo\":\"bar\",\"baz\":\"bang\",\"bing\":\"bang\"}" "foo")
                 '(:obj ("foo" . "bar")))
          "The first keyword one element")
      (is (equal (parse "{\"foo\":\"bar\",\"baz\":\"bang\",\"bing\":\"bang\"}" "baz")
                 '(:obj ("baz" . "bang")))
          "Just one element")
      (is (equal (parse "{\"foo\":\"bar\",\"baz\":\"bang\",\"bing\":\"bang\"}" "bing")
                 '(:obj ("bing" . "bang")))
          "The last keyword")
      (is (equal (parse "{\"foo\":\"bar\",\"baz\":\"bang\",\"bing\":\"bang\"}" "foo" "bing")
                 '(:obj ("foo" . "bar") ("bing" . "bang")))
          "Two keywords")
      (is (equal (parse "{\"foo\":\"bar\",\"baz\":\"bang\",\"bing\":\"bang\"}" "foo" "bing" "baz")
                 '(:obj ("foo" . "bar") ("baz" . "bang") ("bing" . "bang")))
          "All keywords"))

(test with-string-container
      (let ((container (jsown:build-key-container "foo")))
        (is (equal (parse-with-container "{\"foo\":\"bar\",\"baz\":\"bang\",\"bing\":\"bang\"}" container)
                   '(:obj ("foo" . "bar")))
            "The first keyword one element"))
      (let ((container (jsown:build-key-container "foo" "bing")))
        (is (equal (parse-with-container "{\"foo\":\"bar\",\"baz\":\"bang\",\"bing\":\"bang\"}" container)
                   '(:obj ("foo" . "bar") ("bing" . "bang")))
            "First and last element"))
      (let ((container (jsown:build-key-container "foo" "bing" "baz")))
        (is (equal (parse-with-container "{\"foo\":\"bar\",\"baz\":\"bang\",\"bing\":\"bang\"}" container)
                   '(:obj ("foo" . "bar") ("baz" . "bang") ("bing" . "bang")))
            "All elements")))

(test unicode-chars
  (is (equal (parse "{\"\\u03BBlambda\":\"\\ud83d\\udca9poop\"}")
             '(:obj ("λlambda" . "💩poop"))))

  (is (equal (parse "{\"lambda\\u03BB\":\"poop\\ud83d\\udca9\"}")
             '(:obj ("lambdaλ" . "poop💩"))))

  (is (equal (parse "{\"lambda\\u03BBlambda\":\"poop\\ud83d\\udca9poop\"}")
             '(:obj ("lambdaλlambda" . "poop💩poop")))))
