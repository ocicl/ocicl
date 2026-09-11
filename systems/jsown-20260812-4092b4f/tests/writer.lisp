(in-package :jsown-tests)

(def-suite test-writers
    :description "Tests the functionality the writer provides"
    :in test-all)

(in-suite test-writers)

(test write
      (is (string= (to-json '(:obj ("foo" . "bar")))
                   "{\"foo\":\"bar\"}")
          "One string element")
      (is (string= (to-json '(:obj ("foo" . 1000)))
                   "{\"foo\":1000}")
          "One number")
      (is (string= (to-json '(:obj ("bar" . 101/10)))
                   "{\"bar\":10.1}")
          "Could fail on some systems due to rounding errors")
      (is (string= (to-json '(:obj ("bar" . 1.e8)))
                   "{\"bar\":100000000.0}")
          "Large floats should not be in e notation due to
           inconsistency in JSON numeral syntax")
      (is (string= (to-json '(:obj ("baz" "bang" "bing" 10 "bonzo")))
                   "{\"baz\":[\"bang\",\"bing\",10,\"bonzo\"]}")
          "list should expand to a json array")
      (is (string= (to-json '(:obj ("baz" :obj ("bang" . "bing") ("bong" . 10))))
                   "{\"baz\":{\"bang\":\"bing\",\"bong\":10}}")
          "Writing of inner objects should work")

      (is (string= (to-json "foo💩λ")
                   "\"foo\\uD83D\\uDCA9\\u03BB\"")
          "Writing of Unicode chars should work."))

(test write*
      (is (string= (to-json* '(:obj ("foo" . "bar")))
                   "{\"foo\":\"bar\"}")
          "One string element")
      (is (string= (to-json* '(:obj ("foo" . 1000)))
                   "{\"foo\":1000}")
          "One number")
      (is (string= (to-json* '(:obj ("bar" . 101/10)))
                   "{\"bar\":10.1}")
          "Could fail on some systems due to rounding errors")
      (is (string= (to-json* '(:obj ("bar" . 1.e8)))
                   "{\"bar\":100000000.0}")
          "Large floats should not be in e notation due to
           inconsistency in JSON numeral syntax")
      (is (string= (to-json* '(:obj ("baz" "bang" "bing" 10 "bonzo")))
                   "{\"baz\":[\"bang\",\"bing\",10,\"bonzo\"]}")
          "list should expand to a json array")
      (is (string= (to-json* '(:obj ("baz" :obj ("bang" . "bing") ("bong" . 10))))
                   "{\"baz\":{\"bang\":\"bing\",\"bong\":10}}")
          "Writing of inner objects should work")

      (is (string= (to-json "foo💩λ")
                   "\"foo\\uD83D\\uDCA9\\u03BB\"")
          "Writing of Unicode chars should work."))
