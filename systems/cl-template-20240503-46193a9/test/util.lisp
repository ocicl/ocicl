(in-package #:cl-template-tests)

(def-suite util-tests)

(in-suite util-tests)

(test scan-string-until-ignoring
  "Test the function to scan a string until a certain delimiter."
  (labels ((scan-string-until-ignoring (&rest args)
             (apply #'cl-template::scan-string-until-ignoring args)))
    (is (string= "yeah" (scan-string-until-ignoring "yeah" "%}")))
    (is (string= "abc" (scan-string-until-ignoring "abc|def" "|")))
    (is (string= "" (scan-string-until-ignoring "&stuff" "&")))
    (is (string= "hello" (scan-string-until-ignoring "hello##world" "##")))
    (is (string= "#hello" (scan-string-until-ignoring "#hello###world" "###")))
    (is (string= "abc <ignor|ing> def" (scan-string-until-ignoring "abc <ignor|ing> def" "|" :ignore-list '((#\< . #\>)))))
    (is (string= "\"{{stuff}}\"}now" (scan-string-until-ignoring "\"{{stuff}}\"}now}}" "}}" :ignore-list '((#\" . #\")))))
    (is (string= " stuff " (scan-string-until-ignoring "some <% stuff %>" "%>" :start 7)))
    (is (string= "yeah " (scan-string-until-ignoring "yeah garbage" "garbage")))
    (is (string= "ye" (scan-string-until-ignoring "garbage ye:ah garbage" ":" :start 8 :end 13)))))

(test scan-between-delimiters
  "Test scanning a string for a substring between two delimiters."
  (labels ((scan-between-delimiters (&rest args)
             (apply #'cl-template::scan-between-delimiters args)))
    (is (string= "" (scan-between-delimiters "thingy" "{%" "%}")))
    (is (string= " stuff " (scan-between-delimiters "<% stuff %>" "<%" "%>")))
    (is (string= "stuff" (scan-between-delimiters "moar <%=stuff%>" "<%=" "%>")))
    (is (string= " whale " (scan-between-delimiters "<html><%= whale %></html>" "<%=" "%>")))
    (is (string= " other \"<%stuff%>\" " (scan-between-delimiters "<% other \"<%stuff%>\" %>" "<%" "%>" :ignore-list '((#\" . #\")))))))

(test match-pairs-ignoring
  "Test counting the occurrences of a pair in a string."
  (labels ((match-pairs-ignoring (&rest args)
             (apply #'cl-template::match-pairs-ignoring args)))
    (is (= 0 (match-pairs-ignoring "(abc)" '(#\( . #\)))))
    (is (= 1 (match-pairs-ignoring "[hi" '(#\[ . #\]))))
    (is (= -2 (match-pairs-ignoring "yo>>" '(#\< . #\>))))
    (is (= 0 (match-pairs-ignoring ":dolphin<:>olympics;" '(#\: . #\;) :ignore-list '((#\< . #\>)))))))
