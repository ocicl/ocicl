(in-package #:cl-template-tests)

(def-suite templating-tests)

(in-suite templating-tests)

(test compile-template
  "Test that templates compile correctly."
  (labels ((compile-template (string)
             (cl-template::internal-compile-template string "<%" "<%=" "%>" '__stream)))
    (is (equal
         '(macrolet ((@ (var) `(getf cl-template::__data ,(intern (symbol-name var) :keyword))))
           (with-output-to-string (__stream)
             (write-string "I need more creative dummy data." __stream)))
         (compile-template "I need more creative dummy data."))
        "Should compile strings.")
    (is (equal
         '(macrolet ((@ (var) `(getf cl-template::__data ,(intern (symbol-name var) :keyword))))
           (with-output-to-string (__stream)
             (write-string fish __stream)))
         (compile-template "<%= fish %>"))
        "Should echo a variable.")
    (is (equal
         '(macrolet ((@ (var) `(getf cl-template::__data ,(intern (symbol-name var) :keyword))))
           (with-output-to-string (__stream)
             (write-string "fish: " __stream)
             (write-string (@ fish) __stream)
             (write-string "..." __stream)))
         (compile-template "fish: <%= @ fish %>..."))
        "Should add implicit parenthesis.")
    (is (equal
         '(macrolet ((@ (var) `(getf cl-template::__data ,(intern (symbol-name var) :keyword))))
           (with-output-to-string (__stream)
             (write-string "yeah! " __stream)
             (write-string (extra-fish) __stream)))
         (compile-template "yeah! <%= (extra-fish) %>"))
        "Should keep explicit parenthesis.")
    (is (equal
         '(macrolet ((@ (var) `(getf cl-template::__data ,(intern (symbol-name var) :keyword))))
           (with-output-to-string (__stream)
             (write-string "The number is " __stream)
             (write-string (format nil "~r" n) __stream)))
         (compile-template "The number is <%= format nil \"~r\" n %>"))
        "Should echo the result of a function, inferring parenthesis.")
    (is (equal
         '(macrolet ((@ (var) `(getf cl-template::__data ,(intern (symbol-name var) :keyword))))
           (with-output-to-string (__stream)
             (if (@ thing)
                 (progn
                   (write-string (@ thing) __stream)))))
         (compile-template "<% if (@ thing) %><%= @ thing %><% end %>"))
        "Should compile an IF block, inferring parenthesis and adding a progn.")
    (is (equal
         '(macrolet ((@ (var) `(getf cl-template::__data ,(intern (symbol-name var) :keyword))))
           (with-output-to-string (__stream)
             (if (@ thing)
                 (progn
                   (write-string (@ thing) __stream)))))
         (compile-template "<% (if (@ thing) %><%= @ thing %><% ) %>"))
        "Should compile an IF block with explicit parenthesis and add a progn.")
    (is (equal
         '(macrolet ((@ (var) `(getf cl-template::__data ,(intern (symbol-name var) :keyword))))
           (with-output-to-string (__stream)
             (if (@ thing)
                 (write-string (@ thing) __stream))))
         (let ((*add-progn-to-if* nil))
           (compile-template "<% if (@ thing) %><%= @ thing %><% end %>")))
        "Should not add a progn in an IF block if *add-progn-to-if* is nil.")
    (is (equal
         '(macrolet ((@ (var) `(getf cl-template::__data ,(intern (symbol-name var) :keyword))))
           (with-output-to-string (__stream)
             (if (@ thing)
                 (progn
                   (write-string (@ thing) __stream))
                 (progn
                   (write-string "No thing!" __stream)))))
         (compile-template "<% if (@ thing) %><%= @ thing %><% else %>No thing!<% end %>"))
        "Should compile an IF block with an ELSE.") 
    (is (equal
         '(macrolet ((@ (var) `(getf cl-template::__data ,(intern (symbol-name var) :keyword))))
           (with-output-to-string (__stream)
             (loop for i below 10 do
                  (write-string "i: " __stream)
                  (write-string (write-to-string i) __stream))))
         (compile-template "<% loop for i below 10 do %>i: <%= write-to-string i %><% end %>"))
        "Should compile a LOOP block, inferring parenthesis.")
    (is (equal
         '(macrolet ((@ (var) `(getf cl-template::__data ,(intern (symbol-name var) :keyword))))
           (with-output-to-string (__stream)
             (loop for person in (getf __data :people) do
                  (write-string "Name: " __stream)
                  (write-string (name person) __stream))))
         (compile-template "<% (loop for person in (getf __data :people) do %>Name: <%= (name person) %><% ) %>"))
        "Should compile a LOOP block with explicit parenthesis.")
    (is (equal
         '(macrolet ((@ (var) `(getf cl-template::__data ,(intern (symbol-name var) :keyword))))
           (with-output-to-string (__stream)
             (let ((x "42"))
               (write-string "x: " __stream)
               (write-string x __stream))))
         (compile-template "<% let ((x \"42\")) %>x: <%= x %><% end %>"))
        "Should compile a LET block with implicit parenthesis.")))

(test run-template
  "Test that templates run correctly."
  (labels ((run-template (string &optional data)
             (funcall (compile-template string) data)))
    (is (string= "Hello world!" (run-template "Hello world!"))
        "Shouldn't do anything to strings without template directives.")
    (is (string= "red fish" (run-template "<%= getf cl-template::__data :color %> fish" '(:color "red")))
        "Should echo variables.")
    (is (string= "CXXIII" (run-template "<%= format nil \"~@r\" 123 %>"))
        "Should echo the result of a function.")
    (is (string= "fifty-eight" (run-template "<%= format nil \"~r\" (@ n) %>" '(:n 58)))
        "Should echo the result of a function with data provided by the caller.")
    (is (string= "thing 1 thing 2 thing 3 "
                 (run-template "<% loop for i from 1 to 3 do %>thing <%= write-to-string i %> <% end %>"))
        "Should execute a LOOP properly.")
    (is (string= "one" (run-template "<% if (= (getf cl-template::__data :x) 1) %>one<% end %>" '(:x 1)))
        "Should support conditionals.")
    (is (string= "" (run-template "<% if (= (@ x) 1) %>one<% end %>" '(:x 0)))
        "Should return an empty string if the conditional doesn't match.")))

(test cache-template
  "Test that templates are cached."
  (is (eq
       (compile-template "hello world!")
       (compile-template "hello world!")))
  (is (not (eq
            (compile-template "hurray!")
            (compile-template "meh.")))))
