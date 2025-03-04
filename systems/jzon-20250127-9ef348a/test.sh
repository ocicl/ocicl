base_dir=`pwd`
src_dir="$base_dir/src"
test_dir="$base_dir/test"
jzon_asd="$src_dir/com.inuoe.jzon.asd"
jzon_test_asd="$test_dir/com.inuoe.jzon-tests.asd"

test_exp="
(handler-case (sb-ext:exit :code (apply #'com.inuoe.jzon-tests:main sb-ext:*posix-argv*))
  (error ()
    (sb-ext:exit :code 2 :abort t))
  (sb-sys:interactive-interrupt ()
    (sb-ext:exit :code -1073741510 :abort t)))
"

sbcl --noinform \
     --end-runtime-options \
     --no-sysinit \
     --no-userinit \
     --disable-debugger \
     --eval "(load #p\"~/quicklisp/setup.lisp\")" \
     --eval "(asdf:load-asd #p\"$jzon_asd\")" \
     --eval "(asdf:load-asd #p\"$jzon_test_asd\")" \
     --eval "(ql:quickload :com.inuoe.jzon-tests)" \
     --eval "$test_exp"
    
