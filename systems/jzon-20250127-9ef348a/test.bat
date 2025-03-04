@setlocal

@set SRC_DIR=%~dp0src\
@set TEST_DIR=%~dp0test\
@set JZON_ASD=%SRC_DIR%com.inuoe.jzon.asd
@set JZON_TESTS_ASD=%TEST_DIR%com.inuoe.jzon-tests.asd

@rem Escape backslashes for SBCL string literals
@set JZON_ASD_E=%JZON_ASD:\=\\%
@set JZON_TESTS_ASD_E=%JZON_TESTS_ASD:\=\\%

@set TEST_EXP=^
(handler-case (sb-ext:exit :code (apply #'com.inuoe.jzon-tests:main sb-ext:*posix-argv*))^
  (error ()^
    (sb-ext:exit :code 2 :abort t))^
  (sb-sys:interactive-interrupt ()^
    (sb-ext:exit :code -1073741510 :abort t)))

@sbcl^
 --noinform^
 --end-runtime-options^
 --no-sysinit^
 --no-userinit^
 --disable-debugger^
 --eval "(load """"~/quicklisp/setup.lisp"""")"^
 --eval "(asdf:load-asd #p""%JZON_ASD_E%\"")"^
 --eval "(asdf:load-asd #p""%JZON_TESTS_ASD_E%\"")"^
 --eval "(ql:quickload '#:com.inuoe.jzon-tests)"^
 --eval "%TEST_EXP%"
@if %errorlevel% neq 0 exit /b %errorlevel%

@endlocal
