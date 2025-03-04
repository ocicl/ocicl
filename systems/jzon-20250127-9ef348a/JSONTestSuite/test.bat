@setlocal

@set RUN_EXP=^
(handler-case (sb-ext:exit :code (apply #'com.inuoe.json-test-suite-runner:main sb-ext:*posix-argv*))^
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
 --eval "(require '#:uiop)"^
 --load "%~dp0json-test-suite-runner.lisp"^
 --eval "%RUN_EXP%"^
 --end-toplevel-options^
 "%~dp0bin\jzon-parsing.exe"^
 "%~dp0test_parsing"^
 %*
@if %errorlevel% neq 0 exit /b %errorlevel%

@endlocal
