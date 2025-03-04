@setlocal

@set SRC_DIR=%~dp0..\src\
@set BIN_DIR=%~dp0bin\
@set JZON_ASD=%SRC_DIR%com.inuoe.jzon.asd
@set JZON_EXE=%BIN_DIR%jzon-parsing.exe

@rem Escape backslashes for SBCL string literals
@set JZON_ASD_E=%JZON_ASD:\=\\%
@set JZON_EXE_E=%JZON_EXE:\=\\%

@set BUILD_EXP=^
(sb-ext:save-lisp-and-die \"%JZON_EXE_E%\"^
  :toplevel (lambda ()^
              (handler-case (sb-ext:exit :code (apply #'com.inuoe.jzon-parsing:main sb-ext:*posix-argv*))^
                (error ()^
                  (sb-ext:exit :code 2 :abort t))^
                (sb-sys:interactive-interrupt ()^
                  (sb-ext:exit :code -1073741510 :abort t))))^
  :executable t)

@mkdir "%BIN_DIR%"
@sbcl^
 --noinform^
 --end-runtime-options^
 --no-sysinit^
 --no-userinit^
 --disable-debugger^
 --eval "(load """"~/quicklisp/setup.lisp"""")"^
 --eval "(asdf:load-asd #p""%JZON_ASD_E%\"")"^
 --eval "(ql:quickload '#:com.inuoe.jzon)"^
 --eval "(ql:quickload '#:cl-json)"^
 --eval "(ql:quickload '#:jonathan)"^
 --eval "(ql:quickload '#:json-streams)"^
 --eval "(ql:quickload '#:jsown)"^
 --eval "(ql:quickload '#:shasht)"^
 --eval "(ql:quickload '#:yason)"^
 --load "%~dp0jzon-parsing.lisp"^
 --eval "%BUILD_EXP%"
@if %errorlevel% neq 0 exit /b %errorlevel%

@endlocal
