REM Script to remove development cruft from this directory

for %%f in (build test doc .) do del %%f\*.bak, %%f\*.ofasl, %%f\*.*~ /s /q
del methods.lisp lists.lisp hash-tables.lisp test\derived-properties /s /q
