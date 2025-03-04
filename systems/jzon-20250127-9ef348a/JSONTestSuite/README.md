# JSONTestSuite

This is a simple command-line parser & test runner to run the suite of tests from [JSONTestSuite](https://github.com/nst/JSONTestSuite).

To build, run `make.bat`, then run with `test.bat`

You should see output like:

```
i_number_double_huge_neg_exp.json ... OK
i_number_huge_exp.json ... OK
i_number_neg_int_huge_exp.json ... OK
...
```

If a test passes when it shouldn't, the line will read 'ERROR PASSED' instead of 'OK', and 'ERROR FAILED' when it failed when it should have passed.
