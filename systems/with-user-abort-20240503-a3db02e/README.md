# with-user-abort
### _ava fox_

provides a portable way to capture control-c in your lisp programs

## Exports

*Macro* `with-user-abort` &body body 

executes BODY and captures a user-interrupt (control-c) and signals user-abort condition

*Condition* `user-abort`

a simple condition that inherits from your implementation's user-interrupt. 

## Example

```lisp
(handler-case
	(with-user-abort (long-running-call))
  (user-abort ()
	(print "Quitting gracefully")
	(uiop:quit 1)))
```


---

## License

BSD 3-Clause
