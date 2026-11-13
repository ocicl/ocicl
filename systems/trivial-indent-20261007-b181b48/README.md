# About Trivial-Indent
This allows you to define custom indentation hints for your macros if the one recognised by Slime, Sly, or other editors automatically produces unwanted results.

# How To
The only feature of this that you will really need to use is `DEFINE-INDENTATION`.

```
(define-indentation defmacro (4 &lambda &body))
(define-indentation something-more-complex (4 &rest (&whole 2 0 4 &body)))
```

The actual specification of the rules is as follows:

Each element in the rule specifies how the corresponding element in the source form is to be indented. If there's more elements in the source form than there are in the rule, the last element of the rule applies to those elements.

Each element in the rule can be one of the following:

- ``NIL``  
  Use the default indentation behaviour for this element.
- an integer  
  Use this number of spaces to indent the element.
- ``&lambda``  
  Treat this element as a lambda list and use the default indentation depth of 4 for itself.
- ``&rest``  
  Must be followed by another element that specifies the behaviour for all remaining elements explicitly.
- ``&body``  
  Use standard Lisp function body indentation rules for this and all remaining elements.
- ``&whole``  
  Must be followed by another element that specifies the behaviour for the current element. This indentation is then used as an additional offset for all remaining elements.
- a cons  
  Specify the indentation rule recursively for elements of the corresponding source list.

# Backends
Currently the following backends are supported:

- Commander
- Slynk
- Swank
