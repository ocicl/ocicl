# trivial-cltl2

Simple [CLTL2](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node102.html) Compatibility Layer.

Exports symbols from implementation-specific packages.

## Interface

### compiler-let
*Macro* **compiler-let**  (**&rest** *bindings*) **&body** *body*

#### Description
When executed by the Lisp interpreter, compiler-let behaves exactly like let with all the variable bindings implicitly declared special. When the compiler processes this form, however, no code is compiled for the bindings; instead, the processing of the body by the compiler (including, in particular, the expansion of any macro calls within the body) is done with the special variables bound to the indicated values in the execution context of the compiler. This is primarily useful for communication among complicated macros.
Declarations may not appear at the beginning of the body of a compiler-let.

#### Example

``` common-lisp

(defmacro bar ()
  (declare (special *macro-being-processed*))
  (format t "Bar being expanded in ~A" *macro-being-processed*)
  `(values))

(defmacro foo ()
  `(compiler-let ((*macro-being-processed* 'foo))
    (bar)))
```

### variable-information
*Function* **variable-information** *variable* **&optional** *env*

#### Description
This function returns information about the interpretation of the symbol variable when it appears as a variable within the lexical environment env. Three values are returned.

The first value indicates the type of definition or binding for variable in env:

nil
There is no apparent definition or binding for variable.
:special
The variable refers to a special variable, either declared or proclaimed.
:lexical
The variable refers to a lexical variable.
:symbol-macro
The variable refers to a symbol-macrolet binding.
:constant
Either the variable refers to a named constant defined by defconstant or the variable is a keyword symbol.
The second value indicates whether there is a local binding of the name. If the name is locally bound, the second value is true; otherwise, the second value is nil.

The third value is an a-list containing information about declarations that apply to the apparent binding of the variable. The keys in the a-list are symbols that name declaration specifiers, and the format of the corresponding value in the cdr of each pair depends on the particular declaration name involved. The standard declaration names that might appear as keys in this a-list are:

dynamic-extent
A non-nil value indicates that the variable has been declared dynamic-extent. If the value is nil, the pair might be omitted.
ignore
A non-nil value indicates that the variable has been declared ignore. If the value is nil, the pair might be omitted.
type
The value is a type specifier associated with the variable by a type declaration or an abbreviated declaration such as (fixnum variable). If no explicit association exists, either by proclaim or declare, then the type specifier is t. It is permissible for implementations to use a type specifier that is equivalent to or a supertype of the one appearing in the original declaration. If the value is t, the pair might be omitted.
If an implementation supports additional declaration specifiers that apply to variable bindings, those declaration names might also appear in the a-list. However, the corresponding key must not be a symbol that is external in any package defined in the standard or that is otherwise accessible in the common-lisp-user package.
The a-list might contain multiple entries for a given key. The consequences of destructively modifying the list structure of this a-list or its elements (except for values that appear in the a-list as a result of define-declaration) are undefined.

Note that the global binding might differ from the local one and can be retrieved by calling variable-information with a null lexical environment.

### function-information
*Function* **function-information** *function* **&optional** *env*

#### Description
This function returns information about the interpretation of the function-name function when it appears in a functional position within lexical environment env. Three values are returned.

The first value indicates the type of definition or binding of the function-name which is apparent in env:

nil
There is no apparent definition for function.
:function
The function refers to a function.
:macro
The function refers to a macro.
:special-form
The function refers to a special form.
Some function-names can refer to both a global macro and a global special form. In such a case the macro takes precedence and :macro is returned as the first value.
The second value specifies whether the definition is local or global. If local, the second value is true; it is nil when the definition is global.

The third value is an a-list containing information about declarations that apply to the apparent binding of the function. The keys in the a-list are symbols that name declaration specifiers, and the format of the corresponding values in the cdr of each pair depends on the particular declaration name involved. The standard declaration names that might appear as keys in this a-list are:

dynamic-extent
A non-nil value indicates that the function has been declared dynamic-extent. If the value is nil, the pair might be omitted.
inline
The value is one of the symbols inline, notinline, or nil to indicate whether the function-name has been declared inline, declared notinline, or neither, respectively. If the value is nil, the pair might be omitted.
ftype
The value is the type specifier associated with the function-name in the environment, or the symbol function if there is no functional type declaration or proclamation associated with the function-name. This value might not include all the apparent ftype declarations for the function-name. It is permissible for implementations to use a type specifier that is equivalent to or a supertype of the one that appeared in the original declaration. If the value is function, the pair might be omitted.
If an implementation supports additional declaration specifiers that apply to function bindings, those declaration names might also appear in the a-list. However, the corresponding key must not be a symbol that is external in any package defined in the standard or that is otherwise accessible in the common-lisp-user package.
The a-list might contain multiple entries for a given key. In this case the value associated with the first entry has precedence. The consequences of destructively modifying the list structure of this a-list or its elements (except for values that appear in the a-list as a result of define-declaration) are undefined.

Note that the global binding might differ from the local one and can be retrieved by calling function-information with a null lexical environment.

### declaration-information
*Function* **declaration-information** *decl-name* **&optional** *env*

#### Description
This function returns information about declarations named by the symbol decl-name that are in force in the environment env. Only declarations that do not apply to function or variable bindings can be accessed with this function. The format of the information that is returned depends on the decl-name involved.

It is required that this function recognize optimize and declaration as decl-names. The values returned for these two cases are as follows:

optimize
A single value is returned, a list whose entries are of the form (quality value), where quality is one of the standard optimization qualities (speed, safety, compilation-speed, space, debug) or some implementation-specific optimization quality, and value is an integer in the range 0 to 3 (inclusive). The returned list always contains an entry for each of the standard qualities and for each of the implementation-specific qualities. In the absence of any previous declarations, the associated values are implementation-dependent. The list might contain multiple entries for a quality, in which case the first such entry specifies the current value. The consequences of destructively modifying this list or its elements are undefined.
declaration
A single value is returned, a list of the declaration names that have been proclaimed as valid through the use of the declaration proclamation. The consequences of destructively modifying this list or its elements are undefined.
If an implementation is extended to recognize additional declaration specifiers in declare or proclaim, it is required that either the declaration-information function should recognize those declarations also or the implementation should provide a similar accessor that is specialized for that declaration specifier. If declaration-information is used to return the information, the corresponding decl-name must not be a symbol that is external in any package defined in the standard or that is otherwise accessible in the common-lisp-user package.

### augment-environment
*Function* **augment-environment** *env* **&key** *variable* *symbol-macro* *function* *macro* *declare*

#### Description
This function returns a new environment containing the information present in env augmented with the information provided by the keyword arguments. It is intended to be used by program analyzers that perform a code walk.

The arguments are supplied as follows.

:variable
The argument is a list of symbols that will be visible as bound variables in the new environment. Whether each binding is to be interpreted as special or lexical depends on special declarations recorded in the environment or provided in the :declare argument.
:symbol-macro
The argument is a list of symbol macro definitions, each of the form (name definition); that is, the argument is in the same format as the cadr of a symbol-macrolet special form. The new environment will have local symbol-macro bindings of each symbol to the corresponding expansion, so that macroexpand will be able to expand them properly. A type declaration in the :declare argument that refers to a name in this list implicitly modifies the definition associated with the name. The effect is to wrap a the form mentioning the type around the definition.
:function
The argument is a list of function-names that will be visible as local function bindings in the new environment.
:macro
The argument is a list of local macro definitions, each of the form (name definition). Note that the argument is not in the same format as the cadr of a macrolet special form. Each definition must be a function of two arguments (a form and an environment). The new environment will have local macro bindings of each name to the corresponding expander function, which will be returned by macro-function and used by macroexpand.
:declare
The argument is a list of declaration specifiers. Information about these declarations can be retrieved from the resulting environment using variable-information, function-information, and declaration-information.
The consequences of subsequently destructively modifying the list structure of any of the arguments to this function are undefined.
An error is signaled if any of the symbols naming a symbol macro in the :symbol-macro argument is also included in the :variable argument. An error is signaled if any symbol naming a symbol macro in the :symbol-macro argument is also included in a special declaration specifier in the :declare argument. An error is signaled if any symbol naming a macro in the :macro argument is also included in the :function argument. The condition type of each of these errors is program-error.

The extent of the returned environment is the same as the extent of the argument environment env. The result might share structure with env but env is not modified.

While an environment argument received by an *evalhook* function is permitted to be used as the environment argument to augment-environment, the consequences are undefined if an attempt is made to use the result of augment-environment as the environment argument for evalhook. The environment returned by augment-environment can be used only for syntactic analysis, that is, as an argument to the functions defined in this section and functions such as macroexpand.

### define-declaration
*Macro* **define-declaration** *decl-name* *lambda-list* **&body** *body*

#### Description
This macro defines a handler for the named declaration. It is the mechanism by which augment-environment is extended to support additional declaration specifiers. The function defined by this macro will be called with two arguments, a declaration specifier whose car is decl-name and the env argument to augment-environment. This function must return two values. The first value must be one of the following keywords:

:variable
The declaration applies to variable bindings.
:function
The declaration applies to function bindings.
:declare
The declaration does not apply to bindings.
If the first value is :variable or :function then the second value must be a list, the elements of which are lists of the form (binding-name key value). If the corresponding information function (either variable-information or function-information) is applied to the binding-name and the augmented environment, the a-list returned by the information function as its third value will contain the value under the specified key.
If the first value is :declare, the second value must be a cons of the form (key . value). The function declaration-information will return value when applied to the key and the augmented environment.

define-declaration causes decl-name to be proclaimed to be a declaration; it is as if its expansion included a call (proclaim '(declaration decl-name)). As is the case with standard declaration specifiers, the evaluator and compiler are permitted, but not required, to add information about declaration specifiers defined with define-declaration to the macro expansion and *evalhook* environments.

The consequences are undefined if decl-name is a symbol that can appear as the car of any standard declaration specifier.

The consequences are also undefined if the return value from a declaration handler defined with define-declaration includes a key name that is used by the corresponding accessor to return information about any standard declaration specifier. (For example, if the first return value from the handler is :variable, the second return value may not use the symbols dynamic-extent, ignore, or type as key names.)

### parse-macro
*Function* **parse-macro** *lambda-list* *body* **&optional** *env*

#### Description
This function is used to process a macro definition in the same way as defmacro and macrolet. It returns a lambda-expression that accepts two arguments, a form and an environment. The name, lambda-list, and body arguments correspond to the parts of a defmacro or macrolet definition.

The lambda-list argument may include &environment and &whole and may include destructuring. The name argument is used to enclose the body in an implicit block and might also be used for implementation-dependent purposes (such as including the name of the macro in error messages if the form does not match the lambda-list).

### enclose
*Function* **enclose** *lambda-expression* **&optional** *env*

#### Description
This function returns an object of type function that is equivalent to what would be obtained by evaluating `(function ,lambda-expression) in a syntactic environment env. The lambda-expression is permitted to reference only the parts of the environment argument env that are relevant only to syntactic processing, specifically declarations and the definitions of macros and symbol macros. The consequences are undefined if the lambda-expression contains any references to variable or function bindings that are lexically visible in env, any go to a tag that is lexically visible in env, or any return-from mentioning a block name that is lexically visible in env.
