# TISBL debug heuristics

## An introduction

This is a brief description of how debug information is handled in the C99
implementation.  Hopefully it will help enable the creation of larger and more
complex TISBL programs.


## A vague description

This method requires the addition of two kinds of state.

Each value on every stack additionally contains a location.  A location is
a file name and the line number within that file.

Each context in the context stack additionally contains the value it is
currently executing, including its location.  These values together form the
current call stack.

During tokenization of a script or REPL input each value is assigned the
location where it was found.  The file name in REPL mode is set to `(stdin)`.
Tokenization is the only time when new locations are generated.

Values that are copied or moved without modification retain their location and
are not affected by the location of the current verb.

Values that are produced by a literal inherit the location of that literal.

Values that are newly produced by a verb, whether from an external or internal
source or by combining arguments, inherit the location of the string value that
called the verb.


## Locations for outputs per standard verb

Verb       | Output location
---------- | ---------------
\\trace=1  | N/A
\\trace=0  | N/A
\\exec     | copy from input
\\verb     | copy from input
\\if       | copy from input
\\while    | copy from input
\\eq       | inherit from call
\\not      | inherit from call
\\swap     | copy from input
\\dup      | copy from input
\\rm       | N/A
\\mv       | copy from input
\\multipop | copy from input
\\add      | inherit from call
\\sub      | inherit from call
\\mul      | inherit from call
\\div      | inherit from call
\\n        | inherit from call
\\\_       | inherit from call
\\word?    | inherit from call
\\number?  | inherit from call
\\integer? | inherit from call
\\float?   | inherit from call
\\die      | N/A
\\out      | N/A
\\in       | inherit from call


## Some obvious consequences

Because a program cannot generate new locations after tokenization, there are
many possibilities for optimization of storage.

While this method produces useful information for simple code, any non-trivial
code generation will defeat it.

If you are doing non-trivial code generation in TISBL then it... Look, are you
okay?  Do you need a hug?

