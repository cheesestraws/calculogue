TISBL for Ruby 1.9
==================

This is a TISBL intepreter written in Ruby 1.9.

System requirements
-------------------

Ruby 1.9.  It has only been tested with MRI.

To get this to run
------------------

If a file is specified, the file is executed.

```
$ ruby tisbl.rb hello.t
Hello World!
$
```

If no file is specified, a REPL is started.

```
$ ruby tisbl.rb
> 'Hello \_ 'World! \+ \out
Hello World!
>
```

Things to note
--------------

Ruby provides many of even the more obscure operations natively, and nearly all
of the remaining ones can (and have been) implemented with somewhat readable
one-liners.

Plain arrays are used as stacks, as they already provide push and pop
operations.  The stacks are stored in a map indexed by symbols:

 * `:p` The primary stack
 * `:s` The secondary stack
 * `:c` The execution stack
 * `:i` The reference to the input stack
 * `:o` The reference to the output stack
 * `:e` The reference to the execution stack of the parent context

Loaded modules are executed with their own root context but sharing the verb
table with the module that called `\load`.

