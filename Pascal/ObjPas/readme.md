TISBL for Object Pascal
=======================

This is a TISBL interpreter written in Object-Oriented Pascal for anything that Free Pascal targets.  The dialect in use is OBJFPC, which is similar to but not identical to that of Delphi, possibly the most advanced Object Pascal implementation.  For information on how the OBJFPC dialect differs from Delphi, see the Free Pascal manual: http://www.freepascal.org/docs-html/prog/progap4.html#progse62.html.

It uses no platform-specific mechanisms at all; it does use the Free Pascal specific APIs for file streams and so forth.


System requirements
-------------------

Something that can run FPC.  I built it uses FPC2.6.2.  It will probably run under earlier versions too; it will almost certainly run under later versions.


To get this to run
------------------

Install FPC - www.freepascal.org is your friend here.  It may well be in whatever package manager you use.  Then you should just be able to do

$ fpc tisbl
$ ./tisbl
Usage:
  ./tisbl -r              Start a REPL
  ./tisbl -e [toks]       Execute toks
  ./tisbl -f [filename]   Run filename
$ _


Things to note
--------------

One: The problems with procedure pointers have gone away, and now the compiler can deal with procedure pointer types half-way sensibly.  This is especially visible in verbs.pas - the TNativeVerbProc type.

Two: This language can mix OO and non-OO programming techniques in one piece of software, which can be quite handy.  This may offend purists: but look at stdlib.pas, and think about the extra typing involved in wrapping each of these in a class.

Three: Coming back to this language after a period of years, it is interesting to note that the private parts of classes are in the interface section of units - which means that the dependencies in use in the private parts of classes must also be in the interface section.  A point for reflection: how would I redesign this syntax so that the public/private parts of classes line up more neatly with the public/private parts of units without the whole thing descending into chaos?

Four: All objects are on the heap; an object is essentially a pointer.  While it is possible to put some objects on the stack in OBJFPC, things like constructors (and thus, virtual methods, since the constructor allocates the virtual method table) don't work as one might expect.  This makes things like data objects a bit awkward; you have to remember to free /everything/ and adopt a consistent protocol for freeing things.  This makes the standard library in this actually more complicated than it is in the procedural one.

Five: Properties are, alas, broken in Object Pascal.  You can't, for example, declare a property as abstract as a whole, and then work out how you're going to implement it in a subclass.  You have to specify the property as, say, having a getter and setter and then mark /those/ as abstract: you can't leave it up to subclasses as to whether to implement them as methods or variable accesses.

Six: Being able to initialise variables at declaration-time and constructors are both language features that save lots of typing.

Seven: Pascal is case-insensitive; and I seem to fall into a very complicated pattern of when to use capitals and when not.  I don't have an adequate list of the heuristics for this, and if I were working on a 'real' codebase then I'd need to be more careful about code style, I suspect.

Eight: The circular reference problem is still there.  In this version, instead of using procedure pointers, it uses an abstract state class TAbstractCtxes which maintains the interface that the standard library ought to see.

Things to poke
--------------

The 'testing' constant in tisbl.pas turns on and off unit-type tests.