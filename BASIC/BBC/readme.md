TISBL for BBC BASIC V
=====================

This is a TISBL interpreter written in BBC BASIC V.  This isn't the place to describe the full history of BBC BASIC, but it was a kind of collaboration-cum-compromise between the BBC and Acorn.  BASIC V is the version of BASIC built into RISC OS. 

BASIC V sits somewhere between traditional BASICs, all line numbers and spaghetti and modern dialects of BASIC that seem to think they're ALGOL derivatives.  The level of abstraction of execution flow is pretty good: note that there are no GOTOs or anything nasty in that department.  Where it lacks is in data abstraction: there are no records/structs or hashes (except in R. T. Russell's BBC BASIC for Windows), and if you want to do anything complicated you either need to use lots of arrays (as this interpreter does) or allocate yourself a big block of memory and do everything by address.  I'm not intending to implement TISBL in any older BASICs than this, for my own sanity.

BASIC V is actually a fairly good language for application programming, especially on RISC OS.  The problem I've put it to here perhaps does not show off its good points.  However, I still have a fondness for it, and I had forgotten how much fun it was to write in this language.

![](img/screenshot.png?raw=true)

System requirements
-------------------

This has been tested under Brandy Basic V (http://jaguar.orpheusweb.co.uk/branpage.html) under UNIX and under BASIC V 1.54 under RISC OS 5.  It will almost certainly run under Brandy on other OSes and under other versions of BASIC V: this is just what I happened to have handy.

It will probably also run under BBC BASIC for Windows (http://www.rtrussell.co.uk/) if you turn all the LIBRARY statements into INSTALL statements, but I don't have a copy of it, so I can't test.

To get this to run: brandy
--------------------------

```
$ cd <dir-containing-files>
$ brandy -quit tisbl
```

It will ask for a filename; if you want a REPL, hit 'enter'; to run a file enter its name.

To get this to run: BASIC V
---------------------------

Before you run this under RISC OS, you'll need to tokenise the files.  I did this using Steve Fryatt's 'tokenizer' utility, available at http://www.stevefryatt.org.uk/software/build/ but you could just use BASIC's LOADTEXT or StrongEd or Zap or something.

```
*dir <dir-containing-files>
*basic -quit tisbl
```

It will ask for a filename; if you want a REPL, hit 'enter'; to run a file enter its name.  

Things to note
--------------

One: There are arrays EVERYWHERE.  I think the one feature I missed most here is structs/records.  Each kind of stack (primary, secondary, exec) are represented by a two dimensional array declared in 'tisbl'.  There is then a set of primitives implemented in 'stacks' on top of these, each of which takes what kind of stack and what context.  On top of *that* there is an implementation of stack references in 'sr' that then lets you just throw around references to stacks.  Compared to how easy this was in THINK Pascal (just define a stack and throw around a pointer to it) I feel slightly hard-done-by having had to etch my abstractions out of nothing.

Two: For a tolerably old-school BASIC, this language is beautifully well-endowed with control constructs.  We have proper procedures and functions with parameters; we have multi-line IF/ELSE/ENDIFs; we have CASE statements and so forth.  What we do not have is an ELSE IF.

Three: To remind you that this is a proper BASIC, though, you do have the sigils on the end of variables, which I kept forgetting: $ for string, % for integer, no suffix for float.  You can also use & for byte.  Also, all the keywords have to be in all caps.

Four: Although this isn't used in this interpreter, you can in fact ask BASIC to just give you a chunk of memory and then do what you want with it.  This is a big improvement over PEEK and POKE: you can effectively do proper pointer arithmetic and with that you can emulate structures.  However, once you've been allocated a block of memory, there's no real way of freeing it again, so you can't really use it for data abstraction.

Five: Procedures and other blocks look almost ALGOL-y but aren't quite.  Note that the way of exiting a procedure early is to use ENDPROC, which is the same as you would use to end the procedure block.  Compare this to the THINK Pascal interpreter where the blocks are bracketed with begin/end and to exit a procedure early you have to use exit(whatever).

Things to poke
--------------

* The sizes of stack and so forth are at the top of 'tisbl'.  You will need to allocate more memory to the interpreter if you change these too much.
* There's no command line parsing, because I didn't have the energy to do it in a cross-platform fashion (on RISC OS you'd need to use OS_GetEnv; brandy gives you ARGC/ARGV$; BBC BASIC for Windows uses a variable called @cmd$).  Patches welcome!
