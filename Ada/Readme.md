TISBL for Ada 2005
==================

This is a TISBL intepreter written in Ada 2005.

I've heard Ada described as a bondage-and-discipline language.  I'm not sure that's true, myself - largely because I don't think Ada would have any truck with anything as unpredictable as a libido.

It is, however, a language that is designed for large projects, which prioritises ease of reading over ease of writing and makes you express *exactly* what you mean.  Which comes as a nasty surprise after Perl, it must be said.

The Ada Rationales are, in my opinion, very good technical documentation.


System requirements
-------------------

In theory, any sane Ada 2005 compiler should do.  In practice, I was using GNAT 4.6, specifically the version available from apt in Debian 7.0.

GNAT 4.4.3 (as available in apt in Ubuntu 10.04 LTS) fails to compile this, kindly informing the user of a compiler bug.  Therefore, your experience may vary.


To get this to run
------------------

```
$ gnatmake -Ptisbl2.gpr tisbl.adb
$ ./tisbl
Usage:
  ./tisbl -t             -- run tests
  ./tisbl -r             -- run a repl
  ./tisbl -e <toks>      -- run <toks>
  ./tisbl -f <filename>  -- run a file
$
```


Things to note
--------------

One: This is fairly bad Ada, really.  I do things like making Access types (references) publically visible in packages and passing unchecked access values around, and heap-allocating things.  The second is only moderately naughty, so long as it is kept to a moderate degree, but the former is pretty naughty because:

* It's possible for something that receives one of these access values to unilaterally free it.  If it's on the heap and something else is depending on it this is bad enough; if the value is on the stack somewhere, who knows what'll happen.
* It's in theory possible for something like the standard library to keep around a Stack\_Access value after the actual interpreter context has gone away.  At best, this ends up causing some kind of segfault; at worst, it writes something somewhere critical and causes undefined behaviour.
	
Now, in this case, this isn't the end of the world, because I'm the only person building this, and I know very well that I shouldn't do either of those things.  Were this a bigger project, there are various ways around both issues: I could have built a Proxy\_Stack type that wouldn't let itself be assigned to anything (so couldn't outlive its context) and which would have proxy implementations of the methods which would pass it on to a hidden reference (so couldn't be freed).  There's also an Ada Gem that outlines a way of doing this using the new Ada 2012 Implicit\_Dereference feature (http://www.adacore.com/adaanswers/gems/gem-123-implicit-dereferencing-in-ada-2012/). For this project, however, the way I've done it will do.

Two: Ada is extremely verbose compared to any other language I've used.  By lines of code there's more Ada than any other language.  This is particularly striking when comparing the Ada version with the procedural Pascal version: the Ada version manages to be quite noticeably longer, despite using the standard container library while the procedural pascal one had to roll its own.

Three: Ada's take on object orientation is interesting.  It's a fairly conservative extension of the procedural language, and it uses the same tools for encapsulation as the procedural language.  Again, compare this to Object Pascal - in point six of the Readme for the Object Pascal version, I noticed that there are actually two means of encapsulation going on.  Units can have a private part; but the private part of a class is a completely separate mechanism from the private part of a unit.  In Ada this is not the case; the private part of a tagged record type (analogous to a class) is embedded within the private part of the compilation unit in question.

Four: In Ada, subclassing is not subtyping.  This isn't actually used in the interpreter, but it's sufficiently unusual that it's worth mentioning here.  Consider the following class hierarchy, where arrows point from superclass to subclass:

                              +-------+
                              |       |
                              |   A   |
                              |       |
                              +--+-+--+
                                 | |
                           +-----+ +-----+
                           |             |
                       +---v---+     +---v---+
                       |       |     |       |
                       |   B   |     |   C   |
                       |       |     |       |
                       +-------+     +-------+
                       
In a language like Java or Object Pascal, where subclassing is subtyping, a parameter of type A can accept an object of type B or an object of type C.  In Ada, this is not the case.  A parameter of type A can /only/ accept an object of type A.  There is a separate type, called A'Class, which corresponds to the "class" of A - that is to say, A and all its subclasses (called derived types in Ada parlance).

Five: Various syntactic niceties make life easier.  Nested procedures and functions are nice (though to be fair I could have used those in Pascal too).  Operator overloading is never something I've been terribly convinced by, but it sort of makes sense to use it here.  There are separate short-circuiting and non-short-circuiting boolean operators (and vs. and then; or vs. or else).

Six: Features of the language you can shoot yourself in the foot with have long, unwieldy names that tell you that they're dangerous.  For example, Ada.Unchecked\_Deallocation (an operation that terser languages call 'free') and 'Unchecked\_Access (an operation that corresponds, roughly, to the address-of operator).

Seven: Limited types are those which can't be copied.  This is just a note to make the code slightly more comprehensible.