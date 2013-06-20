TISBL for Perl 5
================

This is a TISBL interpreter written in Perl 5.  Perl is a language which excels at text processing and systems integration work, but it's perfectly sane to attach other problems with it at all.

This interpreter should not require installation of any extra perl modules from CPAN, and does not use any extensions to the basic object system (more on this below).

System requirements
-------------------

Perl - I've tested this on 5.10.0 and above.

To get this to run
------------------

`$ perl tisbl.pl -r` for a repl-type thing.

`$ perl tisbl.pl -f <filename>` to run a file.

This one has a foreign function interface!
------------------------------------------

Well, sort of.  It lets you write TISBL modules in Perl.

I suggest taking apart modules/Foo.pm; note that all verb definitions have to end with semicolons.  If you get strange errors about "too many parameters for TISBLModule::is", then you've left out the semicolon on the bottom of one of your verb definitions.

Once the module is written, you can load and access this from TISBL:

```
'Foo \present? \not '\die #1 \if
'Foo \load
\foo
```

Things to note
--------------

One: This is written using the "raw" Perl object-orientation features.  See the perldoc perlobj for the details on this.  Basically, the raw features are quite simple and - if you're only used to Java and so forth - eccentric.  On top of these raw features various other facilities are built as perl modules (such as Moose).  This doesn't use any of those.

The idea is that any data structure can be turned into an object by means of associating a reference to it with ("blessing it into") a class.  A class is just a package/namespace.  When a method is called on the blessed reference, the appropriate subroutine for that method is called in the package that the reference is blessed into, and the object is passed as its first parameter.

This opens up a bewildering variety of choices for how to actually build objects.  This interpreter uses a number: the stack items (in SI.pm) are just blessed scalar references, since they only need to hold one value and a class name.  Stacks are blessed list references, because lists provide pretty much all the functionality needed anyway.  Contexts are blessed hashes (a hash is Perl's name for a dictionary or an associative array), because they need to be a set of name-value pairs.

A more radically strange (compared to other, more boring object-oriented languages) is in the "inside-out object" strategy adopted by InterpState.pm; in this approach, the object is a blessed scalar which is just used as an identifier into a set of arrays or hashes which are lexically scoped to the class.  This takes a bit of a step back to get your head around (or at least it did for me the first time I met it), but it does cut down on the punctuation salad needed to access fields of a blessed hash reference, and it does keep encapsulation nice and bombproof.

Two: Having first-order regular expressions is <i>really</i> handy.  Compare Parser.pm with Tokeniser.pas/Tokeniser.java, and Token.pm with Token.java.  They can be opaque and difficult to read, but they don't have to be.

Three: Perl seems to be prone to producing tiny little domain-specific languages.  This interpreter uses one for writing the standard library (and other TISBL modules; it is implemented in TISBLModule.pm and used in Stdlib.pm).  The unit testing packages are within spitting distance of being domain-specific languages, too: to see the ugly results when two domain-specific languages collide, have a look in tests/TISBLModule.pm.

Four: Testing is very easy; this interpreter is almost certainly the one with the highest test coverage.  On the other hand, there is no real concept of 'type' in perl, which is partly why a lot of those tests are necessary.  Swings, meet roundabouts.

Five: Other features of Perl that may look startling to devotees of other languages which can be found inside this interpreter are: use of eval/die for exception handling, accessing parameters to a subroutine just as another array (called @_), quotelike operators (qw() and so forth) and the <> operator to read lines from a file.