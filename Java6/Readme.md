TISBL for Java 6
================

This is a TISBL interpreter written in Java 1.6.  Java, as a language, needs no introduction.

System requirements
-------------------

The Java JDK 1.6.0.

To get this to run
------------------

$ javac *.java
$ java TISBL <filename>

Things to note
--------------

One: Java is exclusively OO.  The downside of this can be seen in Stdlib.java and comparing the amount of boilerplate involved with the equivalent in Stdlib.pas for the Object Pascal version.  The Java stdlib is actually slightly more liberal in what it expects than the Object Pascal version, because I decided I couldn't face the verbosity.

Two: The actual object semantics are pretty close between the dialects of Object Pascal I've used and Java.  So the actual structure of the software is quite close to that of the Object Pascal version.  There aren't any properties in Java, though.  The garbage collection in Java /does/ permit objects to be thrown around as data far more easily; while I needed to determine when to free stack items in the Object Pascal code, this isn't the case here.

Three: Having built-in Deque (stack-like thing) and HashMap data types is a massive time-saver.  Free Pascal is working on that kind of thing with FPC-STL, but it doesn't seem to be finished yet, and it's not a standard part of the language in quite the same way.  Generic types (being able to say List<String> etc) have made life a lot easier since they turned up in Java.  Modern Object Pascal dialects have a generics feature likewise, but in doing so, they use the same syntax as Java, which is... suboptimal.

This is probably not the place for a rant about the design of generics syntaces, though... ;-)

Four: Having no type aliases is a pain in the backside.  In Pascal I could have typed something along the line of

```delphi
type
	Stack = Deque<StackItem>;
```
	
and then been able to use 'Stack' instead of having to type Deque<StackItem> everywhere.

Five:  I was somewhat hoping to be able to do the \+, \-, \* and \div verbs as two-place methods on stack items, so I'd have been able to do

```java
stringItem.plus(stringItem2);
```

rather than having everything special-cased inside the implementation of \+.  However, since the definition of \+ depends on the run-time type of /both/ sides and Java only has single dispatch, I ended up with about as many instanceof checks as before, just smeared about all over the place.  So it made more sense to have it all in one place.