TISBL for JavaScript
====================

This is a TISBL interpreter written in JavaScript suitable for a web browser.

System requirements
-------------------

I've tested this on tolerably modern versions of Firefox and Safari.  Beyond this, you're on your own.  That said, there's nothing exotic in here.

To get this to run
------------------

Open index.html.  Type or paste your TISBL program in the top pane.  Click 'run'.  Note that state is preserved across 'run' presses!  So if you need a tabula rasa, press 'Reset VM'.

Things to note
--------------

One: I will admit right at the start here that I strongly dislike JavaScript as a language.  It is full of edge cases.  Example of this: the bizarre tests of integrality in Stacks.js.  Another oddity: String.replace can /only/ be used to replace all instances of a substring if that substring is encoded with a regex with a /g flag!  There's a Mozilla non-standard extension for it, but that's non-standard.  As soon as one gets into the intricacies of auto-stringification and the bizarre, non-commutative semantics of '+', then excreta really intersects windmill.

Two: JavaScript is not a class-based language.  It's prototype-based instead.  This results in a lot of people who can't get their heads out of class-based Object-orientation badly reimplementing the 'class' notion, which isn't helped by JS's odd use of "new".  I was involved in a large JS project recently --- a mobile application --- which integrated three different libraries, each of which had a completely incompatible and semantically different concept of 'class'.  This interpreter uses a variation on Crockford's utility function (see the top of Tisbl.js) to inherit from prototypes.

It doesn't use it much; the heirarchy of inheritance is quite low. But what this means is that I can create a context based off the current one very easily, or an object that's based on the same prototype; the line var newCtx = child_of(Object.getPrototypeOf(this)) is rather nice.  The place it /is/ used is when the verbtable for any given interpreter instance is declared as a child_of(stdlib); it inherits all the properties of stdlib and can override them.  This means that, for example, it would be trivial to implement scope by making each new context's verbtable inherit from the previous one rather than taking the whole verbtable over from its parent.  This would be borderline impossible in a class-based system.

Three: The stdlib is written very concisely.  It's nearly as concise as the perl.

Four: The RegExp class is really useful.  The syntax from perl just turning up in the middle of JS is... less convincing.

Five: One interesting thing here has been the point at which the chaotic, 'no typing', objects-having-no-contracts-until-runtime thing has /stopped/ working.  In this project, it was about 450 lines until not being able to statically check it got unbearable.

Six: I/O is abstracted in this.  This was so that I could write a node.js example: but, frankly, I found an example to do console text input in node.js and thought that life was too short.