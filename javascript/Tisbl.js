/* This file bootstraps TISBL.  It contains first two utility functions,
   one to load other files, and one to inherit from a prototype, then it
   loads the other files involved. */

function input(filename) {
	var req = new XMLHttpRequest();
	req.overrideMimeType("text/javascript");
	req.open("GET", filename, false);
	req.send();
	if(req.responseText) {
		eval(req.responseText); // this is EVIL and shouldn't happen in prod.
	} else {
		throw(req.statusText);
	}
}


/* This is from http://javascript.crockford.com/prototypal.html.
   It implements 'proper' prototype inheritance in JS. 
   The second parameter is optional and is an initial set of keys/values:
   essentially the equivalent to initialising with a {...}.
   If the object has a __init__ function, then it'll call that.  This is
   so that objects can tell their children not to fiddle with bits of them! */
   
function child_of(o, literal) {
	function F() {}
	F.prototype = o;
	var obj = new F();
	
	if (obj._init_) {
		obj._init_();
	}
	
	if (arguments.length > 1) {
		for (var key in literal) {
			obj[key] = literal[key];
		}
	}
		
	return obj;
}


function escape_regex_group_chars(str) {
	var buf = str.replace(/\[/g, "\\[");
	buf = buf.replace(/\]/g, "\\]");
	buf = buf.replace(/-/g, "\\-");
	buf = buf.replace(/^\^/g, "\\^");
	return buf;
}

function escape_regex_chars(string){
  return string.replace(/([.*+?^=!:${}()|\[\]\/\\])/g, "\\$1");
}

archetypes = {}; // a namespace to bung objects to be prototype-ed from.


/* Now load the files */

input('Stacks.js');
input('Ctx.js');
input('Stdlib.js');
input('Interpreter.js');