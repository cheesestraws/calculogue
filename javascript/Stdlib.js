stdlib = {
	'trace=0': function(ctx, input, output) {
		ctx.state.trace = false;
	},
	
	'trace=1': function(ctx, input, output) {
		ctx.state.trace = true;
	},
	
	exec: function(ctx, input, output) {
		var newctx = ctx.spawn(input, output);
		var count = input.pop();
		if (!count.isInteger) { throw("\\exec needs an integer parameter!"); }
		count = count.intValue();
		for (var i = 0; i < count; i++) { 
			var item = input.pop();
			newctx.exec.push(item);
		}
		newctx.go();
	},
	
	verb: function(ctx, input, output) {
		// teeheehee closures
		var stk = child_of(archetypes.Stack);
		var name = input.pop();
		if (!name.isString) {
			throw("\\verb needs a string name!"); 
		}
		name = name.stringValue();
		var count = input.pop();
		if (!count.isInteger) { throw("\\verb needs an integer block size!"); }
		count = count.intValue();
		
		for (var i = 0; i < count; i++) {
			var item = input.pop();
			stk.push(item);
		}
		
		// now, construct a function
		var fn = function(c, i, o) {
			var newctx = c.spawn(i, o);
			newctx.exec.cloneFromStack(stk);
			newctx.go();
		}
		
		ctx.verbtable[name] = fn;
	},
	
	'if': function(ctx, input, output) {
		var newctx = ctx.spawn(input, output);
		var count = input.pop();
		if (!count.isInteger) { throw("\\if needs an integer size of code block!"); }
		count = count.intValue();
		for (var i = 0; i < count; i++) { 
			var item = input.pop();
			newctx.exec.push(item);
		}
		
		var condition = input.pop();
		if (!condition.isANumericItem) { throw("\\if needs a numeric condition!"); }
		condition = condition.floatValue();
		if (condition != 0) {
			newctx.go();
		}
	},
	
	'while': function(ctx, input, output) {
		var newstk = child_of(archetypes.Stack);
		var count = input.pop();
		if (!count.isInteger) { throw("\\while needs an integer size of code block!"); }
		count = count.intValue();
		for (var i = 0; i < count; i++) { 
			var item = input.pop();
			newstk.push(item);
		}
		
		var endloop = false;
		do {
			var condition = input.pop();
			if ((condition.isANumericItem) && (condition.floatValue() == 0.0)) {
				endloop = true;
			} else {
				var newctx = ctx.spawn(input, output);
				newctx.exec.cloneFromStack(newstk);
				newctx.go();
			}
		} while (!endloop);
	},
	
	not: function(ctx, input, output) {
		var value = input.pop();
		if ((value.isANumericItem) && (value.floatValue() == 0)) {
			output.push(SI_Int(1));
		} else {
			output.push(SI_Int(0));
		}
	},
	
	swap: function(ctx, input, output) {
		var a = input.pop();
		var b = input.pop();
		output.push(a);
		output.push(b);
	},
	
	dup: function(ctx, input, output) {
		var a = input.peek();
		output.push(a);
	},
	
	rm: function(ctx, input, output) {
		input.pop();
	},
	
	mv: function(ctx, input, output) {
		var a = input.pop();
		output.push(a);
	},
	
	multipop: function(ctx, input, output) {
		var count = input.pop();
		if (!count.isInteger) { throw("\\multipop needs an integer number of pops!"); }
		count = count.intValue();
		for (var i = 0; i < count; i++) {
			var a = input.pop();
			output.push(a);
		}
	},
	
	// oh dear, here we go
	// it's The Unnecessarily Complicated Arithmetic Library [tm].
	'+': function(ctx, input, output) {
		var sec = input.pop();
		var fst = input.pop();
		if (fst.isString || sec.isString) {
			output.push(SI_String(fst.stringValue() + sec.stringValue()));	
		} else if (fst.isFloat || sec.isFloat) {
			output.push(SI_Float(fst.floatValue() + sec.floatValue()));
		} else {
			output.push(SI_Int(fst.intValue() + sec.intValue()));
		}
	},
	
	'-': function(ctx,input,output) {
		// helper functions
		// we want to CACHE these so we don't create them every time
		// I don't trust the garbage collector
		if (typeof(_tisbl_fncache) == "undefined") {
			_tisbl_fncache = {};
		}
		
		if (!_tisbl_fncache.sub_si) {
			_tisbl_fncache.sub_si = function(str, integer) {
				return str.substring(0, str.length - integer);
			};
			
			_tisbl_fncache.sub_ss = function(haystack, needle) {
				// generate a regex
				var str = escape_regex_group_chars(needle);
				
				var re = new RegExp('[' + str + ']', 'g');
				haystack = haystack.replace(re, '');
				return haystack;
			};
		}
		
		var sec = input.pop();
		var fst = input.pop();
		
		if (fst.isInteger && sec.isInteger) {
			output.push(SI_Int(fst.intValue() - sec.intValue()));	
		} else if (fst.isInteger && sec.isString) {
			output.push(SI_String(_tisbl_fncache.sub_si(sec.stringValue(), fst.intValue())));
		} else if (fst.isString && sec.isInteger) {
			output.push(SI_String(_tisbl_fncache.sub_si(fst.stringValue(), sec.intValue())));
		} else if (fst.isFloat && sec.isString) {
			output.push(SI_String(_tisbl_fncache.sub_si(sec.stringValue(), fst.intValue())));
		} else if (fst.isString && sec.isFloat) {
			output.push(SI_String(_tisbl_fncache.sub_si(fst.stringValue(), sec.intValue())));
		} else if (fst.isString && sec.isString) {
			output.push(SI_String(_tisbl_fncache.sub_ss(fst.stringValue(), sec.stringValue())));
		} else if (fst.isFloat || sec.isFloat) {
			output.push(SI_Float(fst.floatValue() - sec.floatValue()));	
		}
	},
	
	'*': function(ctx,input,output) {
		// helper functions
		// we want to CACHE these so we don't create them every time
		// I don't trust the garbage collector
		if (typeof(_tisbl_fncache) == "undefined") {
			_tisbl_fncache = {};
		}
		
		if (!_tisbl_fncache.mul_sf) {
			_tisbl_fncache.mul_sf = function(str, flt) {
				var count = flt;
				var buffer = "";
				while (count >= 1) {
					buffer = buffer + str;
					count--;
				}
				
				if (count > 1/str.length) {
					var charcount = Math.round(count * str.length);
					buffer = buffer + str.substring(0, charcount);
				}
				return buffer;
			};
			
			_tisbl_fncache.mul_ss = function(haystack, needle) {
				// generate a regex
				var firstchar = escape_regex_group_chars(needle[0]);
				
				var re = new RegExp("[" + firstchar + "]", 'g');
				haystack = haystack.replace(re, needle);
				return haystack;
			};
		}
		
		var sec = input.pop();
		var fst = input.pop();
		
		if (fst.isInteger && sec.isInteger) {
			output.push(SI_Int(fst.intValue() * sec.intValue()));	
		} else if (fst.isInteger && sec.isString) {
			output.push(SI_String(_tisbl_fncache.mul_sf(sec.stringValue(), fst.floatValue())));
		} else if (fst.isString && sec.isInteger) {
			output.push(SI_String(_tisbl_fncache.mul_sf(fst.stringValue(), sec.floatValue())));
		} else if (fst.isFloat && sec.isString) {
			output.push(SI_String(_tisbl_fncache.mul_sf(sec.stringValue(), fst.floatValue())));
		} else if (fst.isString && sec.isFloat) {
			output.push(SI_String(_tisbl_fncache.mul_sf(fst.stringValue(), sec.floatValue())));
		} else if (fst.isString && sec.isString) {
			output.push(SI_String(_tisbl_fncache.mul_ss(fst.stringValue(), sec.stringValue())));
		} else if (fst.isFloat || sec.isFloat) {
			output.push(SI_Float(fst.floatValue() * sec.floatValue()));	
		}
	},
	
	'div': function(ctx,input,output) {
		// helper functions
		// we want to CACHE these so we don't create them every time
		// I don't trust the garbage collector
		if (typeof(_tisbl_fncache) == "undefined") {
			_tisbl_fncache = {};
		}
		
		if (!_tisbl_fncache.div_sf) {
			_tisbl_fncache.div_sf = function(str, flt) {
				var chars = Math.round(str.length / flt);
				return str.substring(0, chars);
			};
			
			_tisbl_fncache.div_ss = function(haystack, needle) {
				// generate a regex
				// add slashes to needle.  []^ have special meanings here!
				// todo - GENERALISE
				// And FIX IT.
				var str = escape_regex_chars(needle);
				var firstchar = needle[0];
				
				var re = new RegExp(str, 'g');
				haystack = haystack.replace(re, firstchar);
				return haystack;
			};
		}
		
		var sec = input.pop();
		var fst = input.pop();
		
		if (fst.isInteger && sec.isInteger) {
			output.push(SI_Float(fst.intValue() / sec.intValue()));
		} else if (fst.isInteger && sec.isString) {
			output.push(SI_String(_tisbl_fncache.div_sf(sec.stringValue(), fst.floatValue())));
		} else if (fst.isString && sec.isInteger) {
			output.push(SI_String(_tisbl_fncache.div_sf(fst.stringValue(), sec.floatValue())));
		} else if (fst.isFloat && sec.isString) {
			output.push(SI_String(_tisbl_fncache.div_sf(sec.stringValue(), fst.floatValue())));
		} else if (fst.isString && sec.isFloat) {
			output.push(SI_String(_tisbl_fncache.div_sf(fst.stringValue(), sec.floatValue())));
		} else if (fst.isString && sec.isString) {
			output.push(SI_String(_tisbl_fncache.div_ss(fst.stringValue(), sec.stringValue())));
		} else if (fst.isFloat || sec.isFloat) {
			output.push(SI_Float(fst.floatValue() / sec.floatValue()));	
		}
	},
	
	n: function(ctx, input, output) {
		var s = input.pop();
		s = s.stringValue() + "\n";
		output.push(SI_String(s));
	},
	
	_: function(ctx, input, output) {
		var s = input.pop();
		s = s.stringValue() + " ";
		output.push(SI_String(s));
	},
	
	'string?': function(ctx, input, output) {
		var e = input.pop();
		if (e.isString) {
			output.push(SI_Int(1));
		} else {
			output.push(SI_Int(0));
		}
	},
	
	'number?': function(ctx, input, output) {
		var e = input.pop();
		if (e.isANumericItem) {
			output.push(SI_Int(1));
		} else {
			output.push(SI_Int(0));
		}
	},
	
	'integer?': function(ctx, input, output) {
		var e = input.pop();
		if (e.isInteger) {
			output.push(SI_Int(1));
		} else {
			output.push(SI_Int(0));
		}
	},
	
	'float?': function(ctx, input, output) {
		var e = input.pop();
		if (e.isFloat) {
			output.push(SI_Int(1));
		} else {
			output.push(SI_Int(0));
		}
	},
	
	'eq?': function(ctx, input, output) {
		var a = input.pop();
		var b = input.pop();
		if (a.isString && b.isString && (a.stringValue() == b.stringValue())) {
			output.push(SI_Int(1));
		} else if (a.isString && !b.isString) {
			output.push(SI_Int(0));
		} else if (a.isFloat || b.isFloat) {
			if (a.floatValue() == b.floatValue()) {
				output.push(SI_Int(1));
			} else {
				output.push(SI_Int(0));
			}
		} else {
			// both are ints
				if (a.intValue() == b.intValue()) {
					output.push(SI_Int(1));
				} else {
					output.push(SI_Int(0));
				}
		}
	},
	
	die: function(ctx, input, output) {
		throw("Interpreter died.");
	},
	
	'present?': function(ctx, input, output) {
		alert("unimplemented");
	},
	
	load: function(ctx, input, output) {
		alert("unimplemented");
	},
	
	out: function(ctx, input, output) {
		ctx.abstractions.output(input.pop().stringValue());
	},
	
	'in': function(ctx, input, output) {
		alert("unimplemented");
	},
	
};