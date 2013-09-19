/* Stack items 
   =========== */
   
/* The Big Archetypal Stack Item! */
archetypes.StackItem = {
	isAStackItem: true,
	
	abstractError: function() { throw("Interpreter bug: you shouldn't create direct children of StackItem."); },
	
	/* float values */
	floatValue: function() { this.abstractError(); },
	stringValue: function() { this.abstractError(); },
	intValue: function() { this.abstractError(); },
	
	/* Trace output */
	traceType: function() { return "xxx" },
	traceValue: function() { return this.stringValue() },
	traceString: function() { return this.traceType() + "." + this.traceValue() },
};

/* Int items */
archetypes.SI_Int = child_of(archetypes.StackItem, {
	value: 0,
	isANumericItem: true,
	isInteger: true,
	
	floatValue: function() { return this.value; },
	stringValue: function() { return this.value.toString(); },
	intValue: function() { return this.value; },
	traceType: function() { return "int" }
});

SI_Int = function(val) {
	// JS has no distinction between floats and ints; but we care
	// to make sure the spec is properly implemented.  So our SI_Int
	// function, which returns a stack item, needs to check whether
	// its value is actually an integer.
	//
	// val===+val is a nasty hack, but it seems to work!
	if (!(val===+val) || !(val % 1 == 0)) {
		throw("SI_Int didn't get an integer.  Got: " + val.toString());
	}
	return child_of(archetypes.SI_Int, {value: val});
};

/* Float items */
archetypes.SI_Float = child_of(archetypes.StackItem, {
	value: 0,
	isANumericItem: true,
	isFloat: true,
	
	floatValue: function() { return this.value; },
	stringValue: function() { return this.value.toString(); },
	intValue: function() { return Math.round(this.value); },
	traceType: function() { return "flt" }
});

SI_Float = function(val) {
	if (!(val===+val)) {
		throw("SI_Int didn't get a number.  Got: " + val.toString());
	}
	return child_of(archetypes.SI_Float, {value: val});
};

/* String items */
archetypes.SI_String = child_of(archetypes.StackItem, {
	value: 0,
	isString: true,
	
	floatValue: function() { return parseFloat(this.value); },
	stringValue: function() { return this.value; },
	intValue: function() { return parseInt(this.value); },
	traceType: function() { return "str" }
});

SI_String = function(val) {
	return child_of(archetypes.SI_String, {value: val.toString()});
};


/* A stack */
archetypes.Stack = {
	values: null,
	
	_init_: function() {
		this.values = [];
	},
	
	push: function(item) {
		// This is, of course, trivially subvertable. It's a sanity check,
		// not a defence against active attack.
		if (item.isAStackItem) {
			this.values.push(item);
		} else {
			throw("Intepreter bug: A stack got a non-item");
		}
	},
	
	pop: function() {
		if (this.empty()) { throw("Stack underflow"); }
		return this.values.pop();
	},
	
	peek: function() {
		return this.values[this.values.length - 1];
	},
	
	sneak: function(item) {
		if (item.isAStackItem) {
			this.values.unshift(item);
		} else {
			throw("Intepreter bug: A stack got a non-item");
		}
	},
	
	empty: function() {
		return !(this.values.length > 0);
	},
	
	cloneFromStack: function(stack) {
		this.values = [];
		for (var i = 0; i < stack.values.length; i++) {
			this.values.push(stack.values[i]);
		}
	},
	
	trace: function(name, abstractions) {
		var v = this.values.reverse()
			.map(function(_) { return _.traceString(); })
			.join(' ');
		abstractions.output(name + ": " + v + "\n");
	}
};


