/* A context */
archetypes.Ctx = {
	pri: null,
	sec: null,
	exec: null,
	input: null,
	output: null,
	upexec: null,
	interp: null,
	
	_init_: function() {
		this.pri = child_of(archetypes.Stack);
		this.sec = child_of(archetypes.Stack);
		this.exec = child_of(archetypes.Stack);
		this.interp = {};
		this.depth = 0;
		this.state = { trace: false };
	},
	
	resolveStack: function(stackname, isInputStack) {
		// this resolves a stack name (.,:;) to an actual stack object
		switch (stackname) {
			case ".":
				if (isInputStack) {
					return this.input;
				} else {
					return this.output;
				}
				break;
			case ",":
				return this.exec;
				break;
			case ":":
				return this.sec;
				break;
			case ";":
				return this.upexec;
				break;
			case "":
				return this.pri;
				break;
			default:
				break;
		}
	},
	
	runString: function(stringparam) {
		if (this.state.trace) {
			this.abstractions.output("Context depth: " + this.depth + "\n");
			this.abstractions.output("Executing: " + stringparam + "\n");
		}
		
		var tok = {};
		var str = stringparam;
		var arr;
		// check whether the string is a verb or a noun
		// the slightly odd regex syntax is ... odd.
		if (arr = str.match(/^\\([.,:;]?)(.*?)([.,:;]?)$/)) {
			tok = {
				verbToken: true,
				inStack: arr[1],
				content: arr[2],
				outStack: arr[3]
			};
		} else if (arr = str.match(/^([.,:;]?)([#'])(.*)$/)) {
			tok = {
				nounToken: true,
				nounType: arr[2],
				content: arr[3],
				outStack: arr[1]
			};
		} else if (str == "") {
			// Null program!
			tok = {
				nullToken: true
			};
		} else {
			throw("Bad token: " + stringparam);
		}
		this.runTok(tok);
	},
	
	runTok: function(tok) {
		if (tok.verbToken) {
			var inStack = this.resolveStack(tok.inStack, true);
			var outStack = this.resolveStack(tok.outStack, false);
			if (this.verbtable[tok.content]) {
				this.verbtable[tok.content](this, inStack, outStack);
			} else {
				throw("No such verb: " + tok.content);
			}
		} else if (tok.nounToken) {
			
			// It's a noun token.  What sort?
			var stackItem = null;
			if (tok.nounType == "#") {
				// A number.  See if it's a float or not.
				if (tok.content.indexOf(".") > -1) {
					stackItem = SI_Float(parseFloat(tok.content));
				} else {
					stackItem = SI_Int(parseInt(tok.content));
				}
			} else if (tok.nounType == "'") {
				// It's a string
				stackItem = SI_String(tok.content);
			} else {
				throw("Interpreter bug: bad nounType.  Check definitions of Ctx.runTok and runString to make sure they line up.");
			}
			
			// now, find where we've got to poke it into
			var stk = this.resolveStack(tok.outStack, false);
			stk.push(stackItem);
		} else if (tok.nullToken) {
			return;
		} else {
			throw("Interpreter bug: trying to runTok a non-token.  Check definitions of Ctx.runTok and runString to make sure they line up.");
		}
		
		if (this.state.trace) {
			this.trace();
		}
	},
	
	go: function() {
		var item;
		while(!this.exec.empty()) {
			item = this.exec.pop();
			if (!item.isString) {
				throw("You can only execute strings.  Got: " + item.traceString());
			}
			this.runString(item.stringValue());
		}
	},
	
	spawn: function(input, output) {
		// Clone from the same object we were cloned from!
		var newCtx = child_of(Object.getPrototypeOf(this));
		newCtx.upexec = this.exec;
		newCtx.input = input;
		newCtx.output = output;
		newCtx.abstractions = this.abstractions;
		newCtx.verbtable = this.verbtable;
		newCtx.state = this.state;
		newCtx.depth = this.depth + 1;
		
		return newCtx;
	},
	
	trace: function() {
		this.pri.trace("PRI ", this.abstractions);
		this.sec.trace("SEC ", this.abstractions);
		this.exec.trace("EXEC", this.abstractions);
	}
};

