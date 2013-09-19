/* This is an interpreter! */

archetypes.Interpreter = {
	verbtable: child_of(stdlib),
	
	_init_: function() {
		this.rootCtx = child_of(archetypes.Ctx, {
			verbtable: this.verbtable,
			abstractions: {
				output: function(str) {
					console.log(str);
				},
				
				input: function(str) {
					window.prompt("");
					throw("No input handler!");
				}
			}
		});
	},
	
	splitTokens: function(str) {
		var blob = str;
		// kill end of line comments
		blob = blob.replace(/\s+%.*$/mg, '');
		blob = blob.replace(/^%.*$/mg, '');
		blob = blob.replace(/^\s+/mg, '');

		var list = blob.split(/\s+/);
		return list;
	},
	
	runString: function(str, abstractions) {
		var lst = this.splitTokens(str).reverse();
		// this is a bit naughty since it sort of assumes knowledge of how
		// the stack object works.
		if (abstractions) {
			this.rootCtx.abstractions = abstractions;
		}
		this.rootCtx.exec.values = lst.map(SI_String);
		this.rootCtx.go();
	}
	
}