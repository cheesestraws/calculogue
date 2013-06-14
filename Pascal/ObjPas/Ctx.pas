{$MODE OBJFPC}
unit Ctx;
{ The Ctx unit manages contexts }
{ A Ctx consists of its three stacks and references to three others }

interface
uses
	SysUtils, Classes, AbstractCtx, Trace, Stack, Tokens, Verbs, VerbTable;
	
type
	TCtxes = class; { This is a forward declaration; it will be declared later }
	                { because our Ctx needs to know what its parent is so it can }
					{ spawn new contexts for subprograms }

	TCtx = class (TAbstractCtx, ITraceable)
	private
		fPri, fSec, fExec: TStack;
		
		{ These are references. DO NOT FREE THEM: }
		fIn, fOut, fUpExec: TStack;
		
		{ This is 'private', not 'strictly private', so other }
		{ code in the same unit can see it }
		fParent : TCtxes;
		
		{ These are virtual and separated from go so that subclasses can }
		{ override these if I want to play with weirdy semantics. }
		{ Specifically, this will be useful for TISBLTeX... }
		procedure runstr(s: string); virtual;
		procedure runtok(t : Token); virtual;
		function getStackFor(t: TokenStack; isInputStack: boolean): TStack; virtual;
				
	public
		constructor Create(inp, outp: TStack);
		destructor Destroy; override;
		
		function pri: TStack; override;
		function sec: TStack; override;
		function exec: TStack; override;
				
		procedure go; override;
		procedure printTrace;
	end;
	
	TCtxes = class(TAbstracTCtxes, ITraceable)
		protected
			list : TFPList;
			fVerbs : TVerbTable;
			function curCtx: TAbstractCtx; override;
		public
			constructor Create;
			destructor Destroy; override;
			
			procedure start(inp, outp: TStack); override;
			procedure stop; override;
			procedure defineTISBLVerb(name: string; s: TStack); override;
			
			procedure printTrace;
			
			property verbs : TVerbTable read fVerbs;
	end;

implementation
uses
	ExceptionUnit;

	{ TCtx }
	constructor TCtx.Create(inp, outp: TStack);
	begin
		inherited Create;
		fPri := TStack.Create;
		fSec := TStack.Create;
		fExec := TStack.Create;
		
		{ Stick the input and output stacks into our thingy }
		fIn := inp;
		fOut := outp;
		fUpExec := nil;
		fParent := nil;
	end;
	
	destructor TCtx.Destroy;
	begin
		if fPri <> nil then fPri.free;
		if fSec <> nil then fSec.free;
		if fExec <> nil then fExec.free;
	end;
	
	{ Turns a TokenStack into a TStack }
	function TCtx.getStackFor(t: TokenStack; isInputStack: boolean) : TStack;
	begin
		case t of
			DefaultStack: 
				getStackFor := self.pri;
			SecondaryStack: 
				getStackFor := self.sec;
			ExecStack:
				getStackFor := self.exec;
			IOStack: 
				if isInputStack then
					getStackFor := self.fIn
				else
					getStackFor := self.fOut;
			ParentExecStack:
				getStackFor := self.fUpExec;
		end;
	end;
	
	{ This runs a token }
	procedure TCtx.runtok(t: Token);
	var
		item : TAbstractStackItem;
		s : TStack;
		v : TVerb;
	begin
		{ First, check whether the token is actually a valid token }
		case t.tokType of
		UnknownToken:
			{ Bad Tokens should have been filtered out by strToTok }
			raise EBadToken.Create('There was a bad token, which got through as far as runtok. This is a bug in the interpreter.');
		WordToken:
			begin
				item := TStringStackItem.Create(t.contents);
				s := self.getStackFor(t.stack1, false);
				s.push(item);
			end;
		NumToken:
			if pos('.', t.contents) <> 0 then
			begin
				{ It has a . in it, it's a float! }
				item := TDoubleStackItem.Create(StrToFloat(t.contents));
				s := self.getStackFor(t.stack1, false);
				s.push(item);
			end
			else
			begin
				{ No ., it's an integer! }
				item := TIntegerStackItem.Create(StrToInt(t.contents));
				s := self.getStackFor(t.stack1, false);
				s.push(item);
			end;
		VerbToken:
			begin
				v := self.fParent.verbs.find(t.contents);
				if v <> nil then
					v.run(self.fParent, self.getStackFor(t.stack1, true), 
							self.getStackFor(t.stack2, false))
				else
					raise EBadVerb.Create('Unknown verb: ' + t.contents);
			end;
		end;
	end;
	
	{ This runs a string }
	procedure TCtx.runstr(s: string);
	begin
		trace.trace('Running token: ' + s);
		self.runtok(strToTok(s));
		if self.fParent <> nil then
			{ We have a TCtxes to print state of }
			self.fParent.printTrace;
	end;
	
	{ This executes a context. }
	procedure TCtx.go;
	var
		i: TAbstractStackItem;
	begin
		while self.exec.Count > 0 do
		begin
			i := self.exec.pop;
			{ Is it a string item? }
			if i is TStringStackItem then
			begin
				try
					self.runstr(i.strValue);
				finally
					i.free;
				end
			end
			else
			begin
				i.free;
				raise EBadExecItem.Create('Cannot execute non-string stack item: ' + i.strValue);
			end;
		end;
	end;
	
	procedure TCtx.printTrace;
	begin
		trace.trace('Primary:');
		pri.printTrace;
		trace.trace('Secondary:');
		sec.printTrace;
		trace.trace('Exec:');
		exec.printTrace;
	end;
	
	
	{ These should really be properties, but ... }
	
	function TCtx.pri : TStack;
	begin
		pri := fPri;
	end;
	
	function TCtx.sec : TStack;
	begin
		sec := fSec;
	end;
	
	function TCtx.exec : TStack;
	begin
		exec := fExec;
	end;

	
		
	{ TCtxes }
	constructor TCtxes.Create;
	begin
		inherited Create;
		list := TFPList.Create;
		fVerbs := TVerbTable.Create;
	end;
	
	destructor TCtxes.Destroy;
	var
		c : TCtx;
	begin
		if list <> nil then 
		begin
			while list.Count > 0 do
			begin
				c := TCtx(list.Last);
				list.Delete(list.Count - 1);
				c.free;
			end;
			list.free;
		end;
		fVerbs.free;
		
		inherited Destroy;
	end;
	
	{ This starts a new context on the context stack }
	procedure TCtxes.start(inp, outp: TStack);
	var
		c: TCtx; { the new context }
		pc : TCtx; { the parent context, if one exists }
	begin
		c := TCtx.Create(inp, outp);
		
		{ The old C++ joke: friends can fiddle with each others' }
		{ private parts.  These are in the same unit, so they can! }
		c.fParent := self;
		if list.Count > 0 then
		begin
			pc := TCtx(list.Last);
			c.fUpExec := pc.exec;
		end;
		
		list.add(c);
	end;
	
	procedure TCtxes.printTrace;
	begin
		if list.Count = 1 then
			trace.trace('In topmost context')
		else if list.Count > 1 then
			trace.trace('In a context ' + IntToStr(list.Count - 1) + ' down from top.')
		else
			trace.trace('In a zero or negative context, something''s gone terribly wrong somewhere.  Segfault incoming.');
			
		trace.trace('');
		TCtx(cur).printTrace;
	end;
	
	procedure TCtxes.stop;
	var
		c: TCtx;
	begin
		if list.Count = 0 then
			raise ECtxUnderflow.Create('Tried to stop a Ctx that does not exist.  Probably an interpreter bug.');
		
		c := TCtx(list.Last);
		list.Delete(list.Count - 1);
		c.free;
	end;

	function TCtxes.curCtx : TAbstractCtx;
	begin
		if list.Count = 0 then
			raise ECtxUnderflow.Create('Tried to manipulate a Ctx that does not exist.  Probably an interpreter bug.');
		curCtx := TAbstractCtx(list.Last);
	end;

	procedure TCtxes.defineTISBLVerb(name: string; s: TStack);
	begin
		fVerbs.defineTISBL(name, s);
	end;

end.