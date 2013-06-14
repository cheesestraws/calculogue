{$MODE OBJFPC}
unit AbstractCtx;
{ These are abstract superclasses for TCtx and TCtxes. }
{ These define the interfaces that the stdlib ought to need... }
{ They break the circular dependency between the stdlib and TCtx, in a similar way }
{ as the ProcPtr tricks did in the procedural pascal one.  But - this lets me      }
{ sanity check the interface provided to the stdlib and not let it do things like  }
{ muck with the parent execution stack }

interface
uses Stack;

type
	TAbstractCtx = class(TInterfacedObject)
	public
		function exec: TStack; virtual; abstract;
		function pri: TStack; virtual; abstract;
		function sec: TStack; virtual; abstract;
		
		procedure go; virtual; abstract;
	end;
	
	TAbstractCtxes = class(TInterfacedObject)
	protected
		function curCtx: TAbstractCtx; virtual; abstract;
	public
		procedure start(input, output: TStack); virtual; abstract;
		procedure stop; virtual; abstract;
		
		procedure defineTISBLVerb(name: string; s: TStack); virtual; abstract;
		
		property cur : TAbstractCtx read curCtx;
	end;

implementation

end.