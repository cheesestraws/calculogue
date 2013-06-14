{$MODE OBJFPC}
unit Verbs;
interface
uses 
	Stack, AbstractCtx;
	
{ The verb implementation uses polymorphism.  TVerb is a base class that has a run }
{ method overridden in subclasses.  There are two basic subclasses: TTISBLVerb,    }
{ which implements a TISBL verb, and TNativeVerb which wraps a pascal procedure.   }
{ }
{ Now, in theory, each verb /could/ be implemented as a subclass of TVerb, and     }
{ this is how Java would do it.  But that would result in /huge/ numbers of classes}
{ and here we have a better option - to take a pointer to a procedure.  We'll see  }
{ a similar pattern in Scala, and in the LISP-like languages, but there's an       }
{ important difference: there, functions are actually first-class.  Here, what is  }
{ being thrown about is just a memory address.  This means, for example, there are }
{ no anonymous functions here (Delphi does them, sort of, since 2009), and that    }
{ if you mandate a calling convention for your procedure, the calling convention   }
{ of the procedure type and the procedure you're storing a pointer to must match.  }
{ }
{ Have some pseudo-UML: }
{ }
{                  +-----------------+                  }
{                  |      TVerb      |                  }
{                  +-----+-----+-----+                  }
{                        |     |                        }
{              +---------+     +---------+              }
{              |                         |              }
{              v                         v              }
{     +-----------------+       +-----------------+     }
{     |   TTISBLVerb    |       |   TNativeVerb   |     }
{     +-----------------+       +-----------------+     }

type
	{ The base class of the verb heirarchy }
	TVerb = class
	public
		procedure run(state: TAbstractCtxes; inp, outp: TStack); virtual; abstract;
	end;
	
	{ Note that we have nicely typed procedure pointers. No more ProcPtr tricks! }
	TNativeVerbProc = procedure (state: TAbstractCtxes; inp, outp: TStack);
	TNativeVerb = class(TVerb)
	private
		fProcPtr : TNativeVerbProc;
	public
		constructor Create(proc: TNativeVerbProc);
		procedure run(state: TAbstractCtxes; inp, outp: TStack); override;
	end;
	
	{ A TISBL Verb wraps an initial state for an execution state }
	TTISBLVerb = class(TVerb)
	private
		fStack : TStack;
	public
		constructor Create(s: TStack);
		destructor Destroy; override;
		procedure run(state: TAbstractCtxes; inp, outp: TStack); override;
	end;
	
implementation

{ TNativeVerb implementation }
constructor TNativeVerb.Create(proc: TNativeVerbProc);
begin
	inherited Create;
	fProcPtr := proc;
end;

procedure TNativeVerb.run(state: TAbstractCtxes;inp, outp: TStack);
begin
	if fProcPtr <> nil then
		fProcPtr(state, inp, outp);
end;

{ TTISBLVerb implementation }
constructor TTISBLVerb.Create(s: TStack);
begin
	fStack := s;
end;

procedure TTISBLVerb.run(state: TAbstractCtxes;inp, outp: TStack);
var
	ctx : TAbstractCtx;
begin
	{ create a new context }
	state.start(inp, outp);
	ctx := state.cur;
	
	{ clone the execution stack onto it }
	fStack.cloneItemsTo(ctx.exec);
	
	{ run the context }
	ctx.go;
	
	{ tear it down }
	state.stop;
end;

destructor TTISBLVerb.Destroy;
begin
	if fStack <> nil then fStack.free;
	inherited Destroy;
end;

end.