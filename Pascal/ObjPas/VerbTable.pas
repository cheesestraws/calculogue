{$MODE OBJFPC}
unit VerbTable;
interface
uses
	classes, Stack, Verbs;

{ This is a verb table }
{ It's deliberately simplistic, had a choice between using a simple table and a }
{ largely undocumented generics library.  This was the only way. }
{ }
{ There's actually a potential difference in semantics here between this and the}
{ procedural pascal interpreter, if both implemented a \load for internal,      }
{ native, modules.  In the TP one, the TISBL verbs are always searched before   }
{ the native verbs; here they're in one table.  This means that if we had a     }
{ module 'foo that included a \foo native verb, then this program would behave  }
{ differently on the two interpreters:                                          }
{ }
{ ''foo '\n #2 'foo \verb  % define \foo locally                                }
{ 'foo \present? ''foo '\load \if % load foo module                             }
{ \foo  % on procedural pascal, runs tisbl foo; on here, runs native            }
{ }
{ Fortunately, the ProcPascal one isn't going to implement any modules.         }

type
	TVerbTable = class
	private
		fList : TFPList;
		
	public
		constructor Create;
		destructor Destroy; override;
		
		procedure undefine(name: string);
		procedure define(name: string; verb: TVerb);
		{ This is a convenience method }
		procedure defineNative(name: string; verb: TNativeVerbProc);
		{ This is also a convenience method }
		procedure defineTISBL(name: string; s: TStack);
		function find(name: string) : TVerb;
	end;

implementation

{ This is a private type to the unit.  No reference should ever actually leak out }
{ of the VerbTable anyway }
type
	TVerbTableEntry = class
		name : string;
		verb : TVerb;
		
		destructor Destroy; override;
	end;

constructor TVerbTable.Create;
begin
	inherited Create;
	fList := TFPList.Create;
end;

destructor TVerbTable.Destroy;
var
	i : integer;
	e : TVerbTableEntry;
begin
	if fList <> nil then
	begin
		for i := 0 to fList.Count - 1 do
		begin
			e := TVerbTableEntry(fList.items[i]);
			e.free;
		end;
		fList.free;
	end;
	inherited Destroy;
end;

procedure TVerbTable.undefine(name : string);
var
	i : longint = 0;
	entry : TVerbTableEntry;
begin
	while i < fList.count do
	begin
		entry := TVerbTableEntry(fList.items[i]);
		if entry.name = name then
		begin
			fList.delete(i);
			entry.free;
		end
		else
			i := i + 1;
	end;
end;

procedure TVerbTable.define(name: string; verb: TVerb);
var
	entry: TVerbTableEntry;
begin
	{ Clean up any pre-existing verbs first }
	self.undefine(name);
	entry := TVerbTableEntry.Create;
	entry.name := name;
	entry.verb := verb;
	fList.Add(entry);
end;

procedure TVerbTable.defineNative(name: string; verb: TNativeVerbProc);
var
	v : TVerb;
begin
	v := TNativeVerb.Create(verb);
	self.define(name, v);
end;

procedure TVerbTable.defineTISBL(name: string; s: TStack);
var
	v : TVerb;
begin
	v := TTISBLVerb.Create(s);
	self.define(name, v);
end;

function TVerbTable.find(name: string): TVerb;
var
	i : longint;
	entry : TVerbTableEntry;
begin
	find := nil;
	for i := 0 to fList.count - 1 do
	begin
		entry := TVerbTableEntry(fList.items[i]);
		if entry.name = name then
			find := entry.verb;
	end;
end;


{ Destructor for TVerbTableEntry, to free the verb }
destructor TVerbTableEntry.Destroy;
begin
	verb.free;
	inherited Destroy;
end;

end.