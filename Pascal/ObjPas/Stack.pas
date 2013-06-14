{$MODE OBJFPC}
unit Stack;
interface
{ This unit contains the Stack and the StackItem classes }
{ TAbstractStackItem is the root class which defines the }
{ basic contract for the stack items. }

{ Stack items are data objects.  Every time one is returned }
{ from the Stack, THE CALLER IS RESPONSIBLE FOR FREEING IT. }

{ Things to do here: Add proper exception handling to the }
{ conversion routines. Some caching might not be a bad    }
{ thing too, as the objects are immutable. }

uses
	classes, trace; { for TFPList }

type
	TAbstractStackItem = class
		public
			function intValue: longint; virtual; abstract;
			function dblValue: double;  virtual; abstract;
			function strValue: string;  virtual; abstract;
			function clone: TAbstractStackItem; virtual; abstract;
	end;
	
	TIntegerStackItem = class(TAbstractStackItem)
		private
			val : longint;
		public
			constructor Create(v: longint);
			function intValue: longint; override;
			function dblValue: double;  override;
			function strValue: string;  override;
			function clone: TAbstractStackItem; override;
	end;
	
	TDoubleStackItem = class(TAbstractStackItem)
		private
			val : double;
		public
			constructor Create(v: double);
			function intValue: longint; override;
			function dblValue: double;  override;
			function strValue: string;  override;
			function clone: TAbstractStackItem; override;
	end;
	
	TStringStackItem = class(TAbstractStackItem)
		private
			val: string;
		public
			constructor Create(v: string);
			function intValue: longint; override;
			function dblValue: double;  override;
			function strValue: string;  override;
			function clone: TAbstractStackItem; override;
	end;
	
	TStack = class(TInterfacedObject, ITraceable)
	public
		constructor Create;
		destructor Destroy; override;
		
		procedure push(item : TAbstractStackItem);
		procedure sneak(item : TAbstractStackItem);
		function  pop  : TAbstractStackItem;
		function  peek : TAbstractStackItem;
		
		procedure cloneItemsTo(s: TStack);
		
		procedure printTrace;
		
	private
		list : TFPList;
		function getCount : longint;
		
	public
		property Count : longint read getCount;
	end;

implementation
uses
	SysUtils, ExceptionUnit; { for IntToStr/StrToInt }


{ TIntegerStackItem }
constructor TIntegerStackItem.Create(v: longint);
begin
	inherited Create;
	self.val := v;
end;

function TIntegerStackItem.intValue : longint;
begin
	intValue := val;
end;

function TIntegerStackItem.dblValue : double;
begin
	dblValue := val;
end;

function TIntegerStackItem.strValue : string;
begin
	strValue := IntToStr(val);
end;

function TIntegerStackItem.clone : TAbstractStackItem;
begin
	clone := TIntegerStackItem.Create(val);
end;

{TDoubleStackItem}
constructor TDoubleStackItem.Create(v: double);
begin
	inherited Create;
	self.val := v;
end;

function TDoubleStackItem.intValue : longint;
begin
	intValue := round(val);
end;

function TDoubleStackItem.dblValue : double;
begin
	dblValue := val;
end;

function TDoubleStackItem.strValue : string;
begin
	strValue := FloatToStr(val);
end;

function TDoubleStackItem.clone : TAbstractStackItem;
begin
	clone := TDoubleStackItem.Create(val);
end;

{TStringStackItem}
constructor TStringStackItem.Create(v: string);
begin
	inherited Create;
	self.val := v;
end;

function TStringStackItem.intValue : longint;
begin
	intValue := StrToInt(val);
end;

function TStringStackItem.dblValue : double;
begin
	dblValue := StrToFloat(val);
end;

function TStringStackItem.strValue : string;
begin
	strValue := val;
end;

function TStringStackItem.clone : TAbstractStackItem;
begin
	{ Thankfully, strings are still data types! not pointers! }
	clone := TStringStackItem.Create(val);
end;

{TStack}

constructor TStack.Create;
begin
	inherited Create;
	list := TFPList.Create;
end;

destructor TStack.Destroy;
var
	i : TAbstractStackItem;
begin
	{ FreePascal hasn't got anonymous functions (yet!) }
	{ Delphi has, but it hadn't last time I used it in anger. }
	{ So it'd be nice to .map over the list and free all the ptrs. }
	{ But I'll just do this instead }
	while list.Count > 0 do
	begin
		i := self.pop;
		if i <> nil then i.free;
	end;
	
	list.Free;
	inherited Destroy;
end;

procedure TStack.push(item : TAbstractStackItem);
begin
	list.Add(item);
end;

{ Sneak puts an item at the /beginning/ of the stack }
{ This is only really for the use of the initial setup of }
{ the execution stack of a context. }
procedure TStack.sneak(item : TAbstractStackItem);
begin
	list.Insert(0, item);
end;

function TStack.pop : TAbstractStackItem;
var
	p : Pointer;
begin
	if list.Count = 0 then
		raise EStackUnderflow.Create('Stack underflow');
	p := list.Last;
	list.Delete(list.Count - 1);
	pop := TAbstractStackItem(p);
end;

function TStack.peek : TAbstractStackItem;
var
	i : TAbstractStackItem;
begin
	i := TAbstractStackItem(list.Last);
	peek := i.Clone;
end;

function TStack.getCount : longint;
begin
	getCount := list.Count;
end;

procedure TStack.printTrace;
var
	i : longint;
begin
	mtraceStart;
	mtrace('TOP =>');
	for i := list.Count - 1 downto 0 do
	begin
		mtrace(TAbstractStackItem(list.items[i]).strValue);
	end;
	mtraceEnd;
end;

procedure TStack.cloneItemsTo(s: TStack);
var
	i : longint;
	item : TAbstractStackItem;
begin
	for i := 0 to list.Count - 1 do
	begin
		item := TAbstractStackItem(list.items[i]).clone;
		s.push(item);
	end;
end;

end.