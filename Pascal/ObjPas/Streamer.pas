{$MODE OBJFPC}
{ This unit is sort of a combination of the tokeniser and interp units from the }
{ non-Object Pascal source.  Its purpose is to take a TStream and run it. }
unit streamer;
interface
uses
	Classes, Ctx;
	
type
	{ The states for the tokeniser state machine - see s. 2.6 of TISBL spec 2.0 }
	TStreamerState = (C, S, T);

	TStreamer = class
	private
		fState : TCtxes;
		
		{ The innards of the state machine }
		{ Doesn't seem much point in splitting this into a separate class }
		pstate : TStreamerState;
		buf    : string;
	public
		constructor Create;
		destructor Destroy; override;
		procedure RunStream(stream: TStream);
		procedure Process(str: string);
		
		procedure Take(ch: char);
		
		property state : TCtxes read fState;
	end;
	
implementation
uses
	Trace, Stack;

constructor TStreamer.Create;
begin
	inherited Create;
	fState := TCtxes.Create;
	state.start(nil, nil); { start the root context }
end;

destructor TStreamer.Destroy;
begin
	self.state.free;
end;

procedure TStreamer.RunStream(stream: TStream);
const 
	buf_size = 4096;
var
	runloop : boolean;
	buffer : array[1..buf_size] of char; { no need to dynamically allocate this }
	len : longint;
	i : longint;
begin
	pstate := S;
	buf := '';
	runloop := true;
	repeat
		len := stream.read(buffer, buf_size);
		for i := 1 to len do
			self.take(buffer[i]);
		if len < buf_size then runloop := false;
	until not runloop;
	
	self.take(' '); { force termination of current token }
	
	state.cur.go;
end;

procedure TStreamer.Take(ch: char);
begin
	{ The transition function in the S.M. }
	case pstate of
	C:
		{ The only thing that takes us out of the comment state }
		{ is a newline }
		if (ch = chr(10)) or (ch = chr(13)) then
			pstate := S;
	S:
		{ if it's a % then we need to start a comment }
		if ch = '%' then
			pstate := C
		{ if it's not a nonprinting character or a space }
		{ then enter it in the buffer }
		else if ord(ch) > 32 then
		begin
			buf := ch;
			pstate := T;
		end;
	T:
		{ if it is not a whitespace }
		if ord(ch) > 32 then
			buf := buf + ch
		else { it is a whitespace }
		begin
			self.Process(buf);
			buf := '';
			pstate := S;
		end;
	end;
end;

procedure TStreamer.Process(str: string);
var
	item : TAbstractStackItem;
begin
	item := TStringStackItem.Create(str);
	state.cur.exec.sneak(item);
end;

end.