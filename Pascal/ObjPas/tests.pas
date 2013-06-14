{$MODE OBJFPC}
unit tests;
{ This contains some unit-type tests for the guff inside.  It currently tests: }
{  * Whether the clone semantics on Stack Items are correct }
{  * Whether stack peek correctly returns a different object from pop }
interface

procedure runtests;

implementation
uses 
	SysUtils, Classes, ExceptionUnit, Stack, Ctx, Tokens, Streamer, stdlib;
	
	
	
procedure fail(s: string);
begin
	writeln('[FAIL] ', s);
	raise(ETestFailed.Create(s)) at get_caller_addr(get_frame), get_caller_frame(get_frame);
end;

procedure pass(s: string);
begin
	writeln('[PASS] ', s);
end;



procedure StackItemTests;
var
	stack : TStack;
	i : TIntegerStackItem;
	f : TDoubleStackItem;
	s : TStringStackItem;
	i_, f_, s_ : TAbstractStackItem;
begin
	{ Check clone() semantics }
	writeln('Checking .clone semantics.');
	i := TIntegerStackItem.Create(123);
	i_ := i.clone;
	if i = i_ then
		fail('Cloned TIntegerStackItem pointers identical!')
	else if not (i_ is TIntegerStackItem) then
		fail('Cloned TIntegerStackItem type not identical!')
	else if i.intValue <> i_.intValue then
		fail('Values of cloned TIntegerStackItem not identical!')
	else
		pass('TIntegerStackItem clones correctly.');
		
	f := TDoubleStackItem.Create(2.5);
	f_ := f.clone;
	if f = f_ then
		fail('Cloned TDoubleStackItem pointers identical!')
	else if not (f_ is TDoubleStackItem) then
		fail('Cloned TDoubleStackItem type not identical!')
	else if f.dblValue <> f_.dblValue then
		fail('Values of cloned TDoubleStackItem not identical!')
	else
		pass('TDoubleStackItem clones correctly.');
		
	s := TStringStackItem.Create('hello world');
	s_ := s.clone;
	if s = s_ then
		fail('Cloned TStringStackItem pointers identical!')
	else if not (s_ is TStringStackItem) then
		fail('Cloned TStringStackItem type not identical!')
	else if s.strValue <> s_.strValue then
		fail('Values of cloned TStringStackItem not identical!')
	else
		pass('TStringStackItem clones correctly.');
	
	i.Free; i_.Free;
	f.Free; f_.Free;
	s.Free; s_.Free;
	
	{ Check Stack peek/pop }

	stack := TStack.Create;
	s := TStringStackItem.Create('Hello World');
	stack.push(s);
	s_ := stack.peek;
	s := TStringStackItem(stack.pop);
	
	if (s <> s_) and (s.strValue = s_.strValue) then
		pass('Peek and pop return different, but same-valued objects.')
	else
		fail('Peek and pop do NOT return different but same-valued objects.');
	
	s.free;
	s_.free;
	stack.Free;
end;

procedure CtxTests;
var
	cs : TCtxes;
begin
	cs := TCtxes.Create;
	
	{ Check that underflow exceptions work }
	try
		cs.stop;
		fail('No exception raised.');
	except
		on e: ECtxUnderflow do ;
		on e: Exception do
			fail('Some other exception occurred in place of Ctx underflow: ' + e.message);
	end;
	pass('Ctx Underflow correctly signalled.');
	
	cs.start(nil, nil);
	
	cs.free;
end;

procedure StreamerTests;
var
	streamer : TStreamer;
	stream : TStringStream;
begin
	stream := TStringStream.Create('#1 \not ''''hello ''''world ''\out #3 \if');
	streamer := TStreamer.Create;
	
	installStdlib(streamer.state.verbs);
	
	streamer.RunStream(stream);
	
	stream.free;
	streamer.free;
end;

{ The entry point }
	
procedure runtests;
begin
	StackItemTests;
	CtxTests;
	StreamerTests;
end;


end.