{$MODE OBJFPC}
unit stdlib;

{ This unit is /very/ similar to the non-OO stdlib, and in places identical. }
{ The lack of comments is because it's basically a completely straight       }
{ implementation of the 'Standard Library' section of the language spec.     }

interface
uses 
	VerbTable;
	
procedure installStdLib(vt: TVerbTable);

implementation
uses 
	trace, ExceptionUnit, AbstractCtx, Stack, Verbs;
	
	

procedure out_(c: TAbstractCtxes; inp, outp: TStack);
var
	e : TAbstractStackItem;
begin
	e := inp.pop;
	write(e.strValue);
	e.free;
end;



procedure in_(c: TAbstractCtxes; inp, outp: TStack);
var
	line : string;
begin
	readln(line);
	outp.push(TStringStackItem.Create(line));
end;



procedure exec(c: TAbstractCtxes; inp, outp: TStack);
var
	item: TAbstractStackItem;
	count : longint;
	ctx: TAbstractCtx;
	i : longint;
begin
	{ get the count }
	item := inp.pop;
	if not (item is TIntegerStackItem) then
	begin
		item.free;
		raise EBadParameter.Create('\exec expected an integer, and didn''t get one.');
	end;
	
	count := item.intValue;
	item.free;
	
	{ start a new context }
	c.start(inp, outp);
	ctx := c.cur;
	
	{ copy the code in }
	for i := 1 to count do
	begin
		item := inp.pop;
		ctx.exec.push(item);
	end;
	
	{ run the context }
	ctx.go;
	
	{ kill the context and return to the parent context }
	c.stop;
end;



procedure verb(c: TAbstractCtxes; inp, outp: TStack);
var
	item: TAbstractStackItem; { swap space for dissecting stack items }
	name : string;
	count: longint;
	i : longint;
	s: TStack;
begin
	{ get the verb name }
	item := inp.pop;
	if not (item is TStringStackItem) then
	begin
		item.free;
		raise EBadParameter.Create('\verb expected a verb name, and didn''t get one.');
	end;
		
	name := item.strValue;
	item.free;

	{ get the count }
	item := inp.pop;
	if not (item is TIntegerStackItem) then
	begin
		item.free;
		raise EBadParameter.Create('\verb expected an integer, and didn''t get one.');
	end;
	
	count := item.intValue;
	item.free;

	{ create a template execution stack }
	s := TStack.Create;
	for i := 1 to count do
	begin
		item := inp.pop;
		s.push(item);
	end;
	
	c.defineTISBLVerb(name, s);
end;



procedure if_(c: TAbstractCtxes; inp, outp: TStack);
var
	item: TAbstractStackItem;
	count : longint;
	ctx: TAbstractCtx;
	i : longint;
begin
	{ get the count }
	item := inp.pop;
	if not (item is TIntegerStackItem) then
	begin
		item.free;
		raise EBadParameter.Create('\if expected an integer, and didn''t get one.');
	end;
	
	count := item.intValue;
	item.free;
	
	{ start a new context }
	c.start(inp, outp);
	ctx := c.cur;
	
	{ copy the code in }
	for i := 1 to count do
	begin
		item := inp.pop;
		ctx.exec.push(item);
	end;
	
	{ Grab the condition }
	item := inp.pop;
	{$PUSH}{$B-} { make sure that and/or short-circuit. if this wasn't }
	             { here we might end up with .intValue of a string,    }
				 { which would go pop. The push saves compiler flags.  }
	if ((item is TIntegerStackItem) or (item is TDoubleStackItem))
		and (item.intValue <> 0) then
			{$POP} ctx.go;
			
	item.free;
	
	{ kill the context and return to the parent context }
	c.stop;
end;



procedure while_(c: TAbstractCtxes; inp, outp: TStack);
var
	item: TAbstractStackItem; { swap space for dissecting stack items }
	count: longint;
	i : longint;
	s: TStack;
	ctx : TAbstractCtx;
	endloop, condition: boolean;
begin
	{ get the count }
	item := inp.pop;
	if not (item is TIntegerStackItem) then
	begin
		item.free;
		raise EBadParameter.Create('\verb expected an integer, and didn''t get one.');
	end;
	
	count := item.intValue;
	item.free;

	{ create a template execution stack }
	s := TStack.Create;
	for i := 1 to count do
	begin
		item := inp.pop;
		s.push(item);
	end;
	
	endloop := false;
	repeat
		item := inp.pop;
		condition := true;
		
		{$PUSH}{$B-} { make sure that and/or short-circuit. if this wasn't }
					 { here we might end up with .intValue of a string,    }
					 { which would go pop. The push saves compiler flags.  }
		if ((item is TIntegerStackItem) or (item is TDoubleStackItem))
			and (item.intValue = 0) then
				condition := false;
		{$POP}
		item.free;
		
		if not condition then
			endloop := true
		else
		begin
			{ the condition is true, so - run it! }
			c.start(inp, outp);
			ctx := c.cur;
			s.cloneItemsTo(ctx.exec);
			ctx.go;
			c.stop;
		end;
	until endloop;
	s.free;
end;



procedure not_(c: TAbstractCtxes; inp, outp: TStack);
var
	item : TAbstractStackItem;
begin
	item := inp.pop;
	{$PUSH}{$B-}
	if ((item is TIntegerStackItem) or (item is TDoubleStackItem))
		and (item.intValue <> 0) then
		outp.push(TIntegerStackItem.Create(0))
	else
		outp.push(TIntegerStackItem.Create(1));
	{$POP}
	item.free;
end;



procedure swap(c: TAbstractCtxes; inp, outp: TStack);
var
	a,b: TAbstractStackItem;
begin
	a := inp.pop;
	b := inp.pop;
	outp.push(a);
	outp.push(b);
end;



procedure dup(c: TAbstractCtxes; inp, outp: TStack);
var
	item : TAbstractStackItem;
begin
	item := inp.peek;
	outp.push(item);
end;



procedure rm(c: TAbstractCtxes; inp, outp: TStack);
var
	item : TAbstractStackItem;
begin
	item := inp.pop;
	item.free;
end;



procedure mv(c: TAbstractCtxes; inp, outp: TStack);
var
	item : TAbstractStackItem;
begin
	item := inp.pop;
	outp.push(item);
end;



procedure multipop(c: TAbstractCtxes; inp, outp: TStack);
var
	item: TAbstractStackItem;
	count: longint;
	i : longint;
begin
	{ get the count }
	item := inp.pop;
	if not (item is TIntegerStackItem) then
	begin
		item.free;
		raise EBadParameter.Create('\multipop expected an integer, and didn''t get one.');
	end;
	
	count := item.intValue;
	item.free;
	
	for i := 1 to count do
	begin
		item := inp.pop;
		outp.push(item);
	end;
end;



procedure plus(c: TAbstractCtxes; inp, outp: TStack);
var
	fst, sec: TAbstractStackItem;
	fs, ss : string;
	ff, sf : double;
begin
	sec := inp.pop;
	fst := inp.pop;
	
	if (sec is TStringStackItem) or (fst is TStringStackItem) then
	begin
		ss := sec.strValue;
		fs := fst.strValue;
		outp.push(TStringStackItem.Create(fs + ss));
	end
	else if (sec is TDoubleStackItem) or (fst is TDoubleStackItem) then
	begin
		sf := sec.dblValue;
		ff := fst.dblValue;
		outp.push(TDoubleStackItem.Create(sf + ff));
	end
	else
		outp.push(TIntegerStackItem.Create(fst.intValue + sec.intValue));
	
	sec.free;
	fst.free;
end;



{ Minus! }
{ These two helpers are identical to the non-OO Pascal one }
function sub_si (s: string; i: longint): string;
begin
	sub_si := copy(s, 1, length(s) - i);
end;

function sub_ss (haystack, needle: string): string;
var
	buf: string;
	len, i: byte;
begin
	len := 0;
	buf := '';

	for i := 1 to length(haystack) do
		if pos(haystack[i], needle) = 0 then
			begin
				len := len + 1;
				buf[len] := haystack[i];
			end;
	buf[0] := chr(len);
	sub_ss := buf;
end;

procedure minus(c: TAbstractCtxes; inp, outp: TStack);
var
	fst, sec: TAbstractStackItem;
begin
	sec := inp.pop;
	fst := inp.pop;
	
	if (sec is TIntegerStackItem) and (fst is TIntegerStackItem) then
		outp.push(TIntegerStackItem.Create(fst.intValue - sec.intValue))
	else if (sec is TStringStackItem) and 
		((fst is TIntegerStackItem) or (fst is TDoubleStackItem)) then
			outp.push(TStringStackItem.Create(sub_si(sec.strValue, fst.intValue)))
	else if ((sec is TIntegerStackItem) or (sec is TDoubleStackItem))
		and (fst is TStringStackItem) then
			outp.push(TStringStackItem.Create(sub_si(fst.strValue, sec.intValue)))
	else if (sec is TStringStackItem) and (fst is TStringStackItem) then
		outp.push(TStringStackItem.Create(sub_ss(fst.strValue, sec.strValue)))
	else
		outp.push(TDoubleStackItem.Create(fst.dblValue - sec.dblValue));
	
	sec.free;
	fst.free;
end;



{ Multiplication! }
{ again, the helper functions are the same as the non-OO ones}
function mul_sf (s: string; iter: double): string;
var
	count: double;
	buf: string;
	chars: byte;
begin
	buf := '';
	count := iter;
	while count >= 1 do
		begin
			count := count - 1;
			buf := concat(buf, s);
		end;

	if count > (1 / length(s)) then
		begin
			chars := round(length(s) * count);
			buf := concat(buf, copy(s, 1, chars));
		end;

	mul_sf := buf;
end;

function mul_ss (s, expand: string): string;
var
	buf: string = '';
	i: byte;
begin
	if length(expand) = 0 then
		begin
			mul_ss := s;
			exit(mul_ss);
		end;

	for i := 1 to length(s) do
		if s[i] = expand[1] then
			buf := concat(buf, expand)
		else
			buf := concat(buf, s[i]);

	mul_ss := buf;
end;

procedure mul(c: TAbstractCtxes; inp, outp: TStack);
var
	fst, sec: TAbstractStackItem;
begin
	sec := inp.pop;
	fst := inp.pop;
	
	if (fst is TIntegerStackItem) and (sec is TIntegerStackItem) then
		outp.push(TIntegerStackItem.Create(fst.intValue * sec.intValue))
	else if (fst is TStringStackItem) and 
		((sec is TIntegerStackItem) or (sec is TDoubleStackItem)) then
			outp.push(TStringStackItem.Create(mul_sf(fst.strValue, sec.dblValue)))
	else if ((fst is TIntegerStackItem) or (fst is TDoubleStackItem))
		and (sec is TStringStackItem) then
			outp.push(TStringStackItem.Create(mul_sf(sec.strValue, fst.dblValue)))
	else if (fst is TStringStackItem) and (sec is TStringStackItem) then
		outp.push(TStringStackItem.Create(mul_ss(fst.strValue, sec.strValue)))
	else
		outp.push(TDoubleStackItem.Create(fst.dblValue * sec.dblValue));
		
	sec.free;
	fst.free;
end;



{ division! }
{ see above - helper functions identical to non-OO ones }
function div_sf (s: string; divby: double): string;
var
	l: longint;
begin
	l := round(length(s) / divby);
	div_sf := copy(s, 1, l);
end;

function div_ss (s, expand: string): string;
var
	p: longint;
	buf: string;
begin
	buf := s;
	p := pos(expand, buf);
	while p <> 0 do
		begin
			delete(buf, p + 1, length(expand) - 1);
			p := pos(expand, buf);
		end;
	div_ss := buf;
end;

procedure div_(c: TAbstractCtxes; inp, outp: TStack);
var
	fst, sec: TAbstractStackItem;
begin
	sec := inp.pop;
	fst := inp.pop;
	
	if (fst is TIntegerStackItem) and (sec is TIntegerStackItem) then
		outp.push(TIntegerStackItem.Create(fst.intValue div sec.intValue))
	else if (fst is TStringStackItem) and 
		((sec is TIntegerStackItem) or (sec is TDoubleStackItem)) then
			outp.push(TStringStackItem.Create(div_sf(fst.strValue, sec.dblValue)))
	else if ((fst is TIntegerStackItem) or (fst is TDoubleStackItem))
		and (sec is TStringStackItem) then
			outp.push(TStringStackItem.Create(div_sf(sec.strValue, fst.dblValue)))
	else if (fst is TStringStackItem) and (sec is TStringStackItem) then
		outp.push(TStringStackItem.Create(div_ss(fst.strValue, sec.strValue)))
	else
		outp.push(TDoubleStackItem.Create(fst.dblValue / sec.dblValue));
		
	sec.free;
	fst.free;
end;



procedure n(c: TAbstractCtxes; inp, outp: TStack);
var
	item : TAbstractStackItem;
begin
	item := inp.pop;
	outp.push(TStringStackItem.Create(item.strValue + chr(10)));
	item.free;
end;



procedure _(c: TAbstractCtxes; inp, outp: TStack);
var
	item : TAbstractStackItem;
begin
	item := inp.pop;
	outp.push(TStringStackItem.Create(item.strValue + ' '));
	item.free;
end;



procedure string_(c: TAbstractCtxes; inp, outp: TStack);
var
	item: TAbstractStackItem;
begin
	item := inp.pop;
	if item is TStringStackItem then
		outp.push(TIntegerStackItem.Create(1))
	else
		outp.push(TIntegerStackItem.Create(0));
		
	item.free;
end;



procedure integer_(c: TAbstractCtxes; inp, outp: TStack);
var
	item: TAbstractStackItem;
begin
	item := inp.pop;
	if item is TIntegerStackItem then
		outp.push(TIntegerStackItem.Create(1))
	else
		outp.push(TIntegerStackItem.Create(0));
		
	item.free;
end;



procedure float_(c: TAbstractCtxes; inp, outp: TStack);
var
	item: TAbstractStackItem;
begin
	item := inp.pop;
	if item is TDoubleStackItem then
		outp.push(TIntegerStackItem.Create(1))
	else
		outp.push(TIntegerStackItem.Create(0));
		
	item.free;
end;



procedure number_(c: TAbstractCtxes; inp, outp: TStack);
var
	item: TAbstractStackItem;
begin
	item := inp.pop;
	if (item is TIntegerStackItem) or (item is TDoubleStackItem) then
		outp.push(TIntegerStackItem.Create(1))
	else
		outp.push(TIntegerStackItem.Create(0));
		
	item.free;
end;



{ equality }

function elemEQ(a, b: TAbstractStackItem) : boolean;
begin
	if (a is TStringStackItem) and (b is TStringStackItem) then
		elemEQ := a.strValue = b.strValue
	else if (a is TStringStackItem) and not (b is TStringStackItem) then
		elemEQ := false
	else if not (a is TStringStackItem) and (b is TStringStackItem) then
		elemEQ := false
	else if (a is TDoubleStackItem) or (b is TDoubleStackItem) then
		elemEQ := a.dblValue = b.dblValue
	else
		elemEQ := a.intValue = b.intValue;
end;

procedure eq_(c: TAbstractCtxes; inp, outp: TStack);
var
	a, b: TAbstractStackItem;
begin
	a := inp.pop;
	b := inp.pop;
	if elemEQ(a,b) then
		outp.push(TIntegerStackItem.Create(1))
	else
		outp.push(TIntegerStackItem.Create(0));
	
	a.free;
	b.free;
end;



procedure die(c: TAbstractCtxes; inp, outp: TStack);
begin
	raise EProgramDied.Create('Program called \die');
end;



procedure trace0(c: TAbstractCtxes; inp, outp: TStack);
begin
	traceOff;
end;



procedure trace1(c: TAbstractCtxes; inp, outp: TStack);
begin
	traceOn;
end;



{ installStdLib installs the standard library in a verb table }

procedure installStdLib(vt: TVerbTable);
begin
	vt.defineNative('out', @out_);
	vt.defineNative('in', @in_);
	vt.defineNative('exec', @exec);
	vt.defineNative('verb', @verb);
	vt.defineNative('if', @if_);
	vt.defineNative('while', @while_);
	vt.defineNative('not', @not_);
	vt.defineNative('swap', @swap);
	vt.defineNative('dup', @dup);
	vt.defineNative('rm', @rm);
	vt.defineNative('mv', @mv);
	vt.defineNative('multipop', @multipop);
	vt.defineNative('+', @plus);
	vt.defineNative('-', @minus);
	vt.defineNative('*', @mul);
	vt.defineNative('div', @div_);
	vt.defineNative('n', @n);
	vt.defineNative('_', @_);
	vt.defineNative('string?', @string_);
	vt.defineNative('integer?', @integer_);
	vt.defineNative('float?', @float_);
	vt.defineNative('number?', @number_);
	vt.defineNative('eq?', @eq_);
	vt.defineNative('trace=0', @trace0);
	vt.defineNative('trace=1', @trace1);
end;

end.