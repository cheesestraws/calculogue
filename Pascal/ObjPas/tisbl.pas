{$MODE OBJFPC}
program test;
   uses
     classes, ExceptionUnit, Trace, Stack, Streamer, tests, stdlib, repl;
	 
const
	testing = false;
	
procedure help;
begin
writeln('Usage:');
writeln('  ' + ParamStr(0) + ' -r              Start a REPL');
writeln('  ' + ParamStr(0) + ' -e [toks]       Execute toks');
writeln('  ' + ParamStr(0) + ' -f [filename]   Run filename');
end;
	
procedure runfile(filename : string);
var
	s : TStreamer;
	f : TFileStream;
begin
	f := TFileStream.Create(filename, fmOpenRead);
	s := TStreamer.Create;
	installStdlib(s.state.verbs);
	try
		s.runstream(f);
	except
		on e: ETISBLException do
			writeln('[ERROR] ' + e.message);
	end;
	s.free;
	f.free;
end;

procedure runstring(str : string);
var
	s : TStreamer;
	stream: TStringStream;
begin
	stream := TStringStream.Create(str);
	s := TStreamer.Create;
	installStdlib(s.state.verbs);
	try
		s.runstream(stream);
	except
		on e: ETISBLException do
			writeln('[ERROR] ' + e.message);
	end;
	s.free;
	stream.free;
end;

procedure runparams;
var
	i : longint;
	buf : string = '';
begin
	for i := 2 to ParamCount do
		buf := buf + ' ' + ParamStr(i);
	
	runstring(buf);
end;


begin
	if testing then
	begin
		TraceOn;
		runtests;
	end;
	
	{$B-} { short-circuit boolean operators }
	if (ParamCount = 1) and (ParamStr(1) = '-r') then
		repl.repl
	else if (ParamCount > 1) and (ParamStr(1) = '-e') then
		runparams
	else if (ParamCount = 2) and (ParamStr(1) = '-f') then
		runfile(ParamStr(2))
	else
		help;
	
end.
