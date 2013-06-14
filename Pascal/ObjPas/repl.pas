{$MODE OBJFPC}
unit repl;
interface

{ This unit implements a rudimentary repl for TISBL }

procedure repl;

implementation
uses
	ExceptionUnit, classes, Streamer, VerbTable, stdlib;

procedure replhelp;
begin
	writeln('TISBL REPL');
	writeln('==========');
	writeln;
	writeln(':q - quits the repl');
	writeln(':h - display this message');
end;

procedure repl;
var
	line : string;
	quit : boolean = false;
	interp : TStreamer;
	stream : TStringStream;
begin
	writeln('ObjPas-TISBL v. 1.0');
	
	{ Create the Streamer }
	interp := TStreamer.Create;
	installStdLib(interp.state.verbs);
	
	writeln('');
	repeat
		write('> ');
		readln(line);
		if (line = 'help') or (line = '?') or (line = ':h') then
			replhelp
		else if line = ':q' then
			quit := true
		else
		begin
			stream := TStringStream.Create(line);
			try
				interp.runStream(stream);
			except
				on e: ETISBLException do
					writeln('[ERROR] ' + e.message);
			end;
			stream.free;
		end;
	until quit;
	
	interp.free;
end;

end.