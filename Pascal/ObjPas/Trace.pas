{$MODE OBJFPC}
unit Trace;
interface
{ A global variable to see whether to trace stuff }
	var
		tracebool: boolean = false;

	procedure trace (s: string);
	procedure traceOn;
	procedure traceOff;
	procedure mtraceStart;
	procedure mtraceEnd;
	procedure mtrace (s: string);
	procedure dbg (s: string);
	
	type
		ITraceable = interface
			procedure printTrace;
		end;

implementation
	procedure traceOn;
	begin
		tracebool := true;
	end;

	procedure traceOff;
	begin
		tracebool := false;
	end;

	procedure trace (s: string);
	begin
		if tracebool then
		begin
			write('[trace] ');
			writeln(s);
		end;
	end;

	procedure dbg (s: string);
	begin
		if tracebool then
			trace(s);
	end;

	procedure mtraceStart;
	begin
		if tracebool then write('[trace] ');
	end;

	procedure mtraceEnd;
	begin
		if tracebool then writeln;
	end;

	procedure mtrace (s: string);
	begin
		if tracebool then begin
			write(s);
			write(' ');
		end;
	end;

end.