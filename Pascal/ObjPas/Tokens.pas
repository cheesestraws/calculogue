{$MODE OBJFPC}
unit Tokens;
{ This is actually very nearly identical to the non-Object Pascal Tokens unit.     }
{ The only difference is that instead of dieing, it raises an exception.           }
{ Tokens are still records not objects - objects would be overkill here and unlike }
{ some languages, we're not obliged to use them.  Also records are nicely put      }
{ on the stack. }
interface

	type
		TokenType = (UnknownToken, VerbToken, WordToken, NumToken);
		TokenStack = (DefaultStack, SecondaryStack, ExecStack, IOStack, ParentExecStack);
		Token = record
				tokType: TokenType;
				stack1, stack2: TokenStack;		{stack1 is the stack before the token and }
												{stack2 is the stack after it.  For verbs, }
												{stack1 is the input stack, for nouns it's }
												{the output stack}
				contents: string;
			end;

	function strToTok (s: string): Token;

implementation
uses
	ExceptionUnit;

	type
		TokState = record
				str: string;
				off: integer;
			end;

	procedure tincr (var s: TokState);
	begin
		s.off := s.off + 1;
	end;

	function tchar (var s: TokState): char;
	begin
		tchar := s.str[s.off];
	end;

	function readStackName (var s: TokState): TokenStack;
		var
			c: char;
	begin
		c := tchar(s);

		if c = ':' then
			begin
				readStackName := SecondaryStack;
				tincr(s);
			end
		else if c = '.' then
			begin
				readStackName := IOStack;
				tincr(s);
			end
		else if c = ',' then
			begin
				readStackName := ExecStack;
				tincr(s);
			end
		else if c = ';' then
			begin
				readStackName := ParentExecStack;
				tincr(s);
			end
		else
			readStackName := DefaultStack;
	end;



	function readTokType (var s: TokState): TokenType;
		var
			c: char;
	begin
		c := tchar(s);
		if c = '#' then
			begin
				readTokType := NumToken;
				tincr(s);
			end
		else if c = '''' then
			begin
				readTokType := WordToken;
				tincr(s);
			end
		else
			readTokType := UnknownToken;
	end;



	function strToTok (s: string): Token;
		var
			state: TokState;
			tok: Token;
			stackChar: char;
			oldoffset: integer;
	begin
		state.off := 1;
		state.str := s;
		tok.tokType := UnknownToken;

	{ First, we need to check whether our token is a verb or a noun }
		if s[1] = '\' then
			begin
		{ It's a verb! }
				tok.tokType := VerbToken;
				state.off := state.off + 1;

		{ Read the first stack }
				tok.stack1 := readStackName(state);

		{ Do we have a last stack? }
				stackChar := state.str[length(state.str)];

				if pos(stackChar, '.,:;') > 0 then
					begin
			{There is a final stack!}
						oldoffset := state.off;
						state.off := length(state.str);
						tok.stack2 := readStackName(state);
						tok.contents := copy(state.str, oldoffset, length(state.str) - oldoffset);
					end
				else
					begin
						tok.stack2 := DefaultStack;
						tok.contents := copy(state.str, state.off, length(state.str) - state.off + 1);
					end;
			end
		else
			begin

		{ We're not a verb.  So, first, read our stack1. }
				tok.stack1 := readStackName(state);
				tok.tokType := readTokType(state);
				tok.contents := copy(state.str, state.off, length(state.str));

		{If, after this, tokType is UnknownToken, then the token is bad!}
				if (tok.tokType = UnknownToken) then
					begin
						{ We have working string concatenation here! }
						raise EBadToken.Create('Bad token ' + s);
					end;
			end;

		strToTok := tok;
	end;
end.