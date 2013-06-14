{$MODE OBJFPC}
unit ExceptionUnit;
{ Possibly this violates good encapsulation, but as this is }
{ a fairly small project, it makes sense to keep all the    }
{ exceptions in one place. }

interface
uses
	SysUtils;
	
type
	ETISBLException = class(Exception);

	ETestFailed     = class(ETISBLException);
	EStackUnderflow = class(ETISBLException);
	ECtxUnderflow   = class(ETISBLException);
	EBadToken       = class(ETISBLException);
	EBadExecItem    = class(ETISBLException);
	EBadVerb        = class(ETISBLException);
	EBadParameter   = class(ETISBLException);
	EProgramDied    = class(ETISBLException);

implementation
	
end.