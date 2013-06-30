with Ctxes; use Ctxes;
with Stacks; use Stacks;
with Verbs;

package Interp_State is

   -- the interpreter state is effectively a singleton in this version
   -- as I don't expect to be embedding this in anything else.

   procedure Start(Input, Output: Stack_Access);
   procedure Stop;
   function Cur return Ctx_Access;

   -- We'll roll Run_Current into this package here, because the routine
   -- needs access to the current verb table, and doesn't need visibility
   -- into the innards of Ctx at all.  Also :- the facility to run Ctxes
   -- other than the current one isn't actually particularly useful.
   procedure Run_Current;

   -- Install a native verb
   procedure Install_Native_Verb(Name: String; Subprogram: Verbs.Verb_Subprogram_Access);
   procedure Install_TISBL_Verb(Name: String; St: Stack);

   -- utility subprograms for tracing
   procedure Trace_On;
   procedure Trace_Off;
   function Tracing return Boolean;

end Interp_State;
