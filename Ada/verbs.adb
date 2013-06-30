with Ada.Unchecked_Deallocation; -- ugh

with Interp_State;
with Exceptions;

package body Verbs is

   procedure Run_Verb(v: Verb; Input: Stack_Access; Output: Stack_Access) is
   begin
      case v.Kind is
         when Native =>
            v.Subprogram(Input, Output);
         when TISBL =>
            -- check to see if St exists
            if v.St = null then
               raise Exceptions.Interpreter_Bug with "Tried to execute a TISBL verb with a null stack!";
            end if;
            -- create a new context
            Interp_State.Start(Input, Output);
            -- copy from the template execution stack
            Interp_State.Cur.Exec.Clone_From(v.St.all);
            Interp_State.Run_Current;
            Interp_State.Stop;
      end case;
   end;

   function Native_Verb(Subprogram: not null Verb_Subprogram_Access) return Verb is
   begin
      return (Kind => Native, Subprogram => Subprogram);
   end;

   function TISBL_Verb(St: Stack) return Verb is
      SA : Allocated_Stack_Access := new Stack;
   begin
      SA.Clone_From(St);
      return (Kind => TISBL, St => SA);
   end;

   -- Verb is invalid after calling this!
   procedure Tidy_Verb(Vb: in out Verb) is
      procedure Free_Stack is new Ada.Unchecked_Deallocation(Stack, Allocated_Stack_Access);
   begin
      if Vb.Kind = TISBL then
         Free_Stack(Vb.St);
      end if;
   end;


end Verbs;
