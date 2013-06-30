with Ada.Text_IO;

with Interp_State;
with Stacks; use Stacks;
with Ctxes; use Ctxes;
with Stack_Items; use Stack_Items;
with Verbs; use Verbs;
with Exceptions;

package body Stdlib is

   -- utility function
   function Is_Effectively_False(Item: Stack_Item) return Boolean is
   begin
      return (Kind_Of(Item) = Integer_Item and then Integer_Value_Of(Item) = 0)
        or else (Kind_Of(Item) = Float_Item and then Float_Value_Of(Item) = 0.0);
   end;


   procedure Exec(Input, Output: Stack_Access) is
      Item: Stack_Item;
      Count : Integer;
   begin
      Item := Input.Pop;
      Must_Be(Item, Integer_Item, "\exec expected a number but didn't get one");
      Count := Integer_Value_Of(Item);

      -- Create new context
      Interp_State.Start(Input, Output);

      -- Copy stuff onto it
      for I in Integer range 1 .. Count loop
         Item := Input.Pop;
         Interp_State.Cur.Exec.Push(Item);
      end loop;

      -- Run the context, then kill it.
      Interp_State.Run_Current;
      Interp_State.Stop;
   end;


   procedure Verb(Input, Output: Stack_Access) is
      Name_Item, Count_Item: Stack_Item;
      St: Stack;
   begin
      Name_Item := Input.Pop;
      Must_Be(Name_Item, String_Item, "\verb wanted a name, and didn't get one");
      Count_Item := Input.Pop;
      Must_Be(Count_Item, Integer_Item, "\verb wanted a number of items, and didn't get one");

      -- now we can extract name and count
      declare
         Name : String := String_Value_Of(Name_Item);
         Count: Integer := Integer_Value_Of(Count_Item);
      begin

         for I in Integer range 1 .. Count loop
            St.Push(Input.Pop);
         end loop;
         Interp_State.Install_TISBL_Verb(Name, St);
      end;
   end;


   procedure If_Proc(Input, Output: Stack_Access) is
      Item : Stack_Item;
      Count : Integer;
   begin
      Interp_State.Start(Input, Output);

      -- get the count
      Item := Input.Pop;
      Must_Be(Item, Integer_Item, "\if wanted a count and didn't get one");
      Count := Integer_Value_Of(Item);

      for I in Integer range 1 .. Count loop
         Interp_State.Cur.Exec.Push(Input.Pop);
      end loop;

      -- get the condition
      Item := Input.Pop;
      if Is_Effectively_False(Item) then
         null; -- no-op - 0 means do not execute
      else
         Interp_State.Run_Current;
      end if;

      Interp_State.Stop;
   end;


   procedure While_Proc(Input, Output: Stack_Access) is
      St: Stack;
      Item: Stack_Item;
      Count: Integer;
      End_Loop: Boolean := False;
   begin
      -- Get the count
      Item := Input.Pop;
      Must_Be(Item, Integer_Item, "\while expected a count and didn't get one");
      Count := Integer_Value_Of(Item);

      -- copy to template stack
      for I in Integer range 1 .. Count loop
         St.Push(Input.Pop);
      end loop;

      loop
         Item := Input.Pop;
         if Is_Effectively_False(Item) then
            End_Loop := true;
         else
            -- We got a non-false value
            -- so run the stack
            Interp_State.Start(Input, Output);
            Interp_State.Cur.Exec.Clone_From(St);
            Interp_State.Run_Current;
            Interp_State.Stop;
         end if;

         exit when End_Loop;
      end loop;
   end;


   procedure Not_Proc(Input, Output: Stack_Access) is
      Item: Stack_Item := Input.Pop;
   begin
      if Is_Effectively_False(Item) then
         Output.Push(SI_Integer(1));
      else
         Output.Push(SI_Integer(0));
      end if;
   end;


   procedure Swap(Input, Output: Stack_Access) is
      Item1: Stack_Item := Input.Pop;
      Item2: Stack_Item := Input.Pop;
   begin
      Output.Push(Item1);
      Output.Push(Item2);
   end;


   procedure Dup(Input, Output: Stack_Access) is
      Item: Stack_Item := Input.Peek;
   begin
      Output.Push(Item);
   end;


   procedure Rm(Input, Output: Stack_Access) is
      Item: Stack_Item := Input.Pop;
   begin
      null;
   end;


   procedure Mv(Input, Output: Stack_Access) is
      Item: Stack_Item := Input.Pop;
   begin
      Output.Push(Item);
   end;


   procedure Multipop(Input, Output: Stack_Access) is
      Item: Stack_Item := Input.Pop;
      Count: Integer;
   begin
      -- The count needs to be an integer
      Must_Be(Item, Integer_Item, "\multipop expected a count and didn't get one");
      Count := Integer_Value_Of(Item);

      for I in Integer range 1 .. Count loop
         Output.Push(Input.Pop);
      end loop;

   end;


   procedure Plus(Input, Output: Stack_Access) is
      Sec: Stack_Item := Input.Pop;
      Fst: Stack_Item := Input.Pop;
   begin
      -- We've sneaked this functionality in at the operator level!
      -- Look how simple this looks...
      Output.Push(Fst + Sec);
   end;

   procedure Minus(Input, Output: Stack_Access) is
      Sec: Stack_Item := Input.Pop;
      Fst: Stack_Item := Input.Pop;
   begin
      Output.Push(Fst - Sec);
   end;


   procedure Mul(Input, Output: Stack_Access) is
      Sec: Stack_Item := Input.Pop;
      Fst: Stack_Item := Input.Pop;
   begin
      Output.Push(Fst * Sec);
   end;


   procedure Div(Input, Output: Stack_Access) is
      Sec: Stack_Item := Input.Pop;
      Fst: Stack_Item := Input.Pop;
   begin
      Output.Push(Fst / Sec);
   end;


   procedure N(Input, Output: Stack_Access) is
      Item: Stack_Item := Input.Pop;
   begin
      Item := SI_String(String_Value_Of(Item) & Character'Val(10));
      Output.Push(Item);
   end;


   procedure Space(Input, Output: Stack_Access) is
      Item: Stack_Item := Input.Pop;
   begin
      Item := SI_String(String_Value_Of(Item) & " ");
      Output.Push(Item);
   end;


   procedure String_Predicate(Input, Output: Stack_Access) is
      Item: Stack_Item := Input.Pop;
   begin
      if Kind_Of(Item) = String_Item then
         Output.Push(SI_Integer(1));
      else
         Output.Push(SI_Integer(0));
      end if;
   end;


   procedure Number_Predicate(Input, Output: Stack_Access) is
      Item: Stack_Item := Input.Pop;
   begin
      if Kind_Of(Item) = Float_Item or Kind_Of(Item) = Integer_Item then
         Output.Push(SI_Integer(1));
      else
         Output.Push(SI_Integer(0));
      end if;
   end;


   procedure Integer_Predicate(Input, Output: Stack_Access) is
      Item: Stack_Item := Input.Pop;
   begin
      if Kind_Of(Item) = Integer_Item then
         Output.Push(SI_Integer(1));
      else
         Output.Push(SI_Integer(0));
      end if;
   end;


   procedure Float_Predicate(Input, Output: Stack_Access) is
      Item: Stack_Item := Input.Pop;
   begin
      if Kind_Of(Item) = Float_Item then
         Output.Push(SI_Integer(1));
      else
         Output.Push(SI_Integer(0));
      end if;
   end;


   procedure Equal_Predicate(Input, Output: Stack_Access) is
      Item1: Stack_Item := Input.Pop;
      Item2: Stack_Item := Input.Pop;
   begin
      if Item1 = Item2 then
         Output.Push(SI_Integer(1));
      else
         Output.Push(SI_Integer(0));
      end if;
   end;


   procedure Die(Input, Output: Stack_Access) is
   begin
      raise Exceptions.Program_Died;
   end;


   procedure Present_Predicate(Input, Output: Stack_Access) is
      Item: Stack_Item := Input.Pop;
   begin
      Output.Push(SI_Integer(0));
   end;


   procedure Load(Input, Output: Stack_Access) is
   begin
      raise Exceptions.No_Loadable_Module_Support with "This interpreter does not support loadable modules.";
   end;


   procedure Out_Proc(Input, Output: Stack_Access) is
      Item : Stack_Item := Input.Pop;
   begin
      Ada.Text_IO.Put(String_Value_Of(Item));
   end;


   procedure In_Proc(Input, Output: Stack_Access) is
      Line: String := Ada.Text_IO.Get_Line;
   begin
      Output.Push(SI_String(Line));
   end;


   procedure Trace_0(Input, Output: Stack_Access) is
   begin
      Interp_State.Trace_Off;
   end;


   procedure Trace_1(Input, Output: Stack_Access) is
   begin
      Interp_State.Trace_On;
   end;


   procedure Install is
   begin
      Interp_State.Install_Native_Verb("exec", Exec'Access);
      Interp_State.Install_Native_Verb("verb", Verb'Access);
      Interp_State.Install_Native_Verb("if", If_Proc'Access);
      Interp_State.Install_Native_Verb("while", While_Proc'Access);
      Interp_State.Install_Native_Verb("not", Not_Proc'Access);
      Interp_State.Install_Native_Verb("swap", Swap'Access);
      Interp_State.Install_Native_Verb("dup", Dup'Access);
      Interp_State.Install_Native_Verb("rm", Rm'Access);
      Interp_State.Install_Native_Verb("mv", Mv'Access);
      Interp_State.Install_Native_Verb("multipop", Multipop'Access);
      Interp_State.Install_Native_Verb("+", Plus'Access);
      Interp_State.Install_Native_Verb("-", Minus'Access);
      Interp_State.Install_Native_Verb("*", Mul'Access);
      Interp_State.Install_Native_Verb("div", Div'Access);
      Interp_State.Install_Native_Verb("n", N'Access);
      Interp_State.Install_Native_Verb("_", Space'Access);
      Interp_State.Install_Native_Verb("string?", String_Predicate'Access);
      Interp_State.Install_Native_Verb("number?", Number_Predicate'Access);
      Interp_State.Install_Native_Verb("integer?", Integer_Predicate'Access);
      Interp_State.Install_Native_Verb("float?", Float_Predicate'Access);
      Interp_State.Install_Native_Verb("eq?", Equal_Predicate'Access);
      Interp_State.Install_Native_Verb("present?", Present_Predicate'Access);
      Interp_State.Install_Native_Verb("load", Load'Access);
      Interp_State.Install_Native_Verb("out", Out_Proc'Access);
      Interp_State.Install_Native_Verb("in", In_Proc'Access);
      Interp_State.Install_Native_Verb("die", Die'Access);
      Interp_State.Install_Native_Verb("trace=0", Trace_0'Access);
      Interp_State.Install_Native_Verb("trace=1", Trace_1'Access);
   end;

end Stdlib;
