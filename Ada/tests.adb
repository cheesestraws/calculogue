with Ada.Text_IO;

with Exceptions;
with Interp_State;
with Stack_Items; use Stack_Items;
with Stacks; use Stacks;

package body Tests is

   procedure Pass(Str: String) is
   begin
      Ada.Text_IO.Put("[PASS] ");
      Ada.Text_IO.Put_Line(Str);
   end;

   procedure Fail(Str: String) is
   begin
      Ada.Text_IO.Put("[FAIL] ");
      Ada.Text_IO.Put_Line(Str);
      raise Exceptions.Test_Failed;
   end;


   procedure Run_SI_Tests is
      Item1, Item2: Stack_Item;
   begin
      -- Check for equality working
      Item1 := SI_String("hello");
      Item2 := SI_String("hello");
      if Item1 = Item2 then
         Pass("String equality works (1).");
      else
         Fail("String equality doesn't work (1).");
      end if;

      Item1 := SI_String("hello");
      Item2 := SI_String("hellohello");
      if Item1 = Item2 then
         Fail("String equality doesn't work. (2)");
      else
         Pass("String equality works. (2)");
      end if;

      Item1 := SI_Integer(42);
      Item2 := SI_Integer(42);
      if Item1 = Item2 then
         Pass("Integer equality works. (1)");
      else
         Fail("Integer equality doesn't work. (1)");
      end if;

      Item1 := SI_Integer(42);
      Item2 := SI_Integer(43);
      if Item1 = Item2 then
         Fail("Integer equality doesn't work. (2)");
      else
         Pass("Integer equality works. (2)");
      end if;

      Item1 := SI_Float(42.0);
      Item2 := SI_Float(42.0);
      if Item1 = Item2 then
         Pass("Float equality works. (1)");
      else
         Fail("Float equality doesn't work. (1)");
      end if;

      Item1 := SI_Float(42.0);
      Item2 := SI_Float(43.0);
      if Item1 = Item2 then
         Fail("Float equality doesn't work. (2)");
      else
         Pass("Float equality works. (2)");
      end if;

      -- Test for Kind() working
      Item1 := SI_String("Hello");
      if Kind_Of(Item1) = String_Item then
         Pass("Kind works on strings.");
      else
         Fail("Kind doesn't work on strings.");
      end if;

      Item1 := SI_Integer(42);
      if Kind_Of(Item1) = Integer_Item then
         Pass("Kind works on integers.");
      else
         Fail("Kind doesn't work on integers.");
      end if;

      Item1 := SI_Float(42.0);
      if Kind_Of(Item1) = Float_Item then
         Pass("Kind works on floats.");
      else
         Fail("Kind doesn't work on floats.");
      end if;

      -- check Must_Be
      begin
         Item1 := SI_String("Hello");
         Must_Be(Item1, Integer_Item, "");
         Fail("Must_Be hasn't worked");
      exception
         when E : Exceptions.Bad_Type_Exception =>
            Pass("Must_Be worked.");
      end;
   end Run_SI_Tests;


   procedure Run_Stack_Tests is
      S1 : Stack;
      Item: Stack_Item;
   begin
      -- Test for stack underflow behaviour
      -- On pop
      begin
         Item := S1.Pop;
         Fail("Pop did not raise stack underflow");
      exception
         when E: Exceptions.Stack_Underflow =>
            Pass("Pop raised stack underflow");
      end;

      -- and on peek
      begin
         Item := S1.Peek;
         Fail("Peek did not raise stack underflow");
      exception
         when E: Exceptions.Stack_Underflow =>
            Pass("Peek raised stack underflow");
      end;

      -- check that pushing/popping works
      S1.Push(SI_Integer(1));
      S1.Push(SI_Integer(2));

      if S1.Peek = SI_Integer(2) then
         Pass("Peek is looking at the right end of the stack");
      else
         Fail("Peek is looking at the back end of the stack.");
      end if;

      if S1.Pop = SI_Integer(2) then
         Pass("Pop works as expected.");
      else
         Fail("Pop returned something weird.");
      end if;
   end Run_Stack_Tests;


   procedure Run_Ctx_Tests is
      Item: Stack_Item;
   begin
      Interp_State.Start(null, null);
      Pass("Allegedly made it past Interp_State.Start...");

      Interp_State.Cur.Exec.Sneak(SI_String("#3"));
      Interp_State.Cur.Exec.Sneak(SI_String("#4"));
      Pass("Seem to have made it past loading of stuff into Exec");
      Interp_State.Run_Current;
      Pass("Seem to have made it past Run_Current");

      if Interp_State.Cur.Pri.Pop = SI_Integer(4) then
         Pass("Things seem to be going on the stack in the right order.");
      else
         Fail("Things aren't going onto the stack in the right order, or something");
      end if;

      Item := Interp_State.Cur.Pri.Pop;

      -- Now test other kinds of stuff
      Interp_State.Cur.Exec.Sneak(SI_String("#3.5"));
      Interp_State.Cur.Exec.Sneak(SI_String(":#4.5"));
      Interp_State.Cur.Exec.Sneak(SI_String("'hello"));
      Interp_State.Cur.Exec.Sneak(SI_String(":'goodbye"));
      Interp_State.Run_Current;

      If Interp_State.Cur.Pri.Pop = SI_String("hello") then
         Pass("Parsing string nouns works");
      else
         Fail("Parsing string nouns didn't work.");
      end if;

      If Interp_State.Cur.Pri.Pop = SI_Float(3.5) then
         Pass("Parsing float nouns works");
      else
         Fail("Parsing float nouns didn't work.");
      end if;

      If Interp_State.Cur.Sec.Pop = SI_String("goodbye") then
         Pass("Parsing string nouns onto secondary stack works");
      else
         Fail("Parsing string nouns onto secondary stack didn't work.");
      end if;

      If Interp_State.Cur.Sec.Pop = SI_Float(4.5) then
         Pass("Parsing float nouns onto secondary stack works");
      else
         Fail("Parsing float nouns onto secondary stack didn't work.");
      end if;

      -- What happens if we try to run a verb?
      Interp_State.Cur.Exec.Sneak(SI_String("#3"));
      Interp_State.Cur.Exec.Sneak(SI_String("#4"));
      Interp_State.Cur.Exec.Sneak(SI_String("\+"));
      Interp_State.Run_Current;

      Item := Interp_State.Cur.Pri.Pop;

      if Item = SI_Integer(7) then
         Pass("We can invoke native verbs.");
      else
         Fail("Native verb invocation didn't go as expected: got " & String_Value_Of(Item));
      end if;

      Interp_State.Stop;
      Pass("Seem to have made it past Interp_State.Stop...");
   end Run_Ctx_Tests;


   procedure Run_Tests is
   begin
      Run_SI_Tests;
      Run_Stack_Tests;
      Run_Ctx_Tests;
   end;

end Tests;
