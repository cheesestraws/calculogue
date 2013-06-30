with Ada.Unchecked_Deallocation;

with Exceptions;
with Trace_Output;

package body Stacks is

   procedure Free_SI_Vector is new Ada.Unchecked_Deallocation(SI_Vectors.Vector, SI_Vector_Access);

   -- These are Ada.Limited_Controlled subprograms that I'm overriding
   -- This one is called when a new Stack is created;
   overriding procedure Initialize(St: in out Stack) is
   begin
      St.Vec := new SI_Vectors.Vector;
   end;

   -- This is called when a stack dies :-(
   overriding procedure Finalize(St: in out Stack) is
   begin
      Free_SI_Vector(St.Vec);
   end;


   -- Pop an item from a stack
   function Pop(St: Stack) return Stack_Item is
      Item : Stack_Item;
   begin
      if St.Vec.Length = 0 then
         raise Exceptions.Stack_Underflow with "Stack unexpectedly empty.";
      end if;

      Item := St.Vec.Last_Element;
      St.Vec.Delete_Last;
      return Item;
   end;


   -- Peek at an item from a stack
   function Peek(St: Stack) return Stack_Item is
      Item : Stack_Item;
   begin
      if St.Vec.Length = 0 then
         raise Exceptions.Stack_Underflow with "Stack unexpectedly empty.";
      end if;

      Item := St.Vec.Last_Element;
      return Item;
   end;


   -- Push an item onto the top of the stack
   procedure Push(St: Stack; Item: Stack_Item) is
   begin
      St.Vec.Append(Item);
   end;
   pragma Inline(Push);


   -- sneak an item onto the bottom of a stack
   procedure Sneak(St: Stack; Item: Stack_Item) is
   begin
      St.Vec.Prepend(Item);
   end;
   pragma Inline(Sneak);


   -- Clone a stack into another
   procedure Clone_From(St: Stack; Src: Stack) is
   begin
      St.Vec.all := Src.Vec.all;
   end;

   -- Find how many items are in a stack
   function Length(St: Stack) return Natural is
   begin
      return Natural(St.Vec.Length);
   end;

   procedure Trace(St: Stack; Name: String) is
   begin
      Trace_Output.Trace_Start;
      Trace_Output.Trace_Out(Name & ": TOP =>");
      for Idx in reverse Integer range St.Vec.First_Index .. St.Vec.Last_Index loop
         Trace_Output.Trace_Out(Trace_Value_Of(St.Vec.Element(Idx)));
      end loop;
      Trace_Output.Trace_End;
   end;

end Stacks;
