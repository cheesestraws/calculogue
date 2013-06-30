with Ada.Finalization;
with Ada.Containers.Vectors; use Ada.Containers;

with Stack_Items;  use Stack_Items;

package Stacks is

   package SI_Vectors is new Vectors(Natural, Stack_Item);

   type Stack is new Ada.Finalization.Limited_Controlled with private;
   type Stack_Access is access constant Stack;
   type Allocated_Stack_Access is access Stack; -- this is only for dynamically-allocated stacks
   -- which we should have nearly none in except in the innards of the verb table.

   function Pop(St: Stack) return Stack_Item;
   function Peek(St: Stack) return Stack_Item;
   procedure Push(St: Stack; Item: Stack_Item);
   procedure Sneak(St: Stack; Item: Stack_Item);
   procedure Clone_From(St: Stack; Src: Stack);
   function Length(St: Stack) return Natural;
   procedure Trace(St: Stack; Name: String);

private

   type SI_Vector_Access is access SI_Vectors.Vector;
   type Stack is new Ada.Finalization.Limited_Controlled with record
      Vec: SI_Vector_Access;
   end record;

   overriding procedure Initialize(St: in out Stack);
   overriding procedure Finalize(St: in out Stack);


end Stacks;
