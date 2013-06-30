with Stacks; use Stacks;

package Ctxes is

   -- this is a terrible thing to admit, but:
   -- I'm using a tagged type here /purely/ for .-notation
   type Ctx is tagged limited private;
   type Ctx_Access is access Ctx;

   function Pri(Ct: Ctx)     return Stack_Access;
   function Sec(Ct: Ctx)     return Stack_Access;
   function Exec(Ct: Ctx)    return Stack_Access;
   function Input(Ct: Ctx)   return Stack_Access;
   function Output(Ct: Ctx)  return Stack_Access;
   function Up_Exec(Ct: Ctx) return Stack_Access;

   procedure Set_Ctx_IO(Ct: in out Ctx; Input: Stack_Access; Output: Stack_Access);
   procedure Set_Up_Exec(Ct: in out Ctx; Up_Exec: Stack_Access);

   function Map_Character_To_Stack(Ct: Ctx; Char: Character;
                                   Input_Stack: Boolean := False) return Stack_Access;

   procedure Trace(Ct: Ctx);

private

   type Ctx is tagged limited record
      Primary: aliased Stack;
      Secondary: aliased Stack;
      Execution: aliased Stack;
      Input: Stack_Access;
      Output: Stack_Access;
      Up_Exec: Stack_Access;
   end record;

end Ctxes;
