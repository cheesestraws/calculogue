package body Ctxes is

   function Pri(Ct: Ctx)     return Stack_Access is
   begin
      return Ct.Primary'Unchecked_Access; -- dangerous!
   end;

   function Sec(Ct: Ctx)     return Stack_Access is
   begin
      return Ct.Secondary'Unchecked_Access;
   end;

   function Exec(Ct: Ctx)    return Stack_Access is
   begin
      return Ct.Execution'Unchecked_Access;
   end;

   function Input(Ct: Ctx)   return Stack_Access is
   begin
      return Ct.Input;
   end;

   function Output(Ct: Ctx)  return Stack_Access is
   begin
      return Ct.Output;
   end;

   function Up_Exec(Ct: Ctx) return Stack_Access is
   begin
      return Ct.Up_Exec;
   end;

   procedure Set_Ctx_IO(Ct: in out Ctx; Input: Stack_Access; Output: Stack_Access) is
   begin
      Ct.Input := Input;
      Ct.Output := Output;
   end;

   procedure Set_Up_Exec(Ct: in out Ctx; Up_Exec: Stack_Access) is
   begin
      Ct.Up_Exec := Up_Exec;
   end;

   function Map_Character_To_Stack(Ct: Ctx; Char: Character;
                                   Input_Stack: Boolean := False) return Stack_Access is
   begin
      case Char is
         when ':' => return Ct.Sec;
         when ',' => return Ct.Exec;
         when ';' => return Ct.Up_Exec;
         when '.' =>
            if Input_Stack then
               return Ct.Input;
            else
               return Ct.Output;
            end if;
         when others =>
            return Ct.Pri;
      end case;
   end;

   procedure Trace(Ct: Ctx) is
   begin
      Ct.Primary.Trace("PRI: ");
      Ct.Secondary.Trace("SEC: ");
      Ct.Execution.Trace("EXEC:");
   end;

end Ctxes;
