with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body State_Machine is

   Buffer : Unbounded_String;
   State  : Machine_State := White_Space;

   procedure Clear is
   begin
      Buffer := To_Unbounded_String("");
      State  := White_Space;
   end;

   procedure Swallow(c: Character) is
      Str: String(1..1);
   begin
      case State is
         when White_Space =>
            if c = '%' then
               State := Comment;
            elsif Is_Control(c) or c = ' ' then
               State := White_Space;
            else
               Str(1) := c;
               Buffer := To_Unbounded_String(Str);
               State := Token;
            end if;

         when Token =>
            if Is_Control(c) or c = ' ' then
               State := White_Space;
               -- we don't need to clear the buffer, as it's done in the
               -- White_Space -> Token transition
               Receive_Token(To_String(Buffer));
            else
               Buffer := Buffer & c;
            end if;

         when Comment =>
            -- just go until newline
            if c = Character'Val(13) or c = Character'Val(10) then
               State := White_Space;
            end if;
      end case;
   end;

   procedure Swallow_Line(Line: String) is
   begin
      for Idx in Integer range Line'First .. Line'Last loop
         Swallow(Line(Idx));
      end loop;
      Swallow(Character'Val(13));
   end;

end State_Machine;
