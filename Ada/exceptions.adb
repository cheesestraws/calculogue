with Ada.Text_IO;

-- the "Use" is so I have = - I'm still going to qualify Ada.Exceptions in the
-- source text, to clarify what I'm doing.
with Ada.Exceptions; use Ada.Exceptions;

package body Exceptions is
   procedure Print_Error_Indicator is
   begin
      Ada.Text_IO.Put("[error] ");
   end;

   function Pretty_Exception_Name(Name: String) return String is
   begin
      -- come back to this later
      return Name;
   end;

   procedure Handle_TISBL_Error_Or_Reraise(E: Ada.Exceptions.Exception_Occurrence) is
      Exception_Id: Ada.Exceptions.Exception_Id := Ada.Exceptions.Exception_Identity(E);
   begin
      -- Can't do this with a case statement; Exception_Occurrence is not
      -- a discrete type

      -- Bad type exceptions are so general, we just hope that the stdlib has
      -- filled in the message properly.
      if Exception_Id = Bad_Type_Exception'Identity then
         Print_Error_Indicator;
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(E));

      elsif Exception_Id = Program_Died'Identity then
         Ada.Text_IO.Put_Line("Program died.");

      elsif (Exception_ID = Stack_Underflow'Identity or
               Exception_ID = Bad_Exec_Token'Identity or
                 Exception_ID = Bad_Token'Identity or
                   Exception_ID = Bad_Implicit_Conversion'Identity or
                     Exception_ID = No_Such_Stack'Identity or
                       Exception_ID = No_Such_Verb'Identity or
                         Exception_ID = No_Loadable_Module_Support'Identity) then

         Print_Error_Indicator;
         Ada.Text_IO.Put(Pretty_Exception_Name(Ada.Exceptions.Exception_Name(E)) & ": ");
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(E));
      else
         Ada.Exceptions.Reraise_Occurrence(E);
      end if;
   end;

end Exceptions;
