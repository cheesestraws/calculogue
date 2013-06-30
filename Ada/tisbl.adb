with Ada.Text_IO;
with Ada.Exceptions; with Exceptions;
with Ada.IO_Exceptions;
with Ada.Command_Line;

with Interp_State;
with Tests;
with Stdlib;
with Stack_Items;
with State_Machine;

procedure tisbl is

   -- This is the callback that the state machine will call
   procedure Receive_Token(Tok: String) is
   begin
      Interp_State.Cur.Exec.Sneak(Stack_Items.SI_String(Tok));
   end;

   -- This is the state machine
   package SM is new State_Machine(Receive_Token);

   procedure Repl is
   begin
      Ada.Text_IO.Put_Line("TISBL-Ada 1.0");
      Interp_State.Start(null, null);
      Ada.Text_IO.Put("> ");
      loop

         declare
            S : String := Ada.Text_IO.Get_Line;
         begin
            SM.Swallow_Line(S);
            Interp_State.Run_Current;
         exception
            when E: others =>
               -- This is a bit naughty, but I need a shared exception handler here!
               Exceptions.Handle_TISBL_Error_Or_Reraise(E);
         end;
         Ada.Text_IO.Put("> ");
      end loop;
   exception
      when E: Ada.IO_Exceptions.End_Error =>
         -- got an EOF.
         Ada.Text_IO.Put_Line("Bye!");
   end;

   procedure Run_Command_line is
   begin
      Interp_State.Start(null, null);
      for Idx in Integer range 2 .. Ada.Command_Line.Argument_Count loop
         SM.Swallow_Line(Ada.Command_Line.Argument(Idx));
      end loop;
      Interp_State.Run_Current;
      Interp_State.Stop;
   exception
      when E: others =>
         Exceptions.Handle_TISBL_Error_Or_Reraise(E);
   end;

   procedure Run_File(Name: String) is
      Source_File: Ada.Text_IO.File_Type;
   begin
      ADa.Text_IO.Open(Source_File, Ada.Text_IO.In_File,
                       Name);
      Interp_State.Start(null, null);

      while not Ada.Text_IO.End_Of_File(Source_File) loop
         SM.Swallow_Line(Ada.Text_IO.Get_Line(Source_File));
      end loop;

      Ada.Text_IO.Close(Source_File);

      Interp_State.Run_Current;
      Interp_State.Stop;
   exception
      when E: others =>
         Exceptions.Handle_TISBL_Error_Or_Reraise(E);
   end;

   procedure Help is
      function Get_TISBL_Executable return String is
      begin
         if Ada.Command_Line.Command_Name = "" then
            return "tisbl";
         else
            return Ada.Command_Line.Command_Name;
         end if;
      end;

      Cmd : String := Get_TISBL_Executable;
   begin
      Ada.Text_IO.Put_Line("Usage:");
      Ada.Text_IO.Put_Line("  " & Cmd & " -t             -- run tests");
      Ada.Text_IO.Put_Line("  " & Cmd & " -r             -- run a repl");
      Ada.Text_IO.Put_Line("  " & Cmd & " -e <toks>      -- run <toks>");
      Ada.Text_IO.Put_Line("  " & Cmd & " -f <filename>  -- run a file");
   end;

begin
   Stdlib.Install;

   if Ada.Command_Line.Argument_Count > 0 and then Ada.Command_Line.Argument(1) = "-t" then
      Tests.Run_Tests;
   elsif Ada.Command_Line.Argument_Count > 0 and then Ada.Command_Line.Argument(1) = "-r" then
      Repl;
   elsif Ada.Command_Line.Argument_Count > 0 and then Ada.Command_Line.Argument(1) = "-e" then
      Run_Command_Line;
   elsif Ada.Command_Line.Argument_Count > 1 and then Ada.Command_Line.Argument(1) = "-f" then
      Run_File(Ada.Command_Line.Argument(2));
   else
      Help;
   end if;

end tisbl;
