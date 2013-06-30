with Ada.Text_IO;

package body Trace_Output is

   procedure Trace_Line(Str: String) is
   begin
      Ada.Text_IO.Put("[trace] ");
      Ada.Text_IO.Put_Line(Str);
   end;

   procedure Trace_Start is
   begin
      Ada.Text_IO.Put("[trace] ");
   end;

   procedure Trace_Out(Str: String) is
   begin
      Ada.Text_IO.Put(Str & " ");
   end;

   procedure Trace_End is
   begin
      Ada.Text_IO.New_Line;
   end;

end Trace_Output;
