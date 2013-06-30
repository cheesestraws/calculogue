with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed; with Ada.Strings.Fixed.Hash; use Ada.Strings.Fixed;
with Ada.Containers.Vectors; with Ada.Containers.Indefinite_Hashed_Maps;
use Ada.Containers;

with Trace_Output;
with Stack_Items; use Stack_Items;
with Exceptions;
with Tokens;
with Verbs; use Verbs;

package body Interp_State is

   -- I need a vector of Ctxes.  Ctxes are limited, for good reason, so
   -- this is actually going to be a vector of Ctx /accesses/.
   -- This is a bit unsafe; in theory, the person who gets a Ctx access
   -- could free it.  In practice, I know I won't. :-p

   -- This package contains a vector of ctx access types
   package Ctx_Vectors is new Vectors(Natural, Ctx_Access);

   -- This package contains a map of String -> Verbs
   package Verb_Maps is new Indefinite_Hashed_Maps(Key_Type => String,
                                                   Element_Type => Verbs.Verb,
                                                   Hash => Ada.Strings.Fixed.Hash,
                                                   Equivalent_Keys => "=");
   use Verb_Maps;

   -- We need a free for Ctx Accesses.
   procedure Free_Ctx is new Ada.Unchecked_Deallocation(Ctx, Ctx_Access);

   -- This is the backing store for contexts
   Vec: Ctx_Vectors.Vector;

   -- This is the backing store for verbs.  It maps verb names as strings to verb
   -- definitions.
   Verb_Table : Verb_Maps.Map;

   -- This stores whether Tracing is currently enabled
   Trace : Boolean := False;

   procedure Start(Input, Output: Stack_Access) is
      New_Ctx : Ctx_Access := new Ctx;
   begin
      New_Ctx.Set_Ctx_IO(Input, Output);

      -- Do we have an existing concept where the Up_Exec should be chained
      -- through to the new context?
      if Vec.Length > 0 then
         New_Ctx.Set_Up_Exec(Vec.Last_Element.Exec);
      end if;

      Vec.Append(New_Ctx);
   end;

   procedure Stop is
      -- Grab the Ctx access
      Ct : Ctx_Access := Vec.Last_Element;
   begin
      Vec.Delete_Last;
      Free_Ctx(Ct);
   end;

   function Cur return Ctx_Access is
   begin
      return Vec.Last_Element;
   end;

   -- Running a context

   procedure Run_Verb_Token(Name: String; Input: Stack_Access; Output: Stack_Access) is
      Verb_Cursor: Verb_Maps.Cursor := Verb_Table.Find(Name);
   begin
      -- Check for null stacks!
      if Input = null then
         raise Exceptions.No_Such_Stack with "Input stack for verb " & name & " doesn't exist - " &
                                              "did you use . in the root context?";
      end if;

      if Output = null then
         raise Exceptions.No_Such_Stack with "Output stack for verb " & name & " doesn't exist - " &
                                              "did you use . in the root context?";
      end if;

      if Verb_Cursor = No_Element then
         -- There's no such verb
         raise Exceptions.No_Such_Verb with "No such verb " & Name;
      else
         -- There is
         Run_Verb(Element(Verb_Cursor), Input, Output);
      end if;
   end;


   procedure Run_Token(Tok: Tokens.Token) is
      Input, Output: Stack_Access;
   begin
      case Tok.Tok_Type is
         when Tokens.Word =>
            -- Nice and easy, word types map straight to strings
            Output := Cur.Map_Character_To_Stack(Tok.Out_Stack);
            Output.Push(SI_String(Tok.Content));

         when Tokens.Number =>
            begin
               Output := Cur.Map_Character_To_Stack(Tok.Out_Stack);
               -- Need to check whether it has a '.' in it.
               if Index(Tok.Content, ".") > 0 then
                  -- It has a '.' in it, so it is a float.
                  Output.Push(SI_Float(Float'Value(Tok.Content)));
               else
                  Output.Push(SI_Integer(Integer'Value(Tok.Content)));
               end if;

            exception
               when E: Constraint_Error =>
                  raise Exceptions.Bad_Token with Tok.Content & " is not a number.";

            end;


         when Tokens.Verb =>
            -- Map input and output stacks
            Input := Cur.Map_Character_To_Stack(Tok.In_Stack, True);
            Output := Cur.Map_Character_To_Stack(Tok.Out_Stack);

            Run_Verb_Token(Tok.Content, Input, Output);
      end case;
   end;

   procedure Run_Str(Str: String) is
   begin
      -- if we're tracing, print what it is we're evaluating
      if Tracing then
         Trace_Output.Trace_Line("Running " & Str);
      end if;

      Run_Token(Tokens.String_To_Token(Str));

      if Tracing then

         Cur.Trace;
      end if;
   end;

   procedure Run_Current is
      Ct : Ctx_Access := Cur; -- the current context
      Item : Stack_Item;
   begin
      while Ct.Exec.Length > 0 loop
         -- pop an item and check whether it's a string
         Item := Ct.Exec.Pop;
         if Kind_Of(Item) /= String_Item then
            raise Exceptions.Bad_Exec_Token with "Only string stack items can be executed.";
         end if;

         -- It is - we can try to execute it.
         Run_Str(String_Value_Of(Item));
      end loop;
   end;


   procedure Remove_Verb_If_Exists(Name: String) is
      Verb_Cursor: Verb_Maps.Cursor := Verb_Table.Find(Name);
   begin
      if Verb_Cursor /= Verb_Maps.No_Element then
         -- If the verb exists...
         declare
            v: Verb := Verb_Maps.Element(Verb_Cursor);
         begin
            -- Remove it from the verb table
            Verb_Table.Delete(Verb_Cursor);
            -- And tidy up after it.
            Verbs.Tidy_Verb(v);
         end;
      end if;
   end;


   procedure Install_Native_Verb(Name: String; Subprogram: Verb_Subprogram_Access) is
   begin
      Remove_Verb_If_Exists(Name);

      Verb_Table.Insert(Key      => Name,
                   New_Item => Native_Verb(Subprogram));
   end;

   procedure Install_TISBL_Verb(Name: String; St: Stack) is
   begin
      Remove_Verb_If_Exists(Name);

      Verb_Table.Insert(Key      => Name,
                        New_Item => TISBL_Verb(St));
   end;


   -- tracing util stuff

   procedure Trace_On is
   begin
      Trace := True;
   end;

   procedure Trace_Off is
   begin
      Trace := False;
   end;

   function Tracing return Boolean is
   begin
      return Trace;
   end;


end Interp_State;
