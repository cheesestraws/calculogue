with Ada.Strings.Maps; use Ada.Strings.Maps;
with Exceptions;

package body Tokens is

   Stack_Chars : Character_Set := To_Set(".,:;");

   function Parse_Verb(S: String) return Token is
      First, Last : Character;
      Offset_Start, Offset_End : Natural;
   begin
      Offset_Start := S'First;
      Offset_End := S'Last;

      if Is_In(S(S'First), Stack_Chars) then
         First := S(S'First);
         Offset_Start := OffSet_Start + 1;
      else
         First := ' ';
      end if;

      if Is_In(S(S'Last), Stack_Chars) then
         Last := S(S'Last);
         Offset_End := Offset_End - 1;
      else
         Last := ' ';
      end if;

      return (Fst => Offset_Start, Lst => Offset_End,
              In_Stack => First, Out_Stack => Last,
              Content => S(Offset_Start .. Offset_End),
              Tok_Type => Verb);
   end;



   function Parse_Noun(Out_Stack: Character; S: String) return Token is
      Tok_Type: Token_Type;
   begin
      -- Check we have a valid token type
      if S(S'First) = '#' then
         Tok_Type := Number;
      elsif S(S'First) = ''' then
         Tok_Type := Word;
      else
         raise Exceptions.Bad_Token with "Bad token: " & Out_Stack & S;
      end if;

      -- And return a token
      return (Fst => S'First + 1, Lst => S'Last,
              In_Stack => ' ', Out_Stack => Out_Stack,
              Content => S(S'First + 1 .. S'Last),
              Tok_Type => Tok_Type);
   end;



   function String_To_Token(S: String) return Token is
   begin
      if S'Length = 0 then
         raise Exceptions.Bad_Token with "Bad token: (empty token)";
      elsif S(S'First) = '\' then
         -- it is a verb
         return Parse_Verb(S(S'First + 1 .. S'Last));
      else
         if Is_In(S(S'First), Stack_Chars) then
            return Parse_Noun(S(S'First), S(S'First + 1 .. S'Last));
         else
            return Parse_Noun(' ', S);
         end if;
      end if;
   end;




end Tokens;
