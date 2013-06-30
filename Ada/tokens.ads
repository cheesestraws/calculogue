package Tokens is

   type Token_Type is (Word, Number, Verb);

   type Token(Fst, Lst : Positive) is record
      In_Stack, Out_Stack: Character;
      Tok_Type: Token_Type;
      Content: String(Fst .. Lst);
   end record;

   function String_To_Token(S: String) return Token;

end Tokens;
