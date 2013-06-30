generic
   with procedure Receive_Token(Tok: String);
package State_Machine is

   type Machine_State is (White_Space, Token, Comment);

   procedure Clear;
   procedure Swallow(c: Character);
   procedure Swallow_Line(Line: String);

end State_Machine;
