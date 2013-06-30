with Exceptions;
with Ada.Strings.Maps;

with Trace_Output;

package body Stack_Items is

   function Kind_Of(Item: Stack_Item) return Item_Kind is
   begin
      return Item.Kind;
   end;
   pragma Inline (Kind_Of);

   procedure Must_Be(Item: Stack_Item; Kind: Item_Kind; Msg : String) is
   begin
      if Item.Kind /= Kind then
         raise Exceptions.Bad_Type_Exception with Msg;
      end if;
   end;

   function String_Value_Of(Item: Stack_Item)  return String is
      -- The Ada 2005 RM (section 3.5 paragraph 27.4/2 and 27.6/2) says that
      -- the image of both floating-point and integers has a leading space
      -- if it does not have a -.  The TISBL spec says that the string version
      -- of an integer does /not/ have this space (though this may only be in
      -- the description of \out...).  Thus, we need a replacement Image
      -- function for both to remove the space.
      function Image_Of_Integer(I: Integer) return String is
         St: String := Integer'Image(I);
      begin
         if St'Length > 1 and then St(St'First) = ' ' then
            -- it starts with a space
            return St(St'First+1 .. St'Last);
         else
            return St;
         end if;
      end;

      function Image_Of_Float(F: Float) return String is
         St: String := Float'Image(F);
      begin
         if St'Length > 1 and then St(St'First) = ' ' then
            -- it starts with a space
            return St(St'First+1 .. St'Last);
         else
            return St;
         end if;
      end;

   begin
      case Item.Kind is
         when String_Item =>
            return To_String(Item.String_Value);
         when Integer_Item =>
            return Image_Of_Integer(Item.Integer_Value);
         when Float_Item =>
            return Image_Of_Float(Item.Float_Value);
      end case;
   end;

   function Integer_Value_Of(Item: Stack_Item) return Integer is
   begin
      case Item.Kind is
         when String_Item =>
            return Integer'Value(To_String(Item.String_Value));
         when Integer_Item =>
            return Item.Integer_Value;
         when Float_Item =>
            return Integer(Float'Rounding(Item.Float_Value));
      end case;
   exception
      when E: Constraint_Error =>
         raise Exceptions.Bad_Implicit_Conversion with String_Value_Of(Item) & " cannot be used as an integer";
   end;

   function Float_Value_Of(Item: Stack_Item)   return Float is
   begin
      case Item.Kind is
         when String_Item =>
            return Float'Value(To_String(Item.String_Value));
         when Integer_Item =>
            return Float(Item.Integer_Value);
         when Float_Item =>
            return Item.Float_Value;
      end case;
   exception
      when E: Constraint_Error =>
         raise Exceptions.Bad_Implicit_Conversion with String_Value_Of(Item) & " cannot be used as an integer";
   end;

   -- returns a string for use in trace output for the stack_item
   function Trace_Value_Of(Item: Stack_Item)   return String is
      function Trace_Identifier(Item: Stack_Item) return String is
      begin
         case Item.Kind is
            when String_Item =>
               return "str.";
            when Integer_Item =>
               return "int.";
            when Float_Item =>
               return "flt.";
         end case;
      end;
   begin
      return Trace_Identifier(Item) & String_Value_Of(Item);
   end;


   -- Some constructors for stack items

   function SI_String(Str: String)   return Stack_Item is
   begin
      return (Kind => String_Item, String_Value => To_Unbounded_String(Str));
   end;

   function SI_Integer(Int: Integer) return Stack_Item is
   begin
      return (Kind => Integer_Item, Integer_Value => Int);
   end;

   function SI_Float(Flt: Float)     return Stack_Item is
   begin
      return (Kind => Float_Item, Float_Value => Flt);
   end;

   -- Operators
   -- These implement bits of the standard library.  I'm doing it here because
   -- I guess it might be useful? ;-)  Seriously, these are the operations that
   -- the spec defines on stack items, and this is where they're
   -- going.

   function "+"(Left, Right: Stack_Item) return Stack_Item is
   begin
      if Kind_Of(Left) = String_Item or Kind_Of(Right) = String_Item then
         return SI_String(String_Value_Of(Left) &
                          String_Value_Of(Right));
      elsif Kind_Of(Left) = Float_Item or Kind_Of(Right) = Float_Item then
         return SI_Float(Float_Value_Of(Left) +
                           Float_Value_Of(Right));
      else
         -- Both are integershere
         return SI_Integer(Integer_Value_Of(Left) +
                             Integer_Value_Of(Right));
      end if;
   end;

   function "-"(Left, Right: Stack_Item) return Stack_Item is
      -- helper functions
      function Sub_SI(Str: String; Len: Integer) return String is
         End_Index: Integer := Str'Last - Len;
      begin
         return Str(Str'First .. End_Index);
      end;

      function Sub_SS(Haystack, Needle: String) return String is
         Set_To_Remove: Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set(Needle);
         Working_Space: Unbounded_String := To_Unbounded_String(Haystack);
         Char_Index: Integer;
      begin
         loop
            Char_Index := Index(Working_Space, Set_To_Remove);
            exit when Char_Index = 0;

            Delete(Working_Space, Char_Index, Char_Index);
         end loop;
         return To_String(Working_Space);
      end;

   begin
      if Kind_Of(Left) = Integer_Item and Kind_Of(Right) = Integer_Item then
         return SI_Integer(Integer_Value_Of(Left) - Integer_Value_Of(Right));

      elsif Kind_Of(Left) = String_Item and (Kind_Of(Right) = Integer_Item or
                                             Kind_Of(Right) = Float_Item) then
         return SI_String(Sub_SI(String_Value_Of(Left), Integer_Value_Of(Right)));

      elsif (Kind_Of(Left) = Integer_Item or Kind_Of(Left) = Float_Item)
        and Kind_Of(Right) = String_Item then
         return SI_String(Sub_SI(String_Value_Of(Right), Integer_Value_Of(Left)));

      elsif Kind_Of(Left) = String_Item and Kind_Of(Right) = String_Item then
         return SI_String(Sub_SS(String_Value_Of(Left), String_Value_Of(Right)));

      else
         return SI_Float(Float_Value_Of(Left) - Float_Value_Of(Right));
      end if;
   end;


   function "*"(Left, Right: Stack_Item) return Stack_Item is
      -- helper functions
      function Mul_SF(Str: String; Count: Float) return String is
         Desired_Chars: Integer;
      begin
         -- let's recurse.  Why not.
         if Count = 1.0 then
            return Str;
         elsif Count > 0.0 and Count < 1.0 then
            -- a subchunk of the string
            if Count > 1.0/Float(Str'Length) then
               Desired_Chars := Integer(Float'Rounding(Float(Str'Length) * Count));
               return Str(Str'First .. Str'First + Desired_Chars);
            else
               return "";
            end if;
         elsif Count < 0.0 then
            return "";
         else
            return Str & Mul_SF(Str, Count - 1.0);
         end if;
      end;

      function Mul_SS(Haystack, Expand: String) return String is
         First_Char: Character := Expand(Expand'First);
         Working_Space: Unbounded_String := To_Unbounded_String("");
      begin
         for I in Integer range Haystack'First .. Haystack'Last loop
            if Haystack(I) = First_Char then
               Working_Space := Working_Space & Expand;
            else
               Working_Space := Working_Space & Haystack(I);
            end if;
         end loop;
         return To_String(Working_Space);
      end;

   begin
      if Kind_Of(Left) = Integer_Item and Kind_Of(Right) = Integer_Item then
         return SI_Integer(Integer_Value_Of(Left) * Integer_Value_Of(Right));

      elsif Kind_Of(Left) = String_Item and (Kind_Of(Right) = Integer_Item or
                                             Kind_Of(Right) = Float_Item) then
         return SI_String(Mul_SF(String_Value_Of(Left), Float_Value_Of(Right)));

      elsif (Kind_Of(Left) = Integer_Item or Kind_Of(Left) = Float_Item)
        and Kind_Of(Right) = String_Item then
         return SI_String(Mul_SF(String_Value_Of(Right), Float_Value_Of(Left)));

      elsif Kind_Of(Left) = String_Item and Kind_Of(Right) = String_Item then
         return SI_String(Mul_SS(String_Value_Of(Left), String_Value_Of(Right)));

      else
         return SI_Float(Float_Value_Of(Left) * Float_Value_Of(Right));
      end if;
   end;


   function "/"(Left, Right: Stack_Item) return Stack_Item is
      -- helper functions
      function Div_SF(Str: String; Count: Float) return String is
         Desired_Chars: Integer;
      begin
         if Count <= 1.0 then
            return Str;
         else
            Desired_Chars := Integer(Float'Rounding(Float(Str'Length) / Count));
            Trace_Output.Trace_Line("Debug: " & Integer'Image(Desired_Chars));
            return Str(Str'First .. Str'First + Desired_Chars - 1);
         end if;
      end;

      function Div_SS(Haystack, Needle: String) return String is
         Working_Space: Unbounded_String := To_Unbounded_String(Haystack);
         Needle_Index: Integer;
      begin
         loop
            Needle_Index := Index(Working_Space, Needle);
            exit when Needle_Index = 0;

            Delete(Working_Space, Needle_Index + 1, Needle_Index + Needle'Length - 1);
         end loop;
         return To_String(Working_Space);
      end;

   begin
      if Kind_Of(Left) = Integer_Item and Kind_Of(Right) = Integer_Item then
         return SI_Integer(Integer_Value_Of(Left) / Integer_Value_Of(Right));

      elsif Kind_Of(Left) = String_Item and (Kind_Of(Right) = Integer_Item or
                                             Kind_Of(Right) = Float_Item) then
         return SI_String(Div_SF(String_Value_Of(Left), Float_Value_Of(Right)));

      elsif (Kind_Of(Left) = Integer_Item or Kind_Of(Left) = Float_Item)
        and Kind_Of(Right) = String_Item then
         return SI_String(Div_SF(String_Value_Of(Right), Float_Value_Of(Left)));

      elsif Kind_Of(Left) = String_Item and Kind_Of(Right) = String_Item then
         return SI_String(Div_SS(String_Value_Of(Left), String_Value_Of(Right)));

      else
         return SI_Float(Float_Value_Of(Left) / Float_Value_Of(Right));
      end if;
   end;


   function "="(Left, Right: Stack_Item) return Boolean is
   begin
      if Kind_Of(Left) = String_Item and Kind_Of(Right) = String_Item then
          return String_Value_Of(Left) = String_Value_Of(Right);

      elsif Kind_Of(Left) = String_Item and Kind_Of(Right) /= String_Item then
         return False;

      elsif Kind_Of(Left) /= String_Item and Kind_Of(Right) = String_Item then
         return False;

      elsif Kind_Of(Left) = Float_Item or Kind_Of(Right) = Float_Item then
         return Float_Value_Of(Left) = Float_Value_Of(Right);

      elsif Integer_Value_Of(Left) = Integer_Value_Of(Right) then
         return True;

      else
         return False;
      end if;
   end;

end Stack_Items;
