with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Stack_Items is

   type Item_Kind is (String_Item, Float_Item, Integer_Item);
   type Stack_Item is private;

   -- Return the kind of stack this is
   function Kind_Of(Item: Stack_Item) return Item_Kind;


   procedure Must_Be(Item: Stack_Item; Kind: Item_Kind; Msg : String);

   function String_Value_Of(Item: Stack_Item)  return String;
   function Integer_Value_Of(Item: Stack_Item) return Integer;
   function Float_Value_Of(Item: Stack_Item)   return Float;
   function Trace_Value_Of(Item: Stack_Item)   return String;

   function SI_String(Str: String)   return Stack_Item;
   function SI_Integer(Int: Integer) return Stack_Item;
   function SI_Float(Flt: Float)     return Stack_Item;

   -- Operators over Stack_Items
   function "+"(Left, Right: Stack_Item) return Stack_Item;
   function "-"(Left, Right: Stack_Item) return Stack_Item;
   function "*"(Left, Right: Stack_Item) return Stack_Item;
   function "/"(Left, Right: Stack_Item) return Stack_Item;
   function "="(Left, Right: Stack_Item) return Boolean;

private

   type Stack_Item(Kind: Item_Kind := Integer_Item) is record
      case Kind is
         when Integer_Item =>
            Integer_Value: Integer;
         when Float_Item =>
            Float_Value : Float;
         when String_Item =>
            String_Value: Unbounded_String;
      end case;
   end record;

end Stack_Items;
