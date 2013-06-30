with Stacks; use Stacks;

package Verbs is

   type Verb_Subprogram_Access is access procedure (Input: Stack_Access; Output: Stack_Access);
   type Verb_Type is (Native, TISBL);
   type Verb(Kind: Verb_Type) is private;

   procedure Run_Verb(v: Verb; Input: Stack_Access; Output: Stack_Access);
   function Native_Verb(Subprogram: not null Verb_Subprogram_Access) return Verb;
   function TISBL_Verb(St: Stack) return Verb;
   procedure Tidy_Verb(Vb: in out Verb);

private

   type Verb(Kind: Verb_Type) is record
      case Kind is
         when Native =>
            Subprogram: not null Verb_Subprogram_Access;
         when TISBL =>
            St: Allocated_Stack_Access; -- this can't be not null, as we need to clean it up
      end case;
   end record;

end Verbs;
