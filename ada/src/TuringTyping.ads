with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;

package TuringTyping is
   type TTuringTapeMove is (Left, Right, None);
   subtype TTuringStatus is Unbounded_String;
   subtype TTuringSymbol is Character;

   package TTuringStatusesSets is new
     Ada.Containers.Ordered_Sets
       (Element_Type => TTuringStatus);

   package TTuringSymbolsSets is new
     Ada.Containers.Ordered_Sets
       (Element_Type => TTuringSymbol);

   function FromString (str : Unbounded_String) return TTuringTapeMove;

end TuringTyping;