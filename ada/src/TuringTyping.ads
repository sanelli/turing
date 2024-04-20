with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package TuringTyping is
   type TTuringTapeMove is (Left, Right, None);
   subtype TTuringStatus is Unbounded_String;
   subtype TTuringSymbol is Character;
end TuringTyping;