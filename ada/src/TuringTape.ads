with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package TuringTape is

   package UnlimitedTape is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Character);

   type TTuringTape is tagged record
      EmptySymbol    : Character;
      PositiveTape   : UnlimitedTape.Vector;
      NegativeTape   : UnlimitedTape.Vector;
      CursorPosition : Integer;
   end record;

   type TTuringTapeMove is (Left, Right, None);

   procedure Create
      (turingTape : in out TTuringTape; emptySymbol : Character);

   procedure SetSymbol
      (turingTape : in out TTuringTape; symbol : Character);

   function GetSymbol
      (turingTape : in out TTuringTape) return Character;

   procedure Move
      (turingTape : in out TTuringTape; move : TTuringTapeMove);

   procedure Clear (turingTape : in out TTuringTape);

   procedure Initialize
      (turingTape : in out TTuringTape;
      content : Unbounded_String;
      resetPosition : Boolean := True);

end TuringTape;
