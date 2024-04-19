with Ada.Containers; use Ada.Containers;

package body TuringTape is
   procedure Create (turingTape : in out TTuringTape; emptySymbol : Character)
   is
   begin
      turingTape.CursorPosition := 0;
      turingTape.EmptySymbol    := emptySymbol;
   end Create;

   function GetIndexFromCurrentPosition
      (turingTape : in out TTuringTape) return Natural
   is
   begin
      if turingTape.CursorPosition >= 0 then
         return turingTape.CursorPosition;
      else
         return -turingTape.CursorPosition - 1;
      end if;
   end GetIndexFromCurrentPosition;

   procedure EnsureTapeHasSpaceForCurrentPosition
      (turingTape : in out TTuringTape; Index : out Natural)
   is
   begin
      Index := turingTape.GetIndexFromCurrentPosition;
      if turingTape.CursorPosition >= 0 then
         while turingTape.PositiveTape.Length
            <= Ada.Containers.Count_Type (Index) loop
            turingTape.PositiveTape.Append (turingTape.EmptySymbol);
         end loop;
      else
         while turingTape.NegativeTape.Length
            <= Ada.Containers.Count_Type (Index) loop
            turingTape.NegativeTape.Append (turingTape.EmptySymbol);
         end loop;
      end if;
   end EnsureTapeHasSpaceForCurrentPosition;

   procedure SetSymbol (turingTape : in out TTuringTape; symbol : Character)
   is
      Index : Natural;
   begin
      EnsureTapeHasSpaceForCurrentPosition (turingTape, Index);
      if turingTape.CursorPosition >= 0 then
         turingTape.PositiveTape (Index) := symbol;
      else
         turingTape.NegativeTape (Index) := symbol;
      end if;
   end SetSymbol;

   function GetSymbol (turingTape : in out TTuringTape) return Character
   is
      Index : Natural;
   begin
      EnsureTapeHasSpaceForCurrentPosition (turingTape, Index);
      if turingTape.CursorPosition >= 0 then
         return turingTape.PositiveTape (Index);
      else
         return turingTape.NegativeTape (Index);
      end if;
   end GetSymbol;

   procedure Move (turingTape : in out TTuringTape; move : TTuringTapeMove)
   is
   begin
      case move is
         when Left =>
            turingTape.CursorPosition := turingTape.CursorPosition - 1;
         when Right =>
            turingTape.CursorPosition := turingTape.CursorPosition + 1;
         when None => null;
      end case;
   end Move;

   procedure Clear (turingTape : in out TTuringTape)
   is
   begin
      turingTape.CursorPosition := 0;
      turingTape.NegativeTape.Clear;
      turingTape.PositiveTape.Clear;
   end Clear;

   procedure Initialize
      (turingTape : in out TTuringTape;
      content : Unbounded_String;
      resetPosition : Boolean := True)
   is
   begin
      turingTape.Clear;
      for Index in 1 .. Length (content) loop
         turingTape.SetSymbol (Element (content, Index));
         turingTape.Move (Right);
      end loop;

      if resetPosition then
         turingTape.CursorPosition := 0;
      end if;
   end Initialize;

   function ToString
      (turingTape : in out TTuringTape) return Unbounded_String
   is
      result : Unbounded_String;
      notFirst : Boolean;
   begin
      result := Ada.Strings.Unbounded.To_Unbounded_String ("|");
      notFirst := False;

      for Index in 0 .. UnlimitedTape.Length (turingTape.NegativeTape) - 1 loop
         if notFirst then
            Append (result, Ada.Strings.Unbounded.To_Unbounded_String ("|"));
         end if;
            Append (result, Ada.Strings.Unbounded.To_Unbounded_String
               ((1 => turingTape.NegativeTape (Natural (Index)))));
         notFirst := True;
      end loop;

      for Index in 0 .. UnlimitedTape.Length (turingTape.PositiveTape) - 1 loop
         if notFirst then
            Append (result, Ada.Strings.Unbounded.To_Unbounded_String ("|"));
         end if;
            Append (result, Ada.Strings.Unbounded.To_Unbounded_String
               ((1 => turingTape.PositiveTape (Natural (Index)))));
         notFirst := True;
      end loop;

      Append (result, Ada.Strings.Unbounded.To_Unbounded_String ("|"));
      return result;
   end ToString;

end TuringTape;
