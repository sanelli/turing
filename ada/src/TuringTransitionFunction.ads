with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with TuringTape; use TuringTape;
with Ada.Containers; use Ada.Containers;

package TuringTransitionFunction is

   type TTuringTransitionFunctionFrom is record
      Status   : Unbounded_String;
      Symbol   : Character;
   end record;

   type TTuringTransitionFunctionTo is record
      Status   : Unbounded_String;
      Symbol   : Character;
      Move     : TTuringTapeMove;
   end record;

   function Hash (from : TTuringTransitionFunctionFrom) return Hash_Type;
   function Equals (left, right : TTuringTransitionFunctionFrom) return Boolean;

   package TuringTransitionFunctionMap is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => TTuringTransitionFunctionFrom,
        Element_Type    => TTuringTransitionFunctionTo,
        Hash            => TuringTransitionFunction.Hash,
        Equivalent_Keys => TuringTransitionFunction.Equals);

   type TTuringTransitionFunction is record
      HaltStatus  : Unbounded_String;
      Map         : TuringTransitionFunctionMap.Map;
   end record;
end TuringTransitionFunction;