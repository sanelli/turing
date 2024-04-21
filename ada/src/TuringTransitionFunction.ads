with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;

with TuringTyping; use TuringTyping;

package TuringTransitionFunction is

   type TTuringTransitionFunctionFrom is record
      Status   : TTuringStatus;
      Symbol   : TTuringSymbol;
   end record;

   type TTuringTransitionFunctionTo is record
      Status   : TTuringStatus;
      Symbol   : TTuringSymbol;
      Move     : TTuringTapeMove;
   end record;

   function Hash (from : TTuringTransitionFunctionFrom) return Hash_Type;
   overriding function "=" (left, right : TTuringTransitionFunctionFrom)
      return Boolean;

   package TuringTransitionFunctionMap is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => TTuringTransitionFunctionFrom,
        Element_Type    => TTuringTransitionFunctionTo,
        Hash            => TuringTransitionFunction.Hash,
        Equivalent_Keys => TuringTransitionFunction."=");

   type TTuringTransitionFunction is tagged record
      HaltStatus  : Unbounded_String;
      Map         : TuringTransitionFunctionMap.Map;
   end record;

   procedure Create (
      transitionFunction : in out TTuringTransitionFunction;
      haltStatus  : TTuringStatus);

   procedure Set (
      transitionFunction : in out TTuringTransitionFunction;
      from : TTuringTransitionFunctionFrom;
      to : TTuringTransitionFunctionTo);
   function Get (
      transitionFunction : in out TTuringTransitionFunction;
      from : TTuringTransitionFunctionFrom)
      return TTuringTransitionFunctionTo;

end TuringTransitionFunction;