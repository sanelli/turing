with TuringTyping; use TuringTyping;
with TuringTape; use TuringTape;
with TuringTransitionFunction; use TuringTransitionFunction;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package TuringMachine is

   type TTuringMachine is tagged record
      InitialStatus        : TTuringStatus;
      FinalStatuses        : TTuringStatusesSets.Set;
      Symbols              : TTuringSymbolsSets.Set;
      CurrentStatus        : TTuringStatus;
      Tape                 : TTuringTape;
      TransitionFunction   : TTuringTransitionFunction;
   end record;

   procedure Create (
      machine        : in out TTuringMachine;
      statuses       : TTuringStatusesSets.Set;
      initialStatus  : TTuringStatus;
      finalStatues   : TTuringStatusesSets.Set;
      symbols        : TTuringSymbolsSets.Set;
      emptySymbol    : TTuringSymbol;
      transitions    : TuringTransitionFunctionMap.Map);

   procedure Clear (
      machine : in out TTuringMachine;
      initialTape : Unbounded_String);

   function Halted (
      machine : in out TTuringMachine)
      return Boolean;

   procedure Step (machine : in out TTuringMachine);

   procedure Run (machine : in out TTuringMachine);

end TuringMachine;