with TuringTyping; use TuringTyping;
with TuringTape; use TuringTape;
with TuringTransitionFunction; use TuringTransitionFunction;

package TuringMachine is

   type TTuringMachine is tagged record
      InitialStatus        : TTuringStatus;
      FinalStatuses        : TTuringStatusesSets.Set;
      Symbols              : TTuringSymbolsSets.Set;
      CurrentStatus        : TTuringStatus;
      Tape                 : TTuringTape;
      TransitionFunction   : TTuringTransitionFunction;
   end record;

end TuringMachine;