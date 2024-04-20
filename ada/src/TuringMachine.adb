with TuringException; use TuringException;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body TuringMachine is
   procedure RaiseIfInvalidStatus (
      status : TTuringStatus;
      statuses : TTuringStatusesSets.Set;
      name : String) is
   begin
      if status = Ada.Strings.Unbounded.To_Unbounded_String ("") then
         raise TTuringException with name & " cannot be an empty string";
      end if;

      if not statuses.Contains (status) then
         raise TTuringException with name &
            " is not in the list of valid statuses";
      end if;
   end RaiseIfInvalidStatus;

   procedure RaiseIfInvalidSymbol (
      symbol : TTuringSymbol;
      symbols : TTuringSymbolsSets.Set;
      name : String) is
   begin
      if not symbols.Contains (symbol) then
         raise TTuringException with name &
            " is not in the list of valid symbols";
      end if;
   end RaiseIfInvalidSymbol;

   function GetAnyStatus (statuses : TTuringStatusesSets.Set)
      return TTuringStatus
   is
   begin
      for status of statuses loop
         return status;
      end loop;
      raise TTuringException with
         "The list of statuses cannot be empty";
   end GetAnyStatus;

   procedure Create (
      machine        : in out TTuringMachine;
      statuses       : TTuringStatusesSets.Set;
      initialStatus  : TTuringStatus;
      finalStatues   : TTuringStatusesSets.Set;
      symbols        : TTuringSymbolsSets.Set;
      emptySymbol    : TTuringSymbol;
      transitions    : TuringTransitionFunctionMap.Map)
   is
   begin
      if statuses.Is_Empty then
         raise TTuringException with
            "At least one status must be provided";
      end if;

      RaiseIfInvalidStatus (initialStatus, statuses, "Initial status");

      if finalStatues.Is_Empty then
         raise TTuringException with
            "At least one final status must be provided";
      end if;

      for finalStatus of finalStatues loop
         RaiseIfInvalidStatus (finalStatus, statuses, "Final status");
      end loop;

      if symbols.Is_Empty then
         raise TTuringException with
            "At least one symbol must be provided";
      end if;

      RaiseIfInvalidSymbol (emptySymbol, symbols, "Empty symbol");

      for transition in transitions.Iterate loop
         RaiseIfInvalidSymbol (
            TuringTransitionFunctionMap.Key (transition).Symbol,
            symbols,
            "Symbol");
         RaiseIfInvalidStatus (
            TuringTransitionFunctionMap.Key (transition).Status,
            statuses,
            "Status");
         RaiseIfInvalidSymbol (
            transitions (transition).Symbol,
            symbols,
            "Symbol");
         RaiseIfInvalidStatus (
            transitions (transition).Status,
            statuses,
            "Status");
      end loop;

      machine.InitialStatus := initialStatus;
      machine.CurrentStatus := initialStatus;
      machine.FinalStatuses := finalStatues;
      machine.Symbols := symbols;
      Create (machine.Tape, emptySymbol);
      Create (machine.TransitionFunction, GetAnyStatus (finalStatues));
      for transition in transitions.Iterate loop
         machine.TransitionFunction.Set (TuringTransitionFunctionMap.Key (transition), transitions (transition));
      end loop;
   end Create;

end TuringMachine;