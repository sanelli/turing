unit TuringMachine;
interface
    uses TuringTyping, TuringTape, TuringTransitionFunction;
    type 
        TTuringMachine = record
            InitialState    : TTuringState;
            FinalStates     : TArrayOfStates;
            Symbols         : TSetOfSymbols;
            CurrentState    : TTuringState;
        end;
implementation
end.