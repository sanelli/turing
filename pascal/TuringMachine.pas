unit TuringMachine;
interface
    uses TuringTyping, TuringTape;
    type 
        TTuringMachine = record
            InitialState    : TTuringState;
            FinalStates     : TArrayOfStates;
            Symbols         : TSetOfSymbols;
            CurrentState    : TTuringState;
        end;
implementation
end.