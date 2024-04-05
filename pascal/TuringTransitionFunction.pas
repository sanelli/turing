unit TuringTransitionFunction;

interface
    uses TuringTyping;

    type 
        TTuringTransitionFunctionFrom = record
            State   : TTuringState;
            Symbol  : TTuringSymbol;
        end;

        TTuringTransitionFunctionTo = record
            State   : TTuringState;
            Symbol  : TTuringSymbol;
            Move    : TTapeMove;
        end;

        TTuringTransitionFunctionMap = record
            Source  : TTuringTransitionFunctionFrom;
            Target  : TTuringTransitionFunctionTo;
        end;

        TTuringTransitionFunction = record
            HaltState   : TTuringState;
            Transitions : array of TTuringTransitionFunctionMap;
        end;

        procedure InitializeTuringTransitionFunction(var Transitions: TTuringTransitionFunction; HaltState: TTuringState);
        function HasTransition(var Transitions: TTuringTransitionFunction; State : TTuringState; Symbol: TTuringSymbol) : boolean;

implementation
    procedure InitializeTuringTransitionFunction(var Transitions: TTuringTransitionFunction; HaltState: TTuringState);
    begin
        Transitions.HaltState := HaltState;
        SetLength(Transitions.Transitions, 0);
    end;

    function HasTransition(var Transitions: TTuringTransitionFunction; State : TTuringState; Symbol: TTuringSymbol) : boolean;
    var
        Idx : integer;
    begin
        HasTransition := false;
        Idx := 0;
        while (Idx < Length(Transitions.Transitions)) and not HasTransition do begin
            HasTransition := (Transitions.Transitions[Idx].Source.State = State) and (Transitions.Transitions[Idx].Source.Symbol = Symbol);
            Inc(Idx);
        end;
    end;
end.