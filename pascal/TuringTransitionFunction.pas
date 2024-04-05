unit TuringTransitionFunction;

interface
    uses TuringTyping, TuringUtility;

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
        function HasTransition(var Transitions: TTuringTransitionFunction; State: TTuringState; Symbol: TTuringSymbol) : boolean;
        procedure SetTransition(var Transitions: TTuringTransitionFunction; Source: TTuringTransitionFunctionFrom; Target: TTuringTransitionFunctionTo);
        function GetTransition(var Transitions: TTuringTransitionFunction; Source: TTuringTransitionFunctionFrom) : TTuringTransitionFunctionTo;

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

    procedure SetTransition(var Transitions: TTuringTransitionFunction; Source: TTuringTransitionFunctionFrom; Target: TTuringTransitionFunctionTo);
    begin
        if HasTransition(Transitions, Source.State, Source.Symbol) then Panic('Tranistion from (' + Source.State + ',' + Source.Symbol + ') already exists');
    
        SetLength(Transitions.Transitions, Length(Transitions.Transitions) + 1);

        Transitions.Transitions[ Length(Transitions.Transitions) - 1].Source := Source;
        Transitions.Transitions[ Length(Transitions.Transitions) - 1].Target := Target;
    end;
    
    function GetTransition(var Transitions: TTuringTransitionFunction; Source: TTuringTransitionFunctionFrom) : TTuringTransitionFunctionTo;
    var
        Idx     : integer;
        Found   : boolean;
    begin
        GetTransition.State := Transitions.HaltState;
        GetTransition.Symbol := Source.Symbol;
        GetTransition.Move := None;

        Found := false;
        Idx := 0;
        while (Idx < Length(Transitions.Transitions)) and not Found do begin
            if (Transitions.Transitions[Idx].Source.State = Source.State) and (Transitions.Transitions[Idx].Source.Symbol = Source.Symbol) then begin
                Found := true;
                GetTransition := Transitions.Transitions[Idx].Target
            end;
            Inc(Idx);
        end;

    end;
end.