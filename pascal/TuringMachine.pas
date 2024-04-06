unit TuringMachine;
interface
    uses TuringTyping, TuringTape, TuringTransitionFunction, TuringUtility;
    type 
        TTuringMachine = record
            InitialState    : TTuringState;
            FinalStates     : TArrayOfStates;
            Symbols         : TSetOfSymbols;
            CurrentState    : TTuringState;
            Tape            : TTuringTape;
            Transitions     : TTuringTransitionFunction;
        end;

    procedure InitializeTuringMachine(
        var Machine         : TTuringMachine;
        var States          : TArrayOfStates;
        InitialState        : TTuringState;
        var FinalStates     : TArrayOfStates;
        var Symbols         : TSetOfSymbols;
        EmptySymbol         : TTuringSymbol;
        Transitions         : array of TTuringTransitionFunctionMap);
    procedure ClearTuringMachine(var Machine: TTuringMachine; var Tape: TArrayOfSymbols);
    function StepTuringMachine(var Machine: TTuringMachine) : boolean;
    function HasTuringMachineHalted(var Machine: TTuringMachine) : boolean;
    procedure RunTuringMachine(var Machine: TTuringMachine);
    function GetCurrentTuringMachineStatus(var Machine: TTuringMachine) : TTuringState;
    function GetTuringMachineTape(var Machine: TTuringMachine) : string;

implementation

    procedure InitializeTuringMachine(
        var Machine         : TTuringMachine;
        var States          : TArrayOfStates;
        InitialState        : TTuringState;
        var FinalStates     : TArrayOfStates;
        var Symbols         : TSetOfSymbols;
        EmptySymbol         : TTuringSymbol;
        Transitions         : array of TTuringTransitionFunctionMap);
    var
        Idx: integer;
    begin
        if Length(States) <= 0 then Panic('At least one state must be provided');
        if Length(FinalStates) <= 0 then Panic('At least one final state must be provided');
        if Length(Transitions) <= 0 then Panic('At least one transitiont be provided');
        if Symbols = [] then Panic('At least one symbol must be provided');

        for Idx := 0 to Length(States) - 1 do begin
            if States[idx] = '' then Panic('State cannot be an empty string.');
        end;

        if not IsValidTuringState(InitialState, States) then Panic('Invalid state "' + InitialState + '".');

        for Idx := 0 to Length(FinalStates) - 1 do begin
            if not IsValidTuringState(FinalStates[Idx], States) then Panic('Invalid state "' + FinalStates[Idx] + '".');
        end;

        if not (EmptySymbol in Symbols) then Panic('Invalid empty symbol');

        Machine.InitialState := InitialState;
        Machine.CurrentState := InitialState;
        Machine.FinalStates := FinalStates;
        Machine.Symbols := Symbols;
        
        InitializeTape(Machine.Tape, EmptySymbol);
        InitializeTuringTransitionFunction(Machine.Transitions, Machine.FinalStates[0]);

        for Idx := 0 to Length(Transitions) - 1 do begin
            if not IsValidTuringState(Transitions[Idx].Source.State, States) then Panic('Invalid state "' + Transitions[Idx].Source.State + '".');
            if not (Transitions[Idx].Source.Symbol in Symbols) then Panic('Invalid symbol "' + Transitions[Idx].Source.Symbol + '".');
            if not IsValidTuringState(Transitions[Idx].Target.State, States) then Panic('Invalid state "' + Transitions[Idx].Target.State + '".');
            if not (Transitions[Idx].Target.Symbol in Symbols) then Panic('Invalid symbol "' + Transitions[Idx].Target.Symbol + '".');

            SetTransition(Machine.Transitions, Transitions[Idx].Source, Transitions[Idx].Target);
        end;
    end;

    procedure ClearTuringMachine(var Machine: TTuringMachine; var Tape: TArrayOfSymbols);
    var
        Idx     : integer;
        Symbol  : TTuringSymbol;
    begin
        for Idx := 0 to Length(Tape) - 1 do begin
            Symbol := Tape[Idx];
            if not (Symbol in Machine.Symbols) then Panic('Invalid symbol "' + Symbol + '".');
        end;

       SetTapeContent(Machine.Tape, Tape, true);
    end;

    function StepTuringMachine(var Machine: TTuringMachine) : boolean;
    var
        Transition : TTuringTransitionFunctionTo;
    begin
        StepTuringMachine := not HasTuringMachineHalted(Machine);
        if StepTuringMachine then begin
            Transition := GetTransition(Machine.Transitions, Machine.CurrentState, GetTapeSymbol(Machine.Tape));
            Machine.CurrentState := Transition.State;
            SetTapeSymbol(Machine.Tape, Transition.Symbol);
            MoveTape(Machine.Tape, Transition.Move);
        end;
    end;

    function HasTuringMachineHalted(var Machine: TTuringMachine) : boolean;
    var
        Idx : integer;
    begin
        Idx := 0;
        HasTuringMachineHalted := false;
        while (Idx < Length(Machine.FinalStates)) and (not HasTuringMachineHalted) do begin
            HasTuringMachineHalted := Machine.FinalStates[Idx] = Machine.CurrentState;
            Inc(Idx);
        end;
    end;

    procedure RunTuringMachine(var Machine: TTuringMachine);
    begin
        while not StepTuringMachine(Machine) do;
    end;

    function GetCurrentTuringMachineStatus(var Machine: TTuringMachine) : TTuringState;
    begin
        GetCurrentTuringMachineStatus := Machine.CurrentState;
    end;

    function GetTuringMachineTape(var Machine: TTuringMachine) : string;
    begin
        GetTuringMachineTape := TapeToString(Machine.Tape, '|');
    end;
end.