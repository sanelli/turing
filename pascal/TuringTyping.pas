unit TuringTyping;
interface
    type 
        TTuringSymbol = char;
        TTuringState = string;
        TTapeMove = (None, Left, Right);
        TArrayOfStates = array of TTuringState;
        TSetOfSymbols = set of TTuringSymbol;
        TArrayOfSymbols = array of TTuringSymbol;

    function TapeMoveToStr(Move: TTapeMove) : string;
    function IsValidTuringState(State: TTuringState; States: TArrayOfStates) : boolean;

implementation

    function TapeMoveToStr(Move: TTapeMove) : string;
    begin
        if Move = None then begin
            TapeMoveToStr := 'None';
        end else if Move = Left then begin
            TapeMoveToStr := 'Left';
        end else if Move = Right then begin
            TapeMoveToStr := 'Right';
        end else begin
            TapeMoveToStr := 'Unknown';
        end;
    end;

    function IsValidTuringState(State: TTuringState; States: TArrayOfStates) : boolean;
    var
        Idx: integer;
    begin
        IsValidTuringState := false;
        Idx := 0;
        if State <> '' then
            while (Idx < Length(States)) and (not IsValidTuringState) do begin
                IsValidTuringState := States[Idx] = State;
                Inc(Idx);
            end;
    end;
end.