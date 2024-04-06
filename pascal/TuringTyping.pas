unit TuringTyping;
interface
    uses TuringUtility;

    type 
        TTuringSymbol = char;
        TTuringState = string;
        TTapeMove = (None, Left, Right);
        TArrayOfStates = array of TTuringState;
        TSetOfSymbols = set of TTuringSymbol;
        TArrayOfSymbols = array of TTuringSymbol;

    function TapeMoveToStr(Move: TTapeMove) : string;
    function StrToTapeMove(Move: string) : TTapeMove;
    function IsValidTuringState(State: TTuringState; States: TArrayOfStates) : boolean;

implementation

    function TapeMoveToStr(Move: TTapeMove) : string;
    begin
        if Move = None then begin
            TapeMoveToStr := 'none';
        end else if Move = Left then begin
            TapeMoveToStr := 'left';
        end else if Move = Right then begin
            TapeMoveToStr := 'right';
        end else begin
            Panic('Unknown move type');
        end;
    end;

    function StrToTapeMove(Move: string) : TTapeMove;
    begin
        if Move = 'none' then begin
            StrToTapeMove := None;
        end else if Move = 'left' then begin
            StrToTapeMove := Left;
        end else if Move = 'right' then begin
            StrToTapeMove := Right;
        end else begin
            Panic('Unknown move type');
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