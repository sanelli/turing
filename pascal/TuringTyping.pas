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
end.