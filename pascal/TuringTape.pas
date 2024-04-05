unit TuringTape;
interface
    uses TuringTyping;

    type 
        TTuringTape = record
            EmptySymbol         : TTuringSymbol;
            NegativeSymbols     : TArrayOfSymbols;
            PositiveSymbols     : TArrayOfSymbols;
            CurrentPosition     : integer;           
        end;

    procedure InitializeTape(var tape : TTuringTape; EmptySymbol: TTuringSymbol);
    procedure SetTapeSymbol(var tape : TTuringTape; Symbol: TTuringSymbol);
    function GetTapeSymbol(var tape : TTuringTape) : TTuringSymbol;
implementation
    function GetIndexForTape(position: integer) : integer;
    begin
        if position >= 0 
        then GetIndexForTape := position
        else GetIndexForTape := -position - 1;
    end;

    procedure EnsureTapeHasEnoughSpace(var tape: TArrayOfSymbols; Position: integer; EmptySymbol: TTuringSymbol);
    begin
        while Length(tape) <= Position do
        begin
            SetLength(tape, Length(tape) + 1);
            tape[Length(tape) - 1] := EmptySymbol;
        end;
    end;

    procedure InitializeTape(var tape : TTuringTape; EmptySymbol: TTuringSymbol);
    begin
        tape.CurrentPosition := 0;
        tape.EmptySymbol := EmptySymbol;
        SetLength(tape.NegativeSymbols, 0);
        SetLength(tape.PositiveSymbols, 0);
    end;

    procedure SetTapeSymbol(var tape : TTuringTape; Symbol: TTuringSymbol);
    var
        Position : integer;
    begin
        Position := GetIndexForTape(tape.CurrentPosition);
        if tape.CurrentPosition >= 0 then begin
            EnsureTapeHasEnoughSpace(tape.PositiveSymbols, Position, tape.EmptySymbol);
            tape.PositiveSymbols[position] := Symbol;
        end else begin
            EnsureTapeHasEnoughSpace(tape.NegativeSymbols, Position, tape.EmptySymbol);
            tape.NegativeSymbols[position] := Symbol;
        end;
    end;

    function GetTapeSymbol(var tape : TTuringTape) : TTuringSymbol;
    var
        Position : integer;
    begin
        Position := GetIndexForTape(tape.CurrentPosition);
        if tape.CurrentPosition >= 0 then begin
            EnsureTapeHasEnoughSpace(tape.PositiveSymbols, Position, tape.EmptySymbol);
            GetTapeSymbol := tape.PositiveSymbols[position];
        end else begin
            EnsureTapeHasEnoughSpace(tape.NegativeSymbols, Position, tape.EmptySymbol);
            GetTapeSymbol := tape.NegativeSymbols[position];
        end;
    end;
end.