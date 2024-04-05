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
    procedure MoveTape(var tape : TTuringTape; move: TTapeMove);
    function TapeToString(var tape : TTuringTape; Separator: char) : string;
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

    procedure MoveTape(var tape : TTuringTape; move: TTapeMove);
    begin
        if move = Left then begin
            Dec(tape.CurrentPosition);
        end else if move = Right then begin
            Inc(tape.CurrentPosition);
        end else if move = None then begin
            { Nothing to do }
        end else begin
            WriteLn('Unknown move "', move ,'"');
            Halt(1);
        end;
    end;

    function TapeToString(var tape : TTuringTape; Separator: char) : string;
    var
        Idx   : integer;
        First : boolean;
    begin
        TapeToString := '';
        First := true;

        for Idx := length(tape.NegativeSymbols) -1 downto 0 do
        begin
            if not First then
                TapeToString := TapeToString + Separator;
            TapeToString := TapeToString + tape.NegativeSymbols[Idx];
            First := false;
        end;

        for Idx := 0 to length(tape.NegativeSymbols) - 1 do
        begin
            if not First then
                TapeToString := TapeToString + Separator;
            TapeToString := TapeToString + tape.PositiveSymbols[Idx];
            First := false;
        end;

        TapeToString := Separator + TapeToString + Separator;
    end;
end.