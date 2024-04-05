unit TuringTape;

interface
    uses TuringTyping, TuringUtility;

    type 
        TTuringTape = record
            EmptySymbol         : TTuringSymbol;
            NegativeSymbols     : TArrayOfSymbols;
            PositiveSymbols     : TArrayOfSymbols;
            CurrentPosition     : integer;           
        end;

    procedure InitializeTape(var Tape: TTuringTape; EmptySymbol: TTuringSymbol);
    procedure SetTapeSymbol(var Tape: TTuringTape; Symbol: TTuringSymbol);
    function GetTapeSymbol(var Tape: TTuringTape) : TTuringSymbol;
    procedure MoveTape(var Tape: TTuringTape; Move: TTapeMove);
    function TapeToString(var Tape: TTuringTape; Separator: char) : string;

implementation
    function GetIndexForTape(position: integer) : integer;
    begin
        if position >= 0 
        then GetIndexForTape:= position
        else GetIndexForTape:= -position - 1;
    end;

    procedure EnsureTapeHasEnoughSpace(var Tape: TArrayOfSymbols; Position: integer; EmptySymbol: TTuringSymbol);
    begin
        while Length(Tape) <= Position do
        begin
            SetLength(Tape, Length(Tape) + 1);
            Tape[Length(Tape) - 1] := EmptySymbol;
        end;
    end;

    procedure InitializeTape(var Tape: TTuringTape; EmptySymbol: TTuringSymbol);
    begin
        Tape.CurrentPosition := 0;
        Tape.EmptySymbol := EmptySymbol;
        SetLength(Tape.NegativeSymbols, 0);
        SetLength(Tape.PositiveSymbols, 0);
    end;

    procedure SetTapeSymbol(var Tape: TTuringTape; Symbol: TTuringSymbol);
    var
        Position : integer;
    begin
        Position := GetIndexForTape(Tape.CurrentPosition);
        if Tape.CurrentPosition >= 0 then begin
            EnsureTapeHasEnoughSpace(Tape.PositiveSymbols, Position, Tape.EmptySymbol);
            Tape.PositiveSymbols[position] := Symbol;
        end else begin
            EnsureTapeHasEnoughSpace(Tape.NegativeSymbols, Position, Tape.EmptySymbol);
            Tape.NegativeSymbols[position] := Symbol;
        end;
    end;

    function GetTapeSymbol(var Tape: TTuringTape) : TTuringSymbol;
    var
        Position : integer;
    begin
        Position := GetIndexForTape(Tape.CurrentPosition);
        if Tape.CurrentPosition >= 0 then begin
            EnsureTapeHasEnoughSpace(Tape.PositiveSymbols, Position, Tape.EmptySymbol);
            GetTapeSymbol := Tape.PositiveSymbols[position];
        end else begin
            EnsureTapeHasEnoughSpace(Tape.NegativeSymbols, Position, Tape.EmptySymbol);
            GetTapeSymbol := Tape.NegativeSymbols[position];
        end;
    end;

    procedure MoveTape(var Tape: TTuringTape; Move: TTapeMove);
    begin
        if Move = Left then begin
            Dec(Tape.CurrentPosition);
        end else if Move = Right then begin
            Inc(Tape.CurrentPosition);
        end else if Move = None then begin
            { Nothing to do }
        end else begin
            Panic('Unknown move "' +TapeMoveToStr(Move) + '"');
        end;
    end;

    function TapeToString(var Tape: TTuringTape; Separator: char) : string;
    var
        Idx   : integer;
        First : boolean;
    begin
        TapeToString := '';
        First := true;

        for Idx := length(Tape.NegativeSymbols) -1 downto 0 do
        begin
            if not First then
                TapeToString := TapeToString + Separator;
            TapeToString := TapeToString + Tape.NegativeSymbols[Idx];
            First := false;
        end;

        for Idx := 0 to length(Tape.PositiveSymbols) - 1 do
        begin
            if not First then
                TapeToString := TapeToString + Separator;
            TapeToString := TapeToString + Tape.PositiveSymbols[Idx];
            First := false;
        end;

        TapeToString := Separator + TapeToString + Separator;
    end;
end.