unit TuringUtility;
interface
    const
        EXIT_SUCCESS: integer = 0;
        EXIT_FAILURE: integer = 1;
    procedure Panic(Message: string);
    function ArrayOfStringToString(var a: array of AnsiString) : string;
implementation
    procedure Panic(Message: string);
    begin
        WriteLn(Message);
        Halt(EXIT_FAILURE);
    end;

    function ArrayOfStringToString(var a: array of AnsiString) : string;
    var
        Idx     : integer;
        First   : boolean;
    begin
        ArrayOfStringToString := '[';
        First := true;
        for Idx := 0 to Length(a) - 1 do begin
            if not First then ArrayOfStringToString := ArrayOfStringToString + ', ';
            ArrayOfStringToString := ArrayOfStringToString + '"' + a[Idx] + '"';
            First := false;
        end;
        ArrayOfStringToString := ArrayOfStringToString + ']';
    end;
end.