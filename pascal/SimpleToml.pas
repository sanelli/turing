{
    Simple TOML reader to cater only for this Pascal implementation.
    It only supports:
    - String values
    - Array of strings on a single line
    - Array of sections
    It does not supper sections properly currently (but could be easy to add) or nested sections.
    The format needs to be: variable definitions followerd by contiguous array definitions.
    It supports comments.
}
unit SimpleToml;

interface
    type
        TTomlArrayOfStrings = array of AnsiString;

        TTomlDocumentSpan = record 
            StartFrom   : integer;
            EndTo       : integer;
        end;

        TTomlDocumentValue = record
            ValueName   : string;
            Span        : TTomlDocumentSpan;
        end;

        TTomlDocumentTableSpan = record
            ValueName   : string;
            Spans       : array of TTomlDocumentSpan;
        end;

        TTomlDocument = record
            Filename    : string;
            Content     : AnsiString;
            Values      : array of TTomlDocumentValue;
            Tables      : array of TTomlDocumentTableSpan;
        end;

    procedure LoadTomlDocumentFromFile(var Document: TTomlDocument; Filename: string);
    procedure LoadTomlDocumentFromString(var Document: TTomlDocument; Content: AnsiString);

    function GetStringFromTomlDocument(var Document: TTomlDocument; ValueName: AnsiString) : AnsiString;
    function GetArrayOfStringsFromTomlDocument(var Document: TTomlDocument; ValueName: AnsiString) : TTomlArrayOfStrings;

implementation

    procedure SimpleTomlPanic(Message : string);
    begin
        WriteLn(Message);
        Halt(1);
    end;

    function ReadAllTextFromFile(Filename: string) : AnsiString;
    var
        InputFile       : file of char;
        CurrentError    : integer;
        CurrentErrorStr : string;
        CurrentChar     : char;
    begin
        ReadAllTextFromFile := '';

        Assign(InputFile, Filename);
        CurrentError := IOResult;
        if CurrentError <> 0 then begin
            Str(CurrentError, CurrentErrorStr);
            SimpleTomlPanic('Cannot open file "' + Filename +'" (code: ' + CurrentErrorStr +')');
        end;

        Reset(InputFile);
        CurrentError := IOResult;
        if CurrentError <> 0 then begin
            Str(CurrentError, CurrentErrorStr);
            SimpleTomlPanic('Cannot open file "' + Filename +'" (code: ' + CurrentErrorStr +')');
        end;

        while not Eof(InputFile) do begin
            Read(InputFile, CurrentChar);
            CurrentError := IOResult;
            if CurrentError <> 0 then begin
                Str(CurrentError, CurrentErrorStr);
                SimpleTomlPanic('Cannot read file "' + Filename +'" (code: ' + CurrentErrorStr +')');
            end;

            ReadAllTextFromFile := ReadAllTextFromFile + CurrentChar;
        end;

        Close(InputFile);
        if CurrentError <> 0 then begin
            Str(CurrentError, CurrentErrorStr);
            SimpleTomlPanic('Cannot clode file "' + Filename +'" (code: ' + CurrentErrorStr +')');
        end;
    end;

    function IsEndOfLine(var Content: AnsiString; Location: integer) : boolean;
    var
        AsciiValue: integer;
    begin
        AsciiValue := integer(char(Content[Location]));
        IsEndOfLine := (AsciiValue = 10) or (AsciiValue = 12) or (AsciiValue = 13);
    end; 

    function IsSpace(var Content: AnsiString; Location: integer) : boolean;
    var
        AsciiValue: integer;
    begin
        AsciiValue := integer(char(Content[Location]));
        IsSpace := (AsciiValue = 10) or (AsciiValue = 12) or (AsciiValue = 13) or (AsciiValue = 9) or (AsciiValue = 32);
    end;

    function IsCommentLine(var Content: AnsiString; Location: integer) : boolean;
    var
        AsciiValue: integer;
    begin
        AsciiValue := integer(char(Content[Location]));
        IsCommentLine := (AsciiValue = 35);
    end; 

    function ReadLineBoundaries(var Content: AnsiString; StartFrom: integer) : integer;
    begin
        ReadLineBoundaries := StartFrom + 1;
        while (ReadLineBoundaries < Length(Content)) and not IsEndOfLine(Content, ReadLineBoundaries) do begin
            Inc(ReadLineBoundaries);
        end;
    end;

    procedure TrimLineBoundaries(var Content: AnsiString; var StartFrom: integer; var EndTo: integer);
    begin
        { Skip spaces at the beginning }
        while (StartFrom < EndTo) and IsSpace(Content, StartFrom) do begin
            Inc(StartFrom);
        end;

        { Skip spaces at the end }
        while (StartFrom < EndTo) and IsSpace(Content, EndTo - 1) do begin
            Dec(EndTo);
        end;
    end;

    function SubString(var Content: AnsiString; StartFrom: integer; EndTo: integer) : AnsiString;
    var
        Idx : integer;
    begin
        SubString := '';
        TrimLineBoundaries(Content, StartFrom, EndTo);
        Idx := StartFrom;

        while Idx < EndTo do begin
            SubString := SubString + Content[Idx];
            Inc(Idx);
        end;
    end;

    function IndexOf(var Content: AnsiString; StartFrom: integer; EndTo: integer; Character: integer) : integer;
    var
        Idx         : integer;
        AsciiValue  : integer;
    begin
        IndexOf := -1;
        Idx := StartFrom;
        while (Idx < EndTo) and (IndexOf = -1) do begin
            AsciiValue := integer(char(Content[Idx]));
            if AsciiValue = Character then IndexOf := Idx;
            Inc(Idx);
        end;
    end;

    procedure LoadTomlDocumentFromFile(var Document: TTomlDocument; Filename: string);
    var
        Content: AnsiString;
    begin
        Document.Filename := Filename;
        Content := ReadAllTextFromFile(Filename);
        LoadTomlDocumentFromString(Document, Content);
    end;

    function GetTableIndex(var Document: TTomlDocument; ValueName : AnsiString) : integer;
    var
        Idx: integer;
    begin
        GetTableIndex := -1;
        Idx := 0;
        While (Idx < Length(Document.Tables)) and (GetTableIndex = -1) do begin
            if Document.Tables[Idx].ValueName = ValueName then GetTableIndex := Idx;
            Inc(Idx);
        end;
    end;

    function GetOrCreateTableIndex(var Document: TTomlDocument; ValueName : AnsiString) : integer;
    begin
        GetOrCreateTableIndex := GetTableIndex(Document, ValueName);
        if GetOrCreateTableIndex = -1 then begin
            SetLength(Document.Tables, Length(Document.Tables) + 1);
            GetOrCreateTableIndex := Length(Document.Tables) - 1;
            Document.Tables[GetOrCreateTableIndex].ValueName := ValueName;
            SetLength(Document.Tables[GetOrCreateTableIndex].Spans, 0);
        end;
    end;

    procedure LoadTomlDocumentFromStringWithBoundaries(var Document: TTomlDocument; Content: AnsiString; StartFrom: integer; EndTo: integer);
    var
        StartLine           : integer;
        EndLine             : integer;
        TrimStartLine       : integer;
        TrimEndLine         : integer;
        EqualSignIdx        : integer;
        SqrParIdx1          : integer;
        SqrParIdx2          : integer;
        SqrParIdx3          : integer;
        SqrParIdx4          : integer;
        ValueName           : AnsiString;
        ValueStartFrom      : integer;
        ValueEndTo          : integer;
        TableIndex          : integer;
        CurrentDocumentSpan : integer;
    begin
        Document.Content := Content;
        SetLength(Document.Values, 0);

        StartLine := StartFrom;
        EndLine := StartLine;
        TableIndex := -1;
        CurrentDocumentSpan := -1;
        while EndLine < EndTo do begin
            EndLine := ReadLineBoundaries(Content, StartLine);
            if (EndLine - StartLine) > 1 then begin { Skip empty lines }
                TrimStartLine := StartLine;
                TrimEndLine := EndLine;
                TrimLineBoundaries(Content, TrimStartLine, TrimEndLine);
                if not IsCommentLine(Content, TrimStartLine) then begin
                    EqualSignIdx := IndexOf(Content, TrimStartLine, TrimEndLine, integer(char('=')));
                    if EqualSignIdx <> -1 then begin
                        if TableIndex = -1 then begin { In the root }
                            ValueName := SubString(Content, TrimStartLine, EqualSignIdx);
                            SetLength(Document.Values, Length(Document.Values) + 1);
                            ValueStartFrom := EqualSignIdx + 1;
                            ValueEndTo := TrimEndLine;
                            TrimLineBoundaries(Content, ValueStartFrom, ValueEndTo);
                            Document.Values[Length(Document.Values) - 1].ValueName := ValueName;
                            Document.Values[Length(Document.Values) - 1].Span.StartFrom := ValueStartFrom;
                            Document.Values[Length(Document.Values) - 1].Span.EndTo := ValueEndTo;
                        end else if (TableIndex <> -1) and (CurrentDocumentSpan <> -1) then begin
                            Document.Tables[TableIndex].Spans[CurrentDocumentSpan].EndTo := EndLine;
                        end;
                    end; { Read a value}

                    SqrParIdx1 := IndexOf(Content, TrimStartLine, TrimEndLine, integer(char('[')));
                    SqrParIdx2 := IndexOf(Content, TrimStartLine + 1, TrimEndLine, integer(char('[')));
                    SqrParIdx3 := IndexOf(Content, TrimEndLine - 2, TrimEndLine, integer(char(']')));
                    SqrParIdx4 := IndexOf(Content, TrimEndLine - 1, TrimEndLine, integer(char(']')));
                    if (SqrParIdx1 = TrimStartLine) and (SqrParIdx2 = TrimStartLine + 1) and (SqrParIdx3 = TrimEndLine - 2) and (SqrParIdx4 = TrimEndLine - 1)then begin
                        ValueName := SubString(Content, SqrParIdx2 + 1, SqrParIdx3);
                        TableIndex := GetOrCreateTableIndex(Document, ValueName);
                        Setlength(Document.Tables[TableIndex].Spans, Length(Document.Tables[TableIndex].Spans) + 1);
                        CurrentDocumentSpan := Length(Document.Tables[TableIndex].Spans) -1;
                        Document.Tables[TableIndex].Spans[CurrentDocumentSpan].StartFrom := EndLine;
                        Document.Tables[TableIndex].Spans[CurrentDocumentSpan].EndTo := EndLine + 1;

                    end; { Readin sequence of arrays }
                end; { not comment }
            end; { Not emtpy }
            StartLine := EndLine;
        end; { for each line }
    end;

    procedure LoadTomlDocumentFromString(var Document: TTomlDocument; Content: AnsiString);
    begin
        LoadTomlDocumentFromStringWithBoundaries(Document, Content, 1, Length(Content));
    end;

    function GetIndexForValue(var Document: TTomlDocument; ValueName: AnsiString): integer;
     var FoundIdx: boolean;
    begin
        GetIndexForValue := 0;
        FoundIdx := false;

        while (GetIndexForValue < Length(Document.Values)) and (not FoundIdx) do begin
            FoundIdx := Document.Values[GetIndexForValue].ValueName = ValueName;
            Inc(GetIndexForValue);
        end;

        Dec(GetIndexForValue);

        if not FoundIdx then SimpleTomlPanic('Cannot find value');
    end;

    function ExtractStringValue(var Content: AnsiString; StartFrom: integer; EndTo: integer) : AnsiString;
    begin
        ExtractStringValue := '';
        TrimLineBoundaries(Content, StartFrom, EndTo);
        if (IndexOf(Content, StartFrom, EndTo, integer(char('"'))) = StartFrom)
            and (IndexOf(Content, EndTo - 1, EndTo, integer(char('"'))) = (EndTo - 1)) then begin
        end else SimpleTomlPanic('Value does not look like a string');

        ExtractStringValue := SubString(Content, StartFrom + 1, EndTo - 1);
    end;

    function GetStringFromTomlDocument(var Document: TTomlDocument; ValueName: AnsiString) : AnsiString;
    var Idx: integer;
    begin
        GetStringFromTomlDocument := '';
        Idx := GetIndexForValue(Document, ValueName);
        GetStringFromTomlDocument := ExtractStringValue(Document.Content, Document.Values[Idx].Span.StartFrom, Document.Values[Idx].Span.EndTo);
    end;

    function GetArrayOfStringsFromTomlDocument(var Document: TTomlDocument; ValueName: AnsiString) : TTomlArrayOfStrings;
    var
        Idx         : integer;
        StartFrom   : integer;
        EndTo       : integer;
    begin
        GetArrayOfStringsFromTomlDocument := nil;
        SetLength(GetArrayOfStringsFromTomlDocument, 0);
        Idx := GetIndexForValue(Document, ValueName);

        StartFrom := Document.Values[Idx].Span.StartFrom;
        EndTo := Document.Values[Idx].Span.EndTo;
        TrimLineBoundaries(Document.Content, StartFrom, EndTo);

        if (IndexOf(Document.Content, StartFrom, EndTo, integer(char('['))) = StartFrom)
            and (IndexOf(Document.Content, EndTo - 1, EndTo, integer(char(']'))) = (EndTo - 1)) then begin
            { TODO: IMPLEMENT ME}
        end else SimpleTomlPanic('Value does not look like an array');
    end;

end.