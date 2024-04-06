{
    Simple TOML reader to cater only for this Pascal implementation.
    It only supports:
    - String values
    - Array of strings on a single line
    - Array of sections
    It does not supper sections properly currently or nested sections.
    The format needs to be: variable definitions followerd by contiguous array definitions.
    It supports comments.
}
unit SimpleToml;

interface
    type
        TTomlDocumentValueSpan = record
            ValueName   : string;
            StartFrom   : integer;
            EndTo       : integer;
        end;

        TTomlDocument = record
            Filename    : string;
            Content     : AnsiString;
            Values      : array of TTomlDocumentValueSpan;
        end;

    procedure LoadTomlDocumentFromFile(var Document: TTomlDocument; Filename: string);
    procedure LoadTomlDocumentFromString(var Document: TTomlDocument; Content: AnsiString);

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

    procedure LoadTomlDocumentFromString(var Document: TTomlDocument; Content: AnsiString; StartFrom: integer, EndTo: integer);
    begin
    end;

    procedure LoadTomlDocumentFromString(var Document: TTomlDocument; Content: AnsiString);
    var
        StartLine       : integer;
        EndLine         : integer;
        TrimStartLine   : integer;
        TrimEndLine     : integer;
        EqualSignIdx    : integer;
        SqrParIdx       : integer;
        ValueName       : AnsiString;
        ReadValues      : boolean;
        ValueStartFrom  : integer;
        ValueEndTo      : integer;
    begin
        Document.Content := Content;
        SetLength(Document.Values, 0);

        StartLine := 1;
        EndLine := StartLine;
        ReadValues := true;
        while EndLine < Length(Content) do begin
            EndLine := ReadLineBoundaries(Content, StartLine);
            if (EndLine - StartLine) > 1 then begin { Skip empty lines }
                TrimStartLine := StartLine;
                TrimEndLine := EndLine;
                TrimLineBoundaries(Content, TrimStartLine, TrimEndLine);
                if not IsCommentLine(Content, TrimStartLine) then begin
                    EqualSignIdx := IndexOf(Content, TrimStartLine, TrimEndLine, integer(char('=')));
                    if ReadValues and (EqualSignIdx <> -1) then begin
                        ValueName := SubString(Content, TrimStartLine, EqualSignIdx);
                        SetLength(Document.Values, Length(Document.Values) + 1);
                        ValueStartFrom := EqualSignIdx + 1;
                        ValueEndTo := TrimEndLine;
                        TrimLineBoundaries(Content, ValueStartFrom, ValueEndTo);
                        Document.Values[Length(Document.Values) - 1].ValueName := ValueName;
                        Document.Values[Length(Document.Values) - 1].StartFrom := ValueStartFrom;
                        Document.Values[Length(Document.Values) - 1].EndTo := ValueEndTo;

                        WriteLn('[Debug] Value Found (',ValueName,',',SubString(Content,ValueStartFrom,ValueEndTo),')');
                    end; { Read a value}

                    SqrParIdx := IndexOf(Content, TrimStartLine, TrimEndLine, integer(char('[')));
                    if SqrParIdx = TrimStartLine then begin
                        ReadValues := false; 
                    end; { Readin sequence of arrays }

                end; { not comment }
            end; { Not emtpy }
            StartLine := EndLine;
        end; { for each line }

    end;
end.