unit TuringMachineIO;
interface
    uses SimpleToml, TuringMachine, TuringUtility, TuringTransitionFunction, TuringTyping;

    procedure LoadTuringMachineFromFile(var Machine : TTuringMachine; Format: string; Filename : string; Debug: boolean);
    procedure LoadTuringMachineFromString(var Machine : TTuringMachine; Format: string; Content : AnsiString; Debug: boolean);
    procedure LoadTuringMachineFromTomlFile(var Machine : TTuringMachine; Filename : string; Debug: boolean);
    procedure LoadTuringMachineFromTomlString(var Machine : TTuringMachine; Content : AnsiString; Debug: boolean);

implementation

    procedure LoadTuringMachineFromFile(var Machine : TTuringMachine; Format: string; Filename: string; Debug: boolean);
    begin
        if Format = 'toml' then LoadTuringMachineFromTomlFile(Machine, Filename, Debug)
        else Panic('Unknown format "' + Format + '".');
    end;

    procedure LoadTuringMachineFromString(var Machine : TTuringMachine; Format: string; Content : AnsiString; Debug: boolean);
    begin
        if Format = 'toml' then LoadTuringMachineFromTomlString(Machine, Content, Debug)
        else Panic('Unknown format "' + Format + '".');
    end;

    procedure LoadTuringMachineFromTomlDocument(var Machine : TTuringMachine; var Document : TTomlDocument; Debug: boolean);
    var
        States              : array of AnsiString;
        InitialState        : string;
        FinalStates         : array of AnsiString;
        Symbols             : array of AnsiString;
        EmptySymbol         : string;
        CountTransitions    : integer;
        Transitions         : array of TTuringTransitionFunctionMap;
        TransitionDoc       : TTomlDocument;
        Idx                 : integer;
        TmpSymbol           : string;
    begin
        States := GetArrayOfStringsFromTomlDocument(Document, 'States');
        InitialState := GetStringFromTomlDocument(Document, 'InitialState');
        FinalStates := GetArrayOfStringsFromTomlDocument(Document, 'FinalStates');
        Symbols := GetArrayOfStringsFromTomlDocument(Document, 'Symbols');
        for Idx := 0 to Length(Symbols) - 1 do begin
            if Length(Symbols[Idx]) <> 1 then Panic('Symbol must be exactly one character');
        end;
        
        EmptySymbol := GetStringFromTomlDocument(Document, 'EmptySymbol');
        if Length(EmptySymbol) <> 1 then Panic('Symbol must be exactly one character');

        CountTransitions := GetLengthForTomlDocumentTable(Document, 'Transitions');
        SetLength(Transitions, CountTransitions);
        
        for Idx := 0 to CountTransitions - 1 do begin
            GetTomlDocumentTableAt(Document, TransitionDoc, 'Transitions', Idx);
            Transitions[Idx].Source.State := GetStringFromTomlDocument(TransitionDoc, 'State');
            TmpSymbol := GetStringFromTomlDocument(TransitionDoc, 'Symbol');
            if Length(TmpSymbol) <> 1 then Panic('Symbol must be exactly one character');
            Transitions[Idx].Source.Symbol := TmpSymbol[1];
            Transitions[Idx].Target.State := GetStringFromTomlDocument(TransitionDoc, 'NewState');
            TmpSymbol := GetStringFromTomlDocument(TransitionDoc, 'NewSymbol');
            if Length(TmpSymbol) <> 1 then Panic('Symbol must be exactly one character');
            Transitions[Idx].Target.Symbol := TmpSymbol[1];
            //Transitions[Idx].Target.Move := StrToTapeMove(GetStringFromTomlDocument(TransitionDoc, 'Move'));
        end;

        if Debug then begin
            WriteLn('[Debug] States -> ', ArrayOfStringToString(States));
            WriteLn('[Debug] InitialState -> "', InitialState,'"');
            WriteLn('[Debug] FinalStates -> ', ArrayOfStringToString(FinalStates));
            WriteLn('[Debug] Symbols -> ', ArrayOfStringToString(Symbols));
            WriteLn('[Debug] EmptySymbol -> "', EmptySymbol,'"');
            WriteLn('[Debug] #Transitions -> ', CountTransitions);
            for Idx := 0 to CountTransitions - 1 do begin
                WriteLn('[Debug]    [',Idx,'].Source.State -> "', Transitions[Idx].Source.State,'"');
                WriteLn('[Debug]    [',Idx,'].Source.Symbol -> "', Transitions[Idx].Source.Symbol,'"');
                WriteLn('[Debug]    [',Idx,'].Target.State -> "', Transitions[Idx].Target.State,'"');
                WriteLn('[Debug]    [',Idx,'].Target.Symbol -> "', Transitions[Idx].Target.Symbol,'"');
                WriteLn('[Debug]    [',Idx,'].Target.Move -> ', TapeMoveToStr(Transitions[Idx].Target.Move));
        end;
       end;
        { TODO: Implement me }
    end;

    procedure LoadTuringMachineFromTomlFile(var Machine : TTuringMachine; Filename : string; Debug: boolean);
    var
        Document: TTomlDocument;
    begin
        LoadTomlDocumentFromFile(Document, Filename);
        LoadTuringMachineFromTomlDocument(Machine, Document, Debug);
    end;

    procedure LoadTuringMachineFromTomlString(var Machine : TTuringMachine; Content : AnsiString; Debug: boolean);
    var
        Document: TTomlDocument;
    begin
        LoadTomlDocumentFromString(Document, Content);
        LoadTuringMachineFromTomlDocument(Machine, Document, Debug);
    end;
end.