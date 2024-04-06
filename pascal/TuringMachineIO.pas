unit TuringMachineIO;
interface
    uses SimpleToml, TuringMachine, TuringUtility;

    procedure LoadTuringMachineFromFile(var Machine : TTuringMachine; Format: string; Filename : string);
    procedure LoadTuringMachineFromString(var Machine : TTuringMachine; Format: string; Content : AnsiString);
    procedure LoadTuringMachineFromTomlFile(var Machine : TTuringMachine; Filename : string);
    procedure LoadTuringMachineFromTomlString(var Machine : TTuringMachine; Content : AnsiString);

implementation

    procedure LoadTuringMachineFromFile(var Machine : TTuringMachine; Format: string; Filename: string);
    begin
        if Format = 'toml' then LoadTuringMachineFromTomlFile(Machine, Filename)
        else Panic('Unknown format "' + Format + '".');
    end;

    procedure LoadTuringMachineFromString(var Machine : TTuringMachine; Format: string; Content : AnsiString);
    begin
        if Format = 'toml' then LoadTuringMachineFromTomlString(Machine, Content)
        else Panic('Unknown format "' + Format + '".');
    end;

    procedure LoadTuringMachineFromTomlDocument(var Machine : TTuringMachine; var Document : TTomlDocument);

    begin
        { Implement me }
    end;

    procedure LoadTuringMachineFromTomlFile(var Machine : TTuringMachine; Filename : string);
    var
        Document: TTomlDocument;
    begin
        LoadTomlDocumentFromFile(Document, Filename);
        LoadTuringMachineFromTomlDocument(Machine, Document);
    end;

    procedure LoadTuringMachineFromTomlString(var Machine : TTuringMachine; Content : AnsiString);
    var
        Document: TTomlDocument;
    begin
        LoadTomlDocumentFromString(Document, Content);
        LoadTuringMachineFromTomlDocument(Machine, Document);
    end;
end.