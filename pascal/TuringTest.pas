program TuringTest;
uses TuringMachineIO, TuringMachine, TuringUtility, TuringTyping, TuringTape;

function SubstituteTestTuringMachineDescrition(): AnsiString;
begin
    SubstituteTestTuringMachineDescrition := '';
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'States = [ "replace", "halt" ]'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'InitialState = "replace"'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'FinalStates = [ "halt" ]'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'Symbols = [ " ", "a", "b" ]'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'EmptySymbol = " "'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + ''#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + '[[Transitions]]'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'State = "replace"'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'Symbol = "a"'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'NewState = "replace"'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'NewSymbol = "b"'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'Move = "right"'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + ''#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + '[[Transitions]]'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'State = "replace"'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'Symbol = "b"'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'NewState = "replace"'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'NewSymbol = "a"'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'Move = "right"'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + ''#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + '[[Transitions]]'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'State = "replace"'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'Symbol = " "'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'NewState = "halt"'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'NewSymbol = " "'#13;
    SubstituteTestTuringMachineDescrition := SubstituteTestTuringMachineDescrition + 'Move = "right"';
end;

procedure SubstituteTest();
var
    Success     : boolean;
    InitTape    : TArrayOfSymbols;
    InitTapeStr : string;
    Idx         : integer;
    Machine     : TTuringMachine;
begin
    Success := true;
    Write('- Substitute: ');

    InitTapeStr := 'abba';
    SetLength(InitTape, Length(InitTapeStr));
    for Idx := 1 to Length(InitTapeStr) do
        InitTape[Idx - 1] := InitTapeStr[Idx];

    LoadTuringMachineFromString(Machine, 'toml', SubstituteTestTuringMachineDescrition, false);
    ClearTuringMachine(Machine, InitTape);
    RunTuringMachine(Machine, false);

    Success := '|b|a|a|b| |' = TapeToString(Machine.Tape, '|');
    Success := Success and (Machine.CurrentState = 'halt');

    if Success then WriteLn('OK')
    else begin
        Panic('KO')
    end;
end;

begin
    SubstituteTest();
end.