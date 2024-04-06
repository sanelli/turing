program Turing;
uses TuringMachineIO, TuringMachine, TuringUtility, TuringTyping, TuringTape;
var
    Machine     : TTuringMachine;
    Debug       : boolean;
    InitTape    : TArrayOfSymbols;
    Idx         : integer;
begin
    if (paramCount() <> 3) and (paramCount() <> 4) then begin
        writeLn('Usage: ', paramStr(0), ' <format> <filename> <tape> [-debug]');
        writeLn('Formats:');
        writeLn('   - toml: TOML format');
        Panic('')
    end;

    Debug := (paramCount() = 4) and (paramStr(4) = '-debug');
    SetLength(InitTape, Length(paramStr(3)));
    for Idx := 1 to Length(paramStr(3)) do
        InitTape[Idx - 1] := paramStr(3)[Idx];

    LoadTuringMachineFromFile(Machine, paramStr(1), paramStr(2), Debug);
    ClearTuringMachine(Machine, InitTape);
    WriteLn('Initial tape: |', TapeToString(Machine.Tape, '|'), '|');
    RunTuringMachine(Machine, Debug);
    WriteLn('Final tape: |', TapeToString(Machine.Tape, '|'), '|');
    WriteLn('Final state: ''', Machine.CurrentState,'''');

end.