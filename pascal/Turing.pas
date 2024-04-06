program Turing;
uses TuringMachineIO, TuringMachine, TuringUtility;
var
    Machine : TTuringMachine;
begin
    if paramCount() <> 3 then begin
        writeLn('Usage: ', paramStr(0), ' <format> <filename> <tape>');
        writeLn('Formats:');
        writeLn('   - toml: TOML format');
        Panic('')
    end;

    LoadTuringMachineFromFile(Machine, paramStr(1), paramStr(2));
end.