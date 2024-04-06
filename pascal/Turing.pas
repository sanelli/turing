program Turing;
uses TuringMachineIO, TuringMachine, TuringUtility;
var
    Machine : TTuringMachine;
    Debug   : boolean;
begin
    if (paramCount() <> 3) and (paramCount() <> 4) then begin
        writeLn('Usage: ', paramStr(0), ' <format> <filename> <tape> [-debug]');
        writeLn('Formats:');
        writeLn('   - toml: TOML format');
        Panic('')
    end;

    Debug := (paramCount() = 4) and (paramStr(4) = '-debug');

    LoadTuringMachineFromFile(Machine, paramStr(1), paramStr(2), Debug);
end.