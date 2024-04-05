program Turing;
uses TuringMachine;
begin
    if paramCount() <> 3 then begin
        writeLn('Usage: ', paramStr(0), ' <format> <filename> <tape>');
        writeLn('Formats:');
        writeLn('   - toml: TOML format');
    end else begin
        writeLn('Work in progress:');
        writeLn('   - Format: ',  paramStr(1));
        writeLn('   - Filename: ',  paramStr(2));
        writeLn('   - Tape: ',  paramStr(3));
    end;
end.