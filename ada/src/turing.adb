with TuringMachineIO;
with TuringMachine; use TuringMachine;
with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

procedure Turing
is
   package CLI renames Ada.Command_Line;
   package IO renames Ada.Text_IO;
   package UIO renames Ada.Text_IO.Unbounded_IO;

   Machine  : TTuringMachine;
   Message  : Unbounded_String;
begin
   if CLI.Argument_Count /= 1 then
      IO.Put_Line ("Usage: turing <format> <filename> <tape>");
      IO.Put_Line ("Formats:");
      IO.Put_Line ("   - toml: TOML format for the turing machine.");
      CLI.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      TuringMachineIO.LoadMachineFromFile (
         Machine,
         CLI.Argument (1),
         CLI.Argument (2));

      Machine.Clear (To_Unbounded_String (CLI.Argument (3)));
      Message := To_Unbounded_String ("Initial tape: |");
      Append (Message, Machine.Tape.ToString);
      Append (Message, "|");
      UIO.Put_Line (Message);

      Machine.Run;
      Message := To_Unbounded_String ("Final tape: |");
      Append (Message, Machine.Tape.ToString);
      Append (Message, "|");
      UIO.Put_Line (Message);

      Message := To_Unbounded_String ("Final state: |");
      Append (Message, Machine.CurrentStatus);
      UIO.Put_Line (Message);

      CLI.Set_Exit_Status (Ada.Command_Line.Success);
   end if;
end Turing;
