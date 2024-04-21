with AUnit.Assertions; use AUnit.Assertions;
with TuringMachine; use TuringMachine;
with TuringMachineIO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;

package body TuringTest is
   procedure Test_Substitute (T : in out Test)
   is
      pragma Unreferenced (T);
      Program : Unbounded_String;
      Machine : TTuringMachine;
   begin
      Program := To_Unbounded_String ("");
      Append (Program, "States = [ ""replace"", ""halt"" ]"
         & Ada.Characters.Latin_1.LF);
      Append (Program, "InitialState = ""replace"""
         & Ada.Characters.Latin_1.LF);
      Append (Program, "FinalStates = [ ""halt"" ]"
         & Ada.Characters.Latin_1.LF);
      Append (Program, "Symbols = [ "" "", ""a"", ""b"" ]"
      & Ada.Characters.Latin_1.LF);
      Append (Program, "EmptySymbol = "" """
         & Ada.Characters.Latin_1.LF);
      Append (Program, "[[Transitions]]"
         & Ada.Characters.Latin_1.LF);
      Append (Program, "State = ""replace"""
         & Ada.Characters.Latin_1.LF);
      Append (Program, "Symbol = ""a"""
         & Ada.Characters.Latin_1.LF);
      Append (Program, "NewState = ""replace"""
         & Ada.Characters.Latin_1.LF);
      Append (Program, "NewSymbol = ""b"""
         & Ada.Characters.Latin_1.LF);
      Append (Program, "Move = ""right"""
         & Ada.Characters.Latin_1.LF);
      Append (Program, "[[Transitions]]"
         & Ada.Characters.Latin_1.LF);
      Append (Program, "State = ""replace"""
         & Ada.Characters.Latin_1.LF);
      Append (Program, "Symbol = ""b"""
         & Ada.Characters.Latin_1.LF);
      Append (Program, "NewState = ""replace"""
         & Ada.Characters.Latin_1.LF);
      Append (Program, "NewSymbol = ""a"""
         & Ada.Characters.Latin_1.LF);
      Append (Program, "Move = ""right"""
         & Ada.Characters.Latin_1.LF);
      Append (Program, "[[Transitions]]"
         & Ada.Characters.Latin_1.LF);
      Append (Program, "State = ""replace"""
         & Ada.Characters.Latin_1.LF);
      Append (Program, "Symbol = "" """
         & Ada.Characters.Latin_1.LF);
      Append (Program, "NewState = ""halt"""
         & Ada.Characters.Latin_1.LF);
      Append (Program, "NewSymbol = "" """
         & Ada.Characters.Latin_1.LF);
      Append (Program, "Move = ""right"""
         & Ada.Characters.Latin_1.LF);

      TuringMachineIO.LoadMachineFromContent (
         Machine,
         "toml",
         Program);
      Machine.Clear (To_Unbounded_String ("abba"));
      Machine.Run;

      Assert (To_String (Machine.Tape.ToString) =
         "|b|a|a|b| |", "Expected tape hsould be '|b|a|a|b| |' but it '"
         & To_String (Machine.Tape.ToString) & "'");
      Assert (To_String (Machine.CurrentStatus) =
      "halt", "Expected status should be 'halt' but is '"
      & To_String (Machine.CurrentStatus) & "'");
   end Test_Substitute;
end TuringTest;