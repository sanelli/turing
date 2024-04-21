with TuringMachine; use TuringMachine;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package TuringMachineIO is

   procedure LoadMachineFromContent (machine : in out TTuringMachine;
      format  : String;
      content : Unbounded_String);

   procedure LoadMachineFromFile (machine : in out TTuringMachine;
      format  : String;
      fileName : String);

end TuringMachineIO;