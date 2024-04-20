with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with TuringException; use TuringException;

package body TuringMachineIO is

   procedure LoadMachineFromTomlContent (
      machine : in out TTuringMachine;
      content : Unbounded_String)
   is
   begin
      null;
   end LoadMachineFromTomlContent;

   procedure LoadMachineFromContent (
      machine : in out TTuringMachine;
      format  : String;
      content : Unbounded_String)
   is
   begin
      if format = "toml" then
         LoadMachineFromTomlContent (machine, content);
      else
         raise TTuringException with "Unknown format '" & format & "'";
      end if;
   end LoadMachineFromContent;

   procedure LoadMachineFromFile (
      machine : in out TTuringMachine;
      format  : String;
      fileName : String)
   is
      package UIO renames Ada.Text_IO.Unbounded_IO;

      file : File_Type;
      content : Unbounded_String;
   begin
      content := Ada.Strings.Unbounded.To_Unbounded_String ("");
      Open (file, In_File, fileName);
      while not End_Of_File (file) loop
         content := content & UIO.Get_Line (file);
      end loop;
      Close (file);

      LoadMachineFromContent (machine, format, content);
   end LoadMachineFromFile;

end TuringMachineIO;