with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with TuringException; use TuringException;
with TOML; use TOML;
with TuringTyping; use TuringTyping;
with TuringTransitionFunction; use TuringTransitionFunction;
with Ada.Characters.Latin_1;

package body TuringMachineIO is

   procedure LoadMachineFromTomlContent (
      machine : in out TTuringMachine;
      content : Unbounded_String)
   is
      Result : TOML.Read_Result;

      StatusesToml : TOML_Value;
      Statuses : TTuringStatusesSets.Set;

      InitialState : Unbounded_String;

      FinalStatusesToml : TOML_Value;
      FinalStatuses : TTuringStatusesSets.Set;

      SymbolsToml : TOML_Value;
      Symbols : TTuringSymbolsSets.Set;
      SymbolStr : Unbounded_String;

      EmptySymbolStr : Unbounded_String;
      EmptySymbol : Character;

      TransitionsToml : TOML_Value;
      TransitionToml  : TOML_Value;
      Transitions : TuringTransitionFunctionMap.Map;

      From : TTuringTransitionFunctionFrom;
      FromSymbolStr : Unbounded_String;

      To : TTuringTransitionFunctionTo;
      ToSymbolStr : Unbounded_String;
      ToMoveStr : Unbounded_String;
   begin
      Result := TOML.Load_String (Ada.Strings.Unbounded.To_String (content));

      --  States
      StatusesToml := Get (Result.Value, "States");
      for Index in 1 .. Length (StatusesToml) loop
         Statuses.Insert (Item (StatusesToml, Index).As_Unbounded_String);
      end loop;

      --  Initial state
      InitialState := Get (Result.Value, "InitialState").As_Unbounded_String;

      --  Final States
      FinalStatusesToml := Get (Result.Value, "FinalStates");
      for Index in 1 .. Length (FinalStatusesToml) loop
         FinalStatuses.Insert
            (Item (FinalStatusesToml, Index).As_Unbounded_String);
      end loop;

      --  Symbols
      SymbolsToml := Get (Result.Value, "Symbols");
      for Index in 1 .. Length (SymbolsToml) loop
         SymbolStr := Item (SymbolsToml, Index).As_Unbounded_String;
         if Length (SymbolStr) /= 1 then
            raise TTuringException with "Symbol must have lenght 1";
         end if;

         Symbols.Insert (Element (SymbolStr, 1));
      end loop;

      --  Empty symbol
      EmptySymbolStr :=  Get (Result.Value, "EmptySymbol").As_Unbounded_String;
      if Length (EmptySymbolStr) /= 1 then
         raise TTuringException with "Empty symbol must have lenght 1";
      end if;
      EmptySymbol := Element (EmptySymbolStr, 1);

      --  Transitions
      TransitionsToml := Get (Result.Value, "Transitions");

      for Index in 1 .. Length (TransitionsToml) loop
         TransitionToml := Item (TransitionsToml, Index);
         From.Status := Get (TransitionToml, "State").As_Unbounded_String;
         FromSymbolStr := Get (TransitionToml, "Symbol").As_Unbounded_String;
         To.Status := Get (TransitionToml, "NewState").As_Unbounded_String;
         ToSymbolStr := Get (TransitionToml, "NewSymbol").As_Unbounded_String;
         ToMoveStr := Get (TransitionToml, "Move").As_Unbounded_String;

         if Length (FromSymbolStr) /= 1 then
            raise TTuringException with "Symbol must have length 1";
         end if;

         From.Symbol := Element (FromSymbolStr, 1);

         if Length (ToSymbolStr) /= 1 then
            raise TTuringException with "Symbol must have length 1";
         end if;

         To.Symbol := Element (ToSymbolStr, 1);
         To.Move := FromString (ToMoveStr);

         Transitions.Insert (From, To);
      end loop;

      Create
        (machine       => machine,
         statuses      => Statuses,
         initialStatus => InitialState,
         finalStatues  => FinalStatuses,
         symbols       => Symbols,
         emptySymbol   => EmptySymbol,
         transitions   => Transitions);
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
         content := content & UIO.Get_Line (file) & Ada.Characters.Latin_1.LF;
      end loop;
      Close (file);

      LoadMachineFromContent (machine, format, content);
   end LoadMachineFromFile;

end TuringMachineIO;