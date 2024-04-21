with AUnit.Reporter.Text;
with AUnit.Run;
with TuringTestSuite; use TuringTestSuite;
with AUnit; use AUnit;
with Ada.Command_Line;

procedure TuringTests is
   function Runner is new AUnit.Run.Test_Runner_With_Status (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Result : AUnit.Status;
begin
   Result := Runner (Reporter);

   if Result /= Success then
      Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
   end if;
end TuringTests;