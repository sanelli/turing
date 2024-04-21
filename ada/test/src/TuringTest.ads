with AUnit.Test_Fixtures;

package TuringTest is
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Substitute (T : in out Test);
end TuringTest;