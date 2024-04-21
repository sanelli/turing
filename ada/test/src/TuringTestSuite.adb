with AUnit.Test_Caller;
with TuringTest;

package body TuringTestSuite is

   package Caller is new AUnit.Test_Caller (TuringTest.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
          (Caller.Create ("Substitute", TuringTest.Test_Substitute'Access));
      return Ret;
   end Suite;

end TuringTestSuite;