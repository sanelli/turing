package body TuringTransitionFunction is

   function Hash (from : TTuringTransitionFunctionFrom) return Hash_Type
   is
   begin
      return 0;
   end Hash;

   function Equals (left, right : TTuringTransitionFunctionFrom) return Boolean
   is
   begin
      return False;
   end Equals;
end TuringTransitionFunction;