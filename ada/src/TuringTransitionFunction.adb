with Ada.Strings.Unbounded.Hash;
with TuringException; use TuringException;

package body TuringTransitionFunction is
   function Hash (from : TTuringTransitionFunctionFrom) return Hash_Type
   is
   begin
      return Ada.Strings.Unbounded.Hash (from.Status)
         xor Character'Pos (from.Symbol);
   end Hash;

   overriding function "=" (left, right : TTuringTransitionFunctionFrom)
      return Boolean
   is
   begin
      return left.Status = right.Status and then left.Symbol = right.Symbol;
   end "=";

   procedure Create (
      transitionFunction : in out TTuringTransitionFunction;
      haltStatus  : TTuringStatus)
   is
   begin
      transitionFunction.HaltStatus := haltStatus;
   end Create;

   procedure Set (
      transitionFunction : in out TTuringTransitionFunction;
      from : TTuringTransitionFunctionFrom;
      to : TTuringTransitionFunctionTo)
   is
   begin
      if transitionFunction.Map.Contains (from) then
         raise TTuringException with "Transition already setup";
      else
         transitionFunction.Map.Insert (from, to);
      end if;
   end Set;

   function Get (
      transitionFunction : in out TTuringTransitionFunction;
      from : TTuringTransitionFunctionFrom)
      return TTuringTransitionFunctionTo
   is
      haltTransition : TTuringTransitionFunctionTo;
   begin
      if not transitionFunction.Map.Contains (from) then
         haltTransition.Move := None;
         haltTransition.Status := transitionFunction.HaltStatus;
         haltTransition.Symbol := from.Symbol;
         return haltTransition;
      else
         return transitionFunction.Map.Element (from);
      end if;
   end Get;

end TuringTransitionFunction;