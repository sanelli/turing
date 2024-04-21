with TuringException; use TuringException;

package body TuringTyping is

   function FromString (str : Unbounded_String) return TTuringTapeMove
   is
   begin
      if str = "left" then
         return Left;
      elsif str = "right" then
         return Right;
      elsif str = "none" then
         return None;
      end if;

      raise TTuringException with "Unknown move";
   end FromString;

end TuringTyping;