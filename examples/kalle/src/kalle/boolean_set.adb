--::::::::::
--booleset.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
package body boolean_set is

   function value (right: in element) return boolset is
      tmp: boolset := (others => false);
   begin
      tmp (right) := true;
      return tmp;
   end;

   function array_import (right: in object) return boolset is
      tmp: boolset := (others => false);
   begin
      for n in right'range
      loop
	 tmp (right(n)) := true;
      end loop;
      return tmp;
   end;

   --not-------------------------------------------------------------------

   function "not" (right: in element) return boolset is
   begin
      return not value (right);
   end;

   --set:elm---------------------------------------------------------------

   function "and" (left: in boolset; right: in element) return boolset is
   begin
      return left and value(right);
   end;

   function "or"  (left: in boolset; right: in element) return boolset is
   begin
      return left or value(right);
   end;

   function "xor" (left: in boolset; right: in element) return boolset is
   begin
      return left xor value(right);
   end;

   --elm:set--------------------------------------------------------------

   function "and" (left: in element; right: in boolset) return boolset is
   begin
      return value (left) and right;
   end;

   function "or"  (left: in element; right: in boolset) return boolset is
   begin
      return value (left) or right;
   end;

   function "xor" (left: in element; right: in boolset) return boolset is
   begin
      return value (left) xor right;
   end;

   --elm:elm--------------------------------------------------------------

   function "and" (left: in element; right: in element) return boolset is
   begin
      return value (left) and right;
   end;

   function "or"  (left: in element; right: in element) return boolset is
   begin
      return value (left) or right;
   end;

   function "xor" (left: in element; right: in element) return boolset is
   begin
      return value (left) xor right;
   end;

   package body import_set is

      --set:seq------------------------------------------------------------

      function "not" (right: in object) return boolset is
      begin
	 return not value (right);
      end;

      function "and" (left: in boolset; right: in object) return boolset is
      begin
	 return left and value(right);
      end;

      function "or"  (left: in boolset; right: in object) return boolset is
      begin
	 return left or value(right);
      end;

      function "xor" (left: in boolset; right: in object) return boolset is
	 tmp: boolset := (others => false);
      begin
	 return left xor value(right);
      end;

      --seq:set------------------------------------------------------------

      function "and" (left: in object; right: in boolset) return boolset is
      begin
	 return value (left) xor right;
      end;

      function "or"  (left: in object; right: in boolset) return boolset is
      begin
	 return value (left) xor right;
      end;

      function "xor" (left: in object; right: in boolset) return boolset is
      begin
	 return value (left) xor right;
      end;

      --seq:elm------------------------------------------------------------

      function "and" (left: in object; right: in element) return boolset is
      begin
	 return value (left) and value(right);
      end;

      function "or"  (left: in object; right: in element) return boolset is
      begin
	 return value (left) or value(right);
      end;

      function "xor" (left: in object; right: in element) return boolset is
      begin
	 return value (left) xor value(right);
      end;

      --elm:seq------------------------------------------------------------

      function "and" (left: in element; right: in object) return boolset is
	 tmp: boolset := (others => false);
      begin
	 return value (left) and value(right);
      end;

      function "or"  (left: in element; right: in object) return boolset is
      begin
	 return value (left) or value(right);
      end;

      function "xor" (left: in element; right: in object) return boolset is
      begin
	 return value (left) xor value(right);
      end;

      --seq:seq------------------------------------------------------------

      function "and" (left: in object; right: in object) return boolset is
	 tmp: boolset := (others => false);
      begin
	 return value (left) and right;
      end;

      function "or"  (left: in object; right: in object) return boolset is
      begin
	 return value (left) or right;
      end;

      function "xor" (left: in object; right: in object) return boolset is
      begin
	 return value (left) xor right;
      end;
   end;
end;
