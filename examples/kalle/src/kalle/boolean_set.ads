--::::::::::
--booleset.ads
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
generic
   type element is (<>);
package boolean_set is

   type boolset is array (element) of boolean;
   pragma pack (boolset);

   function value (right: in element) return boolset;

   function "and" (left: in boolset; right: in element) return boolset;
   function "or"  (left: in boolset; right: in element) return boolset;
   function "xor" (left: in boolset; right: in element) return boolset;

   function "and" (left: in element; right: in boolset) return boolset;
   function "or"  (left: in element; right: in boolset) return boolset;
   function "xor" (left: in element; right: in boolset) return boolset;

   function "and" (left: in element; right: in element) return boolset;
   function "or"  (left: in element; right: in element) return boolset;
   function "xor" (left: in element; right: in element) return boolset;

   function "not" (right: in element) return boolset;

   generic
      type index   is (<>);
      type object is array (index range <>) of element;
   function array_import (right: in object) return boolset;

   generic
      type object is limited private;
      with function value (right: in object) return boolset is <>;
   package import_set is

      function "and" (left: in boolset; right: in object) return boolset;
      function "or"  (left: in boolset; right: in object) return boolset;
      function "xor" (left: in boolset; right: in object) return boolset;

      function "and" (left: in object; right: in boolset) return boolset;
      function "or"  (left: in object; right: in boolset) return boolset;
      function "xor" (left: in object; right: in boolset) return boolset;

      function "and" (left: in object; right: in element) return boolset;
      function "or"  (left: in object; right: in element) return boolset;
      function "xor" (left: in object; right: in element) return boolset;

      function "and" (left: in element; right: in object) return boolset;
      function "or"  (left: in element; right: in object) return boolset;
      function "xor" (left: in element; right: in object) return boolset;

      function "and" (left: in object; right: in object) return boolset;
      function "or"  (left: in object; right: in object) return boolset;
      function "xor" (left: in object; right: in object) return boolset;

      function "not" (right: in object) return boolset;

    end;
end;
