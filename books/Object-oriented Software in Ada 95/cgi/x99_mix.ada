---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:51:01 PM BST  --
---------------------------------------------------------------------
with Interfaces.C, Ada.Unchecked_Conversion;
use  Interfaces.C;
function To_C_Int is new
  Ada.Unchecked_Conversion( Integer, Int );

with Interfaces.C, Ada.Unchecked_Conversion;
use  Interfaces.C;
function To_Ada_Integer is new
  Ada.Unchecked_Conversion( Int, Integer );

with Interfaces.C, To_C_Int, To_Ada_Integer;
use  Interfaces.C;
function Double( N:in Integer ) return Integer is
  function C_Double(N:in Int) return Int;
  pragma Import (C, C_Double, "c_double");
begin
  return To_Ada_Integer( C_Double( To_C_Int(N) ) );
end Double;

--[main.adb] Procedure
with Ada.Text_Io, Ada.Integer_Text_Io, Ada.Float_Text_Io, Double;
use  Ada.Text_Io, Ada.Integer_Text_Io, Ada.Float_Text_Io;
procedure Main is
begin
  Put("3 Doubled is "); Put( Double(3) ); New_Line;
end Main;
